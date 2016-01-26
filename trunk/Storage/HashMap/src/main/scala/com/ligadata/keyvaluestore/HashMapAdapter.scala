/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
datastoreConfig should have the following:
	Mandatory Options:
		path/Location

	Optional Options:
		inmemory
		withtransaction

		All the optional values may come from "AdapterSpecificConfig" also. That is the old way of giving more information specific to Adapter
*/

/*

No schema setup

 */

package com.ligadata.keyvaluestore

import com.ligadata.KvBase.{ Key, Value, TimeRange }
import com.ligadata.StorageBase.{ DataStore, Transaction, StorageAdapterObj }

import org.mapdb._
import java.io._
import java.nio.ByteBuffer
import org.apache.logging.log4j._
import com.ligadata.Exceptions._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.ligadata.Utils.{ KamanjaLoaderInfo }

import com.ligadata.Exceptions._

class HashMapAdapter(val kvManagerLoader: KamanjaLoaderInfo, val datastoreConfig: String) extends DataStore {
  val adapterConfig = if (datastoreConfig != null) datastoreConfig.trim else ""
  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)

  private[this] val lock = new Object
  private var containerList: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()
  private var msg: String = ""

  private var tablesMap: scala.collection.mutable.Map[String, HTreeMap[Array[Byte], Array[Byte]]] = new scala.collection.mutable.HashMap()
  private var dbStoreMap: scala.collection.mutable.Map[String, DB] = new scala.collection.mutable.HashMap()

  private def CreateConnectionException(msg: String, ie: Exception): StorageConnectionException = {
    logger.error(msg)
    val ex = new StorageConnectionException("Failed to connect to Database", ie)
    ex
  }

  private def CreateDMLException(msg: String, ie: Exception): StorageDMLException = {
    logger.error(msg)
    val ex = new StorageDMLException("Failed to execute select/insert/delete/update operation on Database", ie)
    ex
  }

  private def CreateDDLException(msg: String, ie: Exception): StorageDDLException = {
    logger.error(msg)
    val ex = new StorageDDLException("Failed to execute create/drop operations on Database", ie)
    ex
  }

  if (adapterConfig.size == 0) {
    msg = "Invalid Cassandra Json Configuration string:" + adapterConfig
    throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
  }

  logger.debug("HashMap configuration:" + adapterConfig)
  var parsed_json: Map[String, Any] = null
  try {
    val json = parse(adapterConfig)
    if (json == null || json.values == null) {
      logger.error("Failed to parse HashMap JSON configuration string:" + adapterConfig)
      throw new Exception("Failed to parse HashMap JSON configuration string:" + adapterConfig)
    }
    parsed_json = json.values.asInstanceOf[Map[String, Any]]
  } catch {
    case e: Exception => {
      var msg = "Failed to parse Cassandra JSON configuration string:%s. Message:%s".format(adapterConfig, e.getMessage)
      throw CreateConnectionException(msg, e)
    }
  }

  // Getting AdapterSpecificConfig if it has
  var adapterSpecificConfig_json: Map[String, Any] = null

  if (parsed_json.contains("AdapterSpecificConfig")) {
    val adapterSpecificStr = parsed_json.getOrElse("AdapterSpecificConfig", "").toString.trim
    if (adapterSpecificStr.size > 0) {
      try {
        val json = parse(adapterSpecificStr)
        if (json == null || json.values == null) {
          msg = "Failed to parse Cassandra JSON configuration string:" + adapterSpecificStr
          throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
        }
        adapterSpecificConfig_json = json.values.asInstanceOf[Map[String, Any]]
      } catch {
        case e: Exception => {
          msg = "Failed to parse Cassandra Adapter Specific JSON configuration string:%s. Message:%s".format(adapterSpecificStr, e.getMessage)
          throw CreateConnectionException(msg, e)
        }
      }
    }
  }

  private def getOptionalField(key: String, main_json: Map[String, Any], adapterSpecific_json: Map[String, Any], default: Any): Any = {
    if (main_json != null) {
      val mainVal = main_json.getOrElse(key, null)
      if (mainVal != null)
        return mainVal
    }
    if (adapterSpecific_json != null) {
      val mainVal1 = adapterSpecific_json.getOrElse(key, null)
      if (mainVal1 != null)
        return mainVal1
    }
    return default
  }

  val path = if (parsed_json.contains("path")) parsed_json.getOrElse("path", ".").toString.trim else parsed_json.getOrElse("Location", ".").toString.trim
  val InMemory = getOptionalField("inmemory", parsed_json, adapterSpecificConfig_json, "false").toString.trim.toBoolean
  val withTransactions = getOptionalField("withtransaction", parsed_json, adapterSpecificConfig_json, "true").toString.trim.toBoolean

  logger.info("Datastore initialization complete")

  private def createTable(tableName: String): Unit = {
    logger.debug("Creating table:" + tableName)
    try {
      var db: DB = null
      if (InMemory == true) {
        db = DBMaker.newMemoryDB().make()
      } else {
        val dir = new File(path);
        if (!dir.exists()) {
          // attempt to create the directory here
          dir.mkdir();
        }
        db = DBMaker.newFileDB(new File(path + "/" + tableName + ".hdb"))
          .closeOnJvmShutdown()
          .mmapFileEnable()
          .transactionDisable()
          .commitFileSyncDisable()
          .make()
        dbStoreMap.put(tableName, db)
        var map = db.createHashMap(tableName)
          .hasher(Hasher.BYTE_ARRAY)
          .makeOrGet[Array[Byte], Array[Byte]]()
        tablesMap.put(tableName, map)
      }
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to create table " + tableName + ":" + e.getMessage(), e)
      }
    }
    logger.debug("Done creating table:" + tableName)
  }

  @throws(classOf[FileNotFoundException])
  def deleteFile(file: File): Unit = {
    if (file.exists()) {
      var ret = true
      if (file.isDirectory) {
        for (f <- file.listFiles) {
          deleteFile(f)
        }
      }
      logger.debug("cleanup: Deleting file '" + file + "'")
      file.delete()
    }
  }

  private def dropTable(tableName: String): Unit = {
    var f = new File(path + "/" + tableName + ".hdb")
    deleteFile(f)
    f = new File(path + "/" + tableName + ".hdb.p")
    deleteFile(f)
  }

  private def CheckTableExists(containerName: String): Unit = {
    logger.debug("CheckTableExists: " + containerName + ",this => " + this)
    if (containerList.contains(containerName)) {
      return
    }
    lock.synchronized {
      if (containerList.contains(containerName) == false) {
        CreateContainer(containerName)
        containerList.add(containerName)
      }
    }
  }

  private def toTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    //containerName.replace('.','_')
    containerName.toLowerCase.replace('.', '_').replace('-', '_')
  }

  private def toFullTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    toTableName(containerName)
  }

  private def CreateContainer(containerName: String): Unit = lock.synchronized {
    if (containerList.contains(containerName) == false) {
      var tableName = toTableName(containerName)
      var fullTableName = toFullTableName(containerName)
      try {
        createTable(fullTableName)
      } catch {
        case e: Exception => {
          val stackTrace = StackTrace.ThrowableTraceString(e)
          logger.error("Stacktrace:" + stackTrace)
        }
      }
    }
  }

  override def CreateContainer(containerNames: Array[String]): Unit = {
    logger.info("create the container tables")
    containerNames.foreach(cont => {
      logger.info("create the container " + cont)
      CreateContainer(cont)
    })
  }

  private def MakeCompositeKey(key: Key): Array[Byte] = {
    var compKey = key.timePartition.toString + "|" + key.bucketKey.mkString(".") +
      "|" + key.transactionId.toString + "|" + key.rowId.toString
    compKey.getBytes()
  }

  private def ValueToByteArray(value: Value, out: DataOutputStream): Unit = {
    out.writeInt(value.serializerType.length);
    out.write(value.serializerType.getBytes());
    out.writeInt(value.serializedInfo.length);
    out.write(value.serializedInfo);
  }

  private def ByteArrayToValue(byteArray: Array[Byte]): Value = {
    var in = new DataInputStream(new ByteArrayInputStream(byteArray));

    var serializerTypeLength = in.readInt();
    var serializerType = new Array[Byte](serializerTypeLength);
    in.read(serializerType, 0, serializerTypeLength);

    var serializedInfoLength = in.readInt();
    var serializedInfo = new Array[Byte](serializedInfoLength);
    in.read(serializedInfo, 0, serializedInfoLength);

    var v = new Value(new String(serializerType), serializedInfo)
    v
  }

  private def Commit: Unit = {
    if (withTransactions) {
      dbStoreMap.foreach(db => {
        logger.debug("Committing transactions for db " + db._1)
        db._2.commit();
      })
    }
  }

  private def Commit(tableName: String): Unit = {
    if (withTransactions) {
      var db = dbStoreMap(tableName)
      db.commit();
    }
  }

  override def put(containerName: String, key: Key, value: Value): Unit = {
    var tableName = toFullTableName(containerName)
    var byteOs = new ByteArrayOutputStream(1024 * 1024);
    var out = new DataOutputStream(byteOs);

    try {
      CheckTableExists(containerName)
      val kba = MakeCompositeKey(key)
      ValueToByteArray(value, out)
      val vba = byteOs.toByteArray()
      val map = tablesMap(tableName)
      map.put(kba, vba)
      Commit(tableName)
    } catch {
      case e: Exception => {
        out.close()
        byteOs.close()
        throw CreateDMLException("Failed to save an object in table " + tableName + ":" + e.getMessage(), e)
      }
    }
    out.close()
    byteOs.close()
  }

  override def put(data_list: Array[(String, Array[(Key, Value)])]): Unit = {
    var byteOs = new ByteArrayOutputStream(1024 * 1024);
    var out = new DataOutputStream(byteOs);
    var tableName = ""
    try {
      data_list.foreach(li => {
        var containerName = li._1
        CheckTableExists(containerName)
        tableName = toFullTableName(containerName)
        var map = tablesMap(tableName)
        var keyValuePairs = li._2
        keyValuePairs.foreach(keyValuePair => {
          val kba = MakeCompositeKey(keyValuePair._1)
          byteOs.reset()
          ValueToByteArray(keyValuePair._2, out)
          var vba = byteOs.toByteArray()
          map.put(kba, vba)
        })
        Commit(tableName)
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save an object in table " + tableName + ":" + e.getMessage(), e)
        out.close()
        byteOs.close()
      }
    }
    out.close()
    byteOs.close()
  }

  // delete operations
  override def del(containerName: String, keys: Array[Key]): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      keys.foreach(key => {
        var kba = MakeCompositeKey(key)
        map.remove(kba)
      })
      Commit(tableName)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def del(containerName: String, time: TimeRange, bucketKeys: Array[Array[String]]): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var bucketKeyMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
      bucketKeys.foreach(bucketKey => {
        var bkey = bucketKey.mkString(".")
        bucketKeyMap.put(bkey, true)
      })

      var map = tablesMap(tableName)
      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val k = new String(kba)
        var keyArray = k.split('|')
        var tp = keyArray(0).toLong
        var bkey = keyArray(1)
        if (tp >= time.beginTime && tp <= time.endTime) {
          logger.info("searching for " + keyArray(1))
          var keyExists = bucketKeyMap.getOrElse(bkey, null)
          if (keyExists != null) {
            map.remove(kba)
          }
        }
      }
      Commit(tableName)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  // get operations
  def getRowCount(containerName: String): Long = {
    var tableName = toFullTableName(containerName)
    var map = tablesMap(tableName)
    var iter = map.keySet().iterator()
    var cnt = 0
    while (iter.hasNext()) {
      val kba = iter.next()
      cnt = cnt + 1
    }
    return cnt
  }

  private def processRow(k: String, value: Value, callbackFunction: (Key, Value) => Unit) {
    var keyArray = k.split('|').toArray
    var timePartition = keyArray(0).toLong
    var keyStr = keyArray(1)
    var tId = keyArray(2).toLong
    var rId = keyArray(3).toInt
    // format the data to create Key/Value
    val bucketKey = if (keyStr != null) keyStr.split('.').toArray else new Array[String](0)
    var key = new Key(timePartition, bucketKey, tId, rId)
    (callbackFunction)(key, value)
  }

  private def processRow(key: Key, value: Value, callbackFunction: (Key, Value) => Unit) {
    (callbackFunction)(key, value)
  }

  private def processKey(k: String, callbackFunction: (Key) => Unit) {
    var keyArray = k.split('|').toArray
    var timePartition = keyArray(0).toLong
    var keyStr = keyArray(1)
    var tId = keyArray(2).toLong
    var rId = keyArray(3).toInt
    // format the data to create Key/Value
    val bucketKey = if (keyStr != null) keyStr.split('.').toArray else new Array[String](0)
    var key = new Key(timePartition, bucketKey, tId, rId)
    (callbackFunction)(key)
  }

  override def get(containerName: String, callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val k = new String(kba)
        var vba = map.get(kba)
        var v = ByteArrayToValue(vba)
        processRow(k, v, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, callbackFunction: (Key) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val k = new String(kba)
        processKey(k, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, keys: Array[Key], callbackFunction: (Key) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      keys.foreach(key => {
        var kba = MakeCompositeKey(key)
        var k = new String(kba)
        var vba = map.get(kba)
        if (vba != null) {
          processKey(k, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.error("Stacktrace:" + stackTrace)
      }
    }
  }

  override def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      keys.foreach(key => {
        var kba = MakeCompositeKey(key)
        var vba = map.get(kba)
        if (vba != null) {
          var value = ByteArrayToValue(vba)
          processRow(key, value, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        time_ranges.foreach(timeRange => {
          var keyArray = key.split('|')
          var tp = keyArray(0).toLong
          if (tp >= timeRange.beginTime && tp <= timeRange.endTime) {
            var vba = map.get(kba)
            if (vba != null) {
              var value = ByteArrayToValue(vba)
              processRow(key, value, callbackFunction)
            }
          }
        })
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key) => Unit): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        time_ranges.foreach(timeRange => {
          var keyArray = key.split('|')
          var tp = keyArray(0).toLong
          if (tp >= timeRange.beginTime && tp <= timeRange.endTime) {
            processKey(key, callbackFunction)
          }
        })
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = ""
    try {
      var bucketKeyMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
      bucketKeys.foreach(bucketKey => {
        var bkey = bucketKey.mkString(".")
        bucketKeyMap.put(bkey, true)
      })

      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        time_ranges.foreach(timeRange => {
          var keyArray = key.split('|')
          var tp = keyArray(0).toLong
          if (tp >= timeRange.beginTime && tp <= timeRange.endTime) {
            var bkey = keyArray(1)
            var keyExists = bucketKeyMap.getOrElse(bkey, null)
            if (keyExists != null) {
              var vba = map.get(kba)
              if (vba != null) {
                var value = ByteArrayToValue(vba)
                processRow(key, value, callbackFunction)
              }
            }
          }
        })
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    var tableName = ""
    try {
      var bucketKeyMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
      bucketKeys.foreach(bucketKey => {
        var bkey = bucketKey.mkString(".")
        bucketKeyMap.put(bkey, true)
      })

      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        time_ranges.foreach(timeRange => {
          var keyArray = key.split('|')
          var tp = keyArray(0).toLong
          if (tp >= timeRange.beginTime && tp <= timeRange.endTime) {
            var bkey = keyArray(1)
            var keyExists = bucketKeyMap.getOrElse(bkey, null)
            if (keyExists != null) {
              processKey(key, callbackFunction)
            }
          }
        })
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = ""
    try {
      var bucketKeyMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
      bucketKeys.foreach(bucketKey => {
        var bkey = bucketKey.mkString(".")
        bucketKeyMap.put(bkey, true)
      })

      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        var keyArray = key.split('|')
        var bkey = keyArray(1)
        var keyExists = bucketKeyMap.getOrElse(bkey, null)
        if (keyExists != null) {
          var vba = map.get(kba)
          if (vba != null) {
            var value = ByteArrayToValue(vba)
            processRow(key, value, callbackFunction)
          }
        }
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    var tableName = ""
    try {
      var bucketKeyMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
      bucketKeys.foreach(bucketKey => {
        var bkey = bucketKey.mkString(".")
        bucketKeyMap.put(bkey, true)
      })

      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)

      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        val key = new String(kba)
        var keyArray = key.split('|')
        var bkey = keyArray(1)
        var keyExists = bucketKeyMap.getOrElse(bkey, null)
        if (keyExists != null) {
          processKey(key, callbackFunction)
        }
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def beginTx(): Transaction = {
    new HashMapAdapterTx(this)
  }

  override def endTx(tx: Transaction): Unit = {}

  override def commitTx(tx: Transaction): Unit = {}

  override def rollbackTx(tx: Transaction): Unit = {}

  override def Shutdown(): Unit = {
    logger.info("Trying to shutdown hashmap db")
    try {
      logger.info("Commit any outstanding transactions")
      dbStoreMap.foreach(db => {
        logger.info("Committing transactions for db " + db._1)
        db._2.commit();
      })
      logger.info("Close all map objects")
      tablesMap.foreach(map => {
        logger.info("Closing the map " + map._1)
        map._2.close();
      })
    } catch {
      case e: NullPointerException => {
        throw CreateDDLException("Failed to shutdown map " + ":" + e.getMessage(), e)
      }
      case e: Exception => {
        throw CreateDDLException("Failed to shutdown map " + ":" + e.getMessage(), e)
      }
    }
  }

  private def TruncateContainer(containerName: String): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var map = tablesMap(tableName)
      var iter = map.keySet().iterator()
      while (iter.hasNext()) {
        val kba = iter.next()
        map.remove(kba)
      }
      Commit
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to truncate table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def TruncateContainer(containerNames: Array[String]): Unit = {
    logger.info("truncate the container tables")
    containerNames.foreach(cont => {
      logger.info("truncate the container " + cont)
      TruncateContainer(cont)
    })
  }

  private def DropContainer(containerName: String): Unit = lock.synchronized {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      dropTable(tableName)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Unable to drop table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def DropContainer(containerNames: Array[String]): Unit = {
    logger.info("drop the container tables")
    containerNames.foreach(cont => {
      logger.info("drop the container " + cont)
      DropContainer(cont)
    })
  }

  def backupContainer(containerName: String): Unit = {
    logger.info("Not Implemeted yet")
  }

  def restoreContainer(containerName: String): Unit = {
    logger.info("Not Implemeted yet")
  }

  override def isContainerExists(containerName: String): Boolean = {
    throw CreateDDLException("Not Implemented yet :", new Exception("Failed to check container existence " + containerName))
  }

  override def copyContainer(srcContainerName: String, destContainerName: String, forceCopy: Boolean): Unit = {
    throw CreateDDLException("Not Implemented yet :", new Exception("Failed to copy container " + srcContainerName))
  }

  override def getAllTables: Array[String] = {
    logger.info("Not Implemeted yet")
    new Array[String](0)
  }

  override def dropTables(tbls: Array[String]): Unit = {
    logger.info("Not Implemeted yet")
  }

  override def dropTables(tbls: Array[(String, String)]): Unit = {
    logger.info("Not Implemeted yet")
  }

  override def copyTable(srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    logger.info("Not Implemeted yet")
  }

  override def copyTable(namespace: String, srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    logger.info("Not Implemeted yet")
  }

  override def isTableExists(tableName: String): Boolean = {
    logger.info("Not Implemeted yet")
    false
  }

  override def isTableExists(tableNamespace: String, tableName: String): Boolean = {
    logger.info("Not Implemeted yet")
    false
  }
}

class HashMapAdapterTx(val parent: DataStore) extends Transaction {

  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)

  override def put(containerName: String, key: Key, value: Value): Unit = {
    parent.put(containerName, key, value)
  }

  override def put(data_list: Array[(String, Array[(Key, Value)])]): Unit = {
    parent.put(data_list)
  }

  // delete operations
  override def del(containerName: String, keys: Array[Key]): Unit = {
    parent.del(containerName, keys)
  }

  override def del(containerName: String, time: TimeRange, keys: Array[Array[String]]): Unit = {
    parent.del(containerName, time, keys)
  }

  // get operations
  override def get(containerName: String, callbackFunction: (Key, Value) => Unit): Unit = {
    parent.get(containerName, callbackFunction)
  }

  override def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit = {
    parent.get(containerName, keys, callbackFunction)
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key, Value) => Unit): Unit = {
    parent.get(containerName, time_ranges, callbackFunction)
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    parent.get(containerName, time_ranges, bucketKeys, callbackFunction)
  }
  override def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    parent.get(containerName, bucketKeys, callbackFunction)
  }

  def getKeys(containerName: String, callbackFunction: (Key) => Unit): Unit = {
    parent.getKeys(containerName, callbackFunction)
  }

  def getKeys(containerName: String, keys: Array[Key], callbackFunction: (Key) => Unit): Unit = {
    parent.getKeys(containerName, keys, callbackFunction)
  }
  def getKeys(containerName: String, timeRanges: Array[TimeRange], callbackFunction: (Key) => Unit): Unit = {
    parent.getKeys(containerName, timeRanges, callbackFunction)
  }

  def getKeys(containerName: String, timeRanges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    parent.getKeys(containerName, timeRanges, bucketKeys, callbackFunction)
  }

  def getKeys(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    parent.getKeys(containerName, bucketKeys, callbackFunction)
  }

  def backupContainer(containerName: String): Unit = {
    parent.backupContainer(containerName: String)
  }

  def restoreContainer(containerName: String): Unit = {
    parent.restoreContainer(containerName: String)
  }

  override def isContainerExists(containerName: String): Boolean = {
    parent.isContainerExists(containerName)
  }

  override def copyContainer(srcContainerName: String, destContainerName: String, forceCopy: Boolean): Unit = {
    parent.copyContainer(srcContainerName, destContainerName, forceCopy)
  }

  override def getAllTables: Array[String] = {
    parent.getAllTables
  }

  // Here tables are full qualified names
  override def dropTables(tbls: Array[String]): Unit = {
    parent.dropTables(tbls)
  }

  override def dropTables(tbls: Array[(String, String)]): Unit = {
    parent.dropTables(tbls)
  }

  // Here tables are full qualified names
  override def copyTable(srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    parent.copyTable(srcTableName, destTableName, forceCopy)
  }

  override def copyTable(namespace: String, srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    parent.copyTable(namespace, srcTableName, destTableName, forceCopy)
  }

  // Here table is full qualified name
  override def isTableExists(tableName: String): Boolean = {
    parent.isTableExists(tableName)
  }

  override def isTableExists(tableNamespace: String, tableName: String): Boolean = {
    parent.isTableExists(tableNamespace, tableName)
  }
}

// To create HashMap Datastore instance
object HashMapAdapter extends StorageAdapterObj {
  override def CreateStorageAdapter(kvManagerLoader: KamanjaLoaderInfo, datastoreConfig: String): DataStore = new HashMapAdapter(kvManagerLoader, datastoreConfig)
}
