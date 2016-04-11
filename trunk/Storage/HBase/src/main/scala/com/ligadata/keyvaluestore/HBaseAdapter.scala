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

package com.ligadata.keyvaluestore

// Hbase core
import org.apache.hadoop.hbase.util.Bytes
import org.apache.hadoop.hbase.HBaseConfiguration
import org.apache.hadoop.hbase.NamespaceDescriptor;
// hbase client
import org.apache.hadoop.hbase.client._
import org.apache.hadoop.hbase.HColumnDescriptor;
import org.apache.hadoop.hbase.HTableDescriptor;
import org.apache.hadoop.hbase.TableName;
// hbase filters
import org.apache.hadoop.hbase.filter.{ Filter, SingleColumnValueFilter, FirstKeyOnlyFilter, FilterList, CompareFilter }
import org.apache.hadoop.hbase.filter.CompareFilter.CompareOp
// hadoop security model
import org.apache.hadoop.security.UserGroupInformation

import org.apache.hadoop.hbase.io.compress.Compression.Algorithm;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.client.{ BufferedMutator, BufferedMutatorParams, Connection, ConnectionFactory }

import org.apache.hadoop.hbase._

import org.apache.logging.log4j._
import java.nio.ByteBuffer
import java.io.IOException
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.ligadata.Exceptions._
import com.ligadata.Utils.{ KamanjaLoaderInfo }
import com.ligadata.KvBase.{ Key, TimeRange }
import com.ligadata.StorageBase.{ DataStore, Transaction, StorageAdapterFactory, Value }
import java.util.{ Date, Calendar, TimeZone }
import java.text.SimpleDateFormat

import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions._

class HBaseAdapter(val kvManagerLoader: KamanjaLoaderInfo, val datastoreConfig: String) extends DataStore {
  val adapterConfig = if (datastoreConfig != null) datastoreConfig.trim else ""
  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)
  private[this] val lock = new Object
  private var containerList: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()
  private var isMetadataMap: scala.collection.mutable.Map[String, Boolean] = new scala.collection.mutable.HashMap()
  private var msg: String = ""

  private val stStrBytes = "serializerType".getBytes()
  private val siStrBytes = "serializedInfo".getBytes()
  private val schemaIdStrBytes = "schemaId".getBytes()
  private val baseStrBytes = "base".getBytes()
  private val isMetadataTableStrBytes = "isMetadataTable".getBytes()
  private val isMetadataTableName = "isMetadata"

  private def CreateConnectionException(msg: String, ie: Exception): StorageConnectionException = {
    logger.error(msg, ie)
    val ex = new StorageConnectionException("Failed to connect to Database", ie)
    ex
  }

  private def CreateDMLException(msg: String, ie: Exception): StorageDMLException = {
    logger.error(msg, ie)
    val ex = new StorageDMLException("Failed to execute select/insert/delete/update operation on Database", ie)
    ex
  }

  private def CreateDDLException(msg: String, ie: Exception): StorageDDLException = {
    logger.error(msg, ie)
    val ex = new StorageDDLException("Failed to execute create/drop operations on Database", ie)
    ex
  }

  if (adapterConfig.size == 0) {
    msg = "Invalid HBase Json Configuration string:" + adapterConfig
    throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
  }

  logger.debug("HBase configuration:" + adapterConfig)
  var parsed_json: Map[String, Any] = null
  try {
    val json = parse(adapterConfig)
    if (json == null || json.values == null) {
      var msg = "Failed to parse HBase JSON configuration string:" + adapterConfig
      throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
    }
    parsed_json = json.values.asInstanceOf[Map[String, Any]]
  } catch {
    case e: Exception => {
      var msg = "Failed to parse HBase JSON configuration string:%s.".format(adapterConfig)
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
          msg = "Failed to parse HBase JSON configuration string:" + adapterSpecificStr
          throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
        }
        adapterSpecificConfig_json = json.values.asInstanceOf[Map[String, Any]]
      } catch {
        case e: Exception => {
          msg = "Failed to parse HBase Adapter Specific JSON configuration string:%s.".format(adapterSpecificStr)
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

  def CreateNameSpace(nameSpace: String): Unit = {
    relogin
    try {
      val nsd = admin.getNamespaceDescriptor(nameSpace)
      return
    } catch {
      case e: Exception => {
        logger.info("Namespace " + nameSpace + " doesn't exist, create it", e)
      }
    }
    try {
      admin.createNamespace(NamespaceDescriptor.create(nameSpace).build)
    } catch {
      case e: Exception => {
        throw CreateConnectionException("Unable to create hbase name space " + nameSpace, e)
      }
    }
  }

  val hostnames = if (parsed_json.contains("hostlist")) parsed_json.getOrElse("hostlist", "localhost").toString.trim else parsed_json.getOrElse("Location", "localhost").toString.trim
  val namespace = if (parsed_json.contains("SchemaName")) parsed_json.getOrElse("SchemaName", "default").toString.trim else parsed_json.getOrElse("SchemaName", "default").toString.trim

  val config = HBaseConfiguration.create();

  config.setInt("zookeeper.session.timeout", getOptionalField("zookeeper_session_timeout", parsed_json, adapterSpecificConfig_json, "5000").toString.trim.toInt);
  config.setInt("zookeeper.recovery.retry", getOptionalField("zookeeper_recovery_retry", parsed_json, adapterSpecificConfig_json, "1").toString.trim.toInt);
  config.setInt("hbase.client.retries.number", getOptionalField("hbase_client_retries_number", parsed_json, adapterSpecificConfig_json, "3").toString.trim.toInt);
  config.setInt("hbase.client.pause", getOptionalField("hbase_client_pause", parsed_json, adapterSpecificConfig_json, "5000").toString.trim.toInt);
  config.set("hbase.zookeeper.quorum", hostnames);

  val keyMaxSz = getOptionalField("hbase_client_keyvalue_maxsize", parsed_json, adapterSpecificConfig_json, "104857600").toString.trim.toInt
  var clntWrtBufSz = getOptionalField("hbase_client_write_buffer", parsed_json, adapterSpecificConfig_json, "104857600").toString.trim.toInt

  if (clntWrtBufSz < keyMaxSz)
    clntWrtBufSz = keyMaxSz + 1024 // 1K Extra

  config.setInt("hbase.client.keyvalue.maxsize", keyMaxSz);
  config.setInt("hbase.client.write.buffer", clntWrtBufSz);

  var isKerberos: Boolean = false
  var ugi: UserGroupInformation = null

  val auth = getOptionalField("authentication", parsed_json, adapterSpecificConfig_json, "").toString.trim
  if (auth.size > 0) {
    isKerberos = auth.compareToIgnoreCase("kerberos") == 0
    if (isKerberos) {
      try {
        val regionserver_principal = getOptionalField("regionserver_principal", parsed_json, adapterSpecificConfig_json, "").toString.trim
        val master_principal = getOptionalField("master_principal", parsed_json, adapterSpecificConfig_json, "").toString.trim
        val principal = getOptionalField("principal", parsed_json, adapterSpecificConfig_json, "").toString.trim
        val keytab = getOptionalField("keytab", parsed_json, adapterSpecificConfig_json, "").toString.trim

        logger.debug("HBase info => Hosts:" + hostnames + ", Namespace:" + namespace + ", Principal:" + principal + ", Keytab:" + keytab + ", hbase.regionserver.kerberos.principal:" + regionserver_principal + ", hbase.master.kerberos.principal:" + master_principal)

        config.set("hadoop.proxyuser.hdfs.groups", "*")
        config.set("hadoop.security.authorization", "true")
        config.set("hbase.security.authentication", "kerberos")
        config.set("hadoop.security.authentication", "kerberos")
        config.set("hbase.regionserver.kerberos.principal", regionserver_principal)
        config.set("hbase.master.kerberos.principal", master_principal)

        org.apache.hadoop.security.UserGroupInformation.setConfiguration(config);

        UserGroupInformation.loginUserFromKeytab(principal, keytab);

        ugi = UserGroupInformation.getLoginUser
      } catch {
        case e: Exception => {
          throw CreateConnectionException("HBase issue from JSON configuration string:%s.".format(adapterConfig), e)
        }
      }
    } else {
      throw CreateConnectionException("Not handling any authentication other than KERBEROS. AdapterSpecificConfig:" + adapterConfig, new Exception("Authentication Exception"))
    }
  }

  var autoCreateTables = "YES"
  if (parsed_json.contains("autoCreateTables")) {
    autoCreateTables = parsed_json.get("autoCreateTables").get.toString.trim
  }

  logger.info("HBase info => Hosts:" + hostnames + ", Namespace:" + namespace + ",autoCreateTables:" + autoCreateTables)

  var conn: Connection = _
  try {
    conn = ConnectionFactory.createConnection(config);
  } catch {
    case e: Exception => {
      throw CreateConnectionException("Unable to connect to hbase at " + hostnames, e)
    }
  }
  val admin = new HBaseAdmin(config);
  CreateNameSpace(namespace)

  private def relogin: Unit = {
    try {
      if (ugi != null)
        ugi.checkTGTAndReloginFromKeytab
    } catch {
      case e: Exception => {
        logger.error("Failed to relogin into HBase.", e)
        // Not throwing exception from here
      }
    }
  }

  private def createTableFromDescriptor(tableDesc: HTableDescriptor): Unit = {
    try {
      relogin
      admin.createTable(tableDesc);
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to create table", e)
      }
    }
  }

  private def createIsMetadataTable: Unit = {
    var tableName = toFullTableName(isMetadataTableName) // prepend namespace
    try {
      relogin
      if (!admin.tableExists(tableName)) {
        val tableDesc = new HTableDescriptor(TableName.valueOf(tableName));
        val colDesc1 = new HColumnDescriptor("key".getBytes())
        val colDesc2 = new HColumnDescriptor(isMetadataTableStrBytes)
        tableDesc.addFamily(colDesc1)
        tableDesc.addFamily(colDesc2)
        createTableFromDescriptor(tableDesc)
      }
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to create table " + tableName, e)
      }
    }
  }

  private def putIsMetadataFlag(containerName: String, isMetadataContainer: Boolean): Unit = {
    var tableName = toFullTableName(isMetadataTableName) // prepend namespace
    var tableHBase: Table = null
    try {
      relogin
      tableHBase = getTableFromConnection(tableName);
      var kba = containerName.getBytes()
      var p = new Put(kba)
      var yesOrNo = if ( isMetadataContainer ) "yes" else "no"
      p.addColumn(isMetadataTableStrBytes, baseStrBytes, Bytes.toBytes(yesOrNo))
      tableHBase.put(p)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save an object in table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  private def getIsMetadataFlag(containerName: String): Boolean = {
    var tableName = toFullTableName(isMetadataTableName) // prepend namespace
    var tableHBase: Table = null
    try {
      relogin
      if (admin.tableExists(tableName)) {
	tableHBase = getTableFromConnection(tableName);
	val g = new Get(containerName.getBytes())
	val r = tableHBase.get(g);
	//val kv : r.raw()
	if( r.isEmpty() ){
	  return false
	}
	else{
	  val v = r.getValue(isMetadataTableStrBytes,baseStrBytes)
	  val b = if (Bytes.toString(v).equalsIgnoreCase("yes")) true else false
	  b
	}
      }
      else{
	return false
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to get an object from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  private def createTable(containerName:String, tableName: String, isMetadata: Boolean, apiType: String): Unit = {
    try {
      relogin
      if (!admin.tableExists(tableName)) {
        if (autoCreateTables.equalsIgnoreCase("NO")) {
          apiType match {
            case "dml" => {
              throw new Exception("The option autoCreateTables is set to NO, So Can't create non-existent table automatically to support the requested DML operation")
            }
            case _ => {
              logger.info("proceed with creating table..")
            }
          }
        }
        createIsMetadataTable
	putIsMetadataFlag(containerName,isMetadata)
        val tableDesc = new HTableDescriptor(TableName.valueOf(tableName));
        val colDesc1 = new HColumnDescriptor("key".getBytes())
        val colDesc2 = new HColumnDescriptor(stStrBytes)
        val colDesc3 = new HColumnDescriptor(siStrBytes)
        val colDesc4 = new HColumnDescriptor(schemaIdStrBytes)
        tableDesc.addFamily(colDesc1)
        tableDesc.addFamily(colDesc2)
        tableDesc.addFamily(colDesc3)
        tableDesc.addFamily(colDesc4)
        createTableFromDescriptor(tableDesc)
	// update local cache
        containerList.add(containerName)
	isMetadataMap(containerName) = isMetadata
      }
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to create table " + tableName, e)
      }
    }
  }

  private def dropTable(tableName: String): Unit = {
    try {
      relogin
      if (admin.tableExists(tableName)) {
        if (admin.isTableEnabled(tableName)) {
          admin.disableTable(tableName)
        }
        admin.deleteTable(tableName)
      }
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to drop table " + tableName, e)
      }
    }
  }

  private def CheckTableExists(containerName: String, apiType: String = "dml"): Boolean = {
    try {
      if (containerList.contains(containerName)) {
        return isMetadataMap(containerName)
      } else {
        val isMetadata = getIsMetadataFlag(containerName)
	var fullTableName = toFullTableName(containerName)
	if ( ! admin.tableExists(fullTableName)) {
	  createTable(containerName,fullTableName,isMetadata,apiType)
	}
        containerList.add(containerName)
	isMetadataMap(containerName) = isMetadata
	isMetadata
      }
    } catch {
      case e: Exception => {
        throw new Exception("Failed to create table  " + toTableName(containerName), e)
      }
    }
  }

  def DropNameSpace(namespace: String): Unit = lock.synchronized {
    logger.info("Drop namespace " + namespace)
    relogin
    try {
      logger.info("Check whether namespace exists " + namespace)
      val nsd = admin.getNamespaceDescriptor(namespace)
    } catch {
      case e: Exception => {
        logger.info("Namespace " + namespace + " doesn't exist, nothing to delete", e)
        return
      }
    }
    try {
      logger.info("delete namespace: " + namespace)
      admin.deleteNamespace(namespace)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Unable to delete hbase name space " + namespace, e)
      }
    }
  }

  def toTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    namespace + ':' + containerName.toLowerCase.replace('.', '_').replace('-', '_').replace(' ', '_')
  }

  private def toFullTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    toTableName(containerName)
  }

  // accessor used for testing
  def getTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    toTableName(containerName)
  }

  private def CreateContainer(containerName: String,isMetadata: Boolean, apiType: String): Unit = lock.synchronized {
    var tableName = toTableName(containerName)
    var fullTableName = toFullTableName(containerName)
    try {
      createTable(containerName,fullTableName,isMetadata,apiType)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  override def CreateContainer(containerNames: Array[String]): Unit = {
    logger.info("create the container tables")
    containerNames.foreach(cont => {
      logger.info("create the container " + cont)
      CreateContainer(cont,false,"ddl")
    })
  }

  override def CreateMetadataContainer(containerNames: Array[String]): Unit = {
    logger.info("create the container tables")
    containerNames.foreach(cont => {
      logger.info("create the container " + cont)
      CreateContainer(cont,true,"ddl")
    })
  }

  private def AddBucketKeyToArrayBuffer(bucketKey: Array[String], ab: ArrayBuffer[Byte]): Unit = {
    // First one is Number of Array Elements
    // Next follows Each Element size & Element Data
    ab += ((bucketKey.size).toByte)
    bucketKey.foreach(k => {
      val kBytes = k.getBytes
      val sz = kBytes.size
      ab += (((sz >>> 8) & 0xFF).toByte)
      ab += (((sz >>> 0) & 0xFF).toByte)
      ab ++= kBytes
    })
  }

  private def MakeBucketKeyToByteArray(bucketKey: Array[String]): Array[Byte] = {
    // First one is Number of Array Elements
    // Next follows Each Element size & Element Data
    val ab = new ArrayBuffer[Byte](128)
    AddBucketKeyToArrayBuffer(bucketKey, ab)
    ab.toArray
  }

  private def MakeBucketKeyFromByteArr(keyBytes: Array[Byte], startIdx: Int): (Array[String], Int) = {
    if (keyBytes.size > startIdx) {
      var cntr = startIdx
      val cnt = (0xff & keyBytes(cntr).toInt)
      cntr += 1

      val bucketKey = new Array[String](cnt)
      for (i <- 0 until cnt) {
        val b1 = keyBytes(cntr)
        cntr += 1
        val b2 = keyBytes(cntr)
        cntr += 1

        val sz = ((0xff & b1.asInstanceOf[Int]) << 8) + ((0xff & b2.asInstanceOf[Int]) << 0)
        bucketKey(i) = new String(keyBytes, cntr, sz)
        cntr += sz
      }

      (bucketKey, (cntr - startIdx))
    } else {
      (Array[String](), 0)
    }
  }

  private def MakeBucketKeyFromByteArr(keyBytes: Array[Byte]): Array[String] = {
    val (bucketKey, consumedBytes) = MakeBucketKeyFromByteArr(keyBytes, 0)
    bucketKey
  }

  private def MakeLongSerializedVal(l: Long): Array[Byte] = {
    val ab = new ArrayBuffer[Byte](16)
    ab += (((l >>> 56) & 0xFF).toByte)
    ab += (((l >>> 48) & 0xFF).toByte)
    ab += (((l >>> 40) & 0xFF).toByte)
    ab += (((l >>> 32) & 0xFF).toByte)
    ab += (((l >>> 24) & 0xFF).toByte)
    ab += (((l >>> 16) & 0xFF).toByte)
    ab += (((l >>> 8) & 0xFF).toByte)
    ab += (((l >>> 0) & 0xFF).toByte)

    ab.toArray
  }

  private def MakeCompositeKey(key: Key,isMetadataContainer: Boolean): Array[Byte] = {
    if( isMetadataContainer ){
      key.bucketKey(0).getBytes
    }
    else{
      val ab = new ArrayBuffer[Byte](256)
      ab += (((key.timePartition >>> 56) & 0xFF).toByte)
      ab += (((key.timePartition >>> 48) & 0xFF).toByte)
      ab += (((key.timePartition >>> 40) & 0xFF).toByte)
      ab += (((key.timePartition >>> 32) & 0xFF).toByte)
      ab += (((key.timePartition >>> 24) & 0xFF).toByte)
      ab += (((key.timePartition >>> 16) & 0xFF).toByte)
      ab += (((key.timePartition >>> 8) & 0xFF).toByte)
      ab += (((key.timePartition >>> 0) & 0xFF).toByte)

      AddBucketKeyToArrayBuffer(key.bucketKey, ab)

      ab += (((key.transactionId >>> 56) & 0xFF).toByte)
      ab += (((key.transactionId >>> 48) & 0xFF).toByte)
      ab += (((key.transactionId >>> 40) & 0xFF).toByte)
      ab += (((key.transactionId >>> 32) & 0xFF).toByte)
      ab += (((key.transactionId >>> 24) & 0xFF).toByte)
      ab += (((key.transactionId >>> 16) & 0xFF).toByte)
      ab += (((key.transactionId >>> 8) & 0xFF).toByte)
      ab += (((key.transactionId >>> 0) & 0xFF).toByte)

      ab += (((key.rowId >>> 24) & 0xFF).toByte)
      ab += (((key.rowId >>> 16) & 0xFF).toByte)
      ab += (((key.rowId >>> 8) & 0xFF).toByte)
      ab += (((key.rowId >>> 0) & 0xFF).toByte)

      ab.toArray
    }
  }

  private def GetKeyFromCompositeKey(compKey: Array[Byte], isMetadataContainer: Boolean): Key = {
    if( isMetadataContainer ){
      var cntr = 0
      val k = new String(compKey)
      val bucketKey = new Array[String](1)
      bucketKey(0) = k
      new Key(0, bucketKey, 0, 0)
    }
    else{
      var cntr = 0
      val tp_b1 = compKey(cntr)
      cntr += 1
      val tp_b2 = compKey(cntr)
      cntr += 1
      val tp_b3 = compKey(cntr)
      cntr += 1
      val tp_b4 = compKey(cntr)
      cntr += 1
      val tp_b5 = compKey(cntr)
      cntr += 1
      val tp_b6 = compKey(cntr)
      cntr += 1
      val tp_b7 = compKey(cntr)
      cntr += 1
      val tp_b8 = compKey(cntr)
      cntr += 1

      val timePartition =
      (((0xff & tp_b1.asInstanceOf[Long]) << 56) + ((0xff & tp_b2.asInstanceOf[Long]) << 48) +
       ((0xff & tp_b3.asInstanceOf[Long]) << 40) + ((0xff & tp_b4.asInstanceOf[Long]) << 32) +
       ((0xff & tp_b5.asInstanceOf[Long]) << 24) + ((0xff & tp_b6.asInstanceOf[Long]) << 16) +
       ((0xff & tp_b7.asInstanceOf[Long]) << 8) + ((0xff & tp_b8.asInstanceOf[Long]) << 0))

      val (bucketKey, consumedBytes) = MakeBucketKeyFromByteArr(compKey, cntr)
      cntr += consumedBytes

      val tx_b1 = compKey(cntr)
      cntr += 1
      val tx_b2 = compKey(cntr)
      cntr += 1
      val tx_b3 = compKey(cntr)
      cntr += 1
      val tx_b4 = compKey(cntr)
      cntr += 1
      val tx_b5 = compKey(cntr)
      cntr += 1
      val tx_b6 = compKey(cntr)
      cntr += 1
      val tx_b7 = compKey(cntr)
      cntr += 1
      val tx_b8 = compKey(cntr)
      cntr += 1

      val transactionId =
      (((0xff & tx_b1.asInstanceOf[Long]) << 56) + ((0xff & tx_b2.asInstanceOf[Long]) << 48) +
       ((0xff & tx_b3.asInstanceOf[Long]) << 40) + ((0xff & tx_b4.asInstanceOf[Long]) << 32) +
       ((0xff & tx_b5.asInstanceOf[Long]) << 24) + ((0xff & tx_b6.asInstanceOf[Long]) << 16) +
       ((0xff & tx_b7.asInstanceOf[Long]) << 8) + ((0xff & tx_b8.asInstanceOf[Long]) << 0))

      val rowid_b1 = compKey(cntr)
      cntr += 1
      val rowid_b2 = compKey(cntr)
      cntr += 1
      val rowid_b3 = compKey(cntr)
      cntr += 1
      val rowid_b4 = compKey(cntr)
      cntr += 1

      val rowId =
      (((0xff & rowid_b1.asInstanceOf[Int]) << 24) + ((0xff & rowid_b2.asInstanceOf[Int]) << 16) +
       ((0xff & rowid_b3.asInstanceOf[Int]) << 8) + ((0xff & rowid_b4.asInstanceOf[Int]) << 0))

      new Key(timePartition, bucketKey, transactionId, rowId)
    }
  }

  private def getTableFromConnection(tableName: String): Table = {
    try {
      relogin
      return conn.getTable(TableName.valueOf(tableName))
    } catch {
      case e: Exception => {
        throw ConnectionFailedException("Failed to get table " + tableName, e)
      }
    }

    return null
  }

  override def put(containerName: String, key: Key, value: Value): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var kba = MakeCompositeKey(key,isMetadata)
      var p = new Put(kba)
      p.addColumn(stStrBytes, baseStrBytes, Bytes.toBytes(value.serializerType))
      p.addColumn(siStrBytes, baseStrBytes, value.serializedInfo)
      p.addColumn(schemaIdStrBytes, baseStrBytes, Bytes.toBytes(value.schemaId))
      tableHBase.put(p)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save an object in table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def put(data_list: Array[(String, Array[(Key, Value)])]): Unit = {
    var tableHBase: Table = null
    try {
      relogin
      data_list.foreach(li => {
        var containerName = li._1
        val isMetadata = CheckTableExists(containerName)
        var tableName = toFullTableName(containerName)
        tableHBase = getTableFromConnection(tableName);
        var keyValuePairs = li._2
        var puts = new Array[Put](0)
        keyValuePairs.foreach(keyValuePair => {
          var key = keyValuePair._1
          var value = keyValuePair._2
          var kba = MakeCompositeKey(key,isMetadata)
          var p = new Put(kba)
          p.addColumn(stStrBytes, baseStrBytes, Bytes.toBytes(value.serializerType))
          p.addColumn(siStrBytes, baseStrBytes, value.serializedInfo)
          p.addColumn(schemaIdStrBytes, baseStrBytes, Bytes.toBytes(value.schemaId))
          puts = puts :+ p
        })
        try {
          if (puts.length > 0) {
            tableHBase.put(puts.toList)
          }
        } catch {
          case e: Exception => {
            throw CreateDMLException("Failed to save an object in table " + tableName, e)
          }
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save a list of objects in table(s) ", e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  // delete operations
  override def del(containerName: String, keys: Array[Key]): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var dels = new ArrayBuffer[Delete]()

      keys.foreach(key => {
        var kba = MakeCompositeKey(key,isMetadata)
        dels += new Delete(kba)
      })

      if (dels.length > 0) {
        // callling tableHBase.delete(dels.toList) results in an exception as below ??
        //  Stacktrace:java.lang.UnsupportedOperationException
        // at java.util.AbstractList.remove(AbstractList.java:161)
        // at org.apache.hadoop.hbase.client.HTable.delete(HTable.java:896)
        // at com.ligadata.keyvaluestore.HBaseAdapter.del(HBaseAdapter.scala:387)
        tableHBase.delete(new java.util.ArrayList(dels.toList)) // callling tableHBase.delete(dels.toList) results java.lang.UnsupportedOperationException
      } else {
        logger.info("No rows found for the delete operation")
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  class ArrayOfStringsComp extends java.util.Comparator[Array[String]] {
    override def compare(k1: Array[String], k2: Array[String]): Int = {
      if (k1 == null && k2 == null)
        return 0

      if (k1 != null && k2 == null)
        return 1

      if (k1 == null && k2 != null)
        return -1

      // Next compare Bucket Keys
      if (k1.size < k2.size)
        return -1
      if (k1.size > k2.size)
        return 1

      for (i <- 0 until k1.size) {
        val cmp = k1(i).compareTo(k2(i))
        if (cmp != 0)
          return cmp
      }
      return 0
    }
  }

  val arrOfStrsComp = new ArrayOfStringsComp()

  private def getUnsignedTimeRanges(timeRanges: Array[TimeRange]): Array[TimeRange] = {
    if (timeRanges == null || timeRanges.size == 0) return Array[TimeRange]()

    val arrTimeRanges = ArrayBuffer[TimeRange]()

    // Assuming each input time range is tr.beginTime <= tr.endTime in Signed comparision
    timeRanges.foreach(tr => {
      if (tr.beginTime >= 0 && tr.endTime >= 0) {
        // Nothing special
        arrTimeRanges += tr
      } else if (tr.beginTime < 0 && tr.endTime >= 0) {
        // Split this into two time ranges. First one is from (0 - tr.endTime) and second one from (-1 to tr.beginTime). more -ve is Bigger value in Unsigned
        arrTimeRanges += TimeRange(0, tr.endTime)
        arrTimeRanges += TimeRange(-1, tr.beginTime)
      } else { // Both tr.beginTime < 0 && tr.endTime < 0
        // Now reverse the time ranges
        arrTimeRanges += TimeRange(tr.endTime, tr.beginTime)
      }
    })

    arrTimeRanges.toArray

  }

  override def del(containerName: String, time: TimeRange, bucketKeys: Array[Array[String]]): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      val bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)
      bucketKeys.foreach(bucketKey => {
        bucketKeySet.add(bucketKey)
      })

      // try scan with beginRow and endRow
      logger.info("beginTime => " + time.beginTime)
      logger.info("endTime => " + time.endTime)

      var dels = new ArrayBuffer[Delete]()

      val tmRanges = getUnsignedTimeRanges(Array(time))
      tmRanges.foreach(tr => {
        bucketKeys.foreach(bucketKey => {
          var scan = new Scan()
          scan.setStartRow(MakeCompositeKey(new Key(tr.beginTime, bucketKey, 0, 0),isMetadata))
          scan.setStopRow(MakeCompositeKey(new Key(tr.endTime, bucketKey, Long.MaxValue, Int.MaxValue),isMetadata))
          val rs = tableHBase.getScanner(scan);
          val it = rs.iterator()
          while (it.hasNext()) {
            val r = it.next()
            var key = GetKeyFromCompositeKey(r.getRow(),isMetadata)
            logger.info("searching for " + key.bucketKey.mkString(","))
            if (bucketKeySet.contains(key.bucketKey)) {
              dels += new Delete(r.getRow())
            }
          }
        })
      })
      if (dels.length > 0) {
        tableHBase.delete(new java.util.ArrayList(dels.toList)) // callling tableHBase.delete(dels.toList) results java.lang.UnsupportedOperationException
      } else {
        logger.info("No rows found for the delete operation")
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  //Added by Yousef Abu Elbeh in 2016-03-13 from here
  override def del(containerName: String, time: TimeRange): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      val bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)

      // try scan with beginRow and endRow
      logger.info("beginTime => " + time.beginTime)
      logger.info("endTime => " + time.endTime)

      var dels = new ArrayBuffer[Delete]()

      val tmRanges = getUnsignedTimeRanges(Array(time))
      tmRanges.foreach(tr => {
          var scan = new Scan()
        scan.setTimeRange(tr.beginTime,tr.endTime)
          val rs = tableHBase.getScanner(scan);
          val it = rs.iterator()
          while (it.hasNext()) {
            val r = it.next()
            logger.info("searching for data in timerange: " + tr.beginTime +"-"+ tr.endTime)
              dels += new Delete(r.getRow())
            }
          })
      if (dels.length > 0) {
        tableHBase.delete(new java.util.ArrayList(dels.toList)) // callling tableHBase.delete(dels.toList) results java.lang.UnsupportedOperationException
      } else {
        logger.info("No rows found for the delete operation")
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }
  // to here

  // get operations
  def getRowCount(containerName: String): Long = {
    var tableHBase: Table = null
    try {
      relogin
      var tableName = toFullTableName(containerName)
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      var scan = new Scan();
      scan.setFilter(new FirstKeyOnlyFilter());
      var rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      var cnt = 0
      while (it.hasNext()) {
        var r = it.next()
        cnt = cnt + 1
      }
      return cnt
    } catch {
      case e: Exception => {
        throw e
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  private def processRow(k: Array[Byte], isMetadata: Boolean, schemaId: Int, st: String, si: Array[Byte], callbackFunction: (Key, Value) => Unit) {
    try {
      var key = GetKeyFromCompositeKey(k,isMetadata)
      // format the data to create Key/Value
      var value = new Value(schemaId, st, si)
      if (callbackFunction != null)
        (callbackFunction)(key, value)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  private def processRow(key: Key, schemaId: Int, st: String, si: Array[Byte], callbackFunction: (Key, Value) => Unit) {
    try {
      var value = new Value(schemaId, st, si)
      if (callbackFunction != null)
        (callbackFunction)(key, value)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  private def processKey(k: Array[Byte], isMetadata: Boolean, callbackFunction: (Key) => Unit) {
    try {
      var key = GetKeyFromCompositeKey(k,isMetadata)
      if (callbackFunction != null)
        (callbackFunction)(key)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  private def processKey(key: Key, callbackFunction: (Key) => Unit) {
    try {
      if (callbackFunction != null)
        (callbackFunction)(key)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  override def get(containerName: String, callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var scan = new Scan();
      var rs = tableHBase.getScanner(scan);

      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        val st = Bytes.toString(r.getValue(stStrBytes, baseStrBytes))
        val si = r.getValue(siStrBytes, baseStrBytes)
        val schemaId = Bytes.toInt(r.getValue(schemaIdStrBytes, baseStrBytes))
        processRow(r.getRow(), isMetadata, schemaId, st, si, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def getKeys(containerName: String, callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var scan = new Scan();
      scan.setFilter(new FirstKeyOnlyFilter());
      var rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        processKey(r.getRow(), isMetadata, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def getKeys(containerName: String, keys: Array[Key], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      val filters = new java.util.ArrayList[Filter]()
      keys.foreach(key => {
        var kba = MakeCompositeKey(key,isMetadata)
        val f = new SingleColumnValueFilter(Bytes.toBytes("key"), baseStrBytes,
          CompareOp.EQUAL, kba)
        filters.add(f);
      })
      val fl = new FilterList(filters);
      val scan = new Scan();
      scan.setFilter(fl);
      val rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        processKey(r.getRow(),isMetadata, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      val filters = new java.util.ArrayList[Filter]()
      keys.foreach(key => {
        var kba = MakeCompositeKey(key,isMetadata)
        val f = new SingleColumnValueFilter(Bytes.toBytes("key"), baseStrBytes,
          CompareOp.EQUAL, kba)
        filters.add(f);
      })
      val fl = new FilterList(filters);
      val scan = new Scan();
      scan.setFilter(fl);
      val rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        val st = Bytes.toString(r.getValue(stStrBytes, baseStrBytes))
        val si = r.getValue(siStrBytes, baseStrBytes)
        val schemaId = Bytes.toInt(r.getValue(schemaIdStrBytes, baseStrBytes))
        processRow(r.getRow(),isMetadata, schemaId, st, si, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      val tmRanges = getUnsignedTimeRanges(time_ranges)
      tmRanges.foreach(time_range => {
        // try scan with beginRow and endRow
        var scan = new Scan()
        scan.setStartRow(MakeLongSerializedVal(time_range.beginTime))
        scan.setStopRow(MakeLongSerializedVal(time_range.endTime + 1))
        val rs = tableHBase.getScanner(scan);
        val it = rs.iterator()
        while (it.hasNext()) {
          val r = it.next()
          val st = Bytes.toString(r.getValue(stStrBytes, baseStrBytes))
          val si = r.getValue(siStrBytes, baseStrBytes)
          val schemaId = Bytes.toInt(r.getValue(schemaIdStrBytes, baseStrBytes))
          processRow(r.getRow(), isMetadata, schemaId, st, si, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      val tmRanges = getUnsignedTimeRanges(time_ranges)
      tmRanges.foreach(time_range => {
        // try scan with beginRow and endRow
        var scan = new Scan()
        scan.setStartRow(MakeLongSerializedVal(time_range.beginTime))
        scan.setStopRow(MakeLongSerializedVal(time_range.endTime + 1))
        val rs = tableHBase.getScanner(scan);
        val it = rs.iterator()
        while (it.hasNext()) {
          val r = it.next()
          processKey(r.getRow(),isMetadata, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]],
                   callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)
      bucketKeys.foreach(bucketKey => {
        bucketKeySet.add(bucketKey)
      })
      val tmRanges = getUnsignedTimeRanges(time_ranges)
      tmRanges.foreach(time_range => {
        // try scan with beginRow and endRow
        bucketKeys.foreach(bucketKey => {
          var scan = new Scan()
          scan.setStartRow(MakeCompositeKey(new Key(time_range.beginTime, bucketKey, 0, 0),isMetadata))
          scan.setStopRow(MakeCompositeKey(new Key(time_range.endTime, bucketKey, Long.MaxValue, Int.MaxValue),isMetadata))
          val rs = tableHBase.getScanner(scan);
          val it = rs.iterator()
          while (it.hasNext()) {
            val r = it.next()
            var key = GetKeyFromCompositeKey(r.getRow(),false)
            if (bucketKeySet.contains(key.bucketKey)) {
              val st = Bytes.toString(r.getValue(stStrBytes, baseStrBytes))
              val si = r.getValue(siStrBytes, baseStrBytes)
              val schemaId = Bytes.toInt(r.getValue(schemaIdStrBytes, baseStrBytes))
              processRow(key, schemaId, st, si, callbackFunction)
            }
          }
        })
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]],
                       callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);

      var bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)
      bucketKeys.foreach(bucketKey => {
        bucketKeySet.add(bucketKey)
      })

      val tmRanges = getUnsignedTimeRanges(time_ranges)
      tmRanges.foreach(time_range => {
        // try scan with beginRow and endRow
        bucketKeys.foreach(bucketKey => {
          var scan = new Scan()
          scan.setStartRow(MakeCompositeKey(new Key(time_range.beginTime, bucketKey, 0, 0),isMetadata))
          scan.setStopRow(MakeCompositeKey(new Key(time_range.endTime, bucketKey, Long.MaxValue, Int.MaxValue),isMetadata))
          val rs = tableHBase.getScanner(scan);
          val it = rs.iterator()
          while (it.hasNext()) {
            val r = it.next()
            var key = GetKeyFromCompositeKey(r.getRow(),false)
            if (bucketKeySet.contains(key.bucketKey)) {
              processKey(key, callbackFunction)
            }
          }
        })
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)
      bucketKeys.foreach(bucketKey => {
        bucketKeySet.add(bucketKey)
      })

      // try scan with beginRow and endRow
      var scan = new Scan()
      val rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      var dels = new Array[Delete](0)
      while (it.hasNext()) {
        val r = it.next()
        var key = GetKeyFromCompositeKey(r.getRow(),false)
        if (bucketKeySet.contains(key.bucketKey)) {
          val st = Bytes.toString(r.getValue(stStrBytes, baseStrBytes))
          val si = r.getValue(siStrBytes, baseStrBytes)
          val schemaId = Bytes.toInt(r.getValue(schemaIdStrBytes, baseStrBytes))
          processRow(key, schemaId, st, si, callbackFunction)
        }
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def getKeys(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var bucketKeySet = new java.util.TreeSet[Array[String]](arrOfStrsComp)
      bucketKeys.foreach(bucketKey => {
        bucketKeySet.add(bucketKey)
      })
      // scan the whole table
      var scan = new Scan()
      val rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        var key = GetKeyFromCompositeKey(r.getRow(),false)
        if (bucketKeySet.contains(key.bucketKey)) {
          processKey(key, callbackFunction)
        }
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def beginTx(): Transaction = {
    new HBaseAdapterTx(this)
  }

  override def endTx(tx: Transaction): Unit = {}

  override def commitTx(tx: Transaction): Unit = {}

  override def rollbackTx(tx: Transaction): Unit = {}

  override def Shutdown(): Unit = {
    logger.info("close the session and connection pool")
    if (conn != null) {
      conn.close()
      conn = null
    }
  }

  private def TruncateContainer(containerName: String): Unit = {
    var tableName = toFullTableName(containerName)
    var tableHBase: Table = null
    try {
      relogin
      val isMetadata = CheckTableExists(containerName)
      tableHBase = getTableFromConnection(tableName);
      var dels = new ArrayBuffer[Delete]()
      var scan = new Scan()
      val rs = tableHBase.getScanner(scan);
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        dels += new Delete(r.getRow())
      }
      if (dels.size > 0)
        tableHBase.delete(new java.util.ArrayList(dels.toList)) // callling tableHBase.delete(dels.toList) results java.lang.UnsupportedOperationException
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to truncate the table " + tableName, e)
      }
    } finally {
      if (tableHBase != null) {
        tableHBase.close()
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
    var fullTableName = toFullTableName(containerName)
    try {
      relogin
      dropTable(fullTableName)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to drop the table " + fullTableName, e)
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

  def renameTable(srcTableName: String, destTableName: String, forceCopy: Boolean = false): Unit = {
    val listener = new BufferedMutator.ExceptionListener() {
      override def onException(e: RetriesExhaustedWithDetailsException, mutator: BufferedMutator) {
        for (i <- 0 until e.getNumExceptions)
          logger.error("Failed to sent put: " + e.getRow(i))
        throw CreateDMLException("Failed to rename the table " + srcTableName, e)
      }
    }

    var tableHBase: Table = null
    var mutator: BufferedMutator = null

    try {
      relogin
      if (!admin.tableExists(srcTableName)) {
        logger.warn("The table being renamed doesn't exist, nothing to be done")
        throw CreateDDLException("Failed to rename the table " + srcTableName + ":", new Exception("Source Table doesn't exist"))
      }
      if (admin.tableExists(destTableName)) {
        if (forceCopy) {
          dropTable(destTableName);
        } else {
          logger.warn("A Destination table already exist, nothing to be done")
          throw CreateDDLException("Failed to rename the table " + srcTableName + ":", new Exception("Destination Table already exist"))
        }
      }

      // Open Source Table
      tableHBase = getTableFromConnection(srcTableName);

      val destTableDesc = new HTableDescriptor(TableName.valueOf(destTableName));
      val srcTableDesc = tableHBase.getTableDescriptor
      destTableDesc.setMaxFileSize(srcTableDesc.getMaxFileSize());
      destTableDesc.setMemStoreFlushSize(srcTableDesc.getMemStoreFlushSize());
      destTableDesc.setReadOnly(srcTableDesc.isReadOnly());
      for (desc <- srcTableDesc.getColumnFamilies) {
        logger.debug(srcTableName + " ColumnFamilyDescription info:" + desc.getNameAsString + ", " + desc.toStringCustomizedValues())
        destTableDesc.addFamily(desc)
      }

      createTableFromDescriptor(destTableDesc)

      val params = new BufferedMutatorParams(TableName.valueOf(destTableName)).listener(listener);

      // Create Mutator for Destination Table
      mutator = conn.getBufferedMutator(params)

      // Scan source table
      var scan = new Scan();
      scan.setMaxVersions();
      scan.setBatch(1024);
      var rs = tableHBase.getScanner(scan);

      // Loop through each row and write it into destination table mutator
      val it = rs.iterator()
      while (it.hasNext()) {
        val r = it.next()
        var p = new Put(r.getRow)
        val rc = r.rawCells()
        if (rc != null) {
          for (kv <- rc) {
            if (CellUtil.isDeleteFamily(kv)) {
            } else if (CellUtil.isDelete(kv)) {
            } else {
              p.add(kv);
            }
          }
        }
        mutator.mutate(p)
      }

    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to rename the table " + srcTableName, e)
      }
    } finally {
      if (mutator != null) {
        mutator.flush()
        mutator.close()
      }
      if (tableHBase != null) {
        tableHBase.close()
      }
    }
  }

  override def isContainerExists(containerName: String): Boolean = {
    relogin
    var tableName = toFullTableName(containerName)
    admin.tableExists(tableName)
  }

  override def copyContainer(srcContainerName: String, destContainerName: String, forceCopy: Boolean): Unit = lock.synchronized {
    if (srcContainerName.equalsIgnoreCase(destContainerName)) {
      throw CreateDDLException("Failed to copy the container " + srcContainerName, new Exception("Source Container Name can't be same as destination container name"))
    }
    var oldTableName = toFullTableName(srcContainerName)
    var newTableName = toFullTableName(destContainerName)
    logger.info("renaming " + oldTableName + " to " + newTableName);
    try {
      relogin
      renameTable(oldTableName, newTableName, forceCopy)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to copy the container " + srcContainerName, e)
      }
    }
  }

  override def backupContainer(containerName: String): Unit = lock.synchronized {
    var oldTableName = toFullTableName(containerName)
    var newTableName = toFullTableName(containerName) + ".bak"
    logger.info("renaming " + oldTableName + " to " + newTableName);
    try {
      relogin
      renameTable(oldTableName, newTableName)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to backup the container " + containerName, e)
      }
    }
  }

  override def restoreContainer(containerName: String): Unit = lock.synchronized {
    var oldTableName = toFullTableName(containerName) + ".bak"
    var newTableName = toFullTableName(containerName)
    logger.info("renaming " + oldTableName + " to " + newTableName);
    try {
      relogin
      renameTable(oldTableName, newTableName)
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to restore the container " + containerName, e)
      }
    }
  }

  def getAllTables: Array[String] = {
    var tables = new Array[String](0)
    try {
      relogin
      // Get all the list of tables using HBaseAdmin object
      val tableDescriptors = admin.listTables();
      tableDescriptors.foreach(t => {
        tables = tables :+ t.getNameAsString()
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch the table list  ", e)
      }
    }
    tables
  }

  override def dropTables(tbls: Array[String]): Unit = {
    try {
      tbls.foreach(t => {
        dropTable(t)
      })
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to drop table list  ", e)
      }
    }
  }

  override def dropTables(tbls: Array[(String, String)]): Unit = {
    dropTables(tbls.map(t => t._1 + ':' + t._2))
  }

  override def copyTable(srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    renameTable(srcTableName, destTableName, forceCopy)
  }

  override def copyTable(namespace: String, srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    copyTable(namespace + ':' + srcTableName, namespace + ':' + destTableName, forceCopy)
  }

  override def isTableExists(tableName: String): Boolean = {
    admin.tableExists(tableName)
  }

  override def isTableExists(tableNamespace: String, tableName: String): Boolean = {
    isTableExists(tableNamespace + ':' + tableName)
  }
}

class HBaseAdapterTx(val parent: DataStore) extends Transaction {

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
  //Added by Yousef Abu Elbeh in 2016-03-13 from here
  override def del(containerName: String, time: TimeRange): Unit = {
    parent.del(containerName, time)
  }
  // to here

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

// To create HBase Datastore instance
object HBaseAdapter extends StorageAdapterFactory {
  override def CreateStorageAdapter(kvManagerLoader: KamanjaLoaderInfo, datastoreConfig: String): DataStore = new HBaseAdapter(kvManagerLoader, datastoreConfig)
}
