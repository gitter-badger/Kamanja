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

import com.datastax.driver.core.Cluster
import com.datastax.driver.core.Session
import com.datastax.driver.core.querybuilder.Insert
import com.datastax.driver.core.Row
import com.datastax.driver.core.ResultSet
import com.datastax.driver.core.ConsistencyLevel
import com.datastax.driver.core.SimpleStatement
import com.datastax.driver.core.PreparedStatement
import com.datastax.driver.core.BatchStatement
import com.datastax.driver.core.HostDistance
import com.datastax.driver.core.PoolingOptions
import java.nio.ByteBuffer
import org.apache.logging.log4j._
import com.ligadata.Exceptions._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.ligadata.Utils.{ KamanjaLoaderInfo }

import com.ligadata.KvBase.{ Key, Value, TimeRange }
import com.ligadata.StorageBase.{ DataStore, Transaction, StorageAdapterObj }
import java.util.{ Date, Calendar, TimeZone }
import java.text.SimpleDateFormat
import java.io.File
import scala.collection.mutable.TreeSet
import java.util.Properties
import com.ligadata.Exceptions._

import scala.collection.JavaConversions._

/*
datastoreConfig should have the following:
	Mandatory Options:
		hostlist/Location
		schema/SchemaName

	Optional Options:
		user
		password
		replication_class
		replication_factor
		ConsistencyLevelRead
		ConsistencyLevelWrite
		ConsistencyLevelDelete
		
		All the optional values may come from "AdapterSpecificConfig" also. That is the old way of giving more information specific to Adapter
*/

/*
  	You open connection to a cluster hostname[,hostname]:port
  	You could provide username/password

 	You can operator on keyspace / table

 	if key space is missing we will try to create
 	if table is missing we will try to create

	-- Lets start with this schema
	--
	CREATE KEYSPACE default WITH replication = { 'class': 'SimpleStrategy', 'replication_factor': '4' };
	USE default;
	CREATE TABLE default (key blob, value blob, primary key(key) );
 */

class CassandraAdapter(val kvManagerLoader: KamanjaLoaderInfo, val datastoreConfig: String) extends DataStore {
  val adapterConfig = if (datastoreConfig != null) datastoreConfig.trim else ""
  val loggerName = this.getClass.getName

  val logger = LogManager.getLogger(loggerName)

  private[this] val lock = new Object
  private var containerList: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()
  private var preparedStatementsMap: scala.collection.mutable.Map[String, PreparedStatement] = new scala.collection.mutable.HashMap()
  private var msg: String = ""
  var autoCreateTables = "YES"

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

  logger.debug("Cassandra configuration:" + adapterConfig)
  var parsed_json: Map[String, Any] = null
  try {
    val json = parse(adapterConfig)
    if (json == null || json.values == null) {
      var msg = "Failed to parse Cassandra JSON configuration string:" + adapterConfig
      throw CreateConnectionException(msg, new Exception("Invalid Configuration"))
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

  // Read all cassandra parameters
  val hostnames = if (parsed_json.contains("hostlist")) parsed_json.getOrElse("hostlist", "localhost").toString.trim else parsed_json.getOrElse("Location", "localhost").toString.trim
  val keyspace = if (parsed_json.contains("schema")) parsed_json.getOrElse("schema", "default").toString.trim else parsed_json.getOrElse("SchemaName", "default").toString.trim
  val replication_class = getOptionalField("replication_class", parsed_json, adapterSpecificConfig_json, "SimpleStrategy").toString.trim
  val replication_factor = getOptionalField("replication_factor", parsed_json, adapterSpecificConfig_json, "1").toString.trim
  val consistencylevelRead = ConsistencyLevel.valueOf(getOptionalField("ConsistencyLevelRead", parsed_json, adapterSpecificConfig_json, "ONE").toString.trim)
  val consistencylevelWrite = ConsistencyLevel.valueOf(getOptionalField("ConsistencyLevelWrite", parsed_json, adapterSpecificConfig_json, "ANY").toString.trim)
  val consistencylevelDelete = ConsistencyLevel.valueOf(getOptionalField("ConsistencyLevelDelete", parsed_json, adapterSpecificConfig_json, "ONE").toString.trim)

  // misc options
  var batchPuts = "NO"
  if (parsed_json.contains("batchPuts")) {
    batchPuts = parsed_json.get("batchPuts").get.toString.trim
  }

  // actual cassandra limit is 48 characters
  // when we create a table backup, we add a suffix ".b" to the actual tableName
  // so we limit it 46
  var tableNameLength = 46
  if (parsed_json.contains("tableNameLength")) {
    tableNameLength = parsed_json.get("tableNameLength").get.toString.trim.toInt
  }

  if (parsed_json.contains("autoCreateTables")) {
    autoCreateTables = parsed_json.get("autoCreateTables").get.toString.trim
  }

  var exportDumpDir = "."
  if (parsed_json.contains("exportDumpDir")) {
    exportDumpDir = parsed_json.get("exportDumpDir").get.toString.trim
  }

  val clusterBuilder = Cluster.builder()
  var cluster: Cluster = _
  var session: Session = _
  var keyspace_exists = false

  try {
    clusterBuilder.addContactPoints(hostnames)
    val usr = getOptionalField("user", parsed_json, adapterSpecificConfig_json, null)
    if (usr != null)
      clusterBuilder.withCredentials(usr.toString.trim, getOptionalField("password", parsed_json, adapterSpecificConfig_json, "").toString.trim)

    // Cassandra connection Pooling
    var poolingOptions = new PoolingOptions();
    var minConPerHostLocal = 4
    if (parsed_json.contains("minConPerHostLocal")) {
      minConPerHostLocal = parsed_json.get("minConPerHostLocal").get.toString.trim.toInt
    }
    var minConPerHostRemote = 2
    if (parsed_json.contains("minConPerHostRemote")) {
      minConPerHostRemote = parsed_json.get("minConPerHostRemote").get.toString.trim.toInt
    }

    var maxConPerHostLocal = 10
    if (parsed_json.contains("maxConPerHostLocal")) {
      maxConPerHostLocal = parsed_json.get("maxConPerHostLocal").get.toString.trim.toInt
    }
    var maxConPerHostRemote = 3
    if (parsed_json.contains("maxConPerHostRemote")) {
      maxConPerHostRemote = parsed_json.get("maxConPerHostRemote").get.toString.trim.toInt
    }

    // set some arbitrary values for coreConnections and maxConnections for now..
    poolingOptions.setCoreConnectionsPerHost(HostDistance.LOCAL, minConPerHostLocal)
    poolingOptions.setCoreConnectionsPerHost(HostDistance.REMOTE, minConPerHostRemote)
    poolingOptions.setMaxConnectionsPerHost(HostDistance.LOCAL, maxConPerHostLocal)
    poolingOptions.setMaxConnectionsPerHost(HostDistance.REMOTE, maxConPerHostRemote)

    cluster = clusterBuilder.withPoolingOptions(poolingOptions).build()

    if (cluster.getMetadata().getKeyspace(keyspace) == null) {
      logger.warn("The keyspace " + keyspace + " doesn't exist yet, we will create a new keyspace and continue")
      // create a session that is not associated with a key space yet so we can create one if needed
      session = cluster.connect();
      // create keyspace if not exists
      val createKeySpaceStmt = "CREATE KEYSPACE IF NOT EXISTS " + keyspace + " with replication = {'class':'" + replication_class + "', 'replication_factor':" + replication_factor + "};"
      try {
        session.execute(createKeySpaceStmt);
      } catch {
        case e: Exception => {
          msg = "Unable to create keyspace " + keyspace + ":" + e.getMessage()
          throw CreateConnectionException(msg, e)
        }
      }
      // make sure the session is associated with the new tablespace, can be expensive if we create recycle sessions  too often
      session.close()
      session = cluster.connect(keyspace)
    } else {
      keyspace_exists = true
      session = cluster.connect(keyspace)
    }
    logger.info("DataStore created successfully")
  } catch {
    case e: Exception => {
      throw CreateConnectionException("Unable to connect to cassandra at " + hostnames + ":" + e.getMessage(), e)
    }
  }

  private def CheckTableExists(containerName: String, apiType: String = "dml"): Unit = {
    try {
      if (containerList.contains(containerName)) {
        return
      } else {
        CreateContainer(containerName, apiType)
        containerList.add(containerName)
      }
    } catch {
      case e: Exception => {
        throw new Exception("Failed to create table  " + toTableName(containerName) + ":" + e.getMessage())
      }
    }
  }

  def DropKeySpace(keyspace: String): Unit = lock.synchronized {
    if (cluster.getMetadata().getKeyspace(keyspace) == null) {
      logger.info("The keyspace " + keyspace + " doesn't exist yet, noting to drop")
      return
    }
    // create keyspace if not exists
    val dropKeySpaceStmt = "DROP KEYSPACE " + keyspace;
    try {
      session.execute(dropKeySpaceStmt);
    } catch {
      case e: Exception => {
        throw CreateDDLException("Unable to drop keyspace " + keyspace + ":" + e.getMessage(), e)
      }
    }
  }

  private def CreateContainer(containerName: String, apiType: String): Unit = lock.synchronized {
    var tableName = toTableName(containerName)
    var fullTableName = toFullTableName(containerName)
    try {
      var ks = cluster.getMetadata().getKeyspace(keyspace)
      var t = ks.getTable(fullTableName)
      if (t != null) {
        logger.debug("The table " + tableName + " already exists ")
      } else {
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
        var query = "create table if not exists " + fullTableName + "(bucketkey varchar,timepartition bigint,transactionid bigint, rowid int, serializertype varchar, serializedinfo blob, primary key(bucketkey,timepartition,transactionid,rowid));"
        session.execute(query);
      }
    } catch {
      case e: Exception => {
        throw CreateDDLException("Failed to create table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def CreateContainer(containerNames: Array[String]): Unit = {
    logger.info("create the container tables")
    containerNames.foreach(cont => {
      logger.info("create the container " + cont)
      CreateContainer(cont, "ddl")
    })
  }

  private def toTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    //containerName.replace('.','_')
    // Cassandra has a limit of 48 characters for table name, so take the first 48 characters only
    // Even though we don't allow duplicate containers within the same namespace,
    // Taking first 48 characters may result in  duplicate table names
    // So I am reversing the long string to ensure unique name
    // Need to be documented, at the least.
    var t = containerName.toLowerCase.replace('.', '_').replace('-', '_')
    if (t.length > tableNameLength) {
      t.reverse.substring(0, tableNameLength)
    } else {
      t
    }
  }

  private def toFullTableName(containerName: String): String = {
    // we need to check for other restrictions as well
    // such as length of the table, special characters etc
    toTableName(containerName)
  }

  private def bucketKeyToString(bucketKey: Array[String]): String = {
    bucketKey.mkString(",")
  }

  private def strToBucketKey(keyStr: String): Array[String] = {
    if (keyStr != null) keyStr.split(",").toArray else new Array[String](0)
  }

  override def put(containerName: String, key: Key, value: Value): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      var query = "UPDATE " + tableName + " SET serializertype = ? , serializedinfo = ? where timepartition = ? and bucketkey = ? and transactionid = ? and rowid = ?;"
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      var byteBuf = ByteBuffer.wrap(value.serializedInfo.toArray[Byte]);
      session.execute(prepStmt.bind(value.serializerType,
        byteBuf,
        new java.lang.Long(key.timePartition),
        bucketKeyToString(key.bucketKey),
        new java.lang.Long(key.transactionId),
        new java.lang.Integer(key.rowId)).
        setConsistencyLevel(consistencylevelWrite))
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save an object in table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def put(data_list: Array[(String, Array[(Key, Value)])]): Unit = {
    var tableName = ""
    try {
      val batch = new BatchStatement
      data_list.foreach(li => {
        var containerName = li._1
        CheckTableExists(containerName)
        tableName = toFullTableName(containerName)
        var query = "UPDATE " + tableName + " SET serializertype = ? , serializedinfo = ? where timepartition = ? and bucketkey = ? and transactionid = ? and rowid = ?;"
        var prepStmt = preparedStatementsMap.getOrElse(query, null)
        if (prepStmt == null) {
          prepStmt = session.prepare(query)
          preparedStatementsMap.put(query, prepStmt)
        }
        var keyValuePairs = li._2
        keyValuePairs.foreach(keyValuePair => {
          var key = keyValuePair._1
          var value = keyValuePair._2
          var byteBuf = ByteBuffer.wrap(value.serializedInfo.toArray[Byte]);
          // Need to sort out the issue by doing more experimentation
          // When we use a batch statement, and when uploading severaljars when we save a model
          // object, I am receiving an exception as follows
          // com.datastax.driver.core.exceptions.WriteTimeoutException: Cassandra timeout during 
          // write query at consistency ONE (1 replica were required but only 0 acknowledged the write)
          // Based on my reading on cassandra, I have the upped the parameter write_request_timeout_in_ms
          // to as much as: 60000( 60 seconds). Now, I see a different exception as shown below
          // com.datastax.driver.core.exceptions.NoHostAvailableException: All host(s) tried for query failed 
          // The later error apparently is a misleading error indicating slowness with cassandra server
          // This may not be an issue in production like configuration
          // As a work around, I have created a optional configuration parameter to determine
          // whether updates are done in bulk or one at a time. By default we are doing this 
          // one at a time until we have a better solution.
          if (batchPuts.equalsIgnoreCase("YES")) {
            batch.add(prepStmt.bind(value.serializerType,
              byteBuf,
              new java.lang.Long(key.timePartition),
              bucketKeyToString(key.bucketKey),
              new java.lang.Long(key.transactionId),
              new java.lang.Integer(key.rowId)).
              setConsistencyLevel(consistencylevelWrite))
          } else {
            session.execute(prepStmt.bind(value.serializerType,
              byteBuf,
              new java.lang.Long(key.timePartition),
              bucketKeyToString(key.bucketKey),
              new java.lang.Long(key.transactionId),
              new java.lang.Integer(key.rowId)).
              setConsistencyLevel(consistencylevelWrite))
          }
        })
      })
      if (batchPuts.equalsIgnoreCase("YES")) {
        session.execute(batch);
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to save object(s) in table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  // delete operations
  override def del(containerName: String, keys: Array[Key]): Unit = {
    var tableName = ""
    try {
      CheckTableExists(containerName)
      tableName = toFullTableName(containerName)
      val batch = new BatchStatement
      var query = "delete from " + tableName + " where timepartition = ? and bucketkey = ? and transactionid = ? and rowid = ?;"
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      keys.foreach(key => {
        batch.add(prepStmt.bind(new java.lang.Long(key.timePartition),
          bucketKeyToString(key.bucketKey),
          new java.lang.Long(key.transactionId),
          new java.lang.Integer(key.rowId)).
          setConsistencyLevel(consistencylevelDelete))
      })
      session.execute(batch);
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  private def getRowKey(rs: Row): Key = {
    var timePartition = rs.getLong("timepartition")
    var keyStr = rs.getString("bucketkey")
    var tId = rs.getLong("transactionid")
    var rId = rs.getInt("rowid")
    val bucketKey = strToBucketKey(keyStr)
    new Key(timePartition, bucketKey, tId, rId)
  }

  // range deletes are not supported by cassandra
  // so identify the rowKeys for the time range and delete them as a batch
  override def del(containerName: String, time: TimeRange, bucketKeys: Array[Array[String]]): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName + " where bucketkey = ? and timepartition >= ?  and timepartition <= ? "
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      var rowKeys = new Array[Key](0)
      bucketKeys.foreach(bucketKey => {
        var rows = session.execute(prepStmt.bind(bucketKeyToString(bucketKey),
          new java.lang.Long(time.beginTime),
          new java.lang.Long(time.endTime)).
          setConsistencyLevel(consistencylevelDelete))
        for (rs <- rows) {
          rowKeys = rowKeys :+ getRowKey(rs)
        }
      })
      del(containerName, rowKeys)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to delete object(s) from table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  // get operations
  def getRowCount(containerName: String, whereClause: String): Long = {
    var tableName = toFullTableName(containerName)
    var getCountStmt = new SimpleStatement("select count(*) from " + tableName)
    var rows = session.execute(getCountStmt)
    var row = rows.one()
    var cnt = row.getLong("count")
    return cnt
  }

  private def convertByteBufToArrayOfBytes(buffer: java.nio.ByteBuffer): Array[Byte] = {
    var ba = new Array[Byte](buffer.remaining())
    buffer.get(ba, 0, ba.length);
    ba
  }

  private def processRow(rs: Row, callbackFunction: (Key, Value) => Unit) {
    var timePartition = rs.getLong("timepartition")
    var keyStr = rs.getString("bucketkey")
    var tId = rs.getLong("transactionid")
    var rId = rs.getInt("rowid")
    var st = rs.getString("serializertype")
    var buf = rs.getBytes("serializedinfo")
    // format the data to create Key/Value
    val bucketKey = strToBucketKey(keyStr)
    var key = new Key(timePartition, bucketKey, tId, rId)
    var ba = convertByteBufToArrayOfBytes(buf)
    var value = new Value(st, ba)
    (callbackFunction)(key, value)
  }

  private def processRow(key: Key, rs: Row, callbackFunction: (Key, Value) => Unit) {
    var st = rs.getString("serializertype")
    var buf = rs.getBytes("serializedinfo")
    var ba = convertByteBufToArrayOfBytes(buf)
    var value = new Value(st, ba)
    (callbackFunction)(key, value)
  }

  private def processKey(rs: Row, callbackFunction: (Key) => Unit) {
    var timePartition = rs.getLong("timepartition")
    var keyStr = rs.getString("bucketkey")
    var tId = rs.getLong("transactionid")
    var rId = rs.getInt("rowid")
    // format the data to create Key/Value
    val bucketKey = strToBucketKey(keyStr)
    var key = new Key(timePartition, bucketKey, tId, rId)
    (callbackFunction)(key)
  }

  private def getData(tableName: String, query: String, callbackFunction: (Key, Value) => Unit): Unit = {
    try {
      var getDataStmt = new SimpleStatement(query)
      var rows = session.execute(getDataStmt)
      var rs: Row = null
      for (rs <- rows) {
        processRow(rs, callbackFunction)
      }
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  override def get(containerName: String, callbackFunction: (Key, Value) => Unit): Unit = {
    CheckTableExists(containerName)
    var tableName = toFullTableName(containerName)
    var query = "select * from " + tableName
    getData(tableName, query, callbackFunction)
  }

  private def getKeys(tableName: String, query: String, callbackFunction: (Key) => Unit): Unit = {
    try {
      var getDataStmt = new SimpleStatement(query)
      var rows = session.execute(getDataStmt)
      var rs: Row = null
      for (rs <- rows) {
        processKey(rs, callbackFunction)
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.error("Stacktrace:" + stackTrace)
      }
    }
  }

  override def getKeys(containerName: String, callbackFunction: (Key) => Unit): Unit = {
    CheckTableExists(containerName)
    var tableName = toFullTableName(containerName)
    var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName
    getKeys(tableName, query, callbackFunction)
  }

  override def getKeys(containerName: String, keys: Array[Key], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName + " where timepartition = ? and bucketkey = ? and transactionid = ? and rowid = ?"
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      keys.foreach(key => {
        var rows = session.execute(prepStmt.bind(new java.lang.Long(key.timePartition),
          bucketKeyToString(key.bucketKey),
          new java.lang.Long(key.transactionId),
          new java.lang.Integer(key.rowId)).
          setConsistencyLevel(consistencylevelRead))
        var rs: Row = null
        for (rs <- rows) {
          processKey(rs, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select serializertype,serializedinfo from " + tableName + " where timepartition = ? and bucketkey = ? and transactionid = ? and rowid = ?"
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      keys.foreach(key => {
        var rows = session.execute(prepStmt.bind(new java.lang.Long(key.timePartition),
          bucketKeyToString(key.bucketKey),
          new java.lang.Long(key.transactionId),
          new java.lang.Integer(key.rowId)).
          setConsistencyLevel(consistencylevelRead))
        var rs: Row = null
        for (rs <- rows) {
          processRow(key, rs, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }

  }

  override def get(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key, Value) => Unit): Unit = {
    CheckTableExists(containerName)
    var tableName = toFullTableName(containerName)
    var query = "select timepartition,bucketkey,transactionid,rowid,serializertype,serializedinfo from " + tableName + " where timepartition >= ? and timepartition <= ? ALLOW FILTERING;"
    var prepStmt = preparedStatementsMap.getOrElse(query, null)
    if (prepStmt == null) {
      prepStmt = session.prepare(query)
      preparedStatementsMap.put(query, prepStmt)
    }
    time_ranges.foreach(time_range => {
      var rs: Row = null
      var rows = session.execute(prepStmt.bind(new java.lang.Long(time_range.beginTime),
        new java.lang.Long(time_range.endTime)).
        setConsistencyLevel(consistencylevelRead))
      for (rs <- rows) {
        processRow(rs, callbackFunction)
      }
    })
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], callbackFunction: (Key) => Unit): Unit = {
    CheckTableExists(containerName)
    var tableName = toFullTableName(containerName)
    var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName + " where timepartition >= ? and timepartition <= ? ALLOW FILTERING;"
    var prepStmt = preparedStatementsMap.getOrElse(query, null)
    if (prepStmt == null) {
      prepStmt = session.prepare(query)
      preparedStatementsMap.put(query, prepStmt)
    }
    time_ranges.foreach(time_range => {
      var rs: Row = null
      var rows = session.execute(prepStmt.bind(new java.lang.Long(time_range.beginTime),
        new java.lang.Long(time_range.endTime)).
        setConsistencyLevel(consistencylevelRead))
      for (rs <- rows) {
        processKey(rs, callbackFunction)
      }
    })
  }

  override def get(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid,serializertype,serializedinfo from " + tableName + " where timepartition >= ?  and timepartition <= ?  and bucketkey = ? "
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      time_ranges.foreach(time_range => {
        bucketKeys.foreach(bucketKey => {
          var rs: Row = null
          var rows = session.execute(prepStmt.bind(new java.lang.Long(time_range.beginTime),
            new java.lang.Long(time_range.endTime),
            bucketKeyToString(bucketKey)).
            setConsistencyLevel(consistencylevelRead))
          for (rs <- rows) {
            processRow(rs, callbackFunction)
          }
        })
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, time_ranges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName + " where timepartition >= ? and timepartition <= ?  and bucketkey = ? "
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      time_ranges.foreach(time_range => {
        bucketKeys.foreach(bucketKey => {
          var rs: Row = null
          var rows = session.execute(prepStmt.bind(new java.lang.Long(time_range.beginTime),
            new java.lang.Long(time_range.endTime),
            bucketKeyToString(bucketKey)).
            setConsistencyLevel(consistencylevelRead))
          for (rs <- rows) {
            processKey(rs, callbackFunction)
          }
        })
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid,serializertype,serializedinfo from " + tableName + " where  bucketkey = ? "
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      bucketKeys.foreach(bucketKey => {
        var rs: Row = null
        var rows = session.execute(prepStmt.bind(bucketKeyToString(bucketKey)).
          setConsistencyLevel(consistencylevelRead))
        for (rs <- rows) {
          processRow(rs, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def getKeys(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit = {
    var tableName = toFullTableName(containerName)
    try {
      CheckTableExists(containerName)
      var query = "select timepartition,bucketkey,transactionid,rowid from " + tableName + " where  bucketkey = ? "
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      if (prepStmt == null) {
        prepStmt = session.prepare(query)
        preparedStatementsMap.put(query, prepStmt)
      }
      bucketKeys.foreach(bucketKey => {
        var rows = session.execute(prepStmt.bind(bucketKeyToString(bucketKey)).
          setConsistencyLevel(consistencylevelRead))
        for (rs <- rows) {
          processKey(rs, callbackFunction)
        }
      })
    } catch {
      case e: Exception => {
        throw CreateDMLException("Failed to fetch data from the table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  override def beginTx(): Transaction = {
    new CassandraAdapterTx(this)
  }

  override def endTx(tx: Transaction): Unit = {}

  override def commitTx(tx: Transaction): Unit = {}

  override def rollbackTx(tx: Transaction): Unit = {}

  override def Shutdown(): Unit = {
    logger.info("close the session and connection pool")
    session.close()
    cluster.close()
  }

  private def TruncateContainer(containerName: String): Unit = {
    var fullTableName = toFullTableName(containerName)
    try {
      var query = "truncate " + fullTableName
      session.execute(query);
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to truncate table " + fullTableName + ":" + e.getMessage(), e)
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
    var tableName = toTableName(containerName)
    var fullTableName = toFullTableName(containerName)
    try {
      var query = "drop table if exists " + fullTableName
      session.execute(query);
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

  private def ExportTable(tableName: String, exportDump: String): Unit = {
    try {
      var query = "copy " + tableName + " to '" + exportDump + "';"
      logger.info("query => " + query)
      session.execute(query)
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to export table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  private def ImportTable(tableName: String, exportDump: String): Unit = {
    try {
      var query = "copy " + tableName + " from '" + exportDump + "';"
      session.execute(query);
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to import table " + tableName + ":" + e.getMessage(), e)
      }
    }
  }

  private def IsTableExists(tableName: String): Boolean = {
    try {
      var ks = cluster.getMetadata().getKeyspace(keyspace)
      var t = ks.getTable(tableName)
      if (t != null) {
        return true;
      } else {
        return false;
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to verify whether table " + tableName + " exists:" + e.getMessage(), e)
      }
    }
  }

  private def IsFileExists(fileName: String): Boolean = {
    new java.io.File(fileName).exists
  }

  private def getColDataType(validator: String): String = {
    var dataType: String = "unknown"
    validator match {
      case "org.apache.cassandra.db.marshal.LongType" => {
        dataType = "bigint"
      }
      case "org.apache.cassandra.db.marshal.UTF8Type" => {
        dataType = "varchar"
      }
      case "org.apache.cassandra.db.marshal.Int32Type" => {
        dataType = "int"
      }
      case "org.apache.cassandra.db.marshal.BytesType" => {
        dataType = "blob"
      }
    }
    return dataType
  }

  private def isKeyCol(column_type: String): Boolean = {
    column_type.equals("partition_key") | column_type.equals("clustering_key")
  }

  private def cloneTable(oldTableName: String, newTableName: String): Unit = {
    try {
      var query = "SELECT column_name,type,validator FROM system.schema_columns WHERE keyspace_name = ? AND columnfamily_name = ?"
      var prepStmt = preparedStatementsMap.getOrElse(query, null)
      prepStmt = session.prepare(query)
      var rows = session.execute(prepStmt.bind(keyspace, oldTableName).setConsistencyLevel(consistencylevelRead))
      var createStmt = "CREATE TABLE IF NOT EXISTS " + newTableName + "("
      var keyColStr = ""
      var colStr = ""
      var colCount = 0
      var colDtypeMap: scala.collection.mutable.Map[String, String] = new scala.collection.mutable.HashMap()

      var columnArray = new Array[String](0)
      for (rs <- rows) {
        var column_name = rs.getString("column_name")
        var column_type = rs.getString("type")
        var validator = rs.getString("validator")
        var dtype = getColDataType(validator);
        createStmt = createStmt + column_name + " " + dtype + ","
        if (isKeyCol(column_type)) {
          keyColStr = keyColStr + column_name + ","
        }
        colStr = colStr + column_name + ","
        colCount = colCount + 1
        columnArray :+ columnArray + column_name
        colDtypeMap(column_name) = dtype;
      }
      // strip the last comma of keyColStr
      keyColStr = keyColStr.stripSuffix(",")
      colStr = colStr.stripSuffix(",")
      // construct complete create statement
      createStmt = createStmt + " primary key ( " + keyColStr + "));"
      logger.info("create table statement => " + createStmt)
      session.execute(createStmt);

      /*
      // copy the data

      var insertStatement = "insert into " + newTableName  + "(" + colStr + ") values (";
      for( _ <- 1 to  colCount ){
	insertStatement = insertStatement + "? "
      }
      insertStatement = insertStatement + ");"
      prepInsertStmt = session.prepare(insertStatement)

      query = "SELECT " + colStr + " FROM " + oldTableName + ";"
      prepStmt = preparedStatementsMap.getOrElse(query,null)
      if( prepStmt == null ){
	prepStmt = session.prepare(query)
	preparedStatementsMap.put(query,prepStmt)
      }
      rows = session.execute(prepStmt.setConsistencyLevel(consistencylevelRead))
      for( rs <- rows ){
	var colValueMap: scala.collection.mutable.Map[String,Any] = new scala.collection.mutable.HashMap()
	columnArray.foreach( col => {
	  colDtypeMap(col) match {
	    case "blob" => {
	      colValues(col) = rs.getBytes(col)
	    }
	    case "bigint" => {
	      colValues(col) = rs.getLong(col)
	    }
	    case "int" => {
	      colValues(col) = rs.getInt(col)
	    }
	    case "varchar" => {
	      colValues(col) = rs.getString(col)
	    }
	  }
	})
      }
      */
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to clone the table " + oldTableName + ":" + e.getMessage(), e)
      }
    }
  }

  private def CreateBackupTable(oldTableName: String, newTableName: String): Unit = lock.synchronized {
    try {
      // check whether new table already exists
      if (IsTableExists(newTableName)) {
        logger.info("The table " + newTableName + " exists, may have beem created already ")
      } else {
        if (!IsTableExists(oldTableName)) {
          logger.info("The table " + oldTableName + " doesn't exist, nothing to rename ")
        } else {
          var dumpFilePath = exportDumpDir + "/" + oldTableName + ".csv"
          if (!IsFileExists(dumpFilePath)) {
            //ExportTable(oldTableName,dumpFilePath)
          }
          // create the new table with the same structure
          cloneTable(oldTableName, newTableName)
          //ImportTable(newTableName,dumpFilePath)
        }
      }
    } catch {
      case e: Exception => {
        throw CreateDMLException("Unable to create the backup table " + newTableName + ":" + e.getMessage(), e)
      }
    }
  }

  def backupContainer(containerName: String): Unit = lock.synchronized {
    throw CreateDDLException("Not Implemented yet :", new Exception("Failed to backup the container " + containerName))

    // create an export file
    //var fullTableName = toFullTableName(containerName)
    //var oldTableName = fullTableName
    //var newTableName = oldTableName + "_b"
    //CreateBackupTable(oldTableName,newTableName)
  }

  def restoreContainer(containerName: String): Unit = lock.synchronized {
    throw CreateDDLException("Not Implemented yet :", new Exception("Failed to restore the container " + containerName))
    // create an export file
    //var fullTableName = toFullTableName(containerName)
    //var newTableName = fullTableName
    //var oldTableName = newTableName + "_b"
    //CreateBackupTable(oldTableName,newTableName)
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

class CassandraAdapterTx(val parent: DataStore) extends Transaction {

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
  override def dropTables(tbls: Array[String]): Unit = {
    parent.dropTables(tbls)
  }

  override def copyTable(srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    parent.copyTable(srcTableName, destTableName, forceCopy)
  }

  override def isTableExists(tableName: String): Boolean = {
    parent.isTableExists(tableName)
  }

  override def isTableExists(tableNamespace: String, tableName: String): Boolean = {
    isTableExists(tableNamespace, tableName)
  }

  override def dropTables(tbls: Array[(String, String)]): Unit = {
    dropTables(tbls)
  }

  override def copyTable(namespace: String, srcTableName: String, destTableName: String, forceCopy: Boolean): Unit = {
    copyTable(namespace, srcTableName, destTableName, forceCopy)
  }
}

// To create Cassandra Datastore instance
object CassandraAdapter extends StorageAdapterObj {
  override def CreateStorageAdapter(kvManagerLoader: KamanjaLoaderInfo, datastoreConfig: String): DataStore = new CassandraAdapter(kvManagerLoader, datastoreConfig)
}
