/*
 * This interface is primarily used for implementing storage adapters where
 * row data is partitioned by a time stamp column and also contains one or more key
 * columns
 *
 */
package com.ligadata.StorageBase

import com.ligadata.Exceptions.{KamanjaException, NotImplementedFunctionException, InvalidArgumentException}
import com.ligadata.HeartBeat.{MonitorComponentInfo, Monitorable}
import com.ligadata.KamanjaBase.{NodeContext, AdaptersSerializeDeserializers, TransactionContext, ContainerInterface}
import com.ligadata.KvBase.{ Key, TimeRange }
import com.ligadata.Utils.{ KamanjaLoaderInfo }
import com.ligadata.kamanja.metadata.AdapterInfo

import scala.collection.mutable.ArrayBuffer
//import org.json4s._
//import org.json4s.JsonDSL._
//import org.json4s.jackson.JsonMethods._

case class Value(schemaId: Int, serializerType: String, serializedInfo: Array[Byte])

trait DataStoreOperations extends AdaptersSerializeDeserializers {
  // update operations, add & update semantics are different for relational databases

  def put(tnxCtxt: TransactionContext, container: ContainerInterface): Unit = {
    if (container == null)
      throw new InvalidArgumentException("container should not be null", null)
    // put(tnxCtxt, Array((container.getFullTypeName.toLowerCase(), Array((key, serializerTyp, value)))))
    // val (outputContainers, serializedContainerData, serializerNames) = serialize(tnxCtxt, Array(container))
    // Call put methods with container name, key & values
    throw new NotImplementedFunctionException("Not yet implemented", null)
  }

  def put(tnxCtxt: TransactionContext, containers: Array[ContainerInterface]): Unit = {
    if (containers == null)
      throw new InvalidArgumentException("containers should not be null", null)
    // put(tnxCtxt, Array((containerName, Array((key, serializerTyp, value)))))
    // val (outputContainers, serializedContainerData, serializerNames) = serialize(tnxCtxt, containers)
    // Call put methods with container name, key & values
    throw new NotImplementedFunctionException("Not yet implemented", null)
  }

  // value could be ContainerInterface or Array[Byte]
  def put(tnxCtxt: TransactionContext, containerName: String, key: Key, serializerTyp: String, value: Any): Unit = {
    if (containerName == null || key == null || serializerTyp == null || value == null)
      throw new InvalidArgumentException("ContainerName, Keys, SerializerTyps and Values should not be null", null)
    put(tnxCtxt, Array((containerName, Array((key, serializerTyp, value)))))
  }

  // value could be ContainerInterface or Array[Byte]
  def put(tnxCtxt: TransactionContext, containerName: String, keys: Array[Key], serializerTyps: Array[String], values: Array[Any]): Unit = {
    if (containerName == null || keys == null || serializerTyps == null || values == null)
      throw new InvalidArgumentException("Keys, SerializerTyps and Values should not be null", null)

    if (keys.size != serializerTyps.size || serializerTyps.size != values.size)
      throw new InvalidArgumentException("Keys:%d, SerializerTyps:%d and Values:%d should have same size".format(keys.size, serializerTyps.size, values.size), null)

    val data = ArrayBuffer[(Key, String, Any)]()

    for (i <- 0 until keys.size) {
      data += ((keys(i), serializerTyps(i), values(i)))
    }
    put(tnxCtxt, Array((containerName, data.toArray)))
  }

  // data_list has List of container names, and each container has list of key & value as ContainerInterface or Array[Byte]
  def put(tnxCtxt: TransactionContext, data_list: Array[(String, Array[(Key, String, Any)])]): Unit = {
    if (data_list == null)
      throw new InvalidArgumentException("Data should not be null", null)

    val putData = data_list.map(oneContainerData => {
      val containerData: Array[(com.ligadata.KvBase.Key, com.ligadata.StorageBase.Value)] = oneContainerData._2.map(row => {
        if (row._3.isInstanceOf[ContainerInterface]) {
          throw new NotImplementedFunctionException("Not yet implemented serializing ContainerInterface", null)
          val cont = row._3.asInstanceOf[ContainerInterface]
          // Value(schemaId: Int, serializerType: String, serializedInfo: Array[Byte])
          (row._1, Value(cont.getSchemaId, "", Array[Byte]()))
        } else {
          (row._1, Value(0, row._2, row._3.asInstanceOf[Array[Byte]]))
        }
      })
      (oneContainerData._1, containerData)
    })

    put(putData)
  }

  // update operations, add & update semantics are different for relational databases
  /* protected */ def put(containerName: String, key: Key, value: Value): Unit
  // def put(containerName: String, data_list: Array[(Key, Value)]): Unit
  /* protected */ def put(data_list: Array[(String, Array[(Key, Value)])]): Unit // data_list has List of container names, and each container has list of key & value

  // delete operations
  def del(containerName: String, keys: Array[Key]): Unit // For the given keys, delete the values
  def del(containerName: String, time: TimeRange, keys: Array[Array[String]]): Unit // For the given multiple bucket key strings, delete the values with in given date range
  def del(containerName: String, time: TimeRange): Unit // For the given date range, delete the values /*Added by Yousef Abu Elbeh at 2016-03-13*/
  // get operations
  // Returns Key, Either[ContainerInterface, Array[Byte]], serializerTyp, TypeName, Version
  def get(containerName: String, callbackFunction: (Key, Any, String, String, Int) => Unit): Unit = {
    val getCallbackFn = (k: Key, v: Value) => {
      if (callbackFunction != null) {
        if (v.schemaId > 0 /* && v.serializerType != null && v.serializerType.size > 0 */) {
          val typFromSchemaId = "" // schemaId
          val (cont, deserializerName) = deserialize(v.serializedInfo, v.serializerType)
          callbackFunction(k, cont, v.serializerType, deserializerName, 0)
        } else {
          callbackFunction(k, v.serializedInfo, v.serializerType, null, 0)
        }
      }
    }
    get(containerName, if (callbackFunction != null) getCallbackFn else null)
  }

  // Returns Key, Either[ContainerInterface, Array[Byte]], serializerTyp, TypeName, Version
  def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Any, String, String, Int) => Unit): Unit = {
    val getCallbackFn = (k: Key, v: Value) => {
      if (callbackFunction != null) {
        if (v.schemaId > 0 /* && v.serializerType != null && v.serializerType.size > 0 */) {
          val (cont, deserializerName) = deserialize(v.serializedInfo, v.serializerType)
          callbackFunction(k, cont, v.serializerType, deserializerName, 0)
        } else {
          callbackFunction(k, v.serializedInfo, v.serializerType, null, 0)
        }
      }
    }
    get(containerName, keys, getCallbackFn)
  }

  // Returns Key, Either[ContainerInterface, Array[Byte]], serializerTyp, TypeName, Version
  def get(containerName: String, timeRanges: Array[TimeRange], callbackFunction: (Key, Any, String, String, Int) => Unit): Unit  = {
    val getCallbackFn = (k: Key, v: Value) => {
      if (callbackFunction != null) {
        if (v.schemaId > 0 /* && v.serializerType != null && v.serializerType.size > 0 */) {
          val (cont, deserializerName) = deserialize(v.serializedInfo, v.serializerType)
          callbackFunction(k, cont, v.serializerType, deserializerName, 0)
        } else {
          callbackFunction(k, v.serializedInfo, v.serializerType, null, 0)
        }
      }
    }
    get(containerName, timeRanges, getCallbackFn)
  }

  // Returns Key, Either[ContainerInterface, Array[Byte]], serializerTyp, TypeName, Version
  def get(containerName: String, timeRanges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key, Any, String, String, Int) => Unit): Unit  = {
    val getCallbackFn = (k: Key, v: Value) => {
      if (callbackFunction != null) {
        if (v.schemaId > 0 /* && v.serializerType != null && v.serializerType.size > 0 */) {
          val (cont, deserializerName) = deserialize(v.serializedInfo, v.serializerType)
          callbackFunction(k, cont, v.serializerType, deserializerName, 0)
        } else {
          callbackFunction(k, v.serializedInfo, v.serializerType, null, 0)
        }
      }
    }
    get(containerName, timeRanges, bucketKeys, getCallbackFn)
  }

  // Returns Key, Either[ContainerInterface, Array[Byte]], serializerTyp, TypeName, Version
  def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Any, String, String, Int) => Unit): Unit  = {
    val getCallbackFn = (k: Key, v: Value) => {
      if (callbackFunction != null) {
        if (v.schemaId > 0 /* && v.serializerType != null && v.serializerType.size > 0 */) {
          val (cont, deserializerName) = deserialize(v.serializedInfo, v.serializerType)
          callbackFunction(k, cont, v.serializerType, deserializerName, 0)
        } else {
          callbackFunction(k, v.serializedInfo, v.serializerType, null, 0)
        }
      }
    }
    get(containerName, bucketKeys, getCallbackFn)
  }

  /* protected */ def get(containerName: String, callbackFunction: (Key, Value) => Unit): Unit
  /* protected */ def get(containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit
  /* protected */ def get(containerName: String, timeRanges: Array[TimeRange], callbackFunction: (Key, Value) => Unit): Unit // Range of dates
  /* protected */ def get(containerName: String, timeRanges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit
  /* protected */ def get(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key, Value) => Unit): Unit

  /*
  // Passing filter to storage
  def get(containerName: String, filterFunction: (Key, Value) => Boolean, callbackFunction: (Key, Value) => Unit): Unit
  def get(containerName: String, timeRanges: Array[TimeRange], filterFunction: (Key, Value) => Boolean, callbackFunction: (Key, Value) => Unit): Unit // Range of dates
  def get(containerName: String, timeRanges: Array[TimeRange], bucketKeys: Array[Array[String]], filterFunction: (Key, Value) => Boolean, callbackFunction: (Key, Value) => Unit): Unit
  def get(containerName: String, bucketKeys: Array[Array[String]], filterFunction: (Key, Value) => Boolean, callbackFunction: (Key, Value) => Unit): Unit
*/

  // getKeys operations similar to get, but only key values
  def getKeys(containerName: String, callbackFunction: (Key) => Unit): Unit
  def getKeys(containerName: String, keys: Array[Key], callbackFunction: (Key) => Unit): Unit
  def getKeys(containerName: String, timeRanges: Array[TimeRange], callbackFunction: (Key) => Unit): Unit // Range of dates
  def getKeys(containerName: String, timeRanges: Array[TimeRange], bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit
  def getKeys(containerName: String, bucketKeys: Array[Array[String]], callbackFunction: (Key) => Unit): Unit

  def copyContainer(srcContainerName: String, destContainerName: String, forceCopy: Boolean): Unit
  def isContainerExists(containerName: String): Boolean

  def backupContainer(containerName: String): Unit
  def restoreContainer(containerName: String): Unit

  def getAllTables: Array[String] //namespace or schemaName is assumed to be attached to adapter object
  def dropTables(tbls: Array[String]) // here tbls are full qualified name (including namespace)
  def dropTables(tbls: Array[(String, String)])
  def copyTable(srcTableName: String, destTableName: String, forceCopy: Boolean): Unit // here srcTableName & destTableName are full qualified name (including namespace)
  def copyTable(namespace: String, srcTableName: String, destTableName: String, forceCopy: Boolean): Unit
  def isTableExists(tableName: String): Boolean // here tableName is full qualified name (including namespace)
  def isTableExists(tableNamespace: String, tableName: String): Boolean
}

trait DataStore extends DataStoreOperations {
  def beginTx(): Transaction
  def endTx(tx: Transaction): Unit // Same as commit
  def commitTx(tx: Transaction): Unit
  def rollbackTx(tx: Transaction): Unit

  // clean up operations
  def Shutdown(): Unit
  def TruncateContainer(containerNames: Array[String]): Unit
  def DropContainer(containerNames: Array[String]): Unit
  def CreateContainer(containerNames: Array[String]): Unit
  def CreateMetadataContainer(containerNames: Array[String]): Unit
}

trait Transaction extends DataStoreOperations {
  val parent: DataStore // Parent Data Store
}

// Storage Adapter Object to create storage adapter
trait StorageAdapterFactory {
  def CreateStorageAdapter(kvManagerLoader: KamanjaLoaderInfo, datastoreConfig: String): DataStore
//  final def CreateStorageAdapter(nodeCtxt: NodeContext, kvManagerLoader: KamanjaLoaderInfo, adapterInfo: AdapterInfo): StorageAdapter = {
//    val datastore = CreateStorageAdapter(kvManagerLoader, adapterInfo.FullAdapterConfig)
//    new StorageAdapter(nodeCtxt, adapterInfo, datastore)
//  }
}

class StorageAdapterConfiguration {
  var Name: String = _
  var tenantId: String = _
  var adapterInfo: AdapterInfo = _
}

class StorageAdapter(nodeCtxt: NodeContext, adapterInfo: AdapterInfo, datastore: DataStore) extends AdaptersSerializeDeserializers with Monitorable {
  // NodeContext
  val _nodeContext: NodeContext = nodeCtxt
  val _TYPE_STORAGE = "Storage_Adapter"
  val _startTime: String = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))

  val _storageConfig: StorageAdapterConfiguration = {
    val cfg = new StorageAdapterConfiguration
    cfg.Name = adapterInfo.Name
    cfg.tenantId = adapterInfo.TenantId
    cfg.adapterInfo = adapterInfo
    cfg
  }

  var _datastore: DataStore = datastore

//  def write(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface]): Unit
//  def read(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface]): Unit

  def Shutdown: Unit = {
    if (_datastore != null)
      _datastore.Shutdown()
    _datastore = null
  }

  def Category = "Storage"

  override def getComponentStatusAndMetrics: MonitorComponentInfo = {
    val lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
    MonitorComponentInfo(_TYPE_STORAGE, _storageConfig.Name, _storageConfig.Name, _startTime, lastSeen, "{}")
  }

  override def getComponentSimpleStats: String = {
    ""
  }
}

