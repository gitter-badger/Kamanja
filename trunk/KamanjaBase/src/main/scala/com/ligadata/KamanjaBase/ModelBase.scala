
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

package com.ligadata.KamanjaBase

import com.ligadata.Exceptions.{DeprecatedException, NotImplementedFunctionException}

import scala.collection.immutable.Map
import com.ligadata.Utils.{CacheConfig, Utils, KamanjaLoaderInfo, ClusterStatus}
import com.ligadata.kamanja.metadata.{MdMgr, ModelDef}
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import java.io.{DataInputStream, DataOutputStream}
import com.ligadata.KvBase.{Key, TimeRange /* , KvBaseDefalts, KeyWithBucketIdAndPrimaryKey, KeyWithBucketIdAndPrimaryKeyCompHelper */}
import com.ligadata.HeartBeat._

import scala.collection.mutable.ArrayBuffer

object MinVarType extends Enumeration {
  type MinVarType = Value
  val Unknown, Active, Predicted, Supplementary, FeatureExtraction = Value

  def StrToMinVarType(tsTypeStr: String): MinVarType = {
    tsTypeStr.toLowerCase.trim() match {
      case "active" => Active
      case "predicted" => Predicted
      case "supplementary" => Supplementary
      case "featureextraction" => FeatureExtraction
      case _ => Unknown
    }
  }
}

import MinVarType._

case class Result(val name: String, val result: Any)

object ModelsResults {
  def ValueString(v: Any): String = {
    if (v == null) {
      return "null"
    }
    if (v.isInstanceOf[Set[_]]) {
      return v.asInstanceOf[Set[_]].mkString(",")
    }
    if (v.isInstanceOf[List[_]]) {
      return v.asInstanceOf[List[_]].mkString(",")
    }
    if (v.isInstanceOf[Array[_]]) {
      return v.asInstanceOf[Array[_]].mkString(",")
    }
    v.toString
  }

  /*
    def Deserialize(modelResults: Array[SavedMdlResult], dis: DataInputStream, mdResolver: MdBaseResolveInfo, loader: java.lang.ClassLoader, savedDataVersion: String): Unit = {

    }

    def Serialize(dos: DataOutputStream): Array[SavedMdlResult] = {
      null
    }
    */
}

/*
class SavedMdlResult {
  var mdlName: String = ""
  var mdlVersion: String = ""
  var uniqKey: String = ""
  var uniqVal: String = ""
  var txnId: Long = 0
  // Current message Index, In case if we have multiple Transformed messages for a given input message
  var xformedMsgCntr: Int = 0
  // Total transformed messages, In case if we have multiple Transformed messages for a given input message
  var totalXformedMsgs: Int = 0
  var mdlRes: ModelResultBase = null

  def withMdlName(mdl_Name: String): SavedMdlResult = {
    mdlName = mdl_Name
    this
  }

  def withMdlVersion(mdl_Version: String): SavedMdlResult = {
    mdlVersion = mdl_Version
    this
  }

  def withUniqKey(uniq_Key: String): SavedMdlResult = {
    uniqKey = uniq_Key
    this
  }

  def withUniqVal(uniq_Val: String): SavedMdlResult = {
    uniqVal = uniq_Val
    this
  }

  def withTxnId(txn_Id: Long): SavedMdlResult = {
    txnId = txn_Id
    this
  }

  def withXformedMsgCntr(xfrmedMsgCntr: Int): SavedMdlResult = {
    xformedMsgCntr = xfrmedMsgCntr
    this
  }

  def withTotalXformedMsgs(totalXfrmedMsgs: Int): SavedMdlResult = {
    totalXformedMsgs = totalXfrmedMsgs
    this
  }

  def withMdlResult(mdl_Res: ModelResultBase): SavedMdlResult = {
    mdlRes = mdl_Res
    this
  }

  def toJson: org.json4s.JsonAST.JObject = {
    val output = if (mdlRes == null) List[org.json4s.JsonAST.JObject]() else mdlRes.toJson
    val json =
      ("ModelName" -> mdlName) ~
        ("ModelVersion" -> mdlVersion) ~
        ("uniqKey" -> uniqKey) ~
        ("uniqVal" -> uniqVal) ~
        ("xformedMsgCntr" -> xformedMsgCntr) ~
        ("totalXformedMsgs" -> totalXformedMsgs) ~
        ("output" -> output)
    return json
  }

  override def toString: String = {
    compact(render(toJson))
  }
}
*/

trait ModelResultBase {
  def toJson: List[org.json4s.JsonAST.JObject]

  // Returns JSON string
  def toString: String

  // Get the value for the given key, if exists, otherwise NULL
  def get(key: String): Any

  // Return all key & values as Map of KeyValue pairs
  def asKeyValuesMap: Map[String, Any]

  // Deserialize this object
  def Deserialize(dis: DataInputStream): Unit

  // Serialize this object
  def Serialize(dos: DataOutputStream): Unit
}

// Keys are handled as case sensitive
class MappedModelResults extends ModelResultBase {
  val results = scala.collection.mutable.Map[String, Any]()

  def withResults(res: Array[Result]): MappedModelResults = {
    if (res != null) {
      res.foreach(r => {
        results(r.name) = r.result
      })

    }
    this
  }

  def withResults(res: Array[(String, Any)]): MappedModelResults = {
    if (res != null) {
      res.foreach(r => {
        results(r._1) = r._2
      })
    }
    this
  }

  def withResults(res: scala.collection.immutable.Map[String, Any]): MappedModelResults = {
    if (res != null) {
      res.foreach(r => {
        results(r._1) = r._2
      })
    }
    this
  }

  def withResult(res: Result): MappedModelResults = {
    if (res != null) {
      results(res.name) = res.result
    }
    this
  }

  def withResult(res: (String, Any)): MappedModelResults = {
    if (res != null) {
      results(res._1) = res._2
    }
    this
  }

  def withResult(key: String, value: Any): MappedModelResults = {
    if (key != null)
      results(key) = value
    this
  }

  override def toJson: List[org.json4s.JsonAST.JObject] = {
    val json =
      results.toList.map(r =>
        (("Name" -> r._1) ~
          ("Value" -> ModelsResults.ValueString(r._2))))
    return json
  }

  override def toString: String = {
    compact(render(toJson))
  }

  override def get(key: String): Any = {
    results.getOrElse(key, null)
  }

  override def asKeyValuesMap: Map[String, Any] = {
    results.toMap
  }

  override def Deserialize(dis: DataInputStream): Unit = {
    // BUGBUG:: Yet to implement
  }

  override def Serialize(dos: DataOutputStream): Unit = {
    // BUGBUG:: Yet to implement
  }
}

// case class ContainerNameAndDatastoreInfo(containerName: String, dataDataStoreInfo: String)

case class KeyValuePair(key: String, value: Any);
case class SerializerTypeValuePair(serializerType: String, value: Array[Byte]);

trait EnvContext /* extends Monitorable */  {
  // Metadata Ops
  var _mgr: MdMgr = _

  def setMdMgr(mgr: MdMgr): Unit

  def getPropertyValue(clusterId: String, key: String): String

//  def setClassLoader(cl: java.lang.ClassLoader): Unit
//  def getClassLoader: java.lang.ClassLoader

  def setMetadataLoader(metadataLoader: KamanjaLoaderInfo): Unit

  def getMetadataLoader: KamanjaLoaderInfo

  def setAdaptersAndEnvCtxtLoader(adaptersAndEnvCtxtLoader: KamanjaLoaderInfo): Unit

  def getAdaptersAndEnvCtxtLoader: KamanjaLoaderInfo

  def setMetadataResolveInfo(mdres: MdBaseResolveInfo): Unit

  // Setting JarPaths
  def setJarPaths(jarPaths: collection.immutable.Set[String]): Unit
  def getJarPaths(): collection.immutable.Set[String]

  // Datastores
  def setDefaultDatastore(dataDataStoreInfo: String): Unit

  // Registerd Messages/Containers
  //  def RegisterMessageOrContainers(containersInfo: Array[ContainerNameAndDatastoreInfo]): Unit

  // RDD Ops
  def getRecent(containerName: String, partKey: List[String], tmRange: TimeRange, f: ContainerInterface => Boolean): Option[ContainerInterface]

  def getRDD(containerName: String, partKey: List[String], tmRange: TimeRange, f: ContainerInterface => Boolean): Array[ContainerInterface]

//  def saveOne(transId: Long, containerName: String, partKey: List[String], value: ContainerInterface): Unit
//
//  def saveRDD(transId: Long, containerName: String, values: Array[ContainerInterface]): Unit

  // RDD Ops
  def Shutdown: Unit

  def getAllObjects(transId: Long, containerName: String): Array[ContainerInterface]

  def getObject(transId: Long, containerName: String, partKey: List[String], primaryKey: List[String]): ContainerInterface

  // if appendCurrentChanges is true return output includes the in memory changes (new or mods) at the end otherwise it ignore them.
  def getHistoryObjects(transId: Long, containerName: String, partKey: List[String], appendCurrentChanges: Boolean): Array[ContainerInterface]

  def setObject(transId: Long, containerName: String, partKey: List[String], value: ContainerInterface): Unit

  def contains(transId: Long, containerName: String, partKey: List[String], primaryKey: List[String]): Boolean

  def containsAny(transId: Long, containerName: String, partKeys: Array[List[String]], primaryKeys: Array[List[String]]): Boolean

  //partKeys.size should be same as primaryKeys.size
  def containsAll(transId: Long, containerName: String, partKeys: Array[List[String]], primaryKeys: Array[List[String]]): Boolean //partKeys.size should be same as primaryKeys.size

  // Adapters Keys & values
//  def setAdapterUniqueKeyValue(transId: Long, key: String, value: String, outputResults: List[(String, String, String)]): Unit

//  def getAdapterUniqueKeyValue(transId: Long, key: String): (Long, String, List[(String, String, String)])

//  def setAdapterUniqKeyAndValues(keyAndValues: List[(String, String)]): Unit

  def getAllAdapterUniqKvDataInfo(keys: Array[String]): Array[(String, (Long, String, List[(String, String, String)]))] // Get Status information from Final table. No Transaction required here.

  //  def getAllIntermediateCommittingInfo: Array[(String, (Long, String, List[(String, String)]))] // Getting intermediate committing information. Once we commit we don't have this, because we remove after commit

  //  def getAllIntermediateCommittingInfo(keys: Array[String]): Array[(String, (Long, String, List[(String, String)]))] // Getting intermediate committing information.

  //  def removeCommittedKey(transId: Long, key: String): Unit
  //  def removeCommittedKeys(keys: Array[String]): Unit

  // Model Results Saving & retrieving. Don't return null, always return empty, if we don't find
  //  def saveModelsResult(transId: Long, key: List[String], value: scala.collection.mutable.Map[String, SavedMdlResult]): Unit
  //
  //  def getModelsResult(transId: Long, key: List[String]): scala.collection.mutable.Map[String, SavedMdlResult]

  // Final Commit for the given transaction
  // outputResults has AdapterName, PartitionKey & Message
  def commitData(txnCtxt: TransactionContext): Unit

  def rollbackData(): Unit

  // Save State Entries on local node & on Leader
  // def PersistLocalNodeStateEntries: Unit
  // def PersistRemainingStateEntriesOnLeader: Unit

  // Clear Intermediate results before Restart processing
  //  def clearIntermediateResults: Unit

  // Clear Intermediate results After updating them on different node or different component (like KVInit), etc
  //  def clearIntermediateResults(unloadMsgsContainers: Array[String]): Unit

  // Changed Data & Reloading data are Time in MS, Bucket Key & TransactionId
  //  def getChangedData(tempTransId: Long, includeMessages: Boolean, includeContainers: Boolean): scala.collection.immutable.Map[String, List[Key]]

  //  def ReloadKeys(tempTransId: Long, containerName: String, keys: List[Key]): Unit

  // Set Reload Flag
  //  def setReloadFlag(transId: Long, containerName: String): Unit

  //  def PersistValidateAdapterInformation(validateUniqVals: Array[(String, String)]): Unit

  //  def GetValidateAdapterInformation: Array[(String, String)]

  /**
    * Answer an empty instance of the message or container with the supplied fully qualified class name.  If the name is
    * invalid, null is returned.
    *
    * @param fqclassname : a full package qualified class name
    * @return a MesssageContainerBase of that ilk
    */
  def NewMessageOrContainer(fqclassname: String): ContainerInterface

  def getContainerInstance(containerName: String): ContainerInterface

  // Just get the cached container key and see what are the containers we need to cache
  //  def CacheContainers(clusterId: String): Unit

  //  def EnableEachTransactionCommit: Boolean

  // Lock functions
//  def lockKeyInCluster(key: String): Unit
//
//  def lockKeyInNode(key: String): Unit
//
//  // Unlock functions
//  def unlockKeyInCluster(key: String): Unit
//
//  def unlockKeyInNode(key: String): Unit

//  def getAllClusterLocks(): Array[String]
//
//  def getAllNodeLocks(nodeId: String): Array[String]

  // Saving & getting temporary objects in cache
  // value should be Array[Byte]
  def saveConfigInClusterCache(key: String, value: Array[Byte]): Unit

  def saveObjectInNodeCache(key: String, value: Any): Unit

  def getConfigFromClusterCache(key: String): Array[Byte]

  def getObjectFromNodeCache(key: String): Any

  def getAllKeysFromClusterCache(): Array[String]

  def getAllKeysFromNodeCache(): Array[String]

  def getAllConfigFromClusterCache(): Array[KeyValuePair]

  def getAllObjectsFromNodeCache(): Array[KeyValuePair]

  // Saving & getting data (from cache or disk)
  def saveDataInPersistentStore(containerName: String, key: String, serializerType: String, value: Array[Byte]): Unit

  def getDataInPersistentStore(containerName: String, key: String): SerializerTypeValuePair

  // Zookeeper functions
  def setDataToZNode(zNodePath: String, value: Array[Byte]): Unit

  def getDataFromZNode(zNodePath: String): Array[Byte]

  def getTransactionRanges(): (Int, Int)

  def hasZkConnectionString(): Boolean

  def setZookeeperInfo(zkConnectString: String, zkBasePath: String, zkSessionTimeoutMs: Int, zkConnectionTimeoutMs: Int): Unit

  def getZookeeperInfo(): (String, String, Int, Int)

  def createZkPathListener(znodePath: String, ListenCallback: (String) => Unit): Unit

  def createZkPathChildrenCacheListener(znodePath: String, getAllChildsData: Boolean, ListenCallback: (String, String, Array[Byte], Array[(String, Array[Byte])]) => Unit): Unit

  // Cache Listeners
  def setListenerCacheKey(key: String, value: String): Unit

  // listenPath is the Path where it has to listen. Ex: /kamanja/notification/node1
  // ListenCallback is the call back called when there is any change in listenPath. The return value is has 3 components. 1 st is eventType, 2 is eventPath and 3rd is eventPathData
  // eventType is PUT, UPDATE, REMOVE etc
  // eventPath is the Path where it changed the data
  // eventPathData is the data of that path
  def createListenerForCacheKey(listenPath: String, ListenCallback: (String, String, String) => Unit): Unit

  // listenPath is the Path where it has to listen and its children
  //    Ex: If we start watching /kamanja/nodification/ all the following puts/updates/removes/etc will notify callback
  //    /kamanja/nodification/node1/1 or /kamanja/nodification/node1/2 or /kamanja/nodification/node1 or /kamanja/nodification/node2 or /kamanja/nodification/node3 or /kamanja/nodification/node4
  // ListenCallback is the call back called when there is any change in listenPath and or its children. The return value is has 3 components. 1 st is eventType, 2 is eventPath and 3rd is eventPathData
  // eventType is PUT, UPDATE, REMOVE etc
  // eventPath is the Path where it changed the data
  // eventPathData is the data of that path
  // eventType: String, eventPath: String, eventPathData: Array[Byte]
  def createListenerForCacheChildern(listenPath: String, ListenCallback: (String, String, String) => Unit): Unit

  // Leader Information
  def getClusterInfo(): ClusterStatus

  // This will give either any node change or leader change
  def registerNodesChangeNotification(EventChangeCallback: (ClusterStatus) => Unit): Unit

//  def unregisterNodesChangeNotification(EventChangeCallback: (ClusterStatus) => Unit): Unit

  def getNodeId(): String
  def getClusterId(): String

  def setNodeInfo(nodeId: String, clusterId: String): Unit

  // This post the message into where ever these messages are associated immediately
  // Later this will be posted to logical queue where it can execute on logical partition.
  def postMessages(msgs: Array[ContainerInterface]): Unit

  // We are handling only one listener at this moment
  def postMessagesListener(postMsgListenerCallback: (Array[ContainerInterface]) => Unit): Unit

  def getPrimaryDatastoreForTenantId(tenantId: String): String

  def setSystemCatalogDatastore(sysCatalog: String): Unit
  def getSystemCatalogDatastore(): String

  def startCache(conf: CacheConfig): Unit
  def getCacheConfig(): CacheConfig
}

// partitionKey is the one used for this message
class ModelContext(val txnContext: TransactionContext, val msg: ContainerInterface, val msgData: Array[Byte], val partitionKey: String) {
  def InputMessageData: Array[Byte] = msgData

  def Message: ContainerInterface = msg

  def TransactionContext: TransactionContext = txnContext

  def PartitionKey: String = partitionKey

  def getPropertyValue(clusterId: String, key: String): String = (txnContext.getPropertyValue(clusterId, key))
}

abstract class ModelBase(var modelContext: ModelContext, val factory: ModelBaseObj) {
  final def EnvContext() = if (modelContext != null && modelContext.txnContext != null && modelContext.txnContext.nodeCtxt != null) modelContext.txnContext.nodeCtxt.getEnvCtxt() else null

  // Model Name
  final def ModelName() = factory.ModelName()

  // Model Version
  final def Version() = factory.Version()

  // Tenant Id
  final def TenantId() = OwnerId()

  // OwnerId Id
  final def OwnerId(): String = null

  final def TransId() = if (modelContext != null && modelContext.txnContext != null) modelContext.txnContext.getTransactionId() else null // transId

  // Instance initialization. Once per instance
  def init(partitionHash: Int): Unit = {}

  // Shutting down the instance
  def shutdown(): Unit = {}

  // if outputDefault is true we will output the default value if nothing matches, otherwise null
  def execute(outputDefault: Boolean): ModelResultBase

  // Can we reuse the instances created for this model?
  def isModelInstanceReusable(): Boolean = false
}

trait ModelBaseObj {
  // Check to fire the model
  def IsValidMessage(msg: ContainerInterface): Boolean

  // Creating same type of object with given values
  def CreateNewModel(mdlCtxt: ModelContext): ModelBase

  // Model Name
  def ModelName(): String

  // Model Version
  def Version(): String

  // ResultClass associated the model. Mainly used for Returning results as well as Deserialization
  def CreateResultObject(): ModelResultBase
}

// ModelInstance will be created from ModelInstanceFactory by demand.
//	If ModelInstanceFactory:isModelInstanceReusable returns true, engine requests one ModelInstance per partition.
//	If ModelInstanceFactory:isModelInstanceReusable returns false, engine requests one ModelInstance per input message related to this model (message is validated with ModelInstanceFactory.isValidMessage).
abstract class ModelInstance(val factory: ModelInstanceFactory) {
  // Getting NodeContext from ModelInstanceFactory
  final def getNodeContext() = factory.getNodeContext()

  // Getting EnvContext from ModelInstanceFactory
  final def getEnvContext() = factory.getEnvContext()

  // Getting ModelName from ModelInstanceFactory
  final def getModelName() = factory.getModelName()

  // Getting Model Version from ModelInstanceFactory
  final def getVersion() = factory.getVersion()

  // Getting ModelInstanceFactory, which is passed in constructor
  final def getModelInstanceFactory() = factory

  // This calls when the instance got created. And only calls once per instance.
  //	Intput:
  //		instanceMetadata: Metadata related to this instance (partition information)
  def init(instanceMetadata: String): Unit = {} // Local Instance level initialization

  // This calls when the instance is shutting down. There is no guarantee
  def shutdown(): Unit = {} // Shutting down this factory. 

  // This calls for each input message related to this model (message is validated with ModelInstanceFactory.isValidMessage)
  //	Intput:
  //		txnCtxt: Transaction context related to this execution
  //		outputDefault: If this is true, engine is expecting output always.
  //	Output:
  //		Derived class of ModelResultBase is the return results expected. null if no results.
  // This call is deprecated and valid for models upto 1.3.x. Use run method for new implementation
  def execute(txnCtxt: TransactionContext, outputDefault: Boolean): ModelResultBase = {
    throw new DeprecatedException("Deprecated", null)
  }

  //	Intput:
  //		txnCtxt: Transaction context related to this execution
  //		outputDefault: If this is true, engine is expecting output always.
  //	Output:
  //		Derived messages are the return results expected.
  def execute(txnCtxt: TransactionContext, execMsgsSet: Array[ContainerOrConcept], triggerdSetIndex: Int, outputDefault: Boolean): Array[ContainerOrConcept] = {
    // Default implementation to invoke old model
    if (execMsgsSet.size == 1 && triggerdSetIndex == 0 && factory.getModelDef().inputMsgSets.size == 1 && factory.getModelDef().inputMsgSets(0).size == 1 &&
      execMsgsSet(0).isInstanceOf[ContainerInterface] && factory.getModelDef().outputMsgs.size == 1) {
      // This could be calling old model
      // Holding current transaction information original message and set the new information. Because the model will pull the message from transaction context
      val (origin, orgInputMsg) = txnCtxt.getInitialMessage
      var returnValues = Array[ContainerOrConcept]()
      try {
        txnCtxt.setInitialMessage("", execMsgsSet(0).asInstanceOf[ContainerInterface], false)
        val mdlResults = execute(txnCtxt, outputDefault)
        if (mdlResults != null) {
          val outContainer = getEnvContext().getContainerInstance(factory.getModelDef().outputMsgs(0))
          val resMap = mdlResults.asKeyValuesMap
          resMap.foreach(kv => {
            outContainer.set(kv._1, kv._2)
          })
          returnValues = Array[ContainerOrConcept](outContainer)
        }
      } catch {
        case e: Exception => throw e
      } finally {
        txnCtxt.setInitialMessage(origin, orgInputMsg, false)
      }
      returnValues
    }
    throw new NotImplementedFunctionException("Not implemented", null)
  }
}

// ModelInstanceFactory will be created from FactoryOfModelInstanceFactory when metadata got resolved (while engine is starting and when metadata adding while running the engine).
abstract class ModelInstanceFactory(val modelDef: ModelDef, val nodeContext: NodeContext) {
  // Getting NodeContext, which is passed in constructor
  final def getNodeContext() = nodeContext

  // Getting EnvContext from nodeContext, if available
  final def getEnvContext() = if (nodeContext != null) nodeContext.getEnvCtxt else null

  // Getting ModelDef, which is passed in constructor
  final def getModelDef() = modelDef

  // This calls when the instance got created. And only calls once per instance.
  // Common initialization for all Model Instances. This gets called once per node during the metadata load or corresponding model def change.
  //	Intput:
  //		txnCtxt: Transaction context to do get operations on this transactionid. But this transaction will be rolledback once the initialization is done.
  def init(txnContext: TransactionContext): Unit = {}

  // This calls when the factory is shutting down. There is no guarantee.
  def shutdown(): Unit = {} // Shutting down this factory. 

  // Getting ModelName.
  def getModelName(): String // Model Name

  // Getting Model Version
  def getVersion(): String // Model Version

  // Checking whether the message is valid to execute this model instance or not.
  // Deprecated and no more supported in new versions from 1.4.0
  def isValidMessage(msg: ContainerInterface): Boolean = {
    throw new DeprecatedException("Deprecated", null)
  }

  // Creating new model instance related to this ModelInstanceFactory.
  def createModelInstance(): ModelInstance

  // Creating ModelResultBase associated this model/modelfactory.
  def createResultObject(): ModelResultBase

  // Is the ModelInstance created by this ModelInstanceFactory is reusable?
  def isModelInstanceReusable(): Boolean = false
}

trait FactoryOfModelInstanceFactory {
  def getModelInstanceFactory(modelDef: ModelDef, nodeContext: NodeContext, loaderInfo: KamanjaLoaderInfo, jarPaths: collection.immutable.Set[String]): ModelInstanceFactory

  // Input:
  //  modelDefStr is Model Definition String
  //  inpMsgName is Input Message Name
  //  outMsgName is output Message Name
  // Output: ModelDef
  def prepareModel(nodeContext: NodeContext, modelDefStr: String, inpMsgName: String, outMsgName: String, loaderInfo: KamanjaLoaderInfo, jarPaths: collection.immutable.Set[String]): ModelDef
}

case class EventOriginInfo(val key: String, val value: String)

// FIXME: Need to have message creator (Model/InputMessage/Get(from db/cache))
class TransactionContext(val transId: Long, val nodeCtxt: NodeContext, val msgData: Array[Byte], val origin: EventOriginInfo, val eventEpochStartTimeInMs: Long, val msgEvent: ContainerInterface) {
  case class ContaienrWithOriginAndPartKey(origin: String, container: ContainerOrConcept, partKey: List[String])

  private var orgInputMsg = ContaienrWithOriginAndPartKey(null, null, null)
  private val containerOrConceptsMapByName = scala.collection.mutable.Map[String, ArrayBuffer[ContaienrWithOriginAndPartKey]]() // Saving by Name
  // private val containerOrConceptsMapByElementId = scala.collection.mutable.Map[Long, ArrayBuffer[ContaienrWithOriginAndPartKey]]() // Saving by ElementId
  // orgInputMsg & msgEvent is part in this also
  private val valuesMap = new java.util.HashMap[String, Any]()

  addContainerOrConcept("", msgEvent, null);

  final def getMessageEvent: ContainerInterface = msgEvent

  final def getInputMessageData(): Array[Byte] = msgData

  final def getPartitionKey(): String = origin.key

  final def getMessage(): ContainerInterface = orgInputMsg.container.asInstanceOf[ContainerInterface] // Original messages

  // Need to lock if we are going to run models parallel
  final def getContainersOrConcepts(typeName: String): Array[(String, ContainerOrConcept)] = containerOrConceptsMapByName.getOrElse(typeName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).map(v => (v.origin, v.container)).toArray

  // Need to lock if we are going to run models parallel
  final def getContainersOrConcepts(origin: String, typeName: String): Array[(String, ContainerOrConcept)] = {
    containerOrConceptsMapByName.getOrElse(typeName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => m.origin.equalsIgnoreCase(origin)).map(v => (v.origin, v.container)).toArray
  }

  // Need to lock if we are going to run models parallel
  final def addContainerOrConcept(origin: String, m: ContainerOrConcept, partKey: List[String]): Unit = {
    if (m == null) return
    val msgNm = m.getFullTypeName.toLowerCase()
    val tmp = containerOrConceptsMapByName.getOrElse(msgNm, ArrayBuffer[ContaienrWithOriginAndPartKey]())
    tmp += ContaienrWithOriginAndPartKey(origin, m, partKey)
    containerOrConceptsMapByName(msgNm) = tmp
  }

  final def addContainerOrConcepts(origin: String, curMsgs: Array[ContainerOrConcept]): Unit = {
    curMsgs.foreach(m => addContainerOrConcept(origin, m, null))
  }

  final def addContainerOrConcepts(origin: String, curMsgs: Array[ContainerInterface]): Unit = {
    curMsgs.foreach(m => addContainerOrConcept(origin, m, null))
  }

  final def setInitialMessage(origin: String, orgMsg: ContainerOrConcept, addToAllMsgs: Boolean = true): Unit = {
    orgInputMsg = ContaienrWithOriginAndPartKey(origin, orgMsg, null)
    if (addToAllMsgs)
      addContainerOrConcept(origin, orgMsg, null)
  }

  final def getInitialMessage: (String, ContainerOrConcept) = {
    (orgInputMsg.origin, orgInputMsg.container)
  }

  final def getTransactionId() = transId

  final def getNodeCtxt() = nodeCtxt

  // Need to lock if we are going to run models parallel
  final def getPropertyValue(clusterId: String, key: String): String = {
    if (nodeCtxt != null) nodeCtxt.getPropertyValue(clusterId, key) else ""
  }

  // Need to lock if we are going to run models parallel
  final def putValue(key: String, value: Any): Unit = {
    valuesMap.put(key, value)
  }

  // Need to lock if we are going to run models parallel
  final def getValue(key: String): Any = {
    valuesMap.get(key)
  }

  final def setContextValue(key: String, value: Any): Unit = {
    putValue(key, value)
  }

  // Deprecated. Use putValue
  final def getContextValue(key: String): Any = {
    getValue(key)
  } // Deprecated. Use getValue

  final def IsEmptyKey(key: List[String]): Boolean = {
    (key == null || key.size == 0 /* || key.filter(k => k != null).size == 0 */)
  }

  final def IsEmptyKey(key: Array[String]): Boolean = {
    (key == null || key.size == 0 /* || key.filter(k => k != null).size == 0 */)
  }

  final def IsSameKey(key1: List[String], key2: List[String]): Boolean = {
    if (key1.size != key2.size)
      return false

    for (i <- 0 until key1.size) {
      if (key1(i).compareTo(key2(i)) != 0)
        return false
    }

    return true
  }

  final def IsSameKey(key1: Array[String], key2: Array[String]): Boolean = {
    if (key1.size != key2.size)
      return false

    for (i <- 0 until key1.size) {
      if (key1(i).compareTo(key2(i)) != 0)
        return false
    }

    return true
  }

  // containerName is full name of the message
  private def getContainersList(containerName: String, partKey: List[String], tmRange: TimeRange, f: ContainerInterface => Boolean): Array[(String, ContainerInterface)] = {
    var tmpMsgs = Array[ContaienrWithOriginAndPartKey]()

    // Try to get from local cache
    if (IsEmptyKey(partKey) == false) {
      val tmRng =
        if (tmRange == null)
          TimeRange(Long.MinValue, Long.MaxValue)
        else
          tmRange

      val partKeyAsArray = partKey.toArray
      if (f != null) {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => {
          val retVal = if (m.container.isInstanceOf[ContainerInterface]) {
            val container = m.container.asInstanceOf[ContainerInterface]
            val tmData = container.getTimePartitionData
            tmData >= tmRange.beginTime && tmData <= tmRange.endTime && IsSameKey(partKeyAsArray, container.getPartitionKey) && f(container) // Comparing time & Key & function call
          } else {
            false
          }
          retVal
        }).toArray
      } else {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => {
          val retVal = if (m.container.isInstanceOf[ContainerInterface]) {
            val container = m.container.asInstanceOf[ContainerInterface]
            val tmData = container.getTimePartitionData
            tmData >= tmRange.beginTime && tmData <= tmRange.endTime && IsSameKey(partKeyAsArray, container.getPartitionKey) // Comparing time & Key
          } else {
            false
          }
          retVal
        }).toArray
      }
    } else if (tmRange != null) {
      if (f != null) {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => {
          val retVal = if (m.container.isInstanceOf[ContainerInterface]) {
            val container = m.container.asInstanceOf[ContainerInterface]
            val tmData = container.getTimePartitionData
            tmData >= tmRange.beginTime && tmData <= tmRange.endTime && f(container) // Comparing time & function call
          } else {
            false
          }
          retVal
        }).toArray
      } else {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => {
          val retVal = if (m.container.isInstanceOf[ContainerInterface]) {
            val container = m.container.asInstanceOf[ContainerInterface]
            val tmData = container.getTimePartitionData
            tmData >= tmRange.beginTime && tmData <= tmRange.endTime // Comparing time
          } else {
            false
          }
          retVal
        }).toArray
      }
    } else {
      if (f != null) {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => {
          val retVal = if (m.container.isInstanceOf[ContainerInterface]) {
            f(m.container.asInstanceOf[ContainerInterface]) // Comparing function call
          } else {
            false
          }
          retVal
        }).toArray
      } else {
        tmpMsgs = containerOrConceptsMapByName.getOrElse(containerName.toLowerCase(), ArrayBuffer[ContaienrWithOriginAndPartKey]()).filter(m => m.container.isInstanceOf[ContainerInterface]).toArray
      }
    }

    return tmpMsgs.map(v => (v.origin, v.container.asInstanceOf[ContainerInterface]))
  }

  // containerName is full name of the message
  final def getRecent(containerName: String, partKey: List[String], tmRange: TimeRange, f: ContainerInterface => Boolean): Option[ContainerInterface] = {
    var tmpMsgs = getContainersList(containerName, partKey, tmRange, f)
    if (tmpMsgs.size > 0)
      return Some(tmpMsgs(tmpMsgs.size - 1)._2) // Take the last one

    if (getNodeCtxt != null && getNodeCtxt.getEnvCtxt != null)
      return getNodeCtxt.getEnvCtxt.getRecent(containerName, partKey, tmRange, f)
    None
  }

  final def getRDD(containerName: String, partKey: List[String], tmRange: TimeRange, f: ContainerInterface => Boolean): Array[ContainerInterface] = {
    val tmpList =
      if (getNodeCtxt != null && getNodeCtxt.getEnvCtxt != null)
        getNodeCtxt.getEnvCtxt.getRDD(containerName, partKey, tmRange, f)
      else
        Array[ContainerInterface]()

    // Now override the current transactionids list on top of this
    val currentList = getContainersList(containerName, partKey, tmRange, f)
    val finalList =
      if (currentList.size > 0 && tmpList.size > 0) {
        //BUGBUG:: Yet to fix -- Same timeRange, Partition Key & PrimaryKeys need to be updated with new ones
        //FIXME:- Fix this -- Same timeRange, Partition Key & PrimaryKeys need to be updated with new ones
        // May be we can create the map of current list and loop thru the list we got tmpList
        tmpList
      } else if (currentList.size > 0) {
        currentList.map(m => m._2)
      } else {
        tmpList
      }

    finalList
  }

  final def saveOne(value: ContainerInterface): Unit = {
    addContainerOrConcept("", value, null)
  }

  final def saveOne(partKey: List[String], value: ContainerInterface): Unit = {
    addContainerOrConcept("", value, partKey)
  }

  final def saveRDD(values: Array[ContainerInterface]): Unit = {
    addContainerOrConcepts("", values)
  }
}

// Node level context
class NodeContext(val gCtx: EnvContext) {
  def getEnvCtxt() = gCtx

  private val valuesMap = new java.util.HashMap[String, Any]()

  def getPropertyValue(clusterId: String, key: String): String = {
    if (gCtx != null) gCtx.getPropertyValue(clusterId, key) else ""
  }

  def putValue(key: String, value: Any): Unit = {
    valuesMap.put(key, value)
  }

  def getValue(key: String): Any = {
    valuesMap.get(key)
  }
}

// 1.1.x/Berkeley1.2 compatible models for execution purpose without changing much in the execution path -- Begin
class ModelBaseMdlInstance(factory: ModelBaseObjMdlInstanceFactory) extends ModelInstance(factory.asInstanceOf[ModelInstanceFactory]) {
  var mdlInst: ModelBase = null

  // Caching if the model is cached. Not using any locks or anything at this moment.
  override def execute(txnCtxt: TransactionContext, outputDefault: Boolean): ModelResultBase = {
    val modelContext = new ModelContext(txnCtxt, txnCtxt.getMessage(), txnCtxt.getInputMessageData(), txnCtxt.getPartitionKey())

    if (mdlInst == null) {
      mdlInst = factory.mdlBaseObj.CreateNewModel(modelContext)
      mdlInst.init(txnCtxt.getPartitionKey().hashCode())
    }
    else {
      mdlInst.modelContext = modelContext
    }

    mdlInst.execute(outputDefault)
  }
}

class ModelBaseObjMdlInstanceFactory(modelDef: ModelDef, nodeContext: NodeContext, val mdlBaseObj: ModelBaseObj) extends ModelInstanceFactory(modelDef, nodeContext) {
  override def getModelName() = mdlBaseObj.ModelName() // Model Name

  override def getVersion() = mdlBaseObj.Version() // Model Version

  override def isValidMessage(msg: ContainerInterface): Boolean = mdlBaseObj.IsValidMessage(msg)

  override def createModelInstance() = new ModelBaseMdlInstance(this)

  override def createResultObject() = mdlBaseObj.CreateResultObject()

  override def isModelInstanceReusable(): Boolean = {
    // Temporary Transaction context & model contexts
    val tmpTxnCtxt = new TransactionContext(0, nodeContext, Array[Byte](), EventOriginInfo(null, null), 0, null)
    val modelContext = new ModelContext(tmpTxnCtxt, null, tmpTxnCtxt.getInputMessageData(), tmpTxnCtxt.getPartitionKey())
    mdlBaseObj.CreateNewModel(modelContext).isModelInstanceReusable()
  }
}

// 1.1.x/Berkeley1.2 compatible models for execution purpose without changing much in the execution path -- End
