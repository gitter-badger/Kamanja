
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

import scala.collection.immutable.Map
import com.ligadata.Utils.Utils
import com.ligadata.kamanja.metadata.MdMgr
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import java.io.{ DataInputStream, DataOutputStream }
import com.ligadata.KvBase.{ TimeRange }
import com.ligadata.KvBase.{ Key, Value, TimeRange/* , KvBaseDefalts, KeyWithBucketIdAndPrimaryKey, KeyWithBucketIdAndPrimaryKeyCompHelper */ }

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

  def Deserialize(modelResults: Array[SavedMdlResult], dis: DataInputStream, mdResolver: MdBaseResolveInfo, loader: java.lang.ClassLoader, savedDataVersion: String): Unit = {

  }

  def Serialize(dos: DataOutputStream): Array[SavedMdlResult] = {
    null
  }
}

class SavedMdlResult {
  var mdlName: String = ""
  var mdlVersion: String = ""
  var uniqKey: String = ""
  var uniqVal: String = ""
  var txnId: Long = 0
  var xformedMsgCntr: Int = 0 // Current message Index, In case if we have multiple Transformed messages for a given input message
  var totalXformedMsgs: Int = 0 // Total transformed messages, In case if we have multiple Transformed messages for a given input message
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

trait ModelResultBase {
  def toJson: List[org.json4s.JsonAST.JObject]
  def toString: String // Returns JSON string
  def get(key: String): Any // Get the value for the given key, if exists, otherwise NULL
  def asKeyValuesMap: Map[String, Any] // Return all key & values as Map of KeyValue pairs
  def Deserialize(dis: DataInputStream): Unit // Serialize this object
  def Serialize(dos: DataOutputStream): Unit // Deserialize this object
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

case class ContainerNameAndDatastoreInfo(containerName: String, dataDataStoreInfo: String)

trait EnvContext {
  // Metadata Ops
  var _mgr: MdMgr = _
  def setMdMgr(mgr: MdMgr): Unit
  def getPropertyValue(clusterId: String, key: String): String
  def SetClassLoader(cl: java.lang.ClassLoader): Unit
  def SetMetadataResolveInfo(mdres: MdBaseResolveInfo): Unit

  // Setting JarPaths
  def SetJarPaths(jarPaths: collection.immutable.Set[String]): Unit

  // Datastores
  def SetDefaultDatastore(dataDataStoreInfo: String): Unit
  def SetStatusInfoDatastore(statusDataStoreInfo: String): Unit

  // Registerd Messages/Containers
  def RegisterMessageOrContainers(containersInfo: Array[ContainerNameAndDatastoreInfo]): Unit

  // RDD Ops
  def getRecent(transId: Long, containerName: String, partKey: List[String], tmRange: TimeRange, f: MessageContainerBase => Boolean): Option[MessageContainerBase]
  def getRDD(transId: Long, containerName: String, partKey: List[String], tmRange: TimeRange, f: MessageContainerBase => Boolean): Array[MessageContainerBase]
  def saveOne(transId: Long, containerName: String, partKey: List[String], value: MessageContainerBase): Unit
  def saveRDD(transId: Long, containerName: String, values: Array[MessageContainerBase]): Unit

  // RDD Ops
  def Shutdown: Unit
  def getAllObjects(transId: Long, containerName: String): Array[MessageContainerBase]
  def getObject(transId: Long, containerName: String, partKey: List[String], primaryKey: List[String]): MessageContainerBase
  def getHistoryObjects(transId: Long, containerName: String, partKey: List[String], appendCurrentChanges: Boolean): Array[MessageContainerBase] // if appendCurrentChanges is true return output includes the in memory changes (new or mods) at the end otherwise it ignore them.
  def setObject(transId: Long, containerName: String, partKey: List[String], value: MessageContainerBase): Unit
  def contains(transId: Long, containerName: String, partKey: List[String], primaryKey: List[String]): Boolean
  def containsAny(transId: Long, containerName: String, partKeys: Array[List[String]], primaryKeys: Array[List[String]]): Boolean //partKeys.size should be same as primaryKeys.size  
  def containsAll(transId: Long, containerName: String, partKeys: Array[List[String]], primaryKeys: Array[List[String]]): Boolean //partKeys.size should be same as primaryKeys.size

  // Adapters Keys & values
  def setAdapterUniqueKeyValue(transId: Long, key: String, value: String, outputResults: List[(String, String, String)]): Unit
  def getAdapterUniqueKeyValue(transId: Long, key: String): (Long, String, List[(String, String, String)])
  /*
  def getAllIntermediateStatusInfo: Array[(String, (String, Int, Int))] // Get all Status information from intermediate table. No Transaction required here.
  def getIntermediateStatusInfo(keys: Array[String]): Array[(String, (String, Int, Int))] // Get Status information from intermediate table for given keys. No Transaction required here.
*/
  def getAllAdapterUniqKvDataInfo(keys: Array[String]): Array[(String, (Long, String, List[(String, String, String)]))] // Get Status information from Final table. No Transaction required here.

//  def getAllIntermediateCommittingInfo: Array[(String, (Long, String, List[(String, String)]))] // Getting intermediate committing information. Once we commit we don't have this, because we remove after commit

//  def getAllIntermediateCommittingInfo(keys: Array[String]): Array[(String, (Long, String, List[(String, String)]))] // Getting intermediate committing information.

//  def removeCommittedKey(transId: Long, key: String): Unit
//  def removeCommittedKeys(keys: Array[String]): Unit

  // Model Results Saving & retrieving. Don't return null, always return empty, if we don't find
  def saveModelsResult(transId: Long, key: List[String], value: scala.collection.mutable.Map[String, SavedMdlResult]): Unit
  def getModelsResult(transId: Long, key: List[String]): scala.collection.mutable.Map[String, SavedMdlResult]

  // Final Commit for the given transaction
  // outputResults has AdapterName, PartitionKey & Message
  def commitData(transId: Long, key: String, value: String, outputResults: List[(String, String, String)]): Unit

  // Save State Entries on local node & on Leader
  // def PersistLocalNodeStateEntries: Unit
  // def PersistRemainingStateEntriesOnLeader: Unit

  // Clear Intermediate results before Restart processing
  def clearIntermediateResults: Unit

  // Clear Intermediate results After updating them on different node or different component (like KVInit), etc
  def clearIntermediateResults(unloadMsgsContainers: Array[String]): Unit

  // Changed Data & Reloading data are Time in MS, Bucket Key & TransactionId
  def getChangedData(tempTransId: Long, includeMessages: Boolean, includeContainers: Boolean): scala.collection.immutable.Map[String, Array[Key]]
  def ReloadKeys(tempTransId: Long, containerName: String, keys: List[Key]): Unit

  // Set Reload Flag
//  def setReloadFlag(transId: Long, containerName: String): Unit

  def PersistValidateAdapterInformation(validateUniqVals: Array[(String, String)]): Unit
  def GetValidateAdapterInformation: Array[(String, String)]

  /**
   *  Answer an empty instance of the message or container with the supplied fully qualified class name.  If the name is
   *  invalid, null is returned.
   *  @param fqclassname : a full package qualified class name
   *  @return a MesssageContainerBase of that ilk
   */
  def NewMessageOrContainer(fqclassname: String): MessageContainerBase
}

abstract class ModelBase(val modelContext: ModelContext, val factory: ModelBaseObj) {
    /**
     * Answer the model's namespace.name.
     */
    final def ModelName() : String = if (modelContext != null && modelContext.modelName != null) modelContext.modelName else factory.ModelName()

    /**
     * Answer the model version.
     */
    final def Version() : String = if (modelContext != null && modelContext.modelVersion != null) modelContext.modelVersion else factory.Version()

    /**
     * Answer the EnvContext that provides access to the persistent storage for models that wish to fetch/store values there
     * during its execution.
     */
    final def EnvContext() = if (modelContext != null && modelContext.txnContext != null) modelContext.txnContext.gCtx else null
    /**
     * Answer the model's owner or tenant. This is useful for cluster accounting in multi-tenant situations..
      */
    final def TenantId() = if (modelContext != null && modelContext.txnContext != null) modelContext.txnContext.tenantId else null
    /**
     * Answer the transaction id for the current model execution.
      */
    final def TransId() = if (modelContext != null && modelContext.txnContext != null) modelContext.txnContext.transId else null // transId

    /**
     * The engine will call the model instance's execute method to process the message it received at CreateNewModel time by its factory.
     * @param outputDefault when true, a model result will always be produced with default values.  If false (ordinary case), output is
     *                      emitted only when the model deems this message worthy of report.  If desired the model may return a 'null'
     *                      for the execute's return value and the engine will not proceed with output processing
     * @return a ModelResultBase derivative or null if there is nothing to report.
     */
    def execute(outputDefault: Boolean): ModelResultBase // if outputDefault is true we will output the default value if nothing matches, otherwise null
}

trait ModelBaseObj {
    /**
     * Determine if the supplied message can be consumed by the model mentioned in the argument list.  The engine will
     * call this method when a new messages has arrived and been prepared.  It is passed to each of the active models
     * in the working set.  Each model has the opportunity to indicate its interest in the message.
     *
     * NOTE: For many model factories that implement this interface, there is only one model to be concerned with and the
     * namespace.name.version can be ignored. Some factories, however, are responsible for servicing many models, so
     * the Kamanja engine's intentions are made known explicitly as to which active model it is currently concerned.
     *
     * @param msg  the message instance that is currently being processed
     * @param modelName the namespace.name of the model that the engine wishes to know if it can process this message
     * @param modelVersion the canonical version of the model (string form) that the engine wishes to know if it can
     *                     process this message
     * @return true if this model can process the message.
     */
  def IsValidMessage(msg: MessageContainerBase, modelName : String, modelVersion : String): Boolean
    /**
     * If the message can be processed, the engine will call this method to get an instance of the model.  Depending upon the model
     * characteristics, it will either obtain one from its instance cache (models that are reusable behave this way), or worse case,
     * instantiate a new model to process the message
     * @param mdlCtxt key information needed by the model to create and intialize itself.
     * @return an instance of the Model that can process the message found in the ModelContext
     */
  def CreateNewModel(mdlCtxt: ModelContext): ModelBase
    /**
     * Answer the name of the model.
     * @return the model name
     */
  def ModelName(): String
    /**
     * Answer the version of the model.
     * @return the model version
     */
  def Version(): String

    /**
     * Create a result object to contain any results the model wishes to report
     * @return a ModelResultBase derivative appropriate for the model
     */
  def CreateResultObject(): ModelResultBase // ResultClass associated the model. Mainly used for Returning results as well as Deserialization
}

/**
 * ModelInfo objects are created at cluster startup and cache information required to manage the creation of models and their
 * execution in the Kamanja engine.
 * @param mdl the factory object that will be asked to decide if the current message can be consumed by an instance of a model
 *            that it can create.
 * @param modelName the namespace.name of the model that the engine wishes to know if it can process this message
 * @param modelVersion the version of the model that the engine wishes to know if it can process this message
 * @param jarPath the location(s) of all jars required to execute models that can be produced by this factory object
 * @param dependencyJarNames the names of the dependency jars that are in fact needed
 * @param tenantId the name of the model owner used for multi-tenancy accounting and security
 */
class ModelInfo(val mdl: ModelBaseObj
                , val modelName : String
                , val modelVersion : String
                , val jarPath: String
                , val dependencyJarNames: Array[String]
                , val tenantId: String) {
}

/**
 * A ModelContext is presented to each model instance when a new message is to be processed by that model instance.
 * The current transaction, access to the persistent store, the model owner, and the message to be processed are available.
 *
 * @param txnContext the TransactionContext describing the transaction id, global context (persistent store interface) and
 *                   tenant id (used for multi tenancy clusters and the accounting required for that).
 * @param msg the instance of the incoming message to be consumed by the model instance.
 * @param modelName the namespace.name of the model that the engine is invoking
 * @param modelVersion the version of the model that the engine wishes to know if it can process this message
 */
class ModelContext(val txnContext: TransactionContext
                   , val msg: MessageContainerBase
                   , val modelName : String
                   , val modelVersion : String ) {
  def getPropertyValue(clusterId: String, key:String): String = (txnContext.getPropertyValue(clusterId, key))
}

class TransactionContext(val transId: Long, val gCtx: EnvContext, val tenantId: String) {
  def getPropertyValue(clusterId: String, key: String): String = { gCtx.getPropertyValue(clusterId, key) }
}

