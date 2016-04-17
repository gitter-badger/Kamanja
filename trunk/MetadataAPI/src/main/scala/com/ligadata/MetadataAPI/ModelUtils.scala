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

package com.ligadata.MetadataAPI

import java.util.Properties
import java.io._
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date
import java.text.ParseException
import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.MetadataAPI.MetadataAPI.ModelType.ModelType

import scala.Enumeration
import scala.io._
import scala.collection.mutable.ArrayBuffer

import scala.collection.mutable._
import scala.reflect.runtime.{universe => ru}

import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MdMgr._

import com.ligadata.kamanja.metadataload.MetadataLoad

// import com.ligadata.keyvaluestore._
import com.ligadata.HeartBeat.{MonitoringContext, HeartBeatUtil}
import com.ligadata.StorageBase.{DataStore, Transaction}
import com.ligadata.KvBase.{Key, TimeRange}

import scala.util.parsing.json.JSON
import scala.util.parsing.json.{JSONObject, JSONArray}
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap

import com.google.common.base.Throwables

import com.ligadata.msgcompiler._
import com.ligadata.Exceptions._

import scala.xml.XML
import org.apache.logging.log4j._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import com.ligadata.ZooKeeper._
import org.apache.curator.framework.CuratorFramework
import org.apache.zookeeper.CreateMode

import com.ligadata.keyvaluestore._
import com.ligadata.Serialize._
import com.ligadata.Utils._
import scala.util.control.Breaks._
import com.ligadata.AuditAdapterInfo._
import com.ligadata.SecurityAdapterInfo.SecurityAdapter
import com.ligadata.keyvaluestore.KeyValueManager
import com.ligadata.Exceptions.StackTrace

import java.util.Date

// The implementation class
object ModelUtils {

  lazy val sysNS = "System"
  // system name space
  lazy val serializerType = "kryo"
  lazy val serializer = SerializerManager.GetSerializer(serializerType)
  private[this] val lock = new Object

  /**
    * Deactivate the model that presumably is active and waiting for input in the working set of the cluster engines.
    *
    * @param nameSpace namespace of the object
    * @param name
    * @param version   Version of the object
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def DeactivateModel(nameSpace: String, name: String, version: Long, userid: Option[String] = None): String = {
    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version)
    MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.DEACTIVATEOBJECT, AuditConstants.MODEL, AuditConstants.SUCCESS, "", dispkey)
    if (DeactivateLocalModel(nameSpace, name, version)) {
      (new ApiResult(ErrorCodeConstants.Success, "Deactivate Model", null, ErrorCodeConstants.Deactivate_Model_Successful + ":" + dispkey)).toString
    } else {
      (new ApiResult(ErrorCodeConstants.Failure, "Deactivate Model", null, ErrorCodeConstants.Deactivate_Model_Failed_Not_Active + ":" + dispkey)).toString
    }
  }

  /**
    * Deactivate a model FIXME: Explain what it means to do this locally.
    *
    * @param nameSpace namespace of the object
    * @param name
    * @param version   Version of the object
    * @return
    */
  private def DeactivateLocalModel(nameSpace: String, name: String, version: Long): Boolean = {
    var key = nameSpace + "." + name + "." + version
    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version)
    val newTranId = PersistenceUtils.GetNewTranId
    try {
      val o = MdMgr.GetMdMgr.Model(nameSpace.toLowerCase, name.toLowerCase, version, true)
      o match {
        case None =>
          None
          logger.debug("No active model found => " + dispkey)
          false
        case Some(m) =>
          logger.debug("model found => " + m.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[ModelDef].Version))
          MetadataAPIImpl.DeactivateObject(m.asInstanceOf[ModelDef])

          // TODO: Need to deactivate the appropriate message?
          m.tranId = newTranId
          var objectsUpdated = new Array[BaseElemDef](0)
          objectsUpdated = objectsUpdated :+ m.asInstanceOf[ModelDef]
          val operations = for (op <- objectsUpdated) yield "Deactivate"
          MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
          true
      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        false
      }
    }
  }

  /**
    * Activate the model with the supplied keys. The engine is notified and the model factory is loaded.
    *
    * @param nameSpace namespace of the object
    * @param name
    * @param version   Version of the object
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def ActivateModel(nameSpace: String, name: String, version: Long, userid: Option[String] = None): String = {
    var key = nameSpace + "." + name + "." + version
    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version)
    var currActiveModel: ModelDef = null
    val newTranId = PersistenceUtils.GetNewTranId

    // Audit this call
    MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.ACTIVATEOBJECT, AuditConstants.MODEL, AuditConstants.SUCCESS, "", nameSpace + "." + name + "." + version)

    try {
      // We may need to deactivate an model if something else is active.  Find the active model
      val oCur = MdMgr.GetMdMgr.Models(nameSpace, name, true, false)
      oCur match {
        case None =>
        case Some(m) =>
          var setOfModels = m.asInstanceOf[scala.collection.immutable.Set[ModelDef]]
          if (setOfModels.size > 1) {
            logger.error("Internal Metadata error, there are more then 1 versions of model " + nameSpace + "." + name + " active on this system.")
          }

          // If some model is active, deactivate it.
          if (setOfModels.size != 0) {
            currActiveModel = setOfModels.last
            if (currActiveModel.NameSpace.equalsIgnoreCase(nameSpace) &&
              currActiveModel.name.equalsIgnoreCase(name) &&
              currActiveModel.Version == version) {
              return (new ApiResult(ErrorCodeConstants.Success, "ActivateModel", null, dispkey + " already active")).toString

            }
            var isSuccess = DeactivateLocalModel(currActiveModel.nameSpace, currActiveModel.name, currActiveModel.Version)
            if (!isSuccess) {
              logger.error("Error while trying to activate " + dispkey + ", unable to deactivate active model. model ")
              val apiResult = new ApiResult(ErrorCodeConstants.Failure, "ActivateModel", null, "Error :" + ErrorCodeConstants.Activate_Model_Failed + ":" + dispkey + " -Unable to deactivate existing model")
              apiResult.toString()
            }
          }

      }

      // Ok, at this point, we have deactivate  a previously active model.. now we activate this one.
      val o = MdMgr.GetMdMgr.Model(nameSpace.toLowerCase, name.toLowerCase, version, false)
      o match {
        case None =>
          None
          logger.debug("No active model found => " + dispkey)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "ActivateModel", null, ErrorCodeConstants.Activate_Model_Failed_Not_Active + ":" + dispkey)
          apiResult.toString()
        case Some(m) =>
          logger.debug("model found => " + m.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[ModelDef].Version))
          MetadataAPIImpl.ActivateObject(m.asInstanceOf[ModelDef])

          // Issue a Notification to all registered listeners that an Acivation took place.
          // TODO: Need to activate the appropriate message?
          var objectsUpdated = new Array[BaseElemDef](0)
          m.tranId = newTranId
          objectsUpdated = objectsUpdated :+ m.asInstanceOf[ModelDef]
          val operations = for (op <- objectsUpdated) yield "Activate"
          MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)

          // No exceptions, we succeded
          val apiResult = new ApiResult(ErrorCodeConstants.Success, "ActivateModel", null, ErrorCodeConstants.Activate_Model_Successful + ":" + dispkey)
          apiResult.toString()
      }
    } catch {

      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "ActivateModel", null, "Error :" + e.toString() + ErrorCodeConstants.Activate_Model_Failed + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  /**
    * Remove model with Model Name and Version Number
    *
    * @param nameSpace namespace of the object
    * @param name
    * @param version   Version of the object
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def RemoveModel(nameSpace: String, name: String, version: Long, userid: Option[String]): String = {
    var key = nameSpace + "." + name + "." + version
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.DELETEOBJECT, "Model", AuditConstants.SUCCESS, "", key)
    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version)
    var newTranId = PersistenceUtils.GetNewTranId
    try {
      val o = MdMgr.GetMdMgr.Model(nameSpace.toLowerCase, name.toLowerCase, version, true)
      o match {
        case None =>
          None
          logger.debug("model not found => " + dispkey)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveModel", null, ErrorCodeConstants.Remove_Model_Failed_Not_Found + ":" + dispkey)
          apiResult.toString()
        case Some(m) =>
          logger.debug("model found => " + m.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[ModelDef].Version))
          MetadataAPIImpl.DeleteObject(m)
          var objectsUpdated = new Array[BaseElemDef](0)
          m.tranId = newTranId
          objectsUpdated = objectsUpdated :+ m
          var operations = for (op <- objectsUpdated) yield "Remove"
          MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
          val apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveModel", null, ErrorCodeConstants.Remove_Model_Successful + ":" + dispkey)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveModel", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Model_Failed + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  /**
    * Remove model with Model Name and Version Number
    *
    * @param modelName the Namespace.Name of the given model to be removed
    * @param version   Version of the given model.  The version should comply with the Kamanja version format.  For example,
    *                  a value of "000001.000001.000001" shows the digits available for version.  All must be base 10 digits
    *                  with up to 6 digits for major version, minor version and micro version sections.
    *                  elper functions are available in MdMgr object for converting to/from strings and 0 padding the
    *                  version sections if desirable.
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def RemoveModel(modelName: String, version: String, userid: Option[String] = None): String = {

    val reasonable: Boolean = modelName != null && modelName.length > 0
    val result: String = if (reasonable) {
      val buffer: StringBuilder = new StringBuilder
      val modelNameAdjusted: String = if (modelName.contains(".")) {
        modelName
      } else {
        logger.warn(s"No namespace qualification given...attempting removal with the ${sysNS} as the namespace")
        s"$sysNS.$modelName"
      }
      val modelNameNodes: Array[String] = modelNameAdjusted.split('.')
      val modelNm: String = modelNameNodes.last
      modelNameNodes.take(modelNameNodes.size - 1).addString(buffer, ".")
      val modelNmSpace: String = buffer.toString

      // old way; The Sytem namespace assumed... RemoveModel(sysNS, modelName, version, userid)

      RemoveModel(modelNmSpace, modelNm, MdMgr.ConvertVersionToLong(version), userid)

    } else {
      val modelNameStr: String = if (modelName == null) "NO MODEL NAME GIVEN" else "MODEL NAME of zero length"
      new ApiResult(ErrorCodeConstants.Failure, "RemoveModel", null, s"${ErrorCodeConstants.Remove_Model_Failed} : supplied model name ($modelNameStr) is bad").toString
    }
    result
  }


  /**
    * The ModelDef returned by the compilers is added to the metadata.
    *
    * @param model
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured,supply something other than None
    * @return
    */
  def AddModel(model: ModelDef, userid: Option[String]): String = {
    var key = model.FullNameWithVer
    val dispkey = model.FullName + "." + MdMgr.Pad0s2Version(model.Version)
    try {
      MetadataAPIImpl.SaveObject(model, MdMgr.GetMdMgr)
      val apiResult = new ApiResult(ErrorCodeConstants.Success, "AddModel", null, ErrorCodeConstants.Add_Model_Successful + ":" + dispkey)
      apiResult.toString()
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "AddModel", null, ErrorCodeConstants.Add_Model_Successful + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  private def AddOutMsgToModelDef(modDef: ModelDef, modelType: ModelType.ModelType, optMsgProduced: Option[String], userid: Option[String]): Unit = {
    // save the outMessage if any
    if (optMsgProduced != None) {
      modDef.outputMsgs = modDef.outputMsgs :+ optMsgProduced.get.toLowerCase
    }
    else {
      // no need to create a default output message if modelconfig defines an output message as well
      if( modDef.outputMsgs.length == 0 ){
        val defaultMessage = MessageAndContainerUtils.createDefaultOutputMessage(modDef, userid)
        modDef.outputMsgs = modDef.outputMsgs :+ defaultMessage
      }
    }
  }
    

  /**
    * AddModelFromSource - compiles and catalogs a custom Scala or Java model from source.
    *
    * @param sourceCode
    * @param sourceLang
    * @param modelName
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  private def AddModelFromSource(sourceCode: String, sourceLang: String, modelName: String, userid: Option[String], tenantId: String, optMsgProduced: Option[String]): String = {
    try {
      var compProxy = new CompilerProxy
      compProxy.setSessionUserId(userid)
      val modDef = compProxy.compileModelFromSource(sourceCode, modelName, sourceLang, userid, tenantId)

      // save the outMessage
      AddOutMsgToModelDef(modDef,ModelType.fromString(sourceLang),optMsgProduced,userid)

      logger.info("Begin uploading dependent Jars, please wait.")
      PersistenceUtils.UploadJarsToDB(modDef)
      logger.info("Finished uploading dependent Jars.")
      val apiResult = AddModel(modDef, userid)

      // Add all the objects and NOTIFY the world
      var objectsAdded = new Array[BaseElemDef](0)
      objectsAdded = objectsAdded :+ modDef
      val operations = for (op <- objectsAdded) yield "Add"
      logger.debug("Notify engine via zookeeper")
      MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
      apiResult
    } catch {
      case e: AlreadyExistsException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error : " + ErrorCodeConstants.Add_Model_Failed_Higher_Version_Required)
        apiResult.toString()
      }
      case e: MsgCompilationFailedException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error : " + ErrorCodeConstants.Model_Compilation_Failed)
        apiResult.toString()
      }
      case e: Exception => {
        logger.error("Unknown compilation error occured", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error : " + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /** Add model. Several model types are currently supported.  They describe the content of the "input" argument:
    *
    * - SCALA - a Scala source string
    * - JAVA - a Java source string
    * - PMML - a PMML source string
    * - KPMML - a Kamanja Pmml source string
    * - BINARY - the path to a jar containing the model
    *
    * The remaining arguments, while noted as optional, are required for some model types.  In particular,
    * the ''modelName'', ''version'', and ''msgConsumed'' must be specified for the PMML model type.  The ''userid'' is
    * required for systems that have been configured with a SecurityAdapter or AuditAdapter.
    *
    * @see [[http://kamanja.org/security/ security wiki]] for more information. The audit adapter, if configured,
    *      will also be invoked to take note of this user's action.
    * @see [[http://kamanja.org/auditing/ auditing wiki]] for more information about auditing.
    *      NOTE: The BINARY model is not supported at this time.  The model submitted for this type will via a jar file.
    * @param modelType      the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY})
    * @param input          the text element to be added dependent upon the modelType specified.
    * @param optUserid      the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                       method.
    * @param optModelName   the namespace.name of the PMML model to be added to the Kamanja metadata
    * @param optVersion     the model version to be used to describe this PMML model
    * @param optMsgConsumed the namespace.name of the message to be consumed by a PMML model
    * @param optMsgVersion  the version of the message to be consumed. By default Some(-1)
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def AddModel(modelType: ModelType.ModelType
               , input: String
               , optUserid: Option[String] = None
               , tenantId: Option[String] = None
               , optModelName: Option[String] = None
               , optVersion: Option[String] = None
               , optMsgConsumed: Option[String] = None
               , optMsgVersion: Option[String] = Some("-1")
               , optMsgProduced: Option[String] = None
              ): String = {

    // No Add Model is allowed without Tenant Id
    if (tenantId == None) {
      return (new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"Tenant ID is required to perform a ADD MODEL operation")).toString
    }

    if (optMsgProduced != None) {
      logger.info("Validating the output message type " + optMsgProduced.get.toLowerCase);
      val msg = MessageAndContainerUtils.IsMessageExists(optMsgProduced.get.toLowerCase)
      if ( msg == null ) {
	logger.info("Unknown outputmsg " + optMsgProduced.get.toLowerCase);
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"Unknown Outmessage ${optMsgProduced.get.toLowerCase} error = ${ErrorCodeConstants.Add_Model_Failed}")
        return apiResult.toString
      }
      else {
	if ( modelType == ModelType.KPMML  && ! MessageAndContainerUtils.IsMappedMessage(msg)) {
	  logger.info("outputmsg " + optMsgProduced.get.toLowerCase + " not a mapped message ");

          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"Outmessage ${optMsgProduced.get.toLowerCase} must be a mapped message for KPPML Models, error = ${ErrorCodeConstants.Add_Model_Failed}")
          return apiResult.toString
	}
      }
      logger.info("A valid message type " + optMsgProduced.get.toLowerCase + " is already found in metadata");
    }
    else {
      logger.debug("We may create a default output message depending on whether they are supplied in ModelConfig object ..")
    }

    val modelResult: String = modelType match {
      case ModelType.KPMML => {
        AddKPMMLModel(input, optUserid, tenantId.get, optMsgProduced)
      }
      case ModelType.JTM => {
        AddJTMModel(input, optUserid, tenantId.get, optModelName)
      }
      case ModelType.JAVA | ModelType.SCALA => {  //ModelUtils.AddModel(modelType, input, optUserid, optTenantid, optModelName, optVersion, optMsgConsumed, optMsgVersion, optMsgProduced)
        val result: String = optModelName.fold(throw new RuntimeException("Model name should be provided for Java/Scala models"))(name => {
          AddModelFromSource(input, modelType.toString, name, optUserid, tenantId.get, optMsgProduced)
        })
        result
      }
      case ModelType.PMML => {
        val modelName: String = optModelName.orNull
        val version: String = optVersion.orNull
        val msgConsumed: String = optMsgConsumed.orNull
        val msgVer: String = optMsgVersion.getOrElse("-1")
        val result: String = if (modelName != null && version != null && msgConsumed != null) {
          val res: String = AddPMMLModel(modelName
            , version
            , msgConsumed
            , msgVer
            , input
            , optUserid
            , tenantId.get
            , optMsgProduced)
          res
        } else {
          val inputRep: String = if (input != null && input.size > 200) input.substring(0, 199)
          else if (input != null) input
          else "no model text"
          val apiResult = new ApiResult(ErrorCodeConstants.Failure
            , "AddModel"
            , null
            , s"One or more PMML arguments have not been specified... modelName = $modelName, version = $version, input = $inputRep error = ${ErrorCodeConstants.Add_Model_Failed}")
          apiResult.toString
        }
        result
      }

      case ModelType.BINARY =>
        new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"BINARY model type NOT SUPPORTED YET ...${ErrorCodeConstants.Add_Model_Failed}").toString

      case _ => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"Unknown model type ${modelType.toString} error = ${ErrorCodeConstants.Add_Model_Failed}")
        apiResult.toString
      }
    }
    modelResult
  }


  /**
    * Add a PMML model to the metadata.
    *
    * PMML models are evaluated, not compiled. To create the model definition, an instance of the evaluator
    * is obtained from the pmml-evaluator component and the ModelDef is constructed and added to the store.
    *
    * @see com.ligadata.MetadataAPI.JpmmlSupport for more information
    * @param modelName   the namespace.name of the model to be injested.
    * @param version     the version as string in the form "MMMMMM.mmmmmmmm.nnnnnn" (3 nodes .. could be fewer chars per node)
    * @param msgConsumed the namespace.name of the message that this model is to consume.  NOTE: the
    *                    fields used in the pmml model and the fields in the message must match.  If
    *                    the message does not supply all input fields in the model, there should be a default
    *                    specified for those not filled in that mining variable.
    * @param msgVersion  the version of the message that this PMML model will consume
    * @param pmmlText    the actual PMML (xml) that is submitted by the client.
    * @param userid      the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                    method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return json string result
    */
  private def AddPMMLModel(modelName: String
                           , version: String
                           , msgConsumed: String
                           , msgVersion: String
                           , pmmlText: String
                           , userid: Option[String]
                           , tenantId: String
                           , optMsgProduced: Option[String]
                          ): String = {
    try {
      val buffer: StringBuilder = new StringBuilder
      val modelNameNodes: Array[String] = modelName.split('.')
      val modelNm: String = modelNameNodes.last
      modelNameNodes.take(modelNameNodes.size - 1).addString(buffer, ".")
      val modelNmSpace: String = buffer.toString
      buffer.clear
      val msgNameNodes: Array[String] = msgConsumed.split('.')
      val msgName: String = msgNameNodes.last
      msgNameNodes.take(msgNameNodes.size - 1).addString(buffer, ".")
      val msgNamespace: String = buffer.toString
      val ownerId: String = if (userid == None) "kamanja" else userid.get
      val jpmmlSupport: JpmmlSupport = new JpmmlSupport(mdMgr
        , modelNmSpace
        , modelNm
        , version
        , msgNamespace
        , msgName
        , msgVersion
        , pmmlText
        , ownerId
        ,tenantId)
      val recompile: Boolean = false
      var modDef: ModelDef = jpmmlSupport.CreateModel(recompile)

      // ModelDef may be null if the model evaluation failed
      val latestVersion: Option[ModelDef] = if (modDef == null) None else GetLatestModel(modDef)
      val isValid: Boolean = if (latestVersion.isDefined) MetadataAPIImpl.IsValidVersion(latestVersion.get, modDef) else true

      if (isValid && modDef != null) {
        val existingModel = MdMgr.GetMdMgr.Model(modDef.NameSpace, modDef.Name, -1, false) // Any version is fine. No need of active
        modDef.uniqueId = MetadataAPIImpl.GetUniqueId
        modDef.mdElementId = if (existingModel == None) MetadataAPIImpl.GetMdElementId else existingModel.get.MdElementId
        MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, pmmlText, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)

	// save the outMessage
	AddOutMsgToModelDef(modDef,ModelType.PMML,optMsgProduced,userid)

        // save the jar file first
        PersistenceUtils.UploadJarsToDB(modDef)
        val apiResult = AddModel(modDef, userid)
        logger.debug("Model is added..")
        var objectsAdded = new Array[BaseElemDef](0)
        objectsAdded = objectsAdded :+ modDef
        val operations = for (op <- objectsAdded) yield "Add"
        logger.debug("Notify engine via zookeeper")
        MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
        apiResult
      } else {
        val reasonForFailure: String = if (modDef != null) {
          ErrorCodeConstants.Add_Model_Failed_Higher_Version_Required
        } else {
          ErrorCodeConstants.Add_Model_Failed
        }
        val modDefName: String = if (modDef != null) modDef.FullName else "(pmml compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        var apiResult = new ApiResult(ErrorCodeConstants.Failure
          , "AddModel"
          , null
          , s"$reasonForFailure : $modDefName.$modDefVer)")
        apiResult.toString()
      }
    } catch {
      case e: ModelCompilationFailedException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure
          , "AddModel"
          , null
          , s"Error : ${e.toString} + ${ErrorCodeConstants.Add_Model_Failed}")
        apiResult.toString()
      }
      case e: AlreadyExistsException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure
          , "AddModel"
          , null
          , s"Error : ${e.toString} + ${ErrorCodeConstants.Add_Model_Failed}")
        apiResult.toString()
      }
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure
          , "AddModel"
          , null
          , s"Error : ${e.toString} + ${ErrorCodeConstants.Add_Model_Failed}")
        apiResult.toString()
      }
    }
  }

  /**
    * Add Kamanja PMML Model (format XML).  Kamanja Pmml models obtain their name and version from the header in the Pmml file.
    *
    * @param pmmlText
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return json string result
    */
  private def AddKPMMLModel(pmmlText: String,
                            userid: Option[String], tenantId: String,
                            optMsgProduced: Option[String]): String = {
    try {
      var compProxy = new CompilerProxy
      //compProxy.setLoggerLevel(Level.TRACE)
      val ownerId: String = if (userid == None) "kamanja" else userid.get
      var (classStr, modDef) = compProxy.compilePmml(pmmlText, ownerId, tenantId)

      // ModelDef may be null if there were pmml compiler errors... act accordingly.  If modelDef present,
      // make sure the version of the model is greater than any of previous models with same FullName
      val latestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val isValid: Boolean = if (latestVersion != None) MetadataAPIImpl.IsValidVersion(latestVersion.get, modDef) else true

      if (isValid && modDef != null) {
        MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, pmmlText, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)

	// save the outMessage
	AddOutMsgToModelDef(modDef,ModelType.KPMML,optMsgProduced,userid)

        // save the jar file first
        PersistenceUtils.UploadJarsToDB(modDef)
        val apiResult = AddModel(modDef, userid)
        logger.debug("Model is added..")
        var objectsAdded = new Array[BaseElemDef](0)
        objectsAdded = objectsAdded :+ modDef
        val operations = for (op <- objectsAdded) yield "Add"
        logger.debug("Notify engine via zookeeper")
        MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
        apiResult
      } else {
        val reasonForFailure: String = if (modDef != null) ErrorCodeConstants.Add_Model_Failed_Higher_Version_Required else ErrorCodeConstants.Add_Model_Failed
        val modDefName: String = if (modDef != null) modDef.FullName else "(kpmml compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ModelCompilationFailedException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
    }
  }

  private def GetTypesAndJarsDependencies(modelName: String, userid: Option[String]): List[String] = {
    val jarsList = ArrayBuffer[String]()
    val depsList = MetadataAPIImpl.getModelDependencies(modelName, userid)
    if (depsList != null)
      jarsList ++= depsList;

    val typesLst = MetadataAPIImpl.getModelMessagesContainers(modelName, userid)

      if (typesLst != null) {
        typesLst.foreach(typ => {
          val cont = MdMgr.GetMdMgr.Container(typ, -1, true)
          var typInfo: BaseElem = null

          if (cont != None) {
              typInfo = cont.get.asInstanceOf[BaseElem]
          } else {
            val msg = MdMgr.GetMdMgr.Message(typ, -1, true)
            if (msg != None)
              typInfo = msg.get.asInstanceOf[BaseElem]
          }

          if (typInfo != null) {
            val tInfo = typInfo
            if (tInfo.JarName != null && tInfo.JarName.trim.size > 0)
              jarsList += tInfo.JarName.trim
            if (tInfo.DependencyJarNames != null && tInfo.DependencyJarNames.size > 0)
              jarsList ++= tInfo.DependencyJarNames
          }
        })
      }

    jarsList.toList
  }

  /**
    * Add Kamanja JTM Model (format json).  Kamanja JTM models obtain their name and version from the header in the JTM s file.
    *
    * @param jsonText
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return json string result
    */
  private def AddJTMModel(jsonText: String, userid: Option[String], tenantId: String, optModelName: Option[String]): String = {
    try {
      var compProxy = new CompilerProxy

      val usr = if (userid == None) "Kamanja" else userid.get

      var compileConfig:String = ""
      // Getting external dependency jars
      val extDepJars =
        if (optModelName != None) {
          var cfgName = optModelName.get.toLowerCase
          if (cfgName.length > 0 && !cfgName.startsWith(usr.toLowerCase() + "."))
            cfgName = usr.toLowerCase() + "." + cfgName
          var cfg = MdMgr.GetMdMgr.GetModelConfig(cfgName)
          compileConfig = JsonSerializer.SerializeModelConfigToJson(cfgName, cfg)
          // MetadataAPIImpl.getModelDependencies(cfgName, userid)
          GetTypesAndJarsDependencies(cfgName, userid)
        } else {
          List[String]()
        }

       //compProxy.setLoggerLevel(Level.TRACE)
      var (classStr, modDef) = compProxy.compileJTM(jsonText, tenantId, extDepJars, usr, compileConfig)

      // ModelDef may be null if there were pmml compiler errors... act accordingly.  If modelDef present,
      // make sure the version of the model is greater than any of previous models with same FullName
      val latestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val isValid: Boolean = if (latestVersion != None) MetadataAPIImpl.IsValidVersion(latestVersion.get, modDef) else true

      if (isValid && modDef != null) {
        MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, jsonText, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)
        // save the jar file first
        PersistenceUtils.UploadJarsToDB(modDef)
        val apiResult = AddModel(modDef, userid)
        logger.debug("Model is added..")
        var objectsAdded = new Array[BaseElemDef](0)
        objectsAdded = objectsAdded :+ modDef
        val operations = for (op <- objectsAdded) yield "Add"
        logger.debug("Notify engine via zookeeper")
        MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
        apiResult
      } else {
        val reasonForFailure: String = if (modDef != null) ErrorCodeConstants.Add_Model_Failed_Higher_Version_Required else ErrorCodeConstants.Add_Model_Failed
        val modDefName: String = if (modDef != null) modDef.FullName else "(JTM compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddJTMModel", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ModelCompilationFailedException => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddJTMModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddJTMModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddJTMModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * Recompile the supplied model. Optionally the message definition is supplied that was just built.
    *
    * @param mod       the model definition that possibly needs to be reconstructed.
    * @param userid    the user id that has invoked this command
    * @param optMsgDef the MessageDef constructed, assuming it was a message def. If a container def has been rebuilt,
    *                  this field will have a value of None.  This is only meaningful at this point when the model to
    *                  be rebuilt is a PMML model.
    * @return the result string reflecting what happened with this operation.
    */
  def RecompileModel(mod: ModelDef, userid: Option[String], optMsgDef: Option[MessageDef]): String = {
    try {
      /** FIXME: This should really handle BINARY models too.  When we start supporting them, we cannot recompile
        * it but we can send notifications to consoles or some thing like this to help identify the need for
        * a replacement to the prior binary model.  This needs to be discussed and documented how we are going to
        * do this.
        * FIXME: Actually an update to a message that supports BINARY models needs to be detected up front and
        * a warning and rejection of the message update made. Perhaps adding a "force" flag to get the message
        * to compile despite this obstacle is warranted.
        */
      val isJpmml: Boolean = mod.modelRepresentation == ModelRepresentation.PMML
      val isJtm: Boolean = (mod.modelRepresentation == ModelRepresentation.JAR && mod.miningModelType == MiningModelType.JTM)
      val msgDef: MessageDef = optMsgDef.orNull
      val modDef: ModelDef = if (!isJpmml) {

        val compProxy = new CompilerProxy
        //compProxy.setLoggerLevel(Level.TRACE)

        // Recompile the model based upon its model type.Models can be either PMML, JSON or Custom Sourced.  See which one we are dealing with
        // here.
        if (isJtm && mod.objectFormat == ObjFormatType.fJSON) {
          val jtmTxt = mod.ObjectDefinition

          // Getting external dependency jars
          val extDepJars =
            if (mod.modelConfig != null) {
              val trimmedMdlCfg = mod.modelConfig.trim
              if (trimmedMdlCfg.size > 0) {
                var deps = ArrayBuffer[String]()
                try {
                  val modelParms = parse(mod.modelConfig).values.asInstanceOf[Map[String, Any]]
                  val typDeps = modelParms.getOrElse(ModelCompilationConstants.DEPENDENCIES, null)
                  if (typDeps != null) {
                    if (typDeps.isInstanceOf[List[_]])
                      deps ++= typDeps.asInstanceOf[List[String]]
                    if (typDeps.isInstanceOf[Array[_]])
                      deps ++= typDeps.asInstanceOf[Array[String]]
                  }

                  val typesLst = {
                    val typDeps = modelParms.getOrElse(ModelCompilationConstants.TYPES_DEPENDENCIES, null)
                    if (typDeps != null) {
                      if (typDeps.isInstanceOf[List[_]])
                        typDeps.asInstanceOf[List[String]]
                      else if (typDeps.isInstanceOf[Array[_]])
                        typDeps.asInstanceOf[Array[String]].toList
                      else
                        List[String]()
                    }
                    else
                      List[String]()
                  }

                  if (typesLst != null) {
                    typesLst.foreach(typ => {

                      val cont = MdMgr.GetMdMgr.Container(typ, -1, true)
                      var typInfo: BaseElem = null

                      if (cont != None) {
                        typInfo = cont.get.asInstanceOf[BaseElem]
                      } else {
                        val msg = MdMgr.GetMdMgr.Message(typ, -1, true)
                        if (msg != None)
                          typInfo = msg.get.asInstanceOf[BaseElem]
                      }

                      if (typInfo != null) {
                        val tInfo = typInfo
                        if (tInfo.JarName != null && tInfo.JarName.trim.size > 0)
                          deps += tInfo.JarName.trim
                        if (tInfo.DependencyJarNames != null && tInfo.DependencyJarNames.size > 0)
                          deps ++= tInfo.DependencyJarNames
                      }
                    })
                  }
                }
                catch {
                  case e: Throwable => {
                    logger.error("Failed to parse model config.", e)
                  }
                }
                deps.toList
              } else {
                List[String]()
              }
            } else {
              List[String]()
            }

          val usr = if (userid == None) "Kamanja" else userid.get
          val (classStrTemp, modDefTemp) = compProxy.compileJTM(jtmTxt, mod.TenantId, extDepJars, usr, mod.modelConfig, true)
          modDefTemp.modelConfig = mod.modelConfig
          modDefTemp
        } else {
          if (mod.objectFormat == ObjFormatType.fXML) {
            val pmmlText = mod.ObjectDefinition
            val ownerId: String = if (userid == None) "kamanja" else userid.get
            val (classStrTemp, modDefTemp) = compProxy.compilePmml(pmmlText, ownerId, mod.TenantId, true)
            // copy outputMsgs
            if (mod.outputMsgs.length > 0) {
              modDefTemp.outputMsgs.foreach(omsg => {
                modDefTemp.outputMsgs = modDefTemp.outputMsgs :+ omsg
              })
            }
            modDefTemp
          } else {
            val saveModelParms = parse(mod.ObjectDefinition).values.asInstanceOf[Map[String, Any]]

            val tmpInputMsgSets = saveModelParms.getOrElse(ModelCompilationConstants.INPUT_TYPES_SETS, null)
            var inputMsgSets = List[List[String]]()
            if (tmpInputMsgSets != null) {
              if (tmpInputMsgSets.isInstanceOf[List[Any]]) {
                if (tmpInputMsgSets.isInstanceOf[List[List[Any]]])
                  inputMsgSets = tmpInputMsgSets.asInstanceOf[List[List[String]]]
                if (tmpInputMsgSets.isInstanceOf[List[Array[Any]]])
                  inputMsgSets = tmpInputMsgSets.asInstanceOf[List[Array[String]]].map(lst => lst.toList)
              } else if (tmpInputMsgSets.isInstanceOf[Array[Any]]) {
                if (tmpInputMsgSets.isInstanceOf[Array[List[Any]]])
                  inputMsgSets = tmpInputMsgSets.asInstanceOf[Array[List[String]]].toList
                if (tmpInputMsgSets.isInstanceOf[Array[Array[Any]]])
                  inputMsgSets = tmpInputMsgSets.asInstanceOf[Array[Array[String]]].map(lst => lst.toList).toList
              }
            }

            var outputMsgs = List[String]()
            val tmpOnputMsgs = saveModelParms.getOrElse(ModelCompilationConstants.OUTPUT_TYPES_SETS, null)
            if (tmpOnputMsgs != null) {
              if (tmpOnputMsgs.isInstanceOf[List[_]])
                outputMsgs = tmpOnputMsgs.asInstanceOf[List[String]]
              if (tmpOnputMsgs.isInstanceOf[Array[_]])
                outputMsgs = tmpOnputMsgs.asInstanceOf[Array[String]].toList
            }

            val custModDef: ModelDef = compProxy.recompileModelFromSource(
              saveModelParms.getOrElse(ModelCompilationConstants.SOURCECODE, "").asInstanceOf[String],
              saveModelParms.getOrElse(ModelCompilationConstants.PHYSICALNAME, "").asInstanceOf[String],
              saveModelParms.getOrElse(ModelCompilationConstants.DEPENDENCIES, List[String]()).asInstanceOf[List[String]],
              saveModelParms.getOrElse(ModelCompilationConstants.TYPES_DEPENDENCIES, List[String]()).asInstanceOf[List[String]],
              inputMsgSets, outputMsgs, ObjFormatType.asString(mod.objectFormat), userid, mod.TenantId)
            custModDef
          }
        }
      } else {
        /** the msgConsumed is namespace.name.version  ... drop the version so as to compare the "FullName" */
        val buffer: StringBuilder = new StringBuilder

        var msgConsumed: String = null
        if (mod.inputMsgSets != null && mod.inputMsgSets.size > 0) {
          if (msgConsumed == null) {
            mod.inputMsgSets.foreach(set => {
              set.foreach(msgInfo => {
                if (msgConsumed == null && msgInfo != null && msgInfo.message != null && msgInfo.message.trim.nonEmpty) {
                  msgConsumed = msgInfo.message
                }
              })
            })
          }
        }

        val modMsgNameParts: Array[String] = if (msgConsumed != null) msgConsumed.split('.') else Array[String]()
        val modMsgFullName: String = modMsgNameParts.dropRight(1).addString(buffer, ".").toString.toLowerCase
        val reasonable: Boolean = (modMsgFullName == msgDef.FullName)
        if (reasonable) {
          val msgName: String = msgDef.Name
          val msgNamespace: String = msgDef.NameSpace
          val msgVersion: String = MdMgr.ConvertLongVersionToString(msgDef.Version)
          val modelNmSpace: String = mod.NameSpace
          val modelName: String = mod.Name
          val modelVersion: String = MdMgr.ConvertLongVersionToString(mod.Version)
          val ownerId: String = if (userid == None) "kamanja" else userid.get
          val jpmmlSupport: JpmmlSupport = new JpmmlSupport(mdMgr
            , modelNmSpace
            , modelName
            , modelVersion
            , msgNamespace
            , msgName
            , msgVersion
            , mod.objectDefinition
            , ownerId
            , mod.TenantId)
          val recompile: Boolean = true
          val model: ModelDef = jpmmlSupport.CreateModel(recompile)
          // copy outputMsgs
          if (mod.outputMsgs.length > 0) {
            model.outputMsgs.foreach(omsg => {
              model.outputMsgs = model.outputMsgs :+ omsg
            })
          }
          model
        } else {
          /** this means that the dependencies are incorrect.. message is not the PMML message of interest */
          logger.error(s"The message names for model ${mod.FullName} and the message just built (${msgDef.FullName}) don't match up. It suggests model dependencies and/or the model type are messed up.")
          null
        }
      }

      val latestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val isValid: Boolean = (modDef != null)
      if (isValid) {
        val existingModel = MdMgr.GetMdMgr.Model(modDef.NameSpace, modDef.Name, -1, false) // Any version is fine. No need of active
        modDef.uniqueId = MetadataAPIImpl.GetUniqueId
        modDef.mdElementId = if (existingModel == None) MetadataAPIImpl.GetMdElementId else existingModel.get.MdElementId

        val rmResult: String = RemoveModel(latestVersion.get.nameSpace, latestVersion.get.name, latestVersion.get.ver, None)
        PersistenceUtils.UploadJarsToDB(modDef)
        val addResult: String = AddModel(modDef, userid)
        var objectsUpdated = new Array[BaseElemDef](0)
        var operations = new Array[String](0)
        objectsUpdated = objectsUpdated :+ latestVersion.get
        operations = operations :+ "Remove"
        objectsUpdated = objectsUpdated :+ modDef
        operations = operations :+ "Add"
        MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
        s"\nRecompileModel results for ${mod.NameSpace}.${mod.Name}.${mod.Version}\n$rmResult$addResult"
      } else {
        val reasonForFailure: String = ErrorCodeConstants.Model_ReCompilation_Failed
        val modDefName: String = if (mod != null) mod.FullName else "(compilation failed)"
        val modDefVer: String = if (mod != null) MdMgr.Pad0s2Version(mod.Version) else MdMgr.UnknownVersion
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "\nRecompileModel", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ModelCompilationFailedException => {
        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "\nRecompileModel", null, "Error in producing scala file or Jar file.." + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "RecompileModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "RecompileModel", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * Update the model with new source of type modelType.
    *
    * Except for the modelType and the input, all fields are marked optional. Note, however, that for some of the
    * model types all arguments should have meaningful values.
    *
    * @see AddModel for semantics.
    *
    *      Note that the message and message version (as seen in AddModel) are not used.  Should a message change that is being
    *      used by one of the PMML models, it will be automatically be updated immediately when the message compilation and
    *      metadata update has completed for it.
    *
    *      Currently only the most recent model cataloged with the name noted in the source file can be "updated".  It is not
    *      possible to have a number of models with the same name differing only by version and be able to update one of them
    *      explicitly.  This is a feature that is to be implemented.
    *
    *      If both the model and the message are changing, consider using AddModel to create a new PMML model and then remove the older
    *      version if appropriate.
    * @param modelType              the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY})
    * @param input                  the text element to be added dependent upon the modelType specified.
    * @param optUserid              the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                               method.
    * @param optModelName           the namespace.name of the PMML model to be added to the Kamanja metadata (Note: KPMML models extract this from
    *                               the pmml source file header and for this reason is not required for the KPMML model types).
    * @param optVersion             the model version to be used to describe this PMML model (for KPMML types this value is obtained from the source file)
    * @param optVersionBeingUpdated not used .. reserved for future release where explicit modelnamespace.modelname.modelversion
    *                               can be updated (not just the latest version)
    * @return result string indicating success or failure of operation
    */
  def UpdateModel(modelType: ModelType.ModelType
                  , input: String
                  , optUserid: Option[String] = None
                  , tenantId: Option[String] = None
                  , optModelName: Option[String] = None
                  , optVersion: Option[String] = None
                  , optVersionBeingUpdated: Option[String] = None
                  , optMsgProduced: Option[String] = None): String = {
    /**
      * FIXME: The current strategy is that only the most recent version can be updated.
      * FIXME: This is not a satisfactory condition. It may be desirable to have 10 models all with
      * FIXME: the same name but differing only in their version numbers. If someone wants to tune
      * FIXME: #6 of the 10, that number six is not the latest.  It is just a unique model.
      *
      * For this reason, the version of the model that is to be changed should be supplied here and all of the
      * associated handler functions that service update for the various model types should be amended to
      * consider which model it is that is to be updated exactly.  The removal of the model being replaced
      * must be properly handled to remove the one with the version supplied.
      */

    if (tenantId == None) return (new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, s"Tenant ID is required to perform a UPDATE MODEL operation")).toString

    val modelResult: String = modelType match {
      case ModelType.KPMML => {
        val result: String = UpdateKPMMLModel(modelType, input, optUserid, tenantId.get, optModelName, optVersion, optMsgProduced)
        result
      }
      case ModelType.JTM => {
        val result: String = UpdateJTMModel(modelType, input, optUserid, tenantId.get, optModelName, optVersion)
        result
      }
      case ModelType.JAVA | ModelType.SCALA => {
        val result: String = UpdateCustomModel(modelType, input, optUserid, tenantId.get, optModelName, optVersion)
        result
      }
      case ModelType.PMML => { //1.1.3
        val result: String = UpdatePMMLModel(modelType, input, optUserid, tenantId.get, optModelName, optVersion, optVersionBeingUpdated, optMsgProduced)
        result
      }
      case ModelType.BINARY =>
        new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"BINARY model type NOT SUPPORTED YET ...${ErrorCodeConstants.Add_Model_Failed}").toString
      case _ => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddModel", null, s"Unknown model type ${modelType.toString} error = ${ErrorCodeConstants.Add_Model_Failed}")
        apiResult.toString
      }
    }
    modelResult
  }

  /**
    * Update a PMML model with the supplied inputs.  The input is presumed to be a new version of a PMML model that
    * is currently cataloged.  The user id should be supplied for any installation that is using the security or audit
    * plugins. The model namespace.name and its new version are supplied.  The message ingested by the current version
    * is used by the for the update.
    *
    * @param modelType              the type of the model... PMML in this case
    * @param input                  the new source to ingest for the model being updated/replaced
    * @param optUserid              the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @param optModelName           the name of the model to be ingested (only relevant for PMML ingestion)
    * @param optModelVersion        the version number of the model to be updated (only relevant for PMML ingestion)
    * @param optVersionBeingUpdated not used... reserved
    * @return result string indicating success or failure of operation
    */
  private def UpdatePMMLModel(modelType: ModelType.ModelType
                              , input: String
                              , optUserid: Option[String] = None
                              , tenantId: String = ""
                              , optModelName: Option[String] = None
                              , optModelVersion: Option[String] = None
                              , optVersionBeingUpdated: Option[String]
                              , optMsgProduced: Option[String]): String = {

    val modelName: String = optModelName.orNull
    val version: String = optModelVersion.getOrElse("-1")
    val result: String = if (modelName != null && version != null) {
      try {
        val buffer: StringBuilder = new StringBuilder
        val modelNameNodes: Array[String] = modelName.split('.')
        val modelNm: String = modelNameNodes.last
        modelNameNodes.take(modelNameNodes.size - 1).addString(buffer, ".")
        val modelNmSpace: String = buffer.toString

        val currentVer: Long = -1
        val onlyActive: Boolean = false /** allow active or inactive models to be updated */
        val optCurrent: Option[ModelDef] = mdMgr.Model(modelNmSpace, modelNm, currentVer, onlyActive)
        val currentModel: ModelDef = optCurrent.orNull

        // See if we get the same tenant Id
        if (!tenantId.equalsIgnoreCase(currentModel.tenantId)) {
          return (new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, s"Tenant ID is different from the one in the existing object.")).toString
        }

        val currentMsg: String = if (currentModel != null) {
          //FIXME: Getting only the first one for now
          var msgConsumed: String = null
          if (currentModel.inputMsgSets != null && currentModel.inputMsgSets.size > 0) {
            if (msgConsumed == null) {
              currentModel.inputMsgSets.foreach(set => {
                set.foreach(msgInfo => {
                  if (msgConsumed == null && msgInfo != null && msgInfo.message != null && msgInfo.message.trim.nonEmpty) {
                    msgConsumed = msgInfo.message
                  }
                })
              })
            }
          }
          msgConsumed
        } else {
          null
        }
        val (currMsgNmSp, currMsgNm, currMsgVer): (String, String, String) = MdMgr.SplitFullNameWithVersion(currentMsg)

        val ownerId: String = if (optUserid == None) "kamanja" else optUserid.get
        val jpmmlSupport: JpmmlSupport = new JpmmlSupport(mdMgr
          , modelNmSpace
          , modelNm
          , version
          , currMsgNmSp
          , currMsgNm
          , currMsgVer
          , input
          , ownerId
          , tenantId)

        val modDef: ModelDef = jpmmlSupport.UpdateModel
        // copy optMsgProduced to outputMsgs
        if (optMsgProduced != None) {
          modDef.outputMsgs = modDef.outputMsgs :+ optMsgProduced.get.toLowerCase
        }

        /**
          * FIXME: The current strategy is that only the most recent version can be updated.
          * FIXME: This is not a satisfactory condition. It may be desirable to have 10 models all with
          * FIXME: the same name but differing only in their version numbers. If someone wants to tune
          * FIXME: #6 of the 10, that number six is not the latest.  It is just a unique model.
          *
          * For this reason, the version of the model that is to be changed should be supplied here and in the
          * more generic interface implementation that calls here.
          */

        //def Model(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[ModelDef]
        val tentativeVersionBeingUpdated: String = optVersionBeingUpdated.orNull
        val versionBeingUpdated: String = if (tentativeVersionBeingUpdated != null) tentativeVersionBeingUpdated else "-1"
        val versionLong: Long = MdMgr.ConvertVersionToLong(version)
        val optVersionUpdated: Option[ModelDef] = MdMgr.GetMdMgr.Model(modelNmSpace, modelNm, versionLong, onlyActive)
        val versionUpdated: ModelDef = optVersionUpdated.orNull

        // ModelDef may be null if the model evaluation failed
        // old .... val latestVersion: Option[ModelDef] = if (modDef == null) None else GetLatestModel(modDef) was compared
        // with modeDef in MetadataAPIImpl.IsValidVersion
        //val isValid: Boolean = if (latestVersion.isDefined) MetadataAPIImpl.IsValidVersion(latestVersion.get, modDef) else true
        val isValid: Boolean = if (optVersionUpdated.isDefined) MetadataAPIImpl.IsValidVersion(versionUpdated, modDef) else true

        if (isValid && modDef != null) {
	  // save the outMessage
	  AddOutMsgToModelDef(modDef,ModelType.PMML,optMsgProduced,optUserid)

          val existingModel = MdMgr.GetMdMgr.Model(modDef.NameSpace, modDef.Name, -1, false) // Any version is fine. No need of active
          modDef.uniqueId = MetadataAPIImpl.GetUniqueId
          modDef.mdElementId = if (existingModel == None) MetadataAPIImpl.GetMdElementId else existingModel.get.MdElementId
          MetadataAPIImpl.logAuditRec(optUserid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, input, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)

          /*
           * FIXME: Considering the design goal of NON-STOP cluster model management, it seems that the window
           * FIXME: for something to go wrong is too likely with this current approach.  The old model is being
           * FIXME: deleted before the engine is notified.  Should the engine ask for metadata on that model
           * FIXME: after the model being updated is removed but before the new version has been added, there
           * FIXME: is likelihood that unpredictable behavior that would be difficult to diagnose could occur.
           *
           * FIXME: Furthermore, who is to say that the user doesn't want the model to be updated all right, but
           * FIXME: but that they are not sure that they want the old version to be removed.  In other words,
           * FIXME: "the model is to be updated" means add the modified version of the model, and atomically
           * FIXME: swap the old active version (deactivate it) and the new version (activate it)?
           *
           * FIXME: We need to think it through... what the semantics of the Update is.  In fact we might want
           * FIXME: to deprecate it altogether.  There should be just Add model, Activate model, Deactivate model,
           * FIXME: Swap Models (activate and deactivate same model/different versions atomically), and Remove
           * FIXME: model. Removes would fail if they are active; they need to be deactivated before removal.
           *
           * FIXME: The design goals are to not stop the cluster and to not miss an incoming message. The windows
           * FIXME: of opportunity for calamity are measured by how long it takes to swap inactive/active.  Everything
           * FIXME: else is "offline" as it were.
           */

          val rmModelResult: String = if (versionUpdated != null) {
            RemoveModel(versionUpdated.NameSpace, versionUpdated.Name, versionUpdated.Version, None)
          } else {
            logger.info("versionUpdated is not null, Can't remove the model")
            ""
          }
          logger.info("Begin uploading dependent Jars, please wait...")
          PersistenceUtils.UploadJarsToDB(modDef)
          logger.info("uploading dependent Jars complete")

          val addResult = AddModel(modDef, optUserid)
          logger.debug("Model is added..")
          var objectsAdded = new Array[BaseElemDef](0)
          objectsAdded = objectsAdded :+ modDef
          val operations = for (op <- objectsAdded) yield "Add"
          logger.debug("Notify engine via zookeeper")
          MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
          s"UpdateModel version $version of $modelNmSpace.$modelNm results:\n$rmModelResult\n$addResult"
        } else {
          val reasonForFailure: String = if (modDef != null) {
            ErrorCodeConstants.Update_Model_Failed_Invalid_Version
          } else {
            ErrorCodeConstants.Update_Model_Failed
          }
          val modDefName: String = if (modDef != null) modDef.FullName else "(pmml compile failed)"
          val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
          var apiResult = new ApiResult(ErrorCodeConstants.Failure
            , "AddModel"
            , null
            , s"$reasonForFailure : $modDefName.$modDefVer)")
          apiResult.toString()
        }
      } catch {
        case e: ModelCompilationFailedException => {
          logger.debug("", e)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure
            , s"UpdateModel(type = PMML)"
            , null
            , s"Error : ${e.toString} + ${ErrorCodeConstants.Update_Model_Failed}")
          apiResult.toString()
        }
        case e: AlreadyExistsException => {

          logger.debug("", e)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure
            , s"UpdateModel(type = PMML)"
            , null
            , s"Error : ${e.toString} + ${ErrorCodeConstants.Update_Model_Failed}")
          apiResult.toString()
        }
        case e: Exception => {

          logger.debug("", e)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure
            , s"UpdateModel(type = PMML)"
            , null
            , s"Error : ${e.toString} + ${ErrorCodeConstants.Update_Model_Failed}")
          apiResult.toString()
        }
      }
    } else {
      val apiResult = new ApiResult(ErrorCodeConstants.Failure
        , s"UpdateModel(type = PMML)"
        , null
        , s"The model name and new version was not supplied for this PMML model : name=$modelName version=$version\nOptionally one should consider supplying the exact version of the model being updated, especially important when you are maintaining multiple versions with the same model name and tweaking versions of same for your 'a/b/c...' score comparisons.")
      apiResult.toString()

    }
    result
  }

  /**
    * Update the java or scala model with new source.
    *
    * @param modelType the type of the model... JAVA | SCALA in this case
    * @param input     the source of the model to ingest
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @param modelName the name of the model to be ingested (PMML)
    *                  or the model's config for java and scala
    * @param version   the version number of the model to be updated (only relevant for PMML ingestion)
    * @return result string indicating success or failure of operation
    */
  private def UpdateCustomModel(modelType: ModelType.ModelType
                                , input: String
                                , userid: Option[String] = None
                                , tenantId: String = ""
                                , modelName: Option[String] = None
                                , version: Option[String] = None): String = {
    val sourceLang: String = modelType.toString

    /** to get here it is either 'java' or 'scala' */
    try {
      val compProxy = new CompilerProxy
      compProxy.setSessionUserId(userid)
      val modelNm: String = modelName.orNull
      val modDef: ModelDef = compProxy.compileModelFromSource(input, modelNm, sourceLang, userid, tenantId)

      /**
        * FIXME: The current strategy is that only the most recent version can be updated.
        * FIXME: This is not a satisfactory condition. It may be desirable to have 10 models all with
        * FIXME: the same name but differing only in their version numbers. If someone wants to tune
        * FIXME: #6 of the 10, that number six is not the latest.  It is just a unique model.
        *
        * For this reason, the version of the model that is to be changed should be supplied here and in the
        * more generic interface implementation that calls here.
        */

      val latestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val isValid: Boolean = if (latestVersion != None) MetadataAPIImpl.IsValidVersion(latestVersion.get, modDef) else true

      if (isValid && modDef != null) {
        MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.UPDATEOBJECT, input, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)
        val key = MdMgr.MkFullNameWithVersion(modDef.nameSpace, modDef.name, modDef.ver)
        if (latestVersion != None) {
          if (!tenantId.equalsIgnoreCase(latestVersion.get.tenantId)) {
            return (new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, s"Tenant ID is different from the one in the existing object.")).toString
          }
          RemoveModel(latestVersion.get.nameSpace, latestVersion.get.name, latestVersion.get.ver, None)
        }
        logger.info("Begin uploading dependent Jars, please wait...")
        PersistenceUtils.UploadJarsToDB(modDef)
        logger.info("Finished uploading dependent Jars.")
        val apiResult = AddModel(modDef, userid)
        var objectsUpdated = new Array[BaseElemDef](0)
        var operations = new Array[String](0)
        if (latestVersion != None) {
          objectsUpdated = objectsUpdated :+ latestVersion.get
          operations = operations :+ "Remove"
        }
        objectsUpdated = objectsUpdated :+ modDef
        operations = operations :+ "Add"
        MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
        apiResult
      } else {
        val reasonForFailure: String = if (modDef != null) ErrorCodeConstants.Add_Model_Failed_Higher_Version_Required else ErrorCodeConstants.Add_Model_Failed
        val modDefName: String = if (modDef != null) modDef.FullName else "(source compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ModelCompilationFailedException => {
        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"${'"'}UpdateModel(type = $sourceLang)${'"'}", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"${'"'}UpdateModel(type = $sourceLang)${'"'}", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"${'"'}UpdateModel(type = $sourceLang)${'"'}", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * UpdateModel - Update a Kamanja Pmml model
    *
    * Current semantics are that the source supplied in pmmlText is compiled and a new model is reproduced. The Kamanja
    * PMML version is specified in the KPMML source itself in the header's Version attribute. The version of the updated
    * model must be > the most recent cataloged one that is being updated. With this strategy ONLY the most recent
    * version can be updated.
    *
    * @param modelType    the type of the model submission... PMML in this case
    * @param pmmlText     the text element to be added dependent upon the modelType specified.
    * @param optUserid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                     method.
    * @param optModelName the model's namespace.name (ignored in this implementation of the UpdatePmmlModel... only used in PMML updates)
    * @param optVersion   the model's version (ignored in this implementation of the UpdatePmmlModel... only used in PMML updates)
    * @return result string indicating success or failure of operation
    */
  private def UpdateKPMMLModel(modelType: ModelType.ModelType
                               , pmmlText: String
                               , optUserid: Option[String] = None
                               , tenantId: String = ""
                               , optModelName: Option[String] = None
                               , optVersion: Option[String] = None
                               , optMsgProduced: Option[String]): String = {
    try {
      var compProxy = new CompilerProxy
      //compProxy.setLoggerLevel(Level.TRACE)
      val ownerId: String = if (optUserid == None) "kamanja" else optUserid.get
      var (classStr, modDef) = compProxy.compilePmml(pmmlText, ownerId, tenantId)
      val optLatestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val latestVersion: ModelDef = optLatestVersion.orNull

      /**
        * FIXME: The current strategy is that only the most recent version can be updated.
        * FIXME: This is not a satisfactory condition. It may be desirable to have 10 PMML models all with
        * FIXME: the same name but differing only in their version numbers. If someone wants to tune
        * FIXME: #6 of the 10, that number six is not the latest.  It is just a unique model.
        *
        * For this reason, the version of the model that is to be changed should be supplied here and in the
        * more generic interface implementation that calls here.
        */

      val isValid: Boolean = (modDef != null && latestVersion != null && latestVersion.Version < modDef.Version)

      if (isValid && modDef != null) {
        MetadataAPIImpl.logAuditRec(optUserid, Some(AuditConstants.WRITE), AuditConstants.UPDATEOBJECT, pmmlText, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)
        val key = MdMgr.MkFullNameWithVersion(modDef.nameSpace, modDef.name, modDef.ver)

        // when a version number changes, latestVersion  has different namespace making it unique
        // latest version may not be found in the cache. So need to remove it
        if (latestVersion != None) {
          // Make sure the TenantIds didn't change
          if (!tenantId.equalsIgnoreCase(latestVersion.tenantId)) {
            return (new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, s"Tenant ID is different from the one in the existing object.")).toString
          }
          RemoveModel(latestVersion.nameSpace, latestVersion.name, latestVersion.ver, None)
        }

        PersistenceUtils.UploadJarsToDB(modDef)
        val result = AddModel(modDef, optUserid)
        var objectsUpdated = new Array[BaseElemDef](0)
        var operations = new Array[String](0)

        if (latestVersion != None) {
          objectsUpdated = objectsUpdated :+ latestVersion
          operations = operations :+ "Remove"
        }

        objectsUpdated = objectsUpdated :+ modDef
        operations = operations :+ "Add"
        MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
        result

      } else {
        val reasonForFailure: String = if (modDef != null) ErrorCodeConstants.Update_Model_Failed_Invalid_Version else ErrorCodeConstants.Update_Model_Failed
        val modDefName: String = if (modDef != null) modDef.FullName else "(kpmml compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = KPMML)", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ObjectNotFoundException => {
        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = KPMML)", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = KPMML)", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * UpdateModel - Update a Json Transformation Model (JTM)
    *
    * Current semantics are that the source supplied in jtmText is compiled and a new model is reproduced.
    *
    * @param modelType    the type of the model submission... PMML in this case
    * @param jtmText      the text element to be added dependent upon the modelType specified.
    * @param optUserid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                     method.
    * @param optModelName the model's namespace.name (ignored in this implementation of the UpdatePmmlModel... only used in PMML updates)
    * @param optVersion   the model's version (ignored in this implementation of the UpdatePmmlModel... only used in PMML updates)
    * @return result string indicating success or failure of operation
    */
  private def UpdateJTMModel(modelType: ModelType.ModelType
                             , jtmText: String
                             , optUserid: Option[String] = None
                             , tenantId: String = ""
                             , optModelName: Option[String] = None
                             , optVersion: Option[String] = None): String = {
    try {
      var compProxy = new CompilerProxy
      var compileConfig = ""
      //compProxy.setLoggerLevel(Level.TRACE)

      val usr = if (optUserid == None) "Kamanja" else optUserid.get

      // Getting external dependency jars
      val extDepJars =
        if (optModelName != None) {
          var cfgName = optModelName.get.toLowerCase
          if (cfgName.length > 0 && !cfgName.startsWith(usr.toLowerCase() + "."))
            cfgName = usr.toLowerCase() + "." + cfgName
          var cfg = MdMgr.GetMdMgr.GetModelConfig(cfgName)
          compileConfig = JsonSerializer.SerializeModelConfigToJson(cfgName, cfg)
          // MetadataAPIImpl.getModelDependencies(cfgName, optUserid)
          GetTypesAndJarsDependencies(cfgName, optUserid)
        } else {
          List[String]()
        }

      var (classStr, modDef) = compProxy.compileJTM(jtmText, tenantId, extDepJars, usr, compileConfig)
      val optLatestVersion = if (modDef == null) None else GetLatestModel(modDef)
      val latestVersion: ModelDef = optLatestVersion.orNull

      /**
        * FIXME: The current strategy is that only the most recent version can be updated.
        * FIXME: This is not a satisfactory condition. It may be desirable to have 10 PMML models all with
        * FIXME: the same name but differing only in their version numbers. If someone wants to tune
        * FIXME: #6 of the 10, that number six is not the latest.  It is just a unique model.
        *
        * For this reason, the version of the model that is to be changed should be supplied here and in the
        * more generic interface implementation that calls here.
        */

      val isValid: Boolean = (modDef != null && latestVersion != null && latestVersion.Version < modDef.Version)

      if (isValid && modDef != null) {
        MetadataAPIImpl.logAuditRec(optUserid, Some(AuditConstants.WRITE), AuditConstants.UPDATEOBJECT, jtmText, AuditConstants.SUCCESS, "", modDef.FullNameWithVer)
        val key = MdMgr.MkFullNameWithVersion(modDef.nameSpace, modDef.name, modDef.ver)

        // when a version number changes, latestVersion  has different namespace making it unique
        // latest version may not be found in the cache. So need to remove it
        if (latestVersion != None) {
          if (!tenantId.equalsIgnoreCase(latestVersion.tenantId)) {
            return (new ApiResult(ErrorCodeConstants.Failure, "UpdateModel", null, s"Tenant ID is different from the one in the existing object.")).toString
          }
          RemoveModel(latestVersion.nameSpace, latestVersion.name, latestVersion.ver, None)
        }

        PersistenceUtils.UploadJarsToDB(modDef)
        val result = AddModel(modDef, optUserid)
        var objectsUpdated = new Array[BaseElemDef](0)
        var operations = new Array[String](0)

        if (latestVersion != None) {
          objectsUpdated = objectsUpdated :+ latestVersion
          operations = operations :+ "Remove"
        }

        objectsUpdated = objectsUpdated :+ modDef
        operations = operations :+ "Add"
        MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
        result

      } else {
        val reasonForFailure: String = if (modDef != null) ErrorCodeConstants.Update_Model_Failed_Invalid_Version else ErrorCodeConstants.Update_Model_Failed
        val modDefName: String = if (modDef != null) modDef.FullName else "(jtm compile failed)"
        val modDefVer: String = if (modDef != null) MdMgr.Pad0s2Version(modDef.Version) else MdMgr.UnknownVersion
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = JTM)", null, reasonForFailure + ":" + modDefName + "." + modDefVer)
        apiResult.toString()
      }
    } catch {
      case e: ObjectNotFoundException => {
        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = JTM)", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
      case e: Exception => {

        logger.debug("", e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, s"UpdateModel(type = JTM)", null, "Error :" + e.toString() + ErrorCodeConstants.Update_Model_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * GetDependentModels
    *
    * @param msgNameSpace
    * @param msgName
    * @param msgVer
    * @return
    */
  def GetDependentModels(msgNameSpace: String, msgName: String, msgVer: Long): Array[ModelDef] = {
    MessageAndContainerUtils.GetDependentModels(msgNameSpace, msgName, msgVer)
  }

  /**
    * Get all available models (format JSON or XML) as string.
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid     the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                   method. If Security and/or Audit are configured, this value must be a value other than None
    * @return string representation in specified format.
    */
  def GetAllModelDefs(formatType: String, userid: Option[String] = None): String = {
    try {
      val modDefs = MdMgr.GetMdMgr.Models(true, true)
      modDefs match {
        case None =>
          None
          logger.debug("No Models found ")
          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllModelDefs", null, ErrorCodeConstants.Get_All_Models_Failed_Not_Available)
          apiResult.toString()
        case Some(ms) =>
          val msa = ms.toArray
          val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllModelDefs", JsonSerializer.SerializeObjectListToJson("Models", msa), ErrorCodeConstants.Get_All_Models_Successful)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllModelDefs", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Models_Failed)
        apiResult.toString()
      }
    }
  }

  /**
    * GetAllModelsFromCache
    *
    * @param active
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetAllModelsFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    var modelList: Array[String] = new Array[String](0)
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.MODEL, AuditConstants.SUCCESS, "", AuditConstants.MODEL)
    try {
      val modDefs = MdMgr.GetMdMgr.Models(active, true)
      modDefs match {
        case None =>
          None
          logger.debug("No Models found ")
          modelList
        case Some(ms) =>
          val msa = ms.toArray
          val modCount = msa.length
          modelList = new Array[String](modCount)
          for (i <- 0 to modCount - 1) {
            modelList(i) = msa(i).FullName + "." + MdMgr.Pad0s2Version(msa(i).Version)
          }
          modelList
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        throw UnexpectedMetadataAPIException("Failed to fetch all the models:" + e.toString, e)
      }
    }
  }


  // Specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
  /**
    *
    * @param nameSpace  namespace of the object
    * @param objectName name of the desired object, possibly namespace qualified
    * @param formatType format of the return value, either JSON or XML
    * @param userid     the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                   method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetModelDef(nameSpace: String, objectName: String, formatType: String, userid: Option[String]): String = {
    try {
      val modDefs = MdMgr.GetMdMgr.Models(nameSpace, objectName, true, true)
      modDefs match {
        case None =>
          None
          logger.debug("No Models found ")
          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetModelDef", null, ErrorCodeConstants.Get_Model_Failed_Not_Available + ":" + nameSpace + "." + objectName)
          apiResult.toString()
        case Some(ms) =>
          val msa = ms.toArray
          val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetModelDef", JsonSerializer.SerializeObjectListToJson("Models", msa), ErrorCodeConstants.Get_Model_Successful)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetModelDef", null, "Error :" + e.toString() + ErrorCodeConstants.Get_Model_Failed + ":" + nameSpace + "." + objectName)
        apiResult.toString()
      }
    }
  }

  /**
    * Get a specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
    *
    * @param objectName name of the desired object, possibly namespace qualified
    * @param formatType format of the return value, either JSON or XML
    * @return
    */
  def GetModelDef(objectName: String, formatType: String, userid: Option[String] = None): String = {
    GetModelDef(sysNS, objectName, formatType, userid)
  }

  /**
    * Get a specific model (format JSON or XML) as a String using modelName(with version) as the key
    *
    * @param nameSpace  namespace of the object
    * @param name
    * @param formatType format of the return value, either JSON or XML
    * @param version    Version of the object
    * @param userid     the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                   method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetModelDefFromCache(nameSpace: String, name: String, formatType: String, version: String, userid: Option[String] = None): String = {
    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version.toLong)
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.GETOBJECT, AuditConstants.MODEL, AuditConstants.SUCCESS, "", dispkey)
    try {
      var key = nameSpace + "." + name + "." + version.toLong
      val o = MdMgr.GetMdMgr.Model(nameSpace.toLowerCase, name.toLowerCase, version.toLong, true)
      o match {
        case None =>
          None
          logger.debug("model not found => " + dispkey)
          val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetModelDefFromCache", null, ErrorCodeConstants.Get_Model_From_Cache_Failed_Not_Active + ":" + dispkey)
          apiResult.toString()
        case Some(m) =>
          logger.debug("model found => " + m.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[ModelDef].Version))
          val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetModelDefFromCache", JsonSerializer.SerializeObjectToJson(m), ErrorCodeConstants.Get_Model_From_Cache_Successful + ":" + dispkey)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetModelDefFromCache", null, "Error :" + e.toString() + ErrorCodeConstants.Get_Model_From_Cache_Failed + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  // Specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
  /**
    *
    * @param nameSpace  namespace of the object
    * @param objectName name of the desired object, possibly namespace qualified
    * @param formatType format of the return value, either JSON or XML
    * @param version    Version of the object
    * @param userid     the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                   method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetModelDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String]): String = {
    MetadataAPIImpl.logAuditRec(userid
      , Some(AuditConstants.READ)
      , AuditConstants.GETOBJECT
      , AuditConstants.MODEL
      , AuditConstants.SUCCESS
      , ""
      , nameSpace + "." + objectName + "." + version)
    GetModelDefFromCache(nameSpace, objectName, formatType, version, None)
  }

  /**
    * Check whether model already exists in metadata manager. Ideally,
    * we should never add the model into metadata manager more than once
    * and there is no need to use this function in main code flow
    * This is just a utility function being used during these initial phases
    *
    * @param modDef the model def to be tested
    * @return
    */
  def DoesModelAlreadyExist(modDef: ModelDef): Boolean = {
    try {
      var key = modDef.nameSpace + "." + modDef.name + "." + modDef.ver
      val dispkey = modDef.nameSpace + "." + modDef.name + "." + MdMgr.Pad0s2Version(modDef.ver)
      val o = MdMgr.GetMdMgr.Model(modDef.nameSpace.toLowerCase,
        modDef.name.toLowerCase,
        modDef.ver,
        false)
      o match {
        case None =>
          None
          logger.debug("model not in the cache => " + dispkey)
          return false;
        case Some(m) =>
          logger.debug("model found => " + m.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[ModelDef].ver))
          return true
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        throw UnexpectedMetadataAPIException(e.getMessage(), e)
      }
    }
  }

  /**
    * Get the latest model for a given FullName
    *
    * @param modDef
    * @return
    */
  def GetLatestModel(modDef: ModelDef): Option[ModelDef] = {
    try {
      var key = modDef.nameSpace + "." + modDef.name + "." + modDef.ver
      val dispkey = modDef.nameSpace + "." + modDef.name + "." + MdMgr.Pad0s2Version(modDef.ver)
      val o = MdMgr.GetMdMgr.Models(modDef.nameSpace.toLowerCase,
        modDef.name.toLowerCase, false, true)
      o match {
        case None =>
          None
          logger.debug("model not in the cache => " + dispkey)
          None
        case Some(m) =>
          if (m.size > 0) {
            logger.debug("model found => " + m.head.asInstanceOf[ModelDef].FullName + "." + MdMgr.Pad0s2Version(m.head.asInstanceOf[ModelDef].ver))
            Some(m.head.asInstanceOf[ModelDef])
          } else
            None
      }
    } catch {
      case e: Exception => {

        logger.debug("", e)
        throw UnexpectedMetadataAPIException(e.getMessage(), e)
      }
    }
  }

  //
  /**
    * Get the latest cataloged models from the supplied set
    *
    * @param modelSet
    * @return
    */
  def GetLatestModelFromModels(modelSet: Set[ModelDef]): ModelDef = {
    var model: ModelDef = null
    var verList: List[Long] = List[Long]()
    var modelmap: scala.collection.mutable.Map[Long, ModelDef] = scala.collection.mutable.Map()
    try {
      modelSet.foreach(m => {
        modelmap.put(m.Version, m)
        verList = m.Version :: verList
      })
      model = modelmap.getOrElse(verList.max, null)
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw new Exception("Error in traversing Model set", e)
      }
    }
    model
  }

  /**
    * Get a specific model definition from persistent store
    *
    * @param nameSpace  namespace of the object
    * @param objectName name of the desired object, possibly namespace qualified
    * @param formatType format of the return value, either JSON or XML
    * @param version    Version of the object
    * @param userid     the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                   method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetModelDefFromDB(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String] = None): String = {
    var key = "ModelDef" + "." + nameSpace + '.' + objectName + "." + version.toLong
    val dispkey = "ModelDef" + "." + nameSpace + '.' + objectName + "." + MdMgr.Pad0s2Version(version.toLong)
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.GETOBJECT, AuditConstants.MODEL, AuditConstants.SUCCESS, "", dispkey)
    try {
      var obj = PersistenceUtils.GetObject(key.toLowerCase, "models")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "GetModelDefFromCache", new String(obj._2.asInstanceOf[Array[Byte]]), ErrorCodeConstants.Get_Model_From_DB_Successful + ":" + dispkey)
      apiResult.toString()
    } catch {
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetModelDefFromCache", null, "Error :" + e.toString() + ErrorCodeConstants.Get_Model_From_DB_Failed + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  /**
    * LoadAllModelConfigsIntoChache
    */
  private def LoadAllModelConfigsIntoCache: Unit = {
    val maxTranId = PersistenceUtils.GetTranId
    MetadataAPIImpl.setCurrentTranLevel(maxTranId)
    logger.debug("Max Transaction Id => " + maxTranId)

    var processed: Long = 0L
    val storeInfo = PersistenceUtils.GetContainerNameAndDataStore("model_config_objects")
    storeInfo._2.get(storeInfo._1, { (k: Key, v: Any, serType: String, typ: String, ver: Int) => {
      processed += 1
      val conf = serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[Map[String, List[String]]]
      MdMgr.GetMdMgr.AddModelConfig(k.bucketKey.mkString("."), conf)
    }
    })

    if (processed == 0) {
      logger.debug("No model config objects available in the Database")
      return
    }
    MdMgr.GetMdMgr.DumpModelConfigs
  }


  /**
    * LoadModelIntoCache
    *
    * @param key
    */
  def LoadModelIntoCache(key: String) {
    try {
      logger.debug("Fetch the object " + key + " from database ")
      val obj = PersistenceUtils.GetObject(key.toLowerCase, "models")
      logger.debug("Deserialize the object " + key)
      val model = MetadataAPISerialization.deserializeMetadata(new String(obj._2.asInstanceOf[Array[Byte]]))//serializer.DeserializeObjectFromByteArray(obj._2.asInstanceOf[Array[Byte]])
      logger.debug("Get the jar from database ")
      val modDef = model.asInstanceOf[ModelDef]
      MetadataAPIImpl.DownloadJarFromDB(modDef)
      logger.debug("Add the object " + key + " to the cache ")
      MetadataAPIImpl.AddObjectToCache(modDef, MdMgr.GetMdMgr)
    } catch {
      case e: Exception => {

        logger.debug("", e)
      }
    }
  }

  /**
    * Answer the model compilation dependencies
    * FIXME: Which ones? input or output?
    *
    * @param modelConfigName
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def getModelDependencies(modelConfigName: String, userid: Option[String] = None): List[String] = {
    var config = MdMgr.GetMdMgr.GetModelConfig(modelConfigName)
    val tmpDeps = config.getOrElse(ModelCompilationConstants.DEPENDENCIES, null)

    if (tmpDeps != null) {
      if (tmpDeps.isInstanceOf[List[_]])
        return tmpDeps.asInstanceOf[List[String]]
      if (tmpDeps.isInstanceOf[Array[_]])
        return tmpDeps.asInstanceOf[Array[String]].toList
    }

    List[String]()
  }
}
