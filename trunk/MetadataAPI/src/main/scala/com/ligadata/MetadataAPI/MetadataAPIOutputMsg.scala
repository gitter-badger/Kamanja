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

import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.Exceptions._

import org.json4s._

import com.ligadata.Serialize._
import com.ligadata.AuditAdapterInfo._
import com.ligadata.Exceptions.StackTrace

object MetadataAPIOutputMsg {

  def AddOutputMessage(outputMsgText: String, format: String, userid: Option[String]): String = {
    try {
      if (format != "JSON") {
        var apiResult = new ApiResult(ErrorCodeConstants.Not_Implemented_Yet, "AddOutputMsg", null, ErrorCodeConstants.Not_Implemented_Yet_Msg + ":" + outputMsgText)
        apiResult.toString()
      } else {
        var outputMsg = com.ligadata.outputmsgdef.OutputMsgDefImpl.parseOutputMessageDef(outputMsgText, "JSON")
        val dispkey = outputMsg.FullName + "." + MdMgr.Pad0s2Version(outputMsg.Version)
        MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, outputMsgText, AuditConstants.SUCCESS, "", outputMsg.FullNameWithVer)
        MetadataAPIImpl.SaveObject(outputMsg, MdMgr.GetMdMgr)
        MetadataAPIImpl.AddObjectToCache(outputMsg, MdMgr.GetMdMgr)
        var objectsAdded = new Array[BaseElemDef](0)
        objectsAdded = objectsAdded :+ outputMsg
        MetadataAPIImpl.SaveOutputMsObjectList(objectsAdded)
        val operations = for (op <- objectsAdded) yield "Add"
        logger.trace("Notify engine via zookeeper")
        MetadataAPIImpl.NotifyEngine(objectsAdded, operations)
        var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddOutputMsg", null, ErrorCodeConstants.Add_OutputMessage_Successful + ":" + dispkey)
        apiResult.toString()

      }
    } catch {
      case e: MappingException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to parse the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddOutputMsg", null, "Error :" + ErrorCodeConstants.Add_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to add the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddOutputMsg", null, "Error :" + e.toString() + ErrorCodeConstants.Add_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: ObjectNolongerExistsException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to add the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddOutputMsg", null, "Error :" + e.toString() + ErrorCodeConstants.Add_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to up the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddOutputMsg", null, "Error :" + e.toString() + ErrorCodeConstants.Add_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
    }
  }

  def UpdateOutputMsg(outputMsgText: String, userid: Option[String]): String = {
    try {
      var outputMsgDef = com.ligadata.outputmsgdef.OutputMsgDefImpl.parseOutputMessageDef(outputMsgText, "JSON")
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.UPDATEOBJECT, outputMsgText, AuditConstants.SUCCESS, "", outputMsgDef.FullNameWithVer)

      val dispkey = outputMsgDef.FullName + "." + MdMgr.Pad0s2Version(outputMsgDef.Version)
      val key = MdMgr.MkFullNameWithVersion(outputMsgDef.nameSpace, outputMsgDef.name, outputMsgDef.ver)
      val latestVersion = GetLatestOutputMsg(outputMsgDef)
      var isValid = true
      if (latestVersion != None) {
        isValid = MetadataAPIImpl.IsValidVersion(latestVersion.get, outputMsgDef)
        println("isValid  " + isValid)
      }
      if (isValid) {
        RemoveOutputMsg(latestVersion.get.nameSpace, latestVersion.get.name, latestVersion.get.ver, userid)
        val result = AddOutputMsg(outputMsgDef)
        var objectsUpdated = new Array[BaseElemDef](0)
        var operations = new Array[String](0)
        objectsUpdated = objectsUpdated :+ latestVersion.get
        operations = operations :+ "Remove"
        objectsUpdated = objectsUpdated :+ outputMsgDef
        operations = operations :+ "Add"
        MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
        result
      } else {
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateOutputMessage", null, "Error :" + null + ErrorCodeConstants.Update_OutputMessage_Failed + ":" + dispkey)
        apiResult.toString()
      }
    } catch {
      case e: MappingException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to parse the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateOutputMessage", null, "Error :" + ErrorCodeConstants.Update_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: AlreadyExistsException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.trace("Failed to add the output message, json => " + outputMsgText + ",Error => " + e.getMessage() + "\nStackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateOutputMessage", null, "Error :" + e.toString() + ErrorCodeConstants.Update_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: ObjectNolongerExistsException => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateOutputMessage", null, "Error :" + e.toString() + ErrorCodeConstants.Update_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "UpdateOutputMessage", null, "Error :" + e.toString() + ErrorCodeConstants.Update_OutputMessage_Failed + ":" + outputMsgText)
        apiResult.toString()
      }
    }
  }

  // Get the latest output message for a given FullName
  def GetLatestOutputMsg(outputMsgDef: OutputMsgDef): Option[OutputMsgDef] = {
    try {
      var key = outputMsgDef.FullName + "." + MdMgr.Pad0s2Version(outputMsgDef.ver.toLong)
      val o = MdMgr.GetMdMgr.OutputMessages(outputMsgDef.nameSpace.toLowerCase,
        outputMsgDef.name.toLowerCase,
        false,
        true)
      o match {
        case None =>
          None
          logger.trace("Output msg not in the cache => " + key)
          None
        case Some(o) =>
          if (o.size > 0) {
            logger.trace("output msg found => " + o.head.asInstanceOf[OutputMsgDef].FullNameWithVer)
            Some(o.head.asInstanceOf[OutputMsgDef])

          } else
            None
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug("StackTrace:" + stackTrace)
        throw UnexpectedMetadataAPIException(e.getMessage(), e)
      }
    }
  }

  // Add Output (outputmsg def)
  def AddOutputMsg(outputMsgDef: OutputMsgDef): String = {
    var key = outputMsgDef.FullName + "." + MdMgr.Pad0s2Version(outputMsgDef.Version)
    try {

      MetadataAPIImpl.SaveObject(outputMsgDef, MdMgr.GetMdMgr)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddOutputMsg", null, ErrorCodeConstants.Add_OutputMessage_Successful + ":" + key)
      apiResult.toString()
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug("StackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddOutputMsg", null, "Error :" + e.toString() + ErrorCodeConstants.Add_OutputMessage_Failed + ":" + key)
        apiResult.toString()
      }
    }
  }

  def GetAllOutputMsgsFromCache(active: Boolean, userid: Option[String]): Array[String] = {
    var outputMsgList: Array[String] = new Array[String](0)
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.OUTPUTMSG, AuditConstants.SUCCESS, "", AuditConstants.OUTPUTMSG)

    try {
      val outputMsgDefs = MdMgr.GetMdMgr.OutputMessages(active, true)
      outputMsgDefs match {
        case None =>
          None
          logger.debug("No Output Msgs found ")
          outputMsgList
        case Some(o) =>
          val os = o.toArray
          val oCount = os.length
          outputMsgList = new Array[String](oCount)
          for (i <- 0 to oCount - 1) {
            outputMsgList(i) = os(i).FullName + "." + MdMgr.Pad0s2Version(os(i).Version)
          }
          outputMsgList
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug("StackTrace:" + stackTrace)
        throw UnexpectedMetadataAPIException("Failed to fetch all the OutputMsgs:" + e.toString, e)
      }
    }
  }

  def GetOutputMessageDef(objectName: String, formatType: String, userid: Option[String]): String = {
    val nameSpace = MdMgr.sysNS
    GetOutputMessageDefFromCache(nameSpace, objectName, formatType, "-1", userid)
  }

  def GetOutputMessageDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String]): String = {
    GetOutputMessageDefFromCache(nameSpace, objectName, formatType, version, userid)
  }
  // Specific message (format JSON or XML) as a String using messageName(with version) as the key
  def GetOutputMessageDefFromCache(nameSpace: String, name: String, formatType: String, version: String, userid: Option[String]): String = {

    val dispkey = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version.toLong)
    var key = nameSpace + "." + name + "." + version.toLong
    if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.OUTPUTMSG, AuditConstants.SUCCESS, "", AuditConstants.OUTPUTMSG)

    try {
      val o = MdMgr.GetMdMgr.OutputMessage(nameSpace.toLowerCase, name.toLowerCase, version.toLong, true)
      o match {
        case None =>
          None
          logger.debug("output message not found => " + dispkey)
          var apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetOutputMessageDefFromCache", null, ErrorCodeConstants.Get_OutputMessage_From_Cache_Failed + ":" + dispkey)
          apiResult.toString()
        case Some(m) =>
          logger.debug("output message found => " + m.asInstanceOf[OutputMsgDef].FullName + "." + MdMgr.Pad0s2Version(m.asInstanceOf[OutputMsgDef].Version))
          var apiResult = new ApiResult(ErrorCodeConstants.Success, "GetOutputMessageDefFromCache", JsonSerializer.SerializeObjectToJson(m), ErrorCodeConstants.Get_OutputMessage_From_Cache_Successful)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug("StackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetOutputMessageDefFromCache", null, "Error :" + e.toString() + ErrorCodeConstants.Get_OutputMessage_From_Cache_Failed + ":" + dispkey)
        apiResult.toString()
      }
    }
  }

  // Remove output message with OutputMsg Name and Version Number
  def RemoveOutputMsg(nameSpace: String, name: String, version: Long, userid: Option[String]): String = {
    var key = nameSpace + "." + name + "." + MdMgr.Pad0s2Version(version)
    try {
      val om = MdMgr.GetMdMgr.OutputMessage(nameSpace.toLowerCase, name.toLowerCase, version, true)

      om match {
        case None =>
          None
          logger.debug("output message not found => " + key)
          var apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveOutputMsg", null, ErrorCodeConstants.Remove_OutputMessage_Failed + ":" + key)
          apiResult.toString()
        case Some(o) =>
          logger.debug("output message found => " + o.asInstanceOf[OutputMsgDef].FullNameWithVer)
          if (userid != None) MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.DELETEOBJECT, "Model", AuditConstants.SUCCESS, "", o.asInstanceOf[OutputMsgDef].FullNameWithVer)
          MetadataAPIImpl.DeleteObject(o.asInstanceOf[OutputMsgDef])
          var objectsUpdated = new Array[BaseElemDef](0)
          var ob = o.asInstanceOf[OutputMsgDef]
          ob.tranId = MetadataAPIImpl.GetNewTranId
          objectsUpdated = objectsUpdated :+ ob
          var operations = for (op <- objectsUpdated) yield "Remove"
          MetadataAPIImpl.NotifyEngine(objectsUpdated, operations)
          var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveOutputMsg", null, ErrorCodeConstants.Remove_OutputMessage_Successful + ":" + key)
          apiResult.toString()
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug("StackTrace:" + stackTrace)
        var apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveOutputMsg", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_OutputMessage_Failed + ":" + key)
        apiResult.toString()
      }
    }
  }

}