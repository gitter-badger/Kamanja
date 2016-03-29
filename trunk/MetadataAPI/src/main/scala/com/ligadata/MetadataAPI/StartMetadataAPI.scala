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

import org.apache.logging.log4j.{ Logger, LogManager }

import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.MetadataAPI.Utility._
import com.ligadata.kamanja.metadata.MdMgr

import scala.collection.mutable
import scala.collection.immutable
import com.ligadata.KamanjaVersion.KamanjaVersion

/**
 * Created by dhaval Kolapkar on 7/24/15.
 */

object StartMetadataAPI {

  var response = ""
  //get default config
  val defaultConfig = scala.util.Properties.envOrElse("KAMANJA_HOME", scala.util.Properties.envOrElse("HOME", "~" )) + "/config/MetadataAPIConfig.properties"
  val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  var action = ""
  var location = ""
  var config = ""
  val WITHDEP = "dependsOn"
  val REMOVE = "remove"
  val GET = "get"
  val ACTIVATE = "activate"
  val OUTPUTMSG = "outputmsg"
  val DEACTIVATE = "deactivate"
  val UPDATE = "update"
  val MODELS = "models"
  val MESSAGES = "messages"
  val CONTAINERS = "containers"
  var expectDep = false
  var expectOutputMsg = false
  var expectRemoveParm = false
  var depName: String = ""
  var outputMsgName: String = ""
  var parmName: String = ""
  val MODELNAME = "MODELNAME"
  val MODELVERSION= "MODELVERSION"
  val MESSAGENAME="MESSAGENAME"
  val extraCmdArgs = mutable.Map[String, String]()
  var expectModelName = false
  var expectModelVer = false
  var expectMessageName = false

  def main(args: Array[String]) {
    if (args.length > 0 && args(0).equalsIgnoreCase("--version")) {
      KamanjaVersion.print
      return
    }

    /** FIXME: the user id should be discovered in the parse of the args array */
    val userId: Option[String] = Some("Kamanja")
    try {
      var argsUntilParm = 2

      args.foreach(arg =>
        if (arg.equalsIgnoreCase(UPDATE) || arg.equalsIgnoreCase(MODELS) || arg.equalsIgnoreCase(MESSAGES) || arg.equalsIgnoreCase(CONTAINERS)) {
          argsUntilParm = 3
        }
      )
      args.foreach(arg => {

        if (arg.endsWith(".json") || arg.endsWith(".xml") || arg.endsWith(".pmml") || arg.endsWith(".scala") || arg.endsWith(".java") || arg.endsWith(".jar")) {
          location = arg

        } else if (arg.endsWith(".properties")) {
          config = arg

        } else {
          if (arg.equalsIgnoreCase(WITHDEP)) {
            expectDep = true
          }
          else if (expectDep) {
            depName = arg
            expectDep = false
          }
	  else if ( arg.equalsIgnoreCase(OUTPUTMSG) ){
	    expectOutputMsg = true
	  }
	  else if(expectOutputMsg ){
	    outputMsgName = arg
	    logger.debug("Found output message definition " + outputMsgName + " in the command ")
	    expectOutputMsg = false
	  }
           else if ((action.equalsIgnoreCase(Action.ADDMODELPMML.toString) || action.equalsIgnoreCase(Action.UPDATEMODELPMML.toString)) && location.size > 0) {
            if(arg.equalsIgnoreCase(MODELNAME)){
              expectModelName=true
            }else if(arg.equalsIgnoreCase(MODELVERSION)){
              expectModelVer=true
            }else if(arg.equalsIgnoreCase(MESSAGENAME)){
              expectMessageName=true
            }
            else if (expectModelName) {
              extraCmdArgs(MODELNAME) = arg
              expectModelName = false
            }
            else if (expectModelVer) {
              extraCmdArgs(MODELVERSION) = arg
              expectModelVer = false
            }
            else if (expectMessageName) {
              extraCmdArgs(MESSAGENAME) = arg
              expectMessageName = false

           }
          }
          else {
            if ((arg.equalsIgnoreCase(REMOVE)) || (arg.equalsIgnoreCase(GET)) || (arg.equalsIgnoreCase(ACTIVATE)) || (arg.equalsIgnoreCase(DEACTIVATE)) || (arg.equalsIgnoreCase(UPDATE))) {
              expectRemoveParm = true
            }
            if (expectRemoveParm) {
              argsUntilParm = argsUntilParm - 1
            }

            if (argsUntilParm < 0) {
              depName = arg
            }
            else if (arg != "debug")
            /** ignore the debug tag */ {
              /** concatenate the args together to form the action string... "add model pmml" becomes "addmodelpmmml" */
              action += arg
            }
          }
        }
      })
      //add configuration
      if (config == "") {
        println("Using default configuration " + defaultConfig)
        config = defaultConfig
      }

      MetadataAPIImpl.InitMdMgrFromBootStrap(config, false)
      if (action == "")
        TestMetadataAPI.StartTest
      else {
        response = route(Action.withName(action.trim), location, depName, outputMsgName, args, userId ,extraCmdArgs.toMap)
        println("Result: " + response)
      }
    }
    catch {
      case nosuchelement: NoSuchElementException => {
        logger.error("", nosuchelement)
        /** preserve the original response ... */
        response = s"Invalid command action! action=$action"

        /** one more try ... going the alternate route */  // do we still need this ??
        val altResponse: String = AltRoute(args)
        if (altResponse != null) {
          //response = altResponse
          println(response)
          usage
        } else {
          /* if the AltRoute doesn't produce a valid result, we will complain with the original failure */
          println(response)
          usage
        }
      }
      case e: Throwable => {
        logger.error("", e)
        e.getStackTrace.toString
      }
    } finally {
      MetadataAPIImpl.shutdown
    }
  }

  def usage : Unit = {
      println(s"Usage:\n  kamanja <action> <optional input> \n e.g. kamanja add message ${'$'}HOME/msg.json" )
  }

  def route(action: Action.Value, input: String, param: String = "", outputMsgName: String = null, originalArgs: Array[String], userId: Option[String] ,extraCmdArgs:immutable.Map[String, String]): String = {
    var response = ""
    var optMsgProduced:Option[String] = None	  
    if( outputMsgName != null ){
      logger.debug("The value of argument optMsgProduced will be " + outputMsgName)
      optMsgProduced = Some(outputMsgName)
    }
    try {
      action match {
        //message management
        case Action.ADDMESSAGE => response = MessageService.addMessage(input)
        case Action.UPDATEMESSAGE => response = MessageService.updateMessage(input)
        case Action.REMOVEMESSAGE => {
          if (param.length == 0)
            response = MessageService.removeMessage()
          else
            response = MessageService.removeMessage(param)
        }

        case Action.GETALLMESSAGES => response = MessageService.getAllMessages
        case Action.GETMESSAGE => {
          if (param.length == 0)
            response = MessageService.getMessage()
          else
            response = MessageService.getMessage(param)
        }

        //model management
        case Action.ADDMODELKPMML => response = ModelService.addModelKPmml(input, userId,optMsgProduced)
        case Action.ADDMODELJTM => response = ModelService.addModelJTM(input, userId)
        case Action.ADDMODELPMML => {
          val modelName: Option[String] = extraCmdArgs.get(MODELNAME)
          val modelVer = extraCmdArgs.getOrElse(MODELVERSION, null)
          val msgName : Option[String]= extraCmdArgs.get(MESSAGENAME)
          val validatedModelVersion = if (modelVer != null) MdMgr.FormatVersion(modelVer) else null
          val optModelVer =  Option(validatedModelVersion)
          val optMsgVer = Option(null)
          response = ModelService.addModelPmml(ModelType.PMML
                                            , input
                                            , userId
                                            , modelName
                                            , optModelVer
                                            , msgName
                                            , optMsgVer
					    , optMsgProduced)
        }

        case Action.ADDMODELSCALA => {
          if (param.length == 0)
            response = ModelService.addModelScala(input, "", userId,optMsgProduced)
          else
            response = ModelService.addModelScala(input, param, userId,optMsgProduced)
        }

        case Action.ADDMODELJAVA => {
          if (param.length == 0)
            response = ModelService.addModelJava(input, "", userId,optMsgProduced)
          else
            response = ModelService.addModelJava(input, param, userId,optMsgProduced)
        }

        case Action.REMOVEMODEL => {
          if (param.length == 0)
            response = ModelService.removeModel("", userId)
          else
            response = ModelService.removeModel(param)
        }

        case Action.ACTIVATEMODEL =>
          response = {
            if (param.length == 0)
              ModelService.activateModel("", userId)
            else
              ModelService.activateModel(param, userId)
          }

        case Action.DEACTIVATEMODEL => response = {
          if (param.length == 0)
            ModelService.deactivateModel("", userId)
          else
            ModelService.deactivateModel(param, userId)
        }
        case Action.UPDATEMODELKPMML => response = ModelService.updateModelKPmml(input, userId)
        case Action.UPDATEMODELJTM => response = ModelService.updateModelJTM(input, userId)

        case Action.UPDATEMODELPMML => {
          val modelName = extraCmdArgs.getOrElse(MODELNAME, "")
          val modelVer = extraCmdArgs.getOrElse(MODELVERSION, null)
          var validatedNewVersion: String = if (modelVer != null) MdMgr.FormatVersion(modelVer) else null
          response = ModelService.updateModelPmml(input, userId, modelName, validatedNewVersion )
          }

        //case Action.UPDATEMODELSCALA => response = ModelService.updateModelscala(input)
        //case Action.UPDATEMODELJAVA => response = ModelService.updateModeljava(input)

        case Action.UPDATEMODELSCALA => {
          if (param.length == 0)
            response = ModelService.updateModelscala(input, "", userId)
          else
            response = ModelService.updateModelscala(input, param, userId)
        }

        case Action.UPDATEMODELJAVA => {
          if (param.length == 0)
            response = ModelService.updateModeljava(input, "", userId)
          else
            response = ModelService.updateModeljava(input, param, userId)
        }

        case Action.GETALLMODELS => response = ModelService.getAllModels(userId)
        case Action.GETMODEL => response = {
          if (param.length == 0)
            ModelService.getModel("", userId)
          else
            ModelService.getModel(param, userId)
        }


        //container management
        case Action.ADDCONTAINER => response = ContainerService.addContainer(input)
        case Action.UPDATECONTAINER => response = ContainerService.updateContainer(input)
        case Action.GETCONTAINER => response = {
          if (param.length == 0)
            ContainerService.getContainer()
          else
            ContainerService.getContainer(param)
        }

        case Action.GETALLCONTAINERS => response = ContainerService.getAllContainers

        case Action.REMOVECONTAINER => {
          if (param.length == 0)
            response = ContainerService.removeContainer()
          else
            response = ContainerService.removeContainer(param)
        }

        //Type management
        case Action.ADDTYPE => response = TypeService.addType(input)
        case Action.GETTYPE => response = {
          if (param.length == 0)
            TypeService.getType()
          else
            TypeService.getType(param)
        }

        case Action.GETALLTYPES => response = TypeService.getAllTypes
        case Action.REMOVETYPE => response = {
          if (param.length == 0)
            TypeService.removeType()
          else
            TypeService.removeType(param)

        }
        case Action.LOADTYPESFROMAFILE => response = TypeService.loadTypesFromAFile(input)
        case Action.DUMPALLTYPESBYOBJTYPEASJSON => response = TypeService.dumpAllTypesByObjTypeAsJson

        //function management
        case Action.ADDFUNCTION => response = FunctionService.addFunction(input)
        case Action.GETFUNCTION => response = {
          if (param.length == 0)
            FunctionService.getFunction()
          else
            FunctionService.getFunction(param)

        }
        case Action.REMOVEFUNCTION => response = {
          if (param.length == 0)
            FunctionService.removeFunction()
          else
            FunctionService.removeFunction(param)
        }

        case Action.UPDATEFUNCTION => response = FunctionService.updateFunction(input)
        case Action.LOADFUNCTIONSFROMAFILE => response = FunctionService.loadFunctionsFromAFile(input)
        case Action.DUMPALLFUNCTIONSASJSON => response = FunctionService.dumpAllFunctionsAsJson

        //config
        case Action.UPLOADCLUSTERCONFIG => response = ConfigService.uploadClusterConfig(input)
        case Action.UPLOADCOMPILECONFIG => response = ConfigService.uploadCompileConfig(input)
        case Action.DUMPALLCFGOBJECTS => response = ConfigService.dumpAllCfgObjects
        case Action.REMOVEENGINECONFIG => response = ConfigService.removeEngineConfig

        //concept
        case Action.ADDCONCEPT => response = ConceptService.addConcept(input)
        case Action.REMOVECONCEPT => response = {
          if (param.length == 0)
            ConceptService.removeConcept("", userId)
          else
            ConceptService.removeConcept(param, userId)

        }

        case Action.LOADCONCEPTSFROMAFILE => response = ConceptService.loadConceptsFromAFile
        case Action.DUMPALLCONCEPTSASJSON => response = ConceptService.dumpAllConceptsAsJson

        //jar
        case Action.UPLOADJAR => response = JarService.uploadJar(input)

        //dumps
        case Action.DUMPMETADATA => response = DumpService.dumpMetadata
        case Action.DUMPALLNODES => response = DumpService.dumpAllNodes
        case Action.DUMPALLCLUSTERS => response = DumpService.dumpAllClusters
        case Action.DUMPALLCLUSTERCFGS => response = DumpService.dumpAllClusterCfgs
        case Action.DUMPALLADAPTERS => response = DumpService.dumpAllAdapters
        case _ => {
          println(s"Unexpected action! action=$action")
          throw new RuntimeException(s"Unexpected action! action=$action")
        }
      }
    }
    catch {

      case e: Exception => {
        logger.warn("", e)
        /** tentative answer of unidentified command type failure. */
        response = s"Unexpected action! action = $action"
        /** one more try ... going the alternate route.
          *
          * ''Do we still need this ?'' Let's keep it for now.
          */
        val altResponse: String = AltRoute(originalArgs)
        if (altResponse != null) {
            //response = altResponse  ... typically a parse error that is only meaningful for AltRoute processing
            println(response)
            sys.exit(1)
         } else {
          /* if the AltRoute doesn't produce a valid result, we will complain with the original failure */
          printf(response)
          sys.exit(1)
        }
      }

    }
    response
  }

  /** AltRoute is invoked only if the 'Action.withName(action.trim)' method fails to discern the appropriate
    * MetadataAPI method to invoke.  The command argument array is reconsidered with the AlternateCmdParser
    * If it produces valid command arguments (a command name and Map[String,String] of arg name/values) **and**
    * it is a command that we currently support with this mechanism (JPMML related commands are currently supported),
    * the service module is invoked.
    *
    * @param origArgs an Array[String] containing all of the arguments (sans debug if present) originally submitted
    * @return the response from successfully recognized commands (good or bad) or null if this mechanism couldn't
    *         make a determination of which command to invoke.  In that case a null is returned and the original
    *         complaint is returned to the caller.
    *
    */
  def AltRoute(origArgs : Array[String]) : String = {

       /** trim off the config argument and if debugging the "debug" argument as well */
       val argsSansConfig : Array[String] = if (origArgs != null && origArgs.size > 0 && origArgs(0).toLowerCase == "debug") {
           origArgs.tail.tail
       } else {
           origArgs.tail
       }

       /** Put the command back together */
       val buffer : StringBuilder = new StringBuilder
       argsSansConfig.addString(buffer," ")
       val originalCmd : String = buffer.toString

      var response: String = ""
      try {
           /** Feed the command string to the alternate parser. If successful, the cmdName will be valid string. */
           val (optCmdName, argMap): (Option[String], Map[String, String]) = AlternateCmdParser.parse(originalCmd)
           val cmdName: String = optCmdName.orNull
           response = if (cmdName != null) {
               /** See if it is one of the **supported** alternate commands */
               val cmd: String = cmdName.toLowerCase

               val resp: String = cmd match {
                   case "addmodel" => {
                       val modelTypeToBeAdded: String = if (argMap.contains("type")) argMap("type").toLowerCase else null
                       if (modelTypeToBeAdded != null && modelTypeToBeAdded == "pmml") {

                           val modelName: Option[String] = if (argMap.contains("name")) Some(argMap("name")) else None
                           val modelVer: String = if (argMap.contains("modelversion")) argMap("modelversion") else null
                           val msgName: Option[String] = if (argMap.contains("message")) Some(argMap("message")) else None
                           /** it is permissable to not supply the messageversion... the latest version is assumed in that case */
                           val msgVer: String = if (argMap.contains("messageversion")) argMap("messageversion") else MdMgr.LatestVersion
                           val pmmlSrc: Option[String] = if (argMap.contains("pmml")) Some(argMap("pmml")) else None
                           val pmmlPath: String = pmmlSrc.orNull

                           var validatedModelVersion: String = null
                           var validatedMsgVersion: String = null
                           try {
                               validatedModelVersion = if (modelVer != null) MdMgr.FormatVersion(modelVer) else null
                               validatedMsgVersion = if (msgVer != null) MdMgr.FormatVersion(msgVer) else null
                           } catch {
                               case e: Exception => throw (new RuntimeException(s"The version parameter is invalid... either not numeric or out of range...modelversion=$modelVer, messageversion=$msgVer", e))
                           }
                           val optModelVer: Option[String] = Option(validatedModelVersion)
                           val optMsgVer: Option[String] = Option(validatedMsgVersion)

                           ModelService.addModelPmml(ModelType.PMML
                               , pmmlPath
                               , Some("Kamanja")
                               , modelName
                               , optModelVer
                               , msgName
                               , optMsgVer)

                       } else {
                           null
                       }
                   }
                   case "updatemodel" => {
                       // updateModel type(jpmml) name(com.anotherCo.jpmml.DahliaRandomForest) newVersion(000000.000001.000002) oldVersion(000000.000001.000001) pmml(/anotherpath/prettierDahliaRandomForest.xml)  <<< NOT AVAILABLE (YET) update an explicit model version... doesn't have to be latest
                       // updateModel type(jpmml) name(com.anotherCo.jpmml.DahliaRandomForest) newVersion(000000.000001.000002) pmml(/anotherpath/prettierDahliaRandomForest.xml)  <<< default to the updating the latest model version there.

                       val modelTypeToBeUpdated: String = if (argMap.contains("type")) argMap("type").toLowerCase else null
                       if (modelTypeToBeUpdated != null && modelTypeToBeUpdated == "pmml") {

                           val optModelName: Option[String] = if (argMap.contains("name")) Some(argMap("name")) else None
                           val newVer: String = if (argMap.contains("newversion")) argMap("newversion") else null
                           /** it is permissable to not supply the old version... we just ask for update of the latest version in that case */
                           val oldVer: String = if (argMap.contains("oldversion")) argMap("oldversion") else MdMgr.LatestVersion
                           if (oldVer != MdMgr.LatestVersion) {
                               val warningMsg: String = "Specific version replacement is not currently supported.  Only the latest version of a model may be updated........"
                               logger.warn(warningMsg)
                               warningMsg
                           } else {

                               val pmmlSrc: Option[String] = if (argMap.contains("pmml")) Some(argMap("pmml")) else None
                               val pmmlPath: String = pmmlSrc.orNull

                               /** NOTE: Despite the presence of the oldVer, it is currently not supported.  The metadata
                                 * manager is not supporting specific version replacement with update.  Only the "latest"
                                 * version of the model can be changed.  That said, we leave this in place for now until
                                 * it has been determined if the verion will become an active part of the metadata
                                 * key that manages models (and messages, containers, and the rest)
                                 */

                               /** Use FormatVersion to normalize the string representation ... padding with appropriate 0's etc. */
                               var validatedOldVersion: String = null
                               var validatedNewVersion: String = null
                               try {
                                   validatedOldVersion = if (oldVer != null && oldVer != MdMgr.LatestVersion) MdMgr.FormatVersion(oldVer)
                                   else {
                                       if (oldVer == MdMgr.LatestVersion) {
                                           MdMgr.LatestVersion
                                       } else {
                                           null
                                       }
                                   }
                                   validatedNewVersion = if (newVer != null) MdMgr.FormatVersion(newVer) else null
                               } catch {
                                   case e: Exception => throw (new RuntimeException(s"One or more version parameters are invalid... oldVer=$oldVer, newVer=$newVer", e))
                               }
                               val optOldVer: Option[String] = Option(validatedOldVersion)

                               /** modelnamespace.modelname expected for modelName value */
                               val modelName: String = optModelName.orNull
                               ModelService.updateModelPmml(pmmlPath
                                   , Some("Kamanja")
                                   , modelName
                                   , validatedNewVersion)
                               //, optOldVer)
                           }
                       } else {
                           null
                       }
                   }

               }
               resp
           } else {
               null
           }
       } catch {
           case e: Exception => logger.debug(s"Exception seen ... e=${e.toString}", e)
           response=""
       }

       response
   }
}
