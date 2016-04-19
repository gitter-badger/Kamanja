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

package com.ligadata.MetadataAPI.Utility

import java.io.{FileNotFoundException, File}

import scala.io.Source

import org.apache.logging.log4j._

import com.ligadata.Exceptions.StackTrace
import com.ligadata.MetadataAPI.{MetadataAPIImpl,ApiResult,ErrorCodeConstants}
import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.MetadataAPI.MetadataAPI.ModelType.ModelType
import com.ligadata.MetadataAPI.MetadataAPIImpl
import scala.io._

/**
 * Created by dhaval on 8/7/15.
 */

object ModelService {
    private val userid: Option[String] = Some("kamanja")
    val loggerName = this.getClass.getName
    lazy val logger = LogManager.getLogger(loggerName)

    /************************************************************************************************
      * Add Models
      **********************************************************************************************/

    /**
      * Add the supplied model to the metadata.
      *
      * @param input the path of the model to be ingested
      * @param dep model configuration indicator
      * @param userid the optional userId. If security and auditing in place this parameter is required.
      * @return the result of the operation
      */
    def addModelScala(input: String
                      , dep: String = ""
                      , userid: Option[String] = Some("kamanja")
		                  , optMsgProduced: Option[String] = None
                      , tid: Option[String] = None): String = {
        var modelDefs= Array[String]()
        var modelConfig=""
        var modelDef=""
        var response: String = ""
        var modelFileDir: String = ""
        if (input == "") {
            //get the messages location from the config file. If error get the location from github
            modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
            if (modelFileDir == null) {
                response = "MODEL_FILES_DIR property missing in the metadata API configuration"
            } else {
                //verify the directory where messages can be present
                IsValidDir(modelFileDir) match {
                    case true => {
                        //get all files with json extension
                        val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".scala"))
                        models.length match {
                            case 0 => {
                                val errorMsg = "Models not found at " + modelFileDir
                                response = errorMsg
                            }
                            case option => {
                                modelDefs=getUserInputFromMainMenu(models)
                            }
                        }
                    }
                    case false => {
                        //println("Message directory is invalid.")
                        response = "Model directory is invalid."
                    }
                }
            }
        } else {
            var model = new File(input.toString)
            if(model.exists()){
                modelDef = Source.fromFile(model).mkString
                modelDefs=modelDefs:+modelDef
            } else {
                response="File does not exist"
            }
        }
        if (modelDefs.nonEmpty) {
            for (modelDef <- modelDefs){
                println("Adding the next model in the queue.")
                if (dep.length > 0) {
                    response+= MetadataAPIImpl.AddModel(ModelType.SCALA, modelDef, userid, tid, Some(userid.get+"."+dep),None,None,None,optMsgProduced)
                } else {
                    //before adding a model, add its config file.
                    var configKeys = MetadataAPIImpl.getModelConfigNames
                    if(configKeys.isEmpty){
                        response="No model configuration loaded in the metadata!"
                    }else{
                        var srNo = 0
                        println("\nPick a Model Definition file(s) from below choices\n")
                        for (configkey <- configKeys) {
                            srNo += 1
                            println("[" + srNo + "]" + configkey)
                        }
                        print("\nEnter your choice: \n")
                        var userOption = readInt()

                        userOption match {
                            case x if ((1 to srNo).contains(userOption)) => {
                                //find the file location corresponding to the config file
                                modelConfig=configKeys(userOption.toInt - 1)
                                println("Model config selected is "+modelConfig)
                            }
                            case _ => {
                                val errorMsg = "Incorrect input " + userOption + ". Please enter the correct option."
                                println(errorMsg)
                                errorMsg
                            }
                        }
                        response+= MetadataAPIImpl.AddModel(ModelType.SCALA, modelDef, userid,tid, Some(modelConfig), None, None, None, optMsgProduced)
                    }
                }
            }
        }

        response
    }

    /**
      * Add the supplied model to the metadata.
      *
      * @param input The input path of the model to be ingested
      * @param dep model configuration
      * @param userid the optional userId. If security and auditing in place this parameter is required.
      * @return the result of the operation
      */
    def addModelJava(input: String, dep: String = ""
                     , userid: Option[String] = Some("kamanja")
		                 , optMsgProduced: Option[String] = None
                     , tid: Option[String] = None): String = {
        var modelDefs= Array[String]()
        var modelConfig=""
        var modelDef=""
        var response: String = ""
        var modelFileDir: String = ""

        if (input == "") {
            //get the messages location from the config file. If error get the location from github
            modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
            if (modelFileDir == null) {
                response = "MODEL_FILES_DIR property missing in the metadata API configuration"
            } else {
                //verify the directory where messages can be present
                IsValidDir(modelFileDir) match {
                    case true => {
                        //get all files with json extension
                        val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".java"))
                        models.length match {
                            case 0 => {
                                val errorMsg = "Models not found at " + modelFileDir
                                response = errorMsg
                            }
                            case option => {

                                modelDefs=getUserInputFromMainMenu(models)
                            }
                        }
                    }
                    case false => {
                        //println("Message directory is invalid.")
                        response = "Model directory is invalid."
                    }
                }
            }
        } else {
            var model = new File(input.toString)
            if(model.exists()){
                modelDef = Source.fromFile(model).mkString
                modelDefs=modelDefs:+modelDef
            }else{
                response="File does not exist"
            }
        }
        if(modelDefs.nonEmpty) {
            for (modelDef <- modelDefs){
                println("Adding the next model in the queue.")
                if (dep.length > 0) {
                    response+= MetadataAPIImpl.AddModel(ModelType.JAVA, modelDef, userid, tid, Some(userid.get+"."+dep), None,None,None,optMsgProduced)
                } else {
                    var configKeys = MetadataAPIImpl.getModelConfigNames
                    println("--> got these many back "+configKeys.size)
                    if(configKeys.isEmpty){
                        response="No model configuration loaded in the metadata!"
                    }else{
                        var srNo = 0
                        println("\nPick a Model Definition file(s) from below choices\n")
                        for (configkey <- configKeys) {
                            srNo += 1
                            println("[" + srNo + "]" + configkey)
                        }
                        print("\nEnter your choice: \n")
                        var userOption = readInt()

                        userOption match {
                            case x if ((1 to srNo).contains(userOption)) => {
                                //find the file location corresponding to the config file
                                modelConfig=configKeys(userOption.toInt - 1)
                                println("Model config selected is "+modelConfig)
                            }
                            case _ => {
                                val errorMsg = "Incorrect input " + userOption + ". Please enter the correct option."
                                println(errorMsg)
                                errorMsg
                            }
                        }
                        response+= MetadataAPIImpl.AddModel(ModelType.JAVA, modelDef, userid, tid,Some(modelConfig), None,None,None,optMsgProduced)
                    }
                }
            }
        }
        response
    }

    /**
     * addModelPmml ingests a PMML model. Pmml model ingestion requires the pmml source file, the model name to be associated
     * with this model, the model's version, and the message consumed by the supplied model.  If the userId is specified and
     * a SecurityAdapter is installed in the MetadataAPI (recommended for production uses), the command will only be
     * attempted if the SecurityAdapter instance deems the user worthy. Similarly if the AuditAdapter is supplied,
     * the userid will be logged there (recommended for production use).
     *
     * NOTE: Pmml models are distinct from the Kamanja Pmml model. At runtime, they use a PMML evaluator to interpret
     * the runtime representation of the PMML model. Kamanja models are compiled to Scala and then to Jars and executed
     * like the custom byte code models based upon Java or Scala.
     *
     * @param modelType the type of model this is (PMML in this case)
     * @param input the pmml source file too ingest
     * @param optUserid the user id attempting to execute this command
     * @param optModelName the full namespace qualified model name
     * @param optVersion the version to associate with this model (in form 999999.999999.999999)
     * @param optMsgConsumed the full namespace qualified message name this model will consume
     * @param optMsgVersion the version of the message ... by default it is Some(-1) to get the most recent message of this name.
     * @return result string from engine describing success or failure
     */
    def addModelPmml(modelType: ModelType.ModelType
                    , input: String
                    , optUserid: Option[String] = Some("kamanja")
                    , optModelName: Option[String] = None
                    , optVersion: Option[String] = None
                    , optMsgConsumed: Option[String] = None
                    , optMsgVersion: Option[String] = Some("-1")
                    , tid: Option[String] = None): String = {
        val response : String = if (input == "") {
            val reply : String = "PMML models are only ingested with command line arguments.. default directory selection is deprecated"
            logger.error(reply)
            null /// FIXME : we will return null for now and complain with first failure
        } else {
            val model = new File(input.toString)
            val resp : String = if(model.exists()){
                val modelDef= Source.fromFile(model).mkString
                MetadataAPIImpl.AddModel(ModelType.PMML, modelDef, optUserid, tid,  optModelName, optVersion, optMsgConsumed,None,optMsgVersion)
            }else{
                val userId : String = optUserid.getOrElse("no user id supplied")
                val modelName : String = optModelName.getOrElse("no model name supplied")
                val version : String = optVersion.getOrElse("no version supplied")
                val msgConsumed : String = optMsgConsumed.getOrElse("no message supplied")

                val reply : String = s"PMML model definition ingestion has failed for model $modelName, version = $version, consumes msg = $msgConsumed user=$userId"
                logger.error(reply)
                null /// FIXME : we will return null for now and complain with first failure/
            }
            resp
        }
        response

    }

    /**
     * Add a new Kamanja Pmml model to the metadata.
     *
     * @param input the path of the pmml to be added as a new model
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def addModelKPmml(input: String
                     , userid: Option[String] = Some("kamanja")
		                 , optMsgProduced: Option[String] = None
                     , tid: Option[String] = None   ): String = {
        var modelDef=""
        var modelConfig=""
        var response: String = ""
        var modelFileDir: String = ""
        if (input == "") {
            //get the messages location from the config file. If error get the location from github
            modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
            if (modelFileDir == null) {
                response = "MODEL_FILES_DIR property missing in the metadata API configuration"
            } else {
                //verify the directory where messages can be present
                IsValidDir(modelFileDir) match {
                    case true => {
                        //get all files with json extension
                        val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".xml"))
                        models.length match {
                            case 0 => {
                                val errorMsg = "Models not found at " + modelFileDir
                                println(errorMsg)
                                response = errorMsg
                            }
                            case option => {
                                var  modelDefs=getUserInputFromMainMenu(models)
                                for (modelDef <- modelDefs)
                                    response += MetadataAPIImpl.AddModel(ModelType.KPMML, modelDef.toString, userid, tid, None,None,None,None,optMsgProduced)
                            }
                        }
                    }
                    case false => {
                        response = "Model directory is invalid."
                    }
                }
            }
        } else {
            //   println("Path provided. Added msg")
            //process message
            var model = new File(input.toString)
            if(model.exists()){
                modelDef= Source.fromFile(model).mkString
                response = MetadataAPIImpl.AddModel(ModelType.KPMML, modelDef.toString, userid, tid, None,None,None,None,optMsgProduced)
            }else{
                response="Model definition file does not exist"
            }
        }
        response
    }

    /**
      * Add a new JTM (Json Transformation Model) model to the metadata.
      *
      * @param input the path of the jtm file to be added as a new model
      * @param userid the optional userId. If security and auditing in place this parameter is required.
      * @return the result of the operation
      */
    def addModelJTM(input: String
                      , userid: Option[String] = Some("kamanja")
                      , tid: Option[String] = None
                      , optModelName: Option[String] = None): String = {
        var modelDef=""
        var response: String = ""
        var modelFileDir: String = ""
        if (input == "") {
            //get the messages location from the config file. If error get the location from github
            modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
            if (modelFileDir == null) {
                response = "MODEL_FILES_DIR property missing in the metadata API configuration"
            } else {
                //verify the directory where messages can be present
                IsValidDir(modelFileDir) match {
                    case true => {
                        //get all files with json extension
                        val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(f => f.getName.endsWith(".jtm") || f.getName.endsWith(".json"))
                        models.length match {
                            case 0 => {
                                val errorMsg = "Models not found at " + modelFileDir
                                println(errorMsg)
                                response = errorMsg
                            }
                            case option => {
                                var  modelDefs=getUserInputFromMainMenu(models)
                                for (modelDef <- modelDefs)
                                    response += MetadataAPIImpl.AddModel(ModelType.JTM, modelDef.toString, userid, tid, optModelName, None, None, None,None)
                            }
                        }
                    }
                    case false => {
                        response = "Model directory is invalid."
                    }
                }
            }
        } else {
            //   println("Path provided. Added msg")
            //process message
            var model = new File(input.toString)
            if(model.exists()){
                modelDef= Source.fromFile(model).mkString
                response = MetadataAPIImpl.AddModel(ModelType.JTM, modelDef.toString, userid, tid, optModelName, None, None, None, None)
            }else{
                response="Model definition file does not exist"
            }
        }
        response
    }

    /**
     * Update a Kamanja Pmml model in the metadata with new pmml
     *
     * @param input the path of the Kamanja pmml model to be used for the update
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
     
    def updateModelKPmml(input: String
                      , userid: Option[String] = Some("kamanja")
                      , tid: Option[String] = None): String = {
      var modelDef = ""
      var response: String = ""
      var modelFileDir: String = ""
      if (input == "") {
        //get the messages location from the config file. If error get the location from github
        modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
        if (modelFileDir == null) {
          response = "MODEL_FILES_DIR property missing in the metadata API configuration"
        } else {
          //verify the directory where messages can be present
          IsValidDir(modelFileDir) match {
            case true => {
              //get all files with json extension
              val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".xml"))
              models.length match {
                case 0 => {
                  val errorMsg = "Models not found at " + modelFileDir
                  println(errorMsg)
                  response = errorMsg
                }
                case option => {
                  var modelDefs = getUserInputFromMainMenu(models)
                  for (modelDef <- modelDefs)
                    response = MetadataAPIImpl.UpdateModel(ModelType.KPMML, modelDef.toString, userid, tid)
                }
              }

            }
            case false => {
              //println("Message directory is invalid.")
              response = "Model directory is invalid."
            }
          }
        }
      } else {
        //   println("Path provided. Added msg")
        //process message
        var model = new File(input.toString)
        if (model.exists()) {
          modelDef = Source.fromFile(model).mkString
          response = MetadataAPIImpl.UpdateModel(ModelType.KPMML, modelDef, userid, tid)
        } else {
          response = "File does not exist"
        }
        //println("Response: " + response)
      }

        response
    }

    /**
      * Update a JTM (Json Transformation Model) model in the metadata with new transformation specification file
      *
      * @param input the path of the JTM model spec file to be used for the update
      * @param userid the optional userId. If security and auditing in place this parameter is required.
      * @return the result of the operation
      */

    def updateModelJTM(input: String
                         , userid: Option[String] = Some("kamanja")
                         , tid: Option[String] = None
                         , optModelName: Option[String] = None): String = {
        var modelDef = ""
        var response: String = ""
        var modelFileDir: String = ""
        if (input == "") {
            //get the messages location from the config file. If error get the location from github
            modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
            if (modelFileDir == null) {
                response = "MODEL_FILES_DIR property missing in the metadata API configuration"
            } else {
                //verify the directory where messages can be present
                IsValidDir(modelFileDir) match {
                    case true => {
                        //get all files with json extension
                        val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(f => f.getName.endsWith(".jtm") || f.getName.endsWith(".json"))
                        models.length match {
                            case 0 => {
                                val errorMsg = "Models not found at " + modelFileDir
                                println(errorMsg)
                                response = errorMsg
                            }
                            case option => {
                                var modelDefs = getUserInputFromMainMenu(models)
                                for (modelDef <- modelDefs)
                                    response = MetadataAPIImpl.UpdateModel(ModelType.JTM, modelDef.toString, userid, tid, optModelName)
                            }
                        }

                    }
                    case false => {
                        response = "Model directory is invalid."
                    }
                }
            }
        } else {
            //   println("Path provided. Added msg")
            //process message
            var model = new File(input.toString)
            if (model.exists()) {
                modelDef = Source.fromFile(model).mkString
                response = MetadataAPIImpl.UpdateModel(ModelType.JTM, modelDef, userid, tid, optModelName)
            } else {
                response = "File does not exist"
            }
        }

    response
  }

    /**
      * Update a Pmml model with the pmml text model found at ''pmmlPath''.  The model namespace, name and version
      * are required.  The userid should have a valid value when authentication and auditing has been enabled on
      * the cluster.
      *
      * @param pmmlPath
      * @param userid
      * @param modelNamespaceName
      * @param newVersion
      * @return result string
      */
  def updateModelPmml(pmmlPath : String
                      ,userid : Option[String]
                      ,modelNamespaceName : String
                      ,newVersion : String
                      ,tid: Option[String] = None ) : String = {

      val response : String = try {
          val jpmmlPath : File = new File(pmmlPath.toString)
              val pmmlText : String = Source.fromFile(jpmmlPath).mkString

              MetadataAPIImpl.UpdateModel(ModelType.PMML
                              , pmmlText
                              , userid
                              , tid
                              , Some(modelNamespaceName)
                              , Some(newVersion), None, None)
      } catch {
        case fnf : FileNotFoundException => {
            val msg : String = s"updateModelPmml... supplied file path not found ... path = $pmmlPath"
            logger.error(msg, fnf)
            msg
        }
        case e : Exception => {
            val msg : String = if (pmmlPath == null) "updateModelPmml pmml path was not supplied" else s"updateModelPmml... exception e = ${e.toString}"
            logger.error(s"$msg...", e)
            msg
        }
      }
      response
  }

    /**
     * Update a model in the metadata with the supplied Java model
     *
     * @param input the path of the model to be used in the model update
     * @param dep the model compile config indication
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def updateModeljava(input: String, dep: String = ""
                      , userid: Option[String] = Some("kamanja")
                      ,tid: Option[String] = None): String = {
        var modelDef=""
        var modelConfig=""
        var response: String = ""
        var modelFileDir: String = ""
        var modelDefs= Array[String]()
        if (input == "") {
          //get the messages location from the config file. If error get the location from github
          modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
          if (modelFileDir == null) {
            response = "MODEL_FILES_DIR property missing in the metadata API configuration"
          } else {
            //verify the directory where messages can be present
            IsValidDir(modelFileDir) match {
              case true => {
                //get all files with json extension
                val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".java"))
                models.length match {
                  case 0 => {
                    val errorMsg = "Models not found at " + modelFileDir
                    println(errorMsg)
                    response = errorMsg
                  }
                  case option => {
                    modelDefs=getUserInputFromMainMenu(models)
                  }
                }
              }
              case false => {
                //println("Message directory is invalid.")
                response = "Model directory is invalid."
              }
            }
          }
        } else {
          //   println("Path provided. Added msg")
          //process message
          var model = new File(input.toString)

          if (model.exists()) {
            modelDef = Source.fromFile(model).mkString
            modelDefs=modelDefs:+modelDef
          } else {
            response = "File does not exist"
          }
        }
        if(modelDefs.nonEmpty) {
          for (modelDef <- modelDefs){
            println("Adding the next model in the queue.")
            if (dep.length > 0) {
              response+= MetadataAPIImpl.UpdateModel( ModelType.JAVA, modelDef, userid, Some(userid.get+"."+dep), tid)
            } else {
              //before adding a model, add its config file.
              var configKeys = MetadataAPIImpl.getModelConfigNames
              if(configKeys.isEmpty){
                response="No model configuration loaded in the metadata!"
              }else{
                var srNo = 0
                println("\nPick a Model Definition file(s) from below choices\n")
                for (configkey <- configKeys) {
                  srNo += 1
                  println("[" + srNo + "]" + configkey)

                }
                print("\nEnter your choice: \n")
                var userOption = readInt()

                userOption match {
                  case x if ((1 to srNo).contains(userOption)) => {
                    //find the file location corresponding to the config file
                    modelConfig=configKeys(userOption.toInt - 1)
                    println("Model config selected is "+modelConfig)
                  }
                  case _ => {
                    val errorMsg = "Incorrect input " + userOption + ". Please enter the correct option."
                    println(errorMsg)
                    errorMsg
                  }
                }
                response+= MetadataAPIImpl.UpdateModel(ModelType.JAVA, modelDef, userid, tid, Some(modelConfig))
              }
            }
          }
        }

        response

  }

    /**
     * Update a model in the metadata with the supplied Scala model
     *
     * @param input the path of the model to be updated
     * @param dep the compile config indication
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def updateModelscala(input: String, dep: String = ""
                       , userid: Option[String] = Some("kamanja")
                       , tid: Option[String] = None): String = {
        var modelDef=""
        var modelConfig=""
        var response: String = ""
        var modelFileDir: String = ""
        var modelDefs= Array[String]()
        if (input == "") {
          //get the messages location from the config file. If error get the location from github
          modelFileDir = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("MODEL_FILES_DIR")
          if (modelFileDir == null) {
            response = "MODEL_FILES_DIR property missing in the metadata API configuration"
          } else {
            //verify the directory where messages can be present
            IsValidDir(modelFileDir) match {
              case true => {
                //get all files with json extension
                val models: Array[File] = new java.io.File(modelFileDir).listFiles.filter(_.getName.endsWith(".scala"))
                models.length match {
                  case 0 => {
                    val errorMsg = "Models not found at " + modelFileDir
                    println(errorMsg)
                    response = errorMsg
                  }
                  case option => {
                    modelDefs=getUserInputFromMainMenu(models)
                  }
                }
              }
              case false => {
                //println("Message directory is invalid.")
                response = "Model directory is invalid."
              }
            }
          }
        } else {
          //   println("Path provided. Added msg")
          //process message
          var model = new File(input.toString)
          if (model.exists()) {
            modelDef = Source.fromFile(model).mkString
            modelDefs=modelDefs:+modelDef
          } else {
            response = "File does not exist"
          }
        }
          if(modelDefs.nonEmpty) {
            for (modelDef <- modelDefs){
              println("Adding the next model in the queue.")
              if (dep.length > 0) {
                response+= MetadataAPIImpl.UpdateModel(ModelType.SCALA, modelDef, userid, tid, Some(userid.get+"."+dep))
              } else {
                //before adding a model, add its config file.
                var configKeys = MetadataAPIImpl.getModelConfigNames
                if(configKeys.isEmpty){
                  response="No model configuration loaded in the metadata!"
                }else{
                  var srNo = 0
                  println("\nPick a Model Definition file(s) from below choices\n")
                  for (configkey <- configKeys) {
                    srNo += 1
                    println("[" + srNo + "]" + configkey)
                  }
                  print("\nEnter your choice: \n")
                  var userOption = readInt()

                  userOption match {
                    case x if ((1 to srNo).contains(userOption)) => {
                      //find the file location corresponding to the config file
                      modelConfig=configKeys(userOption.toInt - 1)
                      println("Model config selected is "+modelConfig)
                    }
                    case _ => {
                      val errorMsg = "Incorrect input " + userOption + ". Please enter the correct option."
                      println(errorMsg)
                      errorMsg
                    }
                  }
                  response+= MetadataAPIImpl.UpdateModel(ModelType.SCALA, modelDef, userid, tid, Some(modelConfig))
                }
              }
            }
          }
        response
  }

    /**
     * Get the supplied model key from the metadata.
     *
     * @param param the namespace.name.version of the model definition to be fetched
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation - a JSON string representation of the ModelDef
     */
    def getModel(param: String = ""
               , userid: Option[String] = Some("kamanja")
               ): String ={
        var response=""
        try {
          if (param.length > 0) {
            val(ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(param)
            try {
              return MetadataAPIImpl.GetModelDefFromCache(ns, name,"JSON" ,ver, userid)
            } catch {
              case e: Exception => logger.error("", e)
            }
          }
          val modelKeys = MetadataAPIImpl.GetAllModelsFromCache(true, None)
          if (modelKeys.length == 0) {
            val errorMsg="Sorry, No models available, in the Metadata, to display!"
            response=errorMsg
          }
          else{
            println("\nPick the model to be displayed from the following list: ")
            var srno = 0
            for(modelKey <- modelKeys){
              srno+=1
              println("["+srno+"] "+modelKey)
            }
            println("Enter your choice: ")
            val choice: Int = readInt()
            if (choice < 1 || choice > modelKeys.length) {
              val errormsg="Invalid choice " + choice + ". Start with the main menu."
              response=errormsg
            }
            val modelKey = modelKeys(choice - 1)
            val(ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelKey)
            val apiResult = MetadataAPIImpl.GetModelDefFromCache(ns, name,"JSON",ver, userid)
            response=apiResult
          }

        } catch {
          case e: Exception => {
            logger.info("", e)
            response=e.getStackTrace.toString
          }
        }
        response
  }

    /**
     * 
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return
     */
    def getAllModels(userid: Option[String] = Some("kamanja")) : String ={
        var response=""
        val modelKeys = MetadataAPIImpl.GetAllModelsFromCache(true, userid)
        if (modelKeys.length == 0) {
          response="Sorry, No models available in the Metadata"
        }else{
          var srNo = 0
          for(modelKey <- modelKeys){
            srNo += 1
            response+="[" + srNo + "]" + modelKey+"\n"
          }
        }
        response
    }

    /**
     * Remove the model with the supplied namespace.name.ver from the metadata.
     *
     * @param modelId the namespace.name.version of the model to remove. If an empty string present list to choose from
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def removeModel(modelId: String = ""
                  , userid: Option[String] = Some("kamanja")
                  ): String ={
        val response : String = try {
          //  logger.setLevel(Level.TRACE); //check again
          if (modelId.length > 0) {
            val (ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelId)
            val result : String = try {
              MetadataAPIImpl.RemoveModel(s"$ns.$name", ver, userid)
            } catch {
              case e: Exception => {
                  val stackTrace = StackTrace.ThrowableTraceString(e)
                  logger.info(stackTrace)
                  stackTrace
              }
            }
            result
          } else {

              val modelKeys = MetadataAPIImpl.GetAllModelsFromCache(true, None)

              if (modelKeys.length == 0) {
                "Sorry, No models available, in the Metadata, to delete!"
              } else {
                  println("\nPick the model to be deleted from the following list: ")
                  var srno = 0
                  for (modelKey <- modelKeys) {
                      srno += 1
                      println("[" + srno + "] " + modelKey)
                  }
                  println("Enter your choice: ")
                  val choice: Int = readInt()

                  if (choice < 1 || choice > modelKeys.length) {
                      "Invalid choice " + choice + ". Start with the main menu."
                  } else {
                      val modelKey = modelKeys(choice - 1)
                      val (ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelKey)
                      MetadataAPIImpl.RemoveModel(s"$ns.$name", ver, userid)
                  }
              }
          }
        } catch {
            case e: Exception => {
                val stackTrace = StackTrace.ThrowableTraceString(e)
                logger.info(stackTrace)
                stackTrace
            }
        }
        response
  }

    /**
     * Activate the model supplied
     *
     * @param modelId the namespace.name.version of the model to activate. If an empty string present list to choose from
     * @param userid the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def activateModel(modelId: String = ""
                    , userid: Option[String] = Some("kamanja")
                    ): String ={
        var response=""
        try {
          if (modelId.length > 0) {
            val(ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelId)
            try {
              return MetadataAPIImpl.ActivateModel(ns, name, ver.toInt, userid)
            } catch {
              case e: Exception => logger.error("", e)
            }
          }
          val modelKeys = MetadataAPIImpl.GetAllModelsFromCache(false, None)
          if (modelKeys.length == 0) {
            val errorMsg="Sorry, No models available, in the Metadata, to activate!"
            response=errorMsg
          }
          else{
            println("\nPick the model to be activated from the following list: ")
            var srno = 0
            for(modelKey <- modelKeys){
              srno+=1
              println("["+srno+"] "+modelKey)
            }
            println("Enter your choice: ")
            val choice: Int = readInt()

            if (choice < 1 || choice > modelKeys.length) {
              val errormsg="Invalid choice " + choice + ". Start with the main menu."
              response=errormsg
            }
            val modelKey = modelKeys(choice - 1)
            val modelKeyTokens = modelKey.split("\\.")
            val (modelNameSpace, modelName, modelVersion) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelKey)
            val apiResult = MetadataAPIImpl.ActivateModel(modelNameSpace, modelName, modelVersion.toLong, userid).toString
            response=apiResult
          }

        } catch {
          case e: Exception => {
            logger.info("", e)
            response=e.getStackTrace.toString
          }
        }
        response
  }

    /**
     * Deactivate the supplied model if given.  If not given present a menu of the active models from which to choose.
     *
     * @param modelId the namespace.name.version of the model to deactivate. If an empty string present list to choose from
     * @param userid the optional userId. If security and auditing in place this parameter is required. the optional userId. If security and auditing in place this parameter is required.
     * @return the result of the operation
     */
    def deactivateModel(modelId: String = ""
                      , userid: Option[String] = Some("kamanja")
                      ):String={
        var response=""
        var progressReport: Int = 0
        try {
          if (modelId.length > 0) {
            val(ns, name, ver) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelId)
            try {
              return MetadataAPIImpl.DeactivateModel(ns, name, ver.toInt, userid)
            } catch {
              case e: Exception => logger.error("", e)
            }
          }
          progressReport = 1
          val modelKeys = MetadataAPIImpl.GetAllModelsFromCache(true, None)

          if (modelKeys.length == 0) {
            val errorMsg="Sorry, No models available, in the Metadata, to deactivate!"
            //println(errorMsg)
            response=errorMsg
          }
          else{
            println("\nPick the model to be de-activated from the following list: ")
            var srno = 0
            for(modelKey <- modelKeys){
              srno+=1
              println("["+srno+"] "+modelKey)
            }
            println("Enter your choice: ")
            val choice: Int = readInt()


            if (choice < 1 || choice > modelKeys.length) {
              val errormsg="Invalid choice " + choice + ". Start with the main menu."
              response=errormsg
            }
            val modelKey = modelKeys(choice - 1)
            val modelKeyTokens = modelKey.split("\\.")
            val (modelNameSpace, modelName, modelVersion) = com.ligadata.kamanja.metadata.Utils.parseNameToken(modelKey)
            val apiResult = MetadataAPIImpl.DeactivateModel(modelNameSpace, modelName, modelVersion.toLong, userid).toString
            response=apiResult
          }
        } catch {
          case e: Exception => {
            if (progressReport == 0) {
              logger.warn("", e)
              response = new ApiResult(ErrorCodeConstants.Failure, "DeactivateModel", null, "Error : Cannot parse ModelName, must be Namespace.Name.Version format").toString
            }
            else {
              response = new ApiResult(ErrorCodeConstants.Failure, "DeactivateModel", null, "Error : An Exception occured during processing").toString
              logger.error("Unkown exception occured during deactivate model processing ", e)
            }
          }
        }
        response
  }

    /**
     * Is the supplied directory path valid?
     *
     * @param dirName directory path
     * @return true if it is
     */
    def IsValidDir(dirName: String): Boolean = {
        val iFile = new File(dirName)
        if (!iFile.exists) {
          println("The File Path (" + dirName + ") is not found: ")
          false
        } else if (!iFile.isDirectory) {
          println("The File Path (" + dirName + ") is not a directory: ")
          false
        } else
          true
  }

    /**
     * 
     * @param models and array of directory file specs
     * @return a list of model defs
     */
    def getUserInputFromMainMenu(models: Array[File]): Array[String] = {
        var listOfModelDef: Array[String]=Array[String]()
        var srNo = 0
        println("\nPick a Model Definition file(s) from below choices\n")
        for (model <- models) {
          srNo += 1
          println("[" + srNo + "]" + model)
        }
        print("\nEnter your choice(If more than 1 choice, please use commas to seperate them): \n")
        var userOptions = readLine().split(",")
        println("User selected the option(s) " + userOptions.length)
        //check if user input valid. If not exit
        for (userOption <- userOptions) {
          userOption.toInt match {
            case x if ((1 to srNo).contains(userOption.toInt)) => {
              //find the file location corresponding to the message

              val model = models(userOption.toInt - 1)
              var modelDef = ""
              //process message
              if(model.exists()){
                 modelDef=Source.fromFile(model).mkString
              }else{
                println("File does not exist")
              }
              //val response: String = MetadataAPIImpl.AddModel(modelDef, userid).toString
              listOfModelDef = listOfModelDef:+modelDef
            }

          }
        }
        listOfModelDef
  }
}
