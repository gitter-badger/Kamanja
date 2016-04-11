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

import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.Serialize._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

/** A class that defines the result any of the API function uniformly
 * @constructor creates a new ApiResult with a statusCode,functionName,statusDescription,resultData
 * @param statusCode status of the API call, 0 => success, non-zero => failure.
 * @param description relevant in case of non-zero status code
 * @param resultData A string value representing string, either XML or JSON format
 */
class ApiResult(var statusCode:Int, var functionName: String, var resultData: String, var description: String){
/**
 * Override toString to return ApiResult as a String
 */
  override def toString: String = {
    
    val json = ("APIResults" -> ("Status Code" -> statusCode) ~
                                ("Function Name" -> functionName) ~ 
                                ("Result Data"  -> resultData) ~ 
                                ("Result Description" -> description))    

    pretty(render(json))
  }
}

object MetadataAPI {

  object ModelType extends Enumeration {
      type ModelType = Value
      val JAVA = Value("java")
      val SCALA = Value("scala")
      val KPMML = Value("kpmml")
      val PMML = Value("pmml")
      val JTM = Value("jtm")
      val BINARY = Value("binary")
      val UNKNOWN = Value("unknown")

      def fromString(typstr : String) : ModelType = {
          val typ : ModelType.Value = typstr.toLowerCase match {
              case "java" => JAVA
              case "scala" => SCALA
              case "pmml" => PMML
              case "kpmml" => KPMML
              case "jtm" => JTM
              case "binary" => BINARY
              case _ => UNKNOWN
          }
          typ
      }
  }

}

/**
 * A class that defines the result any of the API function - The ResultData reuturns a Complex Array of Values.
 * @param statusCode
 * @param functionName
 * @param resultData
 * @param description
 */
class ApiResultComplex (var statusCode:Int, var functionName: String, var resultData: String, var description: String){
  /**
   * Override toString to return ApiResult as a String
   */
  override def toString: String = {

    val resultArray = parse(resultData).values.asInstanceOf[List[Map[String,Any]]]

    var json = ("APIResults" -> ("Status Code" -> statusCode) ~
                                ("Function Name" -> functionName) ~
                                ("Result Data"  -> resultArray.map {nodeInfo => JsonSerializer.SerializeMapToJsonString(nodeInfo)}) ~
                                ("Result Description" -> description))

    pretty(render(json))
  }
}



trait MetadataAPI {
  import ModelType._
  /** MetadataAPI defines the CRUD (create, read, update, delete) operations on metadata objects supported
   * by this system. The metadata objects includes Types, Functions, Concepts, Derived Concepts,
   * MessageDefinitions, Model Definitions. All functions take String values as input in XML or JSON Format
   * returns JSON string of ApiResult object.
   */

  /** Add new types
    * @param typesText an input String of types in a format defined by the next parameter formatType
    * @param formatType format of typesText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    *
    * {{{
    * val sampleScalarTypeStr = """
    * {
    * "MetadataType" : "ScalarTypeDef",
    * "NameSpace" : "system",
    * "Name" : "my_char",
    * "TypeTypeName" : "tScalar",
    * "TypeNameSpace" : "System",
    * "TypeName" : "Char",
    * "PhysicalName" : "Char",
    * "Version" : 100,
    * "JarName" : "basetypes_2.10-0.1.0.jar",
    * "DependencyJars" : [ "metadata_2.10-1.0.jar" ],
    * "Implementation" : "com.ligadata.BaseTypes.CharImpl"
    * }
    * """
    * var apiResult = MetadataAPIImpl.AddType(sampleScalarTypeStr,"JSON")
    * var result = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + result._2)
    * }}}
    *
   */
  def AddType(typesText:String, formatType:String, userid: Option[String] = None): String

  /** Update existing types
    * @param typesText an input String of types in a format defined by the next parameter formatType
    * @param formatType format of typesText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    *
    * {{{
    * val sampleScalarTypeStr = """
    * {
    * "MetadataType" : "ScalarTypeDef",
    * "NameSpace" : "system",
    * "Name" : "my_char",
    * "TypeTypeName" : "tScalar",
    * "TypeNameSpace" : "System",
    * "TypeName" : "Char",
    * "PhysicalName" : "Char",
    * "Version" : 101,
    * "JarName" : "basetypes_2.10-0.1.0.jar",
    * "DependencyJars" : [ "metadata_2.10-1.0.jar" ],
    * "Implementation" : "com.ligadata.BaseTypes.CharImpl"
    * }
    * """
    * var apiResult = MetadataAPIImpl.UpdateType(sampleScalarTypeStr,"JSON")
    * var result = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + result._2)
    * }}}
    *
    */
  def UpdateType(typesText:String, formatType:String, userid: Option[String] = None): String
  /** Remove Type for given typeName and version
    * @param typeName name of the Type
    * @version version of the Type
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    *
    * {{{
    * val apiResult = MetadataAPIImpl.RemoveType(MdMgr.sysNS,"my_char",100)
    * val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + resultData)
    * }}}
    *
    */
  def RemoveType(typeNameSpace:String, typeName:String, version:Long, userid: Option[String] = None): String

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  /** Upload Jars into system. Dependency jars may need to upload first. Once we upload the jar,
    * if we retry to upload it will throw an exception.
    * @param jarPath fullPathName of the jar file
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
   */
  def UploadJar(jarPath:String, userid: Option[String] = None): String

  /** Add new functions
    * @param functionsText an input String of functions in a format defined by the next parameter formatType
    * @param formatType format of functionsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    * {{{
    *   val sampleFunctionStr = """
    *  {
    *  "NameSpace" : "pmml",
    *  "Name" : "my_min",
    *  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
    *  "ReturnTypeNameSpace" : "system",
    *  "ReturnTypeName" : "double",
    *  "Arguments" : [ {
    *  "ArgName" : "expr1",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "int"
    *  }, {
    *  "ArgName" : "expr2",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "double"
    *  } ],
    *  "Version" : 1,
    *  "JarName" : null,
    *  "DependantJars" : [ "basetypes_2.10-0.1.0.jar", "metadata_2.10-1.0.jar" ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.AddFunction(sampleFunctionStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *}}}
    */
  def AddFunctions(functionsText:String, formatType:String, userid: Option[String] = None): String

  /** Update existing functions
    * @param functionsText an input String of functions in a format defined by the next parameter formatType
    * @param formatType format of functionsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    * {{{
    *   val sampleFunctionStr = """
    *  {
    *  "NameSpace" : "pmml",
    *  "Name" : "my_min",
    *  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
    *  "ReturnTypeNameSpace" : "system",
    *  "ReturnTypeName" : "double",
    *  "Arguments" : [ {
    *  "ArgName" : "expr1",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "int"
    *  }, {
    *  "ArgName" : "expr2",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "double"
    *  } ],
    *  "Version" : 1,
    *  "JarName" : null,
    *  "DependantJars" : [ "basetypes_2.10-0.1.0.jar", "metadata_2.10-1.0.jar" ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.UpdateFunction(sampleFunctionStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    * }}}
    *
    */
  def UpdateFunctions(functionsText:String, formatType:String, userid: Option[String] = None): String

  /** Remove function for given FunctionName and Version
    * @param nameSpace the function's namespace
    * @param functionName name of the function
    * @version version of the function
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    * {{{
    * val apiResult = MetadataAPIImpl.RemoveFunction(MdMgr.sysNS,"my_min",100)
    * val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + resultData)
    *}}}
    *
    */
  def RemoveFunction(nameSpace:String, functionName:String, version:Long, userid: Option[String] = None): String

  /** Add new concepts 
    * @param conceptsText an input String of concepts in a format defined by the next parameter formatType
    * @param formatType format of conceptsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    *
    * {{{
    *   val sampleConceptStr = """
    *  {"Concepts" : [
    *  "NameSpace":"Ligadata",
    *  "Name":"ProviderId",
    *  "TypeNameSpace":"System",
    *  "TypeName" : "String",
    *  "Version"  : 100 ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.AddConcepts(sampleConceptStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *}}}
    *
    */
  def AddConcepts(conceptsText:String, formatType:String, userid: Option[String] = None): String // Supported format is JSON/XML

  /** Update existing concepts
    * @param conceptsText an input String of concepts in a format defined by the next parameter formatType
    * @param formatType format of conceptsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    *
    * {{{
    *   val sampleConceptStr = """
    *  {"Concepts" : [
    *  "NameSpace":"Ligadata",
    *  "Name":"ProviderId",
    *  "TypeNameSpace":"System",
    *  "TypeName" : "String",
    *  "Version"  : 101 ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.UpdateConcepts(sampleConceptStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *
    *}}}
    *
    */
  def UpdateConcepts(conceptsText:String, formatType:String, userid: Option[String] = None): String

  /** RemoveConcepts take all concepts names to be removed as an Array
    * @param concepts array of Strings where each string is name of the concept
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example:
    * {{{
    * val apiResult = MetadataAPIImpl.RemoveConcepts(Array("Ligadata.ProviderId.100"))
    * val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + resultData)
    *}}}
    *
    */
  def RemoveConcepts(concepts:Array[String], userid: Option[String] = None): String

  /** Add message given messageText
    *
    * @param messageText text of the message (as JSON/XML string as defined by next parameter formatType)
    * @param formatType format of messageText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example
    *
    * {{{
    * var apiResult = MetadataAPIImpl.AddMessage(msgStr,"JSON"))
    * var result = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + result._2)
    * }}}
    */
  def AddMessage(messageText:String, formatType:String, userid: Option[String] = None, tid: Option[String] = None): String

  /** Update message given messageText
    *
    * @param messageText text of the message (as JSON/XML string as defined by next parameter formatType)
    * @param formatType format of messageText (as JSON/XML string)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def UpdateMessage(messageText:String, formatType:String, userid: Option[String] = None, tid: Option[String] = None): String

  /** Remove message with MessageName and Vesion Number
    *
    * @param messageName Name of the given message
    * @param version   Version of the given message
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def RemoveMessage(messageName:String, version:Long, userid: Option[String]): String

  /** Add container given containerText
    *
    * @param containerText text of the container (as JSON/XML string as defined by next parameter formatType)
    * @param formatType format of containerText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    * Example
    *
    * {{{
    * var apiResult = MetadataAPIImpl.AddContainer(msgStr,"JSON"))
    * var result = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + result._2)
    * }}}
    */
  def AddContainer(containerText:String, formatType:String, userid: Option[String] = None, tenantId: Option[String] = None): String

  /** Update container given containerText
    *
    * @param containerText text of the container (as JSON/XML string as defined by next parameter formatType)
    * @param formatType format of containerText (as JSON/XML string)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def UpdateContainer(containerText:String, formatType:String, userid: Option[String] = None, tenantid: Option[String] = None): String

  /** Remove container with ContainerName and Vesion Number
    *
    * @param containerName Name of the given container
    * @param version   Version of the given container
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def RemoveContainer(containerName:String, version:Long, userid: Option[String]): String

  /** Add model. Several model types are currently supported.  They describe the content of the ''input'' argument:
    *
    *   - SCALA - a Scala source string
    *   - JAVA - a Java source string
    *   - PMML - a PMML source string
    *   - KPMML - a Kamanja Pmml source string
    *   - BINARY - the path to a jar containing the model
    *
    * The remaining arguments, while noted as optional, are required for some model types.  In particular,
    * the ''modelName'', ''version'', and ''msgConsumed'' must be specified for the PMML model type.  The ''userid'' is
    * required for systems that have been configured with a SecurityAdapter or AuditAdapter.
    * @see [[http://kamanja.org/security/ security wiki]] for more information. The audit adapter, if configured,
    *       will also be invoked to take note of this user's action.
    * @see [[http://kamanja.org/auditing/ auditing wiki]] for more information about auditing.
    * NOTE: The BINARY model is not supported at this time.  The model submitted for this type will be via a jar file.
    *
    * @param modelType the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY}
    * @param input the text element to be added dependent upon the modelType specified.
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @param modelName the namespace.name of the PMML model to be added to the Kamanja metadata
    * @param version the model version to be used to describe this PMML model
    * @param msgConsumed the namespace.name of the message to be consumed by a PMML model
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */

  def AddModel( modelType: ModelType
                , input: String
                , userid: Option[String] = None
                , tenantid: Option[String] = None
                , modelName: Option[String] = None
                , version: Option[String] = None
                , msgConsumed: Option[String] = None
                , msgVer : Option[String] = Some("-1")
	              , optMsgProduced: Option[String] = None
              ): String

  /** Update model given the supplied input.  Like the Add model, the ''modelType'' controls the processing and describes the
    * sort of content that has been supplied in the ''input'' argument.  The current ''modelType'' values are:
    *
    *   - SCALA - a Scala source string
    *   - JAVA - a Java source string
    *   - PMML - a PMML source string
    *   - KPMML - a Kamanja Pmml source string
    *   - BINARY - the path to a jar containing the model
    *
    * The remaining arguments, while noted as optional, are required for some model types.  In particular,
    * the ''modelName'' and ''version'' must be specified for the PMML model type.  The ''userid'' is
    * required for systems that have been configured with a SecurityAdapter or AuditAdapter.
    * @see [[http://kamanja.org/security/ security wiki]] for more information. The audit adapter, if configured,
    *       will also be invoked to take note of this user's action.
    * @see [[http://kamanja.org/auditing/ auditing wiki]] for more information about auditing.
    *
    * @param modelType the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY}
    * @param input the text element to be added dependent upon the modelType specified.
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @param modelName appropriate for PMML, the namespace.name of the PMML model to be added to the Kamanja metadata
    * @param version appropriate for PMML, the model version to be assigned. This version ''must'' be greater than the
    *                version in use and unique for models with the modelName
    * @param optVersionBeingUpdated not used .. reserved for future release where explicit modelnamespace.modelname.modelversion
    *                               can be updated (not just the latest version)
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
   */
  def UpdateModel(modelType: ModelType
                  , input: String
                  , userid: Option[String] = None
                  , optTenantid: Option[String] = None
                  , modelName: Option[String] = None
                  , version: Option[String] = None
                  , optVersionBeingUpdated : Option[String] = None
		              , optMsgProduced: Option[String] = None): String

  /** Remove model with the supplied ''modelName'' and ''version''.  If the SecurityAdapter and/or AuditAdapter have
    * been configured, the ''userid'' must also be supplied.
    *
    * @param modelName the Namespace.Name of the given model to be removed
    * @param version   Version of the given model.  The version should comply with the Kamanja version format.  For example,
    *                  a value of 1000001000001 is the value for 1.000001.000001. Helper functions for constructing this
    *                  Long from a string can be found in the MdMgr object,
    *                  @see com.ligadata.kamanja.metadata.ConvertVersionToLong(String):Long for details.
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def RemoveModel(modelName :String, version : String, userid : Option[String] = None): String

  /** Retrieve All available ModelDefs from Metadata Store
   *
   * @param formatType format of the return value, either JSON or XML
   * @return the ModelDef(s) either as a JSON or XML string depending on the parameter formatType
   */
  def GetAllModelDefs(formatType: String, userid: Option[String] = None) : String

  /** Retrieve specific ModelDef(s) from Metadata Store
   *
   * @param objectName Name of the ModelDef
   * @param formatType format of the return value, either JSON or XML
   * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
   * ModelDef(s) either as a JSON or XML string depending on the parameter formatType
   */
  def GetModelDef(objectName:String,formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific ModelDef from Metadata Store
    *
    * @param objectName Name of the ModelDef
    * @param version  Version of the ModelDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the ModelDef either as a JSON or XML string depending on the parameter formatType
   */
  def GetModelDef( objectName:String,version:String, formatType: String, userid: Option[String]) : String


  /** Retrieve All available MessageDefs from Metadata Store
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the MessageDef(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetAllMessageDefs(formatType: String, userid: Option[String] = None) : String

  /** Retrieve specific MessageDef(s) from Metadata Store
    *
    * @param objectName Name of the MessageDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the MessageDef(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetMessageDef(objectName:String,formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific MessageDef from Metadata Store
    *
    * @param objectName Name of the MessageDef
    * @param version  Version of the MessageDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the MessageDef either as a JSON or XML string depending on the parameter formatType
    */
  def GetMessageDef( objectName:String,version:String, formatType: String, userid: Option[String]) : String


  /** Retrieve a specific MessageDef from Metadata Store
    *
    * @param objectNameSpace NameSpace of the MessageDef
    * @param objectName Name of the MessageDef
    * @param version  Version of the MessageDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the MessageDef either as a JSON or XML string depending on the parameter formatType
    */
  def GetMessageDef(objectNameSpace:String,objectName:String,version:String, formatType: String, userid: Option[String]) : String

  /** Retrieve specific ContainerDef(s) from Metadata Store
    *
    * @param objectName Name of the ContainerDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the ContainerDef(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetContainerDef(objectName:String,formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific ContainerDef from Metadata Store
    *
    * @param objectName Name of the ContainerDef
    * @param version  Version of the ContainerDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the ContainerDef either as a JSON or XML string depending on the parameter formatType
    */
  def GetContainerDef( objectName:String,version:String, formatType: String, userid: Option[String]) : String

  /** Retrieve a specific ContainerDef from Metadata Store
    *
    * @param objectNameSpace NameSpace of the ContainerDef
    * @param objectName Name of the ContainerDef
    * @param version  Version of the ContainerDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the ContainerDef either as a JSON or XML string depending on the parameter formatType
    */
  def GetContainerDef(objectNameSpace:String,objectName:String,version:String, formatType: String, userid: Option[String]) : String

   /** Retrieve All available FunctionDefs from Metadata Store. Answer the count and a string representation
    *  of them.
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the function count and the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the FunctionDef(s) either as a JSON or XML string depending on the parameter formatType as a Tuple2[Int,String]
    */

  def GetAllFunctionDefs(formatType: String, userid: Option[String] = None) : (Int,String)

  /** Retrieve specific FunctionDef(s) from Metadata Store
    *
    * @param objectName Name of the FunctionDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the FunctionDef(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetFunctionDef(objectName:String,formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific FunctionDef from Metadata Store
    *
    * @param objectName Name of the FunctionDef
    * @param version  Version of the FunctionDef
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None..
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the FunctionDef either as a JSON or XML string depending on the parameter formatType
    */
  def GetFunctionDef( objectName:String,version:String, formatType: String, userid: Option[String]) : String

  /** Retrieve All available Concepts from Metadata Store
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Concept(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetAllConcepts(formatType: String, userid: Option[String] = None) : String

  /** Retrieve specific Concept(s) from Metadata Store
    *
    * @param objectName Name of the Concept
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None..
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Concept(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetConcept(objectName:String, formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific Concept from Metadata Store
    *
    * @param objectName Name of the Concept
    * @param version  Version of the Concept
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Concept either as a JSON or XML string depending on the parameter formatType
    */
  def GetConcept(objectName:String,version: String, formatType: String, userid: Option[String]) : String


  /** Retrieve All available derived concepts from Metadata Store
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Derived Concept(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetAllDerivedConcepts(formatType: String, userid: Option[String] = None) : String


  /** Retrieve specific Derived Concept(s) from Metadata Store
    *
    * @param objectName Name of the Derived Concept
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Derived Concept(s) either as a JSON or XML string depending on the parameter formatType
    */
  def GetDerivedConcept(objectName:String, formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific Derived Concept from Metadata Store
    *
    * @param objectName Name of the Derived Concept
    * @param version  Version of the Derived Concept
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Derived Concept either as a JSON or XML string depending on the parameter formatType
    */
  def GetDerivedConcept(objectName:String, version:String, formatType: String, userid: Option[String]) : String

  /** Retrieves all available Types from Metadata Store
    *
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the available types as a JSON or XML string depending on the parameter formatType
    */
  def GetAllTypes(formatType: String, userid: Option[String] = None) : String

  /** Retrieve a specific Type  from Metadata Store
    *
    * @param objectName Name of the Type
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    * the Type object either as a JSON or XML string depending on the parameter formatType
    */
  def GetType(objectName:String, formatType: String, userid: Option[String] = None) : String
  
   /**
    * getHealthCheck - will return all the health-check information for the nodeId specified.
    *
    * @parm - nodeId: String - if no parameter specified, return health-check for all nodes
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return status string
    */
  def getHealthCheck(nodeId: String, userid: Option[String] = None): String

  /**
    *  getHealthCheckNodesOnly - will return node info from the health-check information for the nodeId specified.
    *  @param nodeId a cluster node: String - if no parameter specified, return health-check for all nodes
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckNodesOnly(nodeId: String = "", userid: Option[String] = None): String

  /**
    *  getHealthCheckComponentNames - will return partial components info from the health-check information for the nodeId specified.
    *  @param nodeId a cluster node: String - if no parameter specified, return health-check for all nodes
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckComponentNames(nodeId: String = "", userid: Option[String] = None): String

  /**
    *  getHealthCheckComponentDetailsByNames - will return specific components info from the health-check information for the nodeId specified.
    *  @param componentNames names of components required
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckComponentDetailsByNames(componentNames: String = "", userid: Option[String] = None): String
}
