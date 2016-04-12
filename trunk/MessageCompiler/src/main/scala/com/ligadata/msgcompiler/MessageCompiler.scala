package com.ligadata.msgcompiler

import scala.util.parsing.json.JSON;
import scala.io.Source;

import java.io.File;
import java.io.PrintWriter;
import java.util.Date;

import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.ArrayBuffer;

import org.json4s.jackson.JsonMethods._;
import org.json4s.DefaultFormats;
import org.json4s.Formats;
import org.apache.logging.log4j.{ Logger, LogManager }

import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;

class Messages(var messages: List[Message])
class Message(var MsgType: String, var NameSpace: String, var Name: String, var PhysicalName: String, var Version: String, var VersionLong : Long, var Description: String, var Fixed: String, var Persist: Boolean, var Elements: List[Element], var TDataExists: Boolean, var TrfrmData: TransformData, var Jarset: Set[String], var Pkg: String, var Ctype: String, var CCollectiontype: String, var Containers: List[String], var PartitionKeys: List[String], var PrimaryKeys: List[String], var ClsNbr: Long, var MsgLvel: Int, var ArgsList: List[(String, String, String, String, Boolean, String)], var Schema: String, var Definition: String, var timePartition: TimePartition, var schemaId: Int)
class TransformData(var input: Array[String], var output: Array[String], var keys: Array[String])
class Element(var NameSpace: String, var Name: String, var Ttype: String, var CollectionType: String, var ElemType: String, var FieldtypeVer: String, var FieldOrdinal: Int, var FldMetaataType: BaseTypeDef, var FieldTypePhysicalName: String, var FieldTypeImplementationName: String, var FieldObjectDefinition: String, var AttributeTypeInfo: ArrtibuteInfo)
class MessageGenObj(var verScalaClassStr: String, var verJavaClassStr: String, var containerDef: ContainerDef, var noVerScalaClassStr: String, var noVerJavaClassStr: String, var argsList: List[(String, String, String, String, Boolean, String)])
class TimePartition(var Key: String, var Format: String, var DType: String)

// typeCategary ==> 1 - standard type ( typeId: 1..N represent standard avro compatible types, 1001 - container/msg.., 1002 - map of fixed types, 1003 - map of fixed typed key and any base elem)
// valSchemaId ==>      If value is of type container, this represent schema id denoting the definition of contaner; for avro schema this information embedded in schema but for internal/non avro purposes, we want to use more efficient representation
// valTypeId ==> 
// keyTypeId ==> type id for key type if the map is of fixed types; for avro compatible maps, only string as key type is allowed

class ArrtibuteInfo(var typeCategaryName: String, var valTypeId: Int, var keyTypeId: Int, var valSchemaId: Long)
    
    
class MessageCompiler {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var msgGen: MessageGenerator = new MessageGenerator
  var handleMsgFieldTypes: MessageFieldTypesHandler = new MessageFieldTypesHandler
  var createMsg: CreateMessage = new CreateMessage
  var generatedRdd = new GenerateRdd
  var schemaCompiler = new SchemaCompiler
  var rawMsgGenerator = new RawMsgGenerator
  /*
   * parse the message definition json,  add messages to metadata and create the Fixed and Mapped Mesages
   */
  def processMsgDef(jsonstr: String, msgDfType: String, mdMgr: MdMgr, schemaId: Int, recompile: Boolean = false): ((String, String), ContainerDef, (String, String), String) = {

    var messageParser = new MessageParser
    var messages: Messages = null
    var message: Message = null
    var generatedNonVersionedMsg: String = ""
    var generatedVersionedMsg: String = ""
    var generatedNonVersionedJavaRdd: String = ""
    var generatedVersionedJavaRdd: String = ""
    var containerDef: ContainerDef = null
    var generateRawMessage: String = null;

    try {
      if (mdMgr == null)
        throw new Exception("MdMgr is not found")
      if (msgDfType.equalsIgnoreCase("json")) {
        message = messageParser.processJson(jsonstr, mdMgr, recompile)
        message.schemaId = schemaId
        handleMsgFieldTypes.handleFieldTypes(message, mdMgr)
        //log.info("\n\nSchema ==============START" + message.Schema);
        message = schemaCompiler.generateAvroSchema(message, mdMgr);
       /* log.info(" message.Schema: " + message.Schema + "\n\n");
        log.info("Schema ==============END\n\n")
        log.info("JARSET " + message.Jarset.toList);
        log.info("ArgsList Jars " + message.ArgsList);
*/
        val (genVersionedMsg, genNonVersionedMsg) = msgGen.generateMessage(message, mdMgr)
        generatedNonVersionedMsg = genNonVersionedMsg
        generatedVersionedMsg = genVersionedMsg

        val (versionedRddClass, nonVersionedRddClass) = generatedRdd.generateRdd(message)
        generatedNonVersionedJavaRdd = nonVersionedRddClass
        generatedVersionedJavaRdd = versionedRddClass

        containerDef = createMsg.createMessage(message, mdMgr, recompile)

        generateRawMessage = rawMsgGenerator.generateRawMessage(message, mdMgr);

      } else throw new Exception("MsgDef Type JSON is only supported")

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return ((generatedVersionedMsg, generatedVersionedJavaRdd), containerDef, (generatedNonVersionedMsg, generatedNonVersionedJavaRdd), generateRawMessage)
  }
}

 
