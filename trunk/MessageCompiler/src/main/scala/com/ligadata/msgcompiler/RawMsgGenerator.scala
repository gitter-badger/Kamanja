package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;

class RawMsgGenerator {
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var msgGen: MessageGenerator = new MessageGenerator
  var handleMsgFieldTypes: MessageFieldTypesHandler = new MessageFieldTypesHandler
  var schemaCompiler = new SchemaCompiler

  def generateRawMessage(message: Message, mdMgr: MdMgr): String = {
    genRawMsg(message, mdMgr)
  }

  /*
   * update the message fields type to String and Generate the Scala Code
   */
  private def genRawMsg(message: Message, mdMgr: MdMgr): String = {
    var rawMsgwithVersion: String = ""
    var rawMsgWithNoVersion: String = "";
    var rawMsgStruct: Message = null
    try {
      rawMsgStruct = message
      if (rawMsgStruct.Elements != null) {
        rawMsgStruct.Elements.foreach(field => {
          if (field != null) {
            field.Ttype = "system.string"
          }
        })

        handleMsgFieldTypes.handleFieldTypes(rawMsgStruct, mdMgr)
        rawMsgStruct = schemaCompiler.generateAvroSchema(rawMsgStruct, mdMgr);
        val (msgWithVersion, msgWithNoVersion) = msgGen.generateMessage(rawMsgStruct, mdMgr)
        rawMsgwithVersion = msgWithVersion
      }

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    rawMsgwithVersion
  }

}