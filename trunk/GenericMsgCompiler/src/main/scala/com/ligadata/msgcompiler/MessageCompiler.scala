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
import org.apache.log4j.Logger;

import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;

class Messages(var messages: List[Message])
class Message(var MsgType: String, var NameSpace: String, var Name: String, var PhysicalName: String, var Version: String, var Description: String, var Fixed: String, var Persist: Boolean, var Elements: List[Element], var TDataExists: Boolean, var TrfrmData: TransformData, var Jarset: Set[String], var Pkg: String, var Ctype: String, var CCollectiontype: String, var Containers: List[String], var PartitionKey: List[String], var PrimaryKeys: List[String], var ClsNbr: Long, var MsgLvel: Int)
class TransformData(var input: Array[String], var output: Array[String], var keys: Array[String])
class Field(var NameSpace: String, var Name: String, var Ttype: String, var CollectionType: String, var Fieldtype: String, var FieldtypeVer: String)
class Element(var NameSpace: String, var Name: String, var Ttype: String, var CollectionType: String, var ElemType: String, var FieldtypeVer: String, var FieldOrdinal: Int)
class MessageGenObj(var verScalaClassStr: String, var verJavaClassStr: String, var containerDef: ContainerDef, var noVerScalaClassStr: String, var noVerJavaClassStr: String, var argsList: List[(String, String, String, String, Boolean, String)])

object MessageCompiler {

  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)
  var msgGen: MessageGenerator = new MessageGenerator

  /*
   * parse the message definition json,  add messages to metadata and create the Fixed and Mapped Mesages
   */
  def processMsgDef(jsonstr: String, msgDfType: String, mdMgr: MdMgr, recompile: Boolean = false): String = {

    var messageParser = new MessageParser
    var message: Message = null
    var generatedMsg: String = ""

    try {
      if (mdMgr == null)
        throw new Exception("MdMgr is not found")
      if (msgDfType.equals("JSON")) {
        message = messageParser.processJson(jsonstr, mdMgr, recompile).asInstanceOf[Message]
        generatedMsg = msgGen.generateMessage(message)
      } else throw new Exception("MsgDef Type JSON is only supported")

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return generatedMsg
  }

  def main(args: Array[String]) {
    var msgDfType: String = "JSON"
    val jsonstr: String = Source.fromFile("src/main/resources/message1.json").getLines.mkString
    // val jsonstr: String = Source.fromFile("src/main/resources/nestedlvl2.json").getLines.mkString

    val message = MessageCompiler.processMsgDef(jsonstr, msgDfType, MdMgr.GetMdMgr, false)
    log.info("============== Generated Message Start==============")
    log.info(message)
    log.info("============== Generated Message End ==============")

  }

}