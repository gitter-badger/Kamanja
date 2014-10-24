
package com.ligadata.OnLEPManager

import com.ligadata.OnLEPBase.{ BaseMsg, DelimitedData, JsonData, XmlData, EnvContext }
import com.ligadata.Utils.Utils
import java.util.Map
import scala.util.Random
import com.ligadata.OnLEPBase.{ MdlInfo, BaseMsgObj, BaseMsg, InputAdapter, OutputAdapter }
import org.apache.log4j.Logger
import java.io.{ PrintWriter, File }
import scala.xml.XML
import scala.xml.Elem
import scala.util.parsing.json.JSON

class LearningEngine(val input: InputAdapter, val processingPartitionId: Int, val output: Array[OutputAdapter]) {
  val LOG = Logger.getLogger(getClass);
  var cntr: Long = 0
  var totalLatencyFromReadToProcess: Long = 0
  // var totalLatencyFromReadToOutput: Long = 0

  val rand = new Random(hashCode)

  private def createMsg(msgType: String, msgFormat: String, msgData: String): BaseMsg = {
    val msgInfo = OnLEPMetadata.getMessgeInfo(msgType)
    if (msgInfo != null) {
      val msg: BaseMsg = msgInfo.msgobj.CreateNewMessage
      if (msgFormat.equalsIgnoreCase("csv")) {
        try {
          val inputData = new DelimitedData(msgData, ",")
          inputData.tokens = inputData.dataInput.split(inputData.dataDelim, -1)
          inputData.curPos = 0
          msg.populate(inputData)
        } catch {
          case e: Exception => {
            LOG.error("Failed to populate CSV data for messageType:" + msgType)
          }
        }
      } else if (msgFormat.equalsIgnoreCase("json")) {
        try {
          val inputData = new JsonData(msgData)
          inputData.root_json = JSON.parseFull(inputData.dataInput)
          inputData.cur_json = inputData.root_json
          msg.populate(inputData)
        } catch {
          case e: Exception => {
            LOG.error("Failed to populate JSON data for messageType:" + msgType)
          }
        }
      } else if (msgFormat.equalsIgnoreCase("xml")) {
        try {
          val inputData = new XmlData(msgData)
          inputData.root_xml = XML.loadString(inputData.dataInput)
          inputData.cur_xml = inputData.root_xml
          msg.populate(inputData)
        } catch {
          case e: Exception => {
            LOG.error("Failed to populate XML data for messageType:" + msgType)
          }
        }
      } else {
        throw new Exception("Invalid input data type:" + msgFormat)
        return null
      }
      msg
    } else {
      throw new Exception("Not found Message Type:" + msgType)
      null
    }
  }

  private def RunAllModels(msg: BaseMsg, msgData: String, envContext: EnvContext, readTmNs: Long, rdTmMs: Long): Unit = {
    if (msg == null)
      return

    val models: Array[MdlInfo] = OnLEPMetadata.modelObjects.map(mdl => mdl._2).toArray
    var result: StringBuilder = new StringBuilder(8 * 1024)

    result ++= "{\"ModelsResult\" : ["

    // var executedMdls = 0
    var gotResults = 0

    val outputAlways: Boolean = false; // (rand.nextInt(9) == 5) // For now outputting ~(1 out of 9) randomly when we get random == 5

    // Execute all modes here
    models.foreach(md => {
      try {

        if (md.mdl.IsValidMessage(msg)) { // Checking whether this message has any fields/concepts to execute in this model
          // LOG.info("Found Valid Message:" + msgData)
          // executedMdls += 1
          val curMd = md.mdl.CreateNewModel(envContext, msg, md.tenantId)
          if (curMd != null) {
            val res = curMd.execute(outputAlways)
            if (res != null) {
              if (gotResults > 0)
                result ++= ","
              result ++= res.toJsonString(readTmNs, rdTmMs)
              gotResults = gotResults + 1
            } else {
              // Nothing to output
            }
          } else {
            LOG.error("Failed to create model " + md.mdl.getModelName)
          }
        } else {
          // LOG.info("Found Invalid Message:" + msgData)
        }
      } catch {
        case e: Exception => { LOG.error("Model Failed => " + md.mdl.getModelName + ". Error: " + e.getMessage /* + "\n Trace:\n" + e.printStackTrace() */ ) }
      }
    })

    result ++= "]}"

    if (gotResults > 0 && output != null) {
      val resStr = result.toString
      output.foreach(o => {
        o.send(resStr, cntr.toString)
      })
    }
  }

  private def GetTopMsgName(msgName: String): (String, Boolean, MsgObjAndTransformInfo) = {
    val topMsgInfo = OnLEPMetadata.getMessgeInfo(msgName)
    if (topMsgInfo == null || topMsgInfo.parents.size == 0) return (msgName, false, null)
    (topMsgInfo.parents(0)._1, true, topMsgInfo)
  }

  def execute(msgType: String, msgFormat: String, msgData: String, envContext: EnvContext, readTmNs: Long, rdTmMs: Long): Unit = {
    // LOG.info("LE => " + msgData)
    try {
      // BUGBUG:: for now handling only CSV input data.
      val msg = createMsg(msgType, msgFormat, msgData)
      if (msg != null) {
        // BUGBUG::Get Previous History (through Key) of the top level message/container 
        // Get top level Msg for the current msg
        val topMsgTypeAndHasParent = GetTopMsgName(msgType)
        val keyData = msg.PartitionKeyData
        val topObj = envContext.getMsgObject(topMsgTypeAndHasParent._1, keyData)
        var handleMsg: Boolean = true
        if (topMsgTypeAndHasParent._2) {
          handleMsg = topObj != null
        }
        if (handleMsg) {
          val finalTopMsg = if (topObj != null) topObj else msg
          if (topMsgTypeAndHasParent._2)
            finalTopMsg.AddMessage(topMsgTypeAndHasParent._3.parents.toArray, msg)
          // Run all models
          RunAllModels(finalTopMsg, msgData, envContext, readTmNs, rdTmMs)
          var latencyFromReadToProcess = (System.nanoTime - readTmNs) / 1000 // Nanos to micros
          if (latencyFromReadToProcess < 0) latencyFromReadToProcess = 40 // taking minimum 40 micro secs
          totalLatencyFromReadToProcess += latencyFromReadToProcess
          //BUGBUG:: Save the whole message here
          if (topMsgTypeAndHasParent._2 || (topObj == null))
            envContext.setMsgObject(topMsgTypeAndHasParent._1, keyData, finalTopMsg)
        }
      }
    } catch {
      case e: Exception => LOG.error("Failed to create and run message. Error:" + e.getMessage)
    }

    cntr += 1
  }
}

