
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

package com.ligadata.KamanjaManager

import com.ligadata.KamanjaBase._

// import com.ligadata.Utils.Utils
// import java.util.Map
import com.ligadata.utils.dag.{ReadyNode, EdgeId, DagRuntime}
import org.apache.logging.log4j.{Logger, LogManager}

//import java.io.{PrintWriter, File}
//import scala.xml.XML
//import scala.xml.Elem
import scala.collection.mutable.ArrayBuffer

//import org.json4s._
//import org.json4s.JsonDSL._
//import org.json4s.jackson.JsonMethods._
import com.ligadata.InputOutputAdapterInfo.{ExecContext, InputAdapter, PartitionUniqueRecordKey, PartitionUniqueRecordValue}
import com.ligadata.Exceptions.{KamanjaException, StackTrace, MessagePopulationException}

object LeanringEngine {
  // There are 3 types of error that we can create an ExceptionMessage for
//  val invalidMessage: String = "Invalid_message"
//  val invalidResult: String = "Invalid_result"
  val modelExecutionException: String = "Model_Excecution_Exception"

  val engineComponent: String = "Kamanja_Manager"
}

class LearningEngine {
  val LOG = LogManager.getLogger(getClass);
  var cntr: Long = 0
  var mdlsChangedCntr: Long = -1
  var nodeIdModlsObj = scala.collection.mutable.Map[Long, (MdlInfo, ModelInstance)]()
  // Key is Nodeid (Model ElementId), Value is ModelInfo & Previously Initialized model instance in case of Reuse instances
  // ModelName, ModelInfo, IsModelInstanceReusable, Global ModelInstance if the model is IsModelInstanceReusable == true. The last boolean is to check whether we tested message type or not (thi is to check Reusable flag)
  // var models = Array[(String, MdlInfo, Boolean, ModelInstance, Boolean)]()
  // var validateMsgsForMdls = scala.collection.mutable.Set[String]() // Message Names for creating models inst val results = RunAllModels(transId, iances

  var dagRuntime = new DagRuntime()

  def execute(txnCtxt: TransactionContext): Unit = {

    // List of ModelIds that we ran.
    var outMsgIds: ArrayBuffer[Long] = new ArrayBuffer[Long]()
    var modelsForMessage: ArrayBuffer[KamanjaModelEvent] = new ArrayBuffer[KamanjaModelEvent]()

    var msgProcessingEndTime: Long = -1L
    var thisMsgEvent: KamanjaMessageEvent = txnCtxt.getMessageEvent.asInstanceOf[KamanjaMessageEvent]
    var msgProcessingStartTime: Long = System.nanoTime

    try {
      val mdlChngCntr = KamanjaMetadata.GetModelsChangedCounter
      if (mdlChngCntr != mdlsChangedCntr) {
        val (newDag, tmpNodeIdModlsObj, tMdlsChangedCntr) = KamanjaMetadata.getExecutionDag
        dagRuntime.SetDag(newDag)
        mdlsChangedCntr = tMdlsChangedCntr
        val newNodeIdModlsObj = scala.collection.mutable.Map[Long, (MdlInfo, ModelInstance)]()
        tmpNodeIdModlsObj.foreach(info => {
          val oldMdlInfo = nodeIdModlsObj.getOrElse(info._1, null)
          if (oldMdlInfo != null && info._2.mdl.getModelDef().UniqId == oldMdlInfo._1.mdl.getModelDef().UniqId) {
            // If it is same version and same factory
            newNodeIdModlsObj(info._1) = (info._2, oldMdlInfo._2)
          } else {
            newNodeIdModlsObj(info._1) = (info._2, null)
          }
        })
        nodeIdModlsObj = newNodeIdModlsObj
      }

      dagRuntime.ReInit()

      val outputDefault: Boolean = false;
      val (origin, orgMsg) = txnCtxt.getInitialMessage
      val elemId = KamanjaMetadata.getMdMgr.ElementIdForSchemaId(orgMsg.asInstanceOf[ContainerInterface].getSchemaId)
      val readyNodes = dagRuntime.FireEdge(EdgeId(0, elemId))
      if (LOG.isDebugEnabled) {
        val msgTyp = if (orgMsg == null) "null" else orgMsg.getFullTypeName
        val msgElemId = if (orgMsg == null) "0" else KamanjaMetadata.getMdMgr.ElementIdForSchemaId(orgMsg.asInstanceOf[ContainerInterface].getSchemaId)
        val producedNodeIds = if (readyNodes != null) readyNodes.map(nd => "(NodeId:%d,iesPos:%d)".format(nd.nodeId, nd.iesPos)).mkString(",") else ""
        val msg = "LearningEngine:InputMessageFrom:%s with ElementId:%d produced:%s from DAG".format(msgTyp, msgElemId, producedNodeIds)
        LOG.debug(msg)
      }
      thisMsgEvent.messageid = elemId

      val exeQueue = ArrayBuffer[ReadyNode]()
      var execPos = 0

      exeQueue ++= readyNodes

      while (execPos < exeQueue.size) {
        val execNode = exeQueue(execPos)
        execPos += 1

        val execMdl = nodeIdModlsObj.getOrElse(execNode.nodeId, null)
        if (execMdl != null) {
          if (LOG.isDebugEnabled)
            LOG.debug("LearningEngine:Executing Model:" + execMdl._1.mdl.getModelName())
          val curMd =
            if (execMdl._1.mdl.isModelInstanceReusable()) {
              if (execMdl._2 == null) { // First time initialize this
                val tInst = execMdl._1.mdl.createModelInstance()
                tInst.init(txnCtxt.origin.key)
                nodeIdModlsObj(execNode.nodeId) = (execMdl._1, tInst)
                tInst
              } else {
                execMdl._2
              }
            } else { // Not reusable instance. So, create it every time
              val tInst = execMdl._1.mdl.createModelInstance()
              tInst.init(txnCtxt.origin.key)
              tInst
            }

          if (curMd != null) {
            var modelEvent = txnCtxt.getNodeCtxt.getEnvCtxt.getContainerInstance("com.ligadata.KamanjaBase.KamanjaModelEvent").asInstanceOf[KamanjaModelEvent]
            modelEvent.isresultproduced = false
            modelEvent.producedmessages = Array[Long]()
            val modelStartTime = System.nanoTime

            try {
              val execMsgsSet: Array[ContainerOrConcept] = execMdl._1.inputs(execNode.iesPos).map(eid => {
                if (LOG.isDebugEnabled)
                  LOG.debug("MsgInfo: nodeId:" + eid.nodeId + ", edgeTypeId:" + eid.edgeTypeId)
                val tmpElem = KamanjaMetadata.getMdMgr.ContainerForElementId(eid.edgeTypeId)

                val finalEntry =
                  if (tmpElem != None) {
                    val lst =
                      if (eid.nodeId > 0) {
                        val origin =
                          if (eid.nodeId > 0) {
                            val mdlObj = nodeIdModlsObj.getOrElse(eid.nodeId, null)
                            if (mdlObj != null) {
                              mdlObj._1.mdl.getModelName()
                            } else {
                              LOG.debug("Not found any node for eid.nodeId:" + eid.nodeId)
                              ""
                            }
                          } else {
                            LOG.debug("Origin nodeid is not valid")
                            ""
                          }
                        txnCtxt.getContainersOrConcepts(origin, tmpElem.get.FullName)
                      } else {
                        txnCtxt.getContainersOrConcepts(tmpElem.get.FullName)
                      }

                    if (lst != null && lst.size > 0) {
                      lst(0)._2
                    } else {
                      LOG.warn("Not found any message for Msg:" + tmpElem.get.FullName)
                      null
                    }
                  } else {
                    LOG.warn("Not found any message for EdgeTypeId:" + eid.edgeTypeId)
                    null
                  }
                finalEntry
              })

              val res = curMd.execute(txnCtxt, execMsgsSet, execNode.iesPos, outputDefault)
              if (res != null && res.size > 0) {
                modelEvent.isresultproduced = true
                txnCtxt.addContainerOrConcepts(execMdl._1.mdl.getModelName(), res)
                modelEvent.producedmessages = res.map(msg => KamanjaMetadata.getMdMgr.ElementIdForSchemaId(msg.asInstanceOf[ContainerInterface].getSchemaId) )
                val newEges = res.map(msg => EdgeId(execMdl._1.nodeId, KamanjaMetadata.getMdMgr.ElementIdForSchemaId(msg.asInstanceOf[ContainerInterface].getSchemaId)))
                val readyNodes = dagRuntime.FireEdges(newEges)
                exeQueue ++= readyNodes

                if (LOG.isDebugEnabled) {
                  val inputMsgs = execMsgsSet.map(msg => msg.getFullTypeName).mkString(",")
                  val outputMsgs = res.map(msg => msg.getFullTypeName).mkString(",")
                  val inputEges = newEges.map(edge => "(NodeId:%d,edgeTypeId:%d)".format(edge.nodeId, edge.edgeTypeId)).mkString(",")
                  val producedNodeIds = if (readyNodes != null) readyNodes.map(nd => "(NodeId:%d,iesPos:%d)".format(nd.nodeId, nd.iesPos)).mkString(",") else ""
                  val msg = "LearningEngine:Executed Model:%s with NodeId:%d Using messages:%s, which produced %s (with Edges:%s). Adding those message into DAG produced:%s".format(execMdl._1.mdl.getModelName(), execNode.nodeId, inputMsgs, outputMsgs, inputEges, producedNodeIds)
                  LOG.debug(msg)
                }
              } else {
                if (LOG.isDebugEnabled) {
                  val inputMsgs = execMsgsSet.map(msg => msg.getFullTypeName).mkString(",")
                  val msg = "LearningEngine:Executed Model:%s with NodeId:%d Using messages:%s, which did not produce any result".format(execMdl._1.mdl.getModelName(), execNode.nodeId, inputMsgs)
                  LOG.debug(msg)
                }
              }
            } catch {
              case e: Throwable => {
                modelEvent.error = StackTrace.ThrowableTraceString(e)
                LOG.error("Failed to execute model:" + execMdl._1.mdl.getModelName(), e)
              }
            }

            // Model finished executing, add the stats to the modeleventmsg
            //var mdlDefs = KamanjaMetadata.getMdMgr.Models(md.mdl.getModelDef().FullName,true, false).getOrElse(null)
            modelEvent.modelid = execNode.nodeId
            modelEvent.elapsedtimeinms = ((System.nanoTime - modelStartTime) / 1000000.0).toFloat
            modelsForMessage.append(modelEvent)
          } else {
            val errorTxt = "Failed to create model " + execMdl._1.mdl.getModelName()
            LOG.error(errorTxt)
            thisMsgEvent.error = "Failed to create model " + execMdl._1.mdl.getModelName()
            thisMsgEvent.elapsedtimeinms = ((System.nanoTime - msgProcessingStartTime) / 1000000.0).toFloat
            thisMsgEvent.modelinfo = modelsForMessage.toArray[KamanjaModelEvent]
            // Generate an exception event
            val exeptionEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, errorTxt, txnCtxt)
            txnCtxt.getNodeCtxt.getEnvCtxt.postMessages(Array(exeptionEvent))
            // Do we need to throw an error ???????????????????????????????????????? throw new KamanjaException(errorTxt, null)
          }
        }
      }
    } catch {
      case e: Exception => {
        // Generate an exception event
        LOG.error("Failed to execute message", e)
        val st = StackTrace.ThrowableTraceString(e)
        thisMsgEvent.error = st
        thisMsgEvent.elapsedtimeinms = ((System.nanoTime - msgProcessingStartTime) / 1000000.0).toFloat
        thisMsgEvent.modelinfo = modelsForMessage.toArray[KamanjaModelEvent]
        val exeptionEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
        txnCtxt.getNodeCtxt.getEnvCtxt.postMessages(Array(exeptionEvent))
        // throw e
      }
    }
    thisMsgEvent.modelinfo = modelsForMessage.toArray[KamanjaModelEvent]
  }

  private def createExceptionEvent(errorType: String, compName: String, errorString: String, txnCtxt: TransactionContext): KamanjaExceptionEvent = {
    // ExceptionEventFactory is guaranteed to be here....
    var exceptionEvnt = txnCtxt.getNodeCtxt.getEnvCtxt.getContainerInstance("com.ligadata.KamanjaBase.KamanjaExceptionEvent")
    if (exceptionEvnt == null)
      throw new KamanjaException("Unable to create message/container com.ligadata.KamanjaBase.KamanjaExceptionEvent", null)
    var exceptionEvent = exceptionEvnt.asInstanceOf[KamanjaExceptionEvent]
    exceptionEvent.errortype = errorType
    exceptionEvent.timeoferrorepochms = System.currentTimeMillis
    exceptionEvent.componentname = compName
    exceptionEvent.errorstring = errorString
    exceptionEvent
  }

  /*
    private def RunAllModels(transId: Long, inputData: Array[Byte], finalTopMsgOrContainer: ContainerInterface, txnCtxt: TransactionContext, uk: String, uv: String, msgEvent: KamanjaMessageEvent): Unit = {
  //    var results: ArrayBuffer[SavedMdlResult] = new ArrayBuffer[SavedMdlResult]()
      var oMsgIds: ArrayBuffer[Long] = new ArrayBuffer[Long]()

      var tempModelAB: ArrayBuffer[KamanjaModelEvent] = ArrayBuffer[KamanjaModelEvent]()
      if (LOG.isDebugEnabled)
        LOG.debug(s"Processing uniqueKey:$uk, uniqueVal:$uv, finalTopMsgOrContainer:$finalTopMsgOrContainer, previousModles:${models.size}")

      if (finalTopMsgOrContainer != null) {
        // txnCtxt.setInitialMessage(finalTopMsgOrContainer)
        ThreadLocalStorage.txnContextInfo.set(txnCtxt)
        try {
          val mdlChngCntr = KamanjaMetadata.GetModelsChangedCounter
          val msgFullName = finalTopMsgOrContainer.getFullTypeName.toLowerCase()
          if (mdlChngCntr != mdlsChangedCntr) {
            LOG.info("Refreshing models for Partition:%s from %d to %d".format(uk, mdlsChangedCntr, mdlChngCntr))
            val (tmpMdls, tMdlsChangedCntr) = KamanjaMetadata.getAllModels
            val tModels = if (tmpMdls != null) tmpMdls else Array[(String, MdlInfo)]()

            val map = scala.collection.mutable.Map[String, (MdlInfo, Boolean, ModelInstance, Boolean)]()
            models.foreach(q => {
              map(q._1) = ((q._2, q._3, q._4, q._5))
            })

            var newModels = ArrayBuffer[(String, MdlInfo, Boolean, ModelInstance, Boolean)]()
            var newMdlsSet = scala.collection.mutable.Set[String]()

            tModels.foreach(tup => {
              if (LOG.isDebugEnabled)
                LOG.debug("Model:" + tup._1)
              val md = tup._2
              val mInfo = map.getOrElse(tup._1, null)

              var newInfo: (String, MdlInfo, Boolean, ModelInstance, Boolean) = null
              if (mInfo != null) {
                // Make sure previous model version is same as the current model version
                if (md.mdl == mInfo._1.mdl && md.mdl.getVersion().equals(mInfo._1.mdl.getVersion())) {
                  newInfo = ((tup._1, mInfo._1, mInfo._2, mInfo._3, mInfo._4)) // Taking  previous record only if the same instance of the object exists
                } else {
                  // Shutdown previous entry, if exists
                  if (mInfo._2 && mInfo._3 != null) {
                    mInfo._3.shutdown()
                  }
                  if (md.mdl.isValidMessage(finalTopMsgOrContainer)) {
                    val tInst = md.mdl.createModelInstance()
                    val isReusable = md.mdl.isModelInstanceReusable()
                    var newInst: ModelInstance = null
                    if (isReusable) {
                      newInst = tInst
                      newInst.init(uk)
                    }
                    newInfo = ((tup._1, md, isReusable, newInst, true))
                  } else {
                    newInfo = ((tup._1, md, false, null, false))
                  }
                }
              } else {
                if (md.mdl.isValidMessage(finalTopMsgOrContainer)) {
                  var newInst: ModelInstance = null
                  val tInst = md.mdl.createModelInstance()
                  val isReusable = md.mdl.isModelInstanceReusable()
                  if (isReusable) {
                    newInst = tInst
                    newInst.init(uk)
                  }
                  newInfo = ((tup._1, md, isReusable, newInst, true))
                } else {
                  newInfo = ((tup._1, md, false, null, false))
                }
              }
              if (newInfo != null) {
                newMdlsSet += tup._1
                newModels += newInfo
              }
            })

            // Make sure we did shutdown all the instances which are deleted
            models.foreach(mInfo => {
              if (newMdlsSet.contains(mInfo._1) == false) {
                if (mInfo._3 && mInfo._4 != null)
                  mInfo._4.shutdown()
              }
            })

            validateMsgsForMdls.clear()
            models = newModels.toArray
            mdlsChangedCntr = tMdlsChangedCntr
            validateMsgsForMdls += msgFullName

            LOG.info("Refreshed models for Partition:%s, mdlsChangedCntr:%d, total models in metadata:%s, total collected models:%d".format(uk, mdlsChangedCntr, tModels.size, models.size))
          } else if (validateMsgsForMdls.contains(msgFullName) == false) {
            // found new Msg
            for (i <- 0 until models.size) {
              val mInfo = models(i)
              if (mInfo._5 == false && mInfo._2.mdl.isValidMessage(finalTopMsgOrContainer)) {
                var newInst: ModelInstance = null
                val tInst = mInfo._2.mdl.createModelInstance()
                val isReusable = mInfo._2.mdl.isModelInstanceReusable()
                if (isReusable) {
                  newInst = tInst
                  newInst.init(uk)
                }
                val msgTypeWasChecked: Boolean = true
                models(i) = (mInfo._1, mInfo._2, isReusable, newInst, msgTypeWasChecked)
              }
            }
            validateMsgsForMdls += msgFullName
          }

          val outputDefault: Boolean = false;

          // Execute all modes here
          models.foreach(q => {
            val md = q._2
            try {
              if (md.mdl.isValidMessage(finalTopMsgOrContainer)) {
                if (LOG.isDebugEnabled)
                  LOG.debug("Processing uniqueKey:%s, uniqueVal:%s, model:%s".format(uk, uv, md.mdl.getModelName))
                // Checking whether this message has any fields/concepts to execute in this model
                val curMd = if (q._3) {
                  q._4
                } else {
                  val tInst = md.mdl.createModelInstance()
                  tInst.init(uk)
                  tInst
                }
                if (curMd != null) {
                  var modelEvent: KamanjaModelEvent = modelEventFactory.createInstance.asInstanceOf[KamanjaModelEvent]
                  val modelStartTime = System.nanoTime
                  curMd.getModelName()
                  val res = curMd.execute(txnCtxt, outputDefault)

                  // TODO: Add the results to the model Event
                  if (res != null) {
                    modelEvent.isresultproduced = true
                    oMsgIds.append(0L)
  //                  results += new SavedMdlResult().withMdlName(md.mdl.getModelName).withMdlVersion(md.mdl.getVersion).withUniqKey(uk).withUniqVal(uv).withTxnId(transId).withMdlResult(res)
                  } else {
                    modelEvent.isresultproduced = false
                    // Nothing to output
                  }
                  modelEvent.producedmessages = oMsgIds.toArray[Long]
                  modelEvent.elapsedtimeinms = ((System.nanoTime - modelStartTime)/1000000.0).toFloat
                  var mdlId: Long = -1
                  // Get the modelId for reporing purposes
                  var mdlDefs = KamanjaMetadata.getMdMgr.Models(md.mdl.getModelDef().FullName,true, false).getOrElse(null)
                  if (mdlDefs != null)
                    mdlId = mdlDefs.head.uniqueId

                  modelEvent.modelid = mdlId
                  modelEvent.eventepochtime = System.currentTimeMillis()
                  tempModelAB.append(modelEvent)
                } else {
                  LOG.error("Failed to create model " + md.mdl.getModelName())
                }
              } else {
                /** message was not interesting to md... */
              }
            } catch {
              case e: Exception => {
                val st = StackTrace.ThrowableTraceString(e)
                msgEvent.error = "Model Failed: \n" + st
                var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
                // TODO:  Do something with these events (not the msgEvent)
                LOG.error("Model Failed => " + md.mdl.getModelName(), e)
              }
              case t: Throwable => {
                val st = StackTrace.ThrowableTraceString(t)
                msgEvent.error = "Model Failed: \n" + st
                var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
                // TODO:  Do something with these events (not the msgEvent)
                LOG.error("Model Failed => " + md.mdl.getModelName(), t)
              }
            }
          })
        } catch {
          case e: Exception => {
            val st = StackTrace.ThrowableTraceString(e)
            msgEvent.error = "Failed to execute models.: \n" + st
            var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
            // TODO:  Do something with these events (not the msgEvent)
            LOG.error("Failed to execute models.", e)
          }
          case t: Throwable => {
            val st = StackTrace.ThrowableTraceString(t)
            msgEvent.error = "Failed to execute models.: \n" + st
            var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
            // TODO:  Do something with these events (not the msgEvent)
            LOG.error("Failed to execute models.", t)
          }
        } finally {
          ThreadLocalStorage.txnContextInfo.remove
        }
      }
      msgEvent.modelinfo = if (tempModelAB.isEmpty) new Array[KamanjaModelEvent](0) else tempModelAB.toArray[KamanjaModelEvent]
      // return results.toArray
    }

  /*
    private def GetTopMsgName(msgName: String): (String, Boolean, MsgContainerObjAndTransformInfo) = {
      val topMsgInfo = KamanjaMetadata.getMessgeInfo(msgName)
      if (topMsgInfo == null || topMsgInfo.parents.size == 0) return (msgName, false, null)
      (topMsgInfo.parents(0)._1, true, topMsgInfo)
    }
  */


    // Returns Adapter/Queue Name, Partition Key & Output String
    def execute(transId: Long, inputData: Array[Byte], msgType: String, msgInfo: MsgContainerObjAndTransformInfo, inputdata: InputData, txnCtxt: TransactionContext, readTmNs: Long, rdTmMs: Long, uk: String, uv: String): Array[(String, String, String)] = {
      // LOG.debug("LE => " + msgData)
      if (LOG.isDebugEnabled)
      LOG.debug("Processing uniqueKey:%s, uniqueVal:%s".format(uk, uv))
      val returnOutput = ArrayBuffer[(String, String, String)]() // Adapter/Queue name, PartitionKey & output message

      var isValidMsg = false
      var msg: MessageInterface = null
      var createdNewMsg = false
      var isValidPartitionKey = false
      var partKeyDataList: List[String] = null


      // Initialize Event message
      var msgEvent: KamanjaMessageEvent = messageEventFactory.createInstance.asInstanceOf[KamanjaMessageEvent]
      msgEvent.elapsedtimeinms = -1
      msgEvent.messagekey = uk
      msgEvent.messagevalue = uv


      try {
  /*
        if (msgInfo != null && inputdata != null) {
          val partKeyData = msgInfo.contmsgobj.asInstanceOf[MessageFactoryInterface].PartitionKeyData(inputdata) else null
          isValidPartitionKey = (partKeyData != null && partKeyData.size > 0)
          partKeyDataList = if (isValidPartitionKey) partKeyData.toList else null
          val primaryKey = if (isValidPartitionKey) msgInfo.contmsgobj.asInstanceOf[MessageFactoryInterface].PrimaryKeyData(inputdata) else null
          val primaryKeyList = if (primaryKey != null && primaryKey.size > 0) primaryKey.toList else null
          if (isValidPartitionKey && primaryKeyList != null) {
            try {
              val fndmsg = txnCtxt.getNodeCtxt.getEnvCtxt.getObject(transId, msgType, partKeyDataList, primaryKeyList)
              if (fndmsg != null) {
                msg = fndmsg.asInstanceOf[MessageInterface]
                LOG.debug("Found %s message for given partitionkey:%s, primarykey:%s. Msg partitionkey:%s, primarykey:%s".format(msgType, if (partKeyDataList != null) partKeyDataList.mkString(",") else "", if (primaryKeyList != null) primaryKeyList.mkString(",") else "", msg.PartitionKeyData.mkString(","), msg.PrimaryKeyData.mkString(",")))
              } else {
                LOG.debug("Not Found %s message for given partitionkey:%s, primarykey:%s.".format(msgType, if (partKeyDataList != null) partKeyDataList.mkString(",") else "", if (primaryKeyList != null) primaryKeyList.mkString(",") else ""))
              }
            } catch {
              // Treating we did not find the message
              case e: Exception => {
                LOG.warn("", e)
                val st = StackTrace.ThrowableTraceString(e)
                msgEvent.error = "Exception during input message processing: \n " + st
                var eEvent = createExceptionEvent(LeanringEngine.invalidMessage, LeanringEngine.engineComponent, st)
                //TODO: do somethign with this event
              }
              case e: Throwable => {
                LOG.warn("", e)
                val st = StackTrace.ThrowableTraceString(e)
                msgEvent.error = "Exception during input message processing: \n " + st
                var eEvent = createExceptionEvent(LeanringEngine.invalidMessage, LeanringEngine.engineComponent, st)
                //TODO: do somethign with this event
              }
            }
          }
          if (msg == null) {
            createdNewMsg = true
            msg = msgInfo.contmsgobj.asInstanceOf[MessageFactoryInterface].CreateNewMessage
          }
          msg.populate(inputdata)
          isValidMsg = true
        } else {
          msgEvent.error = "Recieved null message object for input"
          var eEvent = createExceptionEvent(LeanringEngine.invalidMessage, LeanringEngine.engineComponent,"Recieved null message object for input:" + inputdata.dataInput )
          LOG.error("Recieved null message object for input:" + inputdata.dataInput)
          // TODO:  Do something with these eEvents
        }
  */
      } catch {
        case e: Exception => {
          var kEx =  MessagePopulationException("Failed to Populate message", e)
          val st = StackTrace.ThrowableTraceString(kEx)
          msgEvent.error = "Failed to Populate message: \n" + st
          var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
          // TODO:  Do something with these eEvents
          throw kEx
        }
        case e: Throwable => {
          var kEx =  MessagePopulationException("Failed to Populate message", e)
          val st = StackTrace.ThrowableTraceString(kEx)
          msgEvent.error = "Failed to Populate message: \n" + st
          var eEvent = createExceptionEvent(LeanringEngine.modelExecutionException, LeanringEngine.engineComponent, st, txnCtxt)
          // TODO:  Do something with eEvent
          throw kEx
        }
      }

      try {
        // Ok, we have a message here, record some metadata regarding this message.
        var msgId: Long = -1
        // Figure out its reporting ID
        var msgDefs = KamanjaMetadata.getMdMgr.Messages(msgType,true, false).getOrElse(null)
        if (msgDefs != null)
          msgId = msgDefs.head.uniqueId
        msgEvent.messageid = msgId
      } catch {
        // If we are hitting this path.. something is really screwed up, but tolerate the error.
        case e: Throwable => {
          LOG.error("Unable to find message " + msgType + " in KmanajaMetadata... resolve the problem", e)
        }
      }

      try {
  //      if (isValidMsg) {
  //        var allMdlsResults: scala.collection.mutable.Map[String, SavedMdlResult] = null
  //        if (allMdlsResults == null)
  //          allMdlsResults = scala.collection.mutable.Map[String, SavedMdlResult]()
  //        // Run all models
  //        val mdlsStartTime = System.nanoTime
  //        val results = RunAllModels(transId, inputData, msg, txnCtxt, uk, uv, msgEvent)
  //        LOG.info(ManagerUtils.getComponentElapsedTimeStr("Models", uv, readTmNs, mdlsStartTime))
  //        msgEvent.elapsedtimeinms = ((System.nanoTime - mdlsStartTime)/ 1000000.0).toFloat
  //        if (results.size > 0) {
  //          var elapseTmFromRead = (System.nanoTime - readTmNs) / 1000
  //
  //          if (elapseTmFromRead < 0)
  //            elapseTmFromRead = 1
  //
  //          try {
  //            // Prepare final output and update the models persistance map
  //            results.foreach(res => {
  //              allMdlsResults(res.mdlName) = res
  //            })
  //          } catch {
  //            case e: Exception => {
  //              LOG.error("Failed to get Model results.", e)
  //              val st = StackTrace.ThrowableTraceString(e)
  //              var eEvent = createExceptionEvent(LeanringEngine.invalidResult, LeanringEngine.engineComponent, st)
  //              // TODO: Do something with these events
  //            }
  //          }
  //          val resMap = scala.collection.mutable.Map[String, Array[(String, Any)]]()
  //
  //          results.map(res => {
  //            resMap(res.mdlName) = res.mdlRes.asKeyValuesMap.map(r => {
  //              (r._1, r._2)
  //            }).toArray
  //          })
  //
  //          val json = ("ModelsResult" -> results.toList.map(res => res.toJson))
  //          // returnOutput ++= allOutputQueueNames.map(adapNm => (adapNm, cntr.toString, compact(render(json)))) // Sending the same result to all queues
  //          // cntr += 1
  //        }
  //      }
        return returnOutput.toArray
      } catch {
        case e: Exception => {
          val st = StackTrace.ThrowableTraceString(e)
          msgEvent.error = "Failed to execute models after creating message: \n" + st
          var eEvent = createExceptionEvent(LeanringEngine.invalidMessage, LeanringEngine.engineComponent, st, txnCtxt)
          // TODO:  Do something with these events
          LOG.error("Failed to execute models after creating message", e)
        }
        case e: Throwable => {
          val st = StackTrace.ThrowableTraceString(e)
          msgEvent.error = "Failed to execute models after creating message: \n " + st
          var eEvent = createExceptionEvent(LeanringEngine.invalidMessage, LeanringEngine.engineComponent, st, txnCtxt)
          // TODO:  Do something with these events
          LOG.error("Failed to execute models after creating message", e)
        }
      }
      return Array[(String, String, String)]()
    } */
}
