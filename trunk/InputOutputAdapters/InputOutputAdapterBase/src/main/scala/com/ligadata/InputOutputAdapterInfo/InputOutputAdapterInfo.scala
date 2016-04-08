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

package com.ligadata.InputOutputAdapterInfo

import com.ligadata.Exceptions.{KamanjaException, StackTrace}
import com.ligadata.KamanjaBase._
import com.ligadata.HeartBeat._
import com.ligadata.transactions.{NodeLevelTransService, SimpleTransService}

//import org.json4s._
//import org.json4s.JsonDSL._
//import org.json4s.jackson.JsonMethods._
import org.apache.logging.log4j.{ Logger, LogManager }

import scala.collection.mutable.ArrayBuffer

object AdapterConfiguration {
  val TYPE_INPUT = "Input_Adapter"
  val TYPE_OUTPUT = "Output_Adapter"
}

class AdapterConfiguration {
  // Name of the Adapter, KafkaQueue Name/MQ Name/File Adapter Logical Name/etc
  var Name: String = _
//  // CSV/JSON/XML for input adapter.
//  var formatName: String = _
//  // For output adapter it is just corresponding validate adapter name.
//  var validateAdapterName: String = _
//  // For input adapter it is just corresponding failed events adapter name.
//  var failedEventsAdapterName: String = _
//  // Queue Associated Message
//  var associatedMsg: String = _
  // Class where the Adapter can be loaded (Object derived from InputAdapterObj)
  var className: String = _
  // Jar where the className can be found
  var jarName: String = _
  // All dependency Jars for jarName
  var dependencyJars: Set[String] = _
  // adapter specific (mostly json) string
  var adapterSpecificCfg: String = _
  var tenantId: String = _
//  // Delimiter String for keyAndValueDelimiter
//  var keyAndValueDelimiter: String = _
//  // Delimiter String for fieldDelimiter
//  var fieldDelimiter: String = _
//  var valueDelimiter: String = _ // Delimiter String for valueDelimiter
}

// Input Adapter Object to create Adapter
trait InputAdapterFactory {
  def CreateInputAdapter(inputConfig: AdapterConfiguration, execCtxtObj: ExecContextFactory, nodeContext: NodeContext): InputAdapter
}

class StartProcPartInfo {
  var _key: PartitionUniqueRecordKey = null
  var _val: PartitionUniqueRecordValue = null
  var _validateInfoVal: PartitionUniqueRecordValue = null
}

// Input Adapter
trait InputAdapter extends Monitorable {
  val nodeContext: NodeContext
  // NodeContext
  val inputConfig: AdapterConfiguration // Configuration

  def UniqueName: String = {
    // Making String from key
    return "{\"Name\" : \"%s\"}".format(inputConfig.Name)
  }

  def Category = "Input"

  def Shutdown: Unit

  def StopProcessing: Unit

  def StartProcessing(partitionInfo: Array[StartProcPartInfo], ignoreFirstMsg: Boolean): Unit

  // each value in partitionInfo is (PartitionUniqueRecordKey, PartitionUniqueRecordValue, Long, PartitionUniqueRecordValue). // key, processed value, Start transactionid, Ignore Output Till given Value (Which is written into Output Adapter) & processing Transformed messages (processing & total)
  def GetAllPartitionUniqueRecordKey: Array[PartitionUniqueRecordKey]

  def DeserializeKey(k: String): PartitionUniqueRecordKey

  def DeserializeValue(v: String): PartitionUniqueRecordValue

  def getAllPartitionBeginValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]

  def getAllPartitionEndValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]
}

// Output Adapter Object to create Adapter
trait OutputAdapterFactory {
  def CreateOutputAdapter(inputConfig: AdapterConfiguration, nodeContext: NodeContext): OutputAdapter
}

// Output Adapter
trait OutputAdapter extends AdaptersSerializeDeserializers with Monitorable {
  // NodeContext
  val nodeContext: NodeContext
  // Configuration
  val inputConfig: AdapterConfiguration

  def send(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface]): Unit = {
    val (outContainers, serializedContainerData, serializerNames) = serialize(tnxCtxt, outputContainers)
    send(tnxCtxt, outContainers, serializedContainerData, serializerNames)
  }

  // This is protected override method. After applying serialization, pass original messages, Serialized data & Serializer names
  protected def send(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface], serializedContainerData: Array[Array[Byte]], serializerNames: Array[String]): Unit

  def Shutdown: Unit

  def Category = "Output"
}

trait ExecContext extends AdaptersSerializeDeserializers {
  val input: InputAdapter
  val curPartitionKey: PartitionUniqueRecordKey
  val nodeContext: NodeContext

  private val LOG = LogManager.getLogger(getClass);
  private val failedEventDtFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ")

  if (nodeContext != null && nodeContext.getEnvCtxt() != null) {
    if (! nodeContext.getEnvCtxt().hasZkConnectionString)
      throw new KamanjaException("Zookeeper information is not yet set", null)
  } else {
    throw new KamanjaException("Not found NodeContext or EnvContext", null)
  }

  val (zkConnectString, zkNodeBasePath, zkSessionTimeoutMs, zkConnectionTimeoutMs)  = nodeContext.getEnvCtxt().getZookeeperInfo
  val (txnIdsRangeForPartition, txnIdsRangeForNode)  = nodeContext.getEnvCtxt().getTransactionRanges

  NodeLevelTransService.init(zkConnectString, zkSessionTimeoutMs, zkConnectionTimeoutMs, zkNodeBasePath, txnIdsRangeForNode,
    nodeContext.getEnvCtxt().getDefaultDatastoreForTenantId(input.inputConfig.tenantId), nodeContext.getEnvCtxt().getJarPaths())
  private val transService = new SimpleTransService
  transService.init(txnIdsRangeForPartition)

  private def SendFailedEvent(data: Array[Byte], deserializer: String, failedMsg: String, uk: String, uv: String, e: Throwable): Unit = {
    val failedTm = failedEventDtFormat.format(new java.util.Date(System.currentTimeMillis))
    val evntData = new String(data)

    val failMsg = if (e != null) e.getMessage else ""
    val stackTrace = if (e != null) StackTrace.ThrowableTraceString(e) else ""

    if (nodeContext != null && nodeContext.getEnvCtxt() != null) {
      // getInstance of Failed Event
      val msgType = "System.FailedEvents"
      val failEventPartInfo = "System.FailedEventsPartitionKeyValue"
      val failEventFailure = "System.FailedEventsMessageInfo"
      try {
        val failureInfo = nodeContext.getEnvCtxt().getContainerInstance(failEventFailure)
        val partInfo = nodeContext.getEnvCtxt().getContainerInstance(failEventPartInfo)
        val msg = nodeContext.getEnvCtxt().getContainerInstance(msgType)
        if (msg != null) {
          try {
            partInfo.set("Key", uk)
            partInfo.set("Value", uv)

            failureInfo.set("Message", failMsg)
            failureInfo.set("StackTrace", stackTrace)

            msg.set("MessageType", failedMsg)
            msg.set("Deserializer", deserializer)
            msg.set("SourceAdapter", input.inputConfig.Name)
            msg.set("FailedAt", failedTm)
            msg.set("EventData", evntData)
            msg.set("Partition", partInfo)
            msg.set("Failure", failureInfo)

            nodeContext.getEnvCtxt().postMessages(Array(msg))
          } catch {
            case e: Throwable => {
              LOG.error("ailed to post message of type:" + msgType, e)
            }
          }
        }
      } catch {
        case e: Throwable => {
          LOG.error("Failed to create message for type:" + msgType, e)
        }
      }
    }
  }

  final def execute(data: Array[Byte], uniqueKey: PartitionUniqueRecordKey, uniqueVal: PartitionUniqueRecordValue, readTmMilliSecs: Long): Unit = {
    var msg: ContainerInterface = null
    var deserializerName: String = ""

    var uk = ""
    var uv = ""

    try {
      uk = if (uniqueKey != null) uniqueKey.Serialize else ""
      uv = if (uniqueVal != null) uniqueVal.Serialize else ""
    } catch {
      case e: Throwable => {
        LOG.error("Failed to serialize PartitionUniqueRecordKey and/or PartitionUniqueRecordValue", e)
      }
    }

    try {
      val (tMsg, tDeserializerName) = deserialize(data)
      msg = tMsg
      deserializerName = tDeserializerName
    } catch {
      case e: Throwable => {
        //FIXME:- Need to populate deserializer & failedMsg -- Begin
        val deserializer = ""
        val failedMsg = ""
        //FIXME:- Need to populate deserializer & failedMsg -- End
        SendFailedEvent(data, deserializer, failedMsg, uk, uv, e)
      }
    }

    try {
      val transId = transService.getNextTransId
      val msgEvent = nodeContext.getEnvCtxt().getContainerInstance("System.KamanjaMessageEvent")
      val txnCtxt = new TransactionContext(transId, nodeContext, data, EventOriginInfo(uk, uv), readTmMilliSecs, msgEvent)
      LOG.debug("Processing uniqueKey:%s, uniqueVal:%s, Datasize:%d".format(uk, uv, data.size))
      txnCtxt.setInitialMessage("", msg)
      executeMessage(txnCtxt, deserializerName): Unit
    } catch {
      case e: Throwable => {
        LOG.error("Failed to execute message : " + msg.getFullTypeName, e)
      }
    } finally {
      // Commit. Writing into OutputAdapters & Storage Adapters
      // nodeContext.getEnvCtxt().CommitData(txnCtxt);
    }
  }

  protected def executeMessage(txnCtxt: TransactionContext, deserializerName: String): Unit
}

trait ExecContextFactory {
  def CreateExecContext(input: InputAdapter, curPartitionKey: PartitionUniqueRecordKey, nodeContext: NodeContext): ExecContext
}

trait PartitionUniqueRecordKey {
  val Type: String

  // Type of the Key -- For now putting File/Kafka like that. This is mostly for readable purpose (for which adapter etc)
  def Serialize: String

  // Making String from key
  def Deserialize(key: String): Unit // Making Key from Serialized String
}

trait PartitionUniqueRecordValue {
  def Serialize: String

  // Making String from Value
  def Deserialize(key: String): Unit // Making Value from Serialized String
}


