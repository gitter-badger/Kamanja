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

package com.ligadata.AdaptersConfiguration

import com.ligadata.InputOutputAdapterInfo.{ AdapterConfiguration, PartitionUniqueRecordKey, PartitionUniqueRecordValue }
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import javax.jms.Connection
import javax.jms.Destination
import javax.jms.JMSException
import javax.jms.MessageProducer
import javax.jms.Session
import javax.jms.TextMessage
import com.ibm.msg.client.jms.JmsConnectionFactory
import com.ibm.msg.client.jms.JmsFactoryFactory
import com.ibm.msg.client.wmq.WMQConstants
import com.ibm.msg.client.wmq.common.CommonConstants
import com.ibm.msg.client.jms.JmsConstants

object MessageType extends Enumeration {
  type MsgType = Value
  val fText, fByteArray = Value
}
import MessageType._

class IbmMqAdapterConfiguration extends AdapterConfiguration {
  var host_name: String = _
  var port: Int = 1414
  var channel: String = _
  var connection_mode: Int = CommonConstants.WMQ_CM_CLIENT
  var queue_manager: String = _
  var application_name: String = _
  var queue_name: String = "" // queue name
  var topic_name: String = "" // topic name
  var content_type: String = "application/json" // default content type for MQ
  var ssl_cipher_suite: String = "" // ssl_cipher_suite
  var msgType = MessageType.fText // Default is Text message
}

object IbmMqAdapterConfiguration {
  def GetAdapterConfig(inputConfig: AdapterConfiguration): IbmMqAdapterConfiguration = {
    if (inputConfig.adapterSpecificCfg == null || inputConfig.adapterSpecificCfg.size == 0) {
      val err = "Not found IBM MQ information for Adapter Config:" + inputConfig.Name
      throw new Exception(err)
    }

    val qc = new IbmMqAdapterConfiguration
    qc.Name = inputConfig.Name
    qc.formatName = inputConfig.formatName
    qc.validateAdapterName = inputConfig.validateAdapterName
    qc.failedEventsAdapterName = inputConfig.failedEventsAdapterName
    qc.className = inputConfig.className
    qc.jarName = inputConfig.jarName
    qc.dependencyJars = inputConfig.dependencyJars
    qc.associatedMsg = if (inputConfig.associatedMsg == null) null else inputConfig.associatedMsg.trim
    qc.keyAndValueDelimiter = if (inputConfig.keyAndValueDelimiter == null) null else inputConfig.keyAndValueDelimiter.trim
    qc.fieldDelimiter = if (inputConfig.fieldDelimiter == null) null else inputConfig.fieldDelimiter.trim
    qc.valueDelimiter = if (inputConfig.valueDelimiter == null) null else inputConfig.valueDelimiter.trim

    val adapCfg = parse(inputConfig.adapterSpecificCfg)
    if (adapCfg == null || adapCfg.values == null) {
      val err = "Not found IBM MQ information for Adapter Config:" + inputConfig.Name
      throw new Exception(err)
    }
    val values = adapCfg.values.asInstanceOf[Map[String, String]]

    values.foreach(kv => {
      if (kv._1.compareToIgnoreCase("host_name") == 0) {
        qc.host_name = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("port") == 0) {
        qc.port = kv._2.trim.toInt
      } else if (kv._1.compareToIgnoreCase("channel") == 0) {
        qc.channel = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("queue_manager") == 0) {
        qc.queue_manager = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("application_name") == 0) {
        qc.application_name = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("queue_name") == 0) {
        qc.queue_name = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("topic_name") == 0) {
        qc.topic_name = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("content_type") == 0) {
        qc.content_type = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("ssl_cipher_suite") == 0) {
        qc.ssl_cipher_suite = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("message_type") == 0) {
        val typ = kv._2.trim.toLowerCase
        typ match {
          case "text" => { qc.msgType = MessageType.fText }
          case "bytearray" => { qc.msgType = MessageType.fByteArray }
          case _ => { qc.msgType = MessageType.fText /* Default is Text message */ }
        }
      }
    })

    qc
  }
}

case class IbmMqKeyData(Version: Int, Type: String, Name: String, QueueManagerName: Option[String], ChannelName: Option[String], QueueName: Option[String], TopicName: Option[String]) // Using most of the values as optional values. Just thinking about future changes. Don't know the performance issues.

class IbmMqPartitionUniqueRecordKey extends PartitionUniqueRecordKey {
  val Version: Int = 1
  val Type: String = "IbmMq"
  var Name: String = _ // Name
  var QueueManagerName: String = _
  var ChannelName: String = _
  var QueueName: String = ""
  var TopicName: String = ""

  override def Serialize: String = { // Making String from key
    val json =
      ("Version" -> Version) ~
        ("Type" -> Type) ~
        ("Name" -> Name) ~
        ("QueueManagerName" -> QueueManagerName) ~
        ("ChannelName" -> ChannelName) ~
        ("QueueName" -> QueueName) ~
        ("TopicName" -> TopicName)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = { // Making Key from Serialized String
    implicit val jsonFormats: Formats = DefaultFormats
    val keyData = parse(key).extract[IbmMqKeyData]
    if (keyData.Version == Version && keyData.Type.compareTo(Type) == 0) {
      Name = keyData.Name
      QueueManagerName = keyData.QueueManagerName.get
      ChannelName = keyData.ChannelName.get
      QueueName = keyData.QueueName.get
      TopicName = keyData.TopicName.get
    }
    // else { } // Not yet handling other versions
  }
}

case class IbmMqRecData(Version: Int, MessageId: Option[String]) // Using most of the values as optional values. Just thinking about future changes. Don't know the performance issues.

class IbmMqPartitionUniqueRecordValue extends PartitionUniqueRecordValue {
  val Version: Int = 1
  var MessageId: String = "" // MessageId

  override def Serialize: String = { // Making String from Value
    val json =
      ("Version" -> Version) ~
        ("MessageId" -> MessageId)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = { // Making Value from Serialized String
    implicit val jsonFormats: Formats = DefaultFormats
    val recData = parse(key).extract[IbmMqRecData]
    if (recData.Version == Version) {
      MessageId = recData.MessageId.get
    }
    // else { } // Not yet handling other versions
  }
}

