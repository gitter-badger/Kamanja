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

package com.ligadata.OutputAdapters

import com.ligadata.AdaptersConfiguration.IbmMqAdapterConfiguration
import com.ligadata.Exceptions.StackTrace
import com.ligadata.InputOutputAdapterInfo.{ AdapterConfiguration, OutputAdapter, OutputAdapterObj, CountersAdapter }
import com.ibm.msg.client.jms.JmsConstants
import com.ibm.msg.client.jms.JmsFactoryFactory
import com.ibm.msg.client.wmq.common.CommonConstants
import javax.jms.{ Connection, Destination, JMSException, MessageProducer, Session }
import org.apache.log4j.Logger

object IbmMqProducer extends OutputAdapterObj {
  def CreateOutputAdapter(inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter): OutputAdapter = new IbmMqProducer(inputConfig, cntrAdapter)
}

class IbmMqProducer(val inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter) extends OutputAdapter {
  private[this] val LOG = Logger.getLogger(getClass)

  //BUGBUG:: Not Checking whether inputConfig is really QueueAdapterConfiguration or not. 
  private[this] val qc = IbmMqAdapterConfiguration.GetAdapterConfig(inputConfig)

  private def printFailure(ex: Exception) {
    if (ex != null) {
      ex match {
        case _: JMSException => processJMSException(ex.asInstanceOf[JMSException])
        case _ => LOG.error(ex)
      }
    }
  }

  private def processJMSException(jmsex: JMSException) {
    LOG.error(jmsex)
    var innerException: Throwable = jmsex.getLinkedException
    if (innerException != null) {
      LOG.error("Inner exception(s):")
    }
    while (innerException != null) {
      LOG.error(innerException)
      innerException = innerException.getCause
    }
  }

  var connection: Connection = null
  var session: Session = null
  var destination: Destination = null
  var producer: MessageProducer = null

  try {
    val ff = JmsFactoryFactory.getInstance(JmsConstants.WMQ_PROVIDER)
    val cf = ff.createConnectionFactory()
    cf.setStringProperty(CommonConstants.WMQ_HOST_NAME, qc.host_name)
    cf.setIntProperty(CommonConstants.WMQ_PORT, qc.port)
    cf.setStringProperty(CommonConstants.WMQ_CHANNEL, qc.channel)
    cf.setIntProperty(CommonConstants.WMQ_CONNECTION_MODE, qc.connection_mode)
    cf.setStringProperty(CommonConstants.WMQ_QUEUE_MANAGER, qc.queue_manager)
    if (qc.ssl_cipher_suite.nonEmpty)
      cf.setStringProperty(CommonConstants.WMQ_SSL_CIPHER_SUITE, qc.ssl_cipher_suite)
    // cf.setStringProperty(WMQConstants.WMQ_APPLICATIONNAME, qc.application_name)
    connection = cf.createConnection()
    session = connection.createSession(false, Session.AUTO_ACKNOWLEDGE)
    if (qc.queue_name != null && qc.queue_name.nonEmpty)
      destination = session.createQueue(qc.queue_name)
    else if (qc.topic_name != null && qc.topic_name.nonEmpty)
      destination = session.createTopic(qc.topic_name)
    else {
      // Throw error
    }
    producer = session.createProducer(destination)
    connection.start()
  } catch {
    case jmsex: Exception => {
      printFailure(jmsex)
      val stackTrace = StackTrace.ThrowableTraceString(jmsex)
      LOG.debug("StackTrace:" + stackTrace)
    }
  }

  // To send an array of messages. messages.size should be same as partKeys.size
  override def send(messages: Array[Array[Byte]], partKeys: Array[Array[Byte]]): Unit = {
    if (messages.length != partKeys.length) {
      LOG.error("Message and Partition Keys hould has same number of elements. Message has %d and Partition Keys has %d".format(messages.length, partKeys.length))
      return
    }
    if (messages.isEmpty) return

    try {
      // Op is not atomic
      messages.foreach(message => {
        // Do we need text Message or Bytes Message?
        if (qc.msgType == com.ligadata.AdaptersConfiguration.MessageType.fByteArray) {
          val outMessage = session.createBytesMessage()
          outMessage.writeBytes(message)
          outMessage.setStringProperty("ContentType", qc.content_type)
          producer.send(outMessage)
        } else { // By default we are taking (qc.msgType == com.ligadata.AdaptersConfiguration.MessageType.fText)
          val outMessage = session.createTextMessage(new String(message))
          outMessage.setStringProperty("ContentType", qc.content_type)
          producer.send(outMessage)
        }
        val key = Category + "/" + qc.Name + "/evtCnt"
        cntrAdapter.addCntr(key, 1) // for now adding each row
      })
    } catch {
      case jmsex: Exception => {
        printFailure(jmsex)
        val stackTrace = StackTrace.ThrowableTraceString(jmsex)
        LOG.debug("StackTrace:" + stackTrace)
      }
    }
  }

  override def Shutdown(): Unit = {
    if (producer != null) {
      try {
        producer.close()
      } catch {
        case jmsex: Exception => {
          LOG.error("Producer could not be closed.")
          printFailure(jmsex)
        }
      }
    }

    // Do we need to close destination ??

    if (session != null) {
      try {
        session.close()
      } catch {
        case jmsex: Exception => {
          LOG.error("Session could not be closed.")
          printFailure(jmsex)
        }
      }
    }
    if (connection != null) {
      try {
        connection.close()
      } catch {
        case jmsex: Exception => {
          LOG.error("Connection could not be closed.")
          printFailure(jmsex)
        }
      }
    }
  }
}
