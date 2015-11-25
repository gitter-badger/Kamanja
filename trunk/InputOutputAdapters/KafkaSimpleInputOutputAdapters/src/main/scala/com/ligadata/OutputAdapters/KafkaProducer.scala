
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

import com.ligadata.AdaptersConfiguration.{ KafkaConstants, KafkaQueueAdapterConfiguration }
import com.ligadata.Exceptions.FatalAdapterException
import com.ligadata.InputOutputAdapterInfo.{ AdapterConfiguration, OutputAdapter, OutputAdapterObj, CountersAdapter }
import com.typesafe.config.ConfigFactory
import java.util.{ Properties, Arrays }
import kafka.common.{ QueueFullException, FailedToSendMessageException }
import kafka.message._
import kafka.producer.{ ProducerConfig, Producer, KeyedMessage, Partitioner }
import kafka.utils.VerifiableProperties
import org.apache.log4j.Logger
import scala.collection.mutable.ArrayBuffer

object KafkaProducer extends OutputAdapterObj {
  def CreateOutputAdapter(inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter): OutputAdapter = new KafkaProducer(inputConfig, cntrAdapter)
}

/**
 * Handles Strings, Array[Byte], Int, and Long
 * @param props
 */
class CustPartitioner(props: VerifiableProperties) extends Partitioner {
  private val random = new java.util.Random

  def partition(key: Any, numPartitions: Int): Int = {
    if (key != null) {
      key.asInstanceOf[Any] match {
        case _: Array[Byte] => scala.math.abs(Arrays.hashCode(key.asInstanceOf[Array[Byte]])) % numPartitions
        case _: String => key.asInstanceOf[String].hashCode() % numPartitions
        case _: Int => key.asInstanceOf[Int] % numPartitions
        case _: Long => (key.asInstanceOf[Long] % numPartitions).asInstanceOf[Int]
        case _ => throw new Exception()
      }
    }
    random.nextInt(numPartitions)
  }
}

class KafkaProducer(val inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter) extends OutputAdapter {
  private[this] val LOG = Logger.getLogger(getClass)

  //BUGBUG:: Not Checking whether inputConfig is really QueueAdapterConfiguration or not. 
  private[this] val qc = KafkaQueueAdapterConfiguration.GetAdapterConfig(inputConfig)

  val clientId = qc.Name + "_" + hashCode.toString

  val config = ConfigFactory.load()
  val compress: Boolean = config.getBoolean("kafkaProducerDefaults.compress")
  val synchronously: Boolean = config.getBoolean("kafkaProducerDefaults.synchronously")
  val batchSize: Integer = config.getInt("kafkaProducerDefaults.batchSize")
  val queueTime: Integer = config.getInt("kafkaProducerDefaults.queueTime")
  val bufferMemory: Integer = config.getInt("kafkaProducerDefaults.bufferMemory_MB") * 1024 * 1024
  val messageSendMaxRetries: Integer = config.getInt("kafkaProducerDefaults.messageSendMaxRetries")
  val requestRequiredAcks: Integer = config.getInt("kafkaProducerDefaults.requestRequiredAcks")
  val MAX_RETRY = config.getInt("kafkaProducerDefaults.MAX_RETRY")
  val ackTimeout = config.getInt("kafkaProducerDefaults.ackTimeout")

  val codec = if (compress) DefaultCompressionCodec.codec else NoCompressionCodec.codec

  val props = new Properties()
  props.put("compression.codec", if (qc.otherConfigs.contains("compression.codec")) qc.otherConfigs.getOrElse("compression.codec", "").toString else codec.toString)
  props.put("producer.type", if (qc.otherConfigs.contains("producer.type")) qc.otherConfigs.getOrElse("producer.type", "sync").toString else if (synchronously) "sync" else "async")
  props.put("metadata.broker.list", qc.hosts.mkString(","))
  props.put("batch.num.messages", if (qc.otherConfigs.contains("batch.num.messages")) qc.otherConfigs.getOrElse("batch.num.messages", "1024").toString else batchSize.toString)
  props.put("queue.buffering.max.messages", if (qc.otherConfigs.contains("queue.buffering.max.messages")) qc.otherConfigs.getOrElse("queue.buffering.max.messages", "1024").toString else batchSize.toString)
  props.put("queue.buffering.max.ms", if (qc.otherConfigs.contains("queue.buffering.max.ms")) qc.otherConfigs.getOrElse("queue.buffering.max.ms", "100").toString else queueTime.toString)
  props.put("message.send.max.retries", if (qc.otherConfigs.contains("message.send.max.retries")) qc.otherConfigs.getOrElse("message.send.max.retries", "1").toString else messageSendMaxRetries.toString)
  props.put("request.required.acks", if (qc.otherConfigs.contains("request.required.acks")) qc.otherConfigs.getOrElse("request.required.acks", "1").toString else requestRequiredAcks.toString)
  props.put("send.buffer.bytes", if (qc.otherConfigs.contains("send.buffer.bytes")) qc.otherConfigs.getOrElse("send.buffer.bytes", "1024000").toString else bufferMemory.toString)
  props.put("request.timeout.ms", if (qc.otherConfigs.contains("request.timeout.ms")) qc.otherConfigs.getOrElse("request.timeout.ms", "10000").toString else ackTimeout.toString)
  props.put("client.id", if (qc.otherConfigs.contains("client.id")) qc.otherConfigs.getOrElse("client.id", "kamanja").toString else clientId)

  val tmpProducersStr = qc.otherConfigs.getOrElse("numberofconcurrentproducers", "1").toString.trim()
  var producersCnt = 1
  if (tmpProducersStr.isEmpty) {
    try {
      val tmpProducers = tmpProducersStr.toInt
      if (tmpProducers > 0)
        producersCnt = tmpProducers
    } catch {
      case e: Exception => {}
      case e: Throwable => {}
    }
  }

  var reqCntr: Int = 0

  val producers = new Array[Producer[Array[Byte], Array[Byte]]](producersCnt)

  for (i <- 0 until producersCnt) {
    LOG.info("Creating Producer:" + (i + 1))
    producers(i) = new Producer[Array[Byte], Array[Byte]](new ProducerConfig(props))
  }

  // To send an array of messages. messages.size should be same as partKeys.size
  override def send(messages: Array[Array[Byte]], partKeys: Array[Array[Byte]]): Unit = {
    if (messages.length != partKeys.length) {
      val szMsg = "KAFKA PRODUCER: Message and Partition Keys should has same number of elements. Message has %d and Partition Keys has %d".format(messages.length, partKeys.length)
      LOG.error(szMsg)
      throw new Exception(szMsg)
    }
    if (messages.isEmpty) return
    try {
      val keyMessages = new ArrayBuffer[KeyedMessage[Array[Byte], Array[Byte]]](messages.length)
      for (i <- messages.indices) {
        keyMessages += new KeyedMessage(qc.topic, partKeys(i), messages(i))
      }

      if (reqCntr > 500000000)
        reqCntr = 0
      reqCntr += 1
      val cntr = reqCntr

      var sendStatus = KafkaConstants.KAFKA_NOT_SEND
      var retryCount = 0
      while (sendStatus != KafkaConstants.KAFKA_SEND_SUCCESS) {
        val result = doSend(cntr, keyMessages)
        sendStatus = result._1
        // Queue is full, wait and retry
        if (sendStatus == KafkaConstants.KAFKA_SEND_Q_FULL) {
          LOG.warn("KAFKA PRODUCER: " + qc.topic + " is temporarily full, retrying.")
          Thread.sleep(1000)
        }

        // Something wrong in sending messages,  Producer will handle internal failover, so we want to retry but only
        //  3 times.
        if (sendStatus == KafkaConstants.KAFKA_SEND_DEAD_PRODUCER) {
          retryCount += 1
          if (retryCount < MAX_RETRY) {
            LOG.warn("KAFKA PRODUCER: Error sending to kafka, Retrying " + retryCount + "/" + MAX_RETRY)
          } else {
            LOG.error("KAFKA PRODUCER: Error sending to kafka,  MAX_RETRY reached... shutting down")
            throw new FatalAdapterException("Unable to send to Kafka, MAX_RETRY reached", result._2.orNull)
          }
        }
      }

      val key = Category + "/" + qc.Name + "/evtCnt"
      cntrAdapter.addCntr(key, messages.length) // for now adding rows
    } catch {
      case fae: FatalAdapterException => throw fae
      case e: Exception               => throw new FatalAdapterException("Unknown exception", e)
    }
  }

  private def doSend(cntr: Int, keyMessages: ArrayBuffer[KeyedMessage[Array[Byte], Array[Byte]]]): (Int, Option[Exception]) = {

    try {
      producers(cntr % producersCnt).send(keyMessages: _*) // Thinking this op is atomic for (can write multiple partitions into one queue, but don't know whether it is atomic per partition in the queue).
    } catch {
      case ftsme: FailedToSendMessageException => return (KafkaConstants.KAFKA_SEND_DEAD_PRODUCER, Some(ftsme))
      case qfe: QueueFullException             => return (KafkaConstants.KAFKA_SEND_Q_FULL, None)
      case e: Exception                        => throw new FatalAdapterException("Unknown exception", e)
    }
    (KafkaConstants.KAFKA_SEND_SUCCESS, None)
  }

  override def Shutdown(): Unit = {
    for (i <- 0 until producersCnt) {
      if (producers(i) != null)
        producers(i).close()
      producers(i) = null

    }
  }
}