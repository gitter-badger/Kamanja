
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

package com.ligadata.InputAdapters

import com.ligadata.AdaptersConfiguration.{ KafkaPartitionUniqueRecordKey, KafkaPartitionUniqueRecordValue, KafkaQueueAdapterConfiguration }
import com.ligadata.InputOutputAdapterInfo._
import kafka.api._
import kafka.common.TopicAndPartition
import org.json4s.jackson.Serialization
import scala.actors.threadpool.{ TimeUnit, ExecutorService, Executors }
import scala.util.control.Breaks._
import kafka.consumer.{ SimpleConsumer }
import java.net.{ InetAddress }
import org.apache.logging.log4j.{ Logger, LogManager }
import scala.collection.mutable.Map
import com.ligadata.Exceptions.{KamanjaException, FatalAdapterException}
import com.ligadata.KamanjaBase.{NodeContext, DataDelimiters}
import com.ligadata.HeartBeat.{Monitorable, MonitorComponentInfo}

case class ExceptionInfo (Last_Failure: String, Last_Recovery: String)

object KafkaSimpleConsumer extends InputAdapterFactory {
  val METADATA_REQUEST_CORR_ID = 2
  val QUEUE_FETCH_REQUEST_TYPE = 1
  val METADATA_REQUEST_TYPE = "metadataLookup"
  val MAX_FAILURES = 2
  val MONITOR_FREQUENCY = 10000 // Monitor Topic queues every 20 seconds
  val SLEEP_DURATION = 1000 // Allow 1 sec between unsucessful fetched
  var CURRENT_BROKER: String = _
  val FETCHSIZE = 1024 * 1024 // 1MB by default
  val ZOOKEEPER_CONNECTION_TIMEOUT_MS = 3000
  val MAX_TIMEOUT = 60000
  val INIT_TIMEOUT = 250
  val HB_PERIOD = 5000

  // Statistics Keys
  val ADAPTER_DESCRIPTION = "Kafka 0.8.2.2 Client"
  val PARTITION_COUNT_KEYS = "Partition Counts"
  val PARTITION_DEPTH_KEYS = "Partition Depths"
  val EXCEPTION_SUMMARY = "Exception Summary"

  def CreateInputAdapter(inputConfig: AdapterConfiguration, execCtxtObj: ExecContextFactory, nodeContext: NodeContext): InputAdapter = new KafkaSimpleConsumer(inputConfig, execCtxtObj, nodeContext)
}

class KafkaSimpleConsumer(val inputConfig: AdapterConfiguration, val execCtxtObj: ExecContextFactory, val nodeContext: NodeContext) extends InputAdapter {
  val input = this
  private val lock = new Object()
  private val LOG = LogManager.getLogger(getClass)
  private var isQuiesced = false
  private var startTime: Long = 0
  private var isShutdown = false

  private var metrics: collection.mutable.Map[String,Any] = collection.mutable.Map[String,Any]()
  private var partitonCounts: collection.mutable.Map[String,Long] = collection.mutable.Map[String,Long]()
  private var partitonDepths: collection.mutable.Map[String,Long] = collection.mutable.Map[String,Long]()
  private var partitionExceptions: collection.mutable.Map[String,ExceptionInfo] = collection.mutable.Map[String,ExceptionInfo]()
  private var msgInQ: Long = 0
  private var startHeartBeat: String = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var lastSeen: String = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  metrics(KafkaSimpleConsumer.PARTITION_COUNT_KEYS) = partitonCounts
  metrics(KafkaSimpleConsumer.EXCEPTION_SUMMARY) = partitionExceptions
  metrics(KafkaSimpleConsumer.PARTITION_DEPTH_KEYS) = partitonDepths

  var localReadOffsets: collection.mutable.Map[Int,Long] = collection.mutable.Map[Int,Long]()

  private val qc = KafkaQueueAdapterConfiguration.GetAdapterConfig(inputConfig)

  LOG.debug("KAFKA ADAPTER: allocating kafka adapter for " + qc.hosts.size + " broker hosts")

  private var numberOfErrors: Int = _
  private var replicaBrokers: Set[String] = Set()
  private var readExecutor: ExecutorService = _
  private val kvs = scala.collection.mutable.Map[Int, (KafkaPartitionUniqueRecordKey, KafkaPartitionUniqueRecordValue, KafkaPartitionUniqueRecordValue)]()
  private var clientName: String = _

  // Heartbeat monitor related variables.
  private var hbRunning: Boolean = false
  private var hbTopicPartitionNumber = -1
  private val hbExecutor2 = Executors.newFixedThreadPool(1)

  private var timeoutTimer = KafkaSimpleConsumer.INIT_TIMEOUT

  /**
   *  This will stop all the running reading threads and close all the underlying connections to Kafka.
   */
  override def Shutdown(): Unit = lock.synchronized {
    StopProcessing
  }

  /**
   * Will stop all the running read threads only - a call to StartProcessing will restart the reading process
   */
  def StopProcessing(): Unit = {
    isShutdown = true
    terminateReaderTasks
  }

  private def getTimeoutTimer: Long = {
    timeoutTimer = timeoutTimer * 2
    if (timeoutTimer > KafkaSimpleConsumer.MAX_TIMEOUT) {
      timeoutTimer = KafkaSimpleConsumer.MAX_TIMEOUT
      return timeoutTimer
    }
    return timeoutTimer
  }

  private def resetTimeoutTimer: Unit = {
    timeoutTimer = KafkaSimpleConsumer.INIT_TIMEOUT
  }


  override def getComponentStatusAndMetrics: MonitorComponentInfo = {
    implicit val formats = org.json4s.DefaultFormats

    var depths:  Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = null

    try {
      depths = getAllPartitionEndValues
    } catch {
      case e: KamanjaException => {
        return new MonitorComponentInfo(AdapterConfiguration.TYPE_INPUT, qc.Name, KafkaSimpleConsumer.ADAPTER_DESCRIPTION, startHeartBeat, lastSeen, Serialization.write(metrics).toString)
      }
      case e: Exception => {
        LOG.error ("KAFKA-ADAPTER: Unexpected exception determining kafka queue depths for " + qc.topic, e)
        return new MonitorComponentInfo(AdapterConfiguration.TYPE_INPUT, qc.Name, KafkaSimpleConsumer.ADAPTER_DESCRIPTION, startHeartBeat, lastSeen, Serialization.write(metrics).toString)
      }
    }

    partitonDepths.clear
    depths.foreach(t => {
      try {
        val partId = t._1.asInstanceOf[KafkaPartitionUniqueRecordKey]
        val localPart = kvs.getOrElse(partId.PartitionId,null)
        if (localPart != null) {
          val partVal = t._2.asInstanceOf[KafkaPartitionUniqueRecordValue]
          var thisDepth: Long = 0
          if(localReadOffsets.contains(partId.PartitionId)) {
            thisDepth = localReadOffsets(partId.PartitionId)
          }
          partitonDepths(partId.PartitionId.toString) = partVal.Offset - thisDepth
        }

      } catch {
        case e: Exception => LOG.warn("KAFKA-ADAPTER: Broker:  error trying to determine kafka queue depths for "+qc.topic,e)
      }
    })

    return new MonitorComponentInfo( AdapterConfiguration.TYPE_INPUT, qc.Name, KafkaSimpleConsumer.ADAPTER_DESCRIPTION, startHeartBeat, lastSeen,  Serialization.write(metrics).toString)
  }

  /**
   * Start processing - will start a number of threads to read the Kafka queues for a topic.  The list of Hosts servicing a
   * given topic, and the topic have been set when this KafkaConsumer_V2 Adapter was instantiated.  The partitionIds should be
   * obtained via a prior call to the adapter.  One of the hosts will be a chosen as a leader to service the requests by the
   * spawned threads.
   * @param ignoreFirstMsg Boolean - if true, ignore the first message sending to engine
   * @param partitionIds Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue, Long, PartitionUniqueRecordValue)] - an Array of partition ids
   */
  def StartProcessing(partitionIds: Array[StartProcPartInfo], ignoreFirstMsg: Boolean): Unit = lock.synchronized {

    var lastHb: Long = 0
    startHeartBeat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))

    LOG.info("START_PROCESSING CALLED")
    // Check to see if this already started
    if (startTime > 0) {
      LOG.error("KAFKA-ADAPTER: already started, or in the process of shutting down")
    }
    startTime = System.nanoTime
    LOG.debug("KAFKA-ADAPTER: Starting to read Kafka queues for topic: " + qc.topic)

    if (partitionIds == null || partitionIds.size == 0) {
      LOG.error("KAFKA-ADAPTER: Cannot process the kafka queue request, invalid parameters - number")
      return
    }

    // Get the data about the request and set the instancePartition list.
    val partitionInfo = partitionIds.map(quad => {
      (quad._key.asInstanceOf[KafkaPartitionUniqueRecordKey],
        quad._val.asInstanceOf[KafkaPartitionUniqueRecordValue],
        quad._validateInfoVal.asInstanceOf[KafkaPartitionUniqueRecordValue])
    })

    qc.instancePartitions = partitionInfo.map(partQuad => { partQuad._1.PartitionId }).toSet

    // Make sure the data passed was valid.
    if (qc.instancePartitions == null) {
      LOG.error("KAFKA-ADAPTER: Cannot process the kafka queue request, invalid parameters - partition instance list")
      return
    }

    // Figure out the size of the thread pool to use and create that pool
    var threads = 0
    if (threads == 0) {
      if (qc.instancePartitions.size == 0)
        threads = 1
      else
        threads = qc.instancePartitions.size
    }

    readExecutor = Executors.newFixedThreadPool(threads)

    // Create a Map of all the partiotion Ids.
    kvs.clear
    partitionInfo.foreach(quad => {
      kvs(quad._1.PartitionId) = quad
    })

    // Enable the adapter to process
    isQuiesced = false
    LOG.debug("KAFKA-ADAPTER: Starting " + kvs.size + " threads to process partitions")

    val fetchSz = qc.otherconfigs.getOrElse("fetchsize", KafkaSimpleConsumer.FETCHSIZE.toString).trim.toInt // FETCHSIZE
    val zkConnTimeOut = qc.otherconfigs.getOrElse("zookeeper_connection_timeout_ms", KafkaSimpleConsumer.ZOOKEEPER_CONNECTION_TIMEOUT_MS.toString).trim.toInt // ZOOKEEPER_CONNECTION_TIMEOUT_MS

    // Schedule a task to perform a read from a give partition.
    kvs.foreach(kvsElement => {

      readExecutor.execute(new Runnable() {
        override def run() {
          val partitionId = kvsElement._1
          val partition = kvsElement._2

          // Initialize the monitoring status
          partitonCounts(partitionId.toString) = 0
          partitonDepths(partitionId.toString) = 0
          partitionExceptions(partitionId.toString) = new ExceptionInfo("n/a","n/a")

          // if the offset is -1, then the server wants to start from the begining, else, it means that the server
          // knows what its doing and we start from that offset.
          var readOffset: Long = -1
          val uniqueRecordValue = if (ignoreFirstMsg) partition._3.Offset else partition._3.Offset - 1

          var messagesProcessed: Long = 0
          var execThread: ExecContext = null
          val uniqueKey = new KafkaPartitionUniqueRecordKey
          val uniqueVal = new KafkaPartitionUniqueRecordValue

          clientName = "Client" + qc.Name + "/" + partitionId
          uniqueKey.Name = qc.Name
          uniqueKey.TopicName = qc.topic
          uniqueKey.PartitionId = partitionId

          // Figure out which of the hosts is the leader for the given partition
          var leadBroker: String = getKafkaConfigId(findLeader(qc.hosts, partitionId))

          // Start processing from either a beginning or a number specified by the KamanjaMananger
          readOffset = getKeyValueForPartition(leadBroker, partitionId, kafka.api.OffsetRequest.EarliestTime)
          if (partition._2.Offset > readOffset) {
            readOffset = partition._2.Offset
          }

          // So, initialize local offsets here.
          localReadOffsets(partitionId) = readOffset

          // See if we can determine the right offset, bail if we can't
          if (readOffset == -1) {
            LOG.error("KAFKA-ADAPTER: Unable to initialize new reader thread for partition {" + partitionId + "} starting at offset " + readOffset + " on server - " + leadBroker + ", Invalid OFFSET")
            return
          }

          LOG.debug("KAFKA-ADAPTER: Initializing new reader thread for partition {" + partitionId + "} starting at offset " + readOffset + " on server - " + leadBroker)

          // If we are forced to retry in case of a failure, get the new Leader.
          //val consumer = brokerConfiguration(leadBroker)
          var brokerId = convertIp(leadBroker)
          var brokerName = brokerId.split(":")
          var consumer: SimpleConsumer = createConsumer(brokerName(0), brokerName(1))

          resetTimeoutTimer

          // Keep processing until you fail enough times.
          while (!isQuiesced) {
            val fetchReq = new FetchRequestBuilder().clientId(clientName).addFetch(qc.topic, partitionId, readOffset, fetchSz).build()
            var fetchResp: FetchResponse = null
            var isFetchError = false
            var isErrorRecorded = false
            // Call the broker and get a response.
            while ((fetchResp == null || isFetchError) && !isQuiesced) {
              try {
                fetchResp = consumer.fetch(fetchReq)
                isFetchError = false
                if (fetchResp.hasError) {
                  if(!isErrorRecorded) {
                    partitionExceptions(partitionId.toString) = new ExceptionInfo(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis)),"n/a")
                    isErrorRecorded = true
                  }
                  isFetchError = true
                  LOG.warn("KAFKA ADAPTER: Error fetching topic " + qc.topic + ", partition " + partitionId + ", retrying due to an error " + fetchResp.errorCode(qc.topic, partitionId))
                  LOG.warn("KAFKA ADAPTER: Error fetching topic " + qc.topic + ", partition " + partitionId + ", recreating kafka leader for this partition")
                  consumer.close
                  leadBroker = getKafkaConfigId(findLeader(qc.hosts, partitionId))

                  LOG.warn("KAFKA ADAPTER: Error fetching topic " + qc.topic + ", partition " + partitionId + ", failing over to the new leader " + leadBroker)
                  brokerId = convertIp(leadBroker)
                  brokerName = brokerId.split(":")
                  var newConsumer = createConsumer(brokerName(0), brokerName(1))
                  consumer.close()
                  consumer = newConsumer
                  Thread.sleep(getTimeoutTimer)
                } else {
                  resetTimeoutTimer
                }
              } catch {
                case e: InterruptedException => {
                  LOG.error(qc.Name + " KAFKA ADAPTER: Read retry interrupted", e)
                  Shutdown()
                  return
                }
                case e: Exception => {
                  if(!isErrorRecorded) {
                    partitionExceptions(partitionId.toString) = new ExceptionInfo(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis)),"n/a")
                    isErrorRecorded = true
                  }
                  LOG.error("KAFKA ADAPTER: Failure fetching topic "+qc.topic+", partition " + partitionId + ", retrying")
                  LOG.warn("KAFKA ADAPTER: Error fetching topic " + qc.topic + ", partition " + partitionId + ", recreating kafka leader for this partition")
                  consumer.close
                  leadBroker = null
                  while (leadBroker == null) {
                    try {
                      Thread.sleep(getTimeoutTimer)
                      leadBroker = getKafkaConfigId(findLeader(qc.hosts, partitionId))
                    } catch {
                      case e: java.lang.InterruptedException =>
                      {
                        LOG.debug("KAFKA ADAPTER: Forcing down the Consumer Reader thread", e)
                        Shutdown()
                      }
                      case e: KamanjaException =>  LOG.warn("KAFKA ADAPTER: Failover target for " + qc.topic + ", partition " + partitionId + ", does not exist - retrying")
                    }
                  }
                  LOG.warn("KAFKA ADAPTER: Recovered from error fetching " + qc.topic + ", partition " + partitionId + ", failing over to the new leader " + leadBroker)
                  brokerId = convertIp(leadBroker)
                  brokerName = brokerId.split(":")
                  var newConsumer = createConsumer(brokerName(0), brokerName(1))
                  consumer.close()
                  consumer = newConsumer
                  Thread.sleep(getTimeoutTimer)
                }
              }
            }

            // Record in metrics if we encountered a problem but it was resolved
            if (isErrorRecorded) {
              var new_exeption_info = new ExceptionInfo(partitionExceptions(partitionId.toString).Last_Failure, new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis)) )
              partitionExceptions(partitionId.toString) = new_exeption_info
            }


            // If we are here under shutdown conditions.. cleanup and bail
            if (isQuiesced) {
              if (consumer != null) { consumer.close }
              return
            }

            val ignoreTillOffset = if (ignoreFirstMsg) partition._2.Offset else partition._2.Offset - 1
            // Successfuly read from the Kafka Adapter - Process messages
            fetchResp.messageSet(qc.topic, partitionId).foreach(msgBuffer => {
              val bufferPayload = msgBuffer.message.payload
              val message: Array[Byte] = new Array[Byte](bufferPayload.limit)
              readOffset = msgBuffer.nextOffset
              breakable {
                val readTmMs = System.currentTimeMillis
                messagesProcessed = messagesProcessed + 1

                // Engine in interested in message at OFFSET + 1, Because I cannot guarantee that offset for a partition
                // is increasing by one, and I cannot simple set the offset to offset++ since that can cause our of
                // range errors on the read, we simple ignore the message by with the offset specified by the engine.
                if (msgBuffer.offset <= ignoreTillOffset) {
                  LOG.debug("KAFKA-ADAPTER: skipping a message at  Broker: " + leadBroker + "_" + partitionId + " OFFSET " + msgBuffer.offset + " " + new String(message, "UTF-8") + " - previously processed! ")
                  break
                }

                // OK, present this message to the Engine.
                bufferPayload.get(message)
                LOG.debug("KAFKA-ADAPTER: Broker: " + leadBroker + "_" + partitionId + " OFFSET " + msgBuffer.offset + " Message: " + new String(message, "UTF-8"))

                // Create a new EngineMessage and call the engine.
                if (execThread == null) {
                  execThread = execCtxtObj.CreateExecContext(input, uniqueKey, nodeContext)
                }

                incrementCountForPartition(partitionId)

                uniqueVal.Offset = msgBuffer.offset
//                val dontSendOutputToOutputAdap = uniqueVal.Offset <= uniqueRecordValue
                execThread.execute(message, uniqueKey, uniqueVal, readTmMs)

                // Kafka offsets are 0 based, so add 1
                localReadOffsets(partitionId) = (uniqueVal.Offset + 1)
                // val key = Category + "/" + qc.Name + "/evtCnt"
                // cntrAdapter.addCntr(key, 1) // for now adding each row
              }

            })

            try {
              // Sleep here, only if input parm for sleep is set and we haven't gotten any messages on the previous kafka call.
              if ((qc.noDataSleepTimeInMs > 0) && (messagesProcessed == 0)) {
                Thread.sleep(qc.noDataSleepTimeInMs)
              }
              messagesProcessed = 0

              // If it has been more then 5 seconds since the last message, externalize the statistics.
              // This loop waits at least qc.noDataSleepTimeInMs seconds between checking kafka for new
              // input data, so this may not be exactly 5 second interval.  The adapter architecture is due
              // for an overhaul, so this weill probably change.
              val thisHb = System.currentTimeMillis
              if ((thisHb - lastHb) > KafkaSimpleConsumer.HB_PERIOD) {
                LOG.debug("KAFKA-ADAPTER: Broker: " + leadBroker + " is marked alive,")
                lastHb = thisHb
                lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
              }

            } catch {
              case e: java.lang.InterruptedException =>
                {
                  LOG.info("KAFKA ADAPTER: shutting down thread for " + qc.topic + " partition: " + partitionId )
                  Shutdown()
                }
            }
          }
          if (consumer != null) { consumer.close }
        }
      })
    })

  }

  /**
   * getServerInfo - returns information about hosts and their coresponding partitions.
   * @return Array[PartitionUniqueRecordKey] - return data
   */
  def GetAllPartitionUniqueRecordKey: Array[PartitionUniqueRecordKey] = lock.synchronized {
    // iterate through all the simple consumers - collect the metadata about this topic on each specified host
    var partitionRecord: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()

    val topics: Array[String] = Array(qc.topic)
    val metaDataReq = new TopicMetadataRequest(topics, KafkaSimpleConsumer.METADATA_REQUEST_CORR_ID)
    var partitionNames: List[KafkaPartitionUniqueRecordKey] = List()

    LOG.debug("KAFKA-ADAPTER - Querying kafka for Topic " + qc.topic + " metadata(partitions)")

    val fetchSz = qc.otherconfigs.getOrElse("fetchsize", KafkaSimpleConsumer.FETCHSIZE.toString).trim.toInt // FETCHSIZE
    val zkConnTimeOut = qc.otherconfigs.getOrElse("zookeeper_connection_timeout_ms", KafkaSimpleConsumer.ZOOKEEPER_CONNECTION_TIMEOUT_MS.toString).trim.toInt // ZOOKEEPER_CONNECTION_TIMEOUT_MS

    qc.hosts.foreach(broker => {
      breakable {
        val brokerName = broker.split(":")
        var partConsumer: SimpleConsumer = null
        try {
          partConsumer = new SimpleConsumer(brokerName(0), brokerName(1).toInt,
            zkConnTimeOut,
            fetchSz,
            KafkaSimpleConsumer.METADATA_REQUEST_TYPE)
        } catch {
          case e: Exception => {
            LOG.warn("KAFKA-ADAPTER: unable to connect to broker " + broker, e)
            break
          }
        }

        try {
          // Keep retrying to connect until MAX number of tries is reached.
          var retryCount = 0
          var result = doSend(partConsumer,metaDataReq)
          while(result._1 == null && !isQuiesced) {
            if (retryCount > KafkaSimpleConsumer.MAX_FAILURES) {
              LOG.warn("KAFKA-ADAPTER: unable to send request to broker " + broker)
              break
            }
            retryCount += 1
            Thread.sleep(1000)
            result = doSend(partConsumer,metaDataReq)
          }

          val metaData = result._1.topicsMetadata
          metaData.foreach(topicMeta => {
            topicMeta.partitionsMetadata.foreach(partitionMeta => {
              //partitionMeta.
              val uniqueKey = new KafkaPartitionUniqueRecordKey
              uniqueKey.PartitionId = partitionMeta.partitionId
              uniqueKey.Name = qc.Name
              uniqueKey.TopicName = qc.topic
              if (!partitionRecord.contains(qc.topic +":"+ partitionMeta.partitionId.toString)) {
                partitionNames = uniqueKey :: partitionNames
                partitionRecord = partitionRecord + (qc.topic +":"+ partitionMeta.partitionId.toString)
              }
            })
          })
          // If we are here, that means that we actually got a Broker to return to us info about this topic.  return
          // what ever is in there now.
          return partitionNames.toArray
        } catch {
          case fae: FatalAdapterException => throw fae
          case npe: NullPointerException => {
            if (isQuiesced) LOG.warn("Kafka Simple Consumer exception during kafka call for partition information -  " +qc.topic , npe)
          }
          case e: Exception => throw FatalAdapterException("failed to SEND MetadataRequest to Kafka Server ", e)
        } finally {
          if (partConsumer != null) { partConsumer.close }
        }
        if (partitionNames.size > 0) return partitionNames.toArray
      }
    })

    if (partitionNames.size == 0) throw FatalAdapterException("Failed to retrieve Topic Metadata for defined partitions", null)
    return partitionNames.toArray
  }

  /**
   *
   * @param consumer
   * @param req
   * @return
   */
  private def doSend(consumer: SimpleConsumer, req: TopicMetadataRequest): (kafka.api.TopicMetadataResponse,Exception) = {
    try {
      val metaDataResp: kafka.api.TopicMetadataResponse = consumer.send(req)
      return (metaDataResp,null)
    } catch {
      case e: Exception => return (null,e)
    }
  }


  /**
   * Return an array of PartitionUniqueKey/PartitionUniqueRecordValues whre key is the partion and value is the offset
   * within the kafka queue where it begins.
   * @return Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]
   */
  override def getAllPartitionBeginValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = lock.synchronized {
    return getKeyValues(kafka.api.OffsetRequest.EarliestTime)
  }

  /**
   * Return an array of PartitionUniqueKey/PartitionUniqueRecordValues whre key is the partion and value is the offset
   * within the kafka queue where it eds.
   * @return Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]
   */
  override def getAllPartitionEndValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = lock.synchronized {
    return getKeyValues(kafka.api.OffsetRequest.LatestTime)
  }

  override def DeserializeKey(k: String): PartitionUniqueRecordKey = {
    val key = new KafkaPartitionUniqueRecordKey
    try {
      LOG.debug("Deserializing Key:" + k)
      key.Deserialize(k)
    } catch {
      case e: Exception => {
        LOG.error("Failed to deserialize Key:%s.".format(k), e)
        throw e
      }
    }
    key
  }

  override def DeserializeValue(v: String): PartitionUniqueRecordValue = {
    val vl = new KafkaPartitionUniqueRecordValue
    if (v != null) {
      try {
        LOG.debug("Deserializing Value:" + v)
        vl.Deserialize(v)
      } catch {
        case e: Exception => {
          LOG.error("Failed to deserialize Value:%s.".format(v), e)
          throw e
        }
      }
    }
    vl
  }

  /**
   *  Find a leader of for this topic for a given partition.
   */
  private def findLeader(brokers: Array[String], inPartition: Int): kafka.api.PartitionMetadata = lock.synchronized {
    var leaderMetadata: kafka.api.PartitionMetadata = null

    val fetchSz = qc.otherconfigs.getOrElse("fetchsize", KafkaSimpleConsumer.FETCHSIZE.toString).trim.toInt // FETCHSIZE
    val zkConnTimeOut = qc.otherconfigs.getOrElse("zookeeper_connection_timeout_ms", KafkaSimpleConsumer.ZOOKEEPER_CONNECTION_TIMEOUT_MS.toString).trim.toInt // ZOOKEEPER_CONNECTION_TIMEOUT_MS

    LOG.debug("KAFKA-ADAPTER: Looking for Kafka Topic Leader for partition " + inPartition)
    try {
      brokers.foreach(broker => {
        breakable {
            // Create a connection to this broker to obtain the metadata for this broker.
            val brokerName = broker.split(":")
            var llConsumer: SimpleConsumer = null
            try {
              llConsumer = new SimpleConsumer(brokerName(0), brokerName(1).toInt,
                zkConnTimeOut,
                fetchSz,
                KafkaSimpleConsumer.METADATA_REQUEST_TYPE)
            } catch {

              case e: Exception => {
                LOG.warn("KAFKA-ADAPTER: Unable to create a leader consumer")
                break
              }

            }
            val topics: Array[String] = Array(qc.topic)
            val llReq = new TopicMetadataRequest(topics, KafkaSimpleConsumer.METADATA_REQUEST_CORR_ID)

            // get the metadata on the llConsumer
            try {
              // Keep retrying to connect until MAX number of tries is reached.
              var retryCount = 0
              var result = doSend(llConsumer,llReq)
              while(result._1 == null && !isQuiesced) {
                if (retryCount > KafkaSimpleConsumer.MAX_FAILURES) {
                  LOG.warn("KAFKA-ADAPTER: Unable to contact " + broker)
                  break
                }
                // throw FatalAdapterException("Failed to retrieve Topic Metadata while searching for LEADER node", result._2)

                retryCount += 1
                Thread.sleep(1000)
                result = doSend(llConsumer,llReq)
              }

              val metaData = result._1.topicsMetadata

              // look at each piece of metadata, and analyze its partitions
              metaData.foreach(metaDatum => {
                val partitionMetadata = metaDatum.partitionsMetadata
                partitionMetadata.foreach(partitionMetadatum => {
                  // If we found the partitionmetadatum for the desired partition then this must be the leader.
                  if (partitionMetadatum.partitionId == inPartition) {
                    // Create a list of replicas to be used in case of a fetch failure here.
                    replicaBrokers.empty
                    partitionMetadatum.replicas.foreach(replica => {
                      replicaBrokers = replicaBrokers + ((replica.host)+":"+replica.port )
                    })
                    if (partitionMetadatum.leader.getOrElse(null) != null)
                      leaderMetadata = partitionMetadatum
                  }
                })
              })
            } catch {
              case fae: FatalAdapterException => throw fae
              case npe: NullPointerException => {
                if (isQuiesced) LOG.warn("KAFKA ADAPTER - unable to find leader (shutdown detected), leader for this partition may be temporarily unavailable. partition ID: " + inPartition, npe)
                else  LOG.warn("KAFKA ADAPTER - unable to find leader, leader for this partition may be temporarily unavailable. partition ID: " + inPartition, npe)
              }
              case e: Exception => throw FatalAdapterException("failed to SEND MetadataRequest to Kafka Server ", e)
            } finally {
              if (llConsumer != null) llConsumer.close()
            }
        }
      })

    } catch {
      case e: Exception => {
        LOG.error("KAFKA ADAPTER - Fatal Error for FindLeader for partition " + inPartition, e)
      }
    }
    if (leaderMetadata == null) throw FatalAdapterException("Failed to find a LEADER node, Topic "+qc.topic+ ", partition " + inPartition,null)
    return leaderMetadata;
  }

  private def createConsumer(bokerName: String, borkerPort: String): SimpleConsumer = {
    val fetchSz = qc.otherconfigs.getOrElse("fetchsize", KafkaSimpleConsumer.FETCHSIZE.toString).trim.toInt // FETCHSIZE
    val zkConnTimeOut = qc.otherconfigs.getOrElse("zookeeper_connection_timeout_ms", KafkaSimpleConsumer.ZOOKEEPER_CONNECTION_TIMEOUT_MS.toString).trim.toInt // ZOOKEEPER_CONNECTION_TIMEOUT_MS

    var consumer: SimpleConsumer = null
    while (consumer == null && !isQuiesced) {
      try {
        consumer = new SimpleConsumer(bokerName, borkerPort.toInt,
          zkConnTimeOut,
          fetchSz,
          KafkaSimpleConsumer.METADATA_REQUEST_TYPE)
      } catch {
        case e: Exception => {
          LOG.error("KAFKA ADAPTER: Failure connecting to Kafka server, retrying", e)
          Thread.sleep(getTimeoutTimer)
        }
      }
    }
    return consumer
  }
  /*
* getKeyValues - get the values from the OffsetMetadata call and combine them into an array
 */
  private def getKeyValues(time: Long): Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = {
    var infoList = List[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]()
    // Always get the complete list of partitions for this.
    val kafkaKnownParitions = GetAllPartitionUniqueRecordKey
    val partitionList = kafkaKnownParitions.map(uKey => { uKey.asInstanceOf[KafkaPartitionUniqueRecordKey].PartitionId }).toSet

    // Now that we know for sure we have a partition list.. process them
    partitionList.foreach(partitionId => {
      var offset: Long = 0
      breakable {
        try {
          offset = getKeyValueForPartition(getKafkaConfigId(findLeader(qc.hosts, partitionId)), partitionId, time)
        } catch {
          case e1: KamanjaException => {
            LOG.warn("KAFKA ADAPTER: Could  not determine a leader for partition " + partitionId)
            break
          }
          case e2: Exception => {
            LOG.error("KAFKA ADAPTER: Unknown exception... Could  not determine a leader for partition " + partitionId, e2)
            break
          }
        }
        val rKey = new KafkaPartitionUniqueRecordKey
        val rValue = new KafkaPartitionUniqueRecordValue
        rKey.PartitionId = partitionId
        rKey.Name = qc.Name
        rKey.TopicName = qc.topic
        rValue.Offset = offset
        infoList = (rKey, rValue) :: infoList
      }
    })
    return infoList.toArray
  }

  /**
   * getKeyValueForPartition - get the valid offset range in a given partition.
   */
  private def getKeyValueForPartition(leadBroker: String, partitionId: Int, timeFrame: Long): Long = {
    var offset: Long = -1
    var llConsumer: kafka.javaapi.consumer.SimpleConsumer = null
    val brokerName = leadBroker.split(":")

    val fetchSz = qc.otherconfigs.getOrElse("fetchsize", KafkaSimpleConsumer.FETCHSIZE.toString).trim.toInt // FETCHSIZE
    val zkConnTimeOut = qc.otherconfigs.getOrElse("zookeeper_connection_timeout_ms", KafkaSimpleConsumer.ZOOKEEPER_CONNECTION_TIMEOUT_MS.toString).trim.toInt // ZOOKEEPER_CONNECTION_TIMEOUT_MS

    try{
      llConsumer = new kafka.javaapi.consumer.SimpleConsumer(brokerName(0), brokerName(1).toInt,
        zkConnTimeOut,
        fetchSz,
        KafkaSimpleConsumer.METADATA_REQUEST_TYPE)
    } catch {
      case e: Exception => throw FatalAdapterException("Unable to create connection to Kafka Server ", e)
    }

    try {
      // Set up the request object
      val jtap: kafka.common.TopicAndPartition = kafka.common.TopicAndPartition(qc.topic.toString, partitionId)
      val requestInfo: java.util.HashMap[TopicAndPartition, PartitionOffsetRequestInfo] = new java.util.HashMap[TopicAndPartition, PartitionOffsetRequestInfo]()
      requestInfo.put(jtap, new PartitionOffsetRequestInfo(timeFrame, 1))
      val offsetRequest: kafka.javaapi.OffsetRequest = new kafka.javaapi.OffsetRequest(requestInfo, kafka.api.OffsetRequest.CurrentVersion, clientName)

      // Issue the call
      // val response: kafka.javaapi.OffsetResponse = llConsumer.getOffsetsBefore(offsetRequest)
      var retryCount = 0
      //  Get the OffsetResponse
      var result = doSendJavaConsumer(llConsumer,offsetRequest)
      while(result._1 == null && !isQuiesced) {
        if (retryCount > KafkaSimpleConsumer.MAX_FAILURES)
          throw FatalAdapterException("Failed to retrieve Topic Metadata while searching for LEADER node", result._2)
        retryCount += 1
        Thread.sleep(1000)
        result = doSendJavaConsumer(llConsumer,offsetRequest)
      }

      // Return the value, or handle the error
      if (result._1.hasError) {
        LOG.error("KAFKA ADAPTER: error occured trying to find out the valid range for partition {" + partitionId + "}")
      } else {
        val offsets: Array[Long] = result._1.offsets(qc.topic.toString, partitionId)
        offset = offsets(0)
      }
    } catch {
      case fae: FatalAdapterException => throw fae
      case npe: NullPointerException => {
        if (isQuiesced) LOG.warn("Kafka Simple Consumer is shutting down during kafka call looking for offsets - ignoring the call", npe)
      }
      case e: java.lang.Exception => {
        LOG.error("KAFKA ADAPTER: Exception during offset inquiry request for partiotion {" + partitionId + "}", e)
      }
    } finally {
      if (llConsumer != null) { llConsumer.close }
    }
    return offset
  }


  /**
   *
   * @param consumer
   * @param req
   * @return
   */
  private def doSendJavaConsumer(consumer: kafka.javaapi.consumer.SimpleConsumer, req: kafka.javaapi.OffsetRequest): (kafka.javaapi.OffsetResponse,Exception) = {
    try {
      val metaDataResp: kafka.javaapi.OffsetResponse = consumer.getOffsetsBefore(req)
      return (metaDataResp,null)
    } catch {
      case e: Exception => return (null,e)
    }
  }

  /**
   *  Convert the "localhost:XXXX" into an actual IP address.
   */
  private def convertIp(inString: String): String = {
    val brokerName = inString.split(":")
    if (brokerName(0).equalsIgnoreCase("localhost")) {
      brokerName(0) = InetAddress.getLocalHost().getHostAddress()
    }
    val brokerId = brokerName(0) + ":" + brokerName(1)
    brokerId
  }

  /**
   * combine the ip address and port number into a Kafka Configuratio ID
   */
  private def getKafkaConfigId(metadata: kafka.api.PartitionMetadata): String = {
    return metadata.leader.get.host + ":" + metadata.leader.get.port;
  }

  /**
   *  terminateReaderTasks - well, just what it says
   */
  private def terminateReaderTasks(): Unit = {
    if (readExecutor == null) return

    // Tell all thread to stop processing on the next interval, and shutdown the Excecutor.
    quiesce

    // Give the threads to gracefully stop their reading cycles, and then execute them with extreme prejudice.
    Thread.sleep(qc.noDataSleepTimeInMs + 1)
    readExecutor.shutdownNow
    while (readExecutor.isTerminated == false) {
      Thread.sleep(100)
    }

    LOG.debug("KAFKA_ADAPTER - Shutdown Complete")
    readExecutor = null
    startTime = 0
  }

  /* no need for any synchronization here... it can only go one way.. worst case scenario, a reader thread gets to try to
  *  read the kafka queue one extra time (100ms lost)
   */
  private def quiesce: Unit = {
    isQuiesced = true
  }

  private def setMetricValue(key: String, value: Any): Unit = {
    metrics(key) = value.toString
  }

  private def incrementCountForPartition(pid: Int): Unit = {
    var cVal: Long = partitonCounts.getOrElse(pid.toString, 0)
    partitonCounts(pid.toString) = cVal + 1
  }


}

