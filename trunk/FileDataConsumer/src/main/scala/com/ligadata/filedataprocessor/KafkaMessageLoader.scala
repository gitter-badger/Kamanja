package com.ligadata.filedataprocessor

import java.io.{ IOException, File, PrintWriter }
import java.nio.file.StandardCopyOption._
import java.nio.file.{ Paths, Files }
import java.text.SimpleDateFormat
import java.util.concurrent.{TimeUnit, Future}
import java.util.{TimeZone, Properties, Date, Arrays}

import com.ligadata.Exceptions._
import com.ligadata.KamanjaBase._
import com.ligadata.MetadataAPI.MetadataAPIImpl
import com.ligadata.Utils.{ Utils, KamanjaLoaderInfo }
import com.ligadata.ZooKeeper.CreateClient
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata.MessageDef
import kafka.common.{ QueueFullException, FailedToSendMessageException }
import kafka.producer.{ KeyedMessage, Producer, Partitioner }
import org.apache.curator.framework.CuratorFramework
import org.apache.logging.log4j.{ Logger, LogManager }
import kafka.utils.VerifiableProperties

import org.apache.kafka.clients.producer.{Callback, RecordMetadata, KafkaProducer, ProducerRecord}
import org.apache.kafka.common.serialization.ByteArraySerializer
import org.apache.kafka.common.serialization.StringSerializer
import org.apache.kafka.common.serialization.ByteArraySerializer
import org.apache.kafka.clients.producer.ProducerConfig
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Promise


/**
 * Created by danielkozin on 9/24/15.
 */
class KafkaMessageLoader(partIdx: Int, inConfiguration: scala.collection.mutable.Map[String, String]) {
  var fileBeingProcessed: String = ""
  var numberOfMessagesProcessedInFile: Int = 0
  var currentOffset: Int = 0
  var startFileProcessingTimeStamp: Long = 0
  var numberOfValidEvents: Int = 0
  var endFileProcessingTimeStamp: Long = 0
  val RC_RETRY: Int = 3
  var retryCount = 0
  var recPartitionMap: scala.collection.mutable.Map[Int,Int] = scala.collection.mutable.Map[Int,Int]()

  val MAX_RETRY = 1
  val INIT_KAFKA_UNAVAILABLE_WAIT_VALUE = 1000
  val MAX_WAIT = 60000

  var currentSleepValue = INIT_KAFKA_UNAVAILABLE_WAIT_VALUE

  var lastOffsetProcessed: Int = 0
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
  dateFormat.setTimeZone(TimeZone.getTimeZone("UTC")); // Setting the UTC timezone.

  // Set up some properties for the Kafka Producer
  val props = new Properties()
  props.put(org.apache.kafka.clients.producer.ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, inConfiguration.get(SmartFileAdapterConstants.KAFKA_BROKER).get)
  props.put(org.apache.kafka.clients.producer.ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG,"org.apache.kafka.common.serialization.ByteArraySerializer")
  props.put(org.apache.kafka.clients.producer.ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG,"org.apache.kafka.common.serialization.ByteArraySerializer")
  props.put("request.required.acks",inConfiguration.getOrElse(SmartFileAdapterConstants.KAFKA_ACK, "0"))

  // create the producer object
 // val producer = new KafkaProducer[Array[Byte], Array[Byte]](new ProducerConfig(props))
  val producer = new KafkaProducer[Array[Byte], Array[Byte]](props)
  var numPartitionsForMainTopic = producer.partitionsFor(inConfiguration(SmartFileAdapterConstants.KAFKA_TOPIC))

  var delimiters = new DataDelimiters
  delimiters.keyAndValueDelimiter = inConfiguration.getOrElse(SmartFileAdapterConstants.KV_SEPARATOR, "\\x01")
  delimiters.fieldDelimiter = inConfiguration.getOrElse(SmartFileAdapterConstants.FIELD_SEPARATOR, "\\x01")
  delimiters.valueDelimiter = inConfiguration.getOrElse(SmartFileAdapterConstants.VALUE_SEPARATOR, "~")

  var debug_IgnoreKafka = inConfiguration.getOrElse("READ_TEST_ONLY", "FALSE")
  var status_frequency: Int = inConfiguration.getOrElse(SmartFileAdapterConstants.STATUS_FREQUENCY, "100000").toInt
  var isZKIgnore: Boolean = inConfiguration.getOrElse(SmartFileAdapterConstants.ZOOKEEPER_IGNORE, "FALSE").toBoolean
  var errorTopic = inConfiguration.getOrElse(SmartFileAdapterConstants.KAFKA_ERROR_TOPIC, null)
  var statusTopic = inConfiguration.getOrElse(SmartFileAdapterConstants.KAFKA_STATUS_TOPIC, null)

  val zkcConnectString = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZOOKEEPER_CONNECT_STRING")
  logger.debug(partIdx + " SMART FILE CONSUMER Using zookeeper " + zkcConnectString)
  //val znodePath = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/smartFileConsumer/" + partIdx
 // var zkc: CuratorFramework = initZookeeper
  var objInst: Any = configureMessageDef
  if (objInst == null) {
    shutdown
    throw UnsupportedObjectException("Unknown message definition " + inConfiguration(SmartFileAdapterConstants.MESSAGE_NAME), null)
  }

  /**
   *
   * @param messages
   */
  def pushData(messages: Array[KafkaMessage]): Unit = {
    // First, if we are handling failover, then the messages could be of size 0.
    logger.debug("SMART FILE CONSUMER **** processing chunk of " + messages.size + " messages")
    if (messages.size == 0) return

    // If we start processing a new file, then mark so in the zk.
    if (fileBeingProcessed.compareToIgnoreCase(messages(0).relatedFileName) != 0) {
      numberOfMessagesProcessedInFile = 0
      currentOffset = 0
      numberOfValidEvents = 0
      startFileProcessingTimeStamp = 0 //scala.compat.Platform.currentTime
      fileBeingProcessed = messages(0).relatedFileName
      recPartitionMap = messages(0).partMap
      val fileTokens = fileBeingProcessed.split("/")
      FileProcessor.addToZK(fileTokens(fileTokens.size - 1), 0)
    }

    if (startFileProcessingTimeStamp == 0)
      startFileProcessingTimeStamp = scala.compat.Platform.currentTime

    val keyMessages = new ArrayBuffer[ProducerRecord[Array[Byte], Array[Byte]]](messages.size)

    var isLast = false
    messages.foreach(msg => {
      if (msg.offsetInFile == FileProcessor.BROKEN_FILE) {
        logger.error("SMART FILE ADAPTER "+ partIdx +": aborting kafka data push for " + msg.relatedFileName + " last successful offset for this file is "+ numberOfValidEvents)
        val tokenName = msg.relatedFileName.split("/")
        FileProcessor.setFileOffset(tokenName(tokenName.size - 1), numberOfValidEvents)
        fileBeingProcessed = ""
        return
      }

      if (msg.offsetInFile == FileProcessor.CORRUPT_FILE) {
        logger.error("SMART FILE ADAPTER "+ partIdx +": aborting kafka data push for " + msg.relatedFileName + " Unrecoverable file corruption detected")
        val tokenName = msg.relatedFileName.split("/")
        FileProcessor.markFileProcessingEnd(tokenName(tokenName.size - 1))
        writeGenericMsg("Corrupt file detected", msg.relatedFileName, inConfiguration(SmartFileAdapterConstants.KAFKA_STATUS_TOPIC))
        closeOutFile(msg.relatedFileName)
        fileBeingProcessed = ""
        return
      }


      if (!msg.isLastDummy) {
        numberOfValidEvents += 1
        var inputData: InputData = null
        val msgStr = new String(msg.msg)
        try {
          inputData = CreateKafkaInput(msgStr, SmartFileAdapterConstants.MESSAGE_NAME, delimiters)
          currentOffset += 1

          // FIXME:- FIX THIS
          val partitionKey: String = "" // objInst.asInstanceOf[ContainerFactoryInterface].PartitionKeyData(inputData).mkString(",") // BUGBUG:- FIX THIS

          // By far the most common path..  just add the message
          if (msg.offsetInFile == FileProcessor.NOT_RECOVERY_SITUATION) {
            keyMessages += new ProducerRecord[Array[Byte],Array[Byte]](inConfiguration(SmartFileAdapterConstants.KAFKA_TOPIC),
                                                                        getPartition(partitionKey.getBytes("UTF8"), numPartitionsForMainTopic.size),
                                                                        partitionKey.getBytes("UTF8"),
                                                                        msgStr.getBytes("UTF8"))
            numberOfMessagesProcessedInFile += 1
          } else {
            // Recovery... do checking for the message
            // Only add those messages that we have not previously processed....
            if (msg.offsetInFile >= 0 && msg.offsetInFile < currentOffset)  {
              // We may be part of in doubt batch.. gotta check partition recovery info
              var partition = getPartition(partitionKey.getBytes("UTF8"), numPartitionsForMainTopic.size)
              if ((!recPartitionMap.contains(partition) ||
                  recPartitionMap.contains(partition) && recPartitionMap(partition) == 0)) {
                keyMessages += new ProducerRecord[Array[Byte],Array[Byte]](inConfiguration(SmartFileAdapterConstants.KAFKA_TOPIC),
                                                                           getPartition(partitionKey.getBytes("UTF8"), numPartitionsForMainTopic.size),
                                                                           partitionKey.getBytes("UTF8"),
                                                                           msgStr.getBytes("UTF8"))
                numberOfMessagesProcessedInFile += 1
              } else {
                if (recPartitionMap.contains(partition)) {
                  // The partition key exists and its > 0...  meaning we need to ignore the messagem but need to
                  // decrement the counter
                  recPartitionMap(partition) = recPartitionMap(partition) - 1
                }
              }
            } else {
              // Ignoring shit...
            }
          }
        } catch {
          case mfe: KVMessageFormatingException =>
            logger.warn("", mfe)
            writeErrorMsg(msg)
          case e: Exception => {
            logger.warn("Unknown message format in partition " + partIdx, e)
            writeErrorMsg(msg)
          }
        }

        if (msg.isLast) {
          isLast = true
        }
      } else {
        isLast = true
      }
    })

    // Write to kafka
    if (isLast)
      sendToKafka(keyMessages, "msgPush1", numberOfValidEvents)
    else
      sendToKafka(keyMessages, "msgPush2", (numberOfValidEvents - messages.size), fileBeingProcessed)

    // Make sure you dont write extra for DummyLast
    if (!isLast) {
      writeStatusMsg(fileBeingProcessed)
      val fileTokens = fileBeingProcessed.split("/")
      FileProcessor.addToZK(fileTokens(fileTokens.size - 1), numberOfValidEvents)
    } else {
      // output the status message to the KAFAKA_STATUS_TOPIC
      writeStatusMsg(fileBeingProcessed, true)
      closeOutFile(fileBeingProcessed)
      numberOfMessagesProcessedInFile = 0
      currentOffset = 0
      startFileProcessingTimeStamp = 0
      numberOfValidEvents = 0
    }
  }

  /**
   *
   * @param messages
   * @return
   */
  //private def sendToKafka(messages: ArrayBuffer[KeyedMessage[Array[Byte], Array[Byte]]]): Int = {
  private def sendToKafka(messages: ArrayBuffer[ProducerRecord[Array[Byte],Array[Byte]]], sentFrom: String, fullSuccessOffset: Int = 0, fileToUpdate: String = null): Int = {
    try {
      var partitionsStats = scala.collection.mutable.Map[Int, Int]()
      logger.info("SMART FILE CONSUMER ("+partIdx+") Sending " + messages.size + " to kafka from " + sentFrom)
      if (messages.size == 0) return FileProcessor.KAFKA_SEND_SUCCESS

      var currentMessage = 0
      // Set up a map of messages to be used to verify if a message has been sent succssfully on not.
      val respFutures: scala.collection.mutable.Map[Int,Future[RecordMetadata]] = scala.collection.mutable.Map[Int,Future[RecordMetadata]]()
      messages.foreach(msg => {
        respFutures(currentMessage) = null
        currentMessage += 1
      })
      var successVector = Array.fill[Boolean](messages.size)(false)  //Array[Boolean](messages.size)
      var isFullySent = false
      var isRetry = false
      var failedPush = 0

      while (!isFullySent) {
        if (isRetry) {
          Thread.sleep(getCurrentSleepTimer)
        }

        currentMessage = 0
        messages.foreach(msg => {
          if (//respFutures.contains(currentMessage) &&
              !successVector(currentMessage)) {
            // Send the request to Kafka
            val response = producer.send(msg, new Callback {
              override def onCompletion(metadata: RecordMetadata, exception: Exception): Unit = {
                if (exception != null) {
                  failedPush += 1
                  logger.warn("SMART FILE CONSUMER ("+partIdx+") has detected a problem with pushing a message into the " +msg.topic + " will retry ", exception)
                }
              }
            })
            respFutures(currentMessage) = response
          }
          currentMessage += 1
        })

        // Make sure all messages have been successfuly sent, and resend them if we detected bad messages
        isFullySent = true
        var numberOfFailuresThisTime = 0
        for (i <- 0 until messages.size) {
          if (!successVector(i)) {
            val (rc, partitionId) = checkMessage(respFutures,i)
            if (rc > 0) {
              isFullySent = false
              isRetry = true
              numberOfFailuresThisTime += 1
            } else {
              if (partitionsStats.contains(partitionId)) {
                partitionsStats(partitionId) = partitionsStats(partitionId) + 1
              } else {
                partitionsStats(partitionId) = 1
              }
              successVector(i) = true
            }
          }
        }

        // We can now fail for some messages, so, we need to update the recovery area in ZK, to make sure the retry does not
        // process these messages.
        if (fileToUpdate != null) {
          val fileTokens = fileBeingProcessed.split("/")
          FileProcessor.addToZK(fileTokens(fileTokens.size - 1), fullSuccessOffset, partitionsStats)
        }
      }
      resetSleepTimer
    } catch {
      case e: Exception =>
        logger.error("SMART FILE CONSUMER ("+partIdx+") Could not add to the queue due to an Exception ", e)
    }
    FileProcessor.KAFKA_SEND_SUCCESS
  }

  private def checkMessage(mapF: scala.collection.mutable.Map[Int,Future[RecordMetadata]], i: Int): (Int,Int) = {
    try {
      val md = mapF(i).get(10, TimeUnit.SECONDS)
      mapF(i) = null
      return(FileProcessor.KAFKA_SEND_SUCCESS, md.partition)
    } catch {
      case e1: java.util.concurrent.ExecutionException => {
        return (FileProcessor.KAFKA_SEND_DEAD_PRODUCER, -1)
      }
      case e: java.util.concurrent.TimeoutException => {
         return (FileProcessor.KAFKA_SEND_DEAD_PRODUCER, -1)
      }
     // case ftsme: FailedToSendMessageException => {
      case ftsme: java.lang.InterruptedException => {
        return (FileProcessor.KAFKA_SEND_DEAD_PRODUCER, -1)
      }
      case e: Exception => {logger.error("CHECK_MESSAGE: Unknown error from Kafka ",e);throw e}
    }
  }

  private def getCurrentSleepTimer: Int = {
    currentSleepValue = currentSleepValue * 2
    logger.error("SMART FILE CONSUMER ("+partIdx+") detected a problem with Kafka... Retry in " + scala.math.min(currentSleepValue, MAX_WAIT) / 1000 + " seconds")
    currentSleepValue = scala.math.min(currentSleepValue, MAX_WAIT)
    currentSleepValue
  }

  private def resetSleepTimer: Unit = {
    currentSleepValue = INIT_KAFKA_UNAVAILABLE_WAIT_VALUE
  }
  /**
   *
   * @param fileName
   */
  private def closeOutFile(fileName: String): Unit = {
    try {
      logger.info("SMART FILE CONSUMER ("+partIdx+") - cleaning up after " + fileName)
      // Either move or rename the file.
      val fileStruct = fileName.split("/")

      if (inConfiguration.getOrElse(SmartFileAdapterConstants.DIRECTORY_TO_MOVE_TO, null) != null) {
        logger.info("SMART FILE CONSUMER ("+partIdx+") Moving File" + fileName + " to " + inConfiguration(SmartFileAdapterConstants.DIRECTORY_TO_MOVE_TO))
        Files.copy(Paths.get(inConfiguration(SmartFileAdapterConstants.DIRECTORY_TO_WATCH) + "/" + fileStruct(fileStruct.size - 1)), Paths.get(inConfiguration(SmartFileAdapterConstants.DIRECTORY_TO_MOVE_TO) + "/" + fileStruct(fileStruct.size - 1)), REPLACE_EXISTING)
        Files.deleteIfExists(Paths.get(inConfiguration(SmartFileAdapterConstants.DIRECTORY_TO_WATCH) + "/" + fileStruct(fileStruct.size - 1)))
      } else {
        logger.info(" SMART FILE CONSUMER ("+partIdx+")  Renaming file " + fileName + " to " + fileName + "_COMPLETE")
        (new File(inConfiguration(SmartFileAdapterConstants.DIRECTORY_TO_WATCH) + "/" + fileStruct(fileStruct.size - 1))).renameTo(new File(fileName + "_COMPLETE"))
      }

      //markFileProcessingEnd(fileName)
      val tokenName = fileName.split("/")
      FileProcessor.fileCacheRemove(tokenName(tokenName.size - 1))
      FileProcessor.removeFromZK(tokenName(tokenName.size - 1))
      FileProcessor.markFileProcessingEnd(tokenName(tokenName.size - 1))
    } catch {
      case ioe: IOException => {
        logger.error("Exception moving the file ",ioe)
        var tokenName = fileName.split("/")
        FileProcessor.setFileState(tokenName(tokenName.size - 1),FileProcessor.FINISHED_FAILED_TO_COPY)
      }
    }
  }

  /**
   *
   * @param fileName
   */
  private def writeStatusMsg(fileName: String, isTotal: Boolean = false): Unit = {

    if (statusTopic == null) return

    try {
      val cdate: Date = new Date
      if (inConfiguration.getOrElse(SmartFileAdapterConstants.KAFKA_STATUS_TOPIC, "").length > 0) {
        endFileProcessingTimeStamp = scala.compat.Platform.currentTime
        var statusMsg: String = null
        val nameToken = fileName.split("/")
        if (!isTotal)
          statusMsg = SmartFileAdapterConstants.KAFKA_LOAD_STATUS + dateFormat.format(cdate) + "," + fileName + "," + numberOfMessagesProcessedInFile + "," + (endFileProcessingTimeStamp - startFileProcessingTimeStamp)
        else
          statusMsg = SmartFileAdapterConstants.TOTAL_FILE_STATUS + dateFormat.format(cdate) + "," + fileName + "," + numberOfMessagesProcessedInFile + "," + (endFileProcessingTimeStamp - FileProcessor.getTimingFromFileCache(nameToken(nameToken.size - 1)))
        val statusPartitionId = "it does not matter"

        // Write a Status Message
        val keyMessages = new ArrayBuffer[ProducerRecord[Array[Byte], Array[Byte]]](1)
        keyMessages += new ProducerRecord(inConfiguration(SmartFileAdapterConstants.KAFKA_STATUS_TOPIC), statusPartitionId.getBytes("UTF8"), new String(statusMsg).getBytes("UTF8"))

        sendToKafka(keyMessages, "Status")

        logger.debug("Status pushed ->" + statusMsg)
      } else {
        logger.debug("NO STATUS Q SPECIFIED")
      }
    } catch {
      case e: Exception => {
        logger.warn(partIdx + " SMART FILE CONSUMER: Unable to externalize status message", e)
      }
    }
  }

  /**
   *
   * @param msg
   */
  private def writeErrorMsg(msg: KafkaMessage): Unit = {

    if (errorTopic == null) return

    val cdate: Date = new Date
    val errorMsg = dateFormat.format(cdate) + "," + msg.relatedFileName + "," + (new String(msg.msg))
    logger.warn(" SMART FILE CONSUMER ("+partIdx+"): invalid message in file " + msg.relatedFileName)
    logger.warn(errorMsg)

    // Write a Error Message
    val keyMessages = new ArrayBuffer[ProducerRecord[Array[Byte], Array[Byte]]](1)
    keyMessages += new ProducerRecord(inConfiguration(SmartFileAdapterConstants.KAFKA_ERROR_TOPIC), "rare event".getBytes("UTF8"), errorMsg.getBytes("UTF8"))
    sendToKafka(keyMessages, "Error")

  }

  private def writeGenericMsg(msg: String, fileName: String, topicName: String): Unit = {

    if (statusTopic == null) return

    val cdate: Date = new Date
    // Corrupted_File_Detected,Date-XXXXXX,FileName,-1
    val genMsg = SmartFileAdapterConstants.CORRUPTED_FILE + dateFormat.format(cdate) + "," + fileName + ",-1"
    logger.warn(" SMART FILE CONSUMER ("+partIdx+"): problem in file " + fileName)
    logger.warn(genMsg)

    // Write a Error Message
 //    val keyMessages = new ArrayBuffer[KeyedMessage[Array[Byte], Array[Byte]]](1)
 //   keyMessages += new KeyedMessage(topicName, "rare event".getBytes("UTF8"), genMsg.getBytes("UTF8"))
    val keyMessages = new ArrayBuffer[ProducerRecord[Array[Byte], Array[Byte]]](1)
    keyMessages += new ProducerRecord(topicName, "rare event".getBytes("UTF8"), genMsg.getBytes("UTF8"))

    sendToKafka(keyMessages, "Generic - Corrupt")

  }

  /**
   *
   * @param inputData
   * @param associatedMsg
   * @param delimiters
   * @return
   */
  private def CreateKafkaInput(inputData: String, associatedMsg: String, delimiters: DataDelimiters): InputData = {
    if (associatedMsg == null || associatedMsg.size == 0) {
      throw new Exception("KV data expecting Associated messages as input.")
    }

    if (delimiters.fieldDelimiter == null) delimiters.fieldDelimiter = ","
    if (delimiters.valueDelimiter == null) delimiters.valueDelimiter = "~"
    if (delimiters.keyAndValueDelimiter == null) delimiters.keyAndValueDelimiter = "\\x01"

    val str_arr = inputData.split(delimiters.fieldDelimiter, -1)
    val inpData = new KvData(inputData, delimiters)
    val dataMap = scala.collection.mutable.Map[String, String]()

    if (delimiters.fieldDelimiter.compareTo(delimiters.keyAndValueDelimiter) == 0) {
      if (str_arr.size % 2 != 0) {
        val errStr = "Expecting Key & Value pairs are even number of tokens when FieldDelimiter & KeyAndValueDelimiter are matched. We got %d tokens from input string %s".format(str_arr.size, inputData)
        logger.error(errStr)
        throw KVMessageFormatingException(errStr, null)
      }
      for (i <- 0 until str_arr.size by 2) {
        dataMap(str_arr(i).trim) = str_arr(i + 1)
      }
    } else {
      str_arr.foreach(kv => {
        val kvpair = kv.split(delimiters.keyAndValueDelimiter)
        if (kvpair.size != 2) {
          throw KVMessageFormatingException("Expecting Key & Value pair only", null)
        }
        dataMap(kvpair(0).trim) = kvpair(1)
      })
    }

    inpData.dataMap = dataMap.toMap
    inpData

  }

  /**
   *
   * @return
   */
  private def configureMessageDef(): com.ligadata.KamanjaBase.MessageFactoryInterface = {
    val loaderInfo = new KamanjaLoaderInfo()
    var msgDef: MessageDef = null
    try {
      val (typNameSpace, typName) = com.ligadata.kamanja.metadata.Utils.parseNameTokenNoVersion(inConfiguration(SmartFileAdapterConstants.MESSAGE_NAME))
    //  msgDef =  FileProcessor.getMsgDef(typNameSpace,typName) //
      msgDef = mdMgr.ActiveMessage(typNameSpace, typName)
    } catch {
      case e: Exception => {
        shutdown
        logger.error("Unable to to parse message defintion", e)
        throw UnsupportedObjectException("Unknown message definition " + inConfiguration(SmartFileAdapterConstants.MESSAGE_NAME), e)
      }
    }

    if (msgDef == null) {
      shutdown
      logger.error("Unable to to retrieve message defintion")
      throw UnsupportedObjectException("Unknown message definition " + inConfiguration(SmartFileAdapterConstants.MESSAGE_NAME), null)
    }
    // Just in case we want this to deal with more then 1 MSG_DEF in a future.  - msgName paramter will probably have to
    // be an array inthat case.. but for now......
    var allJars = collection.mutable.Set[String]()
    allJars = allJars + msgDef.jarName

    var jarPaths0 = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS").split(",").toSet
    jarPaths0 = jarPaths0 + MetadataAPIImpl.GetMetadataAPIConfig.getProperty("COMPILER_WORK_DIR")

    Utils.LoadJars(allJars.map(j => Utils.GetValidJarFile(jarPaths0, j)).toArray, loaderInfo.loadedJars, loaderInfo.loader)
    val jarName0 = Utils.GetValidJarFile(jarPaths0, msgDef.jarName)
    var classNames = Utils.getClasseNamesInJar(jarName0)

    var tempCurClass: Class[_] = null
    classNames.foreach(clsName => {
      try {
        Class.forName(clsName, true, loaderInfo.loader)
      } catch {
        case e: Exception => {
          logger.error("Failed to load Model class %s".format(clsName), e)
          throw e // Rethrow
        }
      }

      var curClz = Class.forName(clsName, true, loaderInfo.loader)
      tempCurClass = curClz

      var isMsg = false
      while (curClz != null && isMsg == false) {
        isMsg = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.MessageFactoryInterface")
        if (isMsg == false)
          curClz = curClz.getSuperclass()
      }

      if (isMsg) {
        try {
          // Trying Singleton Object
          val module = loaderInfo.mirror.staticModule(clsName)
          val obj = loaderInfo.mirror.reflectModule(module)
          objInst = obj.instance
          return objInst.asInstanceOf[com.ligadata.KamanjaBase.MessageFactoryInterface]
        } catch {
          case e: java.lang.NoClassDefFoundError => {
            logger.error("Class not found", e)
            throw e
          }
          case e: Exception => {
            logger.info("", e)
            objInst = tempCurClass.newInstance
            return objInst.asInstanceOf[com.ligadata.KamanjaBase.MessageFactoryInterface]
          }
        }
      }
    })
    return null
  }

  private def getPartition(key: Any, numPartitions: Int): Int = {
    val random = new java.util.Random
    if (key != null) {
      try {
        if (key.isInstanceOf[Array[Byte]]) {
          return (scala.math.abs(Arrays.hashCode(key.asInstanceOf[Array[Byte]])) % numPartitions)
        } else if (key.isInstanceOf[String]) {
          return (key.asInstanceOf[String].hashCode() % numPartitions)
        }
      } catch {
        case e: Exception => {
          logger.warn("", e)
        }
      }
    }
    return random.nextInt(numPartitions)
  }

  /**
   *
   */
  private def shutdown: Unit = {
    MetadataAPIImpl.shutdown
    if (producer != null)
      producer.close
   // if (zkc != null)
   //   zkc.close

    Thread.sleep(2000)
  }
}
