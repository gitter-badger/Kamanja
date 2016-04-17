package com.ligadata.InputAdapters

import java.io.IOException
import com.ligadata.AdaptersConfiguration.SmartFileAdapterConfiguration
import org.apache.logging.log4j.LogManager

import scala.actors.threadpool.Executors

/**
  *
  *
  * @param adapterConfig
  * @param fileHandler file to read messages from
  * @param startOffset offset in the file to start with
  * @param consumerContext has required params
  * @param messageFoundCallback to call for every read message
  * @param finishCallback call when finished reading
  */
class FileMessageExtractor(adapterConfig : SmartFileAdapterConfiguration,
                           fileHandler: SmartFileHandler,
                           startOffset : Int,
                           consumerContext : SmartFileConsumerContext,
                           messageFoundCallback : (SmartFileMessage, SmartFileConsumerContext) => Unit,
                           finishCallback : (SmartFileHandler, SmartFileConsumerContext) => Unit ) {

  private val maxlen: Int = adapterConfig.monitoringConfig.workerBufferSize * 1024 * 1024 //in MB
  private var message_separator : Char = adapterConfig.monitoringConfig.messageSeparator

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  private var msgNum = 0
  private var globalOffset = 0

  private val executor = Executors.newFixedThreadPool(2)

  private var finished = false



  def extractMessages() : Unit = {
    //just run it in a separate thread
    val extractorThread = new Runnable() {
      override def run(): Unit = {
        readBytesChunksFromFile()
      }
    }
    executor.execute(extractorThread)

    //keep updating status so leader knows participant is working fine
    val statusUpdateThread = new Runnable() {
      override def run(): Unit = {
        while(!finished){
          //put filename~offset~timestamp
          val data = fileHandler.getFullPath + "~" + globalOffset + "~" + System.nanoTime
          logger.debug("SMART FILE CONSUMER - Node {} with partition {} is updating status to value {}",
            consumerContext.nodeId, consumerContext.partitionId.toString, data)
          consumerContext.envContext.saveConfigInClusterCache(consumerContext.statusUpdateCacheKey, data.getBytes)

          Thread.sleep(consumerContext.statusUpdateInterval)
        }
      }
    }
    executor.execute(statusUpdateThread)
  }

  private def readBytesChunksFromFile(): Unit = {

    val byteBuffer = new Array[Byte](maxlen)

    var readlen = 0
    var len: Int = 0
    var totalLen = 0
    var chunkNumber = 0

    val fileName = fileHandler.getFullPath

    logger.info("Smart File Consumer - Starting reading messages from file " + fileName)

    //val offset = file.offset
    //val partMap = file.partMap
    //val fileHandler = file.fileHandler

    // Grab the InputStream from the file and start processing it.  Enqueue the chunks onto the BufferQ for the
    // worker bees to pick them up.

    try {
      fileHandler.openForRead()
    } catch {
      // Ok, sooo if the file is not Found, either someone moved the file manually, or this specific destination is not reachable..
      // We just drop the file, if it is still in the directory, then it will get picked up and reprocessed the next tick.
      case fio: java.io.FileNotFoundException => {
        logger.error("SMART_FILE_CONSUMER Exception accessing the file for processing the file - File is missing",fio)
        //markFileProcessingEnd(fileName)
        //fileCacheRemove(fileName)
        return
      }
      case fio: IOException => {
        logger.error("SMART_FILE_CONSUMER Exception accessing the file for processing the file ",fio)
        //setFileState(fileName,FileProcessor.MISSING)
        return
      }
    }

    // Intitialize the leftover area for this file reading.
    var newFileLeftOvers = BufferLeftoversArea(0, Array[Byte](), -1)
    //setLeftovers(newFileLeftOvers, 0)

    var waitedCntr = 0
    var tempFailure = 0

    var previousLeftOverBytes : Array[Byte] = Array()
    do {
      /*waitedCntr = 0
      val st = System.currentTimeMillis
      while ((BufferCounters.inMemoryBuffersCntr.get * 2 + partitionSelectionNumber + 2) * maxlen > maxBufAllowed) { // One counter for bufferQ and one for msgQ and also taken concurrentKafkaJobsRunning and 2 extra in memory
        if (waitedCntr == 0) {
          logger.warn("SMART FILE ADDAPTER (" + partitionId + ") : exceed the allowed memory size (%d) with %d buffers. Halting for free slot".format(maxBufAllowed,
            BufferCounters.inMemoryBuffersCntr.get * 2))
        }
        waitedCntr += 1
        Thread.sleep(throttleTime)
      }

      if (waitedCntr > 0) {
        val timeDiff = System.currentTimeMillis - st
        logger.warn("%d:Got slot after waiting %dms".format(partitionId, timeDiff))
      }*/

      BufferCounters.inMemoryBuffersCntr.incrementAndGet() // Incrementing when we enQBuffer and Decrementing when we deQMsg
      var isLastChunk = false
      try {
        readlen = fileHandler.read(byteBuffer, maxlen - 1)
        // if (readlen < (maxlen - 1)) isLastChunk = true
      } catch {
        /*case ze: ZipException => {
          logger.error("Failed to read file, file currupted " + fileName, ze)
          //val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.CORRUPT_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)
          return
        }*/
        case ioe: IOException => {
          logger.error("Failed to read file " + fileName, ioe)
          /*val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.BROKEN_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)*/
          return
        }
        case e: Exception => {
          logger.error("Failed to read file, file corrupted " + fileName, e)
          /*val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.CORRUPT_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)*/
          return
        }
      }
      if (readlen > 0) {
        totalLen += readlen
        len += readlen
        val buffer = MonitorUtils.toCharArray(byteBuffer)
        //val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, offset, isLastChunk, partMap)
        val bytesToExtract = if(previousLeftOverBytes.length == 0) byteBuffer.slice(0, readlen) else previousLeftOverBytes ++ byteBuffer.slice(0, readlen)
        val result = extractMessages(bytesToExtract)
        previousLeftOverBytes = result

      }

    } while (readlen > 0)

    if(previousLeftOverBytes.length > 0){
      val lastMsg = previousLeftOverBytes
      if(lastMsg.length == 1 && lastMsg(0) == message_separator){
       //only separator is left
      }
      else{
        msgNum += 1
        //println(s"*************** last message ($msgNum): " + new String(lastMsg))
        if(globalOffset >= startOffset) {
          val msgOffset = globalOffset // offset of the message in the file
          val smartFileMessage = new SmartFileMessage(lastMsg, msgOffset, false, false, fileHandler, null, msgOffset)
          messageFoundCallback(smartFileMessage, consumerContext)
        }

        //a message is extracted to passed to engine, update offset in cache
        globalOffset = globalOffset + lastMsg.length
        //consumerContext.envContext.saveConfigInClusterCache(consumerContext.fileOffsetCacheKey, globalOffset.toString.getBytes)

      }
    }

    // Done with this file... mark is as closed
    try {
      // markFileAsFinished(fileName)
      finished = true
      executor.shutdown()
      if (fileHandler != null) fileHandler.close

      if(finishCallback != null)
        finishCallback(fileHandler, consumerContext)
      //bis = null
    } catch {
      case ioe: IOException => {
        logger.warn("SMART FILE CONSUMER: Unable to detect file as being processed " + fileName)
        logger.warn("SMART FILE CONSUMER: Check to make sure the input directory does not still contain this file " + ioe)
      }
    }

  }

  private def extractMessages(chunk : Array[Byte]) : Array[Byte] = {
    var indx = 0
    var prevIndx = indx


    chunk.foreach(x => {
      if (x.asInstanceOf[Char] == message_separator) {
        val newMsg: Array[Byte] = chunk.slice(prevIndx, indx - 1)
        if(newMsg.length > 0) {
          msgNum += 1
          //println(s"*************** new message ($msgNum): " + new String(newMsg))
          if(globalOffset >= startOffset) {//send messages that are only after startOffset
            val msgOffset = globalOffset // offset of the message in the file
            val smartFileMessage = new SmartFileMessage(newMsg, msgOffset, false, false, fileHandler, null, msgOffset)
            messageFoundCallback(smartFileMessage, consumerContext)

            //a message is extracted to passed to engine, update offset in cache
            //consumerContext.envContext.saveConfigInClusterCache(consumerContext.fileOffsetCacheKey, globalOffset.toString.getBytes)
          }
          prevIndx = indx + 1
          globalOffset = globalOffset + newMsg.length + 1 //(1 for separator length)
        }
      }
      indx = indx + 1
    })

    if(prevIndx == chunk.length)
      Array()
    else
      chunk.slice(prevIndx, chunk.length)
  }
}
