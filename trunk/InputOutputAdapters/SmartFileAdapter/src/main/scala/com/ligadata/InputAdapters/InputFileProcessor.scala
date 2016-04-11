package com.ligadata.InputAdapters

import java.util.zip.{ZipException, GZIPInputStream}
import com.ligadata.AdaptersConfiguration.{SmartFileAdapterConfiguration, FileAdapterMonitoringConfig, FileAdapterConnectionConfig}
import com.ligadata.Exceptions.{ MissingPropertyException }
import com.ligadata.KamanjaBase.{EnvContext, NodeContext}
import com.ligadata.MetadataAPI.MetadataAPIImpl
import com.ligadata.Utils.ClusterStatus
import com.ligadata.ZooKeeper.CreateClient
import com.ligadata.kamanja.metadata.MessageDef
import org.apache.curator.framework.CuratorFramework
import org.apache.logging.log4j.{ Logger, LogManager }
import org.json4s.jackson.JsonMethods._
import com.ligadata.kamanja.metadata.MdMgr._
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import util.control.Breaks._
import java.io._
import java.nio.file._
import scala.actors.threadpool.{ Executors, ExecutorService }
import scala.collection.mutable.PriorityQueue
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.Files.copy
import java.nio.file.Paths.get
import java.nio.file.{Path, FileSystems}
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.lang3.RandomStringUtils
import java.net.URLEncoder
import org.apache.tika.io.TikaInputStream
import org.apache.tika.metadata.Metadata
import org.apache.tika.detect.Detector
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.mime.MimeTypes
import scala.collection.mutable.Map
import java.net.URLDecoder
import org.apache.tika.Tika
import net.sf.jmimemagic.Magic
import net.sf.jmimemagic.MagicMatch
import net.sf.jmimemagic.MagicParseException
import net.sf.jmimemagic.MagicMatchNotFoundException
import net.sf.jmimemagic.MagicException


case class BufferLeftoversArea(workerNumber: Int, leftovers: Array[Byte], relatedChunk: Int)
case class BufferToChunk(len: Int, payload: Array[Byte], chunkNumber: Int, relatedFileHandler: SmartFileHandler, firstValidOffset: Int, isEof: Boolean, partMap: scala.collection.mutable.Map[Int,Int])
case class SmartFileMessage(msg: Array[Byte], offsetInFile: Int, isLast: Boolean, isLastDummy: Boolean, relatedFileHandler: SmartFileHandler, partMap: scala.collection.mutable.Map[Int,Int], msgOffset: Long)
case class FileStatus(status: Int, offset: Long, createDate: Long)
case class OffsetValue (lastGoodOffset: Int, partitionOffsets: Map[Int,Int])
case class EnqueuedFileHandler(fileHandler: SmartFileHandler, offset: Int, createDate: Long,  partMap: scala.collection.mutable.Map[Int,Int])

/**
  * This is the global area for the File Processor.  It basically handles File Access and Distribution !!!!!
 * the Individual File Processors ask here for what files it they should be processed.
  */
object FileProcessor {

  //private var watchService: WatchService = null
  //private var keys = new HashMap[WatchKey, Path]

  val DEBUG_MAIN_CONSUMER_THREAD_ACTION = 1000
  val NOT_RECOVERY_SITUATION = -1
  val BROKEN_FILE = -100
  val CORRUPT_FILE = -200
  val REFRESH_RATE = 10000
  val MAX_WAIT_TIME = 60000

  val KAFKA_SEND_SUCCESS = 0
  val KAFKA_SEND_Q_FULL = 1
  val KAFKA_SEND_DEAD_PRODUCER = 2
  val RECOVERY_DUMMY_START_TIME = 100

  // Possible statuses of files being processed
  val ACTIVE = 0
  val MISSING = 1
  val IN_PROCESS_FAILED = 2
  val FINISHED_FAILED_TO_COPY = 3
  val BUFFERING_FAILED = 4

  val HEALTHCHECK_TIMEOUT = 30000
  val maxRetry = 3

}


/**
  * Counter of buffers used by the FileProcessors... there is a limit on how much memory File Consumer can use up.
  */
object BufferCounters {
  val inMemoryBuffersCntr = new java.util.concurrent.atomic.AtomicLong()
}

/**
  *
  * @param partitionId
  */
//class FileProcessor(val path: Path, val partitionId: Int) extends Runnable {
class FileProcessor(val partitionId: Int) extends Runnable {

  private var zkc: CuratorFramework = null
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  var fileConsumers: ExecutorService = Executors.newFixedThreadPool(3)

  var isConsuming = true
  var isProducing = true

  private var workerBees: ExecutorService = null
  private var tempKMS = false
  // QUEUES used in file processing... will be synchronized.s
  private var msgQ: scala.collection.mutable.Queue[Array[SmartFileMessage]] = scala.collection.mutable.Queue[Array[SmartFileMessage]]()
  private var bufferQ: scala.collection.mutable.Queue[BufferToChunk] = scala.collection.mutable.Queue[BufferToChunk]()
  private var blg = new BufferLeftoversArea(-1, null, -1)

  private val msgQLock = new Object
  private val bufferQLock = new Object
  private val beeLock = new Object

  private var msgCount = 0

  // Confugurable Properties
  private var dirToWatch: String = ""
  private var dirToMoveTo: String = ""
  private var message_separator: Char = _
  private var field_separator: Char = _
  private var kv_separator: Char = _
  private var NUMBER_OF_BEES: Int = 2
  private var maxlen: Int = _
  private var partitionSelectionNumber: Int = _
  private var kafkaTopic = ""
  private var readyToProcessKey = ""
  private var maxBufAllowed: Long  = 0
  private var throttleTime: Int = 0
  private var isRecoveryOps = true

  //new configurable properties
  private var authUser : String = ""
  private var authPass : String = ""
  private var host : String = ""
  private var port = -1

  private var connectionConf : FileAdapterConnectionConfig = null
  private var monitoringConf :  FileAdapterMonitoringConfig = null
  private var adapterConfig : SmartFileAdapterConfiguration = null
  private var path = ArrayBuffer[Path]()
  private var smartFileMonitor : SmartFileMonitor = null

  //------------previously object variables------------
  private var fileCache: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map[String, String]()
  private var fileCacheLock = new Object
  // Files in Progress! and their statuses.
  //   Files can be in the following states:
  //    ACTIVE_PROCESSING, BUFFERING, FAILED_TO_MOVE, FAILED_TO_FINISH...
  private var activeFiles: scala.collection.mutable.Map[String, FileStatus] = scala.collection.mutable.Map[String, FileStatus]()
  private val activeFilesLock = new Object
  // READY TO PROCESS FILE QUEUES

  private val bufferingQLock = new Object
  private val zkRecoveryLock = new Object
  var errorWaitTime = 1000
  var reset_watcher = false
  var bufferTimeout: Int = 300000  // Default to 5 minutes
  var maxTimeFileAllowedToLive: Int = 3000  // default to 50 minutes.. will be multiplied by 1000 later
  var refreshRate: Int = 2000 //Refresh rate for monitorBufferingFiles and runFileWatcher methods
  var maxBufferErrors = 5
  var targetMoveDir: String = _
  private var initRecoveryLock = new Object
  private var numberOfReadyConsumers = 0
  private var isBufferMonitorRunning = false
  private var props: scala.collection.mutable.Map[String, String] = null
  var znodePath: String = ""
  var localMetadataConfig: String = ""
  var zkcConnectString: String = ""

  private var sendMsgToEngine : (SmartFileMessage, SmartFileConsumerContext) => Unit = null
  private var smartFileConsumerContext: SmartFileConsumerContext = null

  var globalFileMonitorService: ExecutorService = Executors.newFixedThreadPool(3)

  /**
    * Called by the Directory Listener to initialize
    * @param conf
    */
  def init(conf : SmartFileAdapterConfiguration, context: SmartFileConsumerContext,
           sendMsgCallback : (SmartFileMessage, SmartFileConsumerContext) => Unit): Unit = {

    adapterConfig = conf
    connectionConf = adapterConfig.connectionConfig
    monitoringConf = adapterConfig.monitoringConfig

    monitoringConf.locations.foreach(location => path += FileSystems.getDefault.getPath(location))

    sendMsgToEngine = sendMsgCallback
    smartFileConsumerContext = context

    startGlobalFileMonitor

    // Initialize threads
    /*try {
      kml = new KafkaMessageLoader(partitionId, props)
    } catch {
      case e: Exception => {
        logger.warn("", e)
        shutdown
        throw e
      }
    }*/
  }


  //-------------------------------------------------------------------------------------------------
  /**
    *
    */
  def initZookeeper: CuratorFramework = {
    try {
      zkcConnectString = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZOOKEEPER_CONNECT_STRING")
      logger.info("SMART_FILE_CONSUMER (global) Using zookeeper " + zkcConnectString)
      znodePath = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/smartFileConsumer"
      CreateClient.CreateNodeIfNotExists(zkcConnectString, znodePath)
      return CreateClient.createSimple(zkcConnectString)
    } catch {
      case e: Exception => {
        logger.error("SMART FILE CONSUMER (global): unable to connect to zookeeper using " + zkcConnectString, e )
        throw new Exception("Failed to start a zookeeper session with(" + zkcConnectString + ")", e)
      }
    }
  }

  //
  def addToZK (fileName: String, offset: Int, partitions: scala.collection.mutable.Map[Int,Int] = null) : Unit = {
    zkRecoveryLock.synchronized {
      var zkValue: String = ""
      logger.info("SMART_FILE_CONSUMER (global): Getting zookeeper info for "+ znodePath)

      logger.info("SMART_FILE_CONSUMER (MI): addToZK "+ fileName)
      CreateClient.CreateNodeIfNotExists(zkcConnectString, znodePath + "/" + URLEncoder.encode(fileName,"UTF-8"))
      zkValue = zkValue + offset.toString

      // Set up Partition data
      if (partitions == null) {
        zkValue = zkValue + ",[]"
      } else {
        zkValue = zkValue + ",["
        var isFirst = true
        partitions.keySet.foreach(key => {
          if (!isFirst) zkValue = zkValue + ";"
          var mapVal = partitions(key)
          zkValue = zkValue + key.toString + ":" + mapVal.toString
          isFirst = false
        })
        zkValue = zkValue + "]"
      }

      zkc.setData().forPath(znodePath + "/" + URLEncoder.encode(fileName,"UTF-8"), zkValue.getBytes)
    }
  }

  def removeFromZK (fileName: String): Unit = {
    zkRecoveryLock.synchronized {
      try {
        logger.info("SMART_FILE_CONSUMER (global): Removing file " + fileName + " from zookeeper")
        zkc.delete.forPath(znodePath + "/" + URLEncoder.encode(fileName,"UTF-8"))

      } catch {
        case e: Exception => logger.error("", e)
      }
    }
  }

  /**
    * checkIfFileBeingProcessed - if for some reason a file name is queued twice... this will prevent it
    * @param fileHandler
    * @return
    */
  def checkIfFileBeingProcessed(fileHandler : SmartFileHandler): Boolean = {
    fileCacheLock.synchronized {
      if (fileCache.contains(fileHandler.getFullPath)) {
        return true
      }
      else {
        fileCache(fileHandler.getFullPath) = scala.compat.Platform.currentTime +"::"+RandomStringUtils.randomAlphanumeric(10)
        return false
      }
    }
  }

  def fileCacheRemove(file: String): Unit = {
    fileCacheLock.synchronized {
      fileCache.remove(file)
    }
  }

  def getTimingFromFileCache (file: String): Long = {
    fileCacheLock.synchronized {
      fileCache(file).split("::")(0).toLong
    }
  }

  def getIDFromFileCache (file: String): String = {
    fileCacheLock.synchronized {
      fileCache(file)
    }
  }

  def markFileProcessing (fileName: String, offset: Int, createDate: Long): Unit = {
    activeFilesLock.synchronized{
      logger.info("SMART FILE CONSUMER (global): begin tracking the processing of a file " + fileName + "  begin at offset " + offset)
      var fs = new FileStatus(FileProcessor.ACTIVE, offset, createDate)
      activeFiles(fileName) = fs
    }
  }

  def markFileProcessingEnd (fileName: String): Unit = {
    activeFilesLock.synchronized{
      logger.info("SMART FILE CONSUMER: stop tracking the processing of a file " + fileName)
      activeFiles.remove(fileName)
    }
  }

  def setFileState(fileName: String, state: Int): Unit = {
    activeFilesLock.synchronized {
      logger.info("SMART FILE CONSUMER (global): set file state of file " + fileName + " to " + state)
      if (!activeFiles.contains(fileName)) {
        logger.warn("SMART FILE CONSUMER (global): Trying to set state for an unknown file "+ fileName)
      } else {
        val fs = activeFiles(fileName)
        val fs_new = new FileStatus(state, fs.offset, fs.createDate)
        activeFiles(fileName) = fs_new
      }
    }
  }


  // This only applies to the IN_PROCESS_FAILED
  def setFileOffset(fileName: String, offset: Long): Unit = {
    activeFilesLock.synchronized {
      logger.info("SMART FILE CONSUMER (global): changing intermal offset of a file " + fileName + " to " + offset)
      if (!activeFiles.contains(fileName)) {
        logger.warn("SMART FILE CONSUMER (global): Trying to set state for an unknown file "+ fileName)
      } else {
        val fs = activeFiles(fileName)
        val fs_new = new FileStatus(FileProcessor.IN_PROCESS_FAILED, scala.math.max(fs.offset,offset), fs.createDate)
        activeFiles(fileName) = fs_new
      }
    }
  }

  def getFileStatus(fileName: String): FileStatus = {
    activeFilesLock.synchronized{
      logger.info("SMART FILE CONSUMER (global): checking file in a list of active files " + fileName)
      if (!activeFiles.contains(fileName)) {
        logger.warn("SMART FILE CONSUMER (global): Trying to get status on unknown file " + fileName)
        return null
      }
      return activeFiles(fileName)
    }
  }





  // Code that deals with Buffering Files Queue is below..
  def startGlobalFileMonitor: Unit = {
    /*// If already started by a different File Consumer - return
    if (isBufferMonitorRunning) return
    isBufferMonitorRunning = true

    logger.info("SMART FILE CONSUMER (global): Initializing global queues")

    // Default to 5 minutes (value given in secopnds
    bufferTimeout = 1000 * monitoringConf.fileBufferingTimeout
    localMetadataConfig = monitoringConf.metadataConfigFile
    MetadataAPIImpl.InitMdMgrFromBootStrap(localMetadataConfig, false)
    zkc = initZookeeper

    //watchService = path.getFileSystem().newWatchService()
    //keys = new HashMap[WatchKey, Path]


    isBufferMonitorRunning = true
    globalFileMonitorService.execute(new Runnable() {
      override def run() = {
        monitorBufferingFiles
      }
    })

    globalFileMonitorService.execute(new Runnable() {
      override def run() = {
        runFileWatcher
      }
    })

    globalFileMonitorService.execute(new Runnable() {
      override def run() = {
        monitorActiveFiles
      }
    })*/
  }




  private def processExistingFiles(fileHandler: SmartFileHandler): Unit = {
    //this method is replaced by the monitors: simply detected new files should be enqueued using - FileProcessor.enQBufferedFile(file.toString)

    /*
    // Process all the existing files in the directory that are not marked complete.
    //logger.info("SMART FILE CONSUMER (MI): processExistingFiles on "+d.getAbsolutePath)

    // TODO:  Can this block the processoing????????
    if (fileHandler.exists() && fileHandler.isDirectory) {
      //Additional Filter Conditions, Ignore files starting with a . (period)
      val files = d.listFiles.filter(_.isFile)
        .filter(!_.getName.startsWith("."))
        .sortWith(_.lastModified < _.lastModified).toList
      files.foreach(file => {
        breakable {
          if (!Files.exists(file.toPath())) {
            logger.warn("SMART FILE CONSUMER (global): " + file.toString() + " does not exist in the  " + d.toString )
            break
          }
          if (!checkIfFileBeingProcessed(file.toString)) {
            FileProcessor.enQBufferedFile(file.toString)
          }
        }
      })
    }*/
  }



  /**
    *
    */
  private def runFileWatcher(): Unit = {
    /*try {
      // Lets see if we have failed previously on this partition Id, and need to replay some messages first.
      logger.info(" SMART FILE CONSUMER (global): Recovery operations, checking  => " + MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/smartFileConsumer")
      if (zkc.checkExists().forPath(MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/smartFileConsumer") != null) {
        var priorFailures = zkc.getChildren.forPath(MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/smartFileConsumer")
        if (priorFailures != null) {
          val map = priorFailures.toArray
          //var map = parse(new String(priorFailures)).values.asInstanceOf[Map[String, Any]]
          if (map != null) map.foreach(fileToReprocess => {
            logger.info("SMART FILE CONSUMER (global): Consumer  recovery of file " + URLDecoder.decode(fileToReprocess.asInstanceOf[String],"UTF-8"))

            val fileNameToRecover = URLDecoder.decode(fileToReprocess.asInstanceOf[String],"UTF-8")
            val fileHandlerToRecover = SmartFileHandlerFactory.createSmartFileHandler(adapterConfig, fileNameToRecover)

            if (!checkIfFileBeingProcessed(fileHandlerToRecover)
              //Additional check, see if it exists, possibility that it is moved but not updated in ZK
              //Should we be more particular and check in Processed directory ??? TODO
              && fileHandlerToRecover.exists()) {

              val offset = zkc.getData.forPath(znodePath + "/" + fileToReprocess.asInstanceOf[String])

              var recoveryInfo = new String(offset)
              logger.info("SMART FILE CONSUMER (global): " + fileNameToRecover + " from offset " + recoveryInfo)

              // There will always be 2 parts here.
              var partMap = scala.collection.mutable.Map[Int,Int]()
              var recoveryTokens = recoveryInfo.split(",")
              var parts = recoveryTokens(1).substring(1,recoveryTokens(1).size - 1)
              if (parts.size != 0) {
                var kvs = parts.split(";")
                kvs.foreach(kv => {
                  var pair = kv.split(":")
                  partMap(pair(0).toInt) = pair(1).toInt
                })
              }


              //Start Changes -- Instead of a single file, run with the ArrayBuffer of Paths
              enQFile(fileHandlerToRecover,recoveryTokens(0).toInt, FileProcessor.RECOVERY_DUMMY_START_TIME, partMap)

              //TODO : yasser : looks like this code waits for the files to get deleted/moved, must rewrite in another way
              /*for(dir <- path){
                if(dir.toFile().exists() && dir.toFile().isDirectory()){
                  var files = dir.toFile().listFiles.filter(file => {
                    file.isFile && (file.getName).equals(fileToRecover)
                  })
                  while (files.size != 0) {
                    Thread.sleep(1000)
                    files = dir.toFile().listFiles.filter(file => {
                      file.isFile && (file.getName).equals(fileToRecover)
                    })
                  }
                }
              }*/
              //End Changes -- Instead of a single file, run with the ArrayBuffer of Paths

            }else if(!fileHandlerToRecover.exists()){
              //Check if file is already moved, if yes remove from ZK
              val tokenName = fileNameToRecover.split("/")
              if(Files.exists(Paths.get(targetMoveDir+"/"+tokenName(tokenName.size - 1)))){
                logger.info("SMART FILE CONSUMER (global): Found file " +fileNameToRecover+" processed ")
                removeFromZK(fileNameToRecover)
              }else{
                logger.info("SMART FILE CONSUMER (global): File possibly moved out manually " +fileNameToRecover)
                removeFromZK(fileNameToRecover)
              }

            } else {
              logger.info("SMART FILE CONSUMER (global): " +fileNameToRecover+" already being processed ")
            }
          })
        }
      }

      logger.info("SMART FILE CONSUMER (global): Consumer Continuing Startup process, checking for existing files")

      //TODO C&S- No need to process here just before entering the loop
      //If need is to exit, do a shutdown on error
      //for(dir <- path)
      //processExistingFiles(dir.toFile())


      logger.info("SMART_FILE_CONSUMER partition Initialization complete  Monitoring specified directory for new files")
      // Begin the listening process, TAKE()

      /*//instead of calling processExistingFiles repeatedly, use the monitor
      smartFileMonitor = SmartFileMonitorFactory.createSmartFileMonitor(adapterConfig.Name, adapterConfig._type, processFile)
      smartFileMonitor.init(adapterConfig.adapterSpecificCfg)
      smartFileMonitor.monitor()*/

    }  catch {
      case ie: InterruptedException => logger.error("InterruptedException: " + ie)
      case ioe: IOException         => logger.error("Unable to find the directory to watch, Shutting down File Consumer", ioe)
      case e: Exception             => logger.error("Exception: ", e)
    }*/
  }


  /*private def resetWatcher: Unit = {
    watchService.close()
    watchService = path.getFileSystem().newWatchService()
    keys = new HashMap[WatchKey, Path]
    register(path)
  }*/

  /**
    * Register a particular file or directory to be watched
    */
  /*private def register(dir: Path): Unit = {
    val key = dir.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_MODIFY, StandardWatchEventKinds.OVERFLOW)
    keys(key) = dir
  }*/


  private def monitorActiveFiles: Unit = {
    //TODO :yasser:this method basically handles existing files. this is handled now by the monitors. must check these points
    //-missingFiles
    //-failedFiles
    //-unmovedFiles


    /*var afterErrorConditions = false

    while(true) {
      var isWatchedFileSystemAccesible = true
      var isTargetFileSystemAccesible = true
      var d1: File = null
      logger.info("SMART FILE CONSUMER (global): Scanning for problem files")

      // Watched Directory must be available! NO EXCEPTIONS.. if not, then we need to recreate a file watcher.
      try {
        // DirToWatch is required, TargetMoveDir is  not...
        //d1 = new File(dirToWatch)
        //isWatchedFileSystemAccesible = (d1.canRead && d1.canWrite)
        for(dirName <- dirToWatch.split(System.getProperty("path.separator"))){
          d1 = new File(dirName)
          isWatchedFileSystemAccesible = isWatchedFileSystemAccesible && (d1.canRead && d1.canWrite)
        }
      } catch {
        case fio: IOException => {
          isWatchedFileSystemAccesible = false
        }
      }

      // Target Directory must be available if it is used (it may not be). If it is not, we dont need to restart the watcher
      try {
        var d2: File = null
        if (targetMoveDir != null) {
          d2 = new File(targetMoveDir)
          isTargetFileSystemAccesible = (d2.canRead && d2.canWrite)
        }
      } catch {
        case fio: IOException => {
          isTargetFileSystemAccesible = false
        }
      }


      if (isWatchedFileSystemAccesible && isTargetFileSystemAccesible) {
        logger.info("SMART FILE CONSUMER (global): File system is accessible, perform cleanup for problem files")
        if (afterErrorConditions) {
          try {
            for(dirName <- dirToWatch.split(System.getProperty("path.separator"))){
              d1 = new File(dirName)
              processExistingFiles(d1)
            }
            afterErrorConditions = false
          } catch {
            case e: IOException => {
              // Interesting,  we have just died, and need to move the newly added files to the queue
              logger.warn("SMART FILE CONSUMER (gloabal): Unable to connect to watch directory, will try to shut it down and recreate again")
              isWatchedFileSystemAccesible = false
              afterErrorConditions = true
            }
          }
        }

        if (isWatchedFileSystemAccesible) {
          val missingFiles = getFailedFiles(MISSING)
          missingFiles.foreach(file => FileProcessor.enQFile(file._1, file._2.asInstanceOf[FileStatus].offset.asInstanceOf[Int], file._2.asInstanceOf[FileStatus].createDate))

          val failedFiles = getFailedFiles(IN_PROCESS_FAILED)
          failedFiles.foreach(file => FileProcessor.enQFile(file._1, file._2.asInstanceOf[FileStatus].offset.asInstanceOf[Int], FileProcessor.RECOVERY_DUMMY_START_TIME))

          val unmovedFiles = getFailedFiles(FINISHED_FAILED_TO_COPY)
          unmovedFiles.foreach(file => {completeFile(file._1)})
        }
      } else {
        if (!isWatchedFileSystemAccesible) {
          afterErrorConditions = true
          logger.warn("SMART FILE CONSUMER (gloabal): Unable to access a watched directory, will try to shut it down and recreate")
        }
        logger.warn("SMART FILE CONSUMER (gloabal): File system is not accessible")
      }
      Thread.sleep(HEALTHCHECK_TIMEOUT)
    }*/
  }


  private def getFailedFiles (fileType: Int): Map[String,FileStatus] = {
    var returnMap: Map[String,FileStatus] = Map[String,FileStatus]()
    activeFilesLock.synchronized {
      val iter = activeFiles.iterator
      while(iter.hasNext) {
        var file = iter.next
        val cStatus = file._2
        if (cStatus.status == fileType) {
          returnMap += file
        }
      }
    }
    if (returnMap.size > 0) {
      logger.warn("SMART FILE CONSUMER (gloabal): Tracking issue for file type " + fileType)
      logger.warn("SMART FILE CONSUMER (gloabal): " + returnMap.toString)
      returnMap.foreach(item => {
        setFileState(item._1, FileProcessor.ACTIVE)
      })
    }
    return returnMap
  }

  // This gets called inthe error case by the recovery logic
  // in normal cases, the KafkaMessafeLoader will handle the completing the file.
  private def completeFile (fileHandler: SmartFileHandler): Unit = {
    try {
      logger.info("SMART FILE CONSUMER {global): - cleaning up after " + fileHandler.getFullPath)
      // Either move or rename the file.
      moveFile(fileHandler)
      markFileProcessingEnd(fileHandler.getFullPath)
      removeFromZK(fileHandler.getFullPath)
    } catch {
      case ioe: IOException => {
        logger.error("Exception moving the file ",ioe)
        setFileState(fileHandler.getFullPath,FileProcessor.FINISHED_FAILED_TO_COPY)

      }
    }
  }

  private def moveFile(fileHandler: SmartFileHandler): Unit = {
    val fileStruct = fileHandler.getFullPath.split("/")
    logger.info("SMART FILE CONSUMER Moving File" + fileHandler.getFullPath+ " to " + targetMoveDir)
    if (fileHandler.exists()) {
      fileHandler.moveTo(targetMoveDir + "/" + fileStruct(fileStruct.size - 1))
      fileCacheRemove(fileHandler.getFullPath)
    } else {
      logger.warn("SMART FILE CONSUMER File has been deleted" + fileHandler.getFullPath);
    }
  }
  //---------------------------------------------------------------------------------------------------------------------

  private def enQMsg(buffer: Array[SmartFileMessage], bee: Int): Unit = {
    msgQLock.synchronized {
      msgQ += buffer
    }
  }

  private def deQMsg(): Array[SmartFileMessage] = {
    msgQLock.synchronized {
      if (msgQ.isEmpty) {
        return null
      }
      BufferCounters.inMemoryBuffersCntr.decrementAndGet() // Incrementing when we enQBuffer and Decrementing when we deQMsg
      return msgQ.dequeue
    }
  }

  private def enQBuffer(buffer: BufferToChunk): Unit = {
    bufferQLock.synchronized {
      bufferQ += buffer
    }
  }

  private def deQBuffer(bee: Int): BufferToChunk = {
    bufferQLock.synchronized {
      if (bufferQ.isEmpty) {
        return null
      }
      return bufferQ.dequeue
    }
  }

  private def getLeftovers(code: Int): BufferLeftoversArea = {
    beeLock.synchronized {
      return blg
    }
  }

  private def setLeftovers(in: BufferLeftoversArea, code: Int) = {
    beeLock.synchronized {
      blg = in
    }
  }

  /**
    * Each worker bee will run this code... looking for work to do.
    * @param beeNumber
    */
  private def processBuffers(beeNumber: Int) = {

    var msgNum: Int = 0
    var myLeftovers: BufferLeftoversArea = null
    var buffer: BufferToChunk = null;
    var fileHandlerToProcess: SmartFileHandler = null
    var isEofBuffer = false

    // basically, keep running until shutdown.
    while (isConsuming) {
      var messages: scala.collection.mutable.LinkedHashSet[SmartFileMessage] = null
      var leftOvers: Array[Byte] = new Array[Byte](0)

      // Try to get a new file to process.
      buffer = deQBuffer(beeNumber)

      // If the buffer is there to process, do it
      if (buffer != null) {
        // If the new file being processed,  offsets to messages in this file to 0.
        if (fileHandlerToProcess == null || !fileHandlerToProcess.getFullPath.equalsIgnoreCase(buffer.relatedFileHandler.getFullPath)) {
          msgNum = 0
          fileHandlerToProcess = buffer.relatedFileHandler
          isEofBuffer = false
        }

        // need a ordered structure to keep the messages.
        messages = scala.collection.mutable.LinkedHashSet[SmartFileMessage]()

        var indx = 0
        var prevIndx = indx

        isEofBuffer = buffer.isEof
        if (buffer.firstValidOffset <= FileProcessor.BROKEN_FILE) {
          // Broken File is recoverable, CORRUPTED FILE ISNT!!!!!
          if (buffer.firstValidOffset == FileProcessor.BROKEN_FILE) {
            logger.error("SMART FILE CONSUMER (" + partitionId + "): Detected a broken file")
            messages.add(new SmartFileMessage(Array[Byte](), FileProcessor.BROKEN_FILE, true, true, buffer.relatedFileHandler, buffer.partMap, FileProcessor.BROKEN_FILE))
          } else {
            logger.error("SMART FILE CONSUMER (" + partitionId + "): Detected a broken file")
            messages.add(new SmartFileMessage(Array[Byte](), FileProcessor.CORRUPT_FILE, true, true, buffer.relatedFileHandler, buffer.partMap, FileProcessor.CORRUPT_FILE))
          }
        } else {
          // Look for messages.
          if (!buffer.isEof){
            buffer.payload.foreach(x => {
              if (x.asInstanceOf[Char] == message_separator) {
                var newMsg: Array[Byte] = buffer.payload.slice(prevIndx, indx)
                msgNum += 1
                logger.debug("SMART_FILE_CONSUMER (" + partitionId + ") Message offset " + msgNum + ", and the buffer offset is " + buffer.firstValidOffset)

                // Ok, we could be in recovery, so we have to ignore some messages, but these ignoraable messages must still
                // appear in the leftover areas
                messages.add(new SmartFileMessage(newMsg, buffer.firstValidOffset, false, false, buffer.relatedFileHandler,  buffer.partMap, prevIndx))

                prevIndx = indx + 1
              }
              indx = indx + 1
              
            })
          }
        }

        // Wait for a previous worker be to finish so that we can get the leftovers.,, If we are the first buffer, then
        // just publish
        if (buffer.chunkNumber == 0) {
          enQMsg(messages.toArray, beeNumber)
        }

        var foundRelatedLeftovers = false
        while (!foundRelatedLeftovers && buffer.chunkNumber != 0) {
          myLeftovers = getLeftovers(beeNumber)
          if (myLeftovers.relatedChunk == (buffer.chunkNumber - 1)) {
            
            leftOvers = myLeftovers.leftovers
            foundRelatedLeftovers = true

            // Prepend the leftovers to the first element of the array of messages
            val msgArray = messages.toArray
            var firstMsgWithLefovers: SmartFileMessage = null
            if (isEofBuffer) {
              if (leftOvers.size > 0) {
                firstMsgWithLefovers = new SmartFileMessage(leftOvers, buffer.firstValidOffset, false, false, buffer.relatedFileHandler, buffer.partMap, buffer.firstValidOffset)
                messages.add(firstMsgWithLefovers)
                enQMsg(messages.toArray, beeNumber)
              }
            } else {
              if (messages.size > 0) {
                firstMsgWithLefovers = new SmartFileMessage(leftOvers ++ msgArray(0).msg, msgArray(0).offsetInFile, false, false, buffer.relatedFileHandler, msgArray(0).partMap,
                  msgArray(0).offsetInFile)
                msgArray(0) = firstMsgWithLefovers
                enQMsg(msgArray, beeNumber)
              }
            }
          } else {
            Thread.sleep(100)
          }
        }

        // whatever is left is the leftover we need to pass to another thread.
        indx = scala.math.min(indx, buffer.len)
        
        if (indx != prevIndx) {
          
          if (!isEofBuffer) {
            val newFileLeftOvers = BufferLeftoversArea(beeNumber, buffer.payload.slice(prevIndx, indx), buffer.chunkNumber)
            setLeftovers(newFileLeftOvers, beeNumber)
          } else {
            val newFileLeftOvers = BufferLeftoversArea(beeNumber, new Array[Byte](0), buffer.chunkNumber)
            setLeftovers(newFileLeftOvers, beeNumber)
          }

        } else {
          val newFileLeftOvers = BufferLeftoversArea(beeNumber, new Array[Byte](0), buffer.chunkNumber)
          setLeftovers(newFileLeftOvers, beeNumber)
        }

      } else {
        // Ok, we did not find a buffer to process on the BufferQ.. wait.
        Thread.sleep(100)
      }
    }
  }

  /**
    * This will be run under a CONSUMER THREAD.
    * @param file
    */
  private def readBytesChunksFromFile(file: EnqueuedFileHandler): Unit = {

    val byteBuffer = new Array[Byte](maxlen)

    var readlen = 0
    var len: Int = 0
    var totalLen = 0
    var chunkNumber = 0

    val fileName = file.fileHandler.getFullPath
    val offset = file.offset
    val partMap = file.partMap
    val fileHandler = file.fileHandler

    // Start the worker bees... should only be started the first time..
    if (workerBees == null) {
      workerBees = Executors.newFixedThreadPool(NUMBER_OF_BEES)
      for (i <- 1 to NUMBER_OF_BEES) {
        workerBees.execute(new Runnable() {
          override def run() = {
            processBuffers(i)
          }
        })
      }
    }

    // Grab the InputStream from the file and start processing it.  Enqueue the chunks onto the BufferQ for the
    // worker bees to pick them up.

    //var bis: BufferedReader = null
    try {
/*      if (isCompressed(fileName)) {
        bis = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(fileName))))
      } else {
        bis = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)))
      }*/
      fileHandler.openForRead()
    } catch {
      // Ok, sooo if the file is not Found, either someone moved the file manually, or this specific destination is not reachable..
      // We just drop the file, if it is still in the directory, then it will get picked up and reprocessed the next tick.
      case fio: java.io.FileNotFoundException => {
         logger.error("SMART_FILE_CONSUMER (" + partitionId + ") Exception accessing the file for processing the file - File is missing",fio)
         markFileProcessingEnd(fileName)
         fileCacheRemove(fileName)
         return
      }
      case fio: IOException => {
        logger.error("SMART_FILE_CONSUMER (" + partitionId + ") Exception accessing the file for processing the file ",fio)
        setFileState(fileName,FileProcessor.MISSING)
        return
      }
    }

    // Intitialize the leftover area for this file reading.
    var newFileLeftOvers = BufferLeftoversArea(0, Array[Byte](), -1)
    setLeftovers(newFileLeftOvers, 0)

    var waitedCntr = 0
    var tempFailure = 0
    do {
      waitedCntr = 0
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
      }

      BufferCounters.inMemoryBuffersCntr.incrementAndGet() // Incrementing when we enQBuffer and Decrementing when we deQMsg
      var isLastChunk = false
      try {
        readlen = fileHandler.read(byteBuffer, maxlen - 1)
        // if (readlen < (maxlen - 1)) isLastChunk = true
      } catch {
        case ze: ZipException => {
          logger.error("Failed to read file, file currupted " + fileName, ze)
          //val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.CORRUPT_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)
          return
        }
        case ioe: IOException => {
          logger.error("Failed to read file " + fileName, ioe)
          val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.BROKEN_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)
          return
        }
        case e: Exception => {
          logger.error("Failed to read file, file corrupted " + fileName, e)
          val buffer = MonitorUtils.toCharArray(byteBuffer)
          val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, FileProcessor.CORRUPT_FILE, isLastChunk, partMap)
          enQBuffer(GenericBufferToChunk)
          return
        }
      }
      if (readlen > 0) {
        totalLen += readlen
        len += readlen
        val buffer = MonitorUtils.toCharArray(byteBuffer)
        val GenericBufferToChunk = new BufferToChunk(readlen, byteBuffer.slice(0, readlen), chunkNumber, fileHandler, offset, isLastChunk, partMap)
        enQBuffer(GenericBufferToChunk)
        chunkNumber += 1
      } else {
        val GenericBufferToChunk = new BufferToChunk(readlen, Array[Byte](message_separator.toByte), chunkNumber, fileHandler, offset, true, partMap)
        enQBuffer(GenericBufferToChunk)
        chunkNumber += 1
      }

    } while (readlen > 0)

    // Pass the leftovers..  - some may have been left by the last chunkBuffer... nothing else will pick it up...
    // make it a KamfkaMessage buffer.
    var myLeftovers: BufferLeftoversArea = null
    var foundRelatedLeftovers = false
    while (!foundRelatedLeftovers) {
      myLeftovers = getLeftovers(FileProcessor.DEBUG_MAIN_CONSUMER_THREAD_ACTION)
      // if this is for the last chunk written...
      if (myLeftovers.relatedChunk == (chunkNumber - 1)) {
        // EnqMsg here.. but only if there is something in there.
        if (myLeftovers.leftovers.size > 0) {
          // Not sure how we got there... this should not happen.
          logger.warn("SMART FILE CONSUMER: partition " + partitionId + ": NON-EMPTY final leftovers, this really should not happend... check the file ")
        } else {
          val messages: scala.collection.mutable.LinkedHashSet[SmartFileMessage] = scala.collection.mutable.LinkedHashSet[SmartFileMessage]()
          messages.add(new SmartFileMessage(null, 0, true, true, fileHandler, scala.collection.mutable.Map[Int,Int](), 0))
          enQMsg(messages.toArray, 1000)
        }
        foundRelatedLeftovers = true
      } else {
        Thread.sleep(100)
      }
      
    }
    // Done with this file... mark is as closed
    try {
      // markFileAsFinished(fileName)
      if (fileHandler != null) fileHandler.close
      //bis = null
    } catch {
      case ioe: IOException => {
        logger.warn("SMART FILE CONSUMER: partition " + partitionId + " Unable to detect file as being processed " + fileName)
        logger.warn("SMART FILE CONSUMER: Check to make sure the input directory does not still contain this file " + ioe)
      }
    }

  }

  /**
    *  This is the "FILE CONSUMER"
    */
  private def doSomeConsuming(): Unit = {
   /* while (isConsuming) {
      val fileToProcess = deQFile
      var curTimeStart: Long = 0
      var curTimeEnd: Long = 0
      if (fileToProcess == null) {
        Thread.sleep(500)
      } else {
        logger.info("SMART_FILE_CONSUMER partition " + partitionId + " Processing file " + fileToProcess)
        
        //val tokenName = fileToProcess.name.split("/")
        //FileProcessor.markFileProcessing(tokenName(tokenName.size - 1), fileToProcess.offset, fileToProcess.createDate)
        markFileProcessing(fileToProcess.fileHandler.getFullPath, fileToProcess.offset, fileToProcess.createDate)
        
        curTimeStart = System.currentTimeMillis
        try {
          readBytesChunksFromFile(fileToProcess)
        } catch {
          case fnfe: Exception => {
            logger.warn("Exception Encountered, check the logs.",fnfe)
          }
        }
        curTimeEnd = System.currentTimeMillis
      }
    }*/
  }

  /**
    * This is a "PUSHER" file.
    */
  private def doSomePushing(): Unit = {
    while (isProducing) {
      var msgs = deQMsg
      if (msgs == null) {
        Thread.sleep(250)
      } else {
        logger.debug("---------------doSomePushing, dequeued following messages----------------------");
        msgs.foreach(m => logger.debug("          " + new String(m.msg)))
        logger.debug("----------------------------------------------------------------");
        //push using kafka msg loader
        //kml.pushData(msg)
        msgs.foreach(msg => sendMsgToEngine(msg, smartFileConsumerContext))
        msgs = null
      }
    }
  }


  /**
    * The main directory watching thread
    */
  override def run(): Unit = {
    // Initialize and launch the File Processor thread(s), and kafka producers
    fileConsumers.execute(new Runnable() {
      override def run() = {
        doSomeConsuming
      }
    })

    fileConsumers.execute(new Runnable() {
      override def run() = {
        doSomePushing
      }
    })
  }


  /**
    *
    */
  private def shutdown: Unit = {
    isConsuming = false
    isProducing = false

    if(smartFileMonitor != null)
      smartFileMonitor.shutdown()

    if (fileConsumers != null) {
      fileConsumers.shutdown()
    }
    MetadataAPIImpl.shutdown
    if (zkc != null)
      zkc.close
    Thread.sleep(2000)
  }


}