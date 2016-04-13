package com.ligadata.InputAdapters

import java.io.IOException
import scala.actors.threadpool.TimeUnit
import java.util.zip.ZipException

import com.ligadata.HeartBeat.MonitorComponentInfo
import com.ligadata.InputOutputAdapterInfo._
import com.ligadata.AdaptersConfiguration._
import com.ligadata.KamanjaBase.{EnvContext, NodeContext, DataDelimiters}
import com.ligadata.Utils.ClusterStatus
import org.apache.logging.log4j.LogManager
import org.json4s.jackson.Serialization

import scala.actors.threadpool.{Executors, ExecutorService}
import scala.collection.mutable.{Map, MultiMap, HashMap, ArrayBuffer}

case class BufferLeftoversArea(workerNumber: Int, leftovers: Array[Byte], relatedChunk: Int)
case class BufferToChunk(len: Int, payload: Array[Byte], chunkNumber: Int, relatedFileHandler: SmartFileHandler, firstValidOffset: Int, isEof: Boolean, partMap: scala.collection.mutable.Map[Int,Int])
case class SmartFileMessage(msg: Array[Byte], offsetInFile: Int, isLast: Boolean, isLastDummy: Boolean, relatedFileHandler: SmartFileHandler, partMap: scala.collection.mutable.Map[Int,Int], msgOffset: Long)
case class FileStatus(status: Int, offset: Long, createDate: Long)
case class OffsetValue (lastGoodOffset: Int, partitionOffsets: Map[Int,Int])
case class EnqueuedFileHandler(fileHandler: SmartFileHandler, offset: Int, createDate: Long,  partMap: scala.collection.mutable.Map[Int,Int])


class SmartFileConsumerContext{
  var partitionId: Int = _
  var ignoreFirstMsg: Boolean = _
  var nodeId : String = _
  var envContext : EnvContext = null
  //var fileOffsetCacheKey : String = _
  var statusUpdateCacheKey : String = _
  var statusUpdateInterval : Int = _
}

/**
  * Counter of buffers used by the FileProcessors... there is a limit on how much memory File Consumer can use up.
  */
object BufferCounters {
  val inMemoryBuffersCntr = new java.util.concurrent.atomic.AtomicLong()
}

/**
  * Created by Yasser on 3/13/2016.
  */
object SmartFileConsumer extends InputAdapterFactory {
  val MONITOR_FREQUENCY = 10000 // Monitor Topic queues every 20 seconds
  val SLEEP_DURATION = 1000 // Allow 1 sec between unsucessful fetched
  var CURRENT_BROKER: String = _
  val FETCHSIZE = 64 * 1024
  val ZOOKEEPER_CONNECTION_TIMEOUT_MS = 3000
  val MAX_TIMEOUT = 60000
  val INIT_TIMEOUT = 250
  val ADAPTER_DESCRIPTION = "Smart File Consumer"

  def CreateInputAdapter(inputConfig: AdapterConfiguration, execCtxtObj: ExecContextFactory, nodeContext: NodeContext): InputAdapter = new SmartFileConsumer(inputConfig, execCtxtObj, nodeContext)
}

class SmartFileConsumer(val inputConfig: AdapterConfiguration, val execCtxtObj: ExecContextFactory, val nodeContext: NodeContext) extends InputAdapter {

  val input = this
  lazy val loggerName = this.getClass.getName
  lazy val LOG = LogManager.getLogger(loggerName)

  private val lock = new Object()
  private var readExecutor: ExecutorService = _

  private val adapterConfig = SmartFileAdapterConfiguration.getAdapterConfig(inputConfig)
  private var isShutdown = false
  private var isQuiesced = false
  private var startTime: Long = 0

  private val partitionKVs = scala.collection.mutable.Map[Int, (SmartFilePartitionUniqueRecordKey, SmartFilePartitionUniqueRecordValue, SmartFilePartitionUniqueRecordValue)]()

  private var partitonCounts: collection.mutable.Map[String,Long] = collection.mutable.Map[String,Long]()
  private var metrics: collection.mutable.Map[String,Any] = collection.mutable.Map[String,Any]()
  private var startHeartBeat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var lastSeen: String = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))

  val delimiters : DataDelimiters = new DataDelimiters()

  //******************************************************************************************************
  //***************************node sync related code**********
  val communicationBasePath = ""
  val smartFileCommunicationPath = if(communicationBasePath.length > 0 ) (communicationBasePath + "/" + "SmartFileCommunication") else ("/" + "SmartFileCommunication")
  val smartFileFromLeaderPath = smartFileCommunicationPath + "/FromLeader"
  val smartFileToLeaderPath = smartFileCommunicationPath + "/ToLeader"
  val requestFilePath = smartFileToLeaderPath + "/RequestFile"
  val fileProcessingPath = smartFileToLeaderPath + "/FileProcessing"
  val File_Processing_Status_Finished = "finished"
  val filesParallelismParentPath = smartFileCommunicationPath + "/FilesParallelism"
  val sendStartInfoToLeaderParentPath = smartFileToLeaderPath + "/StartInfo"

  private var envContext : EnvContext = nodeContext.getEnvCtxt()
  private var clusterStatus : ClusterStatus = null
  private var participantExecutor : ExecutorService = null
  private var leaderExecutor : ExecutorService = null
  private var filesParallelism : Int = -1
  private var monitorController : MonitorController = null
  private var initialized = false
  private var prevRegLeader = ""

  private var prevRegParticipantPartitions : List[Int] = List()

  private var _ignoreFirstMsg : Boolean = _

  val statusUpdateInterval = 1000 //ms

  private val allNodesStartInfo = scala.collection.mutable.Map[String, List[(Int, String, Int, Boolean)]]()

  envContext.registerNodesChangeNotification(nodeChangeCallback)

  //add the node callback
  private def initializeNode: Unit ={
    if(nodeContext == null)
      LOG.debug("Smart File Consumer - nodeContext = null" )

    if (nodeContext.getEnvCtxt() == null)
      LOG.debug("Smart File Consumer - nodeContext.getEnvCtxt() = null" )

    if(envContext == null)
      LOG.debug("Smart File Consumer - envContext = null" )
    else{
      if(envContext.getClusterInfo() == null)
        LOG.debug("Smart File Consumer - envContext.getClusterInfo() = null")
    }

    if(clusterStatus == null)
      LOG.debug("Smart File Consumer - clusterStatus = null")
    else{
      LOG.debug("Smart File Consumer - clusterStatus.nodeId = " + clusterStatus.nodeId)
      LOG.debug("Smart File Consumer - clusterStatus.leaderNodeId = " +clusterStatus.leaderNodeId)
    }

    if (initialized == false) {
      val fileParallelismPath = filesParallelismParentPath + "/" + clusterStatus.nodeId
      LOG.debug("Smart File Consumer - participant {} is listening to path {}", clusterStatus.nodeId, fileParallelismPath)
      envContext.createListenerForCacheKey(fileParallelismPath, filesParallelismCallback)
    }

    if(clusterStatus.isLeader && clusterStatus.leaderNodeId.equals(clusterStatus.nodeId)){
      val newfilesParallelism = (adapterConfig.monitoringConfig.consumersCount.toDouble / clusterStatus.participantsNodeIds.size).ceil.toInt
      if (filesParallelism != -1) {
        // BUGBUG:: FIXME:- Unregister stuff
        // It is already distributes. Need to Re-Register stuff????
      }

      //action for the leader node

      //node wasn't a leader before
      if(initialized == false || ! clusterStatus.leaderNodeId.equals(prevRegLeader)){
        LOG.debug("Smart File Consumer - Leader is running on node " + clusterStatus.nodeId)

        allNodesStartInfo.clear()
        envContext.createListenerForCacheChildern(sendStartInfoToLeaderParentPath, collectStartInfo)// listen to start info

        leaderExecutor = Executors.newFixedThreadPool(2)
        val statusCheckerThread = new Runnable() {
          var lastStatus : scala.collection.mutable.Map[String, Long] = null
          override def run(): Unit = {

           while(true){
             lastStatus = checkParticipantsStatus(lastStatus)
             Thread.sleep(statusUpdateInterval)
           }
          }
        }
        leaderExecutor.execute(statusCheckerThread)

        //now register listeners for new requests (other than initial ones)
        LOG.debug("Smart File Consumer - Leader is listening to children of path " + requestFilePath)
        envContext.createListenerForCacheChildern(requestFilePath, requestFileLeaderCallback) // listen to file requests
        LOG.debug("Smart File Consumer - Leader is listening to children of path " + fileProcessingPath)
        envContext.createListenerForCacheChildern(fileProcessingPath, fileProcessingLeaderCallback)// listen to file processing status
      }
      else{//node was already leader

      }

      prevRegLeader = clusterStatus.leaderNodeId

      //FIXME : recalculate partitions for each node and send partition ids to nodes ???
      //set parallelism
      filesParallelism = newfilesParallelism
      //envContext.setListenerCacheKey(filesParallelismPath, filesParallelism.toString)
    }

    initialized = true
  }

  //after leader collects start info, it must pass initial files to monitor and, and assign partitions to participants
  private def handleStartInfo(): Unit ={
    LOG.debug("Smart File Consumer - handleStartInfo()")
    //Thread.sleep(10000) need to keep track of Partitions we got from nodes (whether we got all the participants we sent to engine or not)
    val maximumTrials = 10
    var trialCounter = 1
    while(trialCounter <= maximumTrials && allNodesStartInfo.size < clusterStatus.participantsNodeIds.size){
      Thread.sleep(1000)
      trialCounter += 1
    }

    LOG.debug("Smart File Consumer - allNodesStartInfo = " + allNodesStartInfo)

    //send to each node what partitions to handle (as received from engine)
    allNodesStartInfo.foreach(nodeStartInfo =>  {
      val path = filesParallelismParentPath + "/" + nodeStartInfo._1
      val data = nodeStartInfo._2.map(nodePartitionInfo => nodePartitionInfo._1).mkString(",")
      LOG.debug("Smart File Consumer - Leader is sending parallelism info. key is {}. value is {}", path, data)
      envContext.setListenerCacheKey(path, data)
    })

    //now since we have start info for all participants
    //collect file names and offsets
    val initialFilesToProcess = ArrayBuffer[(String, Int, String, Int)]()
    allNodesStartInfo.foreach(nodeStartInfo => nodeStartInfo._2.foreach(nodePartitionInfo => {
      if(nodePartitionInfo._2.trim.length > 0)
        initialFilesToProcess.append((nodeStartInfo._1,nodePartitionInfo._1, nodePartitionInfo._2, nodePartitionInfo._3))
      //(node, partitionId, file name, offset)
    }))

    // (First we need to process what ever files we get here), if we have file names
    if(initialFilesToProcess.size > 0)
      assignInitialFiles(initialFilesToProcess.toArray)

    //now run the monitor
    monitorController = new MonitorController(adapterConfig, newFileDetectedCallback, initialFilesToProcess.toArray)
    monitorController.startMonitoring()

  }

  def nodeChangeCallback (newClusterStatus : ClusterStatus) : Unit = {
    //action for participant nodes:
    clusterStatus = newClusterStatus

    if (initialized)
      initializeNode // Immediately Changing if participents change. Do we need to wait for engine to do it?

  }

  //will be useful when leader has requests from all nodes but no more files are available. then leader should be notified when new files are detected
  private def newFileDetectedCallback(fileName : String): Unit ={
    LOG.debug("Smart File Consumer - a new file was sent to leader ({}).", fileName)
    assignFileProcessingIfPossible()
  }

  //leader constantly checks processing participants to make sure they are still working
  private def checkParticipantsStatus(previousStatusMap : scala.collection.mutable.Map[String, Long]): scala.collection.mutable.Map[String, Long] ={
    //if previousStatusMap==null means this is first run of checking, no errors

    val processingQueue = getFileProcessingQueue
    val currentStatusMap = scala.collection.mutable.Map[String, Long]()

    processingQueue.foreach(processStr => {
      val processStrTokens = processStr.split(":")
      val pathTokens = processStrTokens(0).split("/")
      val fileInProcess = processStrTokens(1)
      val nodeId = pathTokens(0)
      val partitionId = pathTokens(1)

      val cacheKey = Status_Check_Cache_KeyParent + "/" + nodeId + "/" + partitionId
      val statusData = envContext.getConfigFromClusterCache(cacheKey) // filename~offset~timestamp
      if(previousStatusMap != null && statusData == null){
        LOG.error("Smart File Consumer - file {} is supposed to be processed by partition {} on node {} but not found in node updated status",
          fileInProcess, partitionId, nodeId)
      }
      else{
        val statusDataTokens = statusData.toString.split("~")
        val fileInStatus = statusDataTokens(0)
        val currentTimeStamp = statusDataTokens(2).toLong

        if(previousStatusMap != null && !fileInStatus.equals(fileInProcess))
          LOG.error("Smart File Consumer - file {} is supposed to be processed by partition {} on node {} but found this file {} in node updated status",
            fileInProcess, partitionId, nodeId, fileInStatus)
        else{

          if(previousStatusMap != null && previousStatusMap.contains(fileInStatus)){
            val previousTimestamp = previousStatusMap(fileInStatus)
            if(currentTimeStamp == previousTimestamp)
              LOG.error("Smart File Consumer - file {} is being processed by partition {} on node {}, but status hasn't been updated",
                fileInStatus, partitionId, nodeId)
          }

          currentStatusMap.put(fileInStatus, currentTimeStamp)
        }
      }
    })

    currentStatusMap
  }

  val File_Requests_Cache_Key = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "FileRequests"

  //maintained by leader, stores only files being processed (as list under one key). so that if leader changes, new leader can get the processing status
  val File_Processing_Cache_Key = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "FileProcessing"

  /*val File_Offset_Cache_Key_Prefix = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "Offsets/"//participants sets the value, to be read by leader, stores offset for each file (one key per file)
  def getFileOffsetCacheKey(fileName : String) = File_Offset_Cache_Key_Prefix + fileName
  def getFileOffsetFromCache(fileName : String) : Int = {
    val data = envContext.getConfigFromClusterCache(getFileOffsetCacheKey(fileName))
    if(data == null)
      0 //file is not processed yet, set offset to zero
    else
      (new String(data).toInt)
  }*/

  //value in cache has the format <node1>/<thread1>:<path to receive files>|<node2>/<thread1>:<path to receive files>
  def getFileRequestsQueue : List[String] = {
    val cacheData = envContext.getConfigFromClusterCache(File_Requests_Cache_Key)
    if(cacheData != null){
      val cacheDataStr = new String(cacheData)
      LOG.debug("Smart File Consumer - file request queue from cache is ", cacheDataStr)
      val tokens = cacheDataStr.split("\\|")
      tokens.toList
    }
    else{
      LOG.debug("Smart File Consumer - file request queue from cache is null")
      List()
    }

  }
  def saveFileRequestsQueue(requestQueue : List[String]) : Unit = {
    val cacheData = requestQueue.mkString("|")
    envContext.saveConfigInClusterCache(File_Requests_Cache_Key, cacheData.getBytes)
  }

  //value for file processing queue in cache has the format <node1>/<thread1>:<filename>|<node2>/<thread1>:<filename>
  def getFileProcessingQueue : List[String] = {
    val cacheData = envContext.getConfigFromClusterCache(File_Processing_Cache_Key)

    if(cacheData != null) {
      val cacheDataStr =  new String(cacheData)
      LOG.debug("Smart File Consumer - file processing queue from cache is ", cacheDataStr)
      val tokens = cacheDataStr.split("\\|")
      tokens.toList
    }
    else{
      LOG.debug("Smart File Consumer - file processing queue from cache is null")
      List()
    }
  }
  def saveFileProcessingQueue(requestQueue : List[String]) : Unit = {
    val cacheData = requestQueue.mkString("|")
    envContext.saveConfigInClusterCache(File_Processing_Cache_Key, cacheData.getBytes)
  }

  //what a leader should do when recieving file processing request
  def requestFileLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    LOG.debug("Smart File Consumer - requestFileLeaderCallback: eventType={}, eventPath={}, eventPathData={}",
      eventType, eventPath, eventPathData)

    var addRequestToQueue =false
    if(eventType.equalsIgnoreCase("put") || eventType.equalsIgnoreCase("update") ||
      eventType.equalsIgnoreCase("CHILD_UPDATED") || eventType.equalsIgnoreCase("CHILD_ADDED")) {
      val keyTokens = eventPath.split("/")
      val requestingNodeId = keyTokens(keyTokens.length - 2)
      val requestingThreadId = keyTokens(keyTokens.length - 1)
      val fileToProcessKeyPath = eventPathData //from leader

      LOG.info("Smart File Consumer - Leader has received a request from Node {}, Thread {}", requestingNodeId, requestingThreadId)

      //just add to request queue
      var requestQueue = getFileRequestsQueue
      requestQueue = requestQueue:::List(requestingNodeId + "/" + requestingThreadId + ":" + fileToProcessKeyPath)
      saveFileRequestsQueue(requestQueue)

      assignFileProcessingIfPossible()
    }
    //should do anything for remove?
  }

  //this is to be called whenever we have some changes in requests/new files
  //checks if there is a request ready, if parallelism degree allows new processing
  //   and if there is file needs processing
  //if all conditions met then assign a file to first request in the queue
  private def assignFileProcessingIfPossible(): Unit ={

    LOG.debug("Smart File Consumer - Leader is checking if it is possible to assign a new file to process")

    var processingQueue = getFileProcessingQueue
    val requestQueue = getFileRequestsQueue

    if(requestQueue.length > 0) {//there are ndoes/threads ready to process
    val request = requestQueue.head //take first request
      saveFileRequestsQueue(requestQueue.tail)

      //since a request in cache has the format <node1>/<thread1>:<path to receive files>|<node2>/<thread1>:<path to receive files>
      val requestTokens = request.split(":")
      val fileToProcessKeyPath = requestTokens(1)//something like SmartFileCommunication/FromLeader/<NodeId>/<thread id>
      val requestNodeInfoTokens = requestTokens(0).split("/")
      val requestingNodeId = requestNodeInfoTokens(0)
      val requestingThreadId = requestNodeInfoTokens(1)

      LOG.info("Smart File Consumer - currently " + processingQueue.length + " Files are being processed")
      LOG.info("Smart File Consumer - Maximum processing ops is " + adapterConfig.monitoringConfig.consumersCount)

      //check if it is allowed to process one more file
      if (processingQueue.length < adapterConfig.monitoringConfig.consumersCount) {

        val fileToProcessFullPath = monitorController.getNextFileToProcess
        if (fileToProcessFullPath != null) {

          LOG.debug("Smart File Consumer - Adding a file processing assignment of file + " + fileToProcessFullPath +
            " to Node " + requestingNodeId + ", thread Id=" + requestingThreadId)

          //leave offset management to engine, usually this will be other than zero when calling startProcessing
          val offset = 0//getFileOffsetFromCache(fileToProcessFullPath)
          val data = fileToProcessFullPath + "|" + offset

          //there are files that need to process

          envContext.setListenerCacheKey(fileToProcessKeyPath, data)
          processingQueue = processingQueue ::: List(requestingNodeId + "/" + requestingThreadId + ":" + fileToProcessFullPath)
          saveFileProcessingQueue(processingQueue)
        }
        else{
          LOG.info("Smart File Consumer - No more files currently to process")
        }
      }
      else{
        LOG.info("Smart File Consumer - Cannot assign anymore files to process")
      }
    }
    else{
      LOG.debug("Smart File Consumer - request queue is empty, no participants are available for new processes")
    }
  }

  //used to assign files to participants right after calling start processing, if the engine has files to be processed
  //initialFilesToProcess: list of (node, partitionId, file name, offset)
  private def assignInitialFiles(initialFilesToProcess : Array[(String, Int, String, Int)]): Unit ={
    if(initialFilesToProcess == null || initialFilesToProcess.length == 0)
      return
    var processingQueue = getFileProcessingQueue
    var requestQueue = getFileRequestsQueue

    //wait to get requests from all threads
    val maxTrials = 5
    var trialsCounter = 1
    while(trialsCounter <= maxTrials && requestQueue.size < adapterConfig.monitoringConfig.consumersCount){
      Thread.sleep(1000)
      requestQueue = getFileRequestsQueue
      trialsCounter += 1
    }

    //<node1>/<thread1>:<path to receive files>|<node2>/<thread1>:<path to receive files>
    requestQueue.foreach(requestStr => {
      val reqTokens = requestStr.split(":")
      val fileAssignmentKeyPath = reqTokens(1)
      val participantPathTokens = reqTokens(0).split("/")
      val nodeId = participantPathTokens(0)
      val partitionId = participantPathTokens(1).toInt
      initialFilesToProcess.find(fileInfo => fileInfo._1.equals(nodeId) && fileInfo._2 == partitionId) match{
        case None =>{}
        case Some(fileInfo) => {
          saveFileRequestsQueue(getFileRequestsQueue diff List(requestStr))//remove the current request

          val fileToProcessFullPath = fileInfo._3
          LOG.debug("Smart File Consumer - Adding a file processing assignment of file + " + fileToProcessFullPath +
            " to Node " + nodeId + ", partition Id=" + partitionId)

          val offset = fileInfo._4
          val data = fileToProcessFullPath + "|" + offset

          //there are files that need to process

          envContext.setListenerCacheKey(fileAssignmentKeyPath, data)
          processingQueue = processingQueue ::: List(nodeId + "/" + partitionId + ":" + fileToProcessFullPath)
          saveFileProcessingQueue(processingQueue)//add to processing queue
        }
      }
    })


  }

  //leader
  def collectStartInfo(eventType: String, eventPath: String, eventPathData: String) : Unit = {

    LOG.debug("Smart File Consumer - leader got start info. path is {}, value is {} ", eventPath, eventPathData)

    val pathTokens = eventPath.split("/")
    val sendingNodeId = pathTokens(pathTokens.length - 1)

    //(1,file1,0,true)~(2,file2,0,true)~(3,file3,1000,true)
    val dataAr = eventPathData.split("~")
    val sendingNodeStartInfo = dataAr.map(dataItem => {
      val trimmedItem = dataItem.substring(1,dataItem.length-1) // remove parenthesis
      val itemTokens = trimmedItem.split(",")
      (itemTokens(0).toInt, itemTokens(1), itemTokens(2).toInt, itemTokens(3).toBoolean)
    }).toList

    allNodesStartInfo.put(sendingNodeId, sendingNodeStartInfo)

  }

  //what a leader should do when recieving file processing status update
  def fileProcessingLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    if(eventType.equalsIgnoreCase("put") || eventType.equalsIgnoreCase("update") ||
      eventType.equalsIgnoreCase("CHILD_UPDATED") || eventType.equalsIgnoreCase("CHILD_ADDED")) {
      val keyTokens = eventPath.split("/")
      val processingThreadId = keyTokens(keyTokens.length - 1)
      val processingNodeId = keyTokens(keyTokens.length - 2)
      //value for file processing has the format <file-name>|<status>
      val valueTokens = eventPathData.split("\\|")
      val processingFilePath = valueTokens(0)
      val status = valueTokens(1)
      if(status == File_Processing_Status_Finished){
        LOG.info("Smart File Consumer - File ({}) processing finished", processingFilePath)

        val correspondingRequestFileKeyPath = requestFilePath + "/" + processingNodeId //e.g. SmartFileCommunication/ToLeader/ProcessedFile/<nodeid>

        //remove the file from processing queue
        var processingQueue = getFileProcessingQueue
        val valueInProcessingQueue = processingNodeId + "/" + processingThreadId + ":" + processingFilePath
        processingQueue = processingQueue diff List(valueInProcessingQueue)

        //since a file just got finished, a new one can be processed
        assignFileProcessingIfPossible()

        moveFile(processingFilePath)
      }
      else{//if processing status is NOT finished

      }

    }
    //CHILD_REMOVED should do anything for remove?
  }

  //to be used by leader in case changes happened to nodes
  private def shufflePartitionsToNodes(nodes : List[String], partitionsCount : Int):  HashMap[String, scala.collection.mutable.Set[Int]] with MultiMap[String, Int] ={
    val nodesPartitionsMap = new HashMap[String, scala.collection.mutable.Set[Int]] with MultiMap[String, Int]
    var counter = 0
    for(partition <- 1 to partitionsCount){
      val mod = partition % nodes.size
      val correspondingNodeIndex = if(mod == 0) nodes.size - 1 else mod - 1

      nodesPartitionsMap.addBinding(nodes(correspondingNodeIndex),partition )

      counter += 1
    }

    nodesPartitionsMap
  }

  val Status_Check_Cache_KeyParent = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "Status"
  //what a participant should do when receiving file to process (from leader)
  def fileAssignmentFromLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    //data has format <file name>|offset
    val dataTokens = eventPathData.split("\\|")
    val fileToProcessName = dataTokens(0)
    val offset = dataTokens(1).toInt

    val keyTokens = eventPath.split("/")
    val processingThreadId = keyTokens(keyTokens.length - 1).toInt
    val processingNodeId = keyTokens(keyTokens.length - 2)

    LOG.info("Smart File Consumer - Node Id = {}, Thread Id = {}, File ({}) was assigned to node {}",
      fileToProcessName,clusterStatus.nodeId)

    //start processing the file
    val context = new SmartFileConsumerContext()
    context.partitionId = processingThreadId.toInt
    context.ignoreFirstMsg = _ignoreFirstMsg
    context.nodeId = processingNodeId
    context.envContext = envContext
    //context.fileOffsetCacheKey = getFileOffsetCacheKey(fileToProcessName)
    context.statusUpdateCacheKey = Status_Check_Cache_KeyParent + "/" + processingNodeId + "/" + processingThreadId
    context.statusUpdateInterval = statusUpdateInterval

    val fileHandler = SmartFileHandlerFactory.createSmartFileHandler(adapterConfig, fileToProcessName)
    //now read the file and call sendSmartFileMessageToEngin for each message, and when finished call fileMessagesExtractionFinished_Callback to update status
    val fileMessageExtractor = new FileMessageExtractor(adapterConfig, fileHandler, offset, context, sendSmartFileMessageToEngin, fileMessagesExtractionFinished_Callback)
    fileMessageExtractor.extractMessages()
  }

  //key: SmartFileCommunication/FileProcessing/<node>/<threadId>
  //val: file|status
  def fileMessagesExtractionFinished_Callback(fileHandler: SmartFileHandler, context : SmartFileConsumerContext) : Unit = {

    //set file status as finished
    val pathKey = fileProcessingPath + "/" + context.nodeId + "/" + context.partitionId
    val data = fileHandler.getFullPath + "|" + File_Processing_Status_Finished
    envContext.setListenerCacheKey(pathKey, data)


    //send a new file request to leader
    val requestData =  smartFileFromLeaderPath + "/" + context.nodeId+ "/" + context.partitionId //listen to this SmartFileCommunication/FromLeader/<NodeId>/<partitionId id>
    val requestPathKey = requestFilePath + "/" + context.nodeId + "/" + context.partitionId
    LOG.info ("SMART FILE CONSUMER - participant ({}) - sending a file request to leader on partition ({})", context.nodeId, context.partitionId.toString)
    LOG.debug("SMART FILE CONSUMER - sending the request using path ({}) using value ({})", requestPathKey, requestData)
    envContext.setListenerCacheKey(requestPathKey, requestData)
  }

  //what a participant should do parallelism value changes
  def filesParallelismCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {

    //data is comma separated partition ids
    val currentNodePartitions = eventPathData.split(",").map(idStr => idStr.toInt).toList

    //val newFilesParallelism = eventPathData.toInt

    val nodeId = clusterStatus.nodeId

    LOG.info("Smart File Consumer - Node Id = {}, files parallelism changed. partitions to handle are {}", nodeId, eventPathData)
    LOG.info("Smart File Consumer - Old File Parallelism is {}", filesParallelism.toString)

    var parallelismStatus = ""
    if(filesParallelism == -1)
      parallelismStatus = "Uninitialized"
    else if (currentNodePartitions.size == prevRegParticipantPartitions.size)
      parallelismStatus = "Intact"
    else parallelismStatus = "Changed"

    //filesParallelism changed
    //wait until current threads finish then re-initialize threads
    if(parallelismStatus == "Changed"){
      if(participantExecutor != null) {
        participantExecutor.shutdown() //tell the executor service to shutdown after threads finish tasks in hand
        //if(!participantExecutor.awaitTermination(5, TimeUnit.MINUTES))
        //participantExecutor.shutdownNow()
      }
    }

    filesParallelism = currentNodePartitions.size
    prevRegParticipantPartitions = currentNodePartitions


    //create the threads only if no threads created yet or number of threads changed
    if(parallelismStatus == "Uninitialized" || parallelismStatus == "Changed"){
      LOG.info ("SMART FILE CONSUMER - participant ({}) - creating {} thread(s) to handle partitions ({})",
        nodeId, filesParallelism.toString, eventPathData)
      participantExecutor = Executors.newFixedThreadPool(filesParallelism)
      currentNodePartitions.foreach(partitionId =>{

        val executorThread = new Runnable() {
          private var partitionId: Int = _
          def init(id: Int) = partitionId = id

          override def run(): Unit = {
            val fileProcessingAssignementKeyPath = smartFileFromLeaderPath + "/" + nodeId + "/" + partitionId //listen to this SmartFileCommunication/FromLeader/<NodeId>/<partitionId id>
            //listen to file assignment from leader
            envContext.createListenerForCacheKey(fileProcessingAssignementKeyPath, fileAssignmentFromLeaderCallback) //e.g.   SmartFileCommunication/FromLeader/RequestFile/<nodeid>/<partitionId id>

            //send a file request to leader
            val fileRequestKeyPath = requestFilePath + "/" + nodeId+ "/" + partitionId
            LOG.info ("SMART FILE CONSUMER - participant ({}) - sending a file request to leader on partition ({})", nodeId, partitionId.toString)
            LOG.debug("SMART FILE CONSUMER - sending the request using path ({}) using value ({})", fileRequestKeyPath, fileProcessingAssignementKeyPath)
            envContext.setListenerCacheKey(fileRequestKeyPath, fileProcessingAssignementKeyPath)
          }
        }
        executorThread.init(partitionId)
        participantExecutor.execute(executorThread)
      })
    }

  }

  //after a file is changed, move it into targetMoveDir
  private def moveFile(originalFilePath : String): Unit = {
    val targetMoveDir = adapterConfig.monitoringConfig.targetMoveDir
    val fileStruct = originalFilePath.split("/")
    try {
      val fileHandler = SmartFileHandlerFactory.createSmartFileHandler(adapterConfig, originalFilePath)
      LOG.info("SMART FILE CONSUMER Moving File" + originalFilePath + " to " + targetMoveDir)
      if (fileHandler.exists()) {
        fileHandler.moveTo(targetMoveDir + "/" + fileStruct(fileStruct.size - 1))
        //fileCacheRemove(fileHandler.getFullPath)
      } else {
        LOG.warn("SMART FILE CONSUMER File has been deleted" + originalFilePath);
      }
    }
    catch{
      case e : Exception => LOG.error(s"SMART FILE CONSUMER - Failed to move file ($originalFilePath) into directory ($targetMoveDir)")
    }
  }
  //******************************************************************************************************

  override def Shutdown: Unit = lock.synchronized {
    StopProcessing
  }

  override def DeserializeKey(k: String): PartitionUniqueRecordKey = {
    val key = new SmartFilePartitionUniqueRecordKey
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
    val vl = new SmartFilePartitionUniqueRecordValue
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

  override def getAllPartitionBeginValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = lock.synchronized {
    getKeyValuePairs()
  }

  override def getAllPartitionEndValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = lock.synchronized {
    getKeyValuePairs()
  }

  private def getKeyValuePairs(): Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = {
    val infoBuffer = ArrayBuffer[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)]()

    for(partitionId <- 1 to adapterConfig.monitoringConfig.consumersCount){
      val rKey = new SmartFilePartitionUniqueRecordKey
      val rValue = new SmartFilePartitionUniqueRecordValue

      rKey.PartitionId = partitionId
      rKey.Name = adapterConfig.Name

      rValue.Offset = -1
      rValue.FileName = ""

      infoBuffer.append((rKey, rValue))
    }

    infoBuffer.toArray
  }

  // each value in partitionInfo is (PartitionUniqueRecordKey, PartitionUniqueRecordValue, Long, PartitionUniqueRecordValue). // key, processed value, Start transactionid, Ignore Output Till given Value (Which is written into Output Adapter) & processing Transformed messages (processing & total)
  override def GetAllPartitionUniqueRecordKey: Array[PartitionUniqueRecordKey] = lock.synchronized {

    val infoBuffer = ArrayBuffer[PartitionUniqueRecordKey]()

    for(partitionId <- 1 to adapterConfig.monitoringConfig.consumersCount){
      val rKey = new SmartFilePartitionUniqueRecordKey
      val rValue = new SmartFilePartitionUniqueRecordValue

      rKey.PartitionId = partitionId
      rKey.Name = adapterConfig.Name

      infoBuffer.append(rKey)
    }

    infoBuffer.toArray
  }


  override def StartProcessing(partitionIds: Array[StartProcPartInfo], ignoreFirstMsg: Boolean): Unit = {
    _ignoreFirstMsg = ignoreFirstMsg
    var lastHb: Long = 0
    startHeartBeat = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))

    LOG.info("START_PROCESSING CALLED")

        // Check to see if this already started
        if (startTime > 0) {
          LOG.error("SMART_FILE_ADAPTER: already started, or in the process of shutting down")
        }
        startTime = System.nanoTime

        if (partitionIds == null || partitionIds.size == 0) {
          LOG.error("SMART_FILE_ADAPTER: Cannot process the file adapter request, invalid parameters - number")
          return
        }
    /*
            val partitionInfoArray = partitionIds.map(quad => {
              (quad._key.asInstanceOf[SmartFilePartitionUniqueRecordKey],
                quad._val.asInstanceOf[SmartFilePartitionUniqueRecordValue],
                quad._validateInfoVal.asInstanceOf[SmartFilePartitionUniqueRecordValue])
            })

            //qc.instancePartitions = partitionInfo.map(partQuad => { partQuad._1.PartitionId }).toSet

            // Make sure the data passed was valid.
            if (partitionInfoArray == null) {
              LOG.error("SMART_FILE_ADAPTER: Cannot process the file adapter request, invalid parameters - partition instance list")
              return
            }

            partitionKVs.clear
            partitionInfoArray.foreach(partitionInfo => {
              partitionKVs(partitionInfo._1.PartitionId) = partitionInfo
            })

            // Enable the adapter to process
            isQuiesced = false
            LOG.debug("SMART_FILE_ADAPTER: Starting " + partitionKVs.size + " threads to process partitions")

            // Schedule a task to perform a read from a give partition.
            partitionKVs.foreach(kvsElement => {
              val partitionId = kvsElement._1
              val partition = kvsElement._2


            })
        */
    initializeNode//register the callbacks


    //(1,file1,0,true)~(2,file2,0,true)~(3,file3,1000,true)
    val myPartitionInfo = partitionIds.map(pid => (pid._key.asInstanceOf[SmartFilePartitionUniqueRecordKey].PartitionId,
      pid._val.asInstanceOf[SmartFilePartitionUniqueRecordValue].FileName,
      pid._val.asInstanceOf[SmartFilePartitionUniqueRecordValue].Offset, ignoreFirstMsg)).mkString("~")

    val SendStartInfoToLeaderPath = sendStartInfoToLeaderParentPath + "/" + clusterStatus.nodeId  // Should be different for each Nodes
    LOG.debug("Smart File Consumer - Node {} is sending start info to leader. path is {}, value is {} ",
      clusterStatus.nodeId, SendStartInfoToLeaderPath, myPartitionInfo)
    envContext.setListenerCacheKey(SendStartInfoToLeaderPath, myPartitionInfo) // => Goes to Leader

    if(clusterStatus.isLeader)
      handleStartInfo()
  }

  private def sendSmartFileMessageToEngin(smartMessage : SmartFileMessage,
                                          smartFileConsumerContext: SmartFileConsumerContext): Unit ={

    val partitionId = smartFileConsumerContext.partitionId
    val partition = partitionKVs(partitionId)

    val ignoreFirstMsg = smartFileConsumerContext.ignoreFirstMsg

    // if the offset is -1, then the server wants to start from the begining, else, it means that the server
    // knows what its doing and we start from that offset.
    var readOffset: Long = -1
    val uniqueRecordValue = if (ignoreFirstMsg) partition._3.Offset else partition._3.Offset - 1


    var execThread: ExecContext = null
    val uniqueKey = new SmartFilePartitionUniqueRecordKey
    val uniqueVal = new SmartFilePartitionUniqueRecordValue

    uniqueKey.Name = adapterConfig.Name
    uniqueKey.PartitionId = partitionId

    val readTmMs = System.currentTimeMillis


    val fileName = smartMessage.relatedFileHandler.getFullPath
    val offset = smartMessage.offsetInFile
    val message = smartMessage.msg


    // Create a new EngineMessage and call the engine.
    if (execThread == null) {
      execThread = execCtxtObj.CreateExecContext(input, uniqueKey, nodeContext)
    }

    incrementCountForPartition(partitionId)

    uniqueVal.Offset = offset
    uniqueVal.FileName = fileName
    val dontSendOutputToOutputAdap = uniqueVal.Offset <= uniqueRecordValue

    LOG.debug("Smart File Consumer - Node {} is sending start info to engine. partition id= {}. msg={}",
      smartFileConsumerContext.nodeId, smartFileConsumerContext.partitionId.toString, new String(message))
    execThread.execute(message, uniqueKey, uniqueVal, readTmMs)

  }

  override def StopProcessing: Unit = {
    //BUGBUG:: FIXME:- Clear listeners & queues all stuff related to leader or non-leader. So, Next SartProcessing should start the whole stuff again.
    initialized = false
    isShutdown = true

    if(monitorController!=null)
      monitorController.stopMonitoring

    if(leaderExecutor != null)
      leaderExecutor.shutdownNow()

    terminateReaderTasks
  }

  private def terminateReaderTasks(): Unit = {
    if (participantExecutor == null) return

    // Tell all thread to stop processing on the next interval, and shutdown the Excecutor.
    quiesce

    // Give the threads to gracefully stop their reading cycles, and then execute them with extreme prejudice.
    Thread.sleep(adapterConfig.monitoringConfig.waitingTimeMS)
    participantExecutor.shutdownNow
    while (!participantExecutor.isTerminated) {
      Thread.sleep(100)
    }

    LOG.debug("Smart File Adapter - Shutdown Complete")
    participantExecutor = null
    startTime = 0
  }

  /* no need for any synchronization here... it can only go one way.. worst case scenario, a reader thread gets to try to
*  read  one extra time (100ms lost)
 */
  private def quiesce: Unit = {
    isQuiesced = true
  }

  //TODO : add some data to metrics
  override def getComponentStatusAndMetrics: MonitorComponentInfo = {
    implicit val formats = org.json4s.DefaultFormats

    return new MonitorComponentInfo(AdapterConfiguration.TYPE_INPUT, adapterConfig.Name, SmartFileConsumer.ADAPTER_DESCRIPTION,
      startHeartBeat, lastSeen,  Serialization.write(metrics).toString)
  }

  override def getComponentSimpleStats: String = {
   ""
  }

  private def incrementCountForPartition(pid: Int): Unit = {
    var cVal: Long = partitonCounts.getOrElse(pid.toString, 0)
    partitonCounts(pid.toString) = cVal + 1
  }
}