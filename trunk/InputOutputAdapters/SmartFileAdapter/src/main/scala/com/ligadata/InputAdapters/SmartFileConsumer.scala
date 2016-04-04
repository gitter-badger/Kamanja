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
import scala.collection.mutable.ArrayBuffer


class SmartFileConsumerContext{
  var partitionId: Int = _
  var ignoreFirstMsg: Boolean = _
  var nodeId : String = _
  var threadId : Int  = _
  var envContext : EnvContext = null
  var fileOffsetCacheKey : String = _
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
  val smartFileCommunicationPath = if(communicationBasePath.length > 0 ) communicationBasePath + "/" + "SmartFileCommunication"
  val smartFileFromLeaderPath = smartFileCommunicationPath + "/FromLeader"
  val smartFileToLeaderPath = smartFileCommunicationPath + "/ToLeader"
  val requestFilePath = smartFileToLeaderPath + "/RequestFile"
  val fileProcessingPath = smartFileToLeaderPath + "/FileProcessing"
  val File_Processing_Status_Finished = "finished"
  val filesParallelismPath = smartFileCommunicationPath + "/FilesParallelism"

  private var envContext : EnvContext = null
  private var clusterStatus : ClusterStatus = null
  private var participantExecutor : ExecutorService = null
  private var filesParallelism : Int = -1
  private var monitorController : MonitorController = null

  private var _ignoreFirstMsg : Boolean = _

  //add the node callback
  private def initializeNode(nodeContext: NodeContext): Unit ={
    envContext = nodeContext.gCtx
    envContext.registerNodesChangeNotification(nodeChangeCallback)
  }

  def nodeChangeCallback (newClusterStatus : ClusterStatus) : Unit = {

    if(newClusterStatus.isLeader){
      //action for the leader node

      //node wasn't a leader before
      if(!clusterStatus.isLeader){
        monitorController = new MonitorController(adapterConfig, newFileDetectedCallback)
        monitorController.startMonitoring()

        envContext.createListenerForCacheChildern(requestFilePath, requestFileLeaderCallback) // listen to file requests
        envContext.createListenerForCacheChildern(fileProcessingPath, fileProcessingLeaderCallback)// listen to file processing status

      }
      else{//node was already leader

      }

      //set parallelism
      filesParallelism = (adapterConfig.monitoringConfig.consumersCount.toDouble / newClusterStatus.participantsNodeIds.size).round.toInt
      envContext.setListenerCacheKey(filesParallelismPath, filesParallelism.toString)
    }

    //action for participant nodes:
    val nodeId = newClusterStatus.nodeId
    envContext.createListenerForCacheKey(filesParallelismPath, filesParallelismCallback)


    clusterStatus = newClusterStatus
  }

  //will be useful when leader has requests from all nodes but no more files are available. then leader should be notified when new files are detected
  private def newFileDetectedCallback(fileName : String): Unit ={
    assignFileProcessingIfPossible()
  }


  val File_Requests_Cache_Key = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "FileRequests"

  //maintained by leader, stores only files being processed (as list under one key). so that if leader changes, new leader can get the processing status
  val File_Processing_Cache_Key = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "FileProcessing"

  val File_Offset_Cache_Key_Prefix = "Smart_File_Adapter/" + adapterConfig.Name + "/" + "Offsets/"//participants sets the value, to be read by leader, stores offset for each file (one key per file)
  def getFileOffsetCacheKey(fileName : String) = File_Offset_Cache_Key_Prefix + fileName
  def getFileOffsetFromCache(fileName : String) : Int = {
    val data = envContext.getConfigFromClusterCache(getFileOffsetCacheKey(fileName))
    if(data == null)
      0 //file is not processed yet, set offset to zero
    else
      (new String(data).toInt)
  }

  //value in cache has the format <node1>/<thread1>:<path to receive files>|<node2>/<thread1>:<path to receive files>
  def getFileRequestsQueue : List[String] = {
    val cacheData = new String(envContext.getConfigFromClusterCache(File_Requests_Cache_Key))
    val tokens = cacheData.split("\\|")
    tokens.toList
  }
  def saveFileRequestsQueue(requestQueue : List[String]) : Unit = {
    val cacheData = requestQueue.mkString("|")
    envContext.saveConfigInClusterCache(File_Requests_Cache_Key, cacheData.getBytes)
  }

  //value for file processing queue in cache has the format <node1>/<thread1>:<filename>|<node2>/<thread1>:<filename>
  def getFileProcessingQueue : List[String] = {
    val cacheData = envContext.getConfigFromClusterCache(File_Processing_Cache_Key)
    if(cacheData != null) {
      val tokens = new String(cacheData).split("\\|")
      tokens.toList
    }
    else{
      List()
    }
  }
  def saveFileProcessingQueue(requestQueue : List[String]) : Unit = {
    val cacheData = requestQueue.mkString("|")
    envContext.saveConfigInClusterCache(File_Processing_Cache_Key, cacheData.getBytes)
  }

  //what a leader should do when recieving file processing request
  def requestFileLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    var addRequestToQueue =false
    if(eventType.equalsIgnoreCase("put") || eventType.equalsIgnoreCase("update")) {
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

          val offset = getFileOffsetFromCache(fileToProcessFullPath)
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
  }

  //what a leader should do when recieving file processing status update
  def fileProcessingLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    if(eventType.equalsIgnoreCase("put") || eventType.equalsIgnoreCase("update")) {
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
    //should do anything for remove?
  }

  //according to node order and threads on the node, give a number unique per threads
  def getPartitionNum(nodeId : String, threadNum : Int) : Int = {

    val currentNodeId = clusterStatus.nodeId
    val nodeOrder = clusterStatus.participantsNodeIds.toArray.indexWhere(id => id == currentNodeId) //0 base clearly
    val threadsPerNode = filesParallelism //assuming this is 1 based, result is 1 based

    nodeOrder * threadsPerNode + threadNum
  }

  //what a participant should do when receiving file to process (from leader)
  def fileAssignmentFromLeaderCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    //data has format <file name>|offset
    val dataTokens = eventPathData.split("\\|")
    val fileToProcessName = dataTokens(0)
    val offset = dataTokens(1).toInt

    val keyTokens = eventPath.split("/")
    val processingThreadId = keyTokens(keyTokens.length - 1)
    val processingNodeId = keyTokens(keyTokens.length - 2)

    LOG.info("Smart File Consumer - Node Id = {}, Thread Id = {}, File ({}) was assigned to node {}",
      fileToProcessName,clusterStatus.nodeId)

    //start processing the file
    val context = new SmartFileConsumerContext()
    context.partitionId = getPartitionNum(processingNodeId, processingThreadId.toInt)
    context.ignoreFirstMsg = _ignoreFirstMsg
    context.threadId = processingThreadId.toInt
    context.nodeId = processingNodeId
    context.envContext = envContext
    context.fileOffsetCacheKey = getFileOffsetCacheKey(fileToProcessName)

    val fileHandler = SmartFileHandlerFactory.createSmartFileHandler(adapterConfig, fileToProcessName)
    //now read the file and call sendSmartFileMessageToEngin for each message, and when finished call fileMessagesExtractionFinished_Callback to update status
    val fileMessageExtractor = new FileMessageExtractor(adapterConfig, fileHandler, offset, context, sendSmartFileMessageToEngin, fileMessagesExtractionFinished_Callback)
    fileMessageExtractor.extractMessages()
  }

  //key: SmartFileCommunication/FileProcessing/<node>/<threadId>
  //val: file|status
  def fileMessagesExtractionFinished_Callback(fileHandler: SmartFileHandler, context : SmartFileConsumerContext) : Unit = {

    //set file status as finished
    val pathKey = fileProcessingPath + "/" + context.nodeId + "/" + context.threadId
    val data = fileHandler.getFullPath + "|" + File_Processing_Status_Finished
    envContext.setListenerCacheKey(pathKey, data)
  }

  //what a participant should do parallelism value changes
  def filesParallelismCallback (eventType: String, eventPath: String, eventPathData: String) : Unit = {
    val newFilesParallelism = eventPathData.toInt

    val nodeId = clusterStatus.nodeId

    LOG.info("Smart File Consumer - Node Id = {}, New File Parallelism (maximum threads per node) is {}", nodeId, newFilesParallelism.toString)
    LOG.info("Smart File Consumer - Old File Parallelism is {}", filesParallelism.toString)

    var parallelismStatus = ""
    if(filesParallelism == -1)
      parallelismStatus = "Uninitialized"
    else if (newFilesParallelism == filesParallelism)
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
    filesParallelism = newFilesParallelism


    //create the threads only if no threads created yet or number of threads changed
    if(parallelismStatus == "Uninitialized" || parallelismStatus == "Changed"){
      participantExecutor = Executors.newFixedThreadPool(filesParallelism)
      for(threadId <- 1 to filesParallelism) {

        val executorThread = new Runnable() {
          private var threadId: Int = _
          def init(id: Int) = threadId = id

          override def run(): Unit = {
            val fileProcessingAssignementKeyPath = smartFileFromLeaderPath + "/" + nodeId + "/" + threadId //listen to this SmartFileCommunication/FromLeader/<NodeId>/<thread id>
            //listen to file assignment from leader
            envContext.createListenerForCacheKey(fileProcessingAssignementKeyPath, fileAssignmentFromLeaderCallback) //e.g.   SmartFileCommunication/FromLeader/RequestFile/<nodeid>/<thread id>
            val fileRequestKeyPath = smartFileToLeaderPath + "/" + nodeId+ "/" + threadId
            envContext.setListenerCacheKey(fileRequestKeyPath, fileProcessingAssignementKeyPath)
          }
        }
        executorThread.init(threadId)
        participantExecutor.execute(executorThread)
      }
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
      LOG.error("SMART_FILE_ADAPTER: Cannot process the kafka queue request, invalid parameters - number")
      return
    }

    val partitionInfoArray = partitionIds.map(quad => {
      (quad._key.asInstanceOf[SmartFilePartitionUniqueRecordKey],
        quad._val.asInstanceOf[SmartFilePartitionUniqueRecordValue],
        quad._validateInfoVal.asInstanceOf[SmartFilePartitionUniqueRecordValue])
    })

    //qc.instancePartitions = partitionInfo.map(partQuad => { partQuad._1.PartitionId }).toSet

    // Make sure the data passed was valid.
    if (partitionInfoArray == null) {
      LOG.error("SMART_FILE_ADAPTER: Cannot process the kafka queue request, invalid parameters - partition instance list")
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

    initializeNode(nodeContext)//register the callbacks

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

    execThread.execute(message, uniqueKey, uniqueVal, readTmMs)

  }

  override def StopProcessing: Unit = {
    isShutdown = true
    monitorController.stopMonitoring
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

  private def incrementCountForPartition(pid: Int): Unit = {
    var cVal: Long = partitonCounts.getOrElse(pid.toString, 0)
    partitonCounts(pid.toString) = cVal + 1
  }
}