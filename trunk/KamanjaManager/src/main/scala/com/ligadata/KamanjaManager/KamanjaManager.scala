
package com.ligadata.KamanjaManager

import com.ligadata.HeartBeat.MonitoringContext
import com.ligadata.KamanjaBase._
import com.ligadata.InputOutputAdapterInfo.{ ExecContext, InputAdapter, OutputAdapter, ExecContextFactory, PartitionUniqueRecordKey, PartitionUniqueRecordValue }
import com.ligadata.StorageBase.StorageAdapter
import com.ligadata.ZooKeeper.CreateClient
import org.json4s.jackson.JsonMethods._

import scala.reflect.runtime.{ universe => ru }
import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import collection.mutable.{ MultiMap, Set }
import java.io.{ PrintWriter, File, PrintStream, BufferedReader, InputStreamReader }
import scala.util.Random
import scala.Array.canBuildFrom
import java.util.{ Properties, Observer, Observable }
import java.sql.Connection
import scala.collection.mutable.TreeSet
import java.net.{ Socket, ServerSocket }
import java.util.concurrent.{ Executors, ScheduledExecutorService, TimeUnit }
import com.ligadata.Utils.{ Utils, KamanjaClassLoader, KamanjaLoaderInfo }
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.Exceptions.{ FatalAdapterException }
import scala.actors.threadpool.{ ExecutorService }
import com.ligadata.KamanjaVersion.KamanjaVersion

class KamanjaServer(var mgr: KamanjaManager, port: Int) extends Runnable {
  private val LOG = LogManager.getLogger(getClass);
  private val serverSocket = new ServerSocket(port)

  def run() {
    try {
      while (true) {
        // This will block until a connection comes in.
        val socket = serverSocket.accept()
        (new Thread(new ConnHandler(socket, mgr))).start()
      }
    } catch {
      case e: Exception => {
        LOG.error("Socket Error", e)
      }
    } finally {
      if (serverSocket.isClosed() == false)
        serverSocket.close
    }
  }

  def shutdown() {
    if (serverSocket.isClosed() == false)
      serverSocket.close
  }
}

class ConnHandler(var socket: Socket, var mgr: KamanjaManager) extends Runnable {
  private val LOG = LogManager.getLogger(getClass);
  private val out = new PrintStream(socket.getOutputStream)
  private val in = new BufferedReader(new InputStreamReader(socket.getInputStream))

  def run() {
    val vt = 0
    try {
      breakable {
        while (true) {
          val strLine = in.readLine()
          if (strLine == null)
            break
          mgr.execCmd(strLine)
        }
      }
    } catch {
      case e: Exception => {
        LOG.error("", e)
      }
    } finally {
      socket.close;
    }
  }
}

object KamanjaManangerMonitorContext {
  val monitorCount = new java.util.concurrent.atomic.AtomicLong()
}

case class InitConfigs(val dataDataStoreInfo: String, val jarPaths: collection.immutable.Set[String], val zkConnectString: String,
                      val zkNodeBasePath: String, val zkSessionTimeoutMs: Int, val zkConnectionTimeoutMs: Int)

object KamanjaConfiguration {
  var configFile: String = _
  var allConfigs: Properties = _
  //  var metadataDataStoreInfo: String = _
//  var dataDataStoreInfo: String = _
//  var jarPaths: collection.immutable.Set[String] = _
  var nodeId: Int = _
  var clusterId: String = _
  var nodePort: Int = _
//  var zkConnectString: String = _
//  var zkNodeBasePath: String = _
//  var zkSessionTimeoutMs: Int = _
//  var zkConnectionTimeoutMs: Int = _

//  var txnIdsRangeForNode: Int = 100000 // Each time get txnIdsRange of transaction ids for each Node
//  var txnIdsRangeForPartition: Int = 10000 // Each time get txnIdsRange of transaction ids for each partition

  // Debugging info configs -- Begin
  var waitProcessingSteps = collection.immutable.Set[Int]()
  var waitProcessingTime = 0
  // Debugging info configs -- End

  var shutdown = false
  var participentsChangedCntr: Long = 0
  var baseLoader = new KamanjaLoaderInfo
//  var adaptersAndEnvCtxtLoader = new KamanjaLoaderInfo(baseLoader, true, true)
//  var metadataLoader = new KamanjaLoaderInfo(baseLoader, true, true)

//  var adapterInfoCommitTime = 5000 // Default 5 secs

  def Reset: Unit = {
    configFile = null
    allConfigs = null
    //    metadataDataStoreInfo = null
//    dataDataStoreInfo = null
//    jarPaths = null
    nodeId = 0
    clusterId = null
    nodePort = 0
//    zkConnectString = null
//    zkNodeBasePath = null
//    zkSessionTimeoutMs = 0
//    zkConnectionTimeoutMs = 0

    // Debugging info configs -- Begin
    waitProcessingSteps = collection.immutable.Set[Int]()
    waitProcessingTime = 0
    // Debugging info configs -- End

    shutdown = false
    participentsChangedCntr = 0
  }
}

object ProcessedAdaptersInfo {
  private val LOG = LogManager.getLogger(getClass);
  private val lock = new Object
  private val instances = scala.collection.mutable.Map[Int, scala.collection.mutable.Map[String, String]]()
  private var prevAdapterCommittedValues = Map[String, String]()
  private def getAllValues: Map[String, String] = {
    var maps: List[scala.collection.mutable.Map[String, String]] = null
    lock.synchronized {
      maps = instances.map(kv => kv._2).toList
    }

    var retVals = scala.collection.mutable.Map[String, String]()
    maps.foreach(m => retVals ++= m)
    retVals.toMap
  }

  def getOneInstance(hashCode: Int, createIfNotExists: Boolean): scala.collection.mutable.Map[String, String] = {
    lock.synchronized {
      val inst = instances.getOrElse(hashCode, null)
      if (inst != null) {
        return inst
      }
      if (createIfNotExists == false)
        return null

      val newInst = scala.collection.mutable.Map[String, String]()
      instances(hashCode) = newInst
      return newInst
    }
  }

  def clearInstances: Unit = {
    var maps: List[scala.collection.mutable.Map[String, String]] = null
    lock.synchronized {
      instances.clear()
      prevAdapterCommittedValues
    }
  }

//  def CommitAdapterValues: Boolean = {
//    LOG.debug("CommitAdapterValues. AdapterCommitTime: " + KamanjaConfiguration.adapterInfoCommitTime)
//    var committed = false
//    if (KamanjaMetadata.envCtxt != null) {
//      // Try to commit now
//      var changedValues: List[(String, String)] = null
//      val newValues = getAllValues
//      if (prevAdapterCommittedValues.size == 0) {
//        changedValues = newValues.toList
//      } else {
//        var changedArr = ArrayBuffer[(String, String)]()
//        newValues.foreach(v1 => {
//          val oldVal = prevAdapterCommittedValues.getOrElse(v1._1, null)
//          if (oldVal == null || v1._2.equals(oldVal) == false) { // It is not found or changed, simple take it
//            changedArr += v1
//          }
//        })
//        changedValues = changedArr.toList
//      }
//      // Commit here
//      try {
//        if (changedValues.size > 0)
//          KamanjaMetadata.envCtxt.setAdapterUniqKeyAndValues(changedValues)
//        prevAdapterCommittedValues = newValues
//        committed = true
//      } catch {
//        case e: Exception => {
//          LOG.error("Failed to commit adapter changes. if we can not save this we will reprocess the information when service restarts.", e)
//        }
//        case e: Throwable => {
//          LOG.error("Failed to commit adapter changes. if we can not save this we will reprocess the information when service restarts.", e)
//        }
//      }
//
//    }
//    committed
//  }
}

class KamanjaManager extends Observer {
  private val LOG = LogManager.getLogger(getClass);

  // KamanjaServer Object
  private var serviceObj: KamanjaServer = null

  private val inputAdapters = new ArrayBuffer[InputAdapter]
  private val outputAdapters = new ArrayBuffer[OutputAdapter]
  private val storageAdapters = new ArrayBuffer[StorageAdapter]
//  private val adapterChangedCntr = new java.util.concurrent.atomic.AtomicLong(0)
  private var adapterChangedCntr: Long = 0
//  private val statusAdapters = new ArrayBuffer[OutputAdapter]
//  private val validateInputAdapters = new ArrayBuffer[InputAdapter]

  private var thisEngineInfo: MainInfo = null
  private var adapterMetricInfo: scala.collection.mutable.MutableList[com.ligadata.HeartBeat.MonitorComponentInfo] = null
//  private val failedEventsAdapters = new ArrayBuffer[OutputAdapter]

  private var metricsService: ExecutorService = scala.actors.threadpool.Executors.newFixedThreadPool(1)
  private var isTimerRunning = false
  private var isTimerStarted = false
  private type OptionMap = Map[Symbol, Any]

  def incrAdapterChangedCntr(): Unit = {
    adapterChangedCntr += 1
  }
  def getAdapterChangedCntr: Long = adapterChangedCntr
  def getAllAdaptersInfo: (Array[InputAdapter], Array[OutputAdapter], Array[StorageAdapter], Long) = {
    (inputAdapters.toArray, outputAdapters.toArray, storageAdapters.toArray, adapterChangedCntr)
  }

  private def PrintUsage(): Unit = {
    LOG.warn("Available commands:")
    LOG.warn("    Quit")
    LOG.warn("    Help")
    LOG.warn("    --version")
    LOG.warn("    --config <configfilename>")
  }

  private def Shutdown(exitCode: Int): Int = {
    /*
    if (KamanjaMetadata.envCtxt != null)
      KamanjaMetadata.envCtxt.PersistRemainingStateEntriesOnLeader
*/
    KamanjaLeader.Shutdown
    KamanjaMetadata.Shutdown
    ShutdownAdapters
    PostMessageExecutionQueue.shutdown
    if (KamanjaMetadata.envCtxt != null)
      KamanjaMetadata.envCtxt.Shutdown
    if (serviceObj != null)
      serviceObj.shutdown
    com.ligadata.transactions.NodeLevelTransService.Shutdown
    return exitCode
  }

  private def nextOption(map: OptionMap, list: List[String]): OptionMap = {
    def isSwitch(s: String) = (s(0) == '-')
    list match {
      case Nil => map
      case "--config" :: value :: tail =>
        nextOption(map ++ Map('config -> value), tail)
      case "--version" :: tail =>
        nextOption(map ++ Map('version -> "true"), tail)
      case option :: tail => {
        LOG.error("Unknown option " + option)
        throw new Exception("Unknown option " + option)
      }
    }
  }

  private def LoadDynamicJarsIfRequired(loadConfigs: Properties): Boolean = {
    val dynamicjars: String = loadConfigs.getProperty("dynamicjars".toLowerCase, "").trim

    if (dynamicjars != null && dynamicjars.length() > 0) {
      val jars = dynamicjars.split(",").map(_.trim).filter(_.length() > 0)
      if (jars.length > 0) {
        val qualJars = jars.map(j => Utils.GetValidJarFile(KamanjaMetadata.envCtxt.getJarPaths(), j))
        val nonExistsJars = Utils.CheckForNonExistanceJars(qualJars.toSet)
        if (nonExistsJars.size > 0) {
          LOG.error("Not found jars in given Dynamic Jars List : {" + nonExistsJars.mkString(", ") + "}")
          return false
        }
        return Utils.LoadJars(qualJars.toArray, KamanjaConfiguration.baseLoader.loadedJars, KamanjaConfiguration.baseLoader.loader)
      }
    }

    true
  }

  private def ShutdownAdapters: Boolean = {
    LOG.debug("Shutdown Adapters started @ " + Utils.GetCurDtTmStr)
    val s0 = System.nanoTime
//
//    validateInputAdapters.foreach(ia => {
//      try {
//        ia.Shutdown
//      } catch {
//        case fae: FatalAdapterException => {
//          LOG.error("Validate adapter " + ia.UniqueName + "failed to shutdown", fae)
//        }
//        case e: Exception => {
//          LOG.error("Validate adapter " + ia.UniqueName + "failed to shutdown", e)
//        }
//        case e: Throwable => {
//          LOG.error("Validate adapter " + ia.UniqueName + "failed to shutdown", e)
//        }
//      }
//    })
//
//    validateInputAdapters.clear

    inputAdapters.foreach(ia => {
      try {
        ia.Shutdown
      } catch {
        case fae: FatalAdapterException => {
          LOG.error("Input adapter " + ia.UniqueName + "failed to shutdown", fae)
        }
        case e: Exception => {
          LOG.error("Input adapter " + ia.UniqueName + "failed to shutdown", e)
        }
        case e: Throwable => {
          LOG.error("Input adapter " + ia.UniqueName + "failed to shutdown", e)
        }
      }
    })

    inputAdapters.clear

    outputAdapters.foreach(oa => {
      try {
        oa.Shutdown
      } catch {
        case fae: FatalAdapterException => {
          LOG.error("Output adapter failed to shutdown", fae)
        }
        case e: Exception => {
          LOG.error("Output adapter failed to shutdown", e)
        }
        case e: Throwable => {
          LOG.error("Output adapter failed to shutdown", e)
        }
      }
    })

    outputAdapters.clear

    storageAdapters.foreach(sa => {
      try {
        sa.Shutdown
      } catch {
        case fae: FatalAdapterException => {
          LOG.error("Storage adapter failed to shutdown", fae)
        }
        case e: Exception => {
          LOG.error("Storage adapter failed to shutdown", e)
        }
        case e: Throwable => {
          LOG.error("Storage adapter failed to shutdown", e)
        }
      }
    })

    storageAdapters.clear

//
//    statusAdapters.foreach(oa => {
//      try {
//        oa.Shutdown
//      } catch {
//        case fae: FatalAdapterException => {
//          LOG.error("Status adapter failed to shutdown", fae)
//        }
//        case e: Exception => {
//          LOG.error("Status adapter failed to shutdown", e)
//        }
//        case e: Throwable => {
//          LOG.error("Status adapter failed to shutdown", e)
//        }
//      }
//    })
//
//    statusAdapters.clear
//
//    failedEventsAdapters.foreach(oa => {
//      try {
//        oa.Shutdown
//      } catch {
//        case fae: FatalAdapterException => {
//          LOG.error("FailedEvents adapter failed to shutdown, cause", fae)
//        }
//        case e: Exception => {
//          LOG.error("FailedEvents adapter failed to shutdown, cause", e)
//        }
//        case e: Throwable => {
//          LOG.error("FailedEvents adapter failed to shutdown", e)
//        }
//      }
//    })
//
//    failedEventsAdapters.clear

    val totaltm = "TimeConsumed:%.02fms".format((System.nanoTime - s0) / 1000000.0);
    LOG.debug("Shutdown Adapters done @ " + Utils.GetCurDtTmStr + ". " + totaltm)

    true
  }

  private def initialize: Boolean = {
    var retval: Boolean = true

    val loadConfigs = KamanjaConfiguration.allConfigs

    try {
      KamanjaConfiguration.nodeId = loadConfigs.getProperty("nodeId".toLowerCase, "0").replace("\"", "").trim.toInt
      if (KamanjaConfiguration.nodeId <= 0) {
        LOG.error("Not found valid nodeId. It should be greater than 0")
        return false
      }

//      try {
//        val adapterCommitTime = loadConfigs.getProperty("AdapterCommitTime".toLowerCase, "0").replace("\"", "").trim.toInt
//        if (adapterCommitTime > 0) {
//          KamanjaConfiguration.adapterInfoCommitTime = adapterCommitTime
//        }
//      } catch {
//        case e: Exception => { LOG.warn("", e) }
//      }

      try {
        KamanjaConfiguration.waitProcessingTime = loadConfigs.getProperty("waitProcessingTime".toLowerCase, "0").replace("\"", "").trim.toInt
        if (KamanjaConfiguration.waitProcessingTime > 0) {
          val setps = loadConfigs.getProperty("waitProcessingSteps".toLowerCase, "").replace("\"", "").split(",").map(_.trim).filter(_.length() > 0)
          if (setps.size > 0)
            KamanjaConfiguration.waitProcessingSteps = setps.map(_.toInt).toSet
        }
      } catch {
        case e: Exception => { LOG.warn("", e) }
      }

      LOG.debug("Initializing metadata bootstrap")
      KamanjaMetadata.InitBootstrap
      var intiConfigs: InitConfigs = null

      try{
        intiConfigs = KamanjaMdCfg.InitConfigInfo
      } catch {
        case e: Exception => {
          return false
        }
      }

      LOG.debug("Validating required jars")
      KamanjaMdCfg.ValidateAllRequiredJars(intiConfigs.jarPaths)
      LOG.debug("Load Environment Context")

      KamanjaMetadata.envCtxt = KamanjaMdCfg.LoadEnvCtxt(intiConfigs)
      if (KamanjaMetadata.envCtxt == null)
        return false

      val (zkConnectString, zkNodeBasePath, zkSessionTimeoutMs, zkConnectionTimeoutMs)  = KamanjaMetadata.envCtxt.getZookeeperInfo

      var engineLeaderZkNodePath = ""
      var engineDistributionZkNodePath = ""
      var metadataUpdatesZkNodePath = ""
      var adaptersStatusPath = ""
      var dataChangeZkNodePath = ""
      var zkHeartBeatNodePath = ""

      if (zkNodeBasePath.size > 0) {
        engineLeaderZkNodePath = zkNodeBasePath + "/engineleader"
        engineDistributionZkNodePath = zkNodeBasePath + "/enginedistribution"
        metadataUpdatesZkNodePath = zkNodeBasePath + "/metadataupdate"
        adaptersStatusPath = zkNodeBasePath + "/adaptersstatus"
        dataChangeZkNodePath = zkNodeBasePath + "/datachange"
        zkHeartBeatNodePath = zkNodeBasePath + "/monitor/engine/" + KamanjaConfiguration.nodeId.toString
      }

      KamanjaMetadata.gNodeContext = new NodeContext(KamanjaMetadata.envCtxt)

      PostMessageExecutionQueue.init(KamanjaMetadata.gNodeContext)

      LOG.debug("Loading Adapters")
      // Loading Adapters (Do this after loading metadata manager & models & Dimensions (if we are loading them into memory))

      retval = KamanjaMdCfg.LoadAdapters(inputAdapters, outputAdapters, storageAdapters)

      if (retval) {
        LOG.debug("Initialize Metadata Manager")
        KamanjaMetadata.InitMdMgr(zkConnectString, metadataUpdatesZkNodePath, zkSessionTimeoutMs, zkConnectionTimeoutMs)
//        KamanjaMetadata.envCtxt.CacheContainers(KamanjaConfiguration.clusterId) // Load data for Caching
        LOG.debug("Initializing Leader")

        var txnCtxt: TransactionContext = null
        var txnId = KamanjaConfiguration.nodeId.toString.hashCode()
        if (txnId > 0)
          txnId = -1 * txnId
        // Finally we are taking -ve txnid for this
        try {
          txnCtxt = new TransactionContext(txnId, KamanjaMetadata.gNodeContext, Array[Byte](), EventOriginInfo("", ""), 0, null)
          ThreadLocalStorage.txnContextInfo.set(txnCtxt)

          val (tmpMdls, tMdlsChangedCntr) = KamanjaMetadata.getAllModels
          val tModels = if (tmpMdls != null) tmpMdls else Array[(String, MdlInfo)]()

          tModels.foreach(tup => {
            tup._2.mdl.init(txnCtxt)
          })
        } catch {
          case e: Exception => throw e
          case e: Throwable => throw e
        } finally {
          ThreadLocalStorage.txnContextInfo.remove
          if (txnCtxt != null) {
            KamanjaMetadata.gNodeContext.getEnvCtxt.rollbackData()
          }
        }

        KamanjaLeader.Init(KamanjaConfiguration.nodeId.toString, zkConnectString, engineLeaderZkNodePath, engineDistributionZkNodePath, adaptersStatusPath, inputAdapters, outputAdapters, storageAdapters, KamanjaMetadata.envCtxt, zkSessionTimeoutMs, zkConnectionTimeoutMs, dataChangeZkNodePath)
      }

      /*
      if (retval) {
        try {
          serviceObj = new KamanjaServer(this, KamanjaConfiguration.nodePort)
          (new Thread(serviceObj)).start()
        } catch {
          case e: Exception => {
            LOG.error("Failed to create server to accept connection on port:" + nodePort, e)
            retval = false
          }
        }
      }
*/

    } catch {
      case e: Exception => {
        LOG.error("Failed to initialize.", e)
        retval = false
      }
    } finally {

    }

    return retval
  }

  def execCmd(ln: String): Boolean = {
    if (ln.length() > 0) {
      val trmln = ln.trim
      if (trmln.length() > 0 && (trmln.compareToIgnoreCase("Quit") == 0 || trmln.compareToIgnoreCase("Exit") == 0))
        return true
    }
    return false;
  }

  def update(o: Observable, arg: AnyRef): Unit = {
    val sig = arg.toString
    LOG.debug("Received signal: " + sig)
    if (sig.compareToIgnoreCase("SIGTERM") == 0 || sig.compareToIgnoreCase("SIGINT") == 0 || sig.compareToIgnoreCase("SIGABRT") == 0) {
      LOG.warn("Got " + sig + " signal. Shutting down the process")
      KamanjaConfiguration.shutdown = true
    }
  }

  def run(args: Array[String]): Int = {
    KamanjaConfiguration.Reset
    KamanjaLeader.Reset
    if (args.length == 0) {
      PrintUsage()
      return Shutdown(1)
    }

    val options = nextOption(Map(), args.toList)
    val version = options.getOrElse('version, "false").toString
    if (version.equalsIgnoreCase("true")) {
      KamanjaVersion.print
      return Shutdown(0)
    }
    val cfgfile = options.getOrElse('config, null)
    if (cfgfile == null) {
      LOG.error("Need configuration file as parameter")
      return Shutdown(1)
    }

    KamanjaConfiguration.configFile = cfgfile.toString
    val (loadConfigs, failStr) = Utils.loadConfiguration(KamanjaConfiguration.configFile, true)
    if (failStr != null && failStr.size > 0) {
      LOG.error(failStr)
      return Shutdown(1)
    }
    if (loadConfigs == null) {
      return Shutdown(1)
    }

    KamanjaConfiguration.allConfigs = loadConfigs

    {
      // Printing all configuration
      LOG.info("Configurations:")
      val it = loadConfigs.entrySet().iterator()
      val lowercaseconfigs = new Properties()
      while (it.hasNext()) {
        val entry = it.next();
        LOG.info("\t" + entry.getKey().asInstanceOf[String] + " -> " + entry.getValue().asInstanceOf[String])
      }
      LOG.info("\n")
    }

    if (LoadDynamicJarsIfRequired(loadConfigs) == false) {
      return Shutdown(1)
    }

    if (initialize == false) {
      return Shutdown(1)
    }

    // Jars loaded, create the status factory
    //val statusEventFactory =  KamanjaMetadata.envCtxt.getContainerInstance("system.KamanjaStatusEvent") //KamanjaMetadata.getMessgeInfo("system.KamanjaStatusEvent").contmsgobj.asInstanceOf[MessageFactoryInterface]

    val exceptionStatusAdaps = scala.collection.mutable.Set[String]()
    var curCntr = 0
    val maxFailureCnt = 30

    val statusPrint_PD = new Runnable {
      def run(): Unit = {
        val stats: scala.collection.immutable.Map[String, Long] = Map[String,Long]() // SimpleStats.copyMap
        val statsStr = stats.mkString("~")
        val dispStr = "PD,%d,%s,%s".format(KamanjaConfiguration.nodeId, Utils.GetCurDtTmStr, statsStr)
        val statusMsg: com.ligadata.KamanjaBase.KamanjaStatusEvent = KamanjaMetadata.envCtxt.getContainerInstance("system.KamanjaStatusEvent").asInstanceOf[KamanjaStatusEvent]
        statusMsg.nodeid = KamanjaConfiguration.nodeId.toString
        statusMsg.statusstring = statsStr
        statusMsg.eventtime = Utils.GetCurDtTmInMs // GetCurDtTmStr
        KamanjaMetadata.envCtxt.postMessages(Array[ContainerInterface](statusMsg))
      }
    }
  /*      val stats: scala.collection.immutable.Map[String, Long] = SimpleStats.copyMap
        val statsStr = stats.mkString("~")
        val dispStr = "PD,%d,%s,%s".format(KamanjaConfiguration.nodeId, Utils.GetCurDtTmStr, statsStr)
        var statusMsg = KamanjaMetadata.envCtxt.getContainerInstance("system.KamanjaStatusEvent")
        statusMsg.nodeid = KamanjaConfiguration.nodeId.toString
        statusMsg.statusstring = statsStr
        //statusMsg.eventtime = Utils.GetCurDtTmStr

        if (statusAdapters != null) {
          curCntr += 1
          statusAdapters.foreach(sa => {
            val adapNm = sa.inputConfig.Name
            val alreadyFailed = (exceptionStatusAdaps.size > 0 && exceptionStatusAdaps.contains(adapNm))
            try {
              if (alreadyFailed == false || curCntr >= maxFailureCnt) {
                sa.send(dispStr, "1")
                if (alreadyFailed)
                  exceptionStatusAdaps -= adapNm
              }
            } catch {
              case fae: FatalAdapterException => {
                LOG.error("Failed to send data to status adapter:" + adapNm, fae)
                if (alreadyFailed == false)
                  exceptionStatusAdaps += adapNm
              }
              case e: Exception => {
                LOG.error("Failed to send data to status adapter:" + adapNm, e)
                if (alreadyFailed == false)
                  exceptionStatusAdaps += adapNm
              }
              case t: Throwable => {
                LOG.error("Failed to send data to status adapter:" + adapNm, t)
                if (alreadyFailed == false)
                  exceptionStatusAdaps += adapNm
              }
            }
          })
          if (curCntr >= maxFailureCnt)
            curCntr = 0
        } else {
          LOG.info(dispStr)
        } */


    val metricsCollector = new Runnable {
      def run(): Unit = {
        try {
          validateAndExternalizeMetrics
        } catch {
          case e: Throwable => {
            LOG.warn("KamanjaManager " + KamanjaConfiguration.nodeId.toString + " unable to externalize statistics due to internal error. Check ZK connection", e)
          }
        }
      }
    }

    val scheduledThreadPool = Executors.newScheduledThreadPool(3);

    // scheduledThreadPool.scheduleWithFixedDelay(statusPrint_PD, 0, 1000, TimeUnit.MILLISECONDS);

    /**
     * print("=> ")
     * breakable {
     * for (ln <- io.Source.stdin.getLines) {
     * val rv = execCmd(ln)
     * if (rv)
     * break;
     * print("=> ")
     * }
     * }
     */

    var timeOutEndTime: Long = 0
    var participentsChangedCntr: Long = 0
    var lookingForDups = false
    var cntr: Long = 0
    var prevParticipents = ""

    val nodeNameToSetZk = KamanjaConfiguration.nodeId.toString

    var sh: SignalHandler = null
    try {
      sh = new SignalHandler()
      sh.addObserver(this)
      sh.handleSignal("TERM")
      sh.handleSignal("INT")
      sh.handleSignal("ABRT")
    } catch {
      case e: Throwable => {
        LOG.error("Failed to add signal handler.", e)
      }
    }

//    var nextAdapterValuesCommit = System.currentTimeMillis + KamanjaConfiguration.adapterInfoCommitTime

    LOG.warn("KamanjaManager is running now. Waiting for user to terminate with SIGTERM, SIGINT or SIGABRT signals")
    while (KamanjaConfiguration.shutdown == false) { // Infinite wait for now
//      if (KamanjaMetadata.envCtxt != null && nextAdapterValuesCommit < System.currentTimeMillis) {
//        if (ProcessedAdaptersInfo.CommitAdapterValues)
//          nextAdapterValuesCommit = System.currentTimeMillis + KamanjaConfiguration.adapterInfoCommitTime
//      }
      cntr = cntr + 1
      if (participentsChangedCntr != KamanjaConfiguration.participentsChangedCntr) {
        val dispWarn = (lookingForDups && timeOutEndTime > 0)
        lookingForDups = false
        timeOutEndTime = 0
        participentsChangedCntr = KamanjaConfiguration.participentsChangedCntr
        val cs = KamanjaLeader.GetClusterStatus
        if (cs.leaderNodeId != null && cs.participantsNodeIds != null && cs.participantsNodeIds.size > 0) {
          if (dispWarn) {
            LOG.warn("Got new participents. Trying to see whether the node still has duplicates participents. Previous Participents:{%s} Current Participents:{%s}".format(prevParticipents, cs.participantsNodeIds.mkString(",")))
          }
          prevParticipents = ""
          val isNotLeader = (cs.isLeader == false || cs.leaderNodeId != cs.nodeId)
          if (isNotLeader) {
            val sameNodeIds = cs.participantsNodeIds.filter(p => p == cs.nodeId)
            if (sameNodeIds.size > 1) {
              lookingForDups = true
              val (zkConnectString, zkNodeBasePath, zkSessionTimeoutMs, zkConnectionTimeoutMs)  = KamanjaMetadata.envCtxt.getZookeeperInfo
              var mxTm = if (zkSessionTimeoutMs > zkConnectionTimeoutMs) zkSessionTimeoutMs else zkConnectionTimeoutMs
              if (mxTm < 5000) // if the value is < 5secs, we are taking 5 secs
                mxTm = 5000
              timeOutEndTime = System.currentTimeMillis + mxTm + 2000 // waiting another 2secs
              LOG.error("Found more than one of NodeId:%s in Participents:{%s}. Waiting for %d milli seconds to check whether it is real duplicate or not.".format(cs.nodeId, cs.participantsNodeIds.mkString(","), mxTm))
              prevParticipents = cs.participantsNodeIds.mkString(",")
            }
          }
        }
      }

      if (lookingForDups && timeOutEndTime > 0) {
        if (timeOutEndTime < System.currentTimeMillis) {
          lookingForDups = false
          timeOutEndTime = 0
          val cs = KamanjaLeader.GetClusterStatus
          if (cs.leaderNodeId != null && cs.participantsNodeIds != null && cs.participantsNodeIds.size > 0) {
            val isNotLeader = (cs.isLeader == false || cs.leaderNodeId != cs.nodeId)
            if (isNotLeader) {
              val sameNodeIds = cs.participantsNodeIds.filter(p => p == cs.nodeId)
              if (sameNodeIds.size > 1) {
                LOG.error("Found more than one of NodeId:%s in Participents:{%s} for ever. Shutting down this node.".format(cs.nodeId, cs.participantsNodeIds.mkString(",")))
                KamanjaConfiguration.shutdown = true
              }
            }
          }
        }
      }

      try {
        Thread.sleep(500) // Waiting for 500 milli secs
      } catch {
        case e: Exception => {
          LOG.debug("", e)
        }
      }

      // See if we have to extenrnalize stats, every 5000ms..
      if (LOG.isTraceEnabled)
        LOG.trace("KamanjaManager " + KamanjaConfiguration.nodeId.toString + " running iteration " + cntr)

      if (!isTimerStarted) {
        scheduledThreadPool.scheduleWithFixedDelay(metricsCollector, 0, 5000, TimeUnit.MILLISECONDS);
        isTimerStarted = true
      }
    }

    scheduledThreadPool.shutdownNow()
    sh = null
    return Shutdown(0)
  }


  /**
   *
   */
  private def validateAndExternalizeMetrics: Unit = {
    val (zkConnectString, zkNodeBasePath, zkSessionTimeoutMs, zkConnectionTimeoutMs)  = KamanjaMetadata.envCtxt.getZookeeperInfo
    val zkHeartBeatNodePath = zkNodeBasePath + "/monitor/engine/" + KamanjaConfiguration.nodeId.toString
    val isLogDebugEnabled = LOG.isDebugEnabled

    if (isLogDebugEnabled)
      LOG.debug("KamanjaManager " + KamanjaConfiguration.nodeId.toString + " is externalizing metrics to " + zkNodeBasePath)

    // As part of the metrics externaization, look for changes to the configuration that can be affected
    // while the manager is running
    var changes = KamanjaMetadata.getConfigChanges
    if (!changes.isEmpty) {
      changes.foreach(x=>{ LOG.debug("KamanjaManger config update: " + x)})
    }

    if (thisEngineInfo == null) {
      thisEngineInfo = new MainInfo
      thisEngineInfo.startTime = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
      thisEngineInfo.name = KamanjaConfiguration.nodeId.toString
      thisEngineInfo.uniqueId = MonitoringContext.monitorCount.incrementAndGet
      CreateClient.CreateNodeIfNotExists(zkConnectString, zkHeartBeatNodePath) // Creating the path if missing
    }
    thisEngineInfo.lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
    thisEngineInfo.uniqueId = MonitoringContext.monitorCount.incrementAndGet

    // run through all adapters.
    if (adapterMetricInfo == null) {
      adapterMetricInfo = scala.collection.mutable.MutableList[com.ligadata.HeartBeat.MonitorComponentInfo]()
    }
    adapterMetricInfo.clear

    inputAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
    outputAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
    storageAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
//    statusAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
//    failedEventsAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
//    validateInputAdapters.foreach(ad => { adapterMetricInfo += ad.getComponentStatusAndMetrics })
//    adapterMetricInfo += KamanjaMetadata.envCtxt.getComponentStatusAndMetrics

    // Combine all the junk into a single JSON String
    import org.json4s.JsonDSL._
    val allMetrics =
        ("Name" -> thisEngineInfo.name) ~
        ("Version" -> (KamanjaVersion.getMajorVersion.toString + "." + KamanjaVersion.getMinorVersion.toString + "." + KamanjaVersion.getMicroVersion + "." + KamanjaVersion.getBuildNumber)) ~
        ("UniqueId" -> thisEngineInfo.uniqueId) ~
        ("LastSeen" -> thisEngineInfo.lastSeen) ~
        ("StartTime" -> thisEngineInfo.startTime) ~
        ("Components" -> adapterMetricInfo.map(mci =>
            ("Type" -> mci.typ) ~
            ("Name" -> mci.name) ~
            ("Description" -> mci.description) ~
            ("LastSeen" -> mci.lastSeen) ~
            ("StartTime" -> mci.startTime) ~
            ("Metrics" -> mci.metricsJsonString)))

    val statEvent: com.ligadata.KamanjaBase.KamanjaStatisticsEvent = KamanjaMetadata.envCtxt.getContainerInstance("system.KamanjaStatisticsEvent").asInstanceOf[KamanjaStatisticsEvent]
    statEvent.statistics = compact(render(allMetrics))
    // get the envContext.
    KamanjaLeader.SetNewDataToZkc(zkHeartBeatNodePath, compact(render(allMetrics)).getBytes)
    if (isLogDebugEnabled)
      LOG.debug("KamanjaManager " + KamanjaConfiguration.nodeId.toString + " externalized metrics for UID: " + thisEngineInfo.uniqueId)
  }

  class SignalHandler extends Observable with sun.misc.SignalHandler {
    def handleSignal(signalName: String) {
      sun.misc.Signal.handle(new sun.misc.Signal(signalName), this)
    }
    def handle(signal: sun.misc.Signal) {
      setChanged()
      notifyObservers(signal)
    }
  }
}

class MainInfo {
  var name: String = null
  var uniqueId: Long = 0
  var lastSeen: String = null
  var startTime: String = null
}

class ComponentInfo {
  var typ: String = null
  var name: String = null
  var desc: String = null
  var uniqueId: Long = 0
  var lastSeen: String = null
  var startTime: String = null
  var metrics: collection.mutable.Map[String, Any] = null
}

object KamanjaManager {
  private val LOG = LogManager.getLogger(getClass);
  var curMgr: KamanjaManager = _
  def main(args: Array[String]): Unit = {
    val mgr = new KamanjaManager
    scala.sys.addShutdownHook({
      if (KamanjaConfiguration.shutdown == false) {
        LOG.warn("Got Shutdown request")
        KamanjaConfiguration.shutdown = true // Setting the global shutdown
      }
    })

    curMgr = mgr
    sys.exit(mgr.run(args))
  }

  def incrAdapterChangedCntr(): Unit = curMgr.incrAdapterChangedCntr()
  def getAdapterChangedCntr = curMgr.getAdapterChangedCntr
  def getAllAdaptersInfo: (Array[InputAdapter], Array[OutputAdapter], Array[StorageAdapter], Long) = curMgr.getAllAdaptersInfo
}

