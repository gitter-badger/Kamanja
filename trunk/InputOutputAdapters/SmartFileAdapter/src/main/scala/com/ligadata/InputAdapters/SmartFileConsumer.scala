package com.ligadata.InputAdapters

import com.ligadata.HeartBeat.MonitorComponentInfo
import com.ligadata.InputOutputAdapterInfo._
import com.ligadata.AdaptersConfiguration._
import org.apache.logging.log4j.LogManager

import scala.actors.threadpool.ExecutorService

/**
  * Created by Yasser on 3/13/2016.
  */
object SmartFileConsumer extends InputAdapterObj {
  val MONITOR_FREQUENCY = 10000 // Monitor Topic queues every 20 seconds
  val SLEEP_DURATION = 1000 // Allow 1 sec between unsucessful fetched
  var CURRENT_BROKER: String = _
  val FETCHSIZE = 64 * 1024
  val ZOOKEEPER_CONNECTION_TIMEOUT_MS = 3000
  val MAX_TIMEOUT = 60000
  val INIT_TIMEOUT = 250

  def CreateInputAdapter(inputConfig: AdapterConfiguration, callerCtxt: InputAdapterCallerContext, execCtxtObj: ExecContextObj, cntrAdapter: CountersAdapter): InputAdapter = new SmartFileConsumer(inputConfig, callerCtxt, execCtxtObj, cntrAdapter)
}

class SmartFileConsumer(val inputConfig: AdapterConfiguration, val callerCtxt: InputAdapterCallerContext, val execCtxtObj: ExecContextObj, cntrAdapter: CountersAdapter) extends InputAdapter {

  lazy val loggerName = this.getClass.getName
  lazy val LOG = LogManager.getLogger(loggerName)

  private val lock = new Object()
  private var readExecutor: ExecutorService = _

  private val adapterConfig = SmartFileAdapterConfiguration.getAdapterConfig(inputConfig)
  private var isShutdown = false
  private var isQuiesced = false
  private var startTime: Long = 0

  private var metrics: collection.mutable.Map[String,Any] = collection.mutable.Map[String,Any]()

  override def Shutdown: Unit = lock.synchronized {
    StopProcessing
  }

  override def DeserializeKey(k: String): PartitionUniqueRecordKey = ???

  override def DeserializeValue(v: String): PartitionUniqueRecordValue = ???

  override def getAllPartitionBeginValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = ???

  // each value in partitionInfo is (PartitionUniqueRecordKey, PartitionUniqueRecordValue, Long, PartitionUniqueRecordValue). // key, processed value, Start transactionid, Ignore Output Till given Value (Which is written into Output Adapter) & processing Transformed messages (processing & total)
  override def GetAllPartitionUniqueRecordKey: Array[PartitionUniqueRecordKey] = ???

  override def getAllPartitionEndValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = ???

  override def StartProcessing(partitionInfo: Array[StartProcPartInfo], ignoreFirstMsg: Boolean): Unit = ???

  override def StopProcessing: Unit = {
    isShutdown = true
    terminateReaderTasks
  }

  private def terminateReaderTasks(): Unit = {
    if (readExecutor == null) return

    // Tell all thread to stop processing on the next interval, and shutdown the Excecutor.
    quiesce

    // Give the threads to gracefully stop their reading cycles, and then execute them with extreme prejudice.
    Thread.sleep(adapterConfig.monitoringConfig.waitingTimeMS)
    readExecutor.shutdownNow
    while (readExecutor.isTerminated == false) {
      Thread.sleep(100)
    }

    LOG.debug("Smart File Adapter - Shutdown Complete")
    readExecutor = null
    startTime = 0
  }

  /* no need for any synchronization here... it can only go one way.. worst case scenario, a reader thread gets to try to
*  read  one extra time (100ms lost)
 */
  private def quiesce: Unit = {
    isQuiesced = true
  }

  override def getComponentStatusAndMetrics: MonitorComponentInfo = ???
}