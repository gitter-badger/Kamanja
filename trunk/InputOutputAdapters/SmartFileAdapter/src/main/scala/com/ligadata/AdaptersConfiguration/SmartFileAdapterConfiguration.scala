package com.ligadata.AdaptersConfiguration

import com.ligadata.Exceptions.KamanjaException
import com.ligadata.InputOutputAdapterInfo._
import org.apache.logging.log4j.LogManager
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

/**
  * Created by Yasser on 3/10/2016.
  */
class SmartFileAdapterConfiguration extends AdapterConfiguration {
  var _type: String = _ // FileSystem, hdfs, sftp

  var connectionConfig : FileAdapterConnectionConfig = null
  var monitoringConfig : FileAdapterMonitoringConfig = null
}

class FileAdapterConnectionConfig {
  var hostsList: Array[String] = Array.empty[String]
  var userId: String = _
  var password: String = _
  var authentication: String = _
  var principal: String = _
  var keytab: String = _
  var passphrase: String = _
  var keyFile: String = _
}

class FileAdapterMonitoringConfig {
  var waitingTimeMS : Int = _
  var locations : Array[String] = Array.empty[String] //folders to monitor

  var fileBufferingTimeout = 300 // in seconds
  var targetMoveDir = ""
  var consumersCount : Int = _
  var workerBufferSize : Int = 4 //buffer size in MB to read messages from files
  var messageSeparator : Char = 10
}

object SmartFileAdapterConfiguration{

  val defaultWaitingTimeMS = 1000
  val defaultConsumerCount = 2

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  def getAdapterConfig(inputConfig: AdapterConfiguration): SmartFileAdapterConfiguration = {

    if (inputConfig.adapterSpecificCfg == null || inputConfig.adapterSpecificCfg.size == 0) {
      val err = "Not found Type and Connection info for Smart File Adapter Config:" + inputConfig.Name
      throw new KamanjaException(err, null)
    }

    val adapterConfig = new SmartFileAdapterConfiguration()
    adapterConfig.Name = inputConfig.Name
//    adapterConfig.formatName = inputConfig.formatName
//    adapterConfig.validateAdapterName = inputConfig.validateAdapterName
//    adapterConfig.failedEventsAdapterName = inputConfig.failedEventsAdapterName
    adapterConfig.className = inputConfig.className
    adapterConfig.jarName = inputConfig.jarName
    adapterConfig.dependencyJars = inputConfig.dependencyJars
//    adapterConfig.associatedMsg = if (inputConfig.associatedMsg == null) null else inputConfig.associatedMsg.trim
//    adapterConfig.keyAndValueDelimiter = if (inputConfig.keyAndValueDelimiter == null) null else inputConfig.keyAndValueDelimiter.trim
//    adapterConfig.fieldDelimiter = if (inputConfig.fieldDelimiter == null) null else inputConfig.fieldDelimiter.trim
//    adapterConfig.valueDelimiter = if (inputConfig.valueDelimiter == null) null else inputConfig.valueDelimiter.trim

    adapterConfig.adapterSpecificCfg = inputConfig.adapterSpecificCfg

    logger.debug("SmartFileAdapterConfiguration (getAdapterConfig)- inputConfig.adapterSpecificCfg==null is "+
      (inputConfig.adapterSpecificCfg == null))
    val (_type, connectionConfig, monitoringConfig) = parseSmartFileAdapterSpecificConfig(inputConfig.Name, inputConfig.adapterSpecificCfg)
    adapterConfig._type = _type
    adapterConfig.connectionConfig = connectionConfig
    adapterConfig.monitoringConfig = monitoringConfig

    adapterConfig
  }

  def parseSmartFileAdapterSpecificConfig(adapterName : String, adapterSpecificCfgJson : String) : (String, FileAdapterConnectionConfig, FileAdapterMonitoringConfig) = {

    val adapCfg = parse(adapterSpecificCfgJson)

    if (adapCfg == null || adapCfg.values == null) {
      val err = "Not found Type and Connection info for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }

    val adapCfgValues = adapCfg.values.asInstanceOf[Map[String, Any]]

    if(adapCfgValues.getOrElse("Type", null) == null) {
      val err = "Not found Type for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }
    val _type = adapCfgValues.get("Type").get.toString

    val connectionConfig = new FileAdapterConnectionConfig()
    val monitoringConfig = new FileAdapterMonitoringConfig()

    if(adapCfgValues.getOrElse("ConnectionConfig", null) == null){
      val err = "Not found ConnectionConfig for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }

    val connConf = adapCfgValues.get("ConnectionConfig").get.asInstanceOf[Map[String, String]]
    //val connConfValues = connConf.values.asInstanceOf[Map[String, String]]
    connConf.foreach(kv => {
      if (kv._1.compareToIgnoreCase("HostLists") == 0) {
        connectionConfig.hostsList = kv._2.split(",").map(str => str.trim).filter(str => str.size > 0)
      } else if (kv._1.compareToIgnoreCase("UserId") == 0) {
        connectionConfig.userId = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("Password") == 0) {
        connectionConfig.password = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("Authentication") == 0) {
        connectionConfig.authentication = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("Principal") == 0) {//kerberos
        connectionConfig.principal = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("Keytab") == 0) {//kerberos
        connectionConfig.keytab = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("Passphrase") == 0) {//ssh
        connectionConfig.passphrase = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("KeyFile") == 0) {//ssh
        connectionConfig.keyFile = kv._2.trim
      }
    })
    if(connectionConfig.authentication == null || connectionConfig.authentication == "")
      connectionConfig.authentication = "simple"//default

    if(adapCfgValues.getOrElse("MonitoringConfig", null) == null){
      val err = "Not found MonitoringConfig for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }
    val monConf = (adapCfgValues.get("MonitoringConfig").get.asInstanceOf[Map[String, String]])
    //val monConfValues = monConf.values.asInstanceOf[Map[String, String]]
    monConf.foreach(kv => {
      if (kv._1.compareToIgnoreCase("MaxTimeWait") == 0) {
        monitoringConfig.waitingTimeMS = kv._2.trim.toInt
        if (monitoringConfig.waitingTimeMS < 0)
          monitoringConfig.waitingTimeMS = defaultWaitingTimeMS
      } else if (kv._1.compareToIgnoreCase("ConsumersCount") == 0) {
        monitoringConfig.consumersCount = kv._2.trim.toInt
        if (monitoringConfig.consumersCount < 0)
          monitoringConfig.consumersCount = defaultConsumerCount
      }
      else  if (kv._1.compareToIgnoreCase("Locations") == 0) {
        monitoringConfig.locations = kv._2.split(",").map(str => str.trim).filter(str => str.size > 0)
      }
      else  if (kv._1.compareToIgnoreCase("TargetMoveDir") == 0) {
        monitoringConfig.targetMoveDir = kv._2.trim
      }
      else if (kv._1.compareToIgnoreCase("WorkerBufferSize") == 0) {
        monitoringConfig.workerBufferSize = kv._2.trim.toInt
      }
      else if (kv._1.compareToIgnoreCase("MessageSeparator") == 0) {
        monitoringConfig.messageSeparator = kv._2.trim.toInt.toChar
      }
    })

    if(monitoringConfig.locations == null || monitoringConfig.locations.length == 0) {
      val err = "Not found Locations for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }

    if(monitoringConfig.targetMoveDir == null || monitoringConfig.targetMoveDir.length == 0) {
      val err = "Not found targetMoveDir for Smart File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }

    (_type, connectionConfig, monitoringConfig)
  }
}

case class SmartFileKeyData(Version: Int, Type: String, Name: String, PartitionId: Int)

class SmartFilePartitionUniqueRecordKey extends PartitionUniqueRecordKey {
  val Version: Int = 1
  var Name: String = _ // Name
  val Type: String = "SmartFile"
  var PartitionId: Int = _ // Partition Id

  override def Serialize: String = { // Making String from key
  val json =
    ("Version" -> Version) ~
      ("Type" -> Type) ~
      ("Name" -> Name) ~
      ("PartitionId" -> PartitionId)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = { // Making Key from Serialized String
  implicit val jsonFormats: Formats = DefaultFormats
    val keyData = parse(key).extract[SmartFileKeyData]
    if (keyData.Version == Version && keyData.Type.compareTo(Type) == 0) {
      Name = keyData.Name
      PartitionId = keyData.PartitionId
    }
    // else { } // Not yet handling other versions
  }
}

case class SmartFileRecData(Version: Int, FileName : String, Offset: Option[Long])

class SmartFilePartitionUniqueRecordValue extends PartitionUniqueRecordValue {
  val Version: Int = 1
  var FileName : String = _
  var Offset: Long = -1 // Offset in the file

  override def Serialize: String = {
    // Making String from Value
    val json =
      ("Version" -> Version) ~
        ("Offset" -> Offset) ~
        ("FileName" -> FileName)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = {
    // Making Value from Serialized String
    implicit val jsonFormats: Formats = DefaultFormats
    val recData = parse(key).extract[SmartFileRecData]
    if (recData.Version == Version) {
      Offset = recData.Offset.get
      FileName = recData.FileName
    }
    // else { } // Not yet handling other versions
  }
}