package com.ligadata.tools.containersutility

import java.io.File
import java.util.{TreeMap, Properties}
import com.ligadata.Exceptions._
import com.ligadata.KamanjaBase._
import com.ligadata.KvBase._
import com.ligadata.MetadataAPI.MetadataAPIImpl
import com.ligadata.Serialize.JZKInfo
import com.ligadata.StorageBase.DataStore
import com.ligadata.Utils.{KamanjaClassLoader, Utils, KamanjaLoaderInfo}
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata.{BaseElem, BaseTypeDef, NodeInfo}
import com.ligadata.keyvaluestore.KeyValueManager
import org.apache.curator.framework.CuratorFramework
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Formats}
import scala.collection.mutable.TreeSet

/**
  * Created by Yousef on 3/9/2016.
  */
class UtilityForContainers(val loadConfigs: Properties, val typename: String,val keyfield: String, operation: String, keyid: String) extends LogTrait with MdBaseResolveInfo {

  var isOk: Boolean = true
  var zkcForSetData: CuratorFramework = null
  var totalCommittedMsgs: Int = 0

  val containerUtilityLoder = new KamanjaLoaderInfo

  containersUtilityConfiguration.nodeId = loadConfigs.getProperty("nodeId".toLowerCase, "0").replace("\"", "").trim.toInt
  if (containersUtilityConfiguration.nodeId <= 0) {
    logger.error("Not found valid nodeId. It should be greater than 0")
    isOk = false
  }

  var nodeInfo: NodeInfo = _

  if (isOk) {
    MetadataAPIImpl.InitMdMgrFromBootStrap(containersUtilityConfiguration.configFile, false)

    nodeInfo = mdMgr.Nodes.getOrElse(containersUtilityConfiguration.nodeId.toString, null)
    if (nodeInfo == null) {
      logger.error("Node %d not found in metadata".format(containersUtilityConfiguration.nodeId))
      isOk = false
    }
  }

  if (isOk) {
    containersUtilityConfiguration.jarPaths = if (nodeInfo.JarPaths == null) Array[String]().toSet else nodeInfo.JarPaths.map(str => str.replace("\"", "").trim).filter(str => str.size > 0).toSet
    if (containersUtilityConfiguration.jarPaths.size == 0) {
      logger.error("Not found valid JarPaths.")
      isOk = false
    }
  }

  val cluster = if (isOk) mdMgr.ClusterCfgs.getOrElse(nodeInfo.ClusterId, null) else null
  if (isOk && cluster == null) {
    logger.error("Cluster not found for Node %d  & ClusterId : %s".format(containersUtilityConfiguration.nodeId, nodeInfo.ClusterId))
    isOk = false
  }

  val dataStore = if (isOk) cluster.cfgMap.getOrElse("DataStore", null) else null
  if (isOk && dataStore == null) {
    logger.error("DataStore not found for Node %d  & ClusterId : %s".format(containersUtilityConfiguration.nodeId, nodeInfo.ClusterId))
    isOk = false
  }

  val zooKeeperInfo = if (isOk) cluster.cfgMap.getOrElse("ZooKeeperInfo", null) else null
  if (isOk && dataStore == null) {
    logger.error("ZooKeeperInfo not found for Node %d  & ClusterId : %s".format(containersUtilityConfiguration.nodeId, nodeInfo.ClusterId))
    isOk = false
  }

  var dataDataStoreInfo: String = null
  var zkConnectString: String = null
  var zkNodeBasePath: String = null
  var zkSessionTimeoutMs: Int = 0
  var zkConnectionTimeoutMs: Int = 0

  if (isOk) {
    implicit val jsonFormats: Formats = DefaultFormats
    val zKInfo = parse(zooKeeperInfo).extract[JZKInfo]

    dataDataStoreInfo = dataStore

    if (isOk) {
      zkConnectString = zKInfo.ZooKeeperConnectString.replace("\"", "").trim
      zkNodeBasePath = zKInfo.ZooKeeperNodeBasePath.replace("\"", "").trim
      zkSessionTimeoutMs = if (zKInfo.ZooKeeperSessionTimeoutMs == None || zKInfo.ZooKeeperSessionTimeoutMs == null) 0 else zKInfo.ZooKeeperSessionTimeoutMs.get.toString.toInt
      zkConnectionTimeoutMs = if (zKInfo.ZooKeeperConnectionTimeoutMs == None || zKInfo.ZooKeeperConnectionTimeoutMs == null) 0 else zKInfo.ZooKeeperConnectionTimeoutMs.get.toString.toInt

      // Taking minimum values in case if needed
      if (zkSessionTimeoutMs <= 0)
        zkSessionTimeoutMs = 30000
      if (zkConnectionTimeoutMs <= 0)
        zkConnectionTimeoutMs = 30000
    }

    if (zkConnectString.size == 0) {
      logger.warn("Not found valid Zookeeper connection string.")
    }

    if (zkConnectString.size > 0 && zkNodeBasePath.size == 0) {
      logger.warn("Not found valid Zookeeper ZNode Base Path.")
    }
  }

  var typeNameCorrType: BaseTypeDef = _
  var kvTableName: String = _
  var messageObj: BaseMsgObj = _
  var containerObj: BaseContainerObj = _
  var objFullName: String = _

  if (isOk) {
    typeNameCorrType = mdMgr.ActiveType(typename.toLowerCase)
    if (typeNameCorrType == null || typeNameCorrType == None) {
      logger.error("Not found valid type for " + typename.toLowerCase)
      isOk = false
    } else {
      objFullName = typeNameCorrType.FullName.toLowerCase
      kvTableName = objFullName.replace('.', '_')
    }
  }

  var isMsg = false
  var isContainer = false

  if (isOk) {
    isOk = LoadJarIfNeeded(typeNameCorrType, containerUtilityLoder.loadedJars, containerUtilityLoder.loader)
  }

  if (isOk) {
    var clsName = typeNameCorrType.PhysicalName.trim
    if (clsName.size > 0 && clsName.charAt(clsName.size - 1) != '$') // if no $ at the end we are taking $
      clsName = clsName + "$"

    if (isMsg == false) {
      // Checking for Message
      try {
        // Convert class name into a class
        var curClz = Class.forName(clsName, true, containerUtilityLoder.loader)

        while (curClz != null && isContainer == false) {
          isContainer = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.BaseContainerObj")
          if (isContainer == false)
            curClz = curClz.getSuperclass()
        }
      } catch {
        case e: Exception => {
          logger.error("Failed to load message class %s".format(clsName), e)
        }
      }
    }

    if (isContainer == false) {
      // Checking for container
      try {
        // If required we need to enable this test
        // Convert class name into a class
        var curClz = Class.forName(clsName, true, containerUtilityLoder.loader)

        while (curClz != null && isMsg == false) {
          isMsg = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.BaseMsgObj")
          if (isMsg == false)
            curClz = curClz.getSuperclass()
        }
      } catch {
        case e: Exception => {
          logger.error("Failed to load container class %s".format(clsName), e)
        }
      }
    }

    if (isMsg || isContainer) {
      try {
        val module = containerUtilityLoder.mirror.staticModule(clsName)
        val obj = containerUtilityLoder.mirror.reflectModule(module)
        val objinst = obj.instance
        if (objinst.isInstanceOf[BaseMsgObj]) {
          messageObj = objinst.asInstanceOf[BaseMsgObj]
          logger.debug("Created Message Object")
        } else if (objinst.isInstanceOf[BaseContainerObj]) {
          containerObj = objinst.asInstanceOf[BaseContainerObj]
          logger.debug("Created Container Object")
        } else {
          logger.error("Failed to instantiate message or conatiner object :" + clsName)
          isOk = false
        }
      } catch {
        case e: Exception => {
          logger.error("Failed to instantiate message or conatiner object:" + clsName, e)
          isOk = false
        }
      }
    } else {
      logger.error("Failed to instantiate message or conatiner object :" + clsName)
      isOk = false
    }
  }

  private def LoadJarIfNeeded(elem: BaseElem, loadedJars: TreeSet[String], loader: KamanjaClassLoader): Boolean = {
    if (containersUtilityConfiguration.jarPaths == null) return false

    var retVal: Boolean = true
    var allJars: Array[String] = null

    val jarname = if (elem.JarName == null) "" else elem.JarName.trim

    if (elem.DependencyJarNames != null && elem.DependencyJarNames.size > 0 && jarname.size > 0) {
      allJars = elem.DependencyJarNames :+ jarname
    } else if (elem.DependencyJarNames != null && elem.DependencyJarNames.size > 0) {
      allJars = elem.DependencyJarNames
    } else if (jarname.size > 0) {
      allJars = Array(jarname)
    } else {
      return retVal
    }

    val jars = allJars.map(j => Utils.GetValidJarFile(containersUtilityConfiguration.jarPaths, j))

    // Loading all jars
    for (j <- jars) {
      logger.debug("Processing Jar " + j.trim)
      val fl = new File(j.trim)
      if (fl.exists) {
        try {
          if (loadedJars(fl.getPath())) {
            logger.debug("Jar " + j.trim + " already loaded to class path.")
          } else {
            loader.addURL(fl.toURI().toURL())
            logger.debug("Jar " + j.trim + " added to class path.")
            loadedJars += fl.getPath()
          }
        } catch {
          case e: Exception => {
            logger.error("Jar " + j.trim + " failed added to class path.", e)
            return false
          }
        }
      } else {
        logger.error("Jar " + j.trim + " not found")
        return false
      }
    }

    true
  }

  override def getMessgeOrContainerInstance(MsgContainerType: String): MessageContainerBase = {
    if (MsgContainerType.compareToIgnoreCase(objFullName) != 0)
      return null
    // Simply creating new object and returning. Not checking for MsgContainerType. This is issue if the child level messages ask for the type
    if (isMsg)
      return messageObj.CreateNewMessage
    if (isContainer)
      return containerObj.CreateNewContainer
    return null
  }

  private def GetDataStoreHandle(jarPaths: collection.immutable.Set[String], dataStoreInfo: String): DataStore = {
    try {
      logger.debug("Getting DB Connection for dataStoreInfo:%s".format(dataStoreInfo))
      return KeyValueManager.Get(jarPaths, dataStoreInfo)
    } catch {
      case e: Exception => throw e
      case e: Throwable => throw e
    }
  }



  private def collectKeyAndValues(k: Key, v: Value, dataByBucketKeyPart: TreeMap[KeyWithBucketIdAndPrimaryKey, MessageContainerBaseWithModFlag], loadedKeys: java.util.TreeSet[LoadKeyWithBucketId]): Unit = {
    val value = SerializeDeserialize.Deserialize(v.serializedInfo, this, containerUtilityLoder.loader, true, "")
    val primarykey = value.PrimaryKeyData
    val key = KeyWithBucketIdAndPrimaryKey(KeyWithBucketIdAndPrimaryKeyCompHelper.BucketIdForBucketKey(k.bucketKey), k, primarykey != null && primarykey.size > 0, primarykey)
    dataByBucketKeyPart.put(key, MessageContainerBaseWithModFlag(false, value))

    val bucketId = KeyWithBucketIdAndPrimaryKeyCompHelper.BucketIdForBucketKey(k.bucketKey)
    val loadKey = LoadKeyWithBucketId(bucketId, TimeRange(k.timePartition, k.timePartition), k.bucketKey)
    loadedKeys.add(loadKey)
  }

  private def LoadDataIfNeeded(loadKey: LoadKeyWithBucketId, loadedKeys: java.util.TreeSet[LoadKeyWithBucketId], dataByBucketKeyPart: TreeMap[KeyWithBucketIdAndPrimaryKey, MessageContainerBaseWithModFlag], kvstore: DataStore): Unit = {
    if (loadedKeys.contains(loadKey))
      return
    val buildOne = (k: Key, v: Value) => {
      collectKeyAndValues(k, v, dataByBucketKeyPart, loadedKeys)
    }

    var failedWaitTime = 15000 // Wait time starts at 15 secs
    val maxFailedWaitTime = 60000 // Max Wait time 60 secs
    var doneGet = false

    while (!doneGet) {
      try {
        kvstore.get(objFullName, Array(loadKey.tmRange), Array(loadKey.bucketKey), buildOne)
        loadedKeys.add(loadKey)
        doneGet = true
      } catch {
        case e @ (_: ObjectNotFoundException | _: KeyNotFoundException) => {
          logger.debug("In Container %s Key %s Not found for timerange: %d-%d".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
          doneGet = true
        }
        case e: FatalAdapterException => {
          logger.error("In Container %s Key %s Not found for timerange: %d-%d.".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
        }
        case e: StorageDMLException => {
          logger.error("In Container %s Key %s Not found for timerange: %d-%d.".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
        }
        case e: StorageDDLException => {
          logger.error("In Container %s Key %s Not found for timerange: %d-%d.".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
        }
        case e: Exception => {
          logger.error("In Container %s Key %s Not found for timerange: %d-%d.".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
        }
        case e: Throwable => {
          logger.error("In Container %s Key %s Not found for timerange: %d-%d.".format(objFullName, loadKey.bucketKey.mkString(","), loadKey.tmRange.beginTime, loadKey.tmRange.endTime), e)
        }
      }

      if (!doneGet) {
        try {
          logger.error("Failed to get data from datastore. Waiting for another %d milli seconds and going to start them again.".format(failedWaitTime))
          Thread.sleep(failedWaitTime)
        } catch {
          case e: Exception => { logger.warn("", e) }
        }
        // Adjust time for next time
        if (failedWaitTime < maxFailedWaitTime) {
          failedWaitTime = failedWaitTime * 2
          if (failedWaitTime > maxFailedWaitTime)
            failedWaitTime = maxFailedWaitTime
        }
      }
    }
  }

  private val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  private def SimpDateFmtTimeFromMs(tmMs: Long): String = {
    formatter.format(new java.util.Date(tmMs))
  }

  private def GetCurDtTmStr: String = {
    SimpDateFmtTimeFromMs(System.currentTimeMillis)
  }
}
