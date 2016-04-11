package com.ligadata.tools.containersutility

import java.io.File
import java.util
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
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import scala.collection.immutable.Map
/**
  * Created by Yousef on 3/9/2016.
  */
class UtilityForContainers(val loadConfigs: Properties, val typename: String) extends LogTrait with MdBaseResolveInfo {

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

  val dataStore = if (isOk) cluster.cfgMap.getOrElse("SystemCatalog", null) else null
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
  var messageObj: MessageFactoryInterface = _
  var containerObj: ContainerFactoryInterface = _
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
          isContainer = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.ContainerFactoryInterface")
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
          isMsg = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.MessageFactoryInterface")
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
        if (objinst.isInstanceOf[MessageFactoryInterface]) {
          messageObj = objinst.asInstanceOf[MessageFactoryInterface]
          logger.debug("Created Message Object")
        } else if (objinst.isInstanceOf[ContainerFactoryInterface]) {
          containerObj = objinst.asInstanceOf[ContainerFactoryInterface]
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

  override def getMessgeOrContainerInstance(MsgContainerType: String): ContainerInterface = {
    if (MsgContainerType.compareToIgnoreCase(objFullName) != 0)
      return null
    // Simply creating new object and returning. Not checking for MsgContainerType. This is issue if the child level messages ask for the type
    if (isMsg)
      return messageObj.createInstance.asInstanceOf[ContainerInterface]
    if (isContainer)
      return containerObj.createInstance.asInstanceOf[ContainerInterface]
    return null
  }

   def GetDataStoreHandle(jarPaths: collection.immutable.Set[String], dataStoreInfo: String): DataStore = {
    try {
      logger.debug("Getting DB Connection for dataStoreInfo:%s".format(dataStoreInfo))
      return KeyValueManager.Get(jarPaths, dataStoreInfo)
    } catch {
      case e: Exception => throw e
      case e: Throwable => throw e
    }
  }

  private val formatter = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

  private def SimpDateFmtTimeFromMs(tmMs: Long): String = {
    formatter.format(new java.util.Date(tmMs))
  }

  private def GetCurDtTmStr: String = {
    SimpDateFmtTimeFromMs(System.currentTimeMillis)
  }
  // this method used to purge (truncate) container
   def TruncateContainer(typename: String, kvstore: DataStore): Unit ={
    logger.info("Truncate %s container".format(typename))
    kvstore.TruncateContainer(Array(typename))
  }
  // this method used to dalete data from container for a specific keys in a specific time ranges
   def DeleteFromContainer(typename: String, keyids: Array[Array[String]], timeranges: Array[TimeRange], kvstore: DataStore): Unit ={
//    logger.info("delete data from %s container for %s keys and timerange: %d-%d".format(typename,keyids,timeranges.beginTime,timeranges.endTime))
    if(keyids.length == 0)
      timeranges.foreach(timerange => {
        logger.info("delete from %s container for timerange: %d-%d".format(typename, timerange.beginTime,timerange.endTime))
      kvstore.del(typename, timerange)
      })
    else  if (timeranges.length == 0) {
      var keyList = scala.collection.immutable.List.empty[Key]
      val deleteKey = (k: Key) => {
        keyList = keyList :+ k
      }

      kvstore.getKeys(typename, keyids, deleteKey)
     // val keyArrays: Array[Key] = keyArraybuf.toArray
      val keyArrays: Array[Key] = keyList.toArray
      kvstore.del(typename, keyArrays)
    } else
      timeranges.foreach(timerange => {
        logger.info("delete from %s container for keyid: %s and timerange: %d-%d".format(typename, keyids, timerange.beginTime, timerange.endTime))
        kvstore.del(typename, timerange, keyids)
      })
  }

  //this method used to get data from container for a specific key in a specific time ranges
   def GetFromContainer(typename: String, keyArray: Array[Array[String]], timeranges: Array[TimeRange], kvstore: DataStore): Map[String,String] ={

    var data : Map[String,String] = null
    val retriveData = (k: Key, v: Any, serializerTyp: String, typeName: String, ver: Int)=>{
      val value = v.asInstanceOf[ContainerInterface]
      val primarykey = value.getPrimaryKey
      val key = KeyWithBucketIdAndPrimaryKey(KeyWithBucketIdAndPrimaryKeyCompHelper.BucketIdForBucketKey(k.bucketKey), k, primarykey != null && primarykey.size > 0, primarykey)
      val bucketId = KeyWithBucketIdAndPrimaryKeyCompHelper.BucketIdForBucketKey(k.bucketKey)
     // val keyValue = value.get(k.toString)
      data = data + (k.bucketKey.toString -> value.toString/*keyValue.toString*/) // this includes key and value
    }
      //logger.info("select data from %s container for %s key and timerange: %d-%d".format(typename,timerange.beginTime,timerange.endTime))
    if(keyArray.length == 0)
      kvstore.get(typename, timeranges, retriveData)
    else if (timeranges.length == 0)
      kvstore.get(typename, keyArray, retriveData)
    else
      kvstore.get(typename,timeranges,keyArray,retriveData)
    return data
  }
}
