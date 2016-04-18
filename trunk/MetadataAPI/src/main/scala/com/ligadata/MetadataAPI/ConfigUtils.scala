/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.MetadataAPI

import java.util.Properties
import java.io._
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.Date
import java.text.ParseException
import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.MetadataAPI.MetadataAPI.ModelType.ModelType

import scala.Enumeration
import scala.io._
import scala.collection.mutable.ArrayBuffer

import scala.collection.mutable._
import scala.reflect.runtime.{ universe => ru }

import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MdMgr._

import com.ligadata.kamanja.metadataload.MetadataLoad

// import com.ligadata.keyvaluestore._
import com.ligadata.HeartBeat.{MonitoringContext, HeartBeatUtil}
import com.ligadata.StorageBase.{ DataStore, Transaction }
import com.ligadata.KvBase.{ Key, TimeRange }

import scala.util.parsing.json.JSON
import scala.util.parsing.json.{ JSONObject, JSONArray }
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import scala.collection.mutable.HashMap

import com.google.common.base.Throwables

import com.ligadata.msgcompiler._
import com.ligadata.Exceptions._

import scala.xml.XML
import org.apache.logging.log4j._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import com.ligadata.ZooKeeper._
import org.apache.curator.framework.CuratorFramework
import org.apache.zookeeper.CreateMode

import com.ligadata.keyvaluestore._
import com.ligadata.Serialize._
import com.ligadata.Utils._
import scala.util.control.Breaks._
import com.ligadata.AuditAdapterInfo._
import com.ligadata.SecurityAdapterInfo.SecurityAdapter
import com.ligadata.keyvaluestore.KeyValueManager
import com.ligadata.Exceptions.StackTrace

import java.util.Date
import org.json4s.jackson.Serialization

// The implementation class
object ConfigUtils {
  lazy val serializerType = "json4s"//"kryo"
  //lazy val serializer = SerializerManager.GetSerializer(serializerType)
  /**
   *
   */
  private var cfgmap: Map[String, Any] = null

  // For future debugging  purposes, we want to know which properties were not set - so create a set
  // of values that can be set via our config files
  var pList: Set[String] = Set("ZK_SESSION_TIMEOUT_MS", "ZK_CONNECTION_TIMEOUT_MS", "DATABASE_SCHEMA", "DATABASE", "DATABASE_LOCATION", "DATABASE_HOST", "API_LEADER_SELECTION_ZK_NODE",
    "JAR_PATHS", "JAR_TARGET_DIR", "ROOT_DIR", "GIT_ROOT", "SCALA_HOME", "JAVA_HOME", "MANIFEST_PATH", "CLASSPATH", "NOTIFY_ENGINE", "SERVICE_HOST",
    "ZNODE_PATH", "ZOOKEEPER_CONNECT_STRING", "COMPILER_WORK_DIR", "SERVICE_PORT", "MODEL_FILES_DIR", "TYPE_FILES_DIR", "FUNCTION_FILES_DIR",
    "CONCEPT_FILES_DIR", "MESSAGE_FILES_DIR", "CONTAINER_FILES_DIR", "CONFIG_FILES_DIR", "MODEL_EXEC_LOG", "NODE_ID", "SSL_CERTIFICATE", "SSL_PASSWD", "DO_AUTH", "SECURITY_IMPL_CLASS",
    "SECURITY_IMPL_JAR", "AUDIT_IMPL_CLASS", "AUDIT_IMPL_JAR", "DO_AUDIT", "AUDIT_PARMS", "ADAPTER_SPECIFIC_CONFIG", "METADATA_DATASTORE")


  // This is used to exclude all non-engine related configs from Uplodad Config method 
  private val excludeList: Set[String] = Set[String]("ClusterId", "Nodes", "Config", "Adapters", "SystemCatalog", "ZooKeeperInfo", "EnvironmentContext", "Cache")

    /**
     * AddNode
      *
      * @param nodeId a cluster node
     * @param nodePort
     * @param nodeIpAddr
     * @param jarPaths Set of paths where jars are located
     * @param scala_home
     * @param java_home
     * @param classpath
     * @param clusterId
     * @param power
     * @param roles
     * @param description
     * @return
     */
  def AddNode(nodeId: String, nodePort: Int, nodeIpAddr: String,
    jarPaths: List[String], scala_home: String,
    java_home: String, classpath: String,
    clusterId: String, power: Int,
    roles: Array[String], description: String): String = {
    try {
      // save in memory
      val ni = MdMgr.GetMdMgr.MakeNode(nodeId, nodePort, nodeIpAddr, jarPaths, scala_home,
        java_home, classpath, clusterId, power, roles, description)
      MdMgr.GetMdMgr.AddNode(ni)
      // save in database
      val key = "NodeInfo." + nodeId
      val value = MetadataAPISerialization.serializeObjectToJson(ni).getBytes//serializer.SerializeObjectToByteArray(ni)
      MetadataAPIImpl.SaveObject(key.toLowerCase, value, "config_objects", serializerType)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddNode", null, ErrorCodeConstants.Add_Node_Successful + ":" + nodeId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddNode", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Node_Failed + ":" + nodeId)
        apiResult.toString()
      }
    }
  }

    /**
     * UpdateNode
      *
      * @param nodeId a cluster node
     * @param nodePort
     * @param nodeIpAddr
     * @param jarPaths Set of paths where jars are located
     * @param scala_home
     * @param java_home
     * @param classpath
     * @param clusterId
     * @param power
     * @param roles
     * @param description
     * @return
     */
  def UpdateNode(nodeId: String, nodePort: Int, nodeIpAddr: String,
    jarPaths: List[String], scala_home: String,
    java_home: String, classpath: String,
    clusterId: String, power: Int,
    roles: Array[String], description: String): String = {
    AddNode(nodeId, nodePort, nodeIpAddr, jarPaths, scala_home,
      java_home, classpath,
      clusterId, power, roles, description)
  }

    /**
     * RemoveNode
      *
      * @param nodeId a cluster node
     * @return
     */
  def RemoveNode(nodeId: String): String = {
    try {
      MdMgr.GetMdMgr.RemoveNode(nodeId)
      val key = "NodeInfo." + nodeId
      MetadataAPIImpl.DeleteObject(key.toLowerCase, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveNode", null, ErrorCodeConstants.Remove_Node_Successful + ":" + nodeId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveNode", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Node_Failed + ":" + nodeId)
        apiResult.toString()
      }
    }
  }

  def AddTenant(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): String = {
    try {
      // save in memory
      val ti = MdMgr.GetMdMgr.MakeTenantInfo(tenantId, description, primaryDataStore, cacheConfig)
      MdMgr.GetMdMgr.AddTenantInfo(ti)
      // save in database
      val key = "TenantInfo." + tenantId.trim.toLowerCase()
      val value = MetadataAPISerialization.serializeObjectToJson(ti).getBytes //serializer.SerializeObjectToByteArray(ti)
      MetadataAPIImpl.SaveObject(key.toLowerCase, value, "config_objects", serializerType)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddTenant", null, ErrorCodeConstants.Add_Tenant_Successful + ":" + tenantId)
      apiResult.toString()
    } catch {
      case e: Exception => {

        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddTenant", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Tenant_Failed + ":" + tenantId)
        apiResult.toString()
      }
    }
  }

  def UpdateTenant(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): String = {
    AddTenant(tenantId, description, primaryDataStore, cacheConfig)
  }

  /**
    * RemoveNode
    *
    * @param tenantId a cluster node
    * @return
    */
  def RemoveTenant(tenantId: String): String = {
    try {
      MdMgr.GetMdMgr.RemoveTenantInfo(tenantId)
      val key = "TenantInfo." + tenantId.trim.toLowerCase()
      MetadataAPIImpl.DeleteObject(key.toLowerCase, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveNode", null, ErrorCodeConstants.Remove_Tenant_Successful + ":" + tenantId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveNode", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Tenant_Failed + ":" + tenantId)
        apiResult.toString()
      }
    }
  }

  /**
     * AddAdapter
    *
    * @param name
     * @param typeString
     * @param className
     * @param jarName
     * @param dependencyJars
     * @param adapterSpecificCfg
     * @return
     */
  def AddAdapter(name: String, typeString: String, className: String,
                 jarName: String, dependencyJars: List[String],
                 adapterSpecificCfg: String, tenantId: String, fullAdapterConfig: String): String = {
    try {
      // save in memory
      val ai = MdMgr.GetMdMgr.MakeAdapter(name, typeString, className, jarName, dependencyJars, adapterSpecificCfg, tenantId, fullAdapterConfig)
      MdMgr.GetMdMgr.AddAdapter(ai)
      // save in database
      val key = "AdapterInfo." + name
      val value = MetadataAPISerialization.serializeObjectToJson(ai).getBytes//serializer.SerializeObjectToByteArray(ai)
      MetadataAPIImpl.SaveObject(key.toLowerCase, value, "config_objects", serializerType)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddAdapter", null, ErrorCodeConstants.Add_Adapter_Successful + ":" + name)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddAdapter", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Adapter_Failed + ":" + name)
        apiResult.toString()
      }
    }
  }

    /**
     * RemoveAdapter
      *
      * @param name
     * @param typeString
     * @param className
     * @param jarName
     * @param dependencyJars
     * @param adapterSpecificCfg
     * @return
     */
  def UpdateAdapter(name: String, typeString: String, className: String,
                    jarName: String, dependencyJars: List[String],
                    adapterSpecificCfg: String, tenantId: String, fullAdapterConfig: String): String = {
    AddAdapter(name, typeString, className, jarName, dependencyJars, adapterSpecificCfg, tenantId, fullAdapterConfig)
  }

    /**
     * RemoveAdapter
      *
      * @param name
     * @return
     */
  def RemoveAdapter(name: String): String = {
    try {
      MdMgr.GetMdMgr.RemoveAdapter(name)
      val key = "AdapterInfo." + name
      MetadataAPIImpl.DeleteObject(key.toLowerCase, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveAdapter", null, ErrorCodeConstants.Remove_Adapter_Successful + ":" + name)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveAdapter", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Adapter_Failed + ":" + name)
        apiResult.toString()
      }
    }
  }

    /**
     * AddCluster
      *
      * @param clusterId
     * @param description
     * @param privileges
     * @return
     */
  def AddCluster(clusterId: String, description: String, privileges: String): String = {
    try {
      // save in memory
      val ci = MdMgr.GetMdMgr.MakeCluster(clusterId, description, privileges)
      MdMgr.GetMdMgr.AddCluster(ci)
      // save in database
      val key = "ClusterInfo." + clusterId
      val value = MetadataAPISerialization.serializeObjectToJson(ci).getBytes//serializer.SerializeObjectToByteArray(ci)
      MetadataAPIImpl.SaveObject(key.toLowerCase, value, "config_objects", serializerType)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddCluster", null, ErrorCodeConstants.Add_Cluster_Successful + ":" + clusterId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddCluster", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Cluster_Failed + ":" + clusterId)
        apiResult.toString()
      }
    }
  }

    /**
     * UpdateCluster
      *
      * @param clusterId
     * @param description
     * @param privileges
     * @return
     */
  def UpdateCluster(clusterId: String, description: String, privileges: String): String = {
    AddCluster(clusterId, description, privileges)
  }

    /**
     * RemoveCluster
      *
      * @param clusterId
     * @return
     */
  def RemoveCluster(clusterId: String): String = {
    try {
      MdMgr.GetMdMgr.RemoveCluster(clusterId)
      val key = "ClusterInfo." + clusterId
      MetadataAPIImpl.DeleteObject(key.toLowerCase, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveCluster", null, ErrorCodeConstants.Remove_Cluster_Successful + ":" + clusterId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveCluster", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Cluster_Failed + ":" + clusterId)
        apiResult.toString()
      }
    }
  }

    /**
     * Add a cluster configuration from the supplied map with the supplied identifer key
      *
      * @param clusterCfgId cluster id to add
     * @param cfgMap the configuration map
     * @param modifiedTime when modified
     * @param createdTime when created
     * @return results string
     */
  def AddClusterCfg(clusterCfgId: String, cfgMap: scala.collection.mutable.HashMap[String, String],
    modifiedTime: Date, createdTime: Date): String = {
    try {
      // save in memory
      val ci = MdMgr.GetMdMgr.MakeClusterCfg(clusterCfgId, cfgMap, modifiedTime, createdTime)
      MdMgr.GetMdMgr.AddClusterCfg(ci)
      // save in database
      val key = "ClusterCfgInfo." + clusterCfgId
      val value = MetadataAPISerialization.serializeObjectToJson(ci).getBytes//serializer.SerializeObjectToByteArray(ci)
      MetadataAPIImpl.SaveObject(key.toLowerCase, value, "config_objects", serializerType)
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "AddClusterCfg", null, ErrorCodeConstants.Add_Cluster_Config_Successful + ":" + clusterCfgId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "AddClusterCfg", null, "Error :" + e.toString() + ErrorCodeConstants.Add_Cluster_Config_Failed + ":" + clusterCfgId)
        apiResult.toString()
      }
    }
  }

    /**
     * Update te configuration for the cluster with the supplied id
      *
      * @param clusterCfgId
     * @param cfgMap
     * @param modifiedTime
     * @param createdTime
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def UpdateClusterCfg(clusterCfgId: String, cfgMap: scala.collection.mutable.HashMap[String, String],
    modifiedTime: Date, createdTime: Date, userid: Option[String] = None): String = {
    AddClusterCfg(clusterCfgId, cfgMap, modifiedTime, createdTime)
  }

    /**
     * Remove a cluster configuration with the suppplied id
     *
     * @param clusterCfgId
     * @return results string
     */
  def RemoveClusterCfg(clusterCfgId: String, userid: Option[String] = None): String = {
    try {
      MdMgr.GetMdMgr.RemoveClusterCfg(clusterCfgId)
      val key = "ClusterCfgInfo." + clusterCfgId
      MetadataAPIImpl.DeleteObject(key.toLowerCase, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveCLusterCfg", null, ErrorCodeConstants.Remove_Cluster_Config_Successful + ":" + clusterCfgId)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveCLusterCfg", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Cluster_Config_Failed + ":" + clusterCfgId)
        apiResult.toString()
      }
    }
  }

    /**
     * Remove a cluster configuration
      *
      * @param cfgStr
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param cobjects
     * @return results string
     */
  def RemoveConfig(cfgStr: String, userid: Option[String], cobjects: String): String = {
    var keyList = new Array[String](0)
    MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.REMOVECONFIG, cfgStr, AuditConstants.SUCCESS, "", cobjects)
    try {
      // extract config objects
      val map = JsonSerializer.parseEngineConfig(cfgStr)
      // process clusterInfo object if it exists
      if (map.contains("Clusters")) {
        var globalAdaptersCollected = false // to support previous versions
        val clustersList = map.get("Clusters").get.asInstanceOf[List[_]] //BUGBUG:: Do we need to check the type before converting
        logger.debug("Found " + clustersList.length + " cluster objects ")
        clustersList.foreach(clustny => {
          val cluster = clustny.asInstanceOf[Map[String, Any]] //BUGBUG:: Do we need to check the type before converting
          val ClusterId = cluster.getOrElse("ClusterId", "").toString.trim.toLowerCase

          MdMgr.GetMdMgr.RemoveCluster(ClusterId)
          var key = "ClusterInfo." + ClusterId
          keyList = keyList :+ key.toLowerCase
          MdMgr.GetMdMgr.RemoveClusterCfg(ClusterId)
          key = "ClusterCfgInfo." + ClusterId
          keyList = keyList :+ key.toLowerCase

          if (cluster.contains("Nodes")) {
            val nodes = cluster.get("Nodes").get.asInstanceOf[List[_]]
            nodes.foreach(n => {
              val node = n.asInstanceOf[Map[String, Any]]
              val nodeId = node.getOrElse("NodeId", "").toString.trim.toLowerCase
              if (nodeId.size > 0) {
                MdMgr.GetMdMgr.RemoveNode(nodeId.toLowerCase)
                key = "NodeInfo." + nodeId
                keyList = keyList :+ key.toLowerCase
              }
            })
          }

          if (cluster.contains("Tenants")) {
            val tenants = cluster.get("Tenants").get.asInstanceOf[List[_]]
            tenants.foreach(t => {
              val tenant = t.asInstanceOf[Map[String, Any]]
              val tenantId = tenant.getOrElse("TenantId", "").toString.trim
              if (tenantId.trim.size > 0) {
                MdMgr.GetMdMgr.RemoveTenantInfo(tenantId)
                key = "TenantInfo." + tenantId.trim.toLowerCase()
                keyList = keyList :+ key.toLowerCase

                //BUGBUG:: Need to report to Engine and others
                // FIXME:: Need to report to Engine and others
              }
            })
          }

          if (cluster.contains("Adapters") || (globalAdaptersCollected == false && map.contains("Adapters"))) {
            val adapters = if (cluster.contains("Adapters") && (globalAdaptersCollected == false && map.contains("Adapters"))) {
              map.get("Adapters").get.asInstanceOf[List[_]] ++ cluster.get("Adapters").get.asInstanceOf[List[_]]
            } else if (cluster.contains("Adapters")) {
              cluster.get("Adapters").get.asInstanceOf[List[_]]
            } else if (globalAdaptersCollected == false && map.contains("Adapters")) {
              map.get("Adapters").get.asInstanceOf[List[_]]
            } else {
              List[Any]()
            }

            globalAdaptersCollected = true // to support previous versions

            adapters.foreach(a => {
              val adap = a.asInstanceOf[Map[String, Any]]
              val nm = adap.getOrElse("Name", "").toString.trim.toLowerCase
              if (nm.size > 0) {
                MdMgr.GetMdMgr.RemoveAdapter(nm)
                val key = "AdapterInfo." + nm
                keyList = keyList :+ key.toLowerCase
              }
            })
          }
        })
      }
      if (keyList.size > 0)
        MetadataAPIImpl.RemoveObjectList(keyList, "config_objects")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "RemoveConfig", null, ErrorCodeConstants.Remove_Config_Successful + ":" + cfgStr)
      apiResult.toString()
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "RemoveConfig", null, "Error :" + e.toString() + ErrorCodeConstants.Remove_Config_Failed + ":" + cfgStr)
        apiResult.toString()
      }
    }
  }

    /**
     * Upload a model config.  These are for native models written in Scala or Java
      *
      * @param cfgStr
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param objectList
     * @param isFromNotify
     * @return
     */
  def UploadModelsConfig(cfgStr: String, userid: Option[String], objectList: String, isFromNotify: Boolean = false): String = {
    try{
      var keyList = new Array[String](0)
      var valueList = new Array[Array[Byte]](0)
      val tranId = MetadataAPIImpl.GetNewTranId
      logger.debug("Parsing ModelConfig : " + cfgStr)
      cfgmap = parse(cfgStr).values.asInstanceOf[Map[String, Any]]
      logger.debug("Count of objects in cfgmap : " + cfgmap.keys.size)
      
      var i = 0
      // var objectsAdded: scala.collection.mutable.MutableList[Map[String, List[String]]] = scala.collection.mutable.MutableList[Map[String, List[String]]]()
      var baseElems: Array[BaseElemDef] = new Array[BaseElemDef](cfgmap.keys.size)
      cfgmap.keys.foreach(key => {
	logger.debug("Model Config Key => " + key)
	var mdl = cfgmap(key).asInstanceOf[Map[String, List[String]]]

	// wrap the config objet in Element Def
	var confElem: ConfigDef = new ConfigDef
	confElem.tranId = tranId
	confElem.nameSpace = if(userid != None) userid.get else null
	confElem.contents = JsonSerializer.SerializeMapToJsonString(mdl)
	confElem.name = key
	baseElems(i) = confElem
	i = i + 1

	// Prepare KEY/VALUE for persistent insertion
	var modelKey = userid.getOrElse("_") + "." + key
	var value = confElem.contents.getBytes //serializer.SerializeObjectToByteArray(mdl)
	keyList = keyList :+ modelKey.toLowerCase
	valueList = valueList :+ value
	// Save in memory
	MetadataAPIImpl.AddConfigObjToCache(tranId, modelKey, mdl, MdMgr.GetMdMgr)
      })
      // Save in Database
      MetadataAPIImpl.SaveObjectList(keyList, valueList, "model_config_objects", serializerType)
      if (!isFromNotify) {
	val operations = for (op <- baseElems) yield "Add"
	MetadataAPIImpl.NotifyEngine(baseElems, operations)
      }

      // return reuslts
      val apiResult = new ApiResult(ErrorCodeConstants.Success, "UploadModelsConfig", null, "Upload of model config successful")
      apiResult.toString()
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadModelsConfig", null, "Error :" + e.toString() + ErrorCodeConstants.Upload_Config_Failed + ":" + cfgStr)
        apiResult.toString()
      }
    }
  }

    /**
     * getStringFromJsonNode
      *
      * @param v just any old thing
     * @return a string representation
     */
  private def getStringFromJsonNode(v: Any): String = {
    if (v == null) return ""

    if (v.isInstanceOf[String]) return v.asInstanceOf[String]

    implicit val jsonFormats: Formats = DefaultFormats
    val lst = List(v)
    val str = Serialization.write(lst)
    if (str.size > 2) {
      return str.substring(1, str.size - 1)
    }
    return ""
  }

  /*
  private def getJsonNodeFromString(s: String): Any = {
    if (s.size == 0) return s

    val s1 = "[" + s + "]"
    
    implicit val jsonFormats: Formats = DefaultFormats
    val list = Serialization.read[List[_]](s1)

    return list(0)
  }
*/

    /**
     * Accept a config specification (a JSON str)
      *
      * @param cfgStr the json file to be interpted
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param objectList note on the objects in the configuration to be logged to audit adapter
     * @return
     */
  def UploadConfig(cfgStr: String, userid: Option[String], objectList: String): String = {
    var keyList = new Array[String](0)
    var valueList = new Array[Array[Byte]](0)

    //TODO: BOOOYA
    var clusterNotifications: ArrayBuffer[BaseElemDef] = new ArrayBuffer[BaseElemDef]
    var clusterNotifyActions: ArrayBuffer[String] =  new ArrayBuffer[String]

    MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTCONFIG, cfgStr, AuditConstants.SUCCESS, "", objectList)

    try {
      // extract config objects
      val map = JsonSerializer.parseEngineConfig(cfgStr)
      // process clusterInfo object if it exists
      if (map.contains("Clusters") == false) {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadConfig", null, ErrorCodeConstants.Upload_Config_Failed + ":" + cfgStr)
        apiResult.toString()
      } else {
        if (map.contains("Clusters")) {
          var globalAdaptersCollected = false // to support previous versions
          val clustersList = map.get("Clusters").get.asInstanceOf[List[_]] //BUGBUG:: Do we need to check the type before converting
          logger.debug("Found " + clustersList.length + " cluster objects ")
          clustersList.foreach(clustny => {
            val cluster = clustny.asInstanceOf[Map[String, Any]] //BUGBUG:: Do we need to check the type before converting
            val ClusterId = cluster.getOrElse("ClusterId", "").toString.trim.toLowerCase
            logger.debug("Processing the cluster => " + ClusterId)
            // save in memory
            val ci = MdMgr.GetMdMgr.MakeCluster(ClusterId, "", "")
            val addCluserReuslt = MdMgr.GetMdMgr.AddCluster(ci)

            if (addCluserReuslt != None) {
              var clusterDef: ClusterConfigDef = new ClusterConfigDef
              clusterDef.clusterId = ci.clusterId
              clusterDef.elementType = "clusterDef"
              clusterDef.nameSpace = "cluster"
              clusterDef.name = ci.clusterId
              clusterDef.tranId = MetadataAPIImpl.GetNewTranId
              clusterNotifications.append(clusterDef)
              if (addCluserReuslt.get.equalsIgnoreCase("add"))
                clusterNotifyActions.append("Add")
              else
                clusterNotifyActions.append("Update")
            }

            var key = "ClusterInfo." + ci.clusterId
            var value = MetadataAPISerialization.serializeObjectToJson(ci).getBytes //serializer.SerializeObjectToByteArray(ci)
            keyList = keyList :+ key.toLowerCase
            valueList = valueList :+ value

            // gather config name-value pairs
            val cfgMap = new scala.collection.mutable.HashMap[String, String]
            if (cluster.contains("SystemCatalog"))
              cfgMap("SystemCatalog") = getStringFromJsonNode(cluster.getOrElse("SystemCatalog", null))
            if (cluster.contains("ZooKeeperInfo"))
              cfgMap("ZooKeeperInfo") = getStringFromJsonNode(cluster.getOrElse("ZooKeeperInfo", null))
            if (cluster.contains("EnvironmentContext"))
              cfgMap("EnvironmentContext") = getStringFromJsonNode(cluster.getOrElse("EnvironmentContext", null))
            if (cluster.contains("Cache"))
              cfgMap("Cache") = getStringFromJsonNode(cluster.getOrElse("Cache", null))
            if (cluster.contains("Config")) {
              val config = cluster.get("Config").get.asInstanceOf[Map[String, Any]] //BUGBUG:: Do we need to check the type before converting
              if (config.contains("SystemCatalog"))
                cfgMap("SystemCatalog") = getStringFromJsonNode(config.get("SystemCatalog"))
              if (config.contains("ZooKeeperInfo"))
                cfgMap("ZooKeeperInfo") = getStringFromJsonNode(config.get("ZooKeeperInfo"))
              if (config.contains("EnvironmentContext"))
                cfgMap("EnvironmentContext") = getStringFromJsonNode(config.get("EnvironmentContext"))
            }

            // save in memory
            val cic = MdMgr.GetMdMgr.MakeClusterCfg(ClusterId, cfgMap, null, null)
            val addClusterResult = MdMgr.GetMdMgr.AddClusterCfg(cic)

            if (addClusterResult != None) {
              var clusterInfoDef: ClusterConfigDef = new ClusterConfigDef
              clusterInfoDef.clusterId = cic.clusterId
              clusterInfoDef.elementType = "clusterInfoDef"
              clusterInfoDef.name = cic.clusterId
              clusterInfoDef.nameSpace = "clusterInfo"
              clusterInfoDef.tranId = MetadataAPIImpl.GetNewTranId
              clusterNotifications.append(clusterInfoDef)
              println("cic Change for " + clusterInfoDef.name )
              if (addClusterResult.get.equalsIgnoreCase("add"))
                clusterNotifyActions.append("Add")
              else
                clusterNotifyActions.append("Update")
            }

            key = "ClusterCfgInfo." + cic.clusterId
            value = MetadataAPISerialization.serializeObjectToJson(cic).getBytes//serializer.SerializeObjectToByteArray(cic)
            keyList = keyList :+ key.toLowerCase
            valueList = valueList :+ value

            if (cluster.contains("Nodes")) {
              val nodes = cluster.get("Nodes").get.asInstanceOf[List[_]]
              nodes.foreach(n => {
                val node = n.asInstanceOf[Map[String, Any]]
                val nodeId = node.getOrElse("NodeId", "").toString.trim.toLowerCase
                val nodePort = node.getOrElse("NodePort", "0").toString.trim.toInt
                val nodeIpAddr = node.getOrElse("NodeIpAddr", "").toString.trim
                val scala_home = node.getOrElse("Scala_home", "").toString.trim
                val java_home = node.getOrElse("Java_home", "").toString.trim
                val classpath = node.getOrElse("Classpath", "").toString.trim
                val jarPaths = if (node.contains("JarPaths")) node.get("JarPaths").get.asInstanceOf[List[String]] else List[String]()
                val roles = if (node.contains("Roles")) node.get("Roles").get.asInstanceOf[List[String]] else List[String]()

                val validRoles = NodeRole.ValidRoles.map(r => r.toLowerCase).toSet
                val givenRoles = roles
                var foundRoles = ArrayBuffer[String]()
                var notfoundRoles = ArrayBuffer[String]()
                if (givenRoles != null) {
                  val gvnRoles = givenRoles.foreach(r => {
                    if (validRoles.contains(r.toLowerCase))
                      foundRoles += r
                    else
                      notfoundRoles += r
                  })
                  if (notfoundRoles.size > 0) {
                    logger.error("Found invalid node roles:%s for nodeid: %d".format(notfoundRoles.mkString(","), nodeId))
                  }
                }

                val ni = MdMgr.GetMdMgr.MakeNode(nodeId, nodePort, nodeIpAddr, jarPaths,
                                                 scala_home, java_home, classpath, ClusterId, 0, foundRoles.toArray, "")

                val addNodeResult = MdMgr.GetMdMgr.AddNode(ni)
                if (addNodeResult != None) {
                  var nodeDef: ClusterConfigDef = new ClusterConfigDef
                  nodeDef.name = ni.nodeId
                  nodeDef.tranId = MetadataAPIImpl.GetNewTranId
                  nodeDef.nameSpace = "nodeIds"
                  nodeDef.clusterId = ci.clusterId
                  nodeDef.elementType = "nodeDef"
                  clusterNotifications.append(nodeDef)
                  println("node Change for " + nodeDef.name )
                  if (addNodeResult.get.equalsIgnoreCase("add"))
                    clusterNotifyActions.append("Add")
                  else
                    clusterNotifyActions.append("Update")
                }


                val key = "NodeInfo." + ni.nodeId
                val value = MetadataAPISerialization.serializeObjectToJson(ni).getBytes//serializer.SerializeObjectToByteArray(ni)
                keyList = keyList :+ key.toLowerCase
                valueList = valueList :+ value
              })
            }

            if (cluster.contains("Tenants")) {
              val tenants = cluster.get("Tenants").get.asInstanceOf[List[_]]
              tenants.foreach(t => {
                val tenant = t.asInstanceOf[Map[String, Any]]
                val tenantId = tenant.getOrElse("TenantId", "").toString.trim
                val description = tenant.getOrElse("Description", "").toString.trim
                var primaryDataStore = getStringFromJsonNode(tenant.getOrElse("PrimaryDataStore", null))
                var cacheConfig = getStringFromJsonNode(tenant.getOrElse("CacheConfig", null))

                val ti = MdMgr.GetMdMgr.MakeTenantInfo(tenantId, description, primaryDataStore, cacheConfig)
                val addTenantResult = MdMgr.GetMdMgr.AddTenantInfo(ti)
                if (addTenantResult != None) {
                  val tenantDef: ClusterConfigDef = new ClusterConfigDef
                  tenantDef.name = ti.tenantId.trim.toLowerCase()
                  tenantDef.tranId = MetadataAPIImpl.GetNewTranId
                  tenantDef.nameSpace = "Tenants"
                  tenantDef.clusterId = ci.clusterId
                  tenantDef.elementType = "TenantDef"
                  clusterNotifications.append(tenantDef)
                  println("tenant Change for " + tenantDef.name )
                  if (addTenantResult.get.equalsIgnoreCase("add"))
                    clusterNotifyActions.append("Add")
                  else
                    clusterNotifyActions.append("Update")
                }

                val key = "TenantInfo." + ti.tenantId.trim.toLowerCase()
                val value = MetadataAPISerialization.serializeObjectToJson(ti).getBytes//serializer.SerializeObjectToByteArray(ti)
                keyList = keyList :+ key.toLowerCase
                valueList = valueList :+ value

              })
            }

            if (cluster.contains("Adapters") || (globalAdaptersCollected == false && map.contains("Adapters"))) {
              val adapters = if (cluster.contains("Adapters") && (globalAdaptersCollected == false && map.contains("Adapters"))) {
                map.get("Adapters").get.asInstanceOf[List[_]] ++ cluster.get("Adapters").get.asInstanceOf[List[_]]
              } else if (cluster.contains("Adapters")) {
                cluster.get("Adapters").get.asInstanceOf[List[_]]
              } else if (globalAdaptersCollected == false && map.contains("Adapters")) {
                map.get("Adapters").get.asInstanceOf[List[_]]
              } else {
                List[Any]()
              }

              globalAdaptersCollected = true // to support previous versions

              adapters.foreach(a => {
                val adap = a.asInstanceOf[Map[String, Any]]
                val nm = adap.getOrElse("Name", "").toString.trim
                val typStr = adap.getOrElse("TypeString", "").toString.trim
                val tenantId = adap.getOrElse("TenantId", "").toString.trim
                val clsNm = adap.getOrElse("ClassName", "").toString.trim
                val jarnm = adap.getOrElse("JarName", "").toString.trim

                var depJars: List[String] = null
                if (adap.contains("DependencyJars")) {
                  depJars = adap.get("DependencyJars").get.asInstanceOf[List[String]]
                }
                var ascfg: String = ""
                if (adap.contains("AdapterSpecificCfg")) {
                  ascfg = getStringFromJsonNode(adap.get("AdapterSpecificCfg"))
                }
                val fullAdapterConfig = getStringFromJsonNode(adap) // Saving the full config here in case if we want to use it later. In case of storage we use it

                // save in memory
                val ai = MdMgr.GetMdMgr.MakeAdapter(nm, typStr, clsNm, jarnm, depJars, ascfg, tenantId, fullAdapterConfig)
                val addAdapterResult = MdMgr.GetMdMgr.AddAdapter(ai)
                if (addAdapterResult != None) {
                  var adapterDef: ClusterConfigDef = new ClusterConfigDef
                  adapterDef.name = nm
                  adapterDef.nameSpace = typStr
                  adapterDef.tranId = MetadataAPIImpl.GetNewTranId
                  adapterDef.clusterId = ClusterId
                  adapterDef.elementType = "adapterDef"
                  clusterNotifications.append(adapterDef)
                  println("adapter Change for " + adapterDef.name )
                  if (addAdapterResult.get.equalsIgnoreCase("add"))
                    clusterNotifyActions.append("Add")
                  else
                    clusterNotifyActions.append("Update")
                }
                val key = "AdapterInfo." + ai.name
                val value = MetadataAPISerialization.serializeObjectToJson(ai).getBytes//serializer.SerializeObjectToByteArray(ai)
                keyList = keyList :+ key.toLowerCase
                valueList = valueList :+ value
              })
            } else {
              logger.debug("Found no adapater objects in the config file")
            }

            // Now see if there are any other User Defined Properties in this cluster, if there are any, create a container
            // like we did for adapters and noteds, etc....
            var userDefinedProps: Map[String, Any] = cluster.filter(x => { !excludeList.contains(x._1) })
            if (userDefinedProps.size > 0) {
              val upProps: UserPropertiesInfo = MdMgr.GetMdMgr.MakeUPProps(ClusterId)
              userDefinedProps.keys.foreach(key => {
                upProps.Props(key) = userDefinedProps(key).toString
              })

              val upAddResults = MdMgr.GetMdMgr.AddUserProperty(upProps)
              if (upAddResults != None) {
                var upDef: ClusterConfigDef = new ClusterConfigDef
                upDef.name = upProps.clusterId
                upDef.nameSpace = "userProperties"
                upDef.tranId = MetadataAPIImpl.GetNewTranId
                upDef.clusterId = ClusterId
                upDef.elementType = "upDef"
                clusterNotifications.append(upDef)

                println("UP Change for " + upDef.clusterId )
                if (upAddResults.get.equalsIgnoreCase("add"))
                  clusterNotifyActions.append("Add")
                else
                  clusterNotifyActions.append("Update")
              }

              val upKey = "userProperties." + upProps.clusterId
              val upValue = MetadataAPISerialization.serializeObjectToJson(upProps).getBytes//serializer.SerializeObjectToByteArray(upProps)
              keyList = keyList :+ upKey.toLowerCase
              valueList = valueList :+ upValue
            }
          })

        } else {
          logger.debug("Found no adapater objects in the config file")
        }


        MetadataAPIImpl.SaveObjectList(keyList, valueList, "config_objects", serializerType)
        MetadataAPIImpl.NotifyEngine(clusterNotifications.toArray, clusterNotifyActions.toArray)
        var apiResult = new ApiResult(ErrorCodeConstants.Success, "UploadConfig", cfgStr, ErrorCodeConstants.Upload_Config_Successful)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadConfig", cfgStr, "Error :" + e.toString() + ErrorCodeConstants.Upload_Config_Failed)
        apiResult.toString()
      }
    }
  }

    /**
     * Get a property value
      *
      * @param ci
     * @param key
     * @return
     */
  def getUP(ci: String, key: String): String = {
    MdMgr.GetMdMgr.GetUserProperty(ci, key)
  }

    /**
     * Answer nodes as an array.
      *
      * @return
     */
  def getNodeList1: Array[NodeInfo] = { MdMgr.GetMdMgr.Nodes.values.toArray }
  // All available nodes(format JSON) as a String
    /**
     * Get the nodes as json.
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @return
     */
  def GetAllNodes(formatType: String, userid: Option[String] = None): String = {
    try {
      val nodes = MdMgr.GetMdMgr.Nodes.values.toArray
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETCONFIG, AuditConstants.CONFIG, AuditConstants.SUCCESS, "", "nodes")
      if (nodes.length == 0) {
        logger.debug("No Nodes found ")
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllNodes", null, ErrorCodeConstants.Get_All_Nodes_Failed_Not_Available)
        apiResult.toString()
      } else {
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllNodes", JsonSerializer.SerializeCfgObjectListToJson("Nodes", nodes), ErrorCodeConstants.Get_All_Nodes_Successful)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllNodes", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Nodes_Failed)
        apiResult.toString()
      }
    }
  }

    /**
     * All available adapters(format JSON) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @return
     */
  def GetAllAdapters(formatType: String, userid: Option[String] = None): String = {
    try {
      val adapters = MdMgr.GetMdMgr.Adapters.values.toArray
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETCONFIG, AuditConstants.CONFIG, AuditConstants.FAIL, "", "adapters")
      if (adapters.length == 0) {
        logger.debug("No Adapters found ")
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllAdapters", null, ErrorCodeConstants.Get_All_Adapters_Failed_Not_Available)
        apiResult.toString()
      } else {
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllAdapters", JsonSerializer.SerializeCfgObjectListToJson("Adapters", adapters), ErrorCodeConstants.Get_All_Adapters_Successful)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllAdapters", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Adapters_Failed)

        apiResult.toString()
      }
    }
  }

    /**
     * All available clusters(format JSON) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @return
     */
  def GetAllClusters(formatType: String, userid: Option[String] = None): String = {
    try {
      val clusters = MdMgr.GetMdMgr.Clusters.values.toArray
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETCONFIG, AuditConstants.CONFIG, AuditConstants.SUCCESS, "", "Clusters")
      if (clusters.length == 0) {
        logger.debug("No Clusters found ")
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllClusters", null, ErrorCodeConstants.Get_All_Clusters_Failed_Not_Available)
        apiResult.toString()
      } else {
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllClusters", JsonSerializer.SerializeCfgObjectListToJson("Clusters", clusters), ErrorCodeConstants.Get_All_Clusters_Successful)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllClusters", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Clusters_Failed)
        apiResult.toString()
      }
    }
  }

  // All available clusterCfgs(format JSON) as a String
    /**
     *
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @return
     */
  def GetAllClusterCfgs(formatType: String, userid: Option[String] = None): String = {
    try {
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETCONFIG, AuditConstants.CONFIG, AuditConstants.SUCCESS, "", "ClusterCfg")
      val clusterCfgs = MdMgr.GetMdMgr.ClusterCfgs.values.toArray
      if (clusterCfgs.length == 0) {
        logger.debug("No ClusterCfgs found ")
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllClusterCfgs", null, ErrorCodeConstants.Get_All_Cluster_Configs_Failed_Not_Available)
        apiResult.toString()
      } else {
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllClusterCfgs", JsonSerializer.SerializeCfgObjectListToJson("ClusterCfgs", clusterCfgs), ErrorCodeConstants.Get_All_Cluster_Configs_Successful)

        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllClusterCfgs", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Cluster_Configs_Failed)

        apiResult.toString()
      }
    }
  }

    /**
     * All available config objects(format JSON) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @return
     */
  def GetAllCfgObjects(formatType: String, userid: Option[String] = None): String = {
    var cfgObjList = new Array[Object](0)
    MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETCONFIG, AuditConstants.CONFIG, AuditConstants.SUCCESS, "", "all")
    var jsonStr: String = ""
    var jsonStr1: String = ""
    try {
      val clusters = MdMgr.GetMdMgr.Clusters.values.toArray
      if (clusters.length != 0) {
        cfgObjList = cfgObjList :+ clusters
        jsonStr1 = JsonSerializer.SerializeCfgObjectListToJson("Clusters", clusters)
        jsonStr1 = jsonStr1.substring(1)
        jsonStr1 = JsonSerializer.replaceLast(jsonStr1, "}", ",")
        jsonStr = jsonStr + jsonStr1
      }
      val clusterCfgs = MdMgr.GetMdMgr.ClusterCfgs.values.toArray
      if (clusterCfgs.length != 0) {
        cfgObjList = cfgObjList :+ clusterCfgs
        jsonStr1 = JsonSerializer.SerializeCfgObjectListToJson("ClusterCfgs", clusterCfgs)
        jsonStr1 = jsonStr1.substring(1)
        jsonStr1 = JsonSerializer.replaceLast(jsonStr1, "}", ",")
        jsonStr = jsonStr + jsonStr1
      }
      val nodes = MdMgr.GetMdMgr.Nodes.values.toArray
      if (nodes.length != 0) {
        cfgObjList = cfgObjList :+ nodes
        jsonStr1 = JsonSerializer.SerializeCfgObjectListToJson("Nodes", nodes)
        jsonStr1 = jsonStr1.substring(1)
        jsonStr1 = JsonSerializer.replaceLast(jsonStr1, "}", ",")
        jsonStr = jsonStr + jsonStr1
      }
      val adapters = MdMgr.GetMdMgr.Adapters.values.toArray
      if (adapters.length != 0) {
        cfgObjList = cfgObjList :+ adapters
        jsonStr1 = JsonSerializer.SerializeCfgObjectListToJson("Adapters", adapters)
        jsonStr1 = jsonStr1.substring(1)
        jsonStr1 = JsonSerializer.replaceLast(jsonStr1, "}", ",")
        jsonStr = jsonStr + jsonStr1
      }

      jsonStr = "{" + JsonSerializer.replaceLast(jsonStr, ",", "") + "}"

      if (cfgObjList.length == 0) {
        logger.debug("No Config Objects found ")
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllCfgObjects", null, ErrorCodeConstants.Get_All_Configs_Failed_Not_Available)
        apiResult.toString()
      } else {
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "GetAllCfgObjects", jsonStr, ErrorCodeConstants.Get_All_Configs_Successful)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetAllCfgObjects", null, "Error :" + e.toString() + ErrorCodeConstants.Get_All_Configs_Failed)
        apiResult.toString()
      }
    }
  }

    /**
     * Dump the configuration file to the log
     */
  def dumpMetadataAPIConfig {
    val e = MetadataAPIImpl.GetMetadataAPIConfig.propertyNames()
    while (e.hasMoreElements()) {
      val key = e.nextElement().asInstanceOf[String]
      val value = MetadataAPIImpl.GetMetadataAPIConfig.getProperty(key)
      logger.debug("Key : " + key + ", Value : " + value)
    }
  }

    /**
     * setPropertyFromConfigFile - convert a specific KEY:VALUE pair in the config file into the
     * KEY:VALUE pair in the  Properties object
      *
      * @param key a property key
     * @param value a value
     */
  private def setPropertyFromConfigFile(key: String, value: String) {
    var finalKey = key
    var finalValue = value

    // JAR_PATHs need to be trimmed 
    if (key.equalsIgnoreCase("JarPaths") || key.equalsIgnoreCase("JAR_PATHS")) {
      val jp = value
      val j_paths = jp.split(",").map(s => s.trim).filter(s => s.size > 0)
      finalValue = j_paths.mkString(",")
      finalKey = "JAR_PATHS"
    }

    // Special case 1. for config.  if JAR_PATHS is never set, then it should default to JAR_TARGET_DIR..
    // so we set the JAR_PATH if it was never set.. no worries, if JAR_PATH comes later, it willsimply
    // overwrite the value.
    if (key.equalsIgnoreCase("JAR_TARGET_DIR") && (MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS") == null)) {
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_PATHS", finalValue)
      logger.debug("JAR_PATHS = " + finalValue)
      pList = pList - "JAR_PATHS"
    }

    // Special case 2.. MetadataLocation must set 2 properties in the config object.. 1. prop set by DATABASE_HOST,
    // 2. prop set by DATABASE_LOCATION.  MetadataLocation will overwrite those values, but not the other way around.
    if (key.equalsIgnoreCase("MetadataLocation")) {
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE_LOCATION", finalValue)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE_HOST", finalValue)
      logger.debug("DATABASE_LOCATION  = " + finalValue)
      pList = pList - "DATABASE_LOCATION"
      logger.debug("DATABASE_HOST  = " + finalValue)
      pList = pList - "DATABASE_HOST"
      return
    }

    // SSL_PASSWORD will not be saved in the Config object, since that object is printed out for debugging purposes.
    if (key.equalsIgnoreCase("SSL_PASSWD")) {
      MetadataAPIImpl.setSSLCertificatePasswd(value)
      return
    }

    // Special case 2a.. DATABASE_HOST should not override METADATA_LOCATION
    if (key.equalsIgnoreCase("DATABASE_HOST") && (MetadataAPIImpl.GetMetadataAPIConfig.getProperty(key.toUpperCase) != null)) {
      return
    }
    // Special case 2b.. DATABASE_LOCATION should not override METADATA_LOCATION
    if (key.equalsIgnoreCase("DATABASE_LOCATION") && (MetadataAPIImpl.GetMetadataAPIConfig.getProperty(key.toUpperCase) != null)) {
      return
    }

    // Special case 3: SCHEMA_NAME can come it under several keys, but we store it as DATABASE SCHEMA
    if (key.equalsIgnoreCase("MetadataSchemaName")) {
      finalKey = "DATABASE_SCHEMA"
    }

    if (key.equalsIgnoreCase("MetadataAdapterSpecificConfig")) {
      finalKey = "ADAPTER_SPECIFIC_CONFIG"
    }

    // Special case 4: DATABASE can come under DATABASE or MetaDataStoreType
    if (key.equalsIgnoreCase("DATABASE") || key.equalsIgnoreCase("MetadataStoreType")) {
      finalKey = "DATABASE"
    }

    // Special case 5: NodeId or Node_ID is possible
    if (key.equalsIgnoreCase("NODE_ID") || key.equalsIgnoreCase("NODEID")) {
      finalKey = "NODE_ID"
    }

    if (key.equalsIgnoreCase("MetadataDataStore")) {
      finalKey = "METADATA_DATASTORE"
    }

    // Store the Key/Value pair
    MetadataAPIImpl.GetMetadataAPIConfig.setProperty(finalKey.toUpperCase, finalValue)
    logger.debug(finalKey.toUpperCase + " = " + finalValue)
    pList = pList - finalKey.toUpperCase
  }

    /**
     * Refresh the ClusterConfiguration for the specified node
      *
      * @param nodeId a cluster node
     * @return
     */
  def RefreshApiConfigForGivenNode(nodeId: String): Boolean = {

    val nd = mdMgr.Nodes.getOrElse(nodeId, null)
    if (nd == null) {
      logger.error("Node %s not found in metadata".format(nodeId))
      return false
    }

    val clusterId = nd.ClusterId

    val cluster = mdMgr.ClusterCfgs.getOrElse(nd.ClusterId, null)
    if (cluster == null) {
      logger.error("Cluster not found for Node %s  & ClusterId : %s".format(nodeId, nd.ClusterId))
      return false
    }

    logger.debug("Configurations for the clusterId:" + clusterId)
    cluster.cfgMap.foreach(kv => {
      logger.debug("Key: %s, Value: %s".format(kv._1, kv._2))
    })

    val zooKeeperInfo = cluster.cfgMap.getOrElse("ZooKeeperInfo", null)
    if (zooKeeperInfo == null) {
      logger.error("ZooKeeperInfo not found for Node %s  & ClusterId : %s".format(nodeId, nd.ClusterId))
      return false
    }
    val jarPaths = if (nd.JarPaths == null) Set[String]() else nd.JarPaths.map(str => str.replace("\"", "").trim).filter(str => str.size > 0).toSet
    if (jarPaths.size == 0) {
      logger.error("Not found valid JarPaths.")
      return false
    } else {
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_PATHS", jarPaths.mkString(","))
      logger.debug("JarPaths Based on node(%s) => %s".format(nodeId, jarPaths.mkString(",")))
      val jarDir = compact(render(jarPaths.head)).replace("\"", "").trim

      // If JAR_TARGET_DIR is unset.. set it ot the first value of the the JAR_PATH.. whatever it is... ????? I think we should error on start up.. this seems like wrong
      // user behaviour not to set a variable vital to MODEL compilation.
      if (MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_TARGET_DIR") == null || (MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_TARGET_DIR") != null && MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_TARGET_DIR").length == 0))
        MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_TARGET_DIR", jarDir)
      logger.debug("Jar_target_dir Based on node(%s) => %s".format(nodeId, jarDir))
    }

    implicit val jsonFormats: Formats = DefaultFormats
    val zKInfo = parse(zooKeeperInfo).extract[JZKInfo]

    val zkConnectString = zKInfo.ZooKeeperConnectString.replace("\"", "").trim
    MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZOOKEEPER_CONNECT_STRING", zkConnectString)
    logger.debug("ZOOKEEPER_CONNECT_STRING(based on nodeId) => " + zkConnectString)

    val zkNodeBasePath = zKInfo.ZooKeeperNodeBasePath.replace("\"", "").trim
    MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZNODE_PATH", zkNodeBasePath)
    logger.debug("ZNODE_PATH(based on nodeid) => " + zkNodeBasePath)

    val zkSessionTimeoutMs1 = if (zKInfo.ZooKeeperSessionTimeoutMs == None || zKInfo.ZooKeeperSessionTimeoutMs == null) 0 else zKInfo.ZooKeeperSessionTimeoutMs.get.toString.toInt
    // Taking minimum values in case if needed
    val zkSessionTimeoutMs = if (zkSessionTimeoutMs1 <= 0) 1000 else zkSessionTimeoutMs1
    MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZK_SESSION_TIMEOUT_MS", zkSessionTimeoutMs.toString)
    logger.debug("ZK_SESSION_TIMEOUT_MS(based on nodeId) => " + zkSessionTimeoutMs)

    val zkConnectionTimeoutMs1 = if (zKInfo.ZooKeeperConnectionTimeoutMs == None || zKInfo.ZooKeeperConnectionTimeoutMs == null) 0 else zKInfo.ZooKeeperConnectionTimeoutMs.get.toString.toInt
    // Taking minimum values in case if needed
    val zkConnectionTimeoutMs = if (zkConnectionTimeoutMs1 <= 0) 30000 else zkConnectionTimeoutMs1
    MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZK_CONNECTION_TIMEOUT_MS", zkConnectionTimeoutMs.toString)
    logger.debug("ZK_CONNECTION_TIMEOUT_MS(based on nodeId) => " + zkConnectionTimeoutMs)
    true
  }

    /**
     * Read metadata api configuration properties
      *
      * @param configFile the MetadataAPI configuration file
     */
  @throws(classOf[MissingPropertyException])
  @throws(classOf[InvalidPropertyException])
  def readMetadataAPIConfigFromPropertiesFile(configFile: String): Unit = {
    try {
      if (MetadataAPIImpl.propertiesAlreadyLoaded) {
        logger.debug("Configuratin properties already loaded, skipping the load configuration step")
        return ;
      }

      val (prop, failStr) = com.ligadata.Utils.Utils.loadConfiguration(configFile.toString, true)
      if (failStr != null && failStr.size > 0) {
        logger.error(failStr)
        return
      }
      if (prop == null) {
        logger.error("Failed to load configuration")
        return
      }

      // some zookeper vals can be safely defaulted to.
      setPropertyFromConfigFile("NODE_ID", "Undefined")
      setPropertyFromConfigFile("API_LEADER_SELECTION_ZK_NODE", "/ligadata")
      setPropertyFromConfigFile("ZK_SESSION_TIMEOUT_MS", "3000")
      setPropertyFromConfigFile("ZK_CONNECTION_TIMEOUT_MS", "3000")

      // Loop through and set the rest of the values.
      val eProps1 = prop.propertyNames()
      while (eProps1.hasMoreElements()) {
        val key = eProps1.nextElement().asInstanceOf[String]
        val value = prop.getProperty(key)
        setPropertyFromConfigFile(key, value)
      }
      val mdDataStore = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("METADATA_DATASTORE")

      if (mdDataStore == null) {
        // Prepare from
        val dbType = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE")
        val dbHost = if (MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE_HOST") != null) MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE_HOST") else MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE_LOCATION")
        val dbSchema = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE_SCHEMA")
        val dbAdapterSpecific = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("ADAPTER_SPECIFIC_CONFIG")

        val dbType1 = if (dbType == null) "" else dbType.trim
        val dbHost1 = if (dbHost == null) "" else dbHost.trim
        val dbSchema1 = if (dbSchema == null) "" else dbSchema.trim

        if (dbAdapterSpecific != null) {
          val json = ("StoreType" -> dbType1) ~
            ("SchemaName" -> dbSchema1) ~
            ("Location" -> dbHost1) ~
            ("AdapterSpecificConfig" -> dbAdapterSpecific)
          val jsonStr = pretty(render(json))
          setPropertyFromConfigFile("METADATA_DATASTORE", jsonStr)
        } else {
          val json = ("StoreType" -> dbType1) ~
            ("SchemaName" -> dbSchema1) ~
            ("Location" -> dbHost1)
          val jsonStr = pretty(render(json))
          setPropertyFromConfigFile("METADATA_DATASTORE", jsonStr)
        }
      }

      pList.map(v => logger.warn(v + " remains unset"))
      MetadataAPIImpl.propertiesAlreadyLoaded = true;

    } catch {
      case e: Exception =>
        logger.error("Failed to load configuration", e)
        sys.exit(1)
    }
  }

    /**
     * Read the default configuration property values from config file.
      *
      * @param cfgFile
     */
  @throws(classOf[MissingPropertyException])
  @throws(classOf[LoadAPIConfigException])
  def readMetadataAPIConfigFromJsonFile(cfgFile: String): Unit = {
    try {
      if (MetadataAPIImpl.propertiesAlreadyLoaded) {
        return ;
      }
      var configFile = "MetadataAPIConfig.json"
      if (cfgFile != null) {
        configFile = cfgFile
      }

      val configJson = Source.fromFile(configFile).mkString
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(configJson)

      logger.debug("Parsed the json : " + configJson)
      val configMap = json.extract[MetadataAPIConfig]

      var rootDir = configMap.APIConfigParameters.RootDir
      if (rootDir == null) {
        rootDir = System.getenv("HOME")
      }
      logger.debug("RootDir => " + rootDir)

      var gitRootDir = configMap.APIConfigParameters.GitRootDir
      if (gitRootDir == null) {
        gitRootDir = rootDir + "git_hub"
      }
      logger.debug("GitRootDir => " + gitRootDir)

      var database = configMap.APIConfigParameters.MetadataStoreType
      if (database == null) {
        database = "hashmap"
      }
      logger.debug("Database => " + database)

      var databaseLocation = "/tmp"
      var databaseHost = configMap.APIConfigParameters.MetadataLocation
      if (databaseHost == null) {
        databaseHost = "localhost"
      } else {
        databaseLocation = databaseHost
      }
      logger.debug("DatabaseHost => " + databaseHost + ", DatabaseLocation(applicable to treemap or hashmap databases only) => " + databaseLocation)

      var databaseAdapterSpecificConfig = ""
      var metadataDataStore = ""
      /*
      var tmpMdAdapSpecCfg = configMap.APIConfigParameters.MetadataAdapterSpecificConfig
      if (tmpMdAdapSpecCfg != null && tmpMdAdapSpecCfg != None) {
        databaseAdapterSpecificConfig = tmpMdAdapSpecCfg
      }
*/
      logger.debug("DatabaseAdapterSpecificConfig => " + databaseAdapterSpecificConfig)

      var databaseSchema = "metadata"
      val databaseSchemaOpt = configMap.APIConfigParameters.MetadataSchemaName
      if (databaseSchemaOpt != None) {
        databaseSchema = databaseSchemaOpt.get
      }
      logger.debug("DatabaseSchema(applicable to cassandra only) => " + databaseSchema)

      var jarTargetDir = configMap.APIConfigParameters.JarTargetDir
      if (jarTargetDir == null) {
        throw MissingPropertyException("The property JarTargetDir must be defined in the config file " + configFile, null)
      }
      logger.debug("JarTargetDir => " + jarTargetDir)

      var jarPaths = jarTargetDir // configMap.APIConfigParameters.JarPaths
      if (jarPaths == null) {
        throw MissingPropertyException("The property JarPaths must be defined in the config file " + configFile, null)
      }
      logger.debug("JarPaths => " + jarPaths)

      var scalaHome = configMap.APIConfigParameters.ScalaHome
      if (scalaHome == null) {
        throw MissingPropertyException("The property ScalaHome must be defined in the config file " + configFile, null)
      }
      logger.debug("ScalaHome => " + scalaHome)

      var javaHome = configMap.APIConfigParameters.JavaHome
      if (javaHome == null) {
        throw MissingPropertyException("The property JavaHome must be defined in the config file " + configFile, null)
      }
      logger.debug("JavaHome => " + javaHome)

      var manifestPath = configMap.APIConfigParameters.ManifestPath
      if (manifestPath == null) {
        throw MissingPropertyException("The property ManifestPath must be defined in the config file " + configFile, null)
      }
      logger.debug("ManifestPath => " + manifestPath)

      var classPath = configMap.APIConfigParameters.ClassPath
      if (classPath == null) {
        throw MissingPropertyException("The property ClassPath must be defined in the config file " + configFile, null)
      }
      logger.debug("ClassPath => " + classPath)

      var notifyEngine = configMap.APIConfigParameters.NotifyEngine
      if (notifyEngine == null) {
        throw MissingPropertyException("The property NotifyEngine must be defined in the config file " + configFile, null)
      }
      logger.debug("NotifyEngine => " + notifyEngine)

      var znodePath = configMap.APIConfigParameters.ZnodePath
      if (znodePath == null) {
        throw MissingPropertyException("The property ZnodePath must be defined in the config file " + configFile, null)
      }
      logger.debug("ZNodePath => " + znodePath)

      var zooKeeperConnectString = configMap.APIConfigParameters.ZooKeeperConnectString
      if (zooKeeperConnectString == null) {
        throw MissingPropertyException("The property ZooKeeperConnectString must be defined in the config file " + configFile, null)
      }
      logger.debug("ZooKeeperConnectString => " + zooKeeperConnectString)

      var MODEL_FILES_DIR = ""
      val MODEL_FILES_DIR1 = configMap.APIConfigParameters.MODEL_FILES_DIR
      if (MODEL_FILES_DIR1 == None) {
        MODEL_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Models"
      } else
        MODEL_FILES_DIR = MODEL_FILES_DIR1.get
      logger.debug("MODEL_FILES_DIR => " + MODEL_FILES_DIR)

      var TYPE_FILES_DIR = ""
      val TYPE_FILES_DIR1 = configMap.APIConfigParameters.TYPE_FILES_DIR
      if (TYPE_FILES_DIR1 == None) {
        TYPE_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Types"
      } else
        TYPE_FILES_DIR = TYPE_FILES_DIR1.get
      logger.debug("TYPE_FILES_DIR => " + TYPE_FILES_DIR)

      var FUNCTION_FILES_DIR = ""
      val FUNCTION_FILES_DIR1 = configMap.APIConfigParameters.FUNCTION_FILES_DIR
      if (FUNCTION_FILES_DIR1 == None) {
        FUNCTION_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Functions"
      } else
        FUNCTION_FILES_DIR = FUNCTION_FILES_DIR1.get
      logger.debug("FUNCTION_FILES_DIR => " + FUNCTION_FILES_DIR)

      var CONCEPT_FILES_DIR = ""
      val CONCEPT_FILES_DIR1 = configMap.APIConfigParameters.CONCEPT_FILES_DIR
      if (CONCEPT_FILES_DIR1 == None) {
        CONCEPT_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Concepts"
      } else
        CONCEPT_FILES_DIR = CONCEPT_FILES_DIR1.get
      logger.debug("CONCEPT_FILES_DIR => " + CONCEPT_FILES_DIR)

      var MESSAGE_FILES_DIR = ""
      val MESSAGE_FILES_DIR1 = configMap.APIConfigParameters.MESSAGE_FILES_DIR
      if (MESSAGE_FILES_DIR1 == None) {
        MESSAGE_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Messages"
      } else
        MESSAGE_FILES_DIR = MESSAGE_FILES_DIR1.get
      logger.debug("MESSAGE_FILES_DIR => " + MESSAGE_FILES_DIR)

      var CONTAINER_FILES_DIR = ""
      val CONTAINER_FILES_DIR1 = configMap.APIConfigParameters.CONTAINER_FILES_DIR
      if (CONTAINER_FILES_DIR1 == None) {
        CONTAINER_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/Containers"
      } else
        CONTAINER_FILES_DIR = CONTAINER_FILES_DIR1.get

      logger.debug("CONTAINER_FILES_DIR => " + CONTAINER_FILES_DIR)

      var COMPILER_WORK_DIR = ""
      val COMPILER_WORK_DIR1 = configMap.APIConfigParameters.COMPILER_WORK_DIR
      if (COMPILER_WORK_DIR1 == None) {
        COMPILER_WORK_DIR = "/tmp"
      } else
        COMPILER_WORK_DIR = COMPILER_WORK_DIR1.get

      logger.debug("COMPILER_WORK_DIR => " + COMPILER_WORK_DIR)

      var MODEL_EXEC_FLAG = ""
      val MODEL_EXEC_FLAG1 = configMap.APIConfigParameters.MODEL_EXEC_FLAG
      if (MODEL_EXEC_FLAG1 == None) {
        MODEL_EXEC_FLAG = "false"
      } else
        MODEL_EXEC_FLAG = MODEL_EXEC_FLAG1.get

      logger.debug("MODEL_EXEC_FLAG => " + MODEL_EXEC_FLAG)

      val CONFIG_FILES_DIR = gitRootDir + "/Kamanja/trunk/SampleApplication/Medical/Configs"
      logger.debug("CONFIG_FILES_DIR => " + CONFIG_FILES_DIR)

      var OUTPUTMESSAGE_FILES_DIR = ""
      val OUTPUTMESSAGE_FILES_DIR1 = configMap.APIConfigParameters.OUTPUTMESSAGE_FILES_DIR
      if (OUTPUTMESSAGE_FILES_DIR1 == None) {
        OUTPUTMESSAGE_FILES_DIR = gitRootDir + "/Kamanja/trunk/MetadataAPI/src/test/SampleTestFiles/OutputMsgs"
      } else
        OUTPUTMESSAGE_FILES_DIR = OUTPUTMESSAGE_FILES_DIR1.get
      logger.debug("OUTPUTMESSAGE_FILES_DIR => " + OUTPUTMESSAGE_FILES_DIR)

      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ROOT_DIR", rootDir)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("GIT_ROOT", gitRootDir)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE", database)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE_HOST", databaseHost)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE_SCHEMA", databaseSchema)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("DATABASE_LOCATION", databaseLocation)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ADAPTER_SPECIFIC_CONFIG", databaseAdapterSpecificConfig)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("METADATA_DATASTORE", metadataDataStore)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_TARGET_DIR", jarTargetDir)
      val jp = if (jarPaths != null) jarPaths else jarTargetDir
      val j_paths = jp.split(",").map(s => s.trim).filter(s => s.size > 0)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_PATHS", j_paths.mkString(","))
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("SCALA_HOME", scalaHome)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAVA_HOME", javaHome)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("MANIFEST_PATH", manifestPath)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("CLASSPATH", classPath)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("NOTIFY_ENGINE", notifyEngine)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZNODE_PATH", znodePath)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("ZOOKEEPER_CONNECT_STRING", zooKeeperConnectString)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("MODEL_FILES_DIR", MODEL_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("TYPE_FILES_DIR", TYPE_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("FUNCTION_FILES_DIR", FUNCTION_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("CONCEPT_FILES_DIR", CONCEPT_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("MESSAGE_FILES_DIR", MESSAGE_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("CONTAINER_FILES_DIR", CONTAINER_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("COMPILER_WORK_DIR", COMPILER_WORK_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("MODEL_EXEC_LOG", MODEL_EXEC_FLAG)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("CONFIG_FILES_DIR", CONFIG_FILES_DIR)
      MetadataAPIImpl.GetMetadataAPIConfig.setProperty("OUTPUTMESSAGE_FILES_DIR", OUTPUTMESSAGE_FILES_DIR)

      MetadataAPIImpl.propertiesAlreadyLoaded = true;

    } catch {
      case e: MappingException => {
        
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        
        logger.debug("", e)
        throw LoadAPIConfigException("Failed to load configuration", e)
      }
    }
  }

    /**
     * LoadAllConfigObjectsIntoCache
      *
      * @return
     */
  def LoadAllConfigObjectsIntoCache: Boolean = {
    try {
      var processed: Long = 0L
      val storeInfo = PersistenceUtils.GetContainerNameAndDataStore("config_objects")
      storeInfo._2.get(storeInfo._1, { (k: Key, v: Any, serType: String, typ: String, ver:Int) =>
        {
          val strKey = k.bucketKey.mkString(".")
          val i = strKey.indexOf(".")
          val objType = strKey.substring(0, i)
          val typeName = strKey.substring(i + 1)
          processed += 1
          objType match {
            case "nodeinfo" => {
              val ni = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[NodeInfo]//serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[NodeInfo]
              MdMgr.GetMdMgr.AddNode(ni)

            }
            case "adapterinfo" => {
              val ai = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[AdapterInfo]//serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[AdapterInfo]
              MdMgr.GetMdMgr.AddAdapter(ai)
            }
            case "clusterinfo" => {
              val ci = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[ClusterInfo]//serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[ClusterInfo]
              MdMgr.GetMdMgr.AddCluster(ci)
            }
            case "clustercfginfo" => {
              val ci = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[ClusterCfgInfo]//serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[ClusterCfgInfo]
              MdMgr.GetMdMgr.AddClusterCfg(ci)
            }
            case "userproperties" => {
              val up = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[UserPropertiesInfo]//serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[UserPropertiesInfo]
              MdMgr.GetMdMgr.AddUserProperty(up)
            }
            case "tenantinfo" => {
                val up = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[TenantInfo] //serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[TenantInfo]
                MdMgr.GetMdMgr.AddTenantInfo(up)
            }
            case "adaptermessagebinding" => {
                val binding = MetadataAPISerialization.deserializeMetadata(new String(v.asInstanceOf[Array[Byte]])).asInstanceOf[AdapterMessageBinding] //serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[TenantInfo]
                MdMgr.GetMdMgr.AddAdapterMessageBinding(binding)
            }
            case _ => {
              throw InternalErrorException("LoadAllConfigObjectsIntoCache: Unknown objectType " + objType, null)
            }
          }
        }
      })

      if (processed == 0) {
        logger.debug("No config objects available in the Database")
        return false
      }

      return true
    } catch {
      case e: Exception => {
        
        logger.debug("", e)
        return false
      }
    }
  }

    /**
     * LoadAllModelConfigsIntoChache
     */
  def LoadAllModelConfigsIntoCache: Unit = {
    val maxTranId = PersistenceUtils.GetTranId
    MetadataAPIImpl.setCurrentTranLevel(maxTranId)
    logger.debug("Max Transaction Id => " + maxTranId)

    var processed: Long = 0L
    val storeInfo = PersistenceUtils.GetContainerNameAndDataStore("model_config_objects")
    storeInfo._2.get(storeInfo._1, { (k: Key, v: Any, serType: String, typ: String, ver:Int) =>
      {
        processed += 1
        val json = parse(new String(v.asInstanceOf[Array[Byte]]))
        //val conf = serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[Map[String, List[String]]]
        val conf = json.values.asInstanceOf[Map[String, Any]]
        MdMgr.GetMdMgr.AddModelConfig(k.bucketKey.mkString("."), conf)
      }
    })
    if (processed == 0) {
      logger.debug("No model config objects available in the Database")
      return
    }
    MdMgr.GetMdMgr.DumpModelConfigs
  }

    /**
      * LoadAdapterMessageBindingIntoCache
      *
      * @param key string of the form "s"${zkMessage.ObjectType}.${zkMessage.Name}" where the object type is the
      *            "AdapterMsgBinding" and the Name is the FullBindingName of the object to fetch
      */
    def LoadAdapterMessageBindingIntoCache(key: String) {
        try {
            logger.debug("Fetch the AdapterMessageBinding with key: " + key + " from persistent store ")
            val (objtype, jsonBytes) : (String, Any) = PersistenceUtils.GetObject(key.toLowerCase, "adapter_message_bindings")
            logger.debug("Deserialize the binding " + key)
            val binding : AdapterMessageBinding =
                    MetadataAPISerialization.deserializeMetadata(new String(jsonBytes.asInstanceOf[Array[Byte]])).asInstanceOf[AdapterMessageBinding]
            if (binding != null) {
                if (! binding.IsDeleted) {
                    logger.debug("Add the binding " + key + " to the metadata cache ")
                    MdMgr.GetMdMgr.AddAdapterMessageBinding(binding)
                }
            }
        } catch {
            case e: Exception => {
                logger.warn("Unable to load the object " + key + " into cache ", e)
            }
        }
    }



}
