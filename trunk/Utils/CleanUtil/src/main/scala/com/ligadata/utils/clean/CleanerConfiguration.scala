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

package com.ligadata.utils.clean

import com.ligadata.Exceptions.CleanUtilException
import com.ligadata.MetadataAPI._
import com.ligadata.StorageBase.DataStore
import com.ligadata.keyvaluestore.KeyValueManager
import com.ligadata.kamanja.metadata.{MdMgr, AdapterInfo, ClusterCfgInfo}
import org.json4s._
import org.json4s.jackson.JsonMethods._

case class ZooKeeperInfo(nodeBasePath: String, connStr: String)

class CleanerConfiguration(metadataConfigFile: String) {
  private val logger = org.apache.logging.log4j.LogManager.getLogger(this.getClass)

  var zookeeperInfo: ZooKeeperInfo = null
  var dataStore: DataStore = null
  var metadataStore: DataStore = null
  var topicList: List[String] = List()

  // Constructor
  try {
    MetadataAPIImpl.InitMdMgrFromBootStrap(metadataConfigFile, false)
    initialize
  }
  catch {
    case e: IllegalArgumentException => {
      logger.warn("", e)
      shutdown
      throw e
    }
    case e: CleanUtilException => {
      logger.warn("", e)
      shutdown
      throw e
    }
    case e: Exception => {
      logger.warn("", e)
      shutdown
      throw CleanUtilException("CLEAN-UTIL: Failed to initialize", e)
    }
  }
  // End Constructor

  private def getAdapterList(): Array[AdapterInfo] = {
    val adapters = MdMgr.GetMdMgr.Adapters.values.toArray
    return adapters
  }

  private def getTopicNames: List[String] = {
    logger.info("CLEAN-UTIL: Retrieving topic names from cluster configuration")
    var topicNames:List[String] = List()
    getAdapterList().foreach(adapter => {
      val json = parse(adapter.AdapterSpecificCfg)
      val topicName: String = (json \ "TopicName").values.toString
      if(!topicNames.contains(topicName))
        topicNames = topicNames :+ topicName
    })
    if(topicNames.length == 0)
      logger.error("CLEAN-UTIL: Failed to retrieve topic names from cluster configuration. Please ensure you've uploaded cluster configuration to metadata")
    logger.info("CLEAN-UTIL: Fetched topic names: " + topicNames)
    topicNames
  }

  private def initialize(): Unit = {
    val clusterCfgs: Array[ClusterCfgInfo] = MdMgr.GetMdMgr.ClusterCfgs.values.toArray
    if(clusterCfgs.length == 0 || clusterCfgs == null)
      throw CleanUtilException("CLEAN-UTIL: Failed to retrieve cluster configuration. Please ensure you've uploaded cluster configuration to metadata.", null)
    clusterCfgs.foreach(clusterCfg => {
      // Retrieving Zookeeper configuration
      logger.info("CLEAN-UTIL: Retrieving Zookeeper Node Base Path from cluster configuration")
      val zkJson = parse(clusterCfg.CfgMap("ZooKeeperInfo"))
      val nodeBasePath = (zkJson \ "ZooKeeperNodeBasePath").values.toString
      val connStr: String = (zkJson \ "ZooKeeperConnectString").values.toString
      if (zkJson == null || nodeBasePath == null || connStr == null)
        throw CleanUtilException("CLEAN-UTIL: Failed to retrieve zookeeper configuration. Please ensure you've uploaded cluster configuration to metadata.", null)
      zookeeperInfo = new ZooKeeperInfo(nodeBasePath, connStr)
      // End Zookeeper configuration

      // Retrieving the Jar Paths to be passed into KeyValueManager
      val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
      val jarPaths: Set[String] = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()

      // Get DataStore storage adapter
      dataStore = KeyValueManager.Get(jarPaths, clusterCfg.cfgMap("SystemCatalog"))

      // Get MetadataStore storage adapter
      metadataStore = MetadataAPIImpl.GetMainDS

      // Retrieving a list of Kafka topic names from configuration.
      // TODO: How to handle different input/output adapters. Right now I'm assuming kafka here.
      topicList = getTopicNames
    })
  }

  def shutdown: Unit = {
    MetadataAPIImpl.shutdown
    if(dataStore != null) dataStore.Shutdown
  }

}
