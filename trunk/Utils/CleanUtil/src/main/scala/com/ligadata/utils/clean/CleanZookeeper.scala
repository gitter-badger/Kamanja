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
import org.apache.curator.framework.{CuratorFrameworkFactory, CuratorFramework}
import org.apache.curator.retry.ExponentialBackoffRetry
import org.apache.zookeeper.KeeperException.NoNodeException
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.collection.JavaConversions._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

case class HeartBeats(heartBeats: List[HeartBeat])
case class HeartBeat(name: String, uniqueId: Int, lastSeen: DateTime, startTime: DateTime, components: List[String], metrics: List[String])

object CleanZookeeper {
  private val logger = org.apache.logging.log4j.LogManager.getLogger(this.getClass)
  private lazy val retryPolicy = new ExponentialBackoffRetry(1000, 3)
  implicit val formats = DefaultFormats

  def deletePath(zkInfo: ZooKeeperInfo): Unit = {
    var zkc: CuratorFramework = null
    try {
      zkc = CuratorFrameworkFactory.newClient(zkInfo.connStr, 6000, 6000, retryPolicy)
      zkc.start()
      logger.info("Deleting Zookeeper node " + zkInfo.nodeBasePath)
      zkc.delete().deletingChildrenIfNeeded.forPath(zkInfo.nodeBasePath)
      if (zkc.checkExists().forPath(zkInfo.nodeBasePath) != null) {
        throw CleanUtilException("CLEAN-UTIL: Failed to delete zookeeper path " + zkInfo.nodeBasePath, null)
      }
    }
      catch {
        case e: CleanUtilException => throw(e)
        case e: Exception => throw CleanUtilException("CLEAN-UTIL: Failed to delete zookeeper path " + zkInfo.nodeBasePath, e)
      }
    finally {
      if (zkc != null) zkc.close()
    }
  }

  def isKamanjaClusterRunning(zkInfo: ZooKeeperInfo): Boolean = {
    val monitorPaths: List[String] = getMonitorPaths(zkInfo)
    if(monitorPaths.isEmpty) {
      logger.info("CLEAN-UTIL: Monitor paths not found in zookeeper. Cluster is not running")
      return false
    }
    var zkc: CuratorFramework = null
    var nodesRunning: List[String] = List()
    var hbBeforeMap: Map[String, HeartBeat] = Map()
    var hbAfterMap: Map[String, HeartBeat] = Map()
    try {
      zkc = CuratorFrameworkFactory.builder()
        .connectString(zkInfo.connStr)
        .connectionTimeoutMs(6000)
        .sessionTimeoutMs(6000)
        .retryPolicy(retryPolicy)
        .build()
      zkc.start()
      // Get all heartbeats from zookeeper



      monitorPaths.foreach(path => {
        val data = new String(zkc.getData.forPath(path))
        val hb = parseHeartbeat(data)
        var category = ""
        if(path.contains("metadata"))
          category = "metadata"
        else category = "engine"
          hbBeforeMap = hbBeforeMap + ((category + ":" + hb.name) -> hb)
      })

      // Sleep 5 seconds before getting heartbeats again for comparison
      Thread sleep 5000

      monitorPaths.foreach(path => {
        val data = new String(zkc.getData.forPath(path))
        val hb = parseHeartbeat(data)
        var category = ""
        if(path.contains("metadata"))
          category = "metadata"
        else category = "engine"
        hbAfterMap = hbAfterMap + ((category + ":" + hb.name) -> hb)
      })

      hbBeforeMap.foreach(hbb => {
        val beforeDate = hbb._2.lastSeen
        val afterDate = hbAfterMap(hbb._1).lastSeen
        if(beforeDate.isBefore(afterDate)) {
          nodesRunning = nodesRunning :+ hbb._1
        }
      })
    }
    catch {
      case e: org.apache.zookeeper.KeeperException.NoNodeException => throw CleanUtilException(
        "CLEAN-UTIL: Failed to find the zookeeper node path after retrieving it previously. " +
          "An external process may have altered zookeeper. " +
          "Please check that there are no instances of Kamanja or MetadataAPIService running.", e
      )
      case e: Exception => throw CleanUtilException("CLEAN-UTIL: Failed to determine if the kamanja cluster is running", e)
    }
    finally {
      if(zkc != null)
        zkc.close()
    }
    if(nodesRunning.length > 0) {
      logger.info("CLEAN-UTIL: Nodes running are " + nodesRunning.mkString(",") + " are running. Please shutdown any nodes that are currently running.")
      return true
    }
    else {
      logger.info("CLEAN-UTIL: No nodes were found running")
      return false
    }
  }

  private def parseHeartbeat(heartbeatData: String): HeartBeat = {
    logger.debug("CLEAN-UTIL: Parsing heartbeat data:\n" + heartbeatData)
    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
    val json = parse(heartbeatData)
    val name = (json \ "Name").values.toString
    val uniqueId = (json \ "UniqueId").values.asInstanceOf[BigInt].toInt
    val lastSeen = formatter.parseDateTime((json \ "LastSeen").values.toString)
    val startTime = formatter.parseDateTime((json \ "StartTime").values.toString)
    val components = (json \ "Components").values.asInstanceOf[List[String]]
    val metrics = (json \ "Metrics").values.asInstanceOf[List[String]]

    return new HeartBeat(name, uniqueId, lastSeen, startTime, components, metrics)
  }

  private def getMonitorPaths(zkInfo: ZooKeeperInfo): List[String] = {
    var zkc: CuratorFramework = null
    val engineMonitorPath = zkInfo.nodeBasePath + "/monitor/engine"
    val metadataMonitorPath = zkInfo.nodeBasePath + "/monitor/metadata"
    try {
      zkc = CuratorFrameworkFactory.newClient(zkInfo.connStr, 6000, 6000, retryPolicy)
      zkc.start()
      logger.info("CLEAN-UTIL: Getting Zookeeper node " + engineMonitorPath)

      var children: List[String] = List()
      try {
        val metadataChildren = zkc.getChildren.forPath(metadataMonitorPath)
        metadataChildren.foreach(child => {
          children = children :+ (metadataMonitorPath + "/" + child)
        })
      }
      catch {
        case e: NoNodeException => logger.warn("CLEAN-UTIL: Unable to find zookeeper node: " + metadataMonitorPath + " continuing...")
      }

      try {
        val engineChildren = zkc.getChildren.forPath(engineMonitorPath)
        engineChildren.foreach(child => {
          children = children :+ (engineMonitorPath + "/" + child)
        })
      }
      catch {
        case e: NoNodeException => logger.warn("CLEAN-UTIL: Unable to find zookeeper node: " + engineMonitorPath + " continuing...")
      }

      return children
    }
    catch {
      case e: CleanUtilException => throw(e)
      case e: Exception => throw CleanUtilException("CLEAN-UTIL: Failed to get zookeeper path " + engineMonitorPath, e)
    }
    finally {
      if(zkc != null) zkc.close()
    }
  }
}