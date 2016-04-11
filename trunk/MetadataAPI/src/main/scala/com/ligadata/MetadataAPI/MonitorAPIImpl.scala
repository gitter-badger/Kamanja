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
import com.ligadata.HeartBeat.MonitoringContext
import com.ligadata.Serialize.JsonSerializer
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.apache.logging.log4j._
import org.json4s.jackson.Serialization
import scala.actors.threadpool.{ Executors, ExecutorService }
import scala.collection.mutable.ArrayBuffer


/**
 * MonitorAPIImpl - Implementation for methods required to access Monitor related methods. 
 * @author danielkozin
 */
object MonitorAPIImpl {
  
  val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  val CHILD_ADDED_ACTION = "CHILD_ADDED"
  val CHILD_REMOVED_ACTION = "CHILD_REMOVED"
  val CHILD_UPDATED_ACTION = "CHILD_UPDATED"
  val ENGINE = "engine"
  val METADATA = "metadata"
  var _exec = Executors.newFixedThreadPool(1)

  private var startTime = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var uniqueId: Long = 0
  private var name = ""
  private var metrics: scala.collection.mutable.MutableList[com.ligadata.HeartBeat.MonitorComponentInfo] = scala.collection.mutable.MutableList[com.ligadata.HeartBeat.MonitorComponentInfo]()
  
  private var healthInfo: scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map[String,Any]()
  
  /**
   * updateHeartbeatInfo - this is a callback function, zookeeper listener will call here to update the local cache. Users should not
   *                       calling in here by themselves
   * 
   */
  def updateHeartbeatInfo(eventType: String, eventPath: String, eventPathData: Array[Byte], children: Array[(String, Array[Byte])]): Unit = {
    
    try {
      if (eventPathData == null) return
      logger.debug("eventType ->" + eventType)
      logger.debug("eventPath ->" + eventPath)
      if (eventPathData == null) {  logger.debug("eventPathData is null"); return; }
      logger.debug("eventPathData ->" + new String(eventPathData))
    
      // Ok, we got an event, parse to see what it is.
      var pathTokens = eventPath.split('/')
     
      // We are guaranteed the pathTokens.length - 2 is either a Metadata or Engine
      var componentName = pathTokens(pathTokens.length - 2)
      var nodeName = pathTokens(pathTokens.length - 1)
    
      // Add or Remove the data to/from the map.
      if (eventType.equals(CHILD_ADDED_ACTION) || eventType.equals(CHILD_UPDATED_ACTION)) {
        var temp = new String(eventPathData)
        var tempMap = parse(temp).values
        healthInfo(nodeName) = tempMap
      }
      else 
        if (healthInfo.contains(nodeName)) healthInfo.remove(nodeName)
    } catch {
      case e: Exception => {
        logger.warn("Exception in hearbeat ",e)
      }
    }  
  }
  
  /**
   * getHeartbeatInfo - get the heartbeat information from the zookeeper.  This informatin is placed there
   *                    by Kamanja Engine instances.
   * @return - String
   */
   def getHeartbeatInfo(ids: List[String]) : String = {
     var ar = healthInfo.values.toArray
     var isFirst = true
     var resultJson = "["
       // If List is empty, then return everything. otherwise, return only those items that are present int he
     // list    
     if (ids.length > 0) {
       ids.foreach (id => {
         if (healthInfo.contains(id)) {
           if (!isFirst) resultJson + ","
           resultJson +=  JsonSerializer.SerializeMapToJsonString(healthInfo(id).asInstanceOf[Map[String,Any]])
           isFirst = false
         }
       })
     } else {
       if (healthInfo != null && ar.length > 0) {
         for(i <- 0 until ar.length ) {
           if (!isFirst) resultJson = resultJson + ","
           resultJson += JsonSerializer.SerializeMapToJsonString(ar(i).asInstanceOf[Map[String,Any]])
           isFirst = false
         }
       }      
     }
    return resultJson + "]"
   }

  /**
    * gets only info of the nodes
    * @param ids
    * @return
    */
  def getHBNodesOnly(ids: List[String]) : String = {
    val getAllNodes = ids.length == 0
    val excludedInfo = Set("components")
    var resultJson = "["
    var isFirst = true
    healthInfo.filter(kv => getAllNodes || ids.contains(kv._1)).foreach(kv => {
      val currentNodeId = kv._1
      val currentNodeAllInfo = kv._2.asInstanceOf[Map[String,Any]]
      val currentNodeOnlyInfo = scala.collection.mutable.Map[String,Any]()
      //exclude info that we don't need: components here
      currentNodeAllInfo.foreach(nodeInfoKv => {
        if(!excludedInfo.contains(nodeInfoKv._1.toLowerCase))
          currentNodeOnlyInfo.put(nodeInfoKv._1,nodeInfoKv._2)
      })

      if (!isFirst) resultJson += ","
      isFirst = false
      resultJson += JsonSerializer.SerializeMapToJsonString(currentNodeOnlyInfo.toMap)
    })

    resultJson += "]"
    resultJson
  }


  /**
    * get info for nodes, regarding components, get type and name only
    * @param ids
    * @return
    */
  def getHBComponentNames(ids: List[String]) : String = {
    val getAllNodes = ids.length == 0
    val componentsKey = "Components"
    val neededComponentKeyset = Set("type", "name")
    var isFirst = true
    var resultJson = "["
    healthInfo.filter(kv => getAllNodes || ids.contains(kv._1)).foreach(kv => {
      val currentNodeId = kv._1
      val currentNodeAllInfo = kv._2.asInstanceOf[Map[String,Any]]
      val currentNodeNeededInfo = scala.collection.mutable.Map[String,Any]()

      val currentNodeComponentsBuffer = ArrayBuffer[scala.collection.mutable.Map[String,Any]]()
      currentNodeAllInfo.foreach(nodeInfoKv => {

        if(componentsKey == nodeInfoKv._1){//this is Components
          val componentsAll = nodeInfoKv._2.asInstanceOf[List[Map[String, Any]]]

          componentsAll.foreach(componentInfo => {
            val componentsNeededInfo = scala.collection.mutable.Map[String,Any]()
            componentInfo.foreach(componentKv => {
              if(neededComponentKeyset.contains(componentKv._1.toLowerCase))
                componentsNeededInfo.put(componentKv._1, componentKv._2)
            })
            currentNodeComponentsBuffer += componentsNeededInfo
          })
        }
        else
          currentNodeNeededInfo.put(nodeInfoKv._1,nodeInfoKv._2)

      })

      currentNodeNeededInfo.put(componentsKey, currentNodeComponentsBuffer)

      if (!isFirst) resultJson += ","
      isFirst = false
      resultJson += JsonSerializer.SerializeMapToJsonString(currentNodeNeededInfo.toMap)
    })

    resultJson += "]"
    resultJson
  }

  def getHBComponentDetailsByNames(componentNames: List[String]) : String = {
    val componentsKey = "Components"
    val componentNameKey = "Name"
    var isFirst = true
    var resultJson = "["
    healthInfo.foreach(kv => {
      val currentNodeId = kv._1
      val currentNodeAllInfo = kv._2.asInstanceOf[Map[String,Any]]
      val currentNodeNeededInfo = scala.collection.mutable.Map[String,Any]()
      val currentNodeComponentsBuffer = ArrayBuffer[scala.collection.immutable.Map[String,Any]]()
      currentNodeAllInfo.foreach(nodeInfoKv => {

        if(componentsKey == nodeInfoKv._1){//this is Components
        val componentsAll = nodeInfoKv._2.asInstanceOf[List[Map[String, Any]]]
          val componentsNeededInfo = scala.collection.mutable.Map[String,Any]()
          componentsAll.foreach(componentInfo => {
            if(componentNames.contains(componentInfo(componentNameKey).toString)){
              currentNodeComponentsBuffer += componentInfo
            }
          })
        }
        else
          currentNodeNeededInfo.put(nodeInfoKv._1,nodeInfoKv._2)

      })
      currentNodeNeededInfo.put(componentsKey, currentNodeComponentsBuffer)

      if (!isFirst) resultJson += ","
      isFirst = false
      resultJson += JsonSerializer.SerializeMapToJsonString(currentNodeNeededInfo.toMap)
    })

    resultJson += "]"
    resultJson
  }

  /**
   * clockNewActivity - update Metadata health info, showing its still alive.
   */
  def clockNewActivity: Unit = {

    lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
    uniqueId = MonitoringContext.monitorCount.incrementAndGet
    implicit val formats = org.json4s.DefaultFormats

    val dataJson =
      ("Name" -> name) ~
        ("UniqueId" -> uniqueId) ~
        ("LastSeen" -> lastSeen) ~
        ("StartTime" -> startTime) ~
        ("Components" -> metrics.map(mci =>
          ("Type" -> mci.typ) ~
            ("Name" -> mci.name) ~
            ("Description" -> mci.description) ~
            ("LastSeen" -> mci.lastSeen) ~
            ("StartTime" -> mci.startTime) ~
            ("Metrics" -> mci.metricsJsonString)))

    MetadataAPIImpl.zkc.setData().forPath(MetadataAPIImpl.zkHeartBeatNodePath, compact(render(dataJson)).getBytes)
  }

  def initMonitorValues(inName: String): Unit = {
    startTime = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
    uniqueId = 0
    name = inName
  }

  /**
    * startMetadataHeartbeat - will be called internally by the MetadataAPI task to update the healthcheck every 5 seconds.
    */
   def startMetadataHeartbeat: Unit = {
     _exec.execute(new Runnable() {
       override def run() = {
         var startTime = System.currentTimeMillis
         while (!_exec.isShutdown) {
           try {
             clockNewActivity
             Thread.sleep(5000)
           }
           catch {
             case e: Exception => {
               if (!_exec.isShutdown)
                 logger.warn("", e)
             }
           }
         }
       }
      })
   }
   
   /**
    * shutdownMonitor - Shutdown the Monitor Heartbeat
    */
  def shutdownMonitor: Unit = {_exec.shutdown}
}