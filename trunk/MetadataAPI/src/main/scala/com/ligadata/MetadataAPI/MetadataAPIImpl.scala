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

case class ParameterMap(RootDir: String, GitRootDir: String, MetadataStoreType: String, MetadataSchemaName: Option[String], /* MetadataAdapterSpecificConfig: Option[String], */ MetadataLocation: String, JarTargetDir: String, ScalaHome: String, JavaHome: String, ManifestPath: String, ClassPath: String, NotifyEngine: String, ZnodePath: String, ZooKeeperConnectString: String, MODEL_FILES_DIR: Option[String], TYPE_FILES_DIR: Option[String], FUNCTION_FILES_DIR: Option[String], CONCEPT_FILES_DIR: Option[String], MESSAGE_FILES_DIR: Option[String], CONTAINER_FILES_DIR: Option[String], COMPILER_WORK_DIR: Option[String], MODEL_EXEC_FLAG: Option[String], OUTPUTMESSAGE_FILES_DIR: Option[String])

case class ZooKeeperInfo(ZooKeeperNodeBasePath: String, ZooKeeperConnectString: String, ZooKeeperSessionTimeoutMs: Option[String], ZooKeeperConnectionTimeoutMs: Option[String])

case class MetadataAPIConfig(APIConfigParameters: ParameterMap)

case class APIResultInfo(statusCode: Int, functionName: String, resultData: String, description: String)
case class APIResultJsonProxy(APIResults: APIResultInfo)

object MetadataAPIGlobalLogger {
    val loggerName = this.getClass.getName
    val logger = LogManager.getLogger(loggerName)
}

trait LogTrait {
    val logger = MetadataAPIGlobalLogger.logger
}

// The implementation class
object MetadataAPIImpl extends MetadataAPI with LogTrait {

  lazy val sysNS = "System" // system name space

  lazy val serializerType = "json4s"//"kryo"
  //lazy val serializer = SerializerManager.GetSerializer(serializerType)
  lazy val metadataAPIConfig = new Properties()
  var zkc: CuratorFramework = null
  private var authObj: SecurityAdapter = null
  private var auditObj: AuditAdapter = null
  val configFile = System.getenv("HOME") + "/MetadataAPIConfig.json"
  var propertiesAlreadyLoaded = false
  var isInitilized: Boolean = false
  private var zkListener: ZooKeeperListener = _
  private var cacheOfOwnChanges: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()
  private var currentTranLevel: Long = _
  private var passwd: String = null
  private var compileCfg: String = ""
  private var heartBeat: HeartBeatUtil = null
  var zkHeartBeatNodePath = ""

  def getCurrentTranLevel = currentTranLevel
  def setCurrentTranLevel(tranLevel: Long) = {
    currentTranLevel = tranLevel
  }

  def GetMainDS: DataStore = PersistenceUtils.GetMainDS
  var isCassandra = false
  private[this] val lock = new Object
  var startup = false


    /**
     * CloseZKSession
     */
  def CloseZKSession: Unit = lock.synchronized {
    if (zkc != null) {
      logger.debug("Closing zookeeper session ..")
      try {
        zkc.close()
        logger.debug("Closed zookeeper session ..")
        zkc = null
      } catch {
        case e: Exception => {
          logger.error("Unexpected Error while closing zookeeper session", e)
        }
      }
    }
  }

  def GetSchemaId: Int = GetMetadataId("schemaid", true, 2000001).toInt // This should start atleast from 2,000,001. because 1 - 1,000,000 is reserved for System Containers & 1,000,001 - 2,000,000 is reserved for System Messages
  def GetUniqueId: Long = GetMetadataId("uniqueid", true, 100000) // This starts from 100000
  def GetMdElementId: Long = GetMetadataId("mdelementid", true, 100000) // This starts from 100000

  /**
   *  getHealthCheck - will return all the health-check information for the nodeId specified.
    *
    *  @param nodeId a cluster node: String - if no parameter specified, return health-check for all nodes
   *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
   *               method. If Security and/or Audit are configured, this value must be a value other than None.
   */
  def getHealthCheck(nodeId: String = "", userid: Option[String] = None): String = {
    try {
      val ids = parse(nodeId).values.asInstanceOf[List[String]]
      var apiResult = new ApiResultComplex(ErrorCodeConstants.Success, "GetHeartbeat", MonitorAPIImpl.getHeartbeatInfo(ids), ErrorCodeConstants.GetHeartbeat_Success)
      apiResult.toString
    } catch {
      case cce: java.lang.ClassCastException => {
        logger.warn("Failure processing GET_HEALTH_CHECK - cannot parse the list of desired nodes.", cce)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error:Parsing Error")
        return apiResult.toString
      }
      case e: Exception => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error: Unknown - see Kamanja Logs")
        logger.error("Failure processing GET_HEALTH_CHECK - unknown", e)
        return apiResult.toString
      }
    }

  }

  /**
    *  getHealthCheckNodesOnly - will return node info from the health-check information for the nodeId specified.
    *
    *  @param nodeId a cluster node: String - if no parameter specified, return health-check for all nodes
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckNodesOnly(nodeId: String = "", userid: Option[String] = None): String = {
    try {
      val ids = parse(nodeId).values.asInstanceOf[List[String]]
      var apiResult = new ApiResultComplex(ErrorCodeConstants.Success, "GetHeartbeat", MonitorAPIImpl.getHBNodesOnly(ids), ErrorCodeConstants.GetHeartbeat_Success)
      apiResult.toString
    } catch {
      case cce: java.lang.ClassCastException => {
        logger.warn("Failure processing GET_HEALTH_CHECK(Nodes Only) - cannot parse the list of desired nodes.", cce)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error:Parsing Error")
        return apiResult.toString
      }
      case e: Exception => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error: Unknown - see Kamanja Logs")
        logger.error("Failure processing GET_HEALTH_CHECK(Nodes Only) - unknown", e)
        return apiResult.toString
      }
    }

  }

  /**
    *  getHealthCheckComponentNames - will return partial components info from the health-check information for the nodeId specified.
    *
    *  @param nodeId a cluster node: String - if no parameter specified, return health-check for all nodes
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckComponentNames(nodeId: String = "", userid: Option[String] = None): String = {
    try {
      val ids = parse(nodeId).values.asInstanceOf[List[String]]
      var apiResult = new ApiResultComplex(ErrorCodeConstants.Success, "GetHeartbeat", MonitorAPIImpl.getHBComponentNames(ids), ErrorCodeConstants.GetHeartbeat_Success)
      apiResult.toString
    } catch {
      case cce: java.lang.ClassCastException => {
        logger.warn("Failure processing GET_HEALTH_CHECK(Component Names) - cannot parse the list of desired nodes.", cce)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error:Parsing Error")
        return apiResult.toString
      }
      case e: Exception => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error: Unknown - see Kamanja Logs")
        logger.error("Failure processing GET_HEALTH_CHECK(Component Names) - unknown", e)
        return apiResult.toString
      }
    }

  }

  /**
    *  getHealthCheckComponentDetailsByNames - will return specific components info from the health-check information for the nodeId specified.
    *
    *  @param componentNames names of components required
    *  @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    */
  def getHealthCheckComponentDetailsByNames(componentNames: String = "", userid: Option[String] = None): String = {
    try {
      val components = parse(componentNames).values.asInstanceOf[List[String]]
      var apiResult = new ApiResultComplex(ErrorCodeConstants.Success, "GetHeartbeat", MonitorAPIImpl.getHBComponentDetailsByNames(components), ErrorCodeConstants.GetHeartbeat_Success)
      apiResult.toString
    } catch {
      case cce: java.lang.ClassCastException => {
        logger.warn("Failure processing GET_HEALTH_CHECK(Specific Components) - cannot parse the list of desired nodes.", cce)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error:Parsing Error")
        return apiResult.toString
      }
      case e: Exception => {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetHealthCheck", "No data available", ErrorCodeConstants.GetHeartbeat_Failed + " Error: Unknown - see Kamanja Logs")
        logger.error("Failure processing GET_HEALTH_CHECK(Specific Components) - unknown", e)
        return apiResult.toString
      }
    }

  }

  /**
   * InitSecImpl  - 1. Create the Security Adapter class.  The class name and jar name containing
   *                the implementation of that class are specified in the CONFIG FILE.
   *                2. Create the Audit Adapter class, The class name and jar name containing
   *                the implementation of that class are specified in the CONFIG FILE.
   */
  def InitSecImpl: Unit = {
    logger.debug("Establishing connection to domain security server..")
    val classLoader = new KamanjaLoaderInfo

    // Validate the Auth/Audit flags for valid input.
    if ((metadataAPIConfig.getProperty("DO_AUTH") != null) &&
      (!metadataAPIConfig.getProperty("DO_AUTH").equalsIgnoreCase("YES") &&
        !metadataAPIConfig.getProperty("DO_AUTH").equalsIgnoreCase("NO"))) {
      throw new Exception("Invalid value for DO_AUTH detected.  Correct it and restart")
    }
    if ((metadataAPIConfig.getProperty("DO_AUDIT") != null) &&
      (!metadataAPIConfig.getProperty("DO_AUDIT").equalsIgnoreCase("YES") &&
        !metadataAPIConfig.getProperty("DO_AUDIT").equalsIgnoreCase("NO"))) {
      throw new Exception("Invalid value for DO_AUDIT detected.  Correct it and restart")
    }

    // If already have one, use that!
    if (authObj == null && (metadataAPIConfig.getProperty("DO_AUTH") != null) && (metadataAPIConfig.getProperty("DO_AUTH").equalsIgnoreCase("YES"))) {
      createAuthObj(classLoader)
    }
    if (auditObj == null && (metadataAPIConfig.getProperty("DO_AUDIT") != null) && (metadataAPIConfig.getProperty("DO_AUDIT").equalsIgnoreCase("YES"))) {
      createAuditObj(classLoader)
    }
  }

    /**
     * createAuthObj - private method to instantiate an authObj
      *
      * @param classLoader a
     */
  private def createAuthObj(classLoader: KamanjaLoaderInfo): Unit = {
    // Load the location and name of the implementing class from the
    val implJarName = if (metadataAPIConfig.getProperty("SECURITY_IMPL_JAR") == null) "" else metadataAPIConfig.getProperty("SECURITY_IMPL_JAR").trim
    val implClassName = if (metadataAPIConfig.getProperty("SECURITY_IMPL_CLASS") == null) "" else metadataAPIConfig.getProperty("SECURITY_IMPL_CLASS").trim
    logger.debug("Using " + implClassName + ", from the " + implJarName + " jar file")
    if (implClassName == null) {
      logger.error("Security Adapter Class is not specified")
      return
    }

    // Add the Jarfile to the class loader
    loadJar(classLoader, implJarName)

    try {
      Class.forName(implClassName, true, classLoader.loader)
    } catch {
      case e: Exception => {
        logger.error("Failed to load Security Adapter class %s".format(implClassName), e)
        throw e // Rethrow
      }
    }

    // All is good, create the new class
    var className = Class.forName(implClassName, true, classLoader.loader).asInstanceOf[Class[SecurityAdapter]]
    authObj = className.newInstance
    authObj.init
    logger.debug("Created class " + className.getName)
  }

    /**
     * createAuditObj - private method to instantiate an authObj
      *
      * @param classLoader a
     */
  private def createAuditObj(classLoader: KamanjaLoaderInfo): Unit = {
    // Load the location and name of the implementing class froms the
    val implJarName = if (metadataAPIConfig.getProperty("AUDIT_IMPL_JAR") == null) "" else metadataAPIConfig.getProperty("AUDIT_IMPL_JAR").trim
    val implClassName = if (metadataAPIConfig.getProperty("AUDIT_IMPL_CLASS") == null) "" else metadataAPIConfig.getProperty("AUDIT_IMPL_CLASS").trim
    logger.debug("Using " + implClassName + ", from the " + implJarName + " jar file")
    if (implClassName == null) {
      logger.error("Audit Adapter Class is not specified")
      return
    }

    // Add the Jarfile to the class loader
    loadJar(classLoader, implJarName)

        try
            Class.forName(implClassName, true, classLoader.loader)
        catch {
            case e: Exception => {
                logger.error("Failed to load Audit Adapter class %s".format(implClassName), e)
                throw e // Rethrow
            }
        }
    // All is good, create the new class
    var className = Class.forName(implClassName, true, classLoader.loader).asInstanceOf[Class[AuditAdapter]]
    auditObj = className.newInstance
    auditObj.init(metadataAPIConfig.getProperty("AUDIT_PARMS"))
    logger.debug("Created class " + className.getName)
  }

  def GetAuditObj: AuditAdapter = auditObj
    /**
     * loadJar- load the specified jar into the classLoader
      *
      * @param classLoader a
     * @param implJarName a
     */
  private def loadJar(classLoader: KamanjaLoaderInfo, implJarName: String): Unit = {
    // Add the Jarfile to the class loader
    val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
    val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
    val jarName = com.ligadata.Utils.Utils.GetValidJarFile(jarPaths, implJarName)
    val fl = new File(jarName)
    if (fl.exists) {
      try {
        classLoader.loader.addURL(fl.toURI().toURL())
        logger.debug("Jar " + implJarName.trim + " added to class path.")
        classLoader.loadedJars += fl.getPath()
      } catch {
        case e: Exception => {
          logger.error("Failed to add " + implJarName + " due to internal exception ", e)
          return
        }
      }
    } else {
      logger.error("Unable to locate Jar '" + implJarName + "'")
      return
    }
  }

    /**
     * checkAuth
      *
      * @param usrid a
     * @param password a
     * @param role a
     * @param privilige a
     * @return <description please>
     */
  def checkAuth(usrid: Option[String], password: Option[String], role: Option[String], privilige: String): Boolean = {

    var authParms: java.util.Properties = new Properties
    // Do we want to run AUTH?
    if ((metadataAPIConfig.getProperty("DO_AUTH") == null) ||
      (metadataAPIConfig.getProperty("DO_AUTH") != null && !metadataAPIConfig.getProperty("DO_AUTH").equalsIgnoreCase("YES"))) {
      return true
    }

    // check if the Auth object exists
    if (authObj == null) return false

    // Run the Auth - if userId is supplied, defer to that.
    if ((usrid == None) && (role == None)) return false

    if (usrid != None) authParms.setProperty("userid", usrid.get.asInstanceOf[String])
    if (password != None) authParms.setProperty("password", password.get.asInstanceOf[String])
    if (role != None) authParms.setProperty("role", role.get.asInstanceOf[String])
    authParms.setProperty("privilige", privilige)

    return authObj.performAuth(authParms)
  }

    /**
     * getPrivilegeName
      *
      * @param op <description please>
     * @param objName <description please>
     * @return <description please>
     */
  def getPrivilegeName(op: String, objName: String): String = {
    // check if the Auth object exists
    logger.debug("op => " + op)
    logger.debug("objName => " + objName)
    if (authObj == null) return ""
    return authObj.getPrivilegeName(op, objName)
  }

  /**
   * getSSLCertificatePath
   */
  def getSSLCertificatePath: String = {
    val certPath = metadataAPIConfig.getProperty("SSL_CERTIFICATE")
    if (certPath != null) return certPath
    ""
  }

  /**
   * getSSLCertificatePasswd
   */
  def getSSLCertificatePasswd: String = {
    if (passwd != null) return passwd
    ""
  }

    /**
     * setSSLCertificatePasswd
      *
      * @param pw <description please>
     */
  def setSSLCertificatePasswd(pw: String) = {
    passwd = pw
  }

  /**
   * getCurrentTime - Return string representation of the current Date/Time
   */
  def getCurrentTime: String = {
    new Date().getTime().toString()
  }

    /**
     * logAuditRec - Record an Audit event using the audit adapter
      *
      * @param userOrRole the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. The default is None, but if Security and/or Audit are configured, this value is of little practical use.
     *               Supply one.
     * @param userPrivilege <description please>
     * @param action <description please>
     * @param objectText <description please>
     * @param success <description please>
     * @param transactionId <description please>
     * @param objName <description please>
     */
  def logAuditRec(userOrRole: Option[String], userPrivilege: Option[String], action: String, objectText: String, success: String, transactionId: String, objName: String) = {
    if (auditObj != null) {
      val aRec = new AuditRecord

      // If no userName is provided here, that means that somehow we are not running with Security but with Audit ON.
      var userName = "undefined"
      if (userOrRole != None) {
        userName = userOrRole.get
      }

      // If no priv is provided here, that means that somehow we are not running with Security but with Audit ON.
      var priv = "undefined"
      if (userPrivilege != None) {
        priv = userPrivilege.get
      }

      aRec.userOrRole = userName
      aRec.userPrivilege = priv
      aRec.actionTime = getCurrentTime
      aRec.action = action
      aRec.objectAccessed = objName
      aRec.success = success
      aRec.transactionId = transactionId
      aRec.notes = objectText
      try {
        auditObj.addAuditRecord(aRec)
      } catch {
        case e: Exception => {
          logger.error("", e)
          throw UpdateStoreFailedException("Failed to save audit record" + aRec.toString + ":" + e.getMessage(), e)
        }
      }
    }
  }

    /**
     * Get an audit record from the audit adapter.
      *
      * @param startTime <description please>
     * @param endTime <description please>
      * @param userOrRole the identity to be used by the security adapter to ascertain if this user has access permissions for this
      *               method. If Security and/or Audit are configured, this value should be supplied.
     * @param action <description please>
     * @param objectAccessed <description please>
     * @return <description please>
     */
  def getAuditRec(startTime: Date, endTime: Date, userOrRole: String, action: String, objectAccessed: String): String = {
    var apiResultStr = ""
    if (auditObj == null) {
      apiResultStr = "no audit records found "
      return apiResultStr
    }
    try {
      val recs = auditObj.getAuditRecord(startTime, endTime, userOrRole, action, objectAccessed)
      if (recs.length > 0) {
        apiResultStr = JsonSerializer.SerializeAuditRecordsToJson(recs)
      } else {
        apiResultStr = "no audit records found "
      }
    } catch {
      case e: Exception => {
        logger.error("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "Failed to fetch all the audit objects:", null, "Error :" + e.toString)
        apiResultStr = apiResult.toString()
      }
    }
    logger.debug(apiResultStr)
    apiResultStr
  }

    /**
     * parseDateStr
      *
      * @param dateStr <description please>
     * @return <description please>
     */
  def parseDateStr(dateStr: String): Date = {
    try {
      val format = new java.text.SimpleDateFormat("yyyyMMddHHmmss")
      val d = format.parse(dateStr)
      d
    } catch {
      case e: ParseException => {
        logger.debug("", e)
        val format = new java.text.SimpleDateFormat("yyyyMMdd")
        val d = format.parse(dateStr)
        d
      }
    }
  }

    /**
     * getLeaderHost
      *
      * @param leaderNode <description please>
     * @return <description please>
     */
  def getLeaderHost(leaderNode: String): String = {
    val nodes = MdMgr.GetMdMgr.Nodes.values.toArray
    if (nodes.length == 0) {
      logger.debug("No Nodes found ")
      val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetLeaderHost", null, ErrorCodeConstants.Get_Leader_Host_Failed_Not_Available + " :" + leaderNode)
      apiResult.toString()
    } else {
      val nhosts = nodes.filter(n => n.nodeId == leaderNode)
      if (nhosts.length == 0) {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "GetLeaderHost", null, ErrorCodeConstants.Get_Leader_Host_Failed_Not_Available + " :" + leaderNode)
        apiResult.toString()
      } else {
        val nhost = nhosts(0)
        logger.debug("node addr => " + nhost.NodeAddr)
        nhost.NodeAddr
      }
    }
  }

    /**
     * getAuditRec
      *
      * @param filterParameters <description please>
     * @return <description please>
     */
  def getAuditRec(filterParameters: Array[String]): String = {
    var apiResultStr = ""
    if (auditObj == null) {
      apiResultStr = "no audit records found "
      return apiResultStr
    }
    try {
      var audit_interval = 10
      var ai = metadataAPIConfig.getProperty("AUDIT_INTERVAL")
      if (ai != null) {
        audit_interval = ai.toInt
      }
      var startTime: Date = new Date((new Date).getTime() - audit_interval * 60000)
      var endTime: Date = new Date()
      var userOrRole: String = null
      var action: String = null
      var objectAccessed: String = null

      if (filterParameters != null) {
        val paramCnt = filterParameters.size
        paramCnt match {
          case 1 => {
            startTime = parseDateStr(filterParameters(0))
          }
          case 2 => {
            startTime = parseDateStr(filterParameters(0))
            endTime = parseDateStr(filterParameters(1))
          }
          case 3 => {
            startTime = parseDateStr(filterParameters(0))
            endTime = parseDateStr(filterParameters(1))
            userOrRole = filterParameters(2)
          }
          case 4 => {
            startTime = parseDateStr(filterParameters(0))
            endTime = parseDateStr(filterParameters(1))
            userOrRole = filterParameters(2)
            action = filterParameters(3)
          }
          case 5 => {
            startTime = parseDateStr(filterParameters(0))
            endTime = parseDateStr(filterParameters(1))
            userOrRole = filterParameters(2)
            action = filterParameters(3)
            objectAccessed = filterParameters(4)
          }
        }
      } else {
        logger.debug("filterParameters is null")
      }
      apiResultStr = getAuditRec(startTime, endTime, userOrRole, action, objectAccessed)
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "Failed to fetch all the audit objects:", null, "Error :" + e.toString)
        apiResultStr = apiResult.toString()
      }
    }
    logger.debug(apiResultStr)
    apiResultStr
  }

  /**
   * InitZooKeeper - Establish a connection to zookeeper
   */
  def InitZooKeeper: Unit = {
    logger.debug("Connect to zookeeper..")
    if (zkc != null) {
      // Zookeeper is already connected
      return
    }

    val zkcConnectString = GetMetadataAPIConfig.getProperty("ZOOKEEPER_CONNECT_STRING")
    val znodePath = GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/metadataupdate"
    zkHeartBeatNodePath = metadataAPIConfig.getProperty("ZNODE_PATH") + "/monitor/metadata/" + metadataAPIConfig.getProperty("NODE_ID").toString
    logger.debug("Connect To ZooKeeper using " + zkcConnectString)
    try {
      CreateClient.CreateNodeIfNotExists(zkcConnectString, zkHeartBeatNodePath)
      CreateClient.CreateNodeIfNotExists(zkcConnectString, znodePath)
      zkc = CreateClient.createSimple(zkcConnectString)
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw InternalErrorException("Failed to start a zookeeper session with(" + zkcConnectString + ")", e)
      }
    }
  }

    /**
     * shutdownAuditAdapter
     */
  private def shutdownAuditAdapter(): Unit = {
    if (auditObj != null) auditObj.Shutdown
  }

    /**
     * GetMetadataAPIConfig
      *
      * @return <description please>
     */
  def GetMetadataAPIConfig: Properties = {
    metadataAPIConfig
  }

    /**
     * GetObject
      *
      * @param bucketKeyStr
     * @param typeName
     */
  def GetObject(bucketKeyStr: String, typeName: String): (String, Any) = {
    PersistenceUtils.GetObject(bucketKeyStr,typeName)
  }

    /**
     * SaveObject
      *
      * @param bucketKeyStr
     * @param value
     * @param typeName
     * @param serializerTyp
     */
  def SaveObject(bucketKeyStr: String, value: Array[Byte], typeName: String, serializerTyp: String) {
    PersistenceUtils.SaveObject(bucketKeyStr,value,typeName,serializerTyp)
  }

    /**
     * SaveObjectList
      *
      * @param keyList
     * @param valueList
     * @param typeName
     * @param serializerTyp
     */
  def SaveObjectList(keyList: Array[String], valueList: Array[Array[Byte]], typeName: String, serializerTyp: String) {
    PersistenceUtils.SaveObjectList(keyList,valueList,typeName,serializerTyp)
  }

  /**
    * Remove all of the elements with the supplied keys in the list from the supplied DataStore
    *
    * @param keyList
    * @param typeName
    */
  def RemoveObjectList(keyList: Array[String], typeName: String) {
    PersistenceUtils.RemoveObjectList(keyList,typeName)
  }

    /**
     * Answer which table the supplied BaseElemeDef is stored
      *
      * @param obj
     * @return
     */
  def getMdElemTypeName(obj: BaseElemDef): String = {
    PersistenceUtils.getMdElemTypeName(obj)
  }

    /**
     * getObjectType
      *
      * @param obj <description please>
     * @return <description please>
     */
  def getObjectType(obj: BaseElemDef): String = {
    PersistenceUtils.getObjectType(obj)
  }

    /**
     * SaveObjectList
     *
     * The following batch function is useful when we store data in single table
     * If we use Storage component library, note that table itself is associated with a single
     * database connection( which itself can be mean different things depending on the type
     * of datastore, such as cassandra, hbase, etc..)
     *
     * @param objList
     * @param typeName
     */
  def SaveObjectList(objList: Array[BaseElemDef], typeName: String) {
    PersistenceUtils.SaveObjectList(objList,typeName)
  }

    /**
     * SaveObjectList
     * The following batch function is useful when we store data in multiple tables
     * If we use Storage component library, note that each table is associated with a different
     * database connection( which itself can be mean different things depending on the type
     * of datastore, such as cassandra, hbase, etc..)
     *
     * @param objList
     */
  def SaveObjectList(objList: Array[BaseElemDef]) {
    PersistenceUtils.SaveObjectList(objList)
  }

    /**
     * SaveOutputMsObjectList
      *
      * @param objList <description please>
     */
  def SaveOutputMsObjectList(objList: Array[BaseElemDef]) {
    PersistenceUtils.SaveObjectList(objList, "outputmsgs")
  }

    /**
     * UpdateObject
      *
      * @param key
     * @param value
     * @param typeName
     * @param serializerTyp
     */
  def UpdateObject(key: String, value: Array[Byte], typeName: String, serializerTyp: String) {
     PersistenceUtils.SaveObject(key, value, typeName, serializerTyp)
  }

    /**
     * ZooKeeperMessage
      *
      * @param objList
     * @param operations
     * @return
     */
  def ZooKeeperMessage(objList: Array[BaseElemDef], operations: Array[String], cfgList: Map[String,Any] = null, tid: Long = 0): Array[Byte] = {
    try {
      if (!objList.isEmpty) {
        val notification = JsonSerializer.zkSerializeObjectListToJson("Notifications", objList, operations)
        return notification.getBytes
      } else {
        if (cfgList != null) {
          val notification = JsonSerializer.zkSerializeConfigToJson(tid, "Notifications", cfgList, operations)
          return notification.getBytes
        } else {
          return null
        }
      }
      val notification = JsonSerializer.zkSerializeObjectListToJson("Notifications", objList, operations)
      notification.getBytes
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw InternalErrorException("Failed to generate a zookeeper message from the objList", e)
      }
    }
  }

   def UpdateTranId (objList:Array[BaseElemDef] ): Unit ={
     PersistenceUtils.UpdateTranId(objList)
   }

    /**
     * NotifyEngine
      *
      * @param objList <description please>
     * @param operations <description please>
     */
  def NotifyEngine(objList: Array[BaseElemDef], operations: Array[String]) {
    try {
      val notifyEngine = GetMetadataAPIConfig.getProperty("NOTIFY_ENGINE")

      if (objList.size == 0) return

      // We have to update the currentTranLevel here, since addObjectToCache method does not have the tranId in object
      // yet (a bug that is being ractified now)...  We can remove this code when that is fixed.
      var max: Long = 0
      objList.foreach(obj => {
        max = scala.math.max(max, obj.TranId)
      })

      if (currentTranLevel < max) currentTranLevel = max

      if (notifyEngine != "YES") {
        logger.warn("Not Notifying the engine about this operation because The property NOTIFY_ENGINE is not set to YES")
        PersistenceUtils.PutTranId(max)
        return
      }

      // Set up the cache of CACHES!!!! Since we are listening for changes to Metadata, we will be notified by Zookeeper
      // of this change that we are making.  This cache of Caches will tell us to ignore this.
      var corrId: Int = 0
      objList.foreach(elem => {
        cacheOfOwnChanges.add((operations(corrId) + "." + elem.NameSpace + "." + elem.Name + "." + elem.Version).toLowerCase)
        corrId = corrId + 1
      })

      val data = ZooKeeperMessage(objList, operations)
      InitZooKeeper
      val znodePath = GetMetadataAPIConfig.getProperty("ZNODE_PATH") + "/metadataupdate"
      logger.debug("Set the data on the zookeeper node " + znodePath)
      zkc.setData().forPath(znodePath, data)

      PersistenceUtils.PutTranId(max)
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw InternalErrorException("Failed to notify a zookeeper message from the objectList", e)
      }
    }
  }

  def GetMetadataId(key: String, incrementInDb: Boolean, defaultId: Long = 1): Long = {
    PersistenceUtils.GetMetadataId(key, incrementInDb, defaultId)
  }

  def PutMetadataId(key: String, id: Long) = {
    PersistenceUtils.PutMetadataId(key, id)
  }

    /**
     * GetNewTranId
      *
      * @return <description please>
     */
  def GetNewTranId: Long = {
    PersistenceUtils.GetNewTranId
  }

    /**
     * GetTranId
      *
      * @return <description please>
     */
  def GetTranId: Long = {
    PersistenceUtils.GetTranId
  }

    /**
     * PutTranId
      *
      * @param tId <description please>
     */
  def PutTranId(tId: Long) = {
    PersistenceUtils.PutTranId(tId)
  }

    /**
     * SaveObject
      *
      * @param obj <description please>
     * @param mdMgr the metadata manager receiver
     * @return <description please>
     */
  def SaveObject(obj: BaseElemDef, mdMgr: MdMgr): Boolean = {
    try {
      val key = (getObjectType(obj) + "." + obj.FullNameWithVer).toLowerCase
      val dispkey = (getObjectType(obj) + "." + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version)).toLowerCase
      obj.tranId = GetNewTranId


      logger.debug("Serialize the object: name of the object => " + dispkey)
      var value =MetadataAPISerialization.serializeObjectToJson(obj).getBytes //serializer.SerializeObjectToByteArray(obj)

      val saveObjFn = () => {
        PersistenceUtils.SaveObject(key, value, getMdElemTypeName(obj), serializerType) // Make sure getMdElemTypeName is success full all types we handle here
      }

      obj match {
        case o: ModelDef => {
          logger.debug("Adding the model to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddModelDef(o, false)
        }
        case o: MessageDef => {
          logger.debug("Adding the message to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddMsg(o)
        }
        case o: ContainerDef => {
          logger.debug("Adding the container to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddContainer(o)
        }
        case o: FunctionDef => {
          val funcKey = (obj.getClass().getName().split("\\.").last + "." + o.typeString).toLowerCase
          logger.debug("Adding the function to the cache: name of the object =>  " + funcKey)
          saveObjFn()
          mdMgr.AddFunc(o)
        }
        case o: AttributeDef => {
          logger.debug("Adding the attribute to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddAttribute(o)
        }
        case o: ScalarTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddScalar(o)
        }
        case o: ArrayTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddArray(o)
        }
        case o: MapTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddMap(o)
        }
        case o: ContainerTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          saveObjFn()
          mdMgr.AddContainerType(o)
        }
        case _ => {
          logger.error("SaveObject is not implemented for objects of type " + obj.getClass.getName)
        }
      }
      true
    } catch {
      case e: AlreadyExistsException => {
        logger.error("Failed to Save the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
        false
      }
      case e: Exception => {
        logger.error("Failed to Save the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
        false
      }
    }
  }

    /**
     * UpdateObjectInDB
      *
      * @param obj <description please>
     */
  def UpdateObjectInDB(obj: BaseElemDef) {
    try {
      val key = (getObjectType(obj) + "." + obj.FullNameWithVer).toLowerCase
      val dispkey = (getObjectType(obj) + "." + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version)).toLowerCase

      logger.debug("Serialize the object: name of the object => " + dispkey)
      var value = MetadataAPISerialization.serializeObjectToJson(obj).getBytes//serializer.SerializeObjectToByteArray(obj)

      val updObjFn = () => {
        PersistenceUtils.UpdateObject(key, value, getMdElemTypeName(obj), serializerType) // Make sure getMdElemTypeName is success full all types we handle here
      }

      obj match {
        case o: ModelDef => {
          logger.debug("Updating the model in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: MessageDef => {
          logger.debug("Updating the message in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: ContainerDef => {
          logger.debug("Updating the container in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: FunctionDef => {
          val funcKey = (obj.getClass().getName().split("\\.").last + "." + o.typeString).toLowerCase
          logger.debug("Updating the function in the DB: name of the object =>  " + funcKey)
          updObjFn()
        }
        case o: AttributeDef => {
          logger.debug("Updating the attribute in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: ScalarTypeDef => {
          logger.debug("Updating the Type in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: ArrayTypeDef => {
          logger.debug("Updating the Type in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: MapTypeDef => {
          logger.debug("Updating the Type in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case o: ContainerTypeDef => {
          logger.debug("Updating the Type in the DB: name of the object =>  " + dispkey)
          updObjFn()
        }
        case _ => {
          logger.error("UpdateObject is not implemented for objects of type " + obj.getClass.getName)
        }
      }
    } catch {
      case e: AlreadyExistsException => {
        logger.error("Failed to Update the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
      case e: Exception => {
        logger.error("Failed to Update the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
    }
  }

    /**
     * GetJarAsArrayOfBytes
      *
      * @param jarName <description please>
     * @return <description please>
     */
  def GetJarAsArrayOfBytes(jarName: String): Array[Byte] = {
    try {
      val iFile = new File(jarName)
      if (!iFile.exists) {
        logger.error("Jar file (" + jarName + ") is not found: ")
        throw new FileNotFoundException("Jar file (" + jarName + ") is not found: ")
      }
      val bis = new BufferedInputStream(new FileInputStream(iFile));
      val baos = new ByteArrayOutputStream();
      var readBuf = new Array[Byte](1024) // buffer size
      // read until a single byte is available
      while (bis.available() > 0) {
        val c = bis.read();
        baos.write(c)
      }
      bis.close();
      baos.toByteArray()
    } catch {
      case e: IOException => {
        logger.error("", e)
        throw new FileNotFoundException("Failed to Convert the Jar (" + jarName + ") to array of bytes. Message:" + e.getMessage())
      }
      case e: Exception => {
        logger.error("", e)
        throw InternalErrorException("Failed to Convert the Jar (" + jarName + ") to array of bytes", e)
      }
    }
  }

    /**
     * PutArrayOfBytesToJar
      *
      * @param ba <description please>
     * @param jarName <description please>
     */
  def PutArrayOfBytesToJar(ba: Array[Byte], jarName: String) = {
    logger.info("Downloading the jar contents into the file " + jarName)
    try {
      val iFile = new File(jarName)
      val bos = new BufferedOutputStream(new FileOutputStream(iFile));
      bos.write(ba)
      bos.close();
    } catch {
      case e: Exception => {
        logger.error("Failed to dump array of bytes to the Jar file (" + jarName + "):  " + e.getMessage(), e)
      }
    }
  }

    /**
     * UploadJarsToDB
      *
      * @param obj <description please>
     * @param forceUploadMainJar <description please>
     * @param alreadyCheckedJars <description please>
    */
  def UploadJarsToDB(obj: BaseElemDef, forceUploadMainJar: Boolean = true, alreadyCheckedJars: scala.collection.mutable.Set[String] = null): Unit = {
    PersistenceUtils.UploadJarsToDB(obj,forceUploadMainJar,alreadyCheckedJars)
  }

    /**
     * UploadJarToDB
      *
      * @param jarName <description please>
     */
  def UploadJarToDB(jarName: String) {
    PersistenceUtils.UploadJarToDB(jarName)
  }

    /**
     * UploadJarToDB
      *
      * @param jarName <description please>
     * @param byteArray <description please>
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return <description please>
     */
  def UploadJarToDB(jarName: String, byteArray: Array[Byte], userid: Option[String] = None): String = {
    PersistenceUtils.UploadJarToDB(jarName,byteArray,userid)
  }

    /**
     * GetDependantJars of some base element (e.g., model, type, message, container, etc)
      *
      * @param obj <description please>
     * @return <description please>
     */
  def GetDependantJars(obj: BaseElemDef): Array[String] = {
    try {
      var allJars = new Array[String](0)
      if (obj.JarName != null)
        allJars = allJars :+ obj.JarName
      if (obj.DependencyJarNames != null) {
        obj.DependencyJarNames.foreach(j => {
          if (j.endsWith(".jar")) {
            allJars = allJars :+ j
          }
        })
      }
      allJars
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw InternalErrorException("Failed to get dependant jars for the given object (" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
    }
  }

    /**
     * IsDownloadNeeded
      *
      * @param jar <description please>
     * @param obj <description please>
     * @return <description please>
     */
  def IsDownloadNeeded(jar: String, obj: BaseElemDef): Boolean = {
    try {
      if (jar == null) {
        logger.debug("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " has no jar associated with it. Nothing to download..")
        false
      }
      val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
      val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
      val jarName = com.ligadata.Utils.Utils.GetValidJarFile(jarPaths, jar)
      val f = new File(jarName)
      if (f.exists()) {
        val key = jar
        val mObj = GetObject(key, "jar_store")
        val ba = mObj._2.asInstanceOf[Array[Byte]]
        val fs = f.length()
        if (fs != ba.length) {
          logger.debug("A jar file already exists, but it's size (" + fs + ") doesn't match with the size of the Jar (" +
            jar + "," + ba.length + ") of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")")
          true
        } else {
          logger.debug("A jar file already exists, and it's size (" + fs + ")  matches with the size of the existing Jar (" +
            jar + "," + ba.length + ") of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + "), no need to download.")
          false
        }
      } else {
        logger.debug("The jar " + jarName + " is not available, download from database. ")
        true
      }
    } catch {
      case e: ObjectNotFoundException => {
        logger.debug("", e)
        true
      }
      case e: Exception => {
        logger.debug("", e)
        throw InternalErrorException("Failed to verify whether a download is required for the jar " + jar + " of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
    }
  }
    /**
     * DownloadJarFromDB
      *
      * @param obj <description please>
     */
  def DownloadJarFromDB(obj: BaseElemDef) {
    var curJar: String = ""
    try {
      //val key:String = (getObjectType(obj) + "." + obj.FullNameWithVer).toLowerCase
      if (obj.jarName == null) {
        logger.debug("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " has no jar associated with it. Nothing to download..")
        return
      }
      var allJars = GetDependantJars(obj)
      logger.debug("Found " + allJars.length + " dependent jars. Jars:" + allJars.mkString(","))
      logger.info("Found " + allJars.length + " dependent jars. It make take several minutes first time to download all of these jars:" + allJars.mkString(","))
      if (allJars.length > 0) {
        val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
        val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
        jarPaths.foreach(jardir => {
          val dir = new File(jardir)
          if (!dir.exists()) {
            // attempt to create the missing directory
            dir.mkdir();
          }
        })

        val dirPath = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_TARGET_DIR")
        val dir = new File(dirPath)
        if (!dir.exists()) {
          // attempt to create the missing directory
          dir.mkdir();
        }

        allJars.foreach(jar => {
          if (jar != null && jar.trim.size > 0 && PersistenceUtils.excludeSystemJars.contains(jar.trim) == false) {
            curJar = jar
            try {
              // download only if it doesn't already exists
              val b = IsDownloadNeeded(jar, obj) // Already checking for PersistenceUtils.excludeSystemJars.contains(jar.trim) == false
              if (b == true) {
                val key = jar
                val mObj = GetObject(key, "jar_store")
                val ba = mObj._2.asInstanceOf[Array[Byte]]
                val jarName = dirPath + "/" + jar
                PutArrayOfBytesToJar(ba, jarName)
              } else {
                logger.debug("The jar " + curJar + " was already downloaded... ")
              }
            } catch {
              case e: Exception => {
                logger.error("Failed to download the Jar of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + "'s dep jar " + curJar + ")", e)

              }
            }
          }
        })
      }
    } catch {
      case e: Exception => {
        logger.error("Failed to download the Jar of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + "'s dep jar " + curJar + ")", e)

      }
    }
  }

    /**
     * UpdateObjectInCache
      *
      * @param obj <description please>
     * @param operation depending upon object type, operations to add, remove, et al
     * @param mdMgr the metadata manager receiver
     * @return <description please>
     */
  def UpdateObjectInCache(obj: BaseElemDef, operation: String, mdMgr: MdMgr): BaseElemDef = {
    var updatedObject: BaseElemDef = null

    // Update the current transaction level with this object  ???? What if an exception occurs ????
    if (currentTranLevel < obj.TranId) currentTranLevel = obj.TranId

    try {
      obj match {
        case o: FunctionDef => {
          updatedObject = mdMgr.ModifyFunction(o.nameSpace, o.name, o.ver, operation)
        }
        case o: ModelDef => {
          updatedObject = mdMgr.ModifyModel(o.nameSpace, o.name, o.ver, operation)
        }
        case o: MessageDef => {
          updatedObject = mdMgr.ModifyMessage(o.nameSpace, o.name, o.ver, operation)
        }
        case o: ContainerDef => {
          updatedObject = mdMgr.ModifyContainer(o.nameSpace, o.name, o.ver, operation)
        }
        case o: AttributeDef => {
          updatedObject = mdMgr.ModifyAttribute(o.nameSpace, o.name, o.ver, operation)
        }
        case o: ScalarTypeDef => {
          updatedObject = mdMgr.ModifyType(o.nameSpace, o.name, o.ver, operation)
        }
        case o: ArrayTypeDef => {
          updatedObject = mdMgr.ModifyType(o.nameSpace, o.name, o.ver, operation)
        }
        case o: MapTypeDef => {
          updatedObject = mdMgr.ModifyType(o.nameSpace, o.name, o.ver, operation)
        }
        case o: ContainerTypeDef => {
          updatedObject = mdMgr.ModifyType(o.nameSpace, o.name, o.ver, operation)
        }
        case _ => {
          throw InternalErrorException("UpdateObjectInCache is not implemented for objects of type " + obj.getClass.getName, null)
        }
      }
      updatedObject
    } catch {
      case e: ObjectNolongerExistsException => {
        logger.debug("", e)
        throw ObjectNolongerExistsException("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " nolonger exists in metadata : It may have been removed already", e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw e
      }
    }
  }

  // For now only handle the Model COnfig... Engine Configs will come later
    /**
     * AddConfigObjToCache
      *
      * @param tid <description please>
     * @param key <description please>
     * @param mdlConfig <description please>
     *  @param mdMgr the metadata manager receiver
     */
  def AddConfigObjToCache(tid: Long, key: String, mdlConfig: Map[String, List[String]], mdMgr: MdMgr) {
    // Update the current transaction level with this object  ???? What if an exception occurs ????
    if (currentTranLevel < tid) currentTranLevel = tid
    try {
      mdMgr.AddModelConfig(key, mdlConfig)
    } catch { //Map[String, List[String]]
      case e: AlreadyExistsException => {
        logger.error("Failed to Cache the config object(" + key + ")", e)
      }
      case e: Exception => {
        logger.error("Failed to Cache the config object(" + key + ")", e)
      }
    }
  }

    /**
     * AddObjectToCache
      *
      * @param o <description please>
     *  @param mdMgr the metadata manager receiver
     */
  def AddObjectToCache(o: Object, mdMgr: MdMgr) {
    // If the object's Delete flag is set, this is a noop.
    val obj = o.asInstanceOf[BaseElemDef]

    // Update the current transaction level with this object  ???? What if an exception occurs ????
    if (currentTranLevel < obj.TranId) currentTranLevel = obj.TranId

    if (obj.IsDeleted)
      return
    try {
      val key = obj.FullNameWithVer.toLowerCase
      val dispkey = obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version)
      obj match {
        case o: ModelDef => {
          logger.debug("Adding the model to the cache: name of the object =>  " + dispkey)
          mdMgr.AddModelDef(o, true)
        }
        case o: MessageDef => {
          logger.debug("Adding the message to the cache: name of the object =>  " + dispkey)
          mdMgr.AddMsg(o)
        }
        case o: ContainerDef => {
          logger.debug("Adding the container to the cache: name of the object =>  " + dispkey)
          mdMgr.AddContainer(o)
        }
        case o: FunctionDef => {
          val funcKey = o.typeString.toLowerCase
          logger.debug("Adding the function to the cache: name of the object =>  " + funcKey)
          mdMgr.AddFunc(o)
        }
        case o: AttributeDef => {
          logger.debug("Adding the attribute to the cache: name of the object =>  " + dispkey)
          mdMgr.AddAttribute(o)
        }
        case o: ScalarTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          mdMgr.AddScalar(o)
        }
        case o: ArrayTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          mdMgr.AddArray(o)
        }
        case o: MapTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          mdMgr.AddMap(o)
        }
        case o: ContainerTypeDef => {
          logger.debug("Adding the Type to the cache: name of the object =>  " + dispkey)
          mdMgr.AddContainerType(o)
        }
        case _ => {
          logger.error("SaveObject is not implemented for objects of type " + obj.getClass.getName)
        }
      }
    } catch {
      case e: AlreadyExistsException => {
        logger.error("Already Exists! Failed to Cache the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
      case e: Exception => {
        logger.error("Exception! Failed to Cache the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
    }
  }

    /**
     * ModifyObject
      *
      * @param obj
     * @param operation
     */
  def ModifyObject(obj: BaseElemDef, operation: String) {
    try {
      val o1 = UpdateObjectInCache(obj, operation, MdMgr.GetMdMgr)
      UpdateObjectInDB(o1)
    } catch {
      case e: ObjectNolongerExistsException => {
        logger.error("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " nolonger exists in metadata : It may have been removed already", e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw new Exception("Unexpected error in ModifyObject", e)
      }
    }
  }

    /**
     * DeleteObject
     *
     * @param bucketKeyStr
     * @param typeName
     */
  def DeleteObject(bucketKeyStr: String, typeName: String) {
    PersistenceUtils.DeleteObject(bucketKeyStr,typeName)
  }

    /**
     * DeleteObject
      *
      * @param obj
     */
  def DeleteObject(obj: BaseElemDef) {
    try {
      ModifyObject(obj, "Remove")
    } catch {
      case e: ObjectNolongerExistsException => {
        logger.error("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " nolonger exists in metadata : It may have been removed already", e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw new Exception("Unexpected error in DeleteObject", e)
      }
    }
  }

    /**
     * ActivateObject
      *
      * @param obj
     */
  def ActivateObject(obj: BaseElemDef) {
    try {
      ModifyObject(obj, "Activate")
    } catch {
      case e: ObjectNolongerExistsException => {
        logger.debug("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " nolonger exists in metadata : It may have been removed already", e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw new Exception("Unexpected error in ActivateObject", e)
      }
    }
  }

    /**
     * DeactivateObject
      *
      * @param obj
     */
  def DeactivateObject(obj: BaseElemDef) {
    try {
      ModifyObject(obj, "Deactivate")
    } catch {
      case e: ObjectNolongerExistsException => {
        logger.error("The object " + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + " nolonger exists in metadata : It may have been removed already", e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw new Exception("Unexpected error in DeactivateObject", e)
      }
    }
  }

    /**
     * getApiResult
      *
      * @param apiResultJson
     * @return
     */
  @throws(classOf[Json4sParsingException])
  @throws(classOf[ApiResultParsingException])
  def getApiResult(apiResultJson: String): String = {
    // parse using Json4s
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(apiResultJson)
      //logger.debug("Parsed the json : " + apiResultJson)
      val apiResultInfo = json.extract[APIResultJsonProxy]
      (apiResultInfo.APIResults.statusCode + apiResultInfo.APIResults.functionName + apiResultInfo.APIResults.resultData + apiResultInfo.APIResults.description)
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ApiResultParsingException(e.getMessage(), e)
      }
    }
  }

  /**
     * OpenDbStore
      *
      * @param jarPaths Set of paths where jars are located
     * @param dataStoreInfo information needed to access the data store (kv store dependent)
     */
  def OpenDbStore(jarPaths: collection.immutable.Set[String], dataStoreInfo: String) {
    PersistenceUtils.OpenDbStore(jarPaths,dataStoreInfo)
  }

  /**
     * CreateMetadataTables
     *
     */
  def CreateMetadataTables: Unit = {
    PersistenceUtils.CreateMetadataTables
  }

    /**
     * CloseDbStore
     */
  def CloseDbStore: Unit = lock.synchronized {
    PersistenceUtils.CloseDbStore
  }

    /**
     * TruncateDbStore
     */
  def TruncateDbStore: Unit = lock.synchronized {
    PersistenceUtils.TruncateDbStore
  }

    /**
     * TruncateAuditStore
     */
  def TruncateAuditStore: Unit = lock.synchronized {
    PersistenceUtils.TruncateAuditStore
  }

    /**
     * AddType
      *
      * @param typeText
     * @param format
     * @return
     */
  def AddType(typeText: String, format: String, userid: Option[String] = None): String = {
    TypeUtils.AddType(typeText,format)
  }

    /**
     * AddType
      *
      * @param typeDef
     * @return
     */
  def AddType(typeDef: BaseTypeDef): String = {
    TypeUtils.AddType(typeDef)
  }

    /**
     * AddTypes
      *
      * @param typesText
     * @param format
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def AddTypes(typesText: String, format: String, userid: Option[String] = None): String = {
    TypeUtils.AddTypes(typesText,format,userid)
  }

  /**
    * Remove type for given TypeName and Version
    *
    * @param typeNameSpace
    * @param typeName name of the Type
    * @param version  Version of the object
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *
    *         {{{
    *          val apiResult = MetadataAPIImpl.RemoveType(MdMgr.sysNS,"my_char",100)
    *          val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
    *          println("Result as Json String => \n" + resultData)
    *          }}}
    *
    */
  def RemoveType(typeNameSpace: String, typeName: String, version: Long, userid: Option[String] = None): String = {
    TypeUtils.RemoveType(typeNameSpace,typeName,version,userid)
  }

   /**
    * UpdateType
     *
     * @param typeJson
    * @param format
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *
    *         {{{
    *          val sampleScalarTypeStr = """
    *          {
    * "MetadataType" : "ScalarTypeDef",
    * "NameSpace" : "system",
    * "Name" : "my_char",
    * "TypeTypeName" : "tScalar",
    * "TypeNameSpace" : "System",
    * "TypeName" : "Char",
    * "PhysicalName" : "Char",
    * "Version" : 101,
    * "JarName" : "basetypes_2.10-0.1.0.jar",
    * "DependencyJars" : [ "metadata_2.10-1.0.jar" ],
    * "Implementation" : "com.ligadata.BaseTypes.CharImpl"
    * }
    * """
    * var apiResult = MetadataAPIImpl.UpdateType(sampleScalarTypeStr,"JSON")
    * var result = MetadataAPIImpl.getApiResult(apiResult)
    * println("Result as Json String => \n" + result._2)
    * }}}
    *
    */
  def UpdateType(typeJson: String, format: String, userid: Option[String] = None): String = {
    TypeUtils.UpdateType(typeJson,format,userid)
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    /**
     * Upload Jars into system. Dependency jars may need to upload first. Once we upload the jar, if we retry to upload it will throw an exception.
      *
      * @param jarPath where the jars are
     * @return
     */
  def UploadJar(jarPath: String, userid: Option[String] = None): String = {
    try {
      val iFile = new File(jarPath)
      if (!iFile.exists) {
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadJar", null, ErrorCodeConstants.File_Not_Found + ":" + jarPath)
        apiResult.toString()
      } else {
        val jarName = iFile.getName()
        val ownerId: String = if (userid == None) "kamanja" else userid.get
        val uniqueId = MetadataAPIImpl.GetUniqueId
        val mdElementId = 0L //FIXME:- Not yet handled this
        val jarObject = MdMgr.GetMdMgr.MakeJarDef(MetadataAPIImpl.sysNS, jarName, "100", ownerId, "" /* For now Jars Tenant is empty */, uniqueId, mdElementId)


        logger.debug(" UploadJar  ==>>    ===>> " + jarPath )
        jarObject.tranId = GetNewTranId

        var objectsAdded = new Array[BaseElemDef](0)
        objectsAdded = objectsAdded :+ jarObject
        UploadJarToDB(jarPath)
        val operations = for (op <- objectsAdded) yield "Add"
        NotifyEngine(objectsAdded, operations)
        val apiResult = new ApiResult(ErrorCodeConstants.Success, "UploadJar", null, ErrorCodeConstants.Upload_Jar_Successful + ":" + jarPath)
        apiResult.toString()
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        logger.debug(stackTrace)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadJar", null, "Error :" + e.toString() + ErrorCodeConstants.Upload_Jar_Failed + ":" + jarPath + "\nStackTrace:" + stackTrace)
        apiResult.toString()
      }
    }
  }

    /** '
      * AddDerivedConcept
      *
      * @param conceptsText
      * @param format
      * @return
      */
  def AddDerivedConcept(conceptsText: String, format: String): String = {
    ConceptUtils.AddDerivedConcept(conceptsText, format)
  }

    /**
    * AddConcepts
      *
      * @param conceptsText an input String of concepts in a format defined by the next parameter formatType
    * @param format
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *
    *         {{{
    *            val sampleConceptStr = """
    *           {"Concepts" : [
    *  "NameSpace":"Ligadata",
    *  "Name":"ProviderId",
    *  "TypeNameSpace":"System",
    *  "TypeName" : "String",
    *  "Version"  : 100 ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.AddConcepts(sampleConceptStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *}}}
    *
    */
  def AddConcepts(conceptsText: String, format: String, userid: Option[String] = None): String = {
    ConceptUtils.AddConcepts(conceptsText,format,userid)
  }

    /**
    * UpdateConcepts
      *
      * @param conceptsText an input String of concepts in a format defined by the next parameter formatType
    * @param format
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *
    *         {{{
    *            val sampleConceptStr = """
    *           {"Concepts" : [
    *  "NameSpace":"Ligadata",
    *  "Name":"ProviderId",
    *  "TypeNameSpace":"System",
    *  "TypeName" : "String",
    *  "Version"  : 101 ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.UpdateConcepts(sampleConceptStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *
    *}}}
    *
    */
  def UpdateConcepts(conceptsText: String, format: String, userid: Option[String] = None): String = {
    ConceptUtils.UpdateConcepts(conceptsText,format,userid)
  }

    /**
     * RemoveConcept
      *
      * @param key
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def RemoveConcept(key: String, userid: Option[String] = None): String = {
    ConceptUtils.RemoveConcept(key,userid)
  }

    /**
     * RemoveConcept
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def RemoveConcept(nameSpace: String, name: String, version: Long, userid: Option[String]): String = {
    ConceptUtils.RemoveConcept(nameSpace, name, version, userid)
  }

    /**
     * RemoveConcepts take all concepts names to be removed as an Array
      *
      * @param concepts array of Strings where each string is name of the concept
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     *
     *         Example:
     *         {{{
     *          val apiResult = MetadataAPIImpl.RemoveConcepts(Array("Ligadata.ProviderId.100"))
     *          val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
     *          println("Result as Json String => \n" + resultData)
     *         }}}
     *
     */

  def RemoveConcepts(concepts: Array[String], userid: Option[String] = None): String = {
    ConceptUtils.RemoveConcepts(concepts,userid)
  }

    /**
     * AddContainerDef
      *
      * @param contDef
     * @param recompile
     * @return
     */
  def AddContainerDef(contDef: ContainerDef, recompile: Boolean = false): String = {
    MessageAndContainerUtils.AddContainerDef(contDef,recompile)
  }

    /**
     * AddMessageDef
      *
      * @param msgDef
     * @param recompile
     * @return
     */
  def AddMessageDef(msgDef: MessageDef, recompile: Boolean = false): String = {
    MessageAndContainerUtils.AddMessageDef(msgDef,recompile)
  }

    /**
     * AddMessageTypes
      *
      * @param msgDef
     * @param mdMgr the metadata manager receiver
     * @param recompile
     * @return
     */
  def AddMessageTypes(msgDef: BaseElemDef, mdMgr: MdMgr, recompile: Boolean = false): Array[BaseElemDef] = {
    MessageAndContainerUtils.AddMessageTypes(msgDef,mdMgr,recompile)
  }

    /**
     * AddContainerOrMessage
      *
      * @param contOrMsgText message
     * @param format its format
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param recompile a
     * @return <description please>
     */
  private def AddContainerOrMessage(contOrMsgText: String, format: String, userid: Option[String], tenantId: Option[String] = None, recompile: Boolean = false): String = {
    MessageAndContainerUtils.AddContainerOrMessage(contOrMsgText,format,userid, tenantId, recompile)
  }

    /**
     * AddMessage
      *
      * @param messageText text of the message (as JSON/XML string as defined by next parameter formatType)
     * @param format
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     *
     *         Example
     *
     *         {{{
     *          var apiResult = MetadataAPIImpl.AddMessage(msgStr,"JSON")
     *          var result = MetadataAPIImpl.getApiResult(apiResult)
     *          println("Result as Json String => \n" + result._2)
     *          }}}
     */
  override def AddMessage(messageText: String, format: String, userid: Option[String] = None, tid: Option[String] = None): String = {
    AddContainerOrMessage(messageText, format, userid, tid)
  }

    /**
    * AddContainer
      *
      * @param containerText text of the container (as JSON/XML string as defined by next parameter formatType)
    * @param format
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example
    *
    *         {{{
    *          var apiResult = MetadataAPIImpl.AddContainer(msgStr,"JSON")
    *          var result = MetadataAPIImpl.getApiResult(apiResult)
    *          println("Result as Json String => \n" + result._2)
    *          }}}
    */
  override def AddContainer(containerText: String, format: String, userid: Option[String] = None, tenantId: Option[String] = None): String = {
    AddContainerOrMessage(containerText, format, userid, tenantId)
  }

    /**
     * AddContainer
      *
      * @param containerText
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
 // def AddContainer(containerText: String, userid: Option[String], tenantId: Option[String] = None): String = {
 //   AddContainer(containerText, "JSON", userid, tenantId)
 // }

    /**
     * RecompileMessage
      *
      * @param msgFullName
     * @return
     */
  def RecompileMessage(msgFullName: String): String = {
    MessageAndContainerUtils.RecompileMessage(msgFullName)
  }

    /**
     * UpdateMessage
      *
      * @param messageText text of the message (as JSON/XML string as defined by next parameter formatType)
     * @param format
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     */
  override def UpdateMessage(messageText: String, format: String, userid: Option[String] = None, tid: Option[String] = None): String = {
    MessageAndContainerUtils.UpdateMessage(messageText,format,userid, tid)
  }

    /**
     * UpdateContainer
      *
      * @param messageText
     * @param format
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     */
  override def UpdateContainer(messageText: String, format: String, userid: Option[String] = None, tid: Option[String] = None): String = {
    UpdateMessage(messageText, format, userid)
  }

    /**
     * UpdateContainer
      *
      * @param messageText
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
 // def UpdateContainer(messageText: String, userid: Option[String]): String = {
 //   UpdateMessage(messageText, "JSON", userid)
 // }

    /**
     * UpdateMessage
      *
      * @param messageText
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  //def UpdateMessage(messageText: String, userid: Option[String]): String = {
  //  UpdateMessage(messageText, "JSON", userid)
 // }

    /**
     * Remove container with Container Name and Version Number
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param zkNotify
     * @return
     */
  def RemoveContainer(nameSpace: String, name: String, version: Long, userid: Option[String], zkNotify: Boolean = true): String = {
    MessageAndContainerUtils.RemoveContainer(nameSpace,name,version,userid,zkNotify)
  }

    /**
     * Remove message with Message Name and Version Number
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @param zkNotify
     * @return
     */
  def RemoveMessage(nameSpace: String, name: String, version: Long, userid: Option[String], zkNotify: Boolean = true): String = {
    MessageAndContainerUtils.RemoveMessage(nameSpace,name,version,userid,zkNotify)
  }

    /**
     * When a message or container is compiled, the MetadataAPIImpl will automatically catalog an array, array buffer,
     * sorted set, immutable map of int array, array of array, et al where the message or container is a member element.
     * The type names are of the form <collectiontype>of<message type>.  Currently these container names are created:
     *
     *   {{{
     *       arrayof<message type>
     *       arraybufferof<message type>
     *       sortedsetof<message type>
     *       immutablemapofintarrayof<message type>
     *       immutablemapofstringarrayof<message type>
     *       arrayofarrayof<message type>
     *       mapofstringarrayof<message type>
     *       mapofintarrayof<message type>
     *       setof<message type>
     *       treesetof<message type>
     *   }}}
      *
      * @param msgDef the name of the msgDef's type is used for the type name formation
     * @param mdMgr the metadata manager receiver
     * @return <description please>
     */
  def GetAdditionalTypesAdded(msgDef: BaseElemDef, mdMgr: MdMgr): Array[BaseElemDef] = {
    MessageAndContainerUtils.GetAdditionalTypesAdded(msgDef,mdMgr)
  }

    /**
     * Remove message with Message Name and Version Number based upon advice in supplied notification
      *
      * @param zkMessage
     * @return
     */
  def RemoveMessageFromCache(zkMessage: ZooKeeperNotification) = {
    MessageAndContainerUtils.RemoveMessageFromCache(zkMessage)
  }

    /**
     * RemoveContainerFromCache
      *
      * @param zkMessage
     * @return
     */
  def RemoveContainerFromCache(zkMessage: ZooKeeperNotification) = {
    MessageAndContainerUtils.RemoveContainerFromCache(zkMessage)
  }

    /**
     * Remove message with Message Name and Version Number
      *
      * @param messageName Name of the given message
     * @param version  Version of the given message
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value should be other than None
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     */
  def RemoveMessage(messageName: String, version: Long, userid: Option[String]): String = {
    RemoveMessage(sysNS, messageName, version, userid)
  }

   /**
    * Remove container with Container Name and Version Number
     *
     * @param containerName Name of the given container
    * @param version  Version of the object   Version of the given container
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
  def RemoveContainer(containerName: String, version: Long, userid: Option[String]): String = {
    RemoveContainer(sysNS, containerName, version, userid)
  }

    /**
     * Deactivate the model that presumably is active and waiting for input in the working set of the cluster engines.
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def DeactivateModel(nameSpace: String, name: String, version: Long, userid: Option[String] = None): String = {
    ModelUtils.DeactivateModel(nameSpace,name,version,userid)
  }

    /**
     * Activate the model with the supplied keys. The engine is notified and the model factory is loaded.
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def ActivateModel(nameSpace: String, name: String, version: Long, userid: Option[String] = None): String = {
    ModelUtils.ActivateModel(nameSpace,name,version,userid)
  }

   /**
    * Remove model with Model Name and Version Number
     *
     * @param modelName the Namespace.Name of the given model to be removed
    * @param version   Version of the given model.  The version should comply with the Kamanja version format.  For example,
    *                  a value of "000001.000001.000001" shows the digits available for version.  All must be base 10 digits
    *                  with up to 6 digits for major version, minor version and micro version sections.
    *                  elper functions are available in MdMgr object for converting to/from strings and 0 padding the
    *                  version sections if desirable.
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    */
    override def RemoveModel(modelName: String, version: String, userid: Option[String] = None): String = {
      ModelUtils.RemoveModel(modelName,version,userid)
    }


    /**
     * The ModelDef returned by the compilers is added to the metadata.
      *
      * @param model
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured,supply something other than None
     * @return
     */
  def AddModel(model: ModelDef, userid : Option[String]): String = {
    ModelUtils.AddModel(model,userid)
  }

    /** Add model. Several model types are currently supported.  They describe the content of the ''input'' argument:
      *
      *   - SCALA - a Scala source string
      *   - JAVA - a Java source string
      *   - PMML - a PMML source string
      *   - KPMML - a Kamanja Pmml source string
      *   - JTM - a JSON string for a Jason Transformation Model
      *   - BINARY - the path to a jar containing the model
      *
      * The remaining arguments, while noted as optional, are required for some model types.  In particular,
      * the ''modelName'', ''version'', and ''msgConsumed'' must be specified for the PMML model type.  The ''userid'' is
      * required for systems that have been configured with a SecurityAdapter or AuditAdapter.
      *
      * @see [[http://kamanja.org/security/ security wiki]] for more information. The audit adapter, if configured,
      *       will also be invoked to take note of this user's action.
      * @see [[http://kamanja.org/auditing/ auditing wiki]] for more information about auditing.
      * NOTE: The BINARY model is not supported at this time.  The model submitted for this type will via a jar file.
      * @param modelType the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY}
      * @param input the text element to be added dependent upon the modelType specified.
      * @param optUserid the identity to be used by the security adapter to ascertain if this user has access permissions for this
      *               method.
      * @param optModelName the namespace.name of the PMML model to be added to the Kamanja metadata
      * @param optVersion the model version to be used to describe this PMML model
      * @param optMsgConsumed the namespace.name of the message to be consumed by a PMML model
      * @param optMsgVersion the version of the message to be consumed. By default Some(-1)
      * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
      * indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
      * ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     */
  override def AddModel( modelType: ModelType.ModelType
                           , input: String
                           , optUserid: Option[String] = None
                           , optTenantid: Option[String] = None
                           , optModelName: Option[String] = None
                           , optVersion: Option[String] = None
                           , optMsgConsumed: Option[String] = None
                           , optMsgVersion: Option[String] = Some("-1")
			                     , optMsgProduced: Option[String] = None
		       ): String  = {
    ModelUtils.AddModel(modelType, input, optUserid, optTenantid, optModelName, optVersion, optMsgConsumed, optMsgVersion, optMsgProduced)
  }

    /**
     * Recompile the supplied model. Optionally the message definition is supplied that was just built.
     *
     * @param mod the model definition that possibly needs to be reconstructed.
     * @param userid the user id that has invoked this command
     * @param optMsgDef the MessageDef constructed, assuming it was a message def. If a container def has been rebuilt,
     *               this field will have a value of None.  This is only meaningful at this point when the model to
     *               be rebuilt is a PMML model.
     * @return the result string reflecting what happened with this operation.
     */
    def RecompileModel(mod: ModelDef, userid : Option[String], optMsgDef : Option[MessageDef]): String = {
      ModelUtils.RecompileModel(mod,userid, optMsgDef)
    }

    /**
     * Update the model with new source of type modelType.
     *
     * Except for the modelType and the input, all fields are marked optional. Note, however, that for some of the
     * model types all arguments should have meaningful values.
     *
     * @see AddModel for semantics.
     *
     * Note that the message and message version (as seen in AddModel) are not used.  Should a message change that is being
     * used by one of the PMML models, it will be automatically be updated immediately when the message compilation and
     * metadata update has completed for it.
     *
     * Currently only the most recent model cataloged with the name noted in the source file can be "updated".  It is not
     * possible to have a number of models with the same name differing only by version and be able to update one of them
     * explicitly.  This is a feature that is to be implemented.
     *
     * If both the model and the message are changing, consider using AddModel to create a new PMML model and then remove the older
     * version if appropriate.
      * @param modelType the type of the model submission (any {SCALA,JAVA,PMML,KPMML,BINARY}
     * @param input the text element to be added dependent upon the modelType specified.
     * @param optUserid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method.
     * @param optModelName the namespace.name of the PMML model to be added to the Kamanja metadata (Note: KPMML models extract this from
     *                     the pmml source file header and for this reason is not required for the KPMML model types).
     * @param optVersion the model version to be used to describe this PMML model (for KPMML types this value is obtained from the source file)
     * @param optVersionBeingUpdated not used .. reserved for future release where explicit modelnamespace.modelname.modelversion
     *                               can be updated (not just the latest version)
     * @return  result string indicating success or failure of operation
     */
    override def UpdateModel(modelType: ModelType.ModelType
                            , input: String
                            , optUserid: Option[String] = None
                            , tenantid:  Option[String] = None
                            , optModelName: Option[String] = None
                            , optVersion: Option[String] = None
                            , optVersionBeingUpdated : Option[String] = None
			                      , optMsgProduced: Option[String] = None): String = {
      ModelUtils.UpdateModel(modelType, input, optUserid, tenantid, optModelName, optVersion, optVersionBeingUpdated, optMsgProduced)
    }

    /**
     * GetDependentModels
      *
      * @param msgNameSpace
     * @param msgName
     * @param msgVer
     * @return
     */
  def GetDependentModels(msgNameSpace: String, msgName: String, msgVer: Long): Array[ModelDef] = {
    MessageAndContainerUtils.GetDependentModels(msgNameSpace,msgName,msgVer)
  }

  def GetContainerDefFromCache(nameSpace: String, name: String, formatType: String, version: String, userid: Option[String]): String = {
    MessageAndContainerUtils.GetContainerDefFromCache(nameSpace,name,formatType,version,userid)
  }
    /**
     * Get all available models (format JSON or XML) as string.
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None
     * @return string representation in specified format.
     */
  def GetAllModelDefs(formatType: String, userid: Option[String] = None): String = {
    ModelUtils.GetAllModelDefs(formatType,userid)
  }

    /**
     * GetAllMessageDefs - get all available messages(format JSON or XML) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllMessageDefs(formatType: String, userid: Option[String] = None): String = {
    MessageAndContainerUtils.GetAllMessageDefs(formatType,userid)
  }

  // All available containers(format JSON or XML) as a String
    /**
     * GetAllContainerDefs
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return result as string
     */
  def GetAllContainerDefs(formatType: String, userid: Option[String] = None): String = {
    MessageAndContainerUtils.GetAllContainerDefs(formatType,userid)
  }

    /**
     * GetAllModelsFromCache
      *
      * @param active
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllModelsFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    ModelUtils.GetAllModelsFromCache(active,userid)
  }

    /**
     * GetAllMessagesFromCache
      *
      * @param active
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllMessagesFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    MessageAndContainerUtils.GetAllMessagesFromCache(active,userid)
  }

    /**
     * GetAllContainersFromCache
      *
      * @param active
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllContainersFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    MessageAndContainerUtils.GetAllContainersFromCache(active,userid)
  }

    /**
     * GetAllFunctionsFromCache
      *
      * @param active
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllFunctionsFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    FunctionUtils.GetAllFunctionsFromCache(active,userid)
  }

    /**
     * GetAllConceptsFromCache
      *
      * @param active
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllConceptsFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    ConceptUtils.GetAllConceptsFromCache(active,userid)
  }

    /**
     * GetAllTypesFromCache
      *
      * @param active <description please>
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return <description please>
     */
  def GetAllTypesFromCache(active: Boolean, userid: Option[String] = None): Array[String] = {
    TypeUtils.GetAllTypesFromCache(active,userid)
  }

  // Specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
    /**
     *
     * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetModelDef(nameSpace: String, objectName: String, formatType: String, userid : Option[String]): String = {
    ModelUtils.GetModelDef(nameSpace,objectName,formatType,userid)
  }

    /**
     * Get a specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @return
     */
  def GetModelDef(objectName: String, formatType: String, userid : Option[String] = None): String = {
    GetModelDef(sysNS, objectName, formatType, userid)
  }

    /**
     * Get a specific model (format JSON or XML) as a String using modelName(with version) as the key
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetModelDefFromCache(nameSpace: String, name: String, formatType: String, version: String, userid: Option[String] = None): String = {
    ModelUtils.GetModelDefFromCache(nameSpace,name,formatType,version,userid)
  }

  // Specific models (format JSON or XML) as an array of strings using modelName(without version) as the key
    /**
     *
     * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetModelDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String]): String = {
    logAuditRec(userid
        , Some(AuditConstants.READ)
        , AuditConstants.GETOBJECT
        , AuditConstants.MODEL
        , AuditConstants.SUCCESS
        , ""
        , nameSpace + "." + objectName + "." + version)
    GetModelDefFromCache(nameSpace, objectName, formatType, version, None)
  }

    /**
     * Get the specific message (format JSON or XML) as a String using messageName(with version) as the key
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetMessageDefFromCache(nameSpace: String, name: String, formatType: String, version: String, userid: Option[String] = None): String = {
    MessageAndContainerUtils.GetMessageDefFromCache(nameSpace,name,formatType,version,userid)
  }

    /**
     * Return Specific messageDef object using messageName(with version) as the key
      *
      * @param nameSpace namespace of the object
     * @param name
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @return
     */
  @throws(classOf[ObjectNotFoundException])
  def GetMessageDefInstanceFromCache(nameSpace: String, name: String, formatType: String, version: String): MessageDef = {
    MessageAndContainerUtils.GetMessageDefInstanceFromCache(nameSpace,name,formatType,version)
  }

    /**
     * Check whether model already exists in metadata manager. Ideally,
     * we should never add the model into metadata manager more than once
     * and there is no need to use this function in main code flow
     * This is just a utility function being used during these initial phases
      *
      * @param modDef the model def to be tested
     * @return
     */
  def DoesModelAlreadyExist(modDef: ModelDef): Boolean = {
    ModelUtils.DoesModelAlreadyExist(modDef)
  }

    /**
     * Get the latest model for a given FullName
      *
      * @param modDef
     * @return
     */
  def GetLatestModel(modDef: ModelDef): Option[ModelDef] = {
    ModelUtils.GetLatestModel(modDef)
  }

  //
    /**
     * Get the latest cataloged models from the supplied set
      *
      * @param modelSet
     * @return
     */
  def GetLatestModelFromModels(modelSet: Set[ModelDef]): ModelDef = {
    ModelUtils.GetLatestModelFromModels(modelSet)
  }

    /**
     * GetLatestFunction
      *
      * @param fDef
     * @return
     */
  def GetLatestFunction(fDef: FunctionDef): Option[FunctionDef] = {
    FunctionUtils.GetLatestFunction(fDef)
  }

  // Get the latest message for a given FullName
    /**
     *
     * @param msgDef
     * @return
     */
  def GetLatestMessage(msgDef: MessageDef): Option[MessageDef] = {
    MessageAndContainerUtils.GetLatestMessage(msgDef)
  }

    /**
     * Get the latest container for a given FullName
      *
      * @param contDef
     * @return
     */
  def GetLatestContainer(contDef: ContainerDef): Option[ContainerDef] = {
    MessageAndContainerUtils.GetLatestContainer(contDef)
  }

    /**
     * IsValidVersion
      *
      * @param oldObj
     * @param newObj
     * @return
     */
    def IsValidVersion(oldObj: BaseElemDef, newObj: BaseElemDef): Boolean = {
      if (newObj.ver > oldObj.ver) {
        return true
      } else {
        return false
      }
    }


    /**
     * Check whether message already exists in metadata manager. Ideally,
     * we should never add the message into metadata manager more than once
     * and there is no need to use this function in main code flow
     * This is just a utility function being during these initial phases
      *
      * @param msgDef
     * @return
     */
    def DoesMessageAlreadyExist(msgDef: MessageDef): Boolean = {
        MessageAndContainerUtils.IsMessageAlreadyExists(msgDef)
    }

    /**
     * Check whether message already exists in metadata manager. Ideally,
     * we should never add the message into metadata manager more than once
     * and there is no need to use this function in main code flow
     * This is just a utility function being during these initial phases
      *
      * @param msgDef
     * @return
     */
    def IsMessageAlreadyExists(msgDef: MessageDef): Boolean = {
      MessageAndContainerUtils.IsMessageAlreadyExists(msgDef)
    }

    /**
     * DoesContainerAlreadyExist
      *
      * @param contDef
     * @return
     */
    def DoesContainerAlreadyExist(contDef: ContainerDef): Boolean = {
        MessageAndContainerUtils.IsContainerAlreadyExists(contDef)
    }

    /**
     * IsContainerAlreadyExists
      *
      * @param contDef
     * @return
     */
  def IsContainerAlreadyExists(contDef: ContainerDef): Boolean = {
    MessageAndContainerUtils.IsContainerAlreadyExists(contDef)
  }

    /**
     * DoesConceptAlreadyExist
      *
      * @param attrDef
     * @return
     */
    def DoesConceptAlreadyExist(attrDef: BaseAttributeDef): Boolean = {
        ConceptUtils.IsConceptAlreadyExists(attrDef)
    }

    /**
     * IsConceptAlreadyExists
      *
      * @param attrDef
     * @return
     */
    def IsConceptAlreadyExists(attrDef: BaseAttributeDef): Boolean = {
      ConceptUtils.IsConceptAlreadyExists(attrDef)
    }

    /**
     * Get a specific model definition from persistent store
      *
      * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetModelDefFromDB(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String] = None): String = {
    ModelUtils.GetModelDefFromDB(nameSpace,objectName,formatType,version,userid)
  }

    /**
     * IsTypeObject
      *
      * @param typeName
     * @return
     */
  private def IsTypeObject(typeName: String): Boolean = {
    typeName match {
      case "scalartypedef" | "arraytypedef" | "arraybuftypedef" | "listtypedef" | "settypedef" | "treesettypedef" | "queuetypedef" | "maptypedef" | "immutablemaptypedef" | "hashmaptypedef" | "tupletypedef" | "structtypedef" | "sortedsettypedef" => {
        return true
      }
      case _ => {
        return false
      }
    }
  }

    /**
     * GetAllKeys
      *
      * @param objectType
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllKeys(objectType: String, userid: Option[String] = None): Array[String] = {
    try {
      var keys = scala.collection.mutable.Set[String]()

      // get keys for types "types", "functions", "messages", "containers", "concepts", "models"
      val reqTypes = Array("types", "functions", "messages", "containers", "concepts", "models")
      val processedContainersSet = Set[String]()

      reqTypes.foreach(typ => {
        val storeInfo = PersistenceUtils.GetContainerNameAndDataStore(typ)

        if (processedContainersSet(storeInfo._1) == false) {
          processedContainersSet += storeInfo._1
          storeInfo._2.getKeys(storeInfo._1, { (key: Key) =>
            {
              val strKey = key.bucketKey.mkString(".")
              val i = strKey.indexOf(".")
              val objType = strKey.substring(0, i)
              val typeName = strKey.substring(i + 1)
              objectType match {
                case "TypeDef" => {
                  if (IsTypeObject(objType)) {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.TYPE, AuditConstants.SUCCESS, "", AuditConstants.TYPE)
                }
                case "FunctionDef" => {
                  if (objType == "functiondef") {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.FUNCTION, AuditConstants.SUCCESS, "", AuditConstants.FUNCTION)
                }
                case "MessageDef" => {
                  if (objType == "messagedef") {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.MESSAGE, AuditConstants.SUCCESS, "", AuditConstants.MESSAGE)
                }
                case "ContainerDef" => {
                  if (objType == "containerdef") {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.CONTAINER, AuditConstants.SUCCESS, "", AuditConstants.CONTAINER)
                }
                case "Concept" => {
                  if (objType == "attributedef") {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.CONCEPT, AuditConstants.SUCCESS, "", AuditConstants.CONCEPT)
                }
                case "ModelDef" => {
                  if (objType == "modeldef") {
                    keys.add(typeName)
                  }
                  if (userid != None) logAuditRec(userid, Some(AuditConstants.READ), AuditConstants.GETKEYS, AuditConstants.MODEL, AuditConstants.SUCCESS, "", AuditConstants.MODEL)
                }
                case _ => {
                  logger.error("Unknown object type " + objectType + " in GetAllKeys function")
                  throw InternalErrorException("Unknown object type " + objectType + " in GetAllKeys function", null)
                }
              }
            }
          })
        }
      })

      keys.toArray
    } catch {
      case e: Exception => {

        logger.debug("", e)
        throw InternalErrorException("Failed to get keys from persistent store", e)
      }
    }
  }

  private def DeserializeAndAddObject(k: Key, data: Array[Byte], objectsChanged: ArrayBuffer[BaseElemDef], operations: ArrayBuffer[String], maxTranId: Long): Unit = {
    val mObj= MetadataAPISerialization.deserializeMetadata(new String(data)).asInstanceOf[BaseElemDef] // serializer.DeserializeObjectFromByteArray(v.asInstanceOf[Array[Byte]]).asInstanceOf[BaseElemDef]
    if (mObj != null) {
      if (mObj.tranId <= maxTranId) {
        AddObjectToCache(mObj, MdMgr.GetMdMgr)
        DownloadJarFromDB(mObj)
      } else {
        if (mObj.isInstanceOf[FunctionDef]) {
          // BUGBUG:: Not notifying functions at this moment. This may cause inconsistance between different instances of the metadata.
        } else {
          logger.debug("The transaction id of the object => " + mObj.tranId)
          AddObjectToCache(mObj, MdMgr.GetMdMgr)
          DownloadJarFromDB(mObj)
          logger.error("Transaction is incomplete with the object " + k.bucketKey.mkString(",") + ",we may not have notified engine, attempt to do it now...")
          objectsChanged += mObj
          if (mObj.IsActive) {
            operations += "Add"
          } else {
            operations += "Remove"
          }
        }
      }
    } else {
      throw InternalErrorException("serializer.Deserialize returned a null object", null)
    }
  }

    /**
     * LoadAllObjectsIntoCache
     */
  def LoadAllObjectsIntoCache {
    try {
      val configAvailable = ConfigUtils.LoadAllConfigObjectsIntoCache
      if (configAvailable) {
        ConfigUtils.RefreshApiConfigForGivenNode(metadataAPIConfig.getProperty("NODE_ID"))
      } else {
        logger.debug("Assuming bootstrap... No config objects in persistent store")
      }

      // Load All the Model Configs here...
      ConfigUtils.LoadAllModelConfigsIntoCache
      //LoadAllUserPopertiesIntoChache
      startup = true
      val maxTranId = currentTranLevel
      var objectsChanged = ArrayBuffer[BaseElemDef]()
      var operations = ArrayBuffer[String]()

      val reqTypes = Array("types", "functions", "messages", "containers", "concepts", "models")
      val processedContainersSet = Set[String]()
      var processed: Long = 0L

      var typesYetToProcess = ArrayBuffer[(Key, Array[Byte])]()
      var functionsYetToProcess = ArrayBuffer[(Key, Array[Byte])]()

      reqTypes.foreach(typ => {
        if (typesYetToProcess.size > 0) {
          val unHandledTypes = ArrayBuffer[(Key, Array[Byte])]()
          typesYetToProcess.foreach(typ1 => {
            try {
              DeserializeAndAddObject(typ1._1, typ1._2, objectsChanged, operations, maxTranId)
            } catch {
              case e: Throwable => {
                unHandledTypes += typ1
              }
            }
          })
          typesYetToProcess = unHandledTypes
        }

        if (functionsYetToProcess.size > 0) {
          val unHandledFunctions = ArrayBuffer[(Key, Array[Byte])]()
          functionsYetToProcess.foreach(fun => {
            try {
              DeserializeAndAddObject(fun._1, fun._2, objectsChanged, operations, maxTranId)
            } catch {
              case e: Throwable => {
                unHandledFunctions += fun
              }
            }
          })
          functionsYetToProcess = unHandledFunctions
        }

        val storeInfo = PersistenceUtils.GetContainerNameAndDataStore(typ)
        if (processedContainersSet(storeInfo._1) == false) {
          processedContainersSet += storeInfo._1
          storeInfo._2.get(storeInfo._1, { (k: Key, v: Any, serType: String, typ2: String, ver:Int) =>
            {
              val data = v.asInstanceOf[Array[Byte]]
              try {
                DeserializeAndAddObject(k, data, objectsChanged, operations, maxTranId)
              } catch {
                case e: Throwable => {
                  if (typ.equalsIgnoreCase("types")) {
                    typesYetToProcess += ((k, data))
                  } else  if (typ.equalsIgnoreCase("functions")) {
                    functionsYetToProcess += ((k, data))
                  } else {
                    throw e
                  }
                }
              }
            }
            processed += 1
          })
        }
      })

      var firstException: Throwable = null

      if (typesYetToProcess.size > 0) {
        typesYetToProcess.foreach(typ => {
          try {
            DeserializeAndAddObject(typ._1, typ._2, objectsChanged, operations, maxTranId)
          } catch {
            case e: Throwable => {
              if (firstException != null)
                firstException = e
              logger.debug("Failed to handle type. Key:" + typ._1.bucketKey.mkString(","), e)
            }
          }
        })
      }

      functionsYetToProcess.foreach(fun => {
        try {
          DeserializeAndAddObject(fun._1, fun._2, objectsChanged, operations, maxTranId)
        } catch {
          case e: Throwable => {
            if (firstException != null)
              firstException = e
            logger.debug("Failed to handle function. Key:" + fun._1.bucketKey.mkString(","), e)
          }
        }
      })

      if (firstException != null)
        throw firstException

      if (processed == 0) {
        logger.debug("No metadata objects available in the Database")
        return
      }

      if (objectsChanged.length > 0) {
        NotifyEngine(objectsChanged.toArray, operations.toArray)
      }
      startup = false
    } catch {
      case e: Exception => {

        logger.debug("", e)
      }
    }
  }

  def LoadMessageIntoCache(key: String) {
    MessageAndContainerUtils.LoadMessageIntoCache(key)
  }

    /**
     * LoadTypeIntoCache
      *
      * @param key
     */
  def LoadTypeIntoCache(key: String) {
    TypeUtils.LoadTypeIntoCache(key)
  }

    /**
     * LoadModelIntoCache
      *
      * @param key
     */
  def LoadModelIntoCache(key: String) {
    ModelUtils.LoadModelIntoCache(key)
  }

    /**
     * LoadContainerIntoCache
      *
      * @param key
     */
  def LoadContainerIntoCache(key: String) {
    MessageAndContainerUtils.LoadContainerIntoCache(key)
  }

    /**
     * LoadAttributeIntoCache
      *
      * @param key
     */
  def LoadAttributeIntoCache(key: String) {
    ConceptUtils.LoadAttributeIntoCache(key)
  }

    /**
     * updateThisKey
      *
      * @param zkMessage
     * @param tranId
     */
  private def updateThisKey(zkMessage: ZooKeeperNotification, tranId: Long) {

    var key: String = (zkMessage.ObjectType + "." + zkMessage.NameSpace + "." + zkMessage.Name + "." + zkMessage.Version.toLong).toLowerCase
    val dispkey = (zkMessage.ObjectType + "." + zkMessage.NameSpace + "." + zkMessage.Name + "." + MdMgr.Pad0s2Version(zkMessage.Version.toLong)).toLowerCase

    zkMessage.ObjectType match {
      case "ConfigDef" => {
        zkMessage.Operation match {
          case "Add" => {
            val inConfig = "{\"" + zkMessage.Name + "\":" + zkMessage.ConfigContnent.get + "}"
            AddConfigObjToCache(tranId, zkMessage.NameSpace + "." + zkMessage.Name, parse(inConfig).values.asInstanceOf[Map[String, List[String]]], MdMgr.GetMdMgr)
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "adapterDef" | "nodeDef" | "clusterInfoDef" | "clusterDef" | "upDef"=> {
        zkMessage.Operation match {
          case "Add" | "Update" => {
            updateClusterConfigForKey(zkMessage.ObjectType, zkMessage.Name, zkMessage.NameSpace, zkMessage.Operation)
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "ModelDef" => {
        zkMessage.Operation match {
          case "Add" => {
            LoadModelIntoCache(key)
          }
          case "Remove" | "Activate" | "Deactivate" => {
            try {
              MdMgr.GetMdMgr.ModifyModel(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "MessageDef" => {
        zkMessage.Operation match {
          case "Add" => {
            LoadMessageIntoCache(key)
          }
          case "Remove" => {
            try {
              RemoveMessage(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, None, false)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case "Activate" | "Deactivate" => {
            try {
              MdMgr.GetMdMgr.ModifyMessage(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "ContainerDef" => {
        zkMessage.Operation match {
          case "Add" => {
            LoadContainerIntoCache(key)
          }
          case "Remove" => {
            try {
              RemoveContainer(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, None, false)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case "Activate" | "Deactivate" => {
            try {
              MdMgr.GetMdMgr.ModifyContainer(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "FunctionDef" => {
        zkMessage.Operation match {
          case "Add" => {
            FunctionUtils.LoadFunctionIntoCache(key)
          }
          case "Remove" | "Activate" | "Deactivate" => {
            try {
              MdMgr.GetMdMgr.ModifyFunction(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "AttributeDef" => {
        zkMessage.Operation match {
          case "Add" => {
            LoadAttributeIntoCache(key)
          }
          case "Remove" | "Activate" | "Deactivate" => {
            try {
              MdMgr.GetMdMgr.ModifyAttribute(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "JarDef" => {
        zkMessage.Operation match {
          case "Add" => {
            val ownerId: String = "kamanja" //FIXME:- We need to have some user for this operation.
            val uniqueId = MetadataAPIImpl.GetUniqueId
            val mdElementId = 0L //FIXME:- Not yet handled this
            DownloadJarFromDB(MdMgr.GetMdMgr.MakeJarDef(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version, ownerId, "" /* tenantId as empty */, uniqueId, mdElementId))
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case "ScalarTypeDef" | "ArrayTypeDef" | "ArrayBufTypeDef" | "ListTypeDef" | "MappedMsgTypeDef" | "SetTypeDef" | "TreeSetTypeDef" | "QueueTypeDef" | "MapTypeDef" | "ImmutableMapTypeDef" | "HashMapTypeDef" | "TupleTypeDef" | "StructTypeDef" | "SortedSetTypeDef" => {
        zkMessage.Operation match {
          case "Add" => {
            LoadTypeIntoCache(key)
          }
          case "Remove" | "Activate" | "Deactivate" => {
            try {
              logger.debug("Remove the type " + dispkey + " from cache ")
              MdMgr.GetMdMgr.ModifyType(zkMessage.NameSpace, zkMessage.Name, zkMessage.Version.toLong, zkMessage.Operation)
            } catch {
              case e: ObjectNolongerExistsException => {
                logger.error("The object " + dispkey + " nolonger exists in metadata : It may have been removed already", e)
              }
            }
          }
          case _ => { logger.error("Unknown Operation " + zkMessage.Operation + " in zookeeper notification, notification is not processed ..") }
        }
      }
      case _ => { logger.error("Unknown objectType " + zkMessage.ObjectType + " in zookeeper notification, notification is not processed ..") }
    }
  }

    /**
     * UpdateMdMgr from zookeeper
      *
      * @param zkTransaction
     */
  def UpdateMdMgr(zkTransaction: ZooKeeperTransaction): Unit = {
    var key: String = null
    var dispkey: String = null

    // If we already processed this transaction, currTranLevel will be at least at the level of this notify.
    if (zkTransaction.transactionId.getOrElse("0").toLong <= currentTranLevel) return

    try {
      zkTransaction.Notifications.foreach(zkMessage => {
        key = (zkMessage.Operation + "." + zkMessage.NameSpace + "." + zkMessage.Name + "." + zkMessage.Version).toLowerCase
        dispkey = (zkMessage.Operation + "." + zkMessage.NameSpace + "." + zkMessage.Name + "." + MdMgr.Pad0s2Version(zkMessage.Version.toLong)).toLowerCase
        if (!cacheOfOwnChanges.contains(key)) {
          // Proceed with update.
          updateThisKey(zkMessage, zkTransaction.transactionId.getOrElse("0").toLong)
        } else {
          // Ignore the update, remove the element from set.
          cacheOfOwnChanges.remove(key)
        }
      })
    } catch {
      case e: AlreadyExistsException => {

        logger.warn("Failed to load the object(" + dispkey + ") into cache", e)
      }
      case e: Exception => {

        logger.warn("Failed to load the object(" + dispkey + ") into cache", e)
      }
    }
  }

   /**
    * Get a the most recent mesage def (format JSON or XML) as a String
     *
     * @param objectName the name of the message possibly namespace qualified (is simple name, "system" namespace is substituted)
    * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return
    */
  def GetMessageDef(objectName: String, formatType: String, userid: Option[String] = None): String = {
      MessageAndContainerUtils.GetMessageDef(objectName,formatType,userid)
  }

    /**
     * Get a specific message (format JSON or XML) as a String using messageName(with version) as the key
      *
      * @param objectName Name of the MessageDef, possibly namespace qualified.
     * @param version  Version of the MessageDef
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the MessageDef either as a JSON or XML string depending on the parameter formatType
     */
    def GetMessageDef(objectName: String, version: String, formatType: String, userid: Option[String]): String = {
      MessageAndContainerUtils.GetMessageDef(objectName,version,formatType,userid)
    }

    /**
     * Get a specific message (format JSON or XML) as a String using messageName(with version) as the key
      *
      * @param nameSpace namespace of the object
     * @param objectName Name of the MessageDef
     * @param version  Version of the MessageDef
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the MessageDef either as a JSON or XML string depending on the parameter formatType
     */
    def GetMessageDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String]): String = {
      MessageAndContainerUtils.GetMessageDef(nameSpace,objectName,formatType,version,userid)
    }
    /**
     * Get a specific container (format JSON or XML) as a String using containerName(without version) as the key
      *
      * @param objectName Name of the ContainerDef, possibly namespace qualified. When no namespace, "system" substituted
     * @param formatType
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the ContainerDef either as a JSON or XML string depending on the parameter formatType
     */
  def GetContainerDef(objectName: String, formatType: String, userid: Option[String] = None): String = {
      MessageAndContainerUtils.GetContainerDef(objectName,formatType,userid)
  }

    /**
     * Get a specific container (format JSON or XML) as a String using containerName(with version) as the key
      *
      * @param nameSpace namespace of the object
     * @param objectName Name of the ContainerDef
     * @param formatType format of the return value, either JSON or XML format of the return value, either JSON or XML
     * @param version  Version of the ContainerDef
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the ContainerDef either as a JSON or XML string depending on the parameter formatType
     */
  def GetContainerDef(nameSpace: String
                      , objectName: String
                      , formatType: String
                      , version: String
                      , userid: Option[String]): String = {
      MessageAndContainerUtils.GetContainerDef(nameSpace,objectName,formatType,version,userid)
  }

    /**
     * Get a specific container (format JSON or XML) as a String using containerName(without version) as the key
      *
      * @param objectName Name of the ContainerDef, possibly namespace qualified. When no namespace, "system" substituted
     * @param version  Version of the object
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the ContainerDef either as a JSON or XML string depending on the parameter formatType
     */
  def GetContainerDef(objectName: String, version: String, formatType: String, userid: Option[String]): String = {
    MessageAndContainerUtils.GetContainerDef(objectName,version,formatType,userid)
  }

    /**
    * AddFunctions
      *
      * @param functionsText an input String of functions in a format defined by the next parameter formatType
    * @param formatType format of functionsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *         {{{
    *            val sampleFunctionStr = """
    *           {
    *  "NameSpace" : "pmml",
    *  "Name" : "my_min",
    *  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
    *  "ReturnTypeNameSpace" : "system",
    *  "ReturnTypeName" : "double",
    *  "Arguments" : [ {
    *  "ArgName" : "expr1",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "int"
    *  }, {
    *  "ArgName" : "expr2",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "double"
    *  } ],
    *  "Version" : 1,
    *  "JarName" : null,
    *  "DependantJars" : [ "basetypes_2.10-0.1.0.jar", "metadata_2.10-1.0.jar" ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.AddFunction(sampleFunctionStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)
    *}}}
    */
  def AddFunctions(functionsText:String, formatType:String, userid: Option[String] = None): String = {
    FunctionUtils.AddFunctions(functionsText,formatType,userid)
  }

    /**
    * UpdateFunctions
      *
      * @param functionsText an input String of functions in a format defined by the next parameter formatType
    * @param formatType format of functionsText ( JSON or XML)
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
    *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
    *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
    *
    *         Example:
    *         {{{
    *            val sampleFunctionStr = """
    *           {
    *  "NameSpace" : "pmml",
    *  "Name" : "my_min",
    *  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
    *  "ReturnTypeNameSpace" : "system",
    *  "ReturnTypeName" : "double",
    *  "Arguments" : [ {
    *  "ArgName" : "expr1",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "int"
    *  }, {
    *  "ArgName" : "expr2",
    *  "ArgTypeNameSpace" : "system",
    *  "ArgTypeName" : "double"
    *  } ],
    *  "Version" : 1,
    *  "JarName" : null,
    *  "DependantJars" : [ "basetypes_2.10-0.1.0.jar", "metadata_2.10-1.0.jar" ]
    *  }
    *"""
    *    var apiResult = MetadataAPIImpl.UpdateFunction(sampleFunctionStr,"JSON")
    *    var result = MetadataAPIImpl.getApiResult(apiResult)
    *    println("Result as Json String => \n" + result._2)         * }}}
    *
    */
  def UpdateFunctions(functionsText:String, formatType:String, userid: Option[String] = None): String = {
    FunctionUtils.UpdateFunctions(functionsText,formatType,userid)
  }

    /**
     *   def RemoveFunction(nameSpace:String, functionName:String, version:Long, userid: Option[String] = None): String = {
      *

      * @param nameSpace the function's namespace
     * @param functionName name of the function
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.statusCode
     *         indicates success or failure of operation: 0 for success, Non-zero for failure. The Value of
     *         ApiResult.statusDescription and ApiResult.resultData indicate the nature of the error in case of failure
     *
     *         Example:
     *         {{{
     *          val apiResult = MetadataAPIImpl.RemoveFunction(MdMgr.sysNS,"my_min",100)
     *          val (statusCode,resultData) = MetadataAPIImpl.getApiResult(apiResult)
     *          println("Result as Json String => \n" + resultData)
     *         }}}
     *
     */
  def RemoveFunction(nameSpace:String, functionName:String, version:Long, userid: Option[String] = None): String = {
    FunctionUtils.RemoveFunction(nameSpace,functionName,version,userid)
  }

    /**
     * GetAllFunctionDefs
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the function count and the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the FunctionDef(s) either as a JSON or XML string depending on the parameter formatType as a Tuple2[Int,String]
     */
  def GetAllFunctionDefs(formatType: String, userid: Option[String] = None): (Int, String) = {
    FunctionUtils.GetAllFunctionDefs(formatType,userid)
  }

    /**
     * GetFunctionDef
      *
      * @param objectName Name of the FunctionDef
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the FunctionDef(s) either as a JSON or XML string depending on the parameter formatType
     */
  def GetFunctionDef(objectName:String,formatType: String, userid: Option[String] = None) : String = {
    FunctionUtils.GetFunctionDef(objectName,formatType,userid)
  }

  /**
     * GetFunctionDef
    *
    * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetFunctionDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String]): String = {
    FunctionUtils.GetFunctionDef(nameSpace, objectName, formatType, version, userid)
  }

    /**
     * GetFunctionDef
      *
      * @param objectName Name of the FunctionDef
     * @param version  Version of the FunctionDef
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the FunctionDef either as a JSON or XML string depending on the parameter formatType
     */
  def GetFunctionDef( objectName: String, version: String, formatType: String, userid: Option[String]) : String = {
    val nameSpace = MdMgr.sysNS /** FIXME: This should be removed and the object name parsed for the namespace and name */
    FunctionUtils.GetFunctionDef(nameSpace, objectName, formatType, version, userid)
  }

    /**
     * Get all available concepts as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
     *         the Concept(s) either as a JSON or XML string depending on the parameter formatType
     */
  def GetAllConcepts(formatType: String, userid: Option[String] = None): String = {
    ConceptUtils.GetAllConcepts(formatType,userid)
  }

    /**
     * Get a single concept as a string using name and version as the key
      *
      * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param version  Version of the object
     * @param formatType format of the return value, either JSON or XML
     * @return
     */
  def GetConcept(nameSpace: String, objectName: String, version: String, formatType: String, userid: Option[String]): String = {
    ConceptUtils.GetConcept(nameSpace,objectName,version,formatType)
  }

    /**
     * Get a single concept as a string using name and version as the key
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param version  Version of the object
     * @param formatType format of the return value, either JSON or XML
     * @return
     */
  def GetConcept(objectName: String, version: String, formatType: String, userid: Option[String]): String = {
    GetConcept(MdMgr.sysNS, objectName, version, formatType, userid)
  }


    /**
     * Get a single concept as a string using name and version as the key
      *
      * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetConceptDef(nameSpace: String, objectName: String, formatType: String,
                    version: String, userid: Option[String]): String = {
    ConceptUtils.GetConceptDef(nameSpace, objectName, formatType, version, userid)
  }

    /**
     * Get a list of concept(s) as a string using name
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetConcept(objectName: String, formatType: String, userid: Option[String] = None): String = {
    ConceptUtils.GetConcept(objectName,formatType)
  }

    /**
     * Get all available derived concepts(format JSON or XML) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllDerivedConcepts(formatType: String, userid: Option[String] = None): String = {
    ConceptUtils.GetAllDerivedConcepts(formatType)
  }

  //
    /**
     * Get a derived concept(format JSON or XML) as a string using name(without version) as the key
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetDerivedConcept(objectName: String, formatType: String, userid: Option[String] = None): String = {
    ConceptUtils.GetDerivedConcept(objectName,formatType)
  }

    /**
     * GetDerivedConcept - A derived concept(format JSON or XML) as a string using name and version as the key
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param version  Version of the object
     * @param formatType format of the return value, either JSON or XML
     * @return
     */
  def GetDerivedConcept(objectName: String, version: String, formatType: String, userid: Option[String]): String = {
    ConceptUtils.GetDerivedConcept(objectName,version,formatType)
  }

   /**
    * GetAllTypes - All available types(format JSON or XML) as a String
     *
     * @param formatType format of the return value, either JSON or XML
    * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *               method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return the result as a JSON String of object ApiResult where ApiResult.resultData contains
    *         the available types as a JSON or XML string depending on the parameter formatType
    */
  def GetAllTypes(formatType: String, userid: Option[String] = None): String = {
    TypeUtils.GetAllTypes(formatType,userid)
  }

    /**
     * GetAllTypesByObjType - All available types(format JSON or XML) as a String
      *
      * @param formatType format of the return value, either JSON or XML
     * @param objType
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetAllTypesByObjType(formatType: String, objType: String, userid: Option[String] = None): String = {
    TypeUtils.GetAllTypesByObjType(formatType,objType)
  }

    /**
     * GetType
      *
      * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  // Get types for a given name
  def GetType(objectName: String, formatType: String, userid: Option[String] = None): String = {
    TypeUtils.GetType(objectName,formatType)
  }

    /**
     * GetTypeDef
      *
      * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param formatType format of the return value, either JSON or XML
     * @param version  Version of the object
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetTypeDef(nameSpace: String, objectName: String, formatType: String, version: String, userid: Option[String] = None): String = {
    TypeUtils.GetTypeDef(nameSpace,objectName,formatType,version,userid)
  }

    /**
     * GetType
      *
      * @param nameSpace namespace of the object
     * @param objectName name of the desired object, possibly namespace qualified
     * @param version  Version of the object
     * @param formatType format of the return value, either JSON or XML
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def GetType(nameSpace: String, objectName: String, version: String, formatType: String, userid: Option[String]): Option[BaseTypeDef] = {
    TypeUtils.GetType(nameSpace, objectName, version, formatType, userid)
  }

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
    ConfigUtils.AddNode(nodeId, nodePort, nodeIpAddr, jarPaths, scala_home,
        java_home, classpath, clusterId, power, roles, description)
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
    ConfigUtils.AddNode(nodeId, nodePort, nodeIpAddr, jarPaths, scala_home,
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
    ConfigUtils.RemoveNode(nodeId)
  }

  def AddTenant(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): String = {
    ConfigUtils.AddTenant(tenantId, description, primaryDataStore, cacheConfig)
  }

  def UpdateTenant(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): String = {
    ConfigUtils.UpdateTenant(tenantId, description, primaryDataStore, cacheConfig)
  }

  def RemoveTenant(tenantId: String): String = {
    ConfigUtils.RemoveTenant(tenantId)
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
    ConfigUtils.AddAdapter(name, typeString, className, jarName,
        dependencyJars, adapterSpecificCfg, tenantId, fullAdapterConfig)
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
    ConfigUtils.AddAdapter(name, typeString, className, jarName, dependencyJars, adapterSpecificCfg, tenantId, fullAdapterConfig)
  }

    /**
     * RemoveAdapter
      *
      * @param name
     * @return
     */
  def RemoveAdapter(name: String): String = {
    ConfigUtils.RemoveAdapter(name)
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
    ConfigUtils.AddCluster(clusterId,description,privileges)
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
    ConfigUtils.AddCluster(clusterId, description, privileges)
  }

    /**
     * RemoveCluster
      *
      * @param clusterId
     * @return
     */
  def RemoveCluster(clusterId: String): String = {
    ConfigUtils.RemoveCluster(clusterId)
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
    ConfigUtils.AddClusterCfg(clusterCfgId, cfgMap, modifiedTime, createdTime)
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
    ConfigUtils.AddClusterCfg(clusterCfgId, cfgMap, modifiedTime, createdTime)
  }

    /**
     * Remove a cluster configuration with the suppplied id
     *
     * @param clusterCfgId
     * @return results string
     */
  def RemoveClusterCfg(clusterCfgId: String, userid: Option[String] = None): String = {
    ConfigUtils.RemoveClusterCfg(clusterCfgId)
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
    ConfigUtils.RemoveConfig(cfgStr,userid,cobjects)
  }

   /**
     * Answer the model compilation dependencies
     * FIXME: Which ones? input or output?
     *
     * @param modelConfigName
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def getModelDependencies(modelConfigName: String, userid: Option[String] = None): List[String] = {
      var config = MdMgr.GetMdMgr.GetModelConfig(modelConfigName.toLowerCase)
      val typDeps = config.getOrElse(ModelCompilationConstants.DEPENDENCIES, null)
      if (typDeps != null) {
        if (typDeps.isInstanceOf[List[_]])
          return typDeps.asInstanceOf[List[String]]
        if (typDeps.isInstanceOf[Array[_]])
          return typDeps.asInstanceOf[Array[String]].toList
      }
      List[String]()
  }

    /**
     * getModelMessagesContainers
      *
      * @param modelConfigName
     * @param userid the identity to be used by the security adapter to ascertain if this user has access permissions for this
     *               method. If Security and/or Audit are configured, this value must be a value other than None.
     * @return
     */
  def getModelMessagesContainers(modelConfigName: String, userid: Option[String] = None): List[String] = {
    MessageAndContainerUtils.getModelMessagesContainers(modelConfigName,userid)
  }

  def getModelInputTypesSets(modelConfigName: String, userid: Option[String] = None): List[List[String]] = {
    MessageAndContainerUtils.getModelInputTypesSets(modelConfigName,userid)
  }

  def getModelOutputTypes(modelConfigName: String, userid: Option[String] = None): List[String] = {
    MessageAndContainerUtils.getModelOutputTypes(modelConfigName,userid)
  }

  /**
     * Get the model config keys
      *
      * @return
     */
  def getModelConfigNames(): Array[String] = {
    MdMgr.GetMdMgr.GetModelConfigKeys
  }

  /**
   *
   */
  private var cfgmap: Map[String, Any] = null

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
    ConfigUtils.UploadModelsConfig(cfgStr,userid,objectList,isFromNotify)
  }

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
    ConfigUtils.UploadConfig(cfgStr,userid,objectList)
  }

    /**
     * Get a property value
      *
      * @param ci
     * @param key
     * @return
     */
  def getUP(ci: String, key: String): String = {
    ConfigUtils.getUP(ci,key)
  }

    /**
     * Answer nodes as an array.
      *
      * @return
     */
  def getNodeList1: Array[NodeInfo] = {
    ConfigUtils.getNodeList1
  }
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
    ConfigUtils.GetAllNodes(formatType,userid)
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
    ConfigUtils.GetAllAdapters(formatType,userid)
  }



  private def updateClusterConfigForKey(elemType: String, key: String, clusterId: String, operation: String): Unit = {

    if (elemType.equalsIgnoreCase("adapterDef")) {
      //val obj = GetObject("adapterinfo."+key.toLowerCase, "config_objects")
      val storedInfo: AdapterInfo = MetadataAPISerialization.deserializeMetadata(new String(GetObject("adapterinfo."+key.toLowerCase, "config_objects")._2.asInstanceOf[Array[Byte]])).asInstanceOf[AdapterInfo]
      MdMgr.GetMdMgr.AddAdapter(storedInfo)
      if (operation.equalsIgnoreCase("add")) {
        MdMgr.GetMdMgr.addConfigChange((elemType +"."+"add"+"."+storedInfo.name.toLowerCase, storedInfo))
        return
      }
      if (operation.equalsIgnoreCase("update")) {
        MdMgr.GetMdMgr.addConfigChange((elemType + "." + "update" + "." + storedInfo.name.toLowerCase, storedInfo))
        return
      }
      return
    }

    if (elemType.equalsIgnoreCase("nodeDef")) {
     // val obj = GetObject("nodeinfo."+key.toLowerCase, "config_objects")
      val storedInfo = MetadataAPISerialization.deserializeMetadata(new String(GetObject("nodeinfo."+key.toLowerCase, "config_objects")._2.asInstanceOf[Array[Byte]])).asInstanceOf[NodeInfo]
      MdMgr.GetMdMgr.AddNode(storedInfo)
      if (operation.equalsIgnoreCase("add")) {
        MdMgr.GetMdMgr.addConfigChange((elemType +"."+"add"+"."+storedInfo.nodeId.toLowerCase, storedInfo))
        return
      }
      if (operation.equalsIgnoreCase("update")) {
        MdMgr.GetMdMgr.addConfigChange((elemType + "." + "update" + "." + storedInfo.nodeId.toLowerCase, storedInfo))
        return
      }
      return
    }

    if (elemType.equalsIgnoreCase("clusterInfoDef")) {
      //val obj = GetObject("clustercfginfo."+key.toLowerCase, "config_objects")
      val storedInfo = MetadataAPISerialization.deserializeMetadata(new String(GetObject("clustercfginfo."+key.toLowerCase, "config_objects")._2.asInstanceOf[Array[Byte]])).asInstanceOf[ClusterCfgInfo]
      MdMgr.GetMdMgr.AddClusterCfg(storedInfo)
      if (operation.equalsIgnoreCase("add")) {
        MdMgr.GetMdMgr.addConfigChange((elemType +"."+"add"+"."+storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      if (operation.equalsIgnoreCase("update")) {
        MdMgr.GetMdMgr.addConfigChange((elemType + "." + "update" + "." + storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      return

    }


    if (elemType.equalsIgnoreCase("clusterDef")) {
      //val obj = GetObject("clusterinfo."+key.toLowerCase, "config_objects")
      val storedInfo = MetadataAPISerialization.deserializeMetadata(new String(GetObject("clusterinfo."+key.toLowerCase, "config_objects")._2.asInstanceOf[Array[Byte]])).asInstanceOf[ClusterInfo]
      MdMgr.GetMdMgr.AddCluster(storedInfo)
      if (operation.equalsIgnoreCase("add")) {
        MdMgr.GetMdMgr.addConfigChange((elemType +"."+"add"+"."+storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      if (operation.equalsIgnoreCase("update")) {
        MdMgr.GetMdMgr.addConfigChange((elemType + "." + "update" + "." + storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      return
    }

    if (elemType.equalsIgnoreCase("upDef")) {
      //val obj = GetObject("userproperties."+key.toLowerCase, "config_objects")
      val storedInfo = MetadataAPISerialization.deserializeMetadata(new String(GetObject("userproperties."+key.toLowerCase, "config_objects")._2.asInstanceOf[Array[Byte]])).asInstanceOf[UserPropertiesInfo]
      MdMgr.GetMdMgr.AddUserProperty(storedInfo)
      if (operation.equalsIgnoreCase("add")) {
        MdMgr.GetMdMgr.addConfigChange((elemType +"."+"add"+"."+storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      if (operation.equalsIgnoreCase("update")) {
        MdMgr.GetMdMgr.addConfigChange((elemType + "." + "update" + "." + storedInfo.clusterId.toLowerCase, storedInfo))
        return
      }
      return
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
    ConfigUtils.GetAllClusters(formatType,userid)
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
    ConfigUtils.GetAllClusterCfgs(formatType,userid)
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
    ConfigUtils.GetAllCfgObjects(formatType,userid)
  }

  /**
   * Dump the configuration file to the log
   */
  def dumpMetadataAPIConfig {
    ConfigUtils.dumpMetadataAPIConfig
  }

    /**
     * Refresh the ClusterConfiguration for the specified node
      *
      * @param nodeId a cluster node
     * @return
     */
  def RefreshApiConfigForGivenNode(nodeId: String): Boolean = {
    ConfigUtils.RefreshApiConfigForGivenNode(nodeId)
  }

  /**
    * Read metadata api configuration properties
    *
    * @param configFile the MetadataAPI configuration file
    */
  @throws(classOf[MissingPropertyException])
  @throws(classOf[InvalidPropertyException])
  def readMetadataAPIConfigFromPropertiesFile(configFile: String): Unit = {
    ConfigUtils.readMetadataAPIConfigFromPropertiesFile(configFile)
  }

    /**
     * Read the default configuration property values from json config file.
      *
      * @param cfgFile
     */
  @throws(classOf[MissingPropertyException])
  @throws(classOf[LoadAPIConfigException])
  def readMetadataAPIConfigFromJsonFile(cfgFile: String): Unit = {
    ConfigUtils.readMetadataAPIConfigFromJsonFile(cfgFile)
  }

    /**
     * Initialize the metadata from the bootstrap, establish zookeeper listeners, load the cached information from
     * persistent storage, set up heartbeat and authorization implementations.
     *
     * @param configFile the MetadataAPI configuration file
     * @param startHB
     */
  def InitMdMgr(configFile: String, startHB: Boolean) {

    MdMgr.GetMdMgr.truncate
    val mdLoader = new MetadataLoad(MdMgr.mdMgr, "", "", "", "")
    mdLoader.initialize
    if (configFile.endsWith(".json")) {
      MetadataAPIImpl.readMetadataAPIConfigFromJsonFile(configFile)
    } else {
      MetadataAPIImpl.readMetadataAPIConfigFromPropertiesFile(configFile)
    }
    val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
    val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
    MetadataAPIImpl.OpenDbStore(jarPaths, GetMetadataAPIConfig.getProperty("METADATA_DATASTORE"))
    MetadataAPIImpl.CreateMetadataTables
    MetadataAPIImpl.LoadAllObjectsIntoCache
    MetadataAPIImpl.CloseDbStore
    MetadataAPIImpl.InitSecImpl
    if (startHB) InitHearbeat
    initZkListeners(startHB)
  }

  /**
    * Initialize the metadata from the bootstrap, establish zookeeper listeners, load the cached information from
    * persistent storage, set up heartbeat and authorization implementations.
    * FIXME: Is there a difference between this function and InitMdMgr?
    *
    * @see InitMdMgr(String,Boolean)
    * @param configFile the MetadataAPI configuration file
    * @param startHB
    */
  def InitMdMgrFromBootStrap(configFile: String, startHB: Boolean) {

    MdMgr.GetMdMgr.truncate
    val mdLoader = new MetadataLoad(MdMgr.mdMgr, "", "", "", "")
    mdLoader.initialize
    if (configFile.endsWith(".json")) {
      MetadataAPIImpl.readMetadataAPIConfigFromJsonFile(configFile)
    } else {
      MetadataAPIImpl.readMetadataAPIConfigFromPropertiesFile(configFile)
    }

    initZkListeners(startHB)
    val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
    val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
    MetadataAPIImpl.OpenDbStore(jarPaths, GetMetadataAPIConfig.getProperty("METADATA_DATASTORE"))
    MetadataAPIImpl.CreateMetadataTables
    MetadataAPIImpl.LoadAllObjectsIntoCache
    MetadataAPIImpl.InitSecImpl
    if (startHB) InitHearbeat
    isInitilized = true
    logger.debug("Metadata synching is now available.")

  }

    /**
     * Initialize the heart beat service
     */
  private def InitHearbeat: Unit = {
    InitZooKeeper
    MonitorAPIImpl.initMonitorValues(metadataAPIConfig.getProperty("NODE_ID").toString)
    MonitorAPIImpl.startMetadataHeartbeat
  }

  /**
   * Create a listener to monitor Meatadata Cache
   */
  def initZkListeners(startHB: Boolean): Unit = {
    // Set up a zk listener for metadata invalidation   metadataAPIConfig.getProperty("AUDIT_IMPL_CLASS").trim
    var znodePath = metadataAPIConfig.getProperty("ZNODE_PATH")
    var hbPathEngine = znodePath
    var hbPathMetadata = znodePath
    if (znodePath != null) {
      znodePath = znodePath.trim + "/metadataupdate"
      hbPathEngine = hbPathEngine.trim + "/monitor/engine"
      hbPathMetadata = hbPathMetadata + "/monitor/metadata"
    } else return
    var zkConnectString = metadataAPIConfig.getProperty("ZOOKEEPER_CONNECT_STRING")
    if (zkConnectString != null) zkConnectString = zkConnectString.trim else return

    if (zkConnectString != null && zkConnectString.isEmpty() == false && znodePath != null && znodePath.isEmpty() == false) {
      try {
        CreateClient.CreateNodeIfNotExists(zkConnectString, znodePath)
        zkListener = new ZooKeeperListener
        zkListener.CreateListener(zkConnectString, znodePath, UpdateMetadata, 3000, 3000)
        if (startHB) {
          zkListener.CreatePathChildrenCacheListener(zkConnectString, hbPathEngine, true, MonitorAPIImpl.updateHeartbeatInfo, 3000, 3000)
          zkListener.CreatePathChildrenCacheListener(zkConnectString, hbPathMetadata, true, MonitorAPIImpl.updateHeartbeatInfo, 3000, 3000)
        }
      } catch {
        case e: Exception => {
          logger.error("Failed to initialize ZooKeeper Connection.", e)
          throw e
        }
      }
    }
  }


  /**
   * shutdownZkListener - should be called by application using MetadataAPIImpl directly to disable synching of Metadata cache.
   */
  private def shutdownZkListener: Unit = {
    try {
      CloseZKSession
      if (zkListener != null) {
        zkListener.Shutdown
      }
    } catch {
      case e: Exception => {
        logger.error("Error trying to shutdown zookeeper listener.", e)
        throw e
      }
    }
  }

    /**
     * Shutdown the heart beat monitor
     */
  private def shutdownHeartbeat: Unit = {
    try {
      MonitorAPIImpl.shutdownMonitor
      if (heartBeat != null)
        heartBeat.Shutdown
      heartBeat = null
    } catch {
      case e: Exception => {
        logger.error("Error trying to shutdown Hearbbeat. ", e)
        throw e
      }
    }
  }

  /**
   * Release various resources including heartbeat, dbstore, zk listener, and audit adapter
   * FIXME: What about Security adapter? Should there be a 'release' call on the SecurityAdapter trait?
   */
  def shutdown: Unit = {
    shutdownHeartbeat
    CloseDbStore
    shutdownZkListener
    shutdownAuditAdapter
  }

    /**
     * UpdateMetadata - This is a callback function for the Zookeeper Listener.  It will get called when we detect Metadata being updated from
     *                  a different metadataImpl service.
     *
     * @param receivedJsonStr message from another cluster node
     */
  def UpdateMetadata(receivedJsonStr: String): Unit = {
    logger.debug("Process ZooKeeper notification " + receivedJsonStr)
    if (receivedJsonStr == null || receivedJsonStr.size == 0 || !isInitilized) {
      // nothing to do
      logger.debug("Metadata synching is not available.")
      return
    }

    val zkTransaction = JsonSerializer.parseZkTransaction(receivedJsonStr, "JSON")
    MetadataAPIImpl.UpdateMdMgr(zkTransaction)
  }

    /**
     * InitMdMgr
      *
      * @param mgr
     * @param jarPathsInfo
     * @param databaseInfo
     */
  def InitMdMgr(mgr: MdMgr, jarPathsInfo: String, databaseInfo: String) {

    val mdLoader = new MetadataLoad(mgr, "", "", "", "")
    mdLoader.initialize

    metadataAPIConfig.setProperty("JAR_PATHS", jarPathsInfo)
    metadataAPIConfig.setProperty("METADATA_DATASTORE", databaseInfo)

    val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
    val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
    MetadataAPIImpl.OpenDbStore(jarPaths, GetMetadataAPIConfig.getProperty("METADATA_DATASTORE"))
    MetadataAPIImpl.CreateMetadataTables
    MetadataAPIImpl.LoadAllObjectsIntoCache
  }
}
