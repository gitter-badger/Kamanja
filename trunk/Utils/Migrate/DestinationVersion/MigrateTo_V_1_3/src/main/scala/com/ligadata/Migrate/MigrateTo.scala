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

package com.ligadata.Migrate

import com.ligadata.MigrateBase._
import org.apache.logging.log4j._
import java.io.{File, PrintWriter}

import com.ligadata.kamanja.metadata.MdMgr
import com.ligadata.kamanja.metadataload.MetadataLoad
import com.ligadata.MetadataAPI.MetadataAPIImpl
import com.ligadata.MetadataAPI.MetadataAPI
import com.ligadata.Serialize.JsonSerializer
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import scala.io.Source

import com.ligadata.KvBase.{KvBaseDefalts, TimeRange, Key, Value}

import com.ligadata.StorageBase.{DataStore, DataStoreOperations}
import com.ligadata.keyvaluestore.KeyValueManager
import scala.collection.mutable.ArrayBuffer
import com.ligadata.kamanja.metadata.ModelCompilationConstants
import com.ligadata.Exceptions._

import scala.actors.threadpool.{Executors, ExecutorService, TimeUnit}

class MigrateTo_V_1_3 extends MigratableTo {
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private var _unhandledMetadataDumpDir: String = _
  private var _curMigrationSummaryFlPath: String = _
  private var _sourceVersion: String = _
  private var _fromScalaVersion: String = _
  private var _toScalaVersion: String = _
  private var _destInstallPath: String = _
  private var _apiConfigFile: String = _
  private var _clusterConfigFile: String = _
  private var _metaDataStoreInfo: String = _
  private var _dataStoreInfo: String = _
  private var _statusStoreInfo: String = _
  private var _metaDataStoreDb: DataStore = _
  private var _dataStoreDb: DataStore = _
  private var _statusStoreDb: DataStore = _
  private var _jarPaths: collection.immutable.Set[String] = collection.immutable.Set[String]()
  private var _bInit = false
  private var _flCurMigrationSummary: PrintWriter = _
  private val defaultUserId: Option[String] = Some("kamanja")
  private var _parallelDegree = 0
  private var _mergeContainerAndMessages = true

  private val globalExceptions = ArrayBuffer[(String, Throwable)]()

  private def AddToGlobalException(failedMsg: String, e: Throwable): Unit = {
    globalExceptions += ((failedMsg, e))
  }

  private def AddMdObjToGlobalException(mdObj: (String, Map[String, Any]), failedMsg: String, e: Throwable): Unit = {
    val namespace = mdObj._2.getOrElse("NameSpace", "").toString.trim()
    val name = mdObj._2.getOrElse("Name", "").toString.trim()
    AddToGlobalException(failedMsg + ":" + namespace + "." + name, e)
  }

  private def LogGlobalException: Boolean = {
    if (globalExceptions.size > 0) {
      globalExceptions.foreach(expTup => {
        if (expTup._2 != null)
          logger.error(if (expTup._1 != null) expTup._1 else "", expTup._2)
        else
          logger.error(if (expTup._1 != null) expTup._1 else "")
      })
      return true
    }

    return false
  }


  private def isValidPath(path: String, checkForDir: Boolean = false, checkForFile: Boolean = false, str: String = "path"): Unit = {
    val fl = new File(path)
    if (fl.exists() == false) {
      val szMsg = "Given %s:%s does not exists".format(str, path)
      logger.error(szMsg)
      throw new Exception(szMsg)
    }

    if (checkForDir && fl.isDirectory() == false) {
      val szMsg = "Given %s:%s is not directory".format(str, path)
      logger.error(szMsg)
      throw new Exception(szMsg)
    }

    if (checkForFile && fl.isFile() == false) {
      val szMsg = "Given %s:%s is not file".format(str, path)
      logger.error(szMsg)
      throw new Exception(szMsg)
    }
  }

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

  private def GetDataStoreStatusStoreInfo(cfgStr: String): (String, String) = {
    var dsStr: String = null
    var ssStr: String = ""
    try {
      // extract config objects
      val map = JsonSerializer.parseEngineConfig(cfgStr)
      // process clusterInfo object if it exists
      if (map.contains("Clusters")) {
        val clustersList = map.get("Clusters").get.asInstanceOf[List[_]]
        val clusters = clustersList.length
        logger.debug("Found " + clusters + " cluster objects ")
        clustersList.foreach(clustny => {
          if (dsStr == null || dsStr.size == 0) {
            val cluster = clustny.asInstanceOf[Map[String, Any]]
            val ClusterId = cluster.getOrElse("ClusterId", "").toString.trim.toLowerCase
            logger.debug("Processing the cluster => " + ClusterId)
            if (ClusterId.size > 0 && cluster.contains("DataStore"))
              dsStr = getStringFromJsonNode(cluster.getOrElse("DataStore", null))
          }
          if (ssStr == null || ssStr.size == 0) {
            val cluster = clustny.asInstanceOf[Map[String, Any]]
            val ClusterId = cluster.getOrElse("ClusterId", "").toString.trim.toLowerCase
            logger.debug("Processing the cluster => " + ClusterId)
            if (ClusterId.size > 0 && cluster.contains("StatusInfo"))
              ssStr = getStringFromJsonNode(cluster.getOrElse("StatusInfo", null))
          }
        })
      }
      logger.debug("Found Datastore String:%s and Statusstore String:%s".format(dsStr, ssStr));
      (dsStr, ssStr)
    } catch {
      case e: Exception => {
        throw new Exception("Failed to parse clusterconfig", e)
      }
      case e: Throwable => {
        throw new Exception("Failed to parse clusterconfig", e)
      }
    }
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

  override def init(destInstallPath: String, apiConfigFile: String, clusterConfigFile: String, sourceVersion: String, unhandledMetadataDumpDir: String, curMigrationSummaryFlPath: String, parallelDegree: Int, mergeContainerAndMessages: Boolean, fromScalaVersion: String, toScalaVersion: String): Unit = {
    isValidPath(apiConfigFile, false, true, "apiConfigFile")
    isValidPath(clusterConfigFile, false, true, "clusterConfigFile")

    isValidPath(destInstallPath, true, false, "destInstallPath")
    isValidPath(destInstallPath + "/bin", true, false, "bin folder in destInstallPath")
    isValidPath(destInstallPath + "/lib/system", true, false, "/lib/system folder in destInstallPath")
    isValidPath(destInstallPath + "/lib/application", true, false, "/lib/application folder in destInstallPath")

    isValidPath(unhandledMetadataDumpDir, true, false, "unhandledMetadataDumpDir")

    MdMgr.GetMdMgr.truncate
    val mdLoader = new MetadataLoad(MdMgr.mdMgr, "", "", "", "")
    mdLoader.initialize
    MetadataAPIImpl.readMetadataAPIConfigFromPropertiesFile(apiConfigFile)

    val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
    val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
    val metaDataStoreInfo = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("METADATA_DATASTORE");
    val cfgStr = Source.fromFile(clusterConfigFile).mkString
    val (dataStoreInfo, statusStoreInfo) = GetDataStoreStatusStoreInfo(cfgStr)

    if (metaDataStoreInfo == null || metaDataStoreInfo.size == 0) {
      throw new Exception("Not found valid MetadataStore info in " + apiConfigFile)
    }

    if (dataStoreInfo == null || dataStoreInfo.size == 0) {
      throw new Exception("Not found valid DataStore info in " + clusterConfigFile)
    }

    val sysPath = new File(destInstallPath + "/lib/system")
    val appPath = new File(destInstallPath + "/lib/application")

    val toVersionJarPaths = collection.immutable.Set[String](sysPath.getAbsolutePath, appPath.getAbsolutePath) ++ jarPaths

    _destInstallPath = destInstallPath
    _apiConfigFile = apiConfigFile
    _clusterConfigFile = clusterConfigFile

    _metaDataStoreInfo = metaDataStoreInfo
    _dataStoreInfo = dataStoreInfo
    _statusStoreInfo = if (statusStoreInfo == null) "" else statusStoreInfo
    _jarPaths = toVersionJarPaths
    _sourceVersion = sourceVersion
    _fromScalaVersion = fromScalaVersion
    _toScalaVersion = toScalaVersion

    _unhandledMetadataDumpDir = unhandledMetadataDumpDir
    _curMigrationSummaryFlPath = curMigrationSummaryFlPath

    _flCurMigrationSummary = new PrintWriter(_curMigrationSummaryFlPath, "UTF-8")

    // Open the database here
    _metaDataStoreDb = GetDataStoreHandle(toVersionJarPaths, metaDataStoreInfo)
    _dataStoreDb = GetDataStoreHandle(toVersionJarPaths, dataStoreInfo)

    if (_statusStoreInfo.size > 0) {
      _statusStoreDb = GetDataStoreHandle(toVersionJarPaths, _statusStoreInfo)
    }

    _parallelDegree = if (parallelDegree <= 1) 1 else parallelDegree
    _mergeContainerAndMessages = mergeContainerAndMessages

    _bInit = true
  }

  override def isInitialized: Boolean = _bInit

  override def getMetadataStoreInfo: String = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    _metaDataStoreInfo
  }

  override def getDataStoreInfo: String = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    _dataStoreInfo
  }

  override def getStatusStoreInfo: String = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    _statusStoreInfo
  }

  override def isMetadataTableExists(tblInfo: TableName): Boolean = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    _metaDataStoreDb.isTableExists(tblInfo.namespace, tblInfo.name)
  }

  override def isDataTableExists(tblInfo: TableName): Boolean = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    _dataStoreDb.isTableExists(tblInfo.namespace, tblInfo.name)
  }

  override def isStatusTableExists(tblInfo: TableName): Boolean = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    if (_statusStoreDb != null)
      return _statusStoreDb.isTableExists(tblInfo.namespace, tblInfo.name)
    false
  }

  private def addBackupTablesToExecutor(executor: ExecutorService, storeDb: DataStore, tblsToBackedUp: Array[BackupTableInfo], errMsgTemplate: String, force: Boolean): Unit = {
    tblsToBackedUp.foreach(backupTblInfo => {
      executor.execute(new Runnable() {
        override def run() = {
          try {
            storeDb.copyTable(backupTblInfo.namespace, backupTblInfo.srcTable, backupTblInfo.dstTable, force)
          } catch {
            case e: Exception => AddToGlobalException(errMsgTemplate + "(" + backupTblInfo.namespace + "," + backupTblInfo.srcTable + " => " + backupTblInfo.namespace + "," + backupTblInfo.dstTable + ")", e)
            case e: Throwable => AddToGlobalException(errMsgTemplate + "(" + backupTblInfo.namespace + "," + backupTblInfo.srcTable + " => " + backupTblInfo.namespace + "," + backupTblInfo.dstTable + ")", e)
          }
        }
      })
    })
  }

  override def backupAllTables(metadataTblsToBackedUp: Array[BackupTableInfo], dataTblsToBackedUp: Array[BackupTableInfo], statusTblsToBackedUp: Array[BackupTableInfo], force: Boolean): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    logger.debug("Backup tables => (Metadata Tables:{" + metadataTblsToBackedUp.map(t => "((" + t.namespace + "," + t.srcTable + ") => (" + t.namespace + "," + t.dstTable + "))").mkString(",") + "}, Data Tables:{"
      + dataTblsToBackedUp.map(t => "((" + t.namespace + "," + t.srcTable + ") => (" + t.namespace + "," + t.dstTable + "))").mkString(",") + "}, Status Tables:{"
      + statusTblsToBackedUp.map(t => "((" + t.namespace + "," + t.srcTable + ") => (" + t.namespace + "," + t.dstTable + "))").mkString(",") + "})")
    var executor: ExecutorService = null
    try {
      executor = Executors.newFixedThreadPool(if (_parallelDegree <= 1) 1 else _parallelDegree)

      if (statusTblsToBackedUp.size > 0) {
        if (_statusStoreDb == null)
          throw new Exception("Does not have Status store information")
        addBackupTablesToExecutor(executor, _statusStoreDb, statusTblsToBackedUp, "Failed to backup status table:", force)
      }

      addBackupTablesToExecutor(executor, _metaDataStoreDb, metadataTblsToBackedUp, "Failed to backup metadata table:", force)
      addBackupTablesToExecutor(executor, _dataStoreDb, dataTblsToBackedUp, "Failed to backup data table:", force)
      executor.shutdown();
      try {
        executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
      } catch {
        case e: Exception => AddToGlobalException("Failed to backup tables", e)
        case e: Throwable => AddToGlobalException("Failed to backup tables", e)
      }
    } catch {
      case e: Exception => AddToGlobalException("Failed to backup tables", e)
      case e: Throwable => AddToGlobalException("Failed to backup tables", e)
    }

    if (executor != null) {
      executor.shutdown();
      try {
        executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
      } catch {
        case e: Exception => AddToGlobalException("Failed to backup tables", e)
        case e: Throwable => AddToGlobalException("Failed to backup tables", e)
      }
    }

    if (LogGlobalException) {
      throw new Exception("Failed to Backup tables")
    }
  }

  private def addDropTablesToExecutor(executor: ExecutorService, storeDb: DataStore, tblsToDrop: Array[(String, String)], errMsgTemplate: String): Unit = {
    if (tblsToDrop.size > 0) {
      tblsToDrop.foreach(dropTblTuple => {
        executor.execute(new Runnable() {
          override def run() = {
            try {
              storeDb.dropTables(Array(dropTblTuple))
            } catch {
              case e: Exception => AddToGlobalException(errMsgTemplate + dropTblTuple.toString(), e)
              case e: Throwable => AddToGlobalException(errMsgTemplate + dropTblTuple.toString(), e)
            }
          }
        })
      })
    }
  }

  def dropAllTables(metadataTblsToDrop: Array[TableName], dataTblsToDrop: Array[TableName], statusTblsToDrop: Array[TableName]): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    var executor: ExecutorService = null
    try {
      executor = Executors.newFixedThreadPool(if (_parallelDegree <= 1) 1 else _parallelDegree)

      val metadataTblsTuples = metadataTblsToDrop.map(t => (t.namespace, t.name))
      val dataTblsTuples = dataTblsToDrop.map(t => (t.namespace, t.name))
      val statusTblsTuples = statusTblsToDrop.map(t => (t.namespace, t.name))
      logger.debug("Drop tables => (Metadata Tables:{" + metadataTblsTuples.mkString(",") + "}, Data Tables:" + dataTblsTuples.mkString(",") + "}, Status Tables:" + statusTblsTuples.mkString(",") + "})")

      if (statusTblsTuples.size > 0) {
        if (_statusStoreDb == null)
          throw new Exception("Does not have Status store information")
        addDropTablesToExecutor(executor, _statusStoreDb, statusTblsTuples, "Failed to drop status table:")
      }

      addDropTablesToExecutor(executor, _metaDataStoreDb, metadataTblsTuples, "Failed to drop data table:")
      addDropTablesToExecutor(executor, _dataStoreDb, dataTblsTuples, "Failed to drop metadata table:")

      executor.shutdown();
      try {
        executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
      } catch {
        case e: Exception => AddToGlobalException("Failed to drop tables", e)
        case e: Throwable => AddToGlobalException("Failed to drop tables", e)
      }
    } catch {
      case e: Exception => AddToGlobalException("Failed to drop tables", e)
      case e: Throwable => AddToGlobalException("Failed to drop tables", e)
    }

    if (executor != null) {
      executor.shutdown();
      try {
        executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
      } catch {
        case e: Exception => AddToGlobalException("Failed to drop tables", e)
        case e: Throwable => AddToGlobalException("Failed to drop tables", e)
      }
    }

    if (LogGlobalException) {
      throw new Exception("Failed to Drop tables")
    }
  }

  override def dropMessageContainerTablesFromMetadata(allMetadataElemsJson: Array[MetadataFormat]): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    val messagesAndContainers = scala.collection.mutable.Set[String]()

    allMetadataElemsJson.foreach(mdf => {
      val json = parse(mdf.objDataInJson)
      val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]

      val isActiveStr = jsonObjMap.getOrElse("IsActive", "").toString.trim()
      if (isActiveStr.size > 0) {
        val isActive = jsonObjMap.getOrElse("IsActive", "").toString.trim().toBoolean
        if (isActive) {
          if ((mdf.objType == "MessageDef") || (mdf.objType == "ContainerDef")) {
            val namespace = jsonObjMap.getOrElse("NameSpace", "").toString.trim()
            val name = jsonObjMap.getOrElse("Name", "").toString.trim()
            messagesAndContainers += (namespace + "." + name).toLowerCase()
          }
        }
      }
    })

    if (messagesAndContainers.size > 0) {
      logger.debug("Dropping messages/containers:" + messagesAndContainers.mkString(","))
      var executor: ExecutorService = null
      try {
        executor = Executors.newFixedThreadPool(if (_parallelDegree <= 1) 1 else _parallelDegree)
        messagesAndContainers.foreach(dropContainer => {
          executor.execute(new Runnable() {
            override def run() = {
              try {
                _dataStoreDb.DropContainer(Array(dropContainer))
              } catch {
                case e: Exception => AddToGlobalException("Failed to drop data message/container:" + dropContainer, e)
                case e: Throwable => AddToGlobalException("Failed to drop data message/container:" + dropContainer, e)
              }
            }
          })
        })

        executor.shutdown();
        try {
          executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
        } catch {
          case e: Exception => AddToGlobalException("Failed to drop messsages/containers", e)
          case e: Throwable => AddToGlobalException("Failed to drop messsages/containers", e)
        }
      } catch {
        case e: Exception => AddToGlobalException("Failed to drop messsages/containers", e)
        case e: Throwable => AddToGlobalException("Failed to drop messsages/containers", e)
      }

      if (executor != null) {
        executor.shutdown();
        try {
          executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
        } catch {
          case e: Exception => AddToGlobalException("Failed to drop messsages/containers", e)
          case e: Throwable => AddToGlobalException("Failed to drop messsages/containers", e)
        }
      }

      if (LogGlobalException) {
        throw new Exception("Failed to Drop messsages/containers")
      }
    }
  }

  private def WriteStringToFile(flName: String, str: String): Unit = {
    val out = new PrintWriter(flName, "UTF-8")
    try {
      out.print(str)
    } catch {
      case e: Exception => throw e;
      case e: Throwable => throw e;
    } finally {
      out.close
    }
  }

  private def isFailedStatus(retRes: String): Boolean = {
    implicit val formats = org.json4s.DefaultFormats
    val json = org.json4s.jackson.JsonMethods.parse(retRes)
    val statusCodeAny = (json \\ "Status Code").values
    var statusCode = 0
    if (statusCodeAny.isInstanceOf[List[_]]) {
      val t = statusCodeAny.asInstanceOf[List[_]].map(v => v.toString.toInt)
      statusCode = if (t.size > 0) t(0) else -1
    } else if (statusCodeAny.isInstanceOf[Array[_]]) {
      val t = statusCodeAny.asInstanceOf[Array[_]].map(v => v.toString.toInt)
      statusCode = if (t.size > 0) t(0) else -1
    } else {
      statusCode = statusCodeAny.toString.toInt
    }
    /*
    val functionName = (json \\ "Function Name").values.toString
    val resultData = (json \\ "Results Data").values.toString
    val description = (json \\ "Result Description").values.toString
*/
    return (statusCode != 0)
  }

  private def DepJars(depJars1: List[String]): List[String] = {
    val depJars = (depJars1 diff List("methodextractor_2.10-1.0.jar", "methodextractor_2.11-1.0.jar", "log4j-1.2.17.jar", "log4j-1.2.17.jar", "log4j-1.2.16.jar")) // Removing jars which are not valid any more

    // If source is 2.10 and destination is 2.11, then only tranform this. otherwise just leave them as it is.
    if (_fromScalaVersion.equalsIgnoreCase("2.10") && _toScalaVersion.equalsIgnoreCase("2.11")) {
      val depJarsMap = Map("scalap-2.10.0.jar" -> "scalap-2.11.0.jar", "kvbase_2.10-0.1.0.jar" -> "kvbase_2.11-0.1.0.jar", "kamanjautils_2.10-1.0.jar" -> "kamanjautils_2.11-1.0.jar",
        "kamanjabase_2.10-1.0.jar" -> "kamanjabase_2.11-1.0.jar", "customudflib_2.10-1.0.jar" -> "customudflib_2.11-1.0.jar", "pmmlcompiler_2.10-1.0.jar" -> "pmmlcompiler_2.11-1.0.jar",
        "basetypes_2.10-0.1.0.jar" -> "basetypes_2.11-0.1.0.jar", "basefunctions_2.10-0.1.0.jar" -> "basefunctions_2.11-0.1.0.jar", "json4s-core_2.10-3.2.9.jar" -> "json4s-core_2.11-3.2.9.jar",
        "json4s-jackson_2.10-3.2.9.jar" -> "json4s-jackson_2.11-3.2.9.jar", "pmmlruntime_2.10-1.0.jar" -> "pmmlruntime_2.11-1.0.jar", "pmmludfs_2.10-1.0.jar" -> "pmmludfs_2.11-1.0.jar",
        "datadelimiters_2.10-1.0.jar" -> "datadelimiters_2.11-1.0.jar", "metadata_2.10-1.0.jar" -> "metadata_2.11-1.0.jar", "exceptions_2.10-1.0.jar" -> "exceptions_2.11-1.0.jar",
        "json4s-ast_2.10-3.2.9.jar" -> "json4s-ast_2.11-3.2.9.jar", "json4s-native_2.10-3.2.9.jar" -> "json4s-native_2.11-3.2.9.jar", "bootstrap_2.10-1.0.jar" -> "bootstrap_2.11-1.0.jar",
        "messagedef_2.10-1.0.jar" -> "messagedef_2.11-1.0.jar", "guava-16.0.1.jar" -> "guava-14.0.1.jar", "guava-18.0.jar" -> "guava-14.0.1.jar", "guava-19.0.jar" -> "guava-14.0.1.jar")

      val newDeps = depJars.map(d => {
        if (d.startsWith("scala-reflect-2.10")) {
          "scala-reflect-2.11.7.jar"
        } else if (d.startsWith("scala-library-2.10")) {
          "scala-library-2.11.7.jar"
        } else if (d.startsWith("scala-compiler-2.10")) {
          "scala-compiler-2.11.7.jar"
        } else {
          depJarsMap.getOrElse(d, d)
        }
      })

      newDeps
    } else {
      val depJarsMap = Map("guava-16.0.1.jar" -> "guava-14.0.1.jar", "guava-18.0.jar" -> "guava-14.0.1.jar", "guava-19.0.jar" -> "guava-14.0.1.jar")
      val newDeps = depJars.map(d => {
        depJarsMap.getOrElse(d, d)
      })
      newDeps
    }
  }

  private def ProcessObject(mdObjs: ArrayBuffer[(String, Map[String, Any])]): Unit = {
    try {
      mdObjs.foreach(mdObj => {
        val objType = mdObj._1
        var dispkey = ""
        var ver = ""

        try {
          val namespace = mdObj._2.getOrElse("NameSpace", "").toString.trim()
          val name = mdObj._2.getOrElse("Name", "").toString.trim()
          dispkey = (namespace + "." + name).toLowerCase
          ver = mdObj._2.getOrElse("Version", "0.0.1").toString
          val objFormat = mdObj._2.getOrElse("ObjectFormat", "").toString

          objType match {
            case "ModelDef" => {
              val mdlType = mdObj._2.getOrElse("ModelType", "").toString
              val mdlDefStr = mdObj._2.getOrElse("ObjectDefinition", "").toString

              logger.info("Adding model:" + dispkey + ", ModelType:" + mdlType + ", ObjectFormat:" + objFormat)

              if (_sourceVersion.equalsIgnoreCase("1.1") || _sourceVersion.equalsIgnoreCase("1.2")) {
                if ((objFormat.equalsIgnoreCase("JAVA")) || (objFormat.equalsIgnoreCase("scala"))) {
                  val mdlInfo = parse(mdlDefStr).values.asInstanceOf[Map[String, Any]]
                  val defStr = mdlInfo.getOrElse(ModelCompilationConstants.SOURCECODE, "").asInstanceOf[String]
                  // val phyName = mdlInfo.getOrElse(ModelCompilationConstants.PHYSICALNAME, "").asInstanceOf[String]
                  val deps = DepJars(mdlInfo.getOrElse(ModelCompilationConstants.DEPENDENCIES, List[String]()).asInstanceOf[List[String]])
                  val typs = mdlInfo.getOrElse(ModelCompilationConstants.TYPES_DEPENDENCIES, List[String]()).asInstanceOf[List[String]]

                  val cfgnm = "migrationmodelconfig_from_" + _sourceVersion.replace('.', '_') + "_to_1_3";

                  val mdlConfig = (cfgnm ->
                    ("Dependencies" -> deps) ~
                      ("MessageAndContainers" -> typs))

                  var failed = false

                  try {
                    val mdlCfgStr = compact(render(mdlConfig))
                    logger.debug("Temporary Model Config:" + mdlCfgStr)
                    val retRes = MetadataAPIImpl.UploadModelsConfig(mdlCfgStr, defaultUserId, "configuration", true)
                    failed = isFailedStatus(retRes)

                    if (failed == false) {
                      val retRes1 = MetadataAPIImpl.AddModel(MetadataAPI.ModelType.fromString(objFormat), defStr, defaultUserId, Some((defaultUserId.get + "." + cfgnm).toLowerCase), Some(ver))
                      failed = isFailedStatus(retRes1)
                    }
                  } catch {
                    case e: Exception => {
                      logger.error("Failed to add model:" + dispkey, e)
                      failed = true
                    }
                  }

                  if (failed) {
                    var defFl = _unhandledMetadataDumpDir + "/" + objFormat + "_mdldef_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                    var jsonFl = _unhandledMetadataDumpDir + "/" + objFormat + "_mdlinfo_" + dispkey + "." + ver + ".json"

                    val dumpMdlInfoStr = ("ModelInfo" ->
                      ("Dependencies" -> deps) ~
                        ("MessageAndContainers" -> typs) ~
                        ("ModelType" -> mdlType) ~
                        ("ObjectFormat" -> objFormat) ~
                        ("ModelDefinition" -> defStr) ~
                        ("NameSpace" -> namespace) ~
                        ("Name" -> name) ~
                        ("Version" -> ver))

                    WriteStringToFile(defFl, defStr)
                    WriteStringToFile(jsonFl, compact(render(dumpMdlInfoStr)))

                    val msgStr = ("%s type model failed to migrate. Model %s definition is dumped into %s, more model information dumped to %s.".format(objFormat, dispkey, defFl, jsonFl))
                    logger.error(msgStr)
                    _flCurMigrationSummary.println(msgStr)
                    _flCurMigrationSummary.flush()
                    AddedFailedMetadataKey(objType, dispkey, ver)
                  }
                } else if (objFormat.equalsIgnoreCase("XML")) {
                  var defFl = _unhandledMetadataDumpDir + "/kPMML_mdldef_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                  var failed = false

                  try {
                    val retRes = MetadataAPIImpl.AddModel(MetadataAPI.ModelType.fromString("kpmml"), mdlDefStr, defaultUserId, Some(dispkey), Some(ver))
                    failed = isFailedStatus(retRes)
                  } catch {
                    case e: Exception => {
                      logger.error("Failed to add model:" + dispkey, e)
                      failed = true
                    }
                  }
                  if (failed) {
                    WriteStringToFile(defFl, mdlDefStr)
                    val msgStr = ("kPMML type model failed to migrate. Model %s definition is dumped into %s.".format(dispkey, defFl))
                    logger.error(msgStr)
                    _flCurMigrationSummary.println(msgStr)
                    _flCurMigrationSummary.flush()
                    AddedFailedMetadataKey(objType, dispkey, ver)
                  }
                }
              } else {
                logger.error("Not supported any other source migration version other than 1.1 and 1.2")
              }
            }
            case "MessageDef" => {
              val msgDefStr = mdObj._2.getOrElse("ObjectDefinition", "").toString
              if (msgDefStr != null && msgDefStr.size > 0) {
                logger.info("Adding the message:" + dispkey)
                var defFl = _unhandledMetadataDumpDir + "/message_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                var failed = false

                try {
                  val retRes = MetadataAPIImpl.AddMessage(msgDefStr, "JSON", defaultUserId)
                  failed = isFailedStatus(retRes)
                } catch {
                  case e: Exception => {
                    logger.error("Failed to add message:" + dispkey, e)
                    failed = true
                  }
                }
                if (failed) {
                  WriteStringToFile(defFl, msgDefStr)
                  val msgStr = ("Message failed to migrate. Message %s definition is dumped into %s.".format(dispkey, defFl))
                  logger.error(msgStr)
                  _flCurMigrationSummary.println(msgStr)
                  _flCurMigrationSummary.flush()
                  AddedFailedMessageOrContainer(dispkey)
                  AddedFailedMetadataKey(objType, dispkey, ver)
                }
              } else {
                logger.debug("Bootstrap object. Ignore it")
              }
            }
            case "ContainerDef" => {
              logger.debug("Adding the container:" + dispkey)
              val contDefStr = mdObj._2.getOrElse("ObjectDefinition", "").toString
              if (contDefStr != null && contDefStr.size > 0) {
                logger.info("Adding the message: name of the object =>  " + dispkey)
                var defFl = _unhandledMetadataDumpDir + "/container_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                var failed = false

                try {
                  val retRes = MetadataAPIImpl.AddContainer(contDefStr, "JSON", defaultUserId)
                  failed = isFailedStatus(retRes)
                } catch {
                  case e: Exception => {
                    logger.error("Failed to add container:" + dispkey, e)
                    failed = true
                  }
                }
                if (failed) {
                  WriteStringToFile(defFl, contDefStr)
                  val msgStr = ("Container failed to migrate. Container %s definition is dumped into %s.".format(dispkey, defFl))
                  logger.error(msgStr)
                  _flCurMigrationSummary.println(msgStr)
                  _flCurMigrationSummary.flush()
                  AddedFailedMessageOrContainer(dispkey)
                  AddedFailedMetadataKey(objType, dispkey, ver)
                }
              } else {
                logger.debug("Bootstrap object. Ignore it")
              }
            }
            case "ConfigDef" => {
              val mdlCfg = mdObj._2.getOrElse("ObjectDefinition", "").toString
              if (mdlCfg != null && mdlCfg.size > 0) {
                logger.debug("Adding model config:" + dispkey)
                var defFl = _unhandledMetadataDumpDir + "/ModelConfig_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                var failed = false

                try {

                  val cfgmap = parse(mdlCfg).values.asInstanceOf[Map[String, Any]]
                  val changedCfg = cfgmap.map(kv => {
                    val key = kv._1
                    val mdl = kv._2.asInstanceOf[Map[String, Any]]
                    val deps1 = mdl.getOrElse(ModelCompilationConstants.DEPENDENCIES, null)
                    val typs1 = mdl.getOrElse(ModelCompilationConstants.TYPES_DEPENDENCIES, null)
                    val phyNm = mdl.getOrElse(ModelCompilationConstants.PHYSICALNAME, "").toString()

                    val deps = if (deps1 != null) DepJars(deps1.asInstanceOf[List[String]]) else List[String]()
                    val typs = if (typs1 != null) typs1.asInstanceOf[List[String]] else List[String]()

                    (key, Map(ModelCompilationConstants.DEPENDENCIES -> deps, ModelCompilationConstants.TYPES_DEPENDENCIES -> typs, ModelCompilationConstants.PHYSICALNAME -> phyNm))
                  })

                  implicit val jsonFormats: Formats = DefaultFormats
                  val newMdlCfgStr = Serialization.write(changedCfg)
                  val retRes = MetadataAPIImpl.UploadModelsConfig(newMdlCfgStr, Some[String](namespace), null) // Considering namespace as userid
                  failed = isFailedStatus(retRes)
                } catch {
                  case e: Exception => {
                    logger.error("Failed to add model config:" + dispkey, e)
                    failed = true
                  }
                }
                if (failed) {
                  WriteStringToFile(defFl, mdlCfg)
                  val msgStr = ("Model Config failed to migrate. Model Config %s definition is dumped into %s.".format(dispkey, defFl))
                  logger.error(msgStr)
                  _flCurMigrationSummary.println(msgStr)
                  _flCurMigrationSummary.flush()
                  AddedFailedMetadataKey(objType, dispkey, ver)
                }
              }
            }
            case "FunctionDef" => {
              val fnCfg = mdObj._2.getOrElse("ObjectDefinition", "").toString
              if (fnCfg != null && fnCfg.size > 0) {
                logger.debug("Adding model config:" + dispkey)
                var defFl = _unhandledMetadataDumpDir + "/Function_" + dispkey + "." + ver + "." + objFormat.toLowerCase()
                var failed = false

                try {
                  val retRes = MetadataAPIImpl.AddFunctions(fnCfg, "JSON", defaultUserId)
                  failed = isFailedStatus(retRes)
                } catch {
                  case e: Exception => {
                    logger.error("Failed to add function:" + dispkey, e)
                    failed = true
                  }
                }
                if (failed) {
                  WriteStringToFile(defFl, fnCfg)
                  val msgStr = ("Function failed to migrate. Function %s definition is dumped into %s.".format(dispkey, defFl))
                  logger.error(msgStr)
                  _flCurMigrationSummary.println(msgStr)
                  _flCurMigrationSummary.flush()
                  AddedFailedMetadataKey(objType, dispkey, ver)
                }
              }
            }
            /*
            case "JarDef" => {
              logger.debug("Jar")
              logger.debug("Adding Jar:" + dispkey)
              //FIXME:: Yet to handle
              logger.error("Not yet handled migrating JarDef " + objType)
            }
            */
            /*
            case "AttributeDef" => {
              logger.debug("Adding the attribute: name of the object =>  " + dispkey)
            }
            case "ScalarTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "ArrayTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "ArrayBufTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "ListTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "QueueTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "SetTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "TreeSetTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "SortedSetTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "MapTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "ImmutableMapTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "HashMapTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "TupleTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
            case "ContainerTypeDef" => {
              logger.debug("Adding the Type: name of the object =>  " + dispkey)
            }
  */
            case _ => {
              val msgStr = ("Object type %s of key %s did not handle in migrate. Not captured any information related to this.".format(objType, dispkey))
              logger.error(msgStr)
              _flCurMigrationSummary.println(msgStr)
              _flCurMigrationSummary.flush()
              AddedFailedMetadataKey(objType, dispkey, ver)
            }
          }
        } catch {
          case e: Exception => AddMdObjToGlobalException(mdObj, "Failed to add metadata of type " + objType, e); AddedFailedMetadataKey(objType, dispkey, ver)
          case e: Throwable => AddMdObjToGlobalException(mdObj, "Failed to add metadata of type " + objType, e); AddedFailedMetadataKey(objType, dispkey, ver)
        }
      })
    } catch {
      case e: Exception => throw e
      case e: Throwable => throw e
    }
  }

  private var msgsContainersFailed = ArrayBuffer[String]()
  private var msgsContainersFailLock = new Object
  private var mdFailed = ArrayBuffer[FailedMetadataKey]()
  private var mdFailLock = new Object

  private def AddedFailedMessageOrContainer(con: String): Unit = msgsContainersFailLock.synchronized {
    if (con != null)
      msgsContainersFailed += con
  }

  private def AddedFailedMetadataKey(objType: String, mdKey: String, ver: String): Unit = mdFailLock.synchronized {
    if (objType != null && mdKey != null && ver != null)
      mdFailed += new FailedMetadataKey(objType, mdKey, ver)
  }

  private def GetFailedMessageOrContainer: Array[String] = msgsContainersFailLock.synchronized {
    return msgsContainersFailed.toArray
  }

  override def getFailedMetadataKeys(): Array[FailedMetadataKey] = mdFailLock.synchronized {
    mdFailed.toArray
  }

  private def ProcessMdObjectsParallel(mdObjs: ArrayBuffer[(String, Map[String, Any])], errorStr: String): Unit = {
    if (mdObjs.length > 0) {
      var executor: ExecutorService = null
      try {
        executor = Executors.newFixedThreadPool(if (_parallelDegree <= 1) 1 else _parallelDegree)
        mdObjs.foreach(obj => {
          executor.execute(new Runnable() {
            override def run() = {
              try {
                ProcessObject(ArrayBuffer(obj))
              } catch {
                case e: Exception => AddMdObjToGlobalException(obj, errorStr, e)
                case e: Throwable => AddMdObjToGlobalException(obj, errorStr, e)
              }
            }
          })
        })
        executor.shutdown();
        try {
          executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
        } catch {
          case e: Exception => {
            logger.debug("Failed", e)
          }
        }
      } catch {
        case e: Exception => AddToGlobalException("Failed to add metadata", e)
        case e: Throwable => AddToGlobalException("Failed to add metadata", e)
      }

      if (executor != null) {
        executor.shutdown();
        try {
          executor.awaitTermination(Long.MaxValue, TimeUnit.NANOSECONDS);
        } catch {
          case e: Exception => AddToGlobalException("Failed to add metadata", e)
          case e: Throwable => AddToGlobalException("Failed to add metadata", e)
        }
      }

      if (LogGlobalException) {
        throw new Exception("Failed to add metadata")
      }
    }
  }

  override def addMetadata(allMetadataElemsJson: Array[MetadataFormat], uploadClusterConfig: Boolean, excludeMetadata: Array[String]): java.util.List[String] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    val excludedMetadataTypes = if (excludeMetadata != null && excludeMetadata.length > 0) excludeMetadata.map(t => t.toLowerCase.trim).toSet else Set[String]()

    // Order metadata to add in the given order.
    // First get all the message & containers And also the excluded types we automatically add when we add messages & containers
    val allTemp = ArrayBuffer[(String, Map[String, Any])]()
    val types = ArrayBuffer[(String, Map[String, Any])]()
    val messages = ArrayBuffer[(String, Map[String, Any])]()
    val containers = ArrayBuffer[(String, Map[String, Any])]()
    val functions = ArrayBuffer[(String, Map[String, Any])]()
    val mdlConfig = ArrayBuffer[(String, Map[String, Any])]()
    val models = ArrayBuffer[(String, Map[String, Any])]()
    val jarDef = ArrayBuffer[(String, Map[String, Any])]()
    val configDef = ArrayBuffer[(String, Map[String, Any])]()
    val typesToIgnore = scala.collection.mutable.Set[String]()

    val addedMessagesContainers: java.util.List[String] = new java.util.ArrayList[String]()

    allMetadataElemsJson.foreach(mdf => {
      val json = parse(mdf.objDataInJson)
      val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]

      val isActiveStr = jsonObjMap.getOrElse("IsActive", "").toString.trim()
      if (isActiveStr.size > 0) {
        val isActive = jsonObjMap.getOrElse("IsActive", "").toString.trim().toBoolean
        if (isActive) {
          if (mdf.objType == "MessageDef") {

            val namespace = jsonObjMap.getOrElse("NameSpace", "").toString.trim()
            val name = jsonObjMap.getOrElse("Name", "").toString.trim()

            typesToIgnore += (namespace + ".arrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".arraybufferof" + name).toLowerCase
            typesToIgnore += (namespace + ".sortedsetof" + name).toLowerCase
            typesToIgnore += (namespace + ".immutablemapofintarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".immutablemapofstringarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".arrayofarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".mapofstringarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".mapofintarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".setof" + name).toLowerCase
            typesToIgnore += (namespace + ".treesetof" + name).toLowerCase

            if (excludedMetadataTypes.contains(mdf.objType.toLowerCase()) == false) {
              messages += ((mdf.objType, jsonObjMap))
              addedMessagesContainers.add(namespace + "." + name)
            }
          } else if (mdf.objType == "ContainerDef") {

            val namespace = jsonObjMap.getOrElse("NameSpace", "").toString.trim()
            val name = jsonObjMap.getOrElse("Name", "").toString.trim()

            typesToIgnore += (namespace + ".arrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".arraybufferof" + name).toLowerCase
            typesToIgnore += (namespace + ".sortedsetof" + name).toLowerCase
            typesToIgnore += (namespace + ".immutablemapofintarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".immutablemapofstringarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".arrayofarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".mapofstringarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".mapofintarrayof" + name).toLowerCase
            typesToIgnore += (namespace + ".setof" + name).toLowerCase
            typesToIgnore += (namespace + ".treesetof" + name).toLowerCase

            if (excludedMetadataTypes.contains(mdf.objType.toLowerCase()) == false) {
              containers += ((mdf.objType, jsonObjMap))
              addedMessagesContainers.add(namespace + "." + name)
            }
          } else {
            if (excludedMetadataTypes.contains(mdf.objType.toLowerCase()) == false) {
              allTemp += ((mdf.objType, jsonObjMap))
            }
          }
        }
      }
    })

    allTemp.foreach(jsonObjMap => {
      val objType = jsonObjMap._1
      if (objType == "ModelDef") {
        models += jsonObjMap
      } else if (objType == "ArrayTypeDef" ||
        objType == "ArrayBufTypeDef" ||
        objType == "SortedSetTypeDef" ||
        objType == "ImmutableMapTypeDef" ||
        objType == "MapTypeDef" ||
        objType == "HashMapTypeDef" ||
        objType == "SetTypeDef" ||
        objType == "ImmutableSetTypeDef" ||
        objType == "TreeSetTypeDef") {
        val namespace = jsonObjMap._2.getOrElse("NameSpace", "").toString.trim()
        val name = jsonObjMap._2.getOrElse("Name", "").toString.trim()

        val typ = (namespace + "." + name).toLowerCase
        if (typesToIgnore.contains(typ) == false)
          types += jsonObjMap
      } else if (objType == "FunctionDef") {
        functions += jsonObjMap
      } else if (objType == "JarDef") {
        jarDef += jsonObjMap
      } else if (objType == "ConfigDef") {
        configDef += jsonObjMap
      } else {
        logger.error("ObjectType:%s is not handled".format(objType))
      }
    })

    // Open OpenDbStore
    MetadataAPIImpl.OpenDbStore(_jarPaths, _metaDataStoreInfo)

    val cfgStr = Source.fromFile(_clusterConfigFile).mkString
    logger.debug("Uploading configuration")
    MetadataAPIImpl.UploadConfig(cfgStr, defaultUserId, "ClusterConfig")

    // We need to add the metadata in the following order
    // Jars
    // Types
    // Containers
    // Messages
    // Functions
    // Model configuration
    // Models
    // OutputMessageDef

    ProcessObject(jarDef)
    ProcessObject(types)


    if (_mergeContainerAndMessages) {
      val msgsAndContainers = ArrayBuffer[(String, Map[String, Any])]()
      if (containers.length > 0)
        msgsAndContainers ++= containers
      if (messages.length > 0)
        msgsAndContainers ++= messages
      ProcessMdObjectsParallel(msgsAndContainers, "Failed to add messages/container")
    } else {
      ProcessMdObjectsParallel(containers, "Failed to add container")
      ProcessMdObjectsParallel(messages, "Failed to add message")
    }

    val failedMsgsContainer = GetFailedMessageOrContainer

    if (failedMsgsContainer.size > 0) {
      // Report the errors and Stop migration
      val errMsg = "Migrating Messages/Containers (%s) failed".format(failedMsgsContainer.mkString(","))
      logger.error(errMsg)
      throw new Exception(errMsg)
    }

    ProcessObject(functions)
    ProcessObject(configDef)
    //ProcessObject(models)
    ProcessMdObjectsParallel(models, "Failed to add model")

    return addedMessagesContainers
  }

  private def callSaveData(dataStore: DataStoreOperations, data_list: Array[(String, Array[(Key, Value)])]): Unit = {
    var failedWaitTime = 15000 // Wait time starts at 15 secs
    val maxFailedWaitTime = 60000 // Max Wait time 60 secs
    var doneSave = false

    while (!doneSave) {
      try {
        dataStore.put(data_list)
        doneSave = true
      } catch {
        case e: FatalAdapterException => {
          logger.error("Failed to save data into datastore", e)
        }
        case e: StorageDMLException => {
          logger.error("Failed to save data into datastore", e)
        }
        case e: StorageDDLException => {
          logger.error("Failed to save data into datastore", e)
        }
        case e: Exception => {
          logger.error("Failed to save data into datastore", e)
        }
        case e: Throwable => {
          logger.error("Failed to save data into datastore", e)
        }
      }

      if (!doneSave) {
        try {
          logger.error("Failed to save data into datastore. Waiting for another %d milli seconds and going to start them again.".format(failedWaitTime))
          Thread.sleep(failedWaitTime)
        } catch {
          case e: Exception => {
            logger.warn("", e)
          }
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

  private def callGetData(dataStore: DataStoreOperations, containerName: String, keys: Array[Key], callbackFunction: (Key, Value) => Unit): Unit = {
    var failedWaitTime = 15000 // Wait time starts at 15 secs
    val maxFailedWaitTime = 60000 // Max Wait time 60 secs
    var doneGet = false

    while (!doneGet) {
      try {
        dataStore.get(containerName, keys, callbackFunction)
        doneGet = true
      } catch {
        case e@(_: ObjectNotFoundException | _: KeyNotFoundException) => {
          logger.debug("Failed to get data from container:%s".format(containerName))
          doneGet = true
        }
        case e: FatalAdapterException => {
          val stackTrace = StackTrace.ThrowableTraceString(e.cause)
          logger.error("Failed to get data from container:%s.\nStackTrace:%s".format(containerName, stackTrace))
        }
        case e: StorageDMLException => {
          val stackTrace = StackTrace.ThrowableTraceString(e.cause)
          logger.error("Failed to get data from container:%s.\nStackTrace:%s".format(containerName, stackTrace))
        }
        case e: StorageDDLException => {
          val stackTrace = StackTrace.ThrowableTraceString(e.cause)
          logger.error("Failed to get data from container:%s.\nStackTrace:%s".format(containerName, stackTrace))
        }
        case e: Exception => {
          val stackTrace = StackTrace.ThrowableTraceString(e)
          logger.error("Failed to get data from container:%s.\nStackTrace:%s".format(containerName, stackTrace))
        }
        case e: Throwable => {
          val stackTrace = StackTrace.ThrowableTraceString(e)
          logger.error("Failed to get data from container:%s.\nStackTrace:%s".format(containerName, stackTrace))
        }
      }

      if (!doneGet) {
        try {
          logger.error("Failed to get data from datastore. Waiting for another %d milli seconds and going to start them again.".format(failedWaitTime))
          Thread.sleep(failedWaitTime)
        } catch {
          case e: Exception => {}
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

  // Array of tuples has container name, timepartition value, bucketkey, transactionid, rowid, serializername & data in Gson (JSON) format
  override def populateAndSaveData(data: Array[DataFormat]): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    val containersData = data.groupBy(_.containerName.toLowerCase)
    val data_list = containersData.map(kv => (kv._1, kv._2.map(d => (Key(d.timePartition, d.bucketKey, d.transactionid, d.rowid), Value(d.serializername, d.data))).toArray)).toArray

    callSaveData(_dataStoreDb, data_list);
  }

  override def shutdown: Unit = {
    if (_metaDataStoreDb != null)
      _metaDataStoreDb.Shutdown()
    if (_dataStoreDb != null)
      _dataStoreDb.Shutdown()
    if (_statusStoreDb != null)
      _statusStoreDb.Shutdown()
    if (_flCurMigrationSummary != null)
      _flCurMigrationSummary.close()
    _metaDataStoreDb = null
    _dataStoreDb = null
    _statusStoreDb = null
    _flCurMigrationSummary = null
    MetadataAPIImpl.shutdown
  }

  override def getStatusFromDataStore(key: String): String = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    if (_dataStoreDb == null)
      throw new Exception("Not found valid Datastore DB connection")

    var ret = ""
    val buildAdapOne = (k: Key, v: Value) => {
      ret = new String(v.serializedInfo)
    }

    callGetData(_dataStoreDb, "MigrateStatusInformation", Array(Key(KvBaseDefalts.defaultTime, Array(key.toLowerCase), 0, 0)), buildAdapOne)

    ret
  }

  override def setStatusFromDataStore(key: String, value: String) = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    if (_dataStoreDb == null)
      throw new Exception("Not found valid Datastore DB connection")

    callSaveData(_dataStoreDb, Array(("MigrateStatusInformation", Array((Key(KvBaseDefalts.defaultTime, Array(key.toLowerCase), 0, 0), Value("txt", value.getBytes()))))))
  }
}