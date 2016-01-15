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

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.ligadata.Utils._
import com.ligadata.MigrateBase._
import java.io.File
import org.apache.logging.log4j._
import scala.collection.mutable.ArrayBuffer
import com.ligadata.StorageBase.{ Key, Value, IStorage, DataStoreOperations, DataStore, Transaction, StorageAdapterObj }
import com.ligadata.keyvaluestore._
import com.ligadata.Serialize._
import com.ligadata.kamanja.metadata._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import com.ligadata.KamanjaData._
import com.ligadata.KamanjaBase._
import scala.util.control.Breaks._
import com.google.gson.Gson
import com.google.gson.GsonBuilder

object MigrateFrom_V_1_1 extends MigratableFrom {
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private var _srouceInstallPath: String = _
  private var _metadataStoreInfo: String = _
  private var _dataStoreInfo: String = _
  private var _statusStoreInfo: String = _
  private var _bInit = false

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

  private def GetDataStoreHandle(jarPaths: collection.immutable.Set[String], dataStoreInfo: String, tableName: String): DataStore = {
    try {
      logger.info("Getting DB Connection for dataStoreInfo:%s, tableName:%s".format(dataStoreInfo, tableName))
      return KeyValueManager.Get(jarPaths, dataStoreInfo, tableName)
    } catch {
      case e: Exception => {
        throw e
      }
    }
  }

  private def KeyAsStr(k: Key): String = {
    val k1 = k.toArray[Byte]
    new String(k1)
  }

  private def ValueAsStr(v: Value): String = {
    val v1 = v.toArray[Byte]
    new String(v1)
  }

  private def GetObject(key: Key, store: DataStore): IStorage = {
    try {
      object o extends IStorage {
        var key = new Key;
        var value = new Value

        def Key = key

        def Value = value
        def Construct(k: Key, v: Value) = {
          key = k;
          value = v;
        }
      }

      var k = key
      logger.info("Get the object from store, key => " + KeyAsStr(k))
      store.get(k, o)
      o
    } catch {
      case e: Exception => {
        throw e
      }
      case e: Throwable => {
        throw e
      }
    }
  }

  private def GetObject(key: String, store: DataStore): IStorage = {
    var k = new Key
    for (c <- key) {
      k += c.toByte
    }
    GetObject(k, store)
  }

  private def LoadFqJarsIfNeeded(jars: Array[String], loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoaderFrom): Boolean = {
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
            logger.error("Jar " + j.trim + " failed added to class path. Message: " + e.getMessage)
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

  private def GetValidJarFile(jarPaths: collection.immutable.Set[String], jarName: String): String = {
    if (jarPaths == null) return jarName // Returning base jarName if no jarpaths found
    jarPaths.foreach(jPath => {
      val fl = new File(jPath + "/" + jarName)
      if (fl.exists) {
        return fl.getPath
      }
    })
    return jarName // Returning base jarName if not found in jar paths
  }

  private def LoadJarIfNeeded(jsonObjMap: Map[String, Any], loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoaderFrom, jarPaths: collection.immutable.Set[String]): Boolean = {
    if (jarPaths == null) return false

    var retVal: Boolean = true
    var allJars = ArrayBuffer[String]()

    try {
      val jarName = jsonObjMap.getOrElse("JarName", "").toString.trim()
      val dependantJars = jsonObjMap.getOrElse("DependantJars", null)

      if (dependantJars != null) {
        val depJars = dependantJars.asInstanceOf[List[String]]
        if (depJars.size > 0)
          allJars ++= depJars
      }

      if (jarName.size > 0) {
        allJars += jarName
      }

      if (allJars.size > 0) {
        val jars = allJars.map(j => GetValidJarFile(jarPaths, j)).toArray
        return LoadFqJarsIfNeeded(jars, loadedJars, loader)
      }

      return true
    } catch {
      case e: Exception => {
        logger.error("Failed to collect jars", e)
        throw e
      }
    }
    return true
  }

  private def LoadJarIfNeeded(objJson: String, loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoaderFrom, jarPaths: collection.immutable.Set[String]): Boolean = {
    if (jarPaths == null) return false

    var retVal: Boolean = true
    var allJars = ArrayBuffer[String]()

    try {
      implicit val jsonFormats = DefaultFormats
      val json = parse(objJson)
      val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]

      return LoadJarIfNeeded(jsonObjMap, loadedJars, loader, jarPaths)
    } catch {
      case e: Exception => {
        logger.error("Failed to parse JSON" + objJson, e)
        throw e
      }
    }
    return true
  }

  private def getEmptyIfNull(jarName: String): String = {
    if (jarName != null) jarName else ""
  }

  private def serializeObjectToJson(mdObj: BaseElem): (String, String) = {
    try {
      mdObj match {
        // Assuming that zookeeper transaction will be different based on type of object
        case o: ModelDef => {
          val json = (("ObjectType" -> "ModelDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ModelDef", compact(render(json)))
        }
        case o: MessageDef => {
          val json = (("ObjectType" -> "MessageDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("MessageDef", compact(render(json)))
        }
        case o: MappedMsgTypeDef => {
          val json = (("ObjectType" -> "MappedMsgTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("MappedMsgTypeDef", compact(render(json)))
        }
        case o: StructTypeDef => {
          val json = (("ObjectType" -> "StructTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("StructTypeDef", compact(render(json)))
        }
        case o: ContainerDef => {
          val json = (("ObjectType" -> "ContainerDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ContainerDef", compact(render(json)))
        }
        case o: FunctionDef => {
          val json = (("ObjectType" -> "FunctionDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("FunctionDef", compact(render(json)))
        }
        case o: ArrayTypeDef => {
          val json = (("ObjectType" -> "ArrayTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ArrayTypeDef", compact(render(json)))
        }
        case o: ArrayBufTypeDef => {
          val json = (("ObjectType" -> "ArrayBufTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ArrayBufTypeDef", compact(render(json)))
        }
        case o: SortedSetTypeDef => {
          val json = (("ObjectType" -> "SortedSetTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("SortedSetTypeDef", compact(render(json)))
        }
        case o: ImmutableMapTypeDef => {
          val json = (("ObjectType" -> "ImmutableMapTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ImmutableMapTypeDef", compact(render(json)))
        }
        case o: MapTypeDef => {
          val json = (("ObjectType" -> "MapTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("MapTypeDef", compact(render(json)))
        }
        case o: HashMapTypeDef => {
          val json = (("ObjectType" -> "HashMapTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("HashMapTypeDef", compact(render(json)))
        }
        case o: SetTypeDef => {
          val json = (("ObjectType" -> "SetTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("SetTypeDef", compact(render(json)))
        }
        case o: ImmutableSetTypeDef => {
          val json = (("ObjectType" -> "ImmutableSetTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ImmutableSetTypeDef", compact(render(json)))
        }
        case o: TreeSetTypeDef => {
          val json = (("ObjectType" -> "TreeSetTypeDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("TreeSetTypeDef", compact(render(json)))
        }
        case o: JarDef => {
          val json = (("ObjectType" -> "JarDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("JarDef", compact(render(json)))
        }
        case o: OutputMsgDef => {
          val json = (("ObjectType" -> "OutputMsgDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("OutputMsgDef", compact(render(json)))
        }
        case o: ConfigDef => {
          val json = (("ObjectType" -> "ConfigDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> o.ObjectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.NameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> "0") ~
            ("PhysicalName" -> "") ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> List[String]()) ~
            ("ConfigContnent" -> o.contents))
          ("ConfigDef", compact(render(json)))
        }
        case _ => {
          throw new Exception("serializeObjectToJson doesn't support the objects of type objectType of " + mdObj.getClass().getName() + " yet.")
        }
      }
    } catch {
      case e: Exception => {
        logger.debug("Failed to serialize", e)
        throw e
      }
    }
  }

  override def init(srouceInstallPath: String, metadataStoreInfo: String, dataStoreInfo: String, statusStoreInfo: String): Unit = {
    isValidPath(srouceInstallPath, true, false, "srouceInstallPath")
    isValidPath(srouceInstallPath + "/bin", true, false, "bin folder in srouceInstallPath")
    isValidPath(srouceInstallPath + "/lib/system", true, false, "/lib/system folder in srouceInstallPath")
    isValidPath(srouceInstallPath + "/lib/application", true, false, "/lib/application folder in srouceInstallPath")

    _srouceInstallPath = srouceInstallPath
    _metadataStoreInfo = metadataStoreInfo
    _dataStoreInfo = dataStoreInfo
    _statusStoreInfo = statusStoreInfo
    _bInit = true
  }

  override def isInitialized: Boolean = _bInit

  override def getAllMetadataTableNames: Array[String] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array("config_objects", "jar_store", "metadata_objects", "model_config_objects", "transaction_id")
  }

  override def getAllDataTableNames: Array[String] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array("AllData", "ClusterCounts")
  }

  override def getAllStatusTableNames: Array[String] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array("CommmittingTransactions", "checkPointAdapInfo")
  }

  override def getAllMetadataDataStatusTableNames: Array[String] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array("config_objects", "jar_store", "metadata_objects", "model_config_objects", "transaction_id", "AllData", "ClusterCounts", "CommmittingTransactions", "checkPointAdapInfo")
  }

  // Callback function calls with metadata Object Type & metadata information in JSON string
  override def getAllMetadataObjs(backupTblSufix: String, callbackFunction: MetadataObjectCallBack): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    val installPath = new File(_srouceInstallPath)

    var fromVersionInstallationPath = installPath.getAbsolutePath

    val sysPath = new File(_srouceInstallPath + "/lib/system")
    val appPath = new File(_srouceInstallPath + "/lib/application")

    val fromVersionJarPaths = collection.immutable.Set[String](sysPath.getAbsolutePath, appPath.getAbsolutePath)

    logger.info("fromVersionInstallationPath:%s, fromVersionJarPaths:%s".format(fromVersionInstallationPath, fromVersionJarPaths.mkString(",")))

    val dir = new File(fromVersionInstallationPath + "/bin");

    val mdapiFls = dir.listFiles.filter(_.isFile).filter(_.getName.startsWith("MetadataAPI-")).toList

    var baseFileToLoadFromPrevVer = ""
    if (mdapiFls.size == 0) {
      val kmFls = dir.listFiles.filter(_.isFile).filter(_.getName.startsWith("KamanjaManager-")).toList
      if (kmFls.size == 0) {
        val szMsg = "Not found %s/bin/MetadataAPI-* and %s/bin/KamanjaManager-*".format(fromVersionInstallationPath, fromVersionInstallationPath)
        logger.error(szMsg)
        throw new Exception(szMsg)
      } else {
        baseFileToLoadFromPrevVer = kmFls(0).getAbsolutePath
      }
    } else {
      baseFileToLoadFromPrevVer = mdapiFls(0).getAbsolutePath
    }
    // Loading the base file where we have all the base classes like classes from KamanjaBase, metadata, MetadataAPI, etc
    if (baseFileToLoadFromPrevVer != null && baseFileToLoadFromPrevVer.size > 0)
      LoadFqJarsIfNeeded(Array(baseFileToLoadFromPrevVer), MdResolve._kamanjaLoader.loadedJars, MdResolve._kamanjaLoader.loader)

    if (MdResolve._kryoDataSer != null) {
      MdResolve._kryoDataSer.SetClassLoader(MdResolve._kamanjaLoader.loader)
    }

    val metadataStore = GetDataStoreHandle(fromVersionJarPaths, _metadataStoreInfo, "metadata_objects" + backupTblSufix)

    try {
      // Load all metadata objects
      var keys = scala.collection.mutable.Set[Key]()
      metadataStore.getAllKeys({ (key: Key) => keys.add(key) })
      val keyArray = keys.toArray
      if (keyArray.length == 0) {
        val szMsg = "No objects available in the Database"
        logger.error(szMsg)
        throw new Exception(szMsg)
      }

      val allObjs = ArrayBuffer[Value]()
      keyArray.foreach(key => {
        val obj = GetObject(key, metadataStore)
        allObjs += obj.Value
      })

      allObjs.foreach(o => {
        val mObj = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(o.toArray[Byte]).asInstanceOf[BaseElem]
        try {
          val (typ, jsonStr) = serializeObjectToJson(mObj)
          if (callbackFunction != null) {
            val retVal = callbackFunction.call(new MetadataFormat(typ, jsonStr))
            if (retVal == false) {
              return
            }
          }
        } catch {
          case e: Exception => {

          }
        }
      })
    } catch {
      case e: Exception => {
        throw new Exception("Failed to load metadata objects", e)
      }
    } finally {
      if (metadataStore != null)
        metadataStore.Shutdown()
    }
  }

  private[this] var _serInfoBufBytes = 32

  private def getSerializeInfo(tupleBytes: Value): String = {
    if (tupleBytes.size < _serInfoBufBytes) return ""
    val serInfoBytes = new Array[Byte](_serInfoBufBytes)
    tupleBytes.copyToArray(serInfoBytes, 0, _serInfoBufBytes)
    return (new String(serInfoBytes)).trim
  }

  private def getValueInfo(tupleBytes: Value): Array[Byte] = {
    if (tupleBytes.size < _serInfoBufBytes) return null
    val valInfoBytes = new Array[Byte](tupleBytes.size - _serInfoBufBytes)
    Array.copy(tupleBytes.toArray, _serInfoBufBytes, valInfoBytes, 0, tupleBytes.size - _serInfoBufBytes)
    valInfoBytes
  }

  private def isDerivedFrom(clz: Class[_], clsName: String): Boolean = {
    var isIt: Boolean = false

    val interfecs = clz.getInterfaces()
    logger.debug("Interfaces => " + interfecs.length + ",isDerivedFrom: Class=>" + clsName)

    breakable {
      for (intf <- interfecs) {
        val intfName = intf.getName()
        logger.debug("Interface:" + intfName)
        if (intfName.equals(clsName)) {
          isIt = true
          break
        }
      }
    }

    if (isIt == false) {
      val superclass = clz.getSuperclass
      if (superclass != null) {
        val scName = superclass.getName()
        logger.debug("SuperClass => " + scName)
        if (scName.equals(clsName)) {
          isIt = true
        }
      }
    }

    isIt
  }

  object MdResolve extends MdBaseResolveInfo {
    val _messagesAndContainers = scala.collection.mutable.Map[String, MessageContainerObjBase]()
    val _gson = new Gson();
    val _kamanjaLoader = new KamanjaLoaderInfoFrom
    val _kryoDataSer = SerializerManager.GetSerializer("kryo")
    if (_kryoDataSer != null) {
      _kryoDataSer.SetClassLoader(_kamanjaLoader.loader)
    }

    private val _dataFoundButNoMetadata = scala.collection.mutable.Set[String]()

    def DataFoundButNoMetadata = _dataFoundButNoMetadata.toArray

    def AddMessageOrContianer(objType: String, jsonObjMap: Map[String, Any], jarPaths: collection.immutable.Set[String]): Unit = {
      var isOk = true

      try {
        val objNameSpace = jsonObjMap.getOrElse("NameSpace", "").toString.trim()
        val objName = jsonObjMap.getOrElse("Name", "").toString.trim()
        val objVer = jsonObjMap.getOrElse("Version", "").toString.trim()

        val objFullName = (objNameSpace + "." + objName).toLowerCase
        val physicalName = jsonObjMap.getOrElse("PhysicalName", "").toString.trim()

        var isMsg = false
        var isContainer = false

        if (isOk) {
          isOk = LoadJarIfNeeded(jsonObjMap, _kamanjaLoader.loadedJars, _kamanjaLoader.loader, jarPaths)
        }

        if (isOk) {
          var clsName = physicalName
          if (clsName.size > 0 && clsName.charAt(clsName.size - 1) != '$') // if no $ at the end we are taking $
            clsName = clsName + "$"

          if (isMsg == false) {
            // Checking for Message
            try {
              // Convert class name into a class
              var curClz = Class.forName(clsName, true, _kamanjaLoader.loader)

              while (curClz != null && isContainer == false) {
                isContainer = isDerivedFrom(curClz, "com.ligadata.KamanjaBase.BaseContainerObj")
                if (isContainer == false)
                  curClz = curClz.getSuperclass()
              }
            } catch {
              case e: Exception => {
                logger.error("Failed to load message class %s with Reason:%s Message:%s".format(clsName, e.getCause, e.getMessage))
              }
            }
          }

          if (isContainer == false) {
            // Checking for container
            try {
              // If required we need to enable this test
              // Convert class name into a class
              var curClz = Class.forName(clsName, true, _kamanjaLoader.loader)

              while (curClz != null && isMsg == false) {
                isMsg = isDerivedFrom(curClz, "com.ligadata.KamanjaBase.BaseMsgObj")
                if (isMsg == false)
                  curClz = curClz.getSuperclass()
              }
            } catch {
              case e: Exception => {
                logger.error("Failed to load container class %s with Reason:%s Message:%s".format(clsName, e.getCause, e.getMessage))
              }
            }
          }

          logger.debug("isMsg:%s, isContainer:%s".format(isMsg, isContainer))

          if (isMsg || isContainer) {
            try {
              val mirror = _kamanjaLoader.mirror
              val module = mirror.staticModule(clsName)
              val obj = mirror.reflectModule(module)
              val objinst = obj.instance

              if (isMsg) {
                // objinst
              } else {

              }

              if (objinst.isInstanceOf[BaseMsgObj]) {
                val messageObj = objinst.asInstanceOf[BaseMsgObj]
                logger.debug("Created Message Object")
                _messagesAndContainers(objFullName) = messageObj
              } else if (objinst.isInstanceOf[BaseContainerObj]) {
                val containerObj = objinst.asInstanceOf[BaseContainerObj]
                logger.debug("Created Container Object")
                _messagesAndContainers(objFullName) = containerObj
              } else {
                logger.error("Failed to instantiate message or conatiner object :" + clsName)
                isOk = false
              }
            } catch {
              case e: Exception => {
                logger.error("Failed to instantiate message or conatiner object:" + clsName + ". Reason:" + e.getCause + ". Message:" + e.getMessage())
                isOk = false
              }
            }
          } else {
            logger.error("Failed to instantiate message or conatiner object :" + clsName)
            isOk = false
          }
        }
        if (isOk == false) {
          logger.error("Failed to add message or conatiner object. NameSpace:%s, Name:%s, Version:%s".format(objNameSpace, objName, objVer))
        }
      } catch {
        case e: Exception => {
          logger.error("Failed to Add Message/Contianer", e)
          throw e
        }
      }
    }

    def AddMessageOrContianer(objType: String, objJson: String, jarPaths: collection.immutable.Set[String]): Unit = {
      try {
        implicit val jsonFormats = DefaultFormats
        val json = parse(objJson)
        val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]
        AddMessageOrContianer(objType, jsonObjMap, jarPaths)
      } catch {
        case e: Exception => {
          logger.error("Failed to Add Message/Contianer", e)
          throw e
        }
      }
    }

    override def getMessgeOrContainerInstance(msgContainerType: String): MessageContainerBase = {
      val nm = msgContainerType.toLowerCase()
      val v = _messagesAndContainers.getOrElse(nm, null)
      if (v != null && v.isInstanceOf[BaseMsgObj]) {
        return v.asInstanceOf[BaseMsgObj].CreateNewMessage
      } else if (v != null && v.isInstanceOf[BaseContainerObj]) {
        return v.asInstanceOf[BaseContainerObj].CreateNewContainer
      }
      logger.error("getMessgeOrContainerInstance not found:%s. All List:%s".format(msgContainerType, _messagesAndContainers.map(kv => kv._1).mkString(",")))
      _dataFoundButNoMetadata += nm
      return null
    }
  }

  private def ExtractDataFromTypleData(tupleBytes: Value): Array[DataFormat] = {
    // Get first _serInfoBufBytes bytes
    if (tupleBytes.size < _serInfoBufBytes) {
      val errMsg = s"Invalid input. This has only ${tupleBytes.size} bytes data. But we are expecting serializer buffer bytes as of size ${_serInfoBufBytes}"
      logger.error(errMsg)
      throw new Exception(errMsg)
    }

    val serInfo = getSerializeInfo(tupleBytes).toLowerCase()
    var kd: KamanjaData = null

    serInfo match {
      case "kryo" => {
        val valInfo = getValueInfo(tupleBytes)
        kd = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(valInfo).asInstanceOf[KamanjaData]
      }
      case "manual" => {
        val valInfo = getValueInfo(tupleBytes)
        val datarec = new KamanjaData
        datarec.DeserializeData(valInfo, MdResolve, MdResolve._kamanjaLoader.loader)
        kd = datarec
      }
      case _ => {
        throw new Exception("Found un-handled Serializer Info: " + serInfo)
      }
    }

    if (kd != null) {
      val typName = kd.GetTypeName
      val bucketKey = kd.GetKey
      val data = kd.GetAllData

      // container name, timepartition value, bucketkey, transactionid, rowid, serializername & data in Gson (JSON) format.
      data.map(d => new DataFormat(typName, 0, bucketKey, d.TransactionId(), 0, serInfo, MdResolve._gson.toJson(d)))
    }

    return Array[DataFormat]()
  }

  private def AddActiveMessageOrContianer(metadataElemsJson: Array[MetadataFormat], jarPaths: collection.immutable.Set[String]): Unit = {
    try {
      implicit val jsonFormats = DefaultFormats
      metadataElemsJson.foreach(mdElem => {
        if (mdElem.objType.compareToIgnoreCase("MessageDef") == 0 || mdElem.objType.compareToIgnoreCase("MappedMsgTypeDef") == 0 ||
          mdElem.objType.compareToIgnoreCase("StructTypeDef") == 0 || mdElem.objType.compareToIgnoreCase("ContainerDef") == 0) {
          val json = parse(mdElem.objDataInJson)
          val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]
          val isActiveStr = jsonObjMap.getOrElse("IsActive", "").toString.trim()
          if (isActiveStr.size > 0) {
            val isActive = jsonObjMap.getOrElse("IsActive", "").toString.trim().toBoolean
            if (isActive)
              MdResolve.AddMessageOrContianer(mdElem.objType, jsonObjMap, jarPaths)
          } else {
            val objNameSpace = jsonObjMap.getOrElse("NameSpace", "").toString.trim()
            val objName = jsonObjMap.getOrElse("Name", "").toString.trim()
            val objVer = jsonObjMap.getOrElse("Version", "").toString.trim()
            logger.warn("message or conatiner of this version is not active. So, ignoring to migrate data for this. NameSpace:%s, Name:%s, Version:%s".format(objNameSpace, objName, objVer))
          }
        }
      })
    } catch {
      case e: Exception => {
        logger.error("Failed to Add Message/Contianer", e)
        throw e
      }
    }
  }

  // metadataElemsJson are used for dependency load
  // Callback function calls with container name, timepartition value, bucketkey, transactionid, rowid, serializername & data in Gson (JSON) format.
  override def getAllDataObjs(backupTblSufix: String, metadataElemsJson: Array[MetadataFormat], callbackFunction: DataObjectCallBack): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    val installPath = new File(_srouceInstallPath)

    var fromVersionInstallationPath = installPath.getAbsolutePath

    val sysPath = new File(_srouceInstallPath + "/lib/system")
    val appPath = new File(_srouceInstallPath + "/lib/application")

    val fromVersionJarPaths = collection.immutable.Set[String](sysPath.getAbsolutePath, appPath.getAbsolutePath)

    logger.info("fromVersionInstallationPath:%s, fromVersionJarPaths:%s".format(fromVersionInstallationPath, fromVersionJarPaths.mkString(",")))

    val dir = new File(fromVersionInstallationPath + "/bin");

    val mdapiFls = dir.listFiles.filter(_.isFile).filter(_.getName.startsWith("MetadataAPI-")).toList

    var baseFileToLoadFromPrevVer = ""
    if (mdapiFls.size == 0) {
      val kmFls = dir.listFiles.filter(_.isFile).filter(_.getName.startsWith("KamanjaManager-")).toList
      if (kmFls.size == 0) {
        val szMsg = "Not found %s/bin/MetadataAPI-* and %s/bin/KamanjaManager-*".format(fromVersionInstallationPath, fromVersionInstallationPath)
        logger.error(szMsg)
        throw new Exception(szMsg)
      } else {
        baseFileToLoadFromPrevVer = kmFls(0).getAbsolutePath
      }
    } else {
      baseFileToLoadFromPrevVer = mdapiFls(0).getAbsolutePath
    }
    // Loading the base file where we have all the base classes like classes from KamanjaBase, metadata, MetadataAPI, etc
    if (baseFileToLoadFromPrevVer != null && baseFileToLoadFromPrevVer.size > 0)
      LoadFqJarsIfNeeded(Array(baseFileToLoadFromPrevVer), MdResolve._kamanjaLoader.loadedJars, MdResolve._kamanjaLoader.loader)

    AddActiveMessageOrContianer(metadataElemsJson, fromVersionJarPaths)

    val dataStore = GetDataStoreHandle(fromVersionJarPaths, _dataStoreInfo, "AllData" + backupTblSufix)

    if (MdResolve._kryoDataSer != null) {
      MdResolve._kryoDataSer.SetClassLoader(MdResolve._kamanjaLoader.loader)
    }

    logger.error("==================================>All List:%s".format(MdResolve._messagesAndContainers.map(kv => kv._1).mkString(",")))

    try {
      // Load all metadata objects
      var keys = scala.collection.mutable.Set[Key]()
      dataStore.getAllKeys({ (key: Key) => keys.add(key) })
      val keyArray = keys.toArray
      if (keyArray.length == 0) {
        val szMsg = "No objects available in the Database"
        logger.error(szMsg)
        throw new Exception(szMsg)
      }

      keyArray.foreach(key => {
        val obj = GetObject(key, dataStore)
        val retData = ExtractDataFromTypleData(obj.Value)
        if (retData.size > 0 && callbackFunction != null) {
          callbackFunction.call(retData)
        }
      })
    } catch {
      case e: Exception => {
        throw new Exception("Failed to get data", e)
      }
    } finally {
      if (dataStore != null)
        dataStore.Shutdown()
    }
  }

  override def shutdown: Unit = {

  }
}
