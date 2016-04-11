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

import com.ligadata.Utils._
import com.ligadata.MigrateBase._
import java.io.{ DataOutputStream, ByteArrayOutputStream, File, PrintWriter }
import org.apache.logging.log4j._
import scala.collection.mutable.ArrayBuffer
import com.ligadata.KvBase.{ Key, Value /*, TimeRange, KvBaseDefalts, KeyWithBucketIdAndPrimaryKey, KeyWithBucketIdAndPrimaryKeyCompHelper, LoadKeyWithBucketId */ }
import com.ligadata.StorageBase._
import com.ligadata.keyvaluestore._
import com.ligadata.Serialize._
import com.ligadata.kamanja.metadata._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import com.ligadata.KamanjaBase._
import scala.util.control.Breaks._
import scala.reflect.runtime.{ universe => ru }
import com.ligadata.Exceptions._
// import scala.collection.JavaConversions._

class MigrateFrom_V_1_3 extends MigratableFrom {

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private var _srouceInstallPath: String = _
  private var _metadataStoreInfo: String = _
  private var _dataStoreInfo: String = _
  private var _statusStoreInfo: String = _
  private var _metadataStore: DataStore = _
  private var _sourceReadFailuresFilePath: String = _
  private var _flReadFailures: PrintWriter = _
  private var _bInit = false

  private def dumpKeyValueFailures(k: Key, v: Value): Unit = {
    val keyStr = "%d~%s~%d~%d".format(k.timePartition, k.bucketKey.mkString(","), k.transactionId, k.rowId)
    val msgStr = ("Failed to deserialize key: %s. Written the data related to this key in file:%s".format(keyStr, _sourceReadFailuresFilePath))
    logger.error(msgStr)
    _flReadFailures.println("Key:\n" + keyStr)
    _flReadFailures.println("Record:")
    _flReadFailures.println("SerializeType:\n" + v.serializerType + "\nSerializedInfo:")
    _flReadFailures.println(v.serializedInfo)
    _flReadFailures.flush()
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

  private def GetDataStoreHandle(jarPaths: collection.immutable.Set[String], dataStoreInfo: String): DataStore = {
    try {
      logger.debug("Getting DB Connection for dataStoreInfo:%s".format(dataStoreInfo))
      return KeyValueManager.Get(jarPaths, dataStoreInfo)
    } catch {
      case e: Exception => throw e
      case e: Throwable => throw e
    }
  }

  private def LoadFqJarsIfNeeded(jars: Array[String], loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoader): Boolean = {
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

  private def LoadJarIfNeeded(jsonObjMap: Map[String, Any], loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoader, jarPaths: collection.immutable.Set[String]): Boolean = {
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

  private def LoadJarIfNeeded(objJson: String, loadedJars: scala.collection.mutable.TreeSet[String], loader: KamanjaClassLoader, jarPaths: collection.immutable.Set[String]): Boolean = {
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
    logger.debug("mdObj.Version before conversion =>" + mdObj.Version)
    val ver = MdMgr.ConvertLongVersionToString(mdObj.Version)
    logger.debug("mdObj.Version after conversion  =>" + ver)
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
            ("ModelType" -> o.miningModelType.toString) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("MessageDef", compact(render(json)))
        }
        /*
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("StructTypeDef", compact(render(json)))
        }
        */
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
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("ContainerDef", compact(render(json)))
        }
        case o: FunctionDef => {
          val args = if (o.args == null || o.args.size == 0) List[(String, String, String)]() else o.args.map(arg => (arg.name, arg.aType.NameSpace, arg.aType.Name)).toList
          val features = if (o.features == null || o.features.size == 0) List[String]() else o.features.map(f => f.toString()).toList

          val fnDefJson =
            ("Functions" ->
              List(("NameSpace" -> o.nameSpace) ~
                ("Name" -> o.name) ~
                ("PhysicalName" -> o.physicalName) ~
                ("ReturnTypeNameSpace" -> o.retType.nameSpace) ~
                ("ReturnTypeName" -> o.retType.name) ~
                ("Arguments" -> args.map(arg => ("ArgName" -> arg._1) ~ ("ArgTypeNameSpace" -> arg._2) ~ ("ArgTypeName" -> arg._3))) ~
                ("Features" -> features) ~
                ("Version" -> ver) ~
                ("JarName" -> getEmptyIfNull(o.jarName)) ~
                ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList)))

          val fnDefStr = compact(render(fnDefJson))

          val json = (("ObjectType" -> "FunctionDef") ~
            ("IsActive" -> o.IsActive.toString) ~
            ("IsDeleted" -> o.IsDeleted.toString) ~
            ("TransId" -> o.TranId.toString) ~
            ("OrigDef" -> o.OrigDef) ~
            ("ObjectDefinition" -> fnDefStr) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.ObjectFormat)) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
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
            ("Version" -> ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> getEmptyIfNull(o.jarName)) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          ("JarDef", compact(render(json)))
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

  override def init(srouceInstallPath: String, metadataStoreInfo: String, dataStoreInfo: String, statusStoreInfo: String, sourceReadFailuresFilePath: String): Unit = {
    isValidPath(srouceInstallPath, true, false, "srouceInstallPath")
    isValidPath(srouceInstallPath + "/bin", true, false, "bin folder in srouceInstallPath")
    isValidPath(srouceInstallPath + "/lib/system", true, false, "/lib/system folder in srouceInstallPath")
    isValidPath(srouceInstallPath + "/lib/application", true, false, "/lib/application folder in srouceInstallPath")

    _srouceInstallPath = srouceInstallPath
    _metadataStoreInfo = metadataStoreInfo
    _dataStoreInfo = dataStoreInfo
    _statusStoreInfo = statusStoreInfo
    _sourceReadFailuresFilePath = sourceReadFailuresFilePath

    _flReadFailures = new PrintWriter(_sourceReadFailuresFilePath, "UTF-8")

    _bInit = true
  }

  override def isInitialized: Boolean = _bInit

  override def getAllMetadataTableNames: Array[TableName] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    if (_metadataStoreInfo.trim.size == 0)
      return Array[TableName]()

    var parsed_json: Map[String, Any] = null
    try {
      val json = parse(_metadataStoreInfo)
      if (json == null || json.values == null) {
        val msg = "Failed to parse JSON configuration string:" + _metadataStoreInfo
        throw new Exception(msg)
      }
      parsed_json = json.values.asInstanceOf[Map[String, Any]]
    } catch {
      case e: Exception => {
        throw new Exception("Failed to parse JSON configuration string:" + _metadataStoreInfo, e)
      }
    }

    val namespace = if (parsed_json.contains("SchemaName")) parsed_json.getOrElse("SchemaName", "default").toString.trim else parsed_json.getOrElse("SchemaName", "default").toString.trim
    Array(new TableName(namespace, "config_objects"), new TableName(namespace, "jar_store"), new TableName(namespace, "metadata_objects"),
      new TableName(namespace, "model_config_objects"), new TableName(namespace, "transaction_id"))
  }

  override def getAllDataTableNames: Array[TableName] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array[TableName]()
  }

  override def getAllStatusTableNames: Array[TableName] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    Array[TableName]()
  }

  // BUGBUG:: We can make all gets as simple template for exceptions handling and call that.
  private def callGetData(dataStore: DataStoreOperations, containerName: String, callbackFunction: (Key, Value) => Unit): Unit = {
    var failedWaitTime = 15000 // Wait time starts at 15 secs
    val maxFailedWaitTime = 60000 // Max Wait time 60 secs
    var doneGet = false

    while (!doneGet) {
      try {
        dataStore.get(containerName, callbackFunction)
        doneGet = true
      } catch {
        case e @ (_: ObjectNotFoundException | _: KeyNotFoundException) => {
          logger.debug("Failed to get data from container:%s".format(containerName), e)
          doneGet = true
        }
        case e: FatalAdapterException => {
          logger.error("Failed to get data from container:%s.".format(containerName), e)
        }
        case e: StorageDMLException => {
          logger.error("Failed to get data from container:%s.".format(containerName), e)
        }
        case e: StorageDDLException => {
          logger.error("Failed to get data from container:%s.".format(containerName), e)
        }
        case e: Exception => {
          logger.error("Failed to get data from container:%s.".format(containerName), e)
        }
        case e: Throwable => {
          logger.error("Failed to get data from container:%s.".format(containerName), e)
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

  private def SplitFullName(mdName: String): (String, String) = {
    val buffer: StringBuilder = new StringBuilder
    val nameNodes: Array[String] = mdName.split('.')
    val name: String = nameNodes.last
    if (nameNodes.size > 1)
      nameNodes.take(nameNodes.size - 1).addString(buffer, ".")
    val namespace: String = buffer.toString
    (namespace, name)
  }

  // Callback function calls with metadata Object Type & metadata information in JSON string
  override def getAllMetadataObjs(backupTblSufix: String, callbackFunction: MetadataObjectCallBack, excludeMetadata: Array[String]): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    val excludedMetadataTypes = if (excludeMetadata != null && excludeMetadata.length > 0) excludeMetadata.map(t => t.toLowerCase.trim).toSet else Set[String]()

    val installPath = new File(_srouceInstallPath)

    var fromVersionInstallationPath = installPath.getAbsolutePath

    val sysPath = new File(_srouceInstallPath + "/lib/system")
    val appPath = new File(_srouceInstallPath + "/lib/application")

    val fromVersionJarPaths = collection.immutable.Set[String](sysPath.getAbsolutePath, appPath.getAbsolutePath)

    logger.info("fromVersionInstallationPath:%s, fromVersionJarPaths:%s".format(fromVersionInstallationPath, fromVersionJarPaths.mkString(",")))

    val kamanjaLoader = new KamanjaLoaderInfo
    val kryoDataSer = SerializerManager.GetSerializer("kryo")
    if (kryoDataSer != null) {
      kryoDataSer.SetClassLoader(kamanjaLoader.loader)
    }

    _metadataStore = GetDataStoreHandle(fromVersionJarPaths, _metadataStoreInfo)

    try {
      // Load all metadata objects
      val buildMdlOne = (k: Key, v: Value) => {
        try {
          val mObj = kryoDataSer.DeserializeObjectFromByteArray(v.serializedInfo).asInstanceOf[BaseElemDef]
          try {
            val (typ, jsonStr) = serializeObjectToJson(mObj)
            if (excludedMetadataTypes.contains(typ.toLowerCase()) == false) {
              if (callbackFunction != null) {
                val retVal = callbackFunction.call(new MetadataFormat(typ, jsonStr))
                if (retVal == false) {
                  return
                }
              }
            }
          } catch {
            case e: Exception => { dumpKeyValueFailures(k, v); logger.warn("", e); }
            case e: Throwable => { dumpKeyValueFailures(k, v); logger.warn("", e); }
          }
        } catch {
          case e: Exception => { dumpKeyValueFailures(k, v); logger.warn("", e); }
          case e: Throwable => { dumpKeyValueFailures(k, v); logger.warn("", e); }
        }
      }

      callGetData(_metadataStore, "metadata_objects" + backupTblSufix, buildMdlOne)

      logger.debug("Collected all metadata objects from " + "metadata_objects" + backupTblSufix)

      if (excludedMetadataTypes.contains("ConfigDef".toLowerCase()) == false) {
        logger.debug("Collecting all model configuration objects from " + "model_config_objects" + backupTblSufix)
        val buildMdlCfglOne = (k: Key, v: Value) => {
          try {
            val conf = kryoDataSer.DeserializeObjectFromByteArray(v.serializedInfo).asInstanceOf[Map[String, Any]]

            val (nameSpace, name) = SplitFullName(k.bucketKey(0))

            implicit val jsonFormats: Formats = DefaultFormats
            val str = "{\"" + name + "\" :" + Serialization.write(conf) + "}"

            val json = (("ObjectType" -> "ConfigDef") ~
              ("IsActive" -> "true") ~
              ("IsDeleted" -> "false") ~
              ("TransId" -> "0") ~
              ("OrigDef" -> "") ~
              ("ObjectDefinition" -> str) ~
              ("ObjectFormat" -> "JSON") ~
              ("NameSpace" -> nameSpace) ~
              ("Name" -> name) ~
              ("Version" -> "0") ~
              ("PhysicalName" -> "") ~
              ("JarName" -> "") ~
              ("DependantJars" -> List[String]()))

            val mdlCfg = compact(render(json))
            if (callbackFunction != null) {
              val retVal = callbackFunction.call(new MetadataFormat("ConfigDef", mdlCfg))
              if (retVal == false) {
                return
              }
            }
          } catch {
            case e: Exception => { dumpKeyValueFailures(k, v); logger.warn("", e); }
            case e: Throwable => { dumpKeyValueFailures(k, v); logger.warn("", e); }
          }
        }
        callGetData(_metadataStore, "model_config_objects" + backupTblSufix, buildMdlCfglOne)
      }

    } catch {
      case e: Exception => {
        throw new Exception("Failed to load metadata objects", e)
      }
    }
  }

  // metadataElemsJson are used for dependency load
  // Callback function calls with container name, timepartition value, bucketkey, transactionid, rowid, serializername & data in Gson (JSON) format.
  override def getAllDataObjs(backupTblSufix: String, metadataElemsJson: Array[MetadataFormat], callbackFunction: DataObjectCallBack): Unit = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")
    throw new Exception("Not yet implemented")
  }

  override def shutdown: Unit = {
    if (_metadataStore != null)
      _metadataStore.Shutdown()
    _metadataStore = null
    if (_flReadFailures != null)
      _flReadFailures.close()
    _flReadFailures = null
  }
}

