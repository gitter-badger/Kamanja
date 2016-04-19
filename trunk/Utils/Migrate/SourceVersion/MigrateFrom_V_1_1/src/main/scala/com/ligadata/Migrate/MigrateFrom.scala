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
import scala.reflect.runtime.{ universe => ru }
// import scala.collection.JavaConversions._

class MigrateFrom_V_1_1 extends MigratableFrom {
  case class AdapterUniqueValueDes(T: Long, V: String, Qs: Option[List[String]], Res: Option[List[String]]) // TransactionId, Value, Queues & Result Strings. Queues and Result Strings should be same size.  

  object MdResolve extends MdBaseResolveInfo {
    val _messagesAndContainers = scala.collection.mutable.Map[String, MessageContainerObjBase]()
    val _kamanjaLoader = new KamanjaLoaderInfo
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
                logger.error("Failed to load message class %s".format(clsName), e)
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
                logger.error("Failed to load container class %s".format(clsName), e)
              }
            }
          }

          logger.debug("isMsg:%s, isContainer:%s".format(isMsg, isContainer))

          if (isMsg || isContainer) {
            try {
              val mirror = ru.runtimeMirror(_kamanjaLoader.loader)
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
                logger.error("Failed to instantiate message or conatiner object:" + clsName, e)
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

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private var _srouceInstallPath: String = _
  private var _metadataStoreInfo: String = _
  private var _dataStoreInfo: String = _
  private var _statusStoreInfo: String = _
  private var _sourceReadFailuresFilePath: String = _
  private var _modelConfigStore: DataStore = _
  private var _metadataStore: DataStore = _
  private var _dataStore: DataStore = _
  private var _counterStore: DataStore = _
  private var _flReadFailures: PrintWriter = _
  private var _bInit = false
  private val _kRecordsSeparator = "======================================================================================================================================================="

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
    val ver = MdMgr.ConvertLongVersionToString(mdObj.Version)
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
            ("ModelType" -> o.modelType) ~
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

  private def ExtractCountersData(key: Key, tupleBytes: Value): Array[DataFormat] = {
    // Get first _serInfoBufBytes bytes
    if (tupleBytes.size < _serInfoBufBytes) {
      val errMsg = s"Invalid input. This has only ${tupleBytes.size} bytes data. But we are expecting serializer buffer bytes as of size ${_serInfoBufBytes}"
      logger.error(errMsg)
      throw new Exception(errMsg)
    }

    implicit val jsonFormats: Formats = DefaultFormats
    val parsed_key = parse(new String(key.toArray)).extract[KamanjaDataKey]
    if (parsed_key.T.equalsIgnoreCase("txns") && parsed_key.K.size == 1 && parsed_key.K(0).equalsIgnoreCase("transactions")) {
      val serInfo = getSerializeInfo(tupleBytes).toLowerCase()
      val valInfo = getValueInfo(tupleBytes)
      return Array(new DataFormat("GlobalCounters", 0, Array[String]("TransactionId"), 0, 0, "", valInfo))
    } else {
      logger.error("Not Migrated Table:%s and Key:%s".format(parsed_key.T, parsed_key.K.mkString(",")))
    }

    return Array[DataFormat]()
  }

  private def dumpValueFailures(tupleBytes: Value): Unit = {
    val msgStr = ("Failed to deserialize Value: %s. Written the data related to this key in file:%s".format(new String(tupleBytes.toArray), _sourceReadFailuresFilePath))
    logger.error(msgStr)
    _flReadFailures.println("Record:")
    _flReadFailures.println(tupleBytes.toArray)
    _flReadFailures.println(_kRecordsSeparator)
    _flReadFailures.flush()
  }

  private def dumpKeyValueFailures(key: Key, tupleBytes: Value): Unit = {
    val msgStr = ("Failed to deserialize key: %s. Written the data related to this key in file:%s".format(new String(key.toArray), _sourceReadFailuresFilePath))
    logger.error(msgStr)
    _flReadFailures.println("Key:\n" + new String(key.toArray))
    _flReadFailures.println("Record:")
    _flReadFailures.println(tupleBytes.toArray)
    _flReadFailures.println(_kRecordsSeparator)
    _flReadFailures.flush()
  }

  private def ExtractDataFromTupleData(key: Key, tupleBytes: Value, bos: ByteArrayOutputStream, dos: DataOutputStream): Array[DataFormat] = {
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
        try {
          val valInfo = getValueInfo(tupleBytes)
          val desInfo = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(valInfo)
          if (desInfo.isInstanceOf[KamanjaData]) {
            kd = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(valInfo).asInstanceOf[KamanjaData]
          } else {
            implicit val jsonFormats: Formats = DefaultFormats
            val parsed_key = parse(new String(key.toArray)).extract[KamanjaDataKey]
            val typName = parsed_key.T
            if (typName.equalsIgnoreCase("ModelResults")) {
              val bucketKey = parsed_key.K
              logger.debug("type:%s, key:%s".format(typName, bucketKey))
              return Array(new DataFormat(typName, 0, bucketKey.toArray, 0, 0, serInfo, valInfo))
            } else {
              throw new Exception("Found un-handled data in KRYO format. This is not Kamanja Data or ModelResults")
            }
          }
        } catch {
          case e: Exception => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
          case e: Throwable => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
        }
      }
      case "manual" => {
        try {
          val valInfo = getValueInfo(tupleBytes)
          val datarec = new KamanjaData
          datarec.DeserializeData(valInfo, MdResolve, MdResolve._kamanjaLoader.loader)
          kd = datarec
        } catch {
          case e: Exception => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
          case e: Throwable => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
        }
      }
      case "csv" => {
        try {
          val valInfo = getValueInfo(tupleBytes)
          implicit val jsonFormats: Formats = DefaultFormats
          val parsed_key = parse(new String(key.toArray)).extract[KamanjaDataKey]
          val typName = parsed_key.T
          if (typName.equalsIgnoreCase("AdapterUniqKvData")) {
            var bucketKey = parsed_key.K
            logger.debug("type:%s, key:%s".format(typName, bucketKey))

            implicit val jsonFormats: Formats = DefaultFormats
            val uniqVal = parse(new String(valInfo)).extract[AdapterUniqueValueDes]

            val json = ("T" -> uniqVal.T) ~ ("V" -> uniqVal.V)

            // Hack from 1.1 to next version key change (lower case to case sensitive) -- Begin
            val kafkaAdapterCheck = """{"version":1,"type":"kafka","name":""""

            if (bucketKey.size == 1 && bucketKey(0).startsWith(kafkaAdapterCheck)) {
              try {
                val kafkaPartition = parse(bucketKey(0)).values.asInstanceOf[Map[String, Any]]

                val nm = kafkaPartition.getOrElse("name", null)
                val topicNm = kafkaPartition.getOrElse("topicname", null)
                val partId = kafkaPartition.getOrElse("partitionid", null)

                if (nm != null && topicNm != null && partId != null) {
                  val nm1 = nm.toString()
                  val topicNm1 = topicNm.toString()
                  val partId1 = partId.toString().toInt

                  val json1 =
                    ("Version" -> 1) ~
                      ("Type" -> "Kafka") ~
                      ("Name" -> nm1) ~
                      ("TopicName" -> topicNm1) ~
                      ("PartitionId" -> partId1)
                  bucketKey = List[String](compact(render(json1)))
                } else {
                  logger.warn("For Kafka Adapter, we did not find name or topicname or partitionid from map to migrate to new version. This may lead to reprocessing all data from kafka queue.")
                }
              } catch {
                case e: Exception => {
                  logger.warn("For Kafka Adapter, we got some exception while migrating to new version. This may lead to reprocessing all data from kafka queue.", e)
                }
              }
            }
            // Hack from 1.1 to next version key change (lower case to case sensitive) -- End

            return Array(new DataFormat(typName, 0, bucketKey.toArray, 0, 0, "json", compact(render(json)).getBytes("UTF8")))
          } else {
            throw new Exception("Found un-handled data in CSV format. This is not AdapterUniqKvData")
          }
        } catch {
          case e: Exception => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
          case e: Throwable => { dumpKeyValueFailures(key, tupleBytes); logger.warn("", e); }
        }
      }
      case _ => {
        throw new Exception("Found un-handled Serializer Info: " + serInfo)
      }
    }

    if (kd != null) {
      val typName = kd.GetTypeName
      val bucketKey = kd.GetKey
      val data = kd.GetAllData

      logger.debug("type:%s, key:%s, data records count:%d".format(typName, bucketKey, data.size))

      var rowIdCntr = 0

      // container name, timepartition value, bucketkey, transactionid, rowid, serializername & data in Serialized ByteArray.
      return data.map(d => {
        bos.reset()
        dos.writeUTF(d.FullName)
        dos.writeUTF(d.Version)
        dos.writeUTF(d.getClass.getName)
        d.Serialize(dos)
        val arr = bos.toByteArray()

        rowIdCntr += 1
        // logger.debug("type:%s, key:%s, data record TxnId:%d, ByteArraySize:%d".format(typName, bucketKey, d.TransactionId(), arr.length))
        new DataFormat(typName, 0, bucketKey, d.TransactionId(), rowIdCntr, serInfo, arr)
      })
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
        } else if (mdElem.objType.compareToIgnoreCase("ModelDef") == 0) {
          val json = parse(mdElem.objDataInJson)
          val jsonObjMap = json.values.asInstanceOf[Map[String, Any]]
          val isActiveStr = jsonObjMap.getOrElse("IsActive", "").toString.trim()
          if (isActiveStr.size > 0) {
            val isActive = jsonObjMap.getOrElse("IsActive", "").toString.trim().toBoolean
            if (isActive)
              LoadJarIfNeeded(jsonObjMap, MdResolve._kamanjaLoader.loadedJars, MdResolve._kamanjaLoader.loader, jarPaths)
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

    if (_dataStoreInfo.trim.size == 0)
      return Array[TableName]()

    var parsed_json: Map[String, Any] = null
    try {
      val json = parse(_dataStoreInfo)
      if (json == null || json.values == null) {
        val msg = "Failed to parse JSON configuration string:" + _dataStoreInfo
        throw new Exception(msg)
      }
      parsed_json = json.values.asInstanceOf[Map[String, Any]]
    } catch {
      case e: Exception => {
        throw new Exception("Failed to parse JSON configuration string:" + _dataStoreInfo, e)
      }
    }

    val namespace = if (parsed_json.contains("SchemaName")) parsed_json.getOrElse("SchemaName", "default").toString.trim else parsed_json.getOrElse("SchemaName", "default").toString.trim
    Array(new TableName(namespace, "AllData"), new TableName(namespace, "ClusterCounts"))
  }

  override def getAllStatusTableNames: Array[TableName] = {
    if (_bInit == false)
      throw new Exception("Not yet Initialized")

    if (_statusStoreInfo.trim.size == 0)
      return Array[TableName]()

    var parsed_json: Map[String, Any] = null
    try {
      val json = parse(_statusStoreInfo)
      if (json == null || json.values == null) {
        val msg = "Failed to parse JSON configuration string:" + _statusStoreInfo
        throw new Exception(msg)
      }
      parsed_json = json.values.asInstanceOf[Map[String, Any]]
    } catch {
      case e: Exception => {
        throw new Exception("Failed to parse JSON configuration string:" + _statusStoreInfo, e)
      }
    }

    val namespace = if (parsed_json.contains("SchemaName")) parsed_json.getOrElse("SchemaName", "default").toString.trim else parsed_json.getOrElse("SchemaName", "default").toString.trim
    Array(new TableName(namespace, "CommmittingTransactions"), new TableName(namespace, "checkPointAdapInfo"))
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

    if (MdResolve._kryoDataSer != null) {
      MdResolve._kryoDataSer.SetClassLoader(MdResolve._kamanjaLoader.loader)
    }

    _modelConfigStore = GetDataStoreHandle(fromVersionJarPaths, _metadataStoreInfo, "model_config_objects" + backupTblSufix)
    _metadataStore = GetDataStoreHandle(fromVersionJarPaths, _metadataStoreInfo, "metadata_objects" + backupTblSufix)

    try {
      // Load all metadata objects
      var keys = scala.collection.mutable.Set[Key]()
      var mdlCfgKeys = scala.collection.mutable.Set[Key]()
      _metadataStore.getAllKeys({ (key: Key) => keys.add(key) })
      _modelConfigStore.getAllKeys({ (key: Key) => mdlCfgKeys.add(key) })
      if (keys.size == 0 && mdlCfgKeys.size == 0) {
        val szMsg = "No objects available in model_config_objects, transaction_id & metadata_objects"
        logger.warn(szMsg)
        return
      }

      val allObjs = ArrayBuffer[Value]()
      keys.foreach(key => {
        val obj = GetObject(key, _metadataStore)
        allObjs += obj.Value
      })

      allObjs.foreach(o => {
        try {
          val mObj = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(o.toArray[Byte]).asInstanceOf[BaseElem]
          try {
            val (typ, jsonStr) = serializeObjectToJson(mObj)

            if (excludedMetadataTypes.contains(typ.toLowerCase()) == false) {
              if (callbackFunction != null) {
                val retVal = callbackFunction.call(new MetadataFormat(typ, jsonStr))
                if (retVal == false) {
                  return
                }
              }
            } else {
              // This type is excluded
            }

          } catch {
            case e: Exception => { dumpValueFailures(o); logger.warn("", e); }
            case e: Throwable => { dumpValueFailures(o); logger.warn("", e); }
          }
        } catch {
          case e: Exception => { dumpValueFailures(o); logger.warn("", e); }
          case e: Throwable => { dumpValueFailures(o); logger.warn("", e); }
        }
      })

      if (excludedMetadataTypes.contains("ConfigDef".toLowerCase()) == false) {
        mdlCfgKeys.foreach(key => {
          try {
            val obj = GetObject(key, _modelConfigStore)
            try {
              val conf = MdResolve._kryoDataSer.DeserializeObjectFromByteArray(obj.Value.toArray[Byte]).asInstanceOf[Map[String, Any]]
              val (nameSpace, name) = SplitFullName(KeyAsStr(key))

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
              case e: Exception => { dumpKeyValueFailures(obj.Key, obj.Value); logger.warn("", e); }
              case e: Throwable => { dumpKeyValueFailures(obj.Key, obj.Value); logger.warn("", e); }
            }
          } catch {
            case e: Exception => throw e
            case e: Throwable => throw e
          }
        })
      } else {
        // This type is excluded
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

    _dataStore = GetDataStoreHandle(fromVersionJarPaths, _dataStoreInfo, "AllData" + backupTblSufix)
    _counterStore = GetDataStoreHandle(fromVersionJarPaths, _dataStoreInfo, "ClusterCounts" + backupTblSufix)

    if (MdResolve._kryoDataSer != null) {
      MdResolve._kryoDataSer.SetClassLoader(MdResolve._kamanjaLoader.loader)
    }

    logger.debug("All Messages and Containers:%s".format(MdResolve._messagesAndContainers.map(kv => kv._1).mkString(",")))

    val bos = new ByteArrayOutputStream(1024 * 1024)
    val dos = new DataOutputStream(bos)

    try {
      // Load all metadata objects
      var keys = scala.collection.mutable.Set[Key]()
      _dataStore.getAllKeys({ (key: Key) => keys.add(key) })
      var countersKeys = scala.collection.mutable.Set[Key]()
      _counterStore.getAllKeys({ (key: Key) => countersKeys.add(key) })

      if (keys.size == 0 && countersKeys.size == 0) {
        val szMsg = "No objects available in AllData & ClusterCounts"
        logger.warn(szMsg)
        return
      }

      keys.foreach(key => {
        val obj = GetObject(key, _dataStore)
        val retData = ExtractDataFromTupleData(key, obj.Value, bos, dos)
        if (retData.size > 0 && callbackFunction != null) {
          if (callbackFunction.call(retData) == false)
            throw new Exception("Data failed to consume")
        }
      })

      countersKeys.foreach(key => {
        val obj = GetObject(key, _counterStore)
        val retData = ExtractCountersData(key, obj.Value)
        if (retData.size > 0 && callbackFunction != null) {
          if (callbackFunction.call(retData) == false)
            throw new Exception("Data failed to consume")
        }
      })
    } catch {
      case e: Exception => {
        throw new Exception("Failed to get data", e)
      }
    } finally {
      dos.close()
      bos.close()
    }
  }

  override def shutdown: Unit = {
    if (_modelConfigStore != null)
      _modelConfigStore.Shutdown()
    if (_metadataStore != null)
      _metadataStore.Shutdown()
    if (_dataStore != null)
      _dataStore.Shutdown()
    if (_counterStore != null)
      _counterStore.Shutdown()
    if (_flReadFailures != null)
      _flReadFailures.close()
    _flReadFailures = null
    _modelConfigStore = null
    _metadataStore = null
    _dataStore = null
    _counterStore = null
  }
}

