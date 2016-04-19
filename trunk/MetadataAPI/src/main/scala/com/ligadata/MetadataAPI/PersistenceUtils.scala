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
import com.ligadata.KamanjaVersion.KamanjaVersion
import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.MetadataAPI.MetadataAPI.ModelType.ModelType

import scala.Enumeration
import scala.io._
import scala.collection.mutable.ArrayBuffer

import scala.collection.mutable._
import scala.reflect.runtime.{universe => ru}

import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MdMgr._

import com.ligadata.kamanja.metadataload.MetadataLoad

// import com.ligadata.keyvaluestore._
import com.ligadata.HeartBeat.{MonitoringContext, HeartBeatUtil}
import com.ligadata.StorageBase.{DataStore, Transaction}
import com.ligadata.KvBase.{Key, TimeRange}

import scala.util.parsing.json.JSON
import scala.util.parsing.json.{JSONObject, JSONArray}
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


// The implementation class
object PersistenceUtils {

  // system name space
  lazy val sysNS = "System"
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  private[this] val lock = new Object
  private var mainDS: DataStore = _
  private var tableStoreMap: Map[String, (String, DataStore)] = Map()
  private val storageDefaultTime = 0L
  private val storageDefaultTxnId = 0L
  lazy val serializerType = "json4s"//"kryo"
  //lazy val serializer = SerializerManager.GetSerializer(serializerType)

  lazy val versionStr = s"${KamanjaVersion.getMajorVersion}.${KamanjaVersion.getMinorVersion}.${KamanjaVersion.getMicroVersion}"
  lazy val excludeSystemJars = Set(s"ExtDependencyLibs_2.11-${versionStr}.jar", s"ExtDependencyLibs2_2.11-${versionStr}.jar", s"KamanjaInternalDeps_2.11-${versionStr}.jar",
                                      s"ExtDependencyLibs_2.10-${versionStr}.jar", s"ExtDependencyLibs2_2.10-${versionStr}.jar", s"KamanjaInternalDeps_2.10-${versionStr}.jar")

  def GetMainDS: DataStore = mainDS

  def GetContainerNameAndDataStore(typeName: String): (String, DataStore) = {
    val info = tableStoreMap.getOrElse(typeName, null)
    if (info != null)
      info
    else
      (typeName.toLowerCase(), mainDS)
  }

  def GetObject(bucketKeyStr: String, typeName: String): (String, Any) = {
    val (containerName, store) = GetContainerNameAndDataStore(typeName)
    var objs = new Array[(String, Any)](1)
    val getObjFn = (k: Key, v: Any, serType: String, typ: String, ver: Int) => {
      objs(0) = (serType, v)
    }

    try {
      objs(0) = null
      store.get(containerName, Array(TimeRange(storageDefaultTime, storageDefaultTime)), Array(Array(bucketKeyStr)), getObjFn)
      if (objs(0) == null)
        throw ObjectNotFoundException("Object %s not found in container %s".format(bucketKeyStr, containerName), null)
      objs(0)
    } catch {
      case e: ObjectNotFoundException => {
        logger.debug("ObjectNotFound Exception", e)
        throw e
      }
      case e: Exception => {
        logger.debug("General Exception", e)
        throw ObjectNotFoundException(e.getMessage(), e)
      }
    }
  }

  def SaveObject(bucketKeyStr: String, value: Array[Byte], typeName: String, serializerTyp: String) {
    val (containerName, store) = GetContainerNameAndDataStore(typeName)
    val k = Key(storageDefaultTime, Array(bucketKeyStr), storageDefaultTxnId, 0)

    try {
      store.put(null, containerName, k, serializerTyp, value)
    } catch {
      case e: Exception => {
        logger.error("Failed to insert/update object for : " + bucketKeyStr, e)
        throw UpdateStoreFailedException("Failed to insert/update object for : " + bucketKeyStr, e)
      }
    }
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
    val (containerName, store) = GetContainerNameAndDataStore(typeName)
    var i = 0
    var storeObjects = new Array[(Key, String, Any)](keyList.length)
    i = 0
    keyList.foreach(bucketKeyStr => {
      var value = valueList(i)
      val k = Key(storageDefaultTime, Array(bucketKeyStr), storageDefaultTxnId, 0)
      storeObjects(i) = (k, serializerTyp, value)
      i = i + 1
    })

    try {
      store.put(null, Array((containerName, storeObjects)))
    } catch {
      case e: Exception => {
        logger.error("Failed to insert/update objects for : " + keyList.mkString(","), e)
        throw UpdateStoreFailedException("Failed to insert/update object for : " + keyList.mkString(","), e)
      }
    }
  }

  /**
    * Remove all of the elements with the supplied keys in the list from the supplied DataStore
    */
  def RemoveObjectList(keyList: Array[String], typeName: String) {
    val (containerName, store) = GetContainerNameAndDataStore(typeName)
    var i = 0
    var delKeys = new Array[(Key)](keyList.length)
    i = 0
    keyList.foreach(bucketKeyStr => {
      val k = Key(storageDefaultTime, Array(bucketKeyStr), storageDefaultTxnId, 0)
      delKeys(i) = k
      i = i + 1
    })

    try {
      store.del(containerName, delKeys)
    } catch {
      case e: Exception => {
        logger.error("Failed to delete object batch for : " + keyList.mkString(","), e)
        throw UpdateStoreFailedException("Failed to delete object batch for : " + keyList.mkString(","), e)
      }
    }
  }

  /**
    * Answer which table the supplied BaseElemeDef is stored
    *
    * @param obj
    * @return
    */
  def getMdElemTypeName(obj: BaseElemDef): String = {
    obj match {
      case o: ModelDef => {
        "models"
      }
      case o: MessageDef => {
        "messages"
      }
      case o: ContainerDef => {
        "containers"
      }
      case o: FunctionDef => {
        "functions"
      }
      case o: AttributeDef => {
        "concepts"
      }
      case o: BaseTypeDef => {
        "types"
      }
      case _ => {
        logger.error("getMdElemTypeName is not implemented for objects of type " + obj.getClass.getName)
        throw InternalErrorException("getMdElemTypeName is not implemented for objects of type " + obj.getClass.getName, null)
      }
    }
  }

  /**
    * getObjectType
    *
    * @param obj <description please>
    * @return <description please>
    */
  def getObjectType(obj: BaseElemDef): String = {
    val className = obj.getClass().getName();
    className.split("\\.").last
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

    logger.debug("Save " + objList.length + " objects in a single transaction ")
    val tranId = GetNewTranId
    var keyList = new Array[String](objList.length)
    var valueList = new Array[Array[Byte]](objList.length)
    try {
      var i = 0;
      objList.foreach(obj => {
        obj.tranId = tranId
        val key = (getObjectType(obj) + "." + obj.FullNameWithVer).toLowerCase
        var value = MetadataAPISerialization.serializeObjectToJson(obj).getBytes //serializer.SerializeObjectToByteArray(obj)
        keyList(i) = key
        valueList(i) = value
        i = i + 1
      })
      SaveObjectList(keyList, valueList, typeName, serializerType)
    } catch {
      case e: Exception => {
        logger.error("Failed to insert/update object for : " + keyList.mkString(","), e)
        throw UpdateStoreFailedException("Failed to insert/update object for : " + keyList.mkString(","), e)
      }
    }
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
    logger.debug("Save " + objList.length + " objects in a single transaction ")
    val tranId = GetNewTranId
    var saveDataMap = scala.collection.mutable.Map[String, ArrayBuffer[(Key, String, Any)]]()

    try {
      var i = 0;
      objList.foreach(obj => {
        obj.tranId = tranId
        val key = (getObjectType(obj) + "." + obj.FullNameWithVer).toLowerCase
        var value = MetadataAPISerialization.serializeObjectToJson(obj).getBytes//serializer.SerializeObjectToByteArray(obj)
        val elemTyp = getMdElemTypeName(obj)

        val k = Key(storageDefaultTime, Array(key), storageDefaultTxnId, 0)

        val ab = saveDataMap.getOrElse(elemTyp, null)
        if (ab != null) {
          ab += ((k, serializerType, value))
          saveDataMap(elemTyp) = ab
        } else {
          val newab = ArrayBuffer[(Key, String, Any)]()
          newab += ((k, serializerType, value))
          saveDataMap(elemTyp) = newab
        }
        i = i + 1
      })

      var storeData = scala.collection.mutable.Map[String, (DataStore, ArrayBuffer[(String, Array[(Key, String, Any)])])]()

      saveDataMap.foreach(elemTypData => {
        val storeInfo = GetContainerNameAndDataStore(elemTypData._1)
        val oneStoreData = storeData.getOrElse(storeInfo._1, null)
        if (oneStoreData != null) {
          oneStoreData._2 += ((elemTypData._1, elemTypData._2.toArray))
          storeData(storeInfo._1) = ((oneStoreData._1, oneStoreData._2))
        } else {
          val ab = ArrayBuffer[(String, Array[(Key, String, Any)])]()
          ab += ((elemTypData._1, elemTypData._2.toArray))
          storeData(storeInfo._1) = ((storeInfo._2, ab))
        }
      })

      storeData.foreach(oneStoreData => {
        try {
          oneStoreData._2._1.put(null, oneStoreData._2._2.toArray)
        } catch {
          case e: Exception => {
            logger.error("Failed to insert/update objects in : " + oneStoreData._1, e)
            throw UpdateStoreFailedException("Failed to insert/update object for : " + oneStoreData._1, e)
          }
        }
      })
    } catch {
      case e: Exception => {
        logger.error("Failed to insert/update objects", e)
        throw UpdateStoreFailedException("Failed to insert/update objects", e)
      }
    }
  }

  def SaveSchemaInformation(schemaId: Int, nameSpace: String, name: String, version: Long, physicalName: String, avroSchema: String, containerType: String): Unit = {
    val (containerName, store) = GetContainerNameAndDataStore("AvroSchemaInfo")

    val json = "AvroSchemaInfo" ->
      ("SchemaId" -> schemaId) ~
        ("NameSpace" -> nameSpace) ~
        ("Name" -> name) ~
        ("Version" -> version) ~
        ("PhysicalName" -> physicalName) ~
        ("AvroSchema" -> avroSchema) ~
        ("ContainerType" -> containerType)

    val outputJson = compact(render(json))

    var storeObjects = new Array[(Key, String, Any)](1)
    val k = Key(storageDefaultTime, Array(schemaId.toString), storageDefaultTxnId, 0)
    storeObjects(0) = (k, "JSON", outputJson.getBytes())

    try {
      store.put(null, Array((containerName, storeObjects)))
    } catch {
      case e: Exception => {
        logger.error("Failed to insert/update object for schemaid: " + schemaId, e)
        throw UpdateStoreFailedException("Failed to insert/update object for schemaid: " + schemaId, e)
      }
    }
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
    SaveObject(key, value, typeName, serializerTyp)
  }

  def UpdateTranId(objList: Array[BaseElemDef]): Unit = {
    var max: Long = 0
    objList.foreach(obj => {
      max = scala.math.max(max, obj.TranId)
    })
    if (MetadataAPIImpl.getCurrentTranLevel < max)
      MetadataAPIImpl.setCurrentTranLevel(max)
    PutTranId(max)
  }

  def GetMetadataId(key: String, incrementInDb: Boolean, defaultId: Long = 1): Long = {
    var id: Long = defaultId
    try {
      val (serTyp, obj) = GetObject(key.toLowerCase(), "metadatacounters")
      val idStr = new String(obj.asInstanceOf[Array[Byte]])
      id = idStr.toLong
    } catch {
      case e: ObjectNotFoundException => {
        // first time
        id = defaultId
      }
      case e: Exception => {
        throw KeyNotFoundException("Unable to retrieve Metadata Id for Key:" + key, e)
      }
    }

    if (incrementInDb) {
      // Put it back after increment
      PutMetadataId(key, id + 1)
    }

    id
  }

  def PutMetadataId(key: String, id: Long) = {
    try {
      SaveObject(key.toLowerCase(), id.toString.getBytes, "metadatacounters", "")
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw UpdateStoreFailedException("Unable to Save the Metadata Id:%d for Key:%s".format(id, key), e)
      }
    }
  }


  /**
    * GetNewTranId
    *
    * @return <description please>
    */
  def GetNewTranId: Long = {
    try {
      val (serTyp, obj) = GetObject("transaction_id", "transaction_id")
      val idStr = new String(obj.asInstanceOf[Array[Byte]])
      idStr.toLong + 1
    } catch {
      case e: ObjectNotFoundException => {
        // first time
        1
      }
      case e: Exception => {
        throw TranIdNotFoundException("Unable to retrieve the transaction id", e)
      }
    }
  }

  /**
    * GetTranId
    *
    * @return <description please>
    */
  def GetTranId: Long = {
    try {
      val (serTyp, obj) = GetObject("transaction_id", "transaction_id")
      val idStr = new String(obj.asInstanceOf[Array[Byte]])
      idStr.toLong
    } catch {
      case e: ObjectNotFoundException => {
        // first time
        0
      }
      case e: Exception => {
        throw TranIdNotFoundException("Unable to retrieve the transaction id", e)
      }
    }
  }

  /**
    * PutTranId
    *
    * @param tId <description please>
    */
  def PutTranId(tId: Long) = {
    try {
      SaveObject("transaction_id", tId.toString.getBytes, "transaction_id", "")
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw UpdateStoreFailedException("Unable to Save the transaction id " + tId, e)
      }
    }
  }


  /**
    * UploadJarsToDB
    *
    * @param obj                <description please>
    * @param forceUploadMainJar <description please>
    * @param alreadyCheckedJars <description please>
    */
  def UploadJarsToDB(obj: BaseElemDef, forceUploadMainJar: Boolean = true, alreadyCheckedJars: scala.collection.mutable.Set[String] = null): Unit = {
    val checkedJars: scala.collection.mutable.Set[String] = if (alreadyCheckedJars == null) scala.collection.mutable.Set[String]() else alreadyCheckedJars

    try {
      var keyList = new ArrayBuffer[String](0)
      var valueList = new ArrayBuffer[Array[Byte]](0)

      val tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
      val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
      if (obj.jarName != null && (forceUploadMainJar || checkedJars.contains(obj.jarName) == false)) {
        //BUGBUG
        val jarsPathsInclTgtDir = jarPaths + MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_TARGET_DIR")
        var jarName = com.ligadata.Utils.Utils.GetValidJarFile(jarsPathsInclTgtDir, obj.jarName)
        var value = MetadataAPIImpl.GetJarAsArrayOfBytes(jarName)

        var loadObject = false

        if (forceUploadMainJar) {
          loadObject = true
        } else {
          var mObj: (String, Any) = null
          try {
            mObj = GetObject(obj.jarName, "jar_store")
          } catch {
            case e: ObjectNotFoundException => {
              logger.debug("", e)
              loadObject = true
            }
            case e: Exception => {
              logger.debug("", e)
              loadObject = true
            }
          }

          if (loadObject == false) {
            val ba = mObj._2.asInstanceOf[Array[Byte]]
            val fs = ba.length
            if (fs != value.length) {
              logger.debug("A jar file already exists, but it's size (" + fs + ") doesn't match with the size of the Jar (" +
                jarName + "," + value.length + ") of the object(" + obj.FullNameWithVer + ")")
              loadObject = true
            }
          }
        }

        checkedJars += obj.jarName

        if (loadObject) {
          logger.debug("Update the jarfile (size => " + value.length + ") of the object: " + obj.jarName)
          keyList += obj.jarName
          valueList += value
        }
      }

      if (obj.DependencyJarNames != null) {
        obj.DependencyJarNames.foreach(j => {
          // do not upload if it already exist & just uploaded/checked in db, minor optimization
          if (j.endsWith(".jar") && checkedJars.contains(j) == false && excludeSystemJars.contains(j) == false) {
            var loadObject = false
            val jarName = com.ligadata.Utils.Utils.GetValidJarFile(jarPaths, j)
            val value = MetadataAPIImpl.GetJarAsArrayOfBytes(jarName)
            var mObj: (String, Any) = null
            try {
              mObj = GetObject(j, "jar_store")
            } catch {
              case e: ObjectNotFoundException => {
                logger.debug("", e)
                loadObject = true
              }
            }

            if (loadObject == false) {
              val ba = mObj._2.asInstanceOf[Array[Byte]]
              val fs = ba.length
              if (fs != value.length) {
                logger.debug("A jar file already exists, but it's size (" + fs + ") doesn't match with the size of the Jar (" +
                  jarName + "," + value.length + ") of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")")
                loadObject = true
              }
            }

            if (loadObject) {
              keyList += j
              logger.debug("Update the jarfile (size => " + value.length + ") of the object: " + j)
              valueList += value
            } else {
              logger.debug("The jarfile " + j + " already exists in DB.")
            }
            checkedJars += j
          }
        })
      }
      if (keyList.length > 0) {
        SaveObjectList(keyList.toArray, valueList.toArray, "jar_store", "")
      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw InternalErrorException("Failed to Update the Jar of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + ")", e)
      }
    }
  }

  /**
    * UploadJarToDB
    *
    * @param jarName <description please>
    */
  def UploadJarToDB(jarName: String) {
    try {
      val f = new File(jarName)
      if (f.exists()) {
        var key = f.getName()
        var value = MetadataAPIImpl.GetJarAsArrayOfBytes(jarName)
        logger.debug("Update the jarfile (size => " + value.length + ") of the object: " + jarName)
        SaveObject(key, value, "jar_store", "")

        var apiResult = new ApiResult(ErrorCodeConstants.Success, "UploadJarToDB", null, ErrorCodeConstants.Upload_Jar_Successful + ":" + jarName)
        apiResult.toString()

      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadJarToDB", null, "Error : " + e.toString() + ErrorCodeConstants.Upload_Jar_Failed + ":" + jarName)
        apiResult.toString()
      }
    }
  }

  /**
    * UploadJarToDB
    *
    * @param jarName   <description please>
    * @param byteArray <description please>
    * @param userid    the identity to be used by the security adapter to ascertain if this user has access permissions for this
    *                  method. If Security and/or Audit are configured, this value must be a value other than None.
    * @return <description please>
    */
  def UploadJarToDB(jarName: String, byteArray: Array[Byte], userid: Option[String] = None): String = {
    try {
      var key = jarName
      var value = byteArray
      logger.debug("Update the jarfile (size => " + value.length + ") of the object: " + jarName)
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTJAR, jarName, AuditConstants.SUCCESS, "", jarName)
      SaveObject(key, value, "jar_store", "")
      var apiResult = new ApiResult(ErrorCodeConstants.Success, "UploadJarToDB", null, ErrorCodeConstants.Upload_Jar_Successful + ":" + jarName)
      apiResult.toString()
    } catch {
      case e: Exception => {
        logger.debug("", e)
        val apiResult = new ApiResult(ErrorCodeConstants.Failure, "UploadJarToDB", null, "Error : " + e.toString() + ErrorCodeConstants.Upload_Jar_Failed + ":" + jarName)
        apiResult.toString()
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
      var allJars = MetadataAPIImpl.GetDependantJars(obj)
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
          curJar = jar
          try {
            if (PersistenceUtils.excludeSystemJars.contains(jar.trim) == false) {
              // download only if it doesn't already exists
              val b = MetadataAPIImpl.IsDownloadNeeded(jar, obj)
              if (b == true) {
                val key = jar
                val mObj = GetObject(key, "jar_store")
                val ba = mObj._2.asInstanceOf[Array[Byte]]
                val jarName = dirPath + "/" + jar
                MetadataAPIImpl.PutArrayOfBytesToJar(ba, jarName)
              } else {
                logger.debug("The jar " + curJar + " was already downloaded... ")
              }
            }
          } catch {
            case e: Exception => {
              logger.error("Failed to download the Jar of the object(" + obj.FullName + "." + MdMgr.Pad0s2Version(obj.Version) + "'s dep jar " + curJar + ")", e)

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
    * DeleteObject
    */
  def DeleteObject(bucketKeyStr: String, typeName: String) {
    val (containerName, store) = GetContainerNameAndDataStore(typeName)
    store.del(containerName, Array(Key(storageDefaultTime, Array(bucketKeyStr), storageDefaultTxnId, 0)))
  }


  /**
    * GetDataStoreHandle
    *
    * @param jarPaths      Set of paths where jars are located Set of paths where jars are located
    * @param dataStoreInfo information needed to access the data store (kv store dependent)
    * @return
    */
  private def GetDataStoreHandle(jarPaths: collection.immutable.Set[String], dataStoreInfo: String): DataStore = {
    try {
      logger.debug("Getting DB Connection for dataStoreInfo:%s".format(dataStoreInfo))
      return KeyValueManager.Get(jarPaths, dataStoreInfo)
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw new CreateStoreFailedException(e.getMessage(), e)
      }
    }
  }

  /**
    * OpenDbStore
    *
    * @param jarPaths      Set of paths where jars are located
    * @param dataStoreInfo information needed to access the data store (kv store dependent)
    */
  def OpenDbStore(jarPaths: collection.immutable.Set[String], dataStoreInfo: String) {
    try {
      logger.debug("Opening datastore")
      mainDS = GetDataStoreHandle(jarPaths, dataStoreInfo)

      tableStoreMap = Map("metadata_objects" ->("metadata_objects", mainDS),
        "models" ->("metadata_objects", mainDS),
        "messages" ->("metadata_objects", mainDS),
        "containers" ->("metadata_objects", mainDS),
        "functions" ->("metadata_objects", mainDS),
        "concepts" ->("metadata_objects", mainDS),
        "types" ->("metadata_objects", mainDS),
        "others" ->("metadata_objects", mainDS),
        "outputmsgs" ->("metadata_objects", mainDS),
        "jar_store" ->("jar_store", mainDS),
        "config_objects" ->("config_objects", mainDS),
        "adapter_message_bindings" ->("config_objects", mainDS),
        "model_config_objects" ->("model_config_objects", mainDS),
        "transaction_id" ->("transaction_id", mainDS))
    } catch {
      case e: FatalAdapterException => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: StorageConnectionException => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: StorageFetchException => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: StorageDMLException => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: StorageDDLException => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
      case e: Throwable => {
        logger.error("Failed to connect to Datastore", e)
        throw CreateStoreFailedException(e.getMessage(), e)
      }
    }
  }

  /**
    * CreateMetadataTables
    */
  def CreateMetadataTables: Unit = lock.synchronized {
    try {
      logger.debug("Creating MetadataTables")
      if (mainDS != null) {
	val metadataTables = Array("metadata_objects","jar_store","config_objects","model_config_objects","transaction_id","metadatacounters","avroschemainfo")
        mainDS.CreateMetadataContainer(metadataTables)
        logger.debug("Created Metadata Tables")
      }
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw e;
      }
    }
  }

  /**
    * CloseDbStore
    */
  def CloseDbStore: Unit = lock.synchronized {
    try {
      logger.debug("Closing datastore")
      if (mainDS != null) {
        mainDS.Shutdown()
        mainDS = null
        logger.debug("main datastore closed")
      }
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw e;
      }
    }
  }

  /**
    * TruncateDbStore
    */
  def TruncateDbStore: Unit = lock.synchronized {
    try {
      logger.debug("Not allowing to truncate the whole datastore")
      // mainDS.TruncateStore
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw e;
      }
    }
  }

  /**
    * TruncateAuditStore
    */
  def TruncateAuditStore: Unit = lock.synchronized {
    try {
      logger.debug("Truncating Audit datastore")
      if (MetadataAPIImpl.GetAuditObj != null) {
        MetadataAPIImpl.GetAuditObj.TruncateStore
      }
    } catch {
      case e: Exception => {
        logger.error("", e)
        throw e;
      }
    }
  }
}
