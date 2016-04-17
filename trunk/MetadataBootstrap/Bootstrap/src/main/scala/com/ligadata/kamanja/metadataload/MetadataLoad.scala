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

package com.ligadata.kamanja.metadataload

import scala.collection.mutable.{Set}
import org.apache.logging.log4j.{Logger, LogManager}
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata._
import com.ligadata.KamanjaBase._
import com.ligadata.BaseTypes._
import org.joda.time.base
import org.joda.time.chrono
import org.joda.time.convert
import org.joda.time.field
import org.joda.time.format
import org.joda.time.tz
import org.joda.time.LocalDate
import org.joda.time.DateTime
import org.joda.time.Years


trait LogTrait {
  val loggerName = this.getClass.getName()
  val logger = LogManager.getLogger(loggerName)
}

/**
  * FIXME: As an intermediate development, we might load the metadata manager with file content before resurrecting
  * the cache from a kv store... hence the arguments (currently unused)
  *
  * For now, we just call some functions in the object MetadataLoad to load the various kinds of metadata.
  * The functions used to load metadata depend on metadata that the loaded element needs being present
  * before hand in the metadata store (e.g., a type of a function arg must exist before the function can
  * be loaded.
  *
  *
  */

object MetadataLoad {
  val baseTypesOwnerId = "kamanja"
  val baseTypesTenantId = ""
  val baseTypesVer: Long = 1000000
  // Which is 00.01.000000
  val baseTypesUniqId: Long = 0
  val baseTypesElementId: Long = 0

  //NOTE NOTE:-       1-1000000 SchemaIds are reserved for Standard Containers
  //NOTE NOTE:- 1000001-2000000 SchemaIds are reserved for Standard Messages

  def ContainerInterfacesInfo: Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)] = {
    // nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)
    return Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)](
      (MdMgr.sysNS, "EnvContext", "com.ligadata.KamanjaBase.EnvContext", List(), 1, ""), // Assigned SchemaId as 1. Never change this for this container
      (MdMgr.sysNS, "MessageInterface", "com.ligadata.KamanjaBase.MessageInterface", List(), 2, ""), // Assigned SchemaId as 2. Never change this for this container
      (MdMgr.sysNS, "ContainerInterface", "com.ligadata.KamanjaBase.ContainerInterface", List(), 3, ""), // Assigned SchemaId as 3. Never change this for this container
      (MdMgr.sysNS, "Context", "com.ligadata.pmml.runtime.Context", List(), 4, "") // Assigned SchemaId as 4. Never change this for this container
      // NOTE NOTE:- Next SchemaId should start from 5
    )
  }

  def BaseMessagesInfo: Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)] = {
    return Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)](
      ("com.ligadata.KamanjaBase", "KamanjaStatusEvent", "com.ligadata.KamanjaBase.KamanjaStatusEvent$", List(("system", "nodeid", "system", "string", false, null), ("system", "eventtime", "system", "long", false, null), ("system", "statusstring", "system", "string", false, null)), 1000001, ""), // Assigned SchemaId as 1000001. Never change this for this message
      ("com.ligadata.KamanjaBase", "KamanjaModelEvent", "com.ligadata.KamanjaBase.KamanjaModelEvent$", List(("system", "modelid", "system", "Long", false, null), ("system", "elapsedtimeinms", "system", "Float", false, null), ("system", "eventepochtime", "system", "Long", false, null), ("system", "isresultproduced", "system", "Boolean", false, null), ("system", "producedmessages", "system", "ArrayOfLong", false, null), ("system", "error", "system", "String", false, null)), 1000002, ""), // Assigned SchemaId as 1000003. Never change this for this message
      ("com.ligadata.KamanjaBase", "KamanjaStatisticsEvent", "com.ligadata.KamanjaBase.KamanjaStatisticsEvent$", List(("system", "statistics", "system", "string", false, null)), 1000003, ""), // Assigned SchemaId as 1000005. Never change this for this message
      ("com.ligadata.KamanjaBase", "KamanjaExceptionEvent", "com.ligadata.KamanjaBase.KamanjaExceptionEvent$", List(("system", "componentname" , "system", "string", false, null),("system", "timeoferrorepochms" , "system", "long", false, null),("system", "errortype" , "system", "string", false, null),("system", "errorstring" , "system", "string", false, null)), 1000004, ""), // Assigned SchemaId as 1000004. Never change this for this message
      ("com.ligadata.KamanjaBase", "KamanjaExecutionFailureEvent", "com.ligadata.KamanjaBase.KamanjaExecutionFailureEvent$", List(("system", "msgid" , "system", "long", false, null),("system", "timeoferrorepochms" , "system", "long", false, null),("system", "msgcontent" , "system", "string", false, null),("system", "errordetail" , "system", "string", false, null)), 1000005, "") // Assigned SchemaId as 1000005. Never change this for this message
      // NOTE NOTE:- Next SchemaId should start from 1000007
    )
  }

  def ExtMessagesInfo: Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)] = {
    return Array[(String, String, String, List[(String, String, String, String, Boolean, String)], Int, String)](
      ("com.ligadata.KamanjaBase", "KamanjaMessageEvent", "com.ligadata.KamanjaBase.KamanjaMessageEvent$", List(("system", "messageid", "system", "long", false, null),("system", "modelinfo", "com.ligadata.KamanjaBase", "ArrayOfKamanjaModelEvent", false, null),("system", "elapsedtimeinms", "system", "float", false, null),("system", "messagekey", "system", "string", false, null),("system", "messagevalue", "system", "string", false, null),("system", "error", "system", "string", false, null)), 1000006, "") // Assigned SchemaId as 1000002. Never change this for this message
    )
  }
}

class MetadataLoad(val mgr: MdMgr, val typesPath: String, val fcnPath: String, val attrPath: String, msgCtnPath: String) extends LogTrait {
  /** construct the loader and call this to complete the cache initialization */
  def initialize {

    logger.debug("MetadataLoad...loading typedefs")
    InitSystemTenantId

    logger.debug("MetadataLoad...loading typedefs")
    InitTypeDefs

    logger.debug("MetadataLoad...loading ContainerInterfaces definitions")
    InitContainerInterfaces

    logger.debug("MetadataLoad...loading Metric Message definitions")
    InitBaseMessages

    logger.debug("MetadataLoad...loading Pmml udfs")
    init_com_ligadata_pmml_udfs_Udfs

    logger.debug("MetadataLoad...loading Iterable functions")
    InitFcns

    logger.debug("MetadataLoad...loading function macro definitions")
    initMacroDefs

    logger.debug("MetadataLoad...loading FactoryOfModelInstanceFactories definitions")
    initFactoryOfModelInstanceFactories

    logger.debug("MetadataLoad...loading SerializeDeserializeConfig instances for kbinary, csv, and json")
    initSerializeDeserializeConfigs

  }


  private def InitSystemTenantId: Unit = {
    mgr.AddTenantInfo("System" // TenantId
      , "System TenantId" // Description
      , null // no primary database
      , null // no cache config
    )
  }

  /**
    * **HACK ALERT**
    *
    * The serializer/deserializer registration bootstrapped so others can begin activating the adapters.
    *
    * Remove once the md api ingestion is complete........
    */
  private def initSerializeDeserializeConfigs: Unit = {
    mgr.AddSerializer("com.ligadata.kamanja.serializer" // namespace
      , "delimitedserdeser" //name: String
      , 1 //version: Long = 1
      , SerializeDeserializeType.CSV //serializerType: SerializeDeserializeType.SerDeserType
      , "org.kamanja.serdeser.csv.CsvSerDeser" //physicalName: String
      , MetadataLoad.baseTypesOwnerId //ownerId: String
      , MetadataLoad.baseTypesTenantId //tenantId: String
      , 1 //uniqueId: Long
      , 1 //mdElementId: Long
      , "" //jarNm: String = null. Ex: csvserdeser_2.11-1.0.jar
      , Array() //depJars: Array[String]
    )
    mgr.AddSerializer("com.ligadata.kamanja.serializer" // namespace
      , "jsonserdeser" //name: String
      , 1 //version: Long = 1
      , SerializeDeserializeType.JSON //serializerType: SerializeDeserializeType.SerDeserType
      , "org.kamanja.serdeser.json.JsonSerDeser" //physicalName: String
      , MetadataLoad.baseTypesOwnerId //ownerId: String
      , MetadataLoad.baseTypesTenantId //tenantId: String
      , 2 //uniqueId: Long
      , 2 //mdElementId: Long
      , "" //jarNm: String = null. Ex: jsonserdeser_2.11-1.0.jar
      , Array() //depJars: Array[String] = null)
    )
    mgr.AddSerializer("com.ligadata.kamanja.serializer" // namespace
      , "kbinaryserdeser" //name: String
      , 1 //version: Long = 1
      , SerializeDeserializeType.KBinary //serializerType: SerializeDeserializeType.SerDeserType
      , "org.kamanja.serdeser.kbinary.KBinarySerDeser" //physicalName: String
      , MetadataLoad.baseTypesOwnerId //ownerId: String
      , MetadataLoad.baseTypesTenantId //tenantId: String
      , 3 //uniqueId: Long
      , 3 //mdElementId: Long
      , "" //jarNm: String = null. Ex: kbinaryserdeser_2.11-1.0.jar
      , Array() //depJars: Array[String] = null)
    )
  }

  private def initFactoryOfModelInstanceFactories: Unit = {
    ScalaVersionDependentInit.initFactoryOfModelInstanceFactories(mgr)
  }

  def InitBaseMessages: Unit = {
    val baseMessageInfo = MetadataLoad.BaseMessagesInfo
    baseMessageInfo.foreach(bc => {
      logger.debug("MetadataLoad...loading " + bc._2)
      mgr.AddFixedMsg(bc._1, bc._2, bc._3, bc._4, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, bc._5, bc._6)
    })

    // Adding Array types
    baseMessageInfo.foreach(bc => {
      logger.debug("MetadataLoad... adding array of " + bc._2)
      mgr.AddArray(bc._1, "ArrayOf" + bc._2, bc._1, bc._2, 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    })

    val extMessagesInfo = MetadataLoad.ExtMessagesInfo
    extMessagesInfo.foreach(bc => {
      logger.debug("MetadataLoad...loading " + bc._2)
      mgr.AddFixedMsg(bc._1, bc._2, bc._3, bc._4, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, bc._5, bc._6)
    })

    // Adding Array types
    extMessagesInfo.foreach(bc => {
      logger.debug("MetadataLoad... adding array of " + bc._2)
      mgr.AddArray(bc._1, "ArrayOf" + bc._2, bc._1, bc._2, 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    })
  }

  // CMS messages + the dimensional data (treated as Containers)
  def InitContainerInterfaces: Unit = {
    val baseContainerInfo = MetadataLoad.ContainerInterfacesInfo
    baseContainerInfo.foreach(bc => {
      logger.debug("MetadataLoad...loading " + bc._2)
      mgr.AddFixedContainer(bc._1, bc._2, bc._3, bc._4, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, bc._5, bc._6)
    })
  }

  /** Define any types that may be used in the container, message, fcn, and model metadata.  These are broken into smaller functions that
    * will prevent compilation failures due to large function size. */
  def InitTypeDefs = {
    InitTypeDefs1
    InitTypeDefs2
  }

  /** Define any types that may be used in the container, message, fcn, and model metadata */

  private def InitTypeDefs1 = {
    ScalaVersionDependentInit.InitTypeDefs(mgr)

    mgr.AddArray(MdMgr.sysNS, "ArrayOfAny", MdMgr.sysNS, "Any", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfString", MdMgr.sysNS, "String", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfInt", MdMgr.sysNS, "Int", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfLong", MdMgr.sysNS, "Long", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfDouble", MdMgr.sysNS, "Double", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfFloat", MdMgr.sysNS, "Float", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddArray(MdMgr.sysNS, "ArrayOfBoolean", MdMgr.sysNS, "Boolean", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfAny", MdMgr.sysNS, "ArrayOfAny", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfString", MdMgr.sysNS, "ArrayOfString", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfInt", MdMgr.sysNS, "ArrayOfInt", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfLong", MdMgr.sysNS, "ArrayOfLong", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfDouble", MdMgr.sysNS, "ArrayOfDouble", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfFloat", MdMgr.sysNS, "ArrayOfFloat", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddArray(MdMgr.sysNS, "ArrayOfArrayOfBoolean", MdMgr.sysNS, "ArrayOfBoolean", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }

  private def InitTypeDefs2 = {
    mgr.AddMap(MdMgr.sysNS, "MapOfAny", MdMgr.sysNS, "Any", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfFloat", MdMgr.sysNS, "Float", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfDouble", MdMgr.sysNS, "Double", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfInt", MdMgr.sysNS, "Int", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfLong", MdMgr.sysNS, "Long", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfString", MdMgr.sysNS, "String", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddMap(MdMgr.sysNS, "MapOfBoolean", MdMgr.sysNS, "Boolean", MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }

  /** Initialize the function decls for the core pmml udfs.  These are broken into a set of smaller functions so that
    * the maximum function size limit is not exceeded during compilation
    */

  def init_com_ligadata_pmml_udfs_Udfs {

    init_com_ligadata_pmml_udfs_Udfs0
    init_com_ligadata_pmml_udfs_Udfs1
    init_com_ligadata_pmml_udfs_Udfs2
    init_com_ligadata_pmml_udfs_Udfs3
    init_com_ligadata_pmml_udfs_Udfs4
    init_com_ligadata_pmml_udfs_Udfs5
    init_com_ligadata_pmml_udfs_Udfs6
    init_com_ligadata_pmml_udfs_Udfs7
  }


  private def init_com_ligadata_pmml_udfs_Udfs0 {
    mgr.AddArray(MdMgr.sysNS, "ArrayOfContainerInterface", MdMgr.sysNS, "ContainerInterface", 1, MetadataLoad.baseTypesVer, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "idGen", "com.ligadata.pmml.udfs.Udfs.idGen", ("System", "String"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "concat", "com.ligadata.pmml.udfs.Udfs.concat", ("System", "String"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "replace", "com.ligadata.pmml.udfs.Udfs.replace", ("System", "String"), List(("replacewithin", "System", "Any"), ("inWord", "System", "Any"), ("replacewith", "System", "Any")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "matches", "com.ligadata.pmml.udfs.Udfs.matches", ("System", "Boolean"), List(("matchwithin", "System", "Any"), ("matchwith", "System", "Any")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "random", "com.ligadata.pmml.udfs.Udfs.random", ("System", "Double"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "formatNumber", "com.ligadata.pmml.udfs.Udfs.formatNumber", ("System", "String"), List(("num", "System", "Any"), ("formatting", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "logMsg", "com.ligadata.pmml.udfs.Udfs.logMsg", ("System", "Boolean"), List(("severity", "System", "String"), ("contextMsg", "System", "String"), ("eventMsg", "System", "String"), ("bool", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    mgr.AddFunc("Pmml", "CompoundStatementBoolean", "com.ligadata.pmml.udfs.Udfs.CompoundStatementBoolean", ("System", "Boolean"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatementString", "com.ligadata.pmml.udfs.Udfs.CompoundStatementString", ("System", "String"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatementInt", "com.ligadata.pmml.udfs.Udfs.CompoundStatementInt", ("System", "Int"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatementLong", "com.ligadata.pmml.udfs.Udfs.CompoundStatementLong", ("System", "Long"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatementFloat", "com.ligadata.pmml.udfs.Udfs.CompoundStatementFloat", ("System", "Float"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatementDouble", "com.ligadata.pmml.udfs.Udfs.CompoundStatementDouble", ("System", "Double"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompoundStatement", "com.ligadata.pmml.udfs.Udfs.CompoundStatement", ("System", "Any"), List(("args", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    mgr.AddFunc("Pmml", "length", "com.ligadata.pmml.udfs.Udfs.length", ("System", "String"), List(("str", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "trimBlanks", "com.ligadata.pmml.udfs.Udfs.trimBlanks", ("System", "String"), List(("str", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "endsWith", "com.ligadata.pmml.udfs.Udfs.endsWith", ("System", "Boolean"), List(("inThis", "System", "String"), ("findThis", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "startsWith", "com.ligadata.pmml.udfs.Udfs.startsWith", ("System", "Boolean"), List(("inThis", "System", "String"), ("findThis", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "substring", "com.ligadata.pmml.udfs.Udfs.substring", ("System", "String"), List(("str", "System", "String"), ("startidx", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "substring", "com.ligadata.pmml.udfs.Udfs.substring", ("System", "String"), List(("str", "System", "String"), ("startidx", "System", "Int"), ("len", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "lowercase", "com.ligadata.pmml.udfs.Udfs.lowercase", ("System", "String"), List(("str", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "uppercase", "com.ligadata.pmml.udfs.Udfs.uppercase", ("System", "String"), List(("str", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "round", "com.ligadata.pmml.udfs.Udfs.round", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ceil", "com.ligadata.pmml.udfs.Udfs.ceil", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "floor", "com.ligadata.pmml.udfs.Udfs.floor", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "threshold", "com.ligadata.pmml.udfs.Udfs.threshold", ("System", "Int"), List(("x", "System", "Double"), ("y", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "threshold", "com.ligadata.pmml.udfs.Udfs.threshold", ("System", "Int"), List(("x", "System", "Float"), ("y", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "threshold", "com.ligadata.pmml.udfs.Udfs.threshold", ("System", "Int"), List(("x", "System", "Long"), ("y", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "threshold", "com.ligadata.pmml.udfs.Udfs.threshold", ("System", "Int"), List(("x", "System", "Int"), ("y", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "pow", "com.ligadata.pmml.udfs.Udfs.pow", ("System", "Double"), List(("x", "System", "Double"), ("y", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "exp", "com.ligadata.pmml.udfs.Udfs.exp", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "abs", "com.ligadata.pmml.udfs.Udfs.abs", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "abs", "com.ligadata.pmml.udfs.Udfs.abs", ("System", "Float"), List(("expr", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "abs", "com.ligadata.pmml.udfs.Udfs.abs", ("System", "Long"), List(("expr", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "abs", "com.ligadata.pmml.udfs.Udfs.abs", ("System", "Int"), List(("expr", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "sqrt", "com.ligadata.pmml.udfs.Udfs.sqrt", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ln", "com.ligadata.pmml.udfs.Udfs.ln", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "log10", "com.ligadata.pmml.udfs.Udfs.log10", ("System", "Double"), List(("expr", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Product", "com.ligadata.pmml.udfs.Udfs.Product", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Product", "com.ligadata.pmml.udfs.Udfs.Product", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Product", "com.ligadata.pmml.udfs.Udfs.Product", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Product", "com.ligadata.pmml.udfs.Udfs.Product", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Median", "com.ligadata.pmml.udfs.Udfs.Median", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Median", "com.ligadata.pmml.udfs.Udfs.Median", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Median", "com.ligadata.pmml.udfs.Udfs.Median", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Median", "com.ligadata.pmml.udfs.Udfs.Median", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Count", "com.ligadata.pmml.udfs.Udfs.Count", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Count", "com.ligadata.pmml.udfs.Udfs.Count", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Count", "com.ligadata.pmml.udfs.Udfs.Count", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Count", "com.ligadata.pmml.udfs.Udfs.Count", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Avg", "com.ligadata.pmml.udfs.Udfs.Avg", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Avg", "com.ligadata.pmml.udfs.Udfs.Avg", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Avg", "com.ligadata.pmml.udfs.Udfs.Avg", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Avg", "com.ligadata.pmml.udfs.Udfs.Avg", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Sum", "com.ligadata.pmml.udfs.Udfs.Sum", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Sum", "com.ligadata.pmml.udfs.Udfs.Sum", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Sum", "com.ligadata.pmml.udfs.Udfs.Sum", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Sum", "com.ligadata.pmml.udfs.Udfs.Sum", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }


  private def init_com_ligadata_pmml_udfs_Udfs1 {


    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Max", "com.ligadata.pmml.udfs.Udfs.Max", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Min", "com.ligadata.pmml.udfs.Udfs.Min", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Divide", "com.ligadata.pmml.udfs.Udfs.Divide", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Multiply", "com.ligadata.pmml.udfs.Udfs.Multiply", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }


  private def init_com_ligadata_pmml_udfs_Udfs2 {
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Minus", "com.ligadata.pmml.udfs.Udfs.Minus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("exprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("exprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("exprs", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("exprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "String"), List(("exprs", "System", "ArrayOfString")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("expr1", "System", "Long"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Long"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Float"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double"), ("expr3", "System", "Double"), ("expr4", "System", "Double"), ("expr5", "System", "Double"), ("expr6", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double"), ("expr3", "System", "Double"), ("expr4", "System", "Double"), ("expr5", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double"), ("expr3", "System", "Double"), ("expr4", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double"), ("expr3", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Double"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int"), ("expr5", "System", "Int"), ("expr6", "System", "Int"), ("expr7", "System", "Int"), ("expr8", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int"), ("expr5", "System", "Int"), ("expr6", "System", "Int"), ("expr7", "System", "Int"), ("expr8", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long"), ("expr3", "System", "Long"), ("expr4", "System", "Long"), ("expr5", "System", "Long"), ("expr6", "System", "Long"), ("expr7", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int"), ("expr5", "System", "Int"), ("expr6", "System", "Int"), ("expr7", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long"), ("expr3", "System", "Long"), ("expr4", "System", "Long"), ("expr5", "System", "Long"), ("expr6", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int"), ("expr5", "System", "Int"), ("expr6", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long"), ("expr3", "System", "Long"), ("expr4", "System", "Long"), ("expr5", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int"), ("expr5", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long"), ("expr3", "System", "Long"), ("expr4", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int"), ("expr4", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Long"), List(("expr1", "System", "Long"), ("expr2", "System", "Long"), ("expr3", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int"), ("expr3", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "Int"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Plus", "com.ligadata.pmml.udfs.Udfs.Plus", ("System", "String"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Boolean"), ("expr2", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotEqual", "com.ligadata.pmml.udfs.Udfs.NotEqual", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Boolean"), ("expr2", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Equal", "com.ligadata.pmml.udfs.Udfs.Equal", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessThan", "com.ligadata.pmml.udfs.Udfs.LessThan", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "LessOrEqual", "com.ligadata.pmml.udfs.Udfs.LessOrEqual", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterOrEqual", "com.ligadata.pmml.udfs.Udfs.GreaterOrEqual", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Long"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Float"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Double"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "Int"), ("expr2", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GreaterThan", "com.ligadata.pmml.udfs.Udfs.GreaterThan", ("System", "Boolean"), List(("expr1", "System", "String"), ("expr2", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }


  private def init_com_ligadata_pmml_udfs_Udfs3 {
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Float"), ("leftMargin", "System", "Float"), ("rightMargin", "System", "Float"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Int"), ("leftMargin", "System", "Int"), ("rightMargin", "System", "Float"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Float"), ("leftMargin", "System", "Float"), ("rightMargin", "System", "Int"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Float"), ("leftMargin", "System", "Float"), ("rightMargin", "System", "Double"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Double"), ("leftMargin", "System", "Double"), ("rightMargin", "System", "Float"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Double"), ("leftMargin", "System", "Double"), ("rightMargin", "System", "Double"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Int"), ("leftMargin", "System", "Int"), ("rightMargin", "System", "Double"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Double"), ("leftMargin", "System", "Double"), ("rightMargin", "System", "Int"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Int"), ("leftMargin", "System", "Long"), ("rightMargin", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Long"), ("leftMargin", "System", "Long"), ("rightMargin", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "Int"), ("leftMargin", "System", "Int"), ("rightMargin", "System", "Int"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Between", "com.ligadata.pmml.udfs.Udfs.Between", ("System", "Boolean"), List(("thisOne", "System", "String"), ("leftMargin", "System", "String"), ("rightMargin", "System", "String"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Not", "com.ligadata.pmml.udfs.Udfs.Not", ("System", "Boolean"), List(("boolexpr", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddFunc("Pmml", "First", "com.ligadata.pmml.udfs.Udfs.First", ("System", "Any"), List(("coll", "System", "ArrayOfAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddFunc("Pmml", "Last", "com.ligadata.pmml.udfs.Udfs.Last", ("System", "Any"), List(("coll", "System", "ArrayOfAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Contains", "com.ligadata.pmml.udfs.Udfs.Contains", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfDouble"), ("key", "System", "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Contains", "com.ligadata.pmml.udfs.Udfs.Contains", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfFloat"), ("key", "System", "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Contains", "com.ligadata.pmml.udfs.Udfs.Contains", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfInt"), ("key", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Contains", "com.ligadata.pmml.udfs.Udfs.Contains", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfLong"), ("key", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Contains", "com.ligadata.pmml.udfs.Udfs.Contains", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfString"), ("key", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "NotAnyBetween", "com.ligadata.pmml.udfs.Udfs.NotAnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfDouble"), ("leftMargin", "System", "Double"), ("rightMargin", "System", "Double"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotAnyBetween", "com.ligadata.pmml.udfs.Udfs.NotAnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfFloat"), ("leftMargin", "System", "Float"), ("rightMargin", "System", "Float"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotAnyBetween", "com.ligadata.pmml.udfs.Udfs.NotAnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfInt"), ("leftMargin", "System", "Int"), ("rightMargin", "System", "Int"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotAnyBetween", "com.ligadata.pmml.udfs.Udfs.NotAnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfLong"), ("leftMargin", "System", "Long"), ("rightMargin", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "NotAnyBetween", "com.ligadata.pmml.udfs.Udfs.NotAnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfString"), ("leftMargin", "System", "String"), ("rightMargin", "System", "String"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "AnyBetween", "com.ligadata.pmml.udfs.Udfs.AnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfDouble"), ("leftMargin", "System", "Double"), ("rightMargin", "System", "Double"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "AnyBetween", "com.ligadata.pmml.udfs.Udfs.AnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfFloat"), ("leftMargin", "System", "Float"), ("rightMargin", "System", "Float"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "AnyBetween", "com.ligadata.pmml.udfs.Udfs.AnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfInt"), ("leftMargin", "System", "Int"), ("rightMargin", "System", "Int"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "AnyBetween", "com.ligadata.pmml.udfs.Udfs.AnyBetween", ("System", "Boolean"), List(("arrayExpr", "System", "ArrayOfString"), ("leftMargin", "System", "String"), ("rightMargin", "System", "String"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "IsIn", "com.ligadata.pmml.udfs.Udfs.IsIn", ("System", "Boolean"), List(("fldRefExpr", "System", "Double"), ("setExprs", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "IsIn", "com.ligadata.pmml.udfs.Udfs.IsIn", ("System", "Boolean"), List(("fldRefExpr", "System", "Float"), ("setExprs", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "IsIn", "com.ligadata.pmml.udfs.Udfs.IsIn", ("System", "Boolean"), List(("fldRefExpr", "System", "Int"), ("setExprs", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "IsIn", "com.ligadata.pmml.udfs.Udfs.IsIn", ("System", "Boolean"), List(("fldRefExpr", "System", "String"), ("setExprs", "System", "ArrayOfString")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }


  private def init_com_ligadata_pmml_udfs_Udfs4 {
    mgr.AddFunc("Pmml", "IntAnd", "com.ligadata.pmml.udfs.Udfs.IntAnd", ("System", "Boolean"), List(("boolexpr", "System", "Boolean")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "And", "com.ligadata.pmml.udfs.Udfs.And", ("System", "Boolean"), List(("boolexpr", "System", "Boolean")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "IntOr", "com.ligadata.pmml.udfs.Udfs.IntOr", ("System", "Boolean"), List(("boolexpr", "System", "Boolean")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Or", "com.ligadata.pmml.udfs.Udfs.Or", ("System", "Boolean"), List(("boolexpr", "System", "Boolean")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "GetArray", "com.ligadata.pmml.udfs.Udfs.GetArray", ("System", "ArrayOfContainerInterface"), List(("xId", "System", "Long"), ("gCtx", "System", "EnvContext"), ("containerId", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "GetArray", "com.ligadata.pmml.udfs.Udfs.GetArray", ("System", "ArrayOfContainerInterface"), List(("ctx", "System", "Context"), ("containerId", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfString")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Float")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Double")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfBoolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Any")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Put", "com.ligadata.pmml.udfs.Udfs.Put", (MdMgr.sysNS, "Boolean"), List(("ctx", "System", "Context"), ("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

  }


  private def init_com_ligadata_pmml_udfs_Udfs5 {
    //		mgr.AddFunc("Pmml", "CollectionLength", "com.ligadata.pmml.udfs.Udfs.CollectionLength", ("System", "Int"), List(("coll", "System", "ArrayOfAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //		mgr.AddFunc("Pmml", "CollectionLength", "com.ligadata.pmml.udfs.Udfs.CollectionLength", ("System", "Int"), List(("coll", "System", "MapOfAnyAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //mgr.AddFunc("Pmml", "CollectionLength", "com.ligadata.pmml.udfs.Udfs.CollectionLength", ("System", "Int"), List(("coll", "System", "Stack[T]")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    //mgr.AddFunc("Pmml", "CollectionLength", "com.ligadata.pmml.udfs.Udfs.CollectionLength", ("System", "Int"), List(("coll", "System", "Vector[T]")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    //  		mgr.AddFunc("Pmml", "Add", "com.ligadata.pmml.udfs.Udfs.Add", ("System", "ArrayofAny"), List(("coll", "System", "ArrayOfAny"), ("items", "System", "Any")), scala.collection.mutable.Set[FcnMacroAttr.Feature](FcnMacroAttr.HAS_INDEFINITE_ARITY), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "getXid", "com.ligadata.pmml.udfs.Udfs.getXid", ("System", "Long"), List(("ctx", "System", "Context")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }

  private def init_com_ligadata_pmml_udfs_Udfs6 {

    //		mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfAny"), List(("arr", "System", "ArrayOfAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfInt"), List(("arr", "System", "ArrayOfInt")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfLong"), List(("arr", "System", "ArrayOfLong")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfDouble"), List(("arr", "System", "ArrayOfDouble")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfFloat"), List(("arr", "System", "ArrayOfFloat")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfBoolean"), List(("arr", "System", "ArrayOfBoolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "ToArray", "com.ligadata.pmml.udfs.Udfs.ToArray", ("System", "ArrayOfString"), List(("arr", "System", "ArrayOfString")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }

  private def init_com_ligadata_pmml_udfs_Udfs7 {

    // 		mgr.AddFunc("Pmml", "MapKeys", "com.ligadata.pmml.udfs.Udfs.MapKeys", ("System", "ArrayOfInt"), List(("receiver", "System", "MapOfIntAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    // 		mgr.AddFunc("Pmml", "MapKeys", "com.ligadata.pmml.udfs.Udfs.MapKeys", ("System", "ArrayOfAny"), List(("receiver", "System", "MapOfAnyAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    // 		mgr.AddFunc("Pmml", "MapValues", "com.ligadata.pmml.udfs.Udfs.MapValues", ("System", "ArrayOfAny"), List(("receiver", "System", "MapOfAnyAny")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    /** time/date functions */
    mgr.AddFunc("Pmml", "AgeCalc", "com.ligadata.pmml.udfs.Udfs.AgeCalc", ("System", "Int"), List(("yyyymmdd", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompressedTimeHHMMSSCC2Secs", "com.ligadata.pmml.udfs.Udfs.CompressedTimeHHMMSSCC2Secs", ("System", "Int"), List(("compressedTime", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "AsCompressedDate", "com.ligadata.pmml.udfs.Udfs.AsCompressedDate", ("System", "Int"), List(("milliSecs", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "MonthFromISO8601Int", "com.ligadata.pmml.udfs.Udfs.MonthFromISO8601Int", ("System", "Int"), List(("dt", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "YearFromISO8601Int", "com.ligadata.pmml.udfs.Udfs.YearFromISO8601Int", ("System", "Int"), List(("dt", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "DayOfMonthFromISO8601Int", "com.ligadata.pmml.udfs.Udfs.DayOfMonthFromISO8601Int", ("System", "Int"), List(("dt", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "AsSeconds", "com.ligadata.pmml.udfs.Udfs.AsSeconds", ("System", "Long"), List(("milliSecs", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Timenow", "com.ligadata.pmml.udfs.Udfs.Timenow", ("System", "Long"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Now", "com.ligadata.pmml.udfs.Udfs.Now", ("System", "Long"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "YearsAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("numYrs", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "YearsAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("someDate", "System", "Int"), ("numYrs", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "MonthsAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("numMos", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "MonthsAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("someDate", "System", "Int"), ("numMos", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "WeeksAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("numWks", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "WeeksAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("someDate", "System", "Int"), ("numWks", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "DaysAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "DaysAgo", "com.ligadata.pmml.udfs.Udfs.YearsAgo", ("System", "Long"), List(("someDate", "System", "Int"), ("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "toMillisFromJulian", "com.ligadata.pmml.udfs.Udfs.toMillisFromJulian", ("System", "Long"), List(("yyddd", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "CompressedTimeHHMMSSCC2MilliSecs", "com.ligadata.pmml.udfs.Udfs.CompressedTimeHHMMSSCC2MilliSecs", ("System", "Long"), List(("compressedTime", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "DaysAgoAsISO8601", "com.ligadata.pmml.udfs.Udfs.DaysAgoAsISO8601", ("System", "Int"), List(("someDate", "System", "Int"), ("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "WeeksAgoAsISO8601", "com.ligadata.pmml.udfs.Udfs.WeeksAgoAsISO8601", ("System", "Int"), List(("someDate", "System", "Int"), ("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "MonthsAgoAsISO8601", "com.ligadata.pmml.udfs.Udfs.MonthsAgoAsISO8601", ("System", "Int"), List(("someDate", "System", "Int"), ("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "YearsAgoAsISO8601", "com.ligadata.pmml.udfs.Udfs.YearsAgoAsISO8601", ("System", "Int"), List(("someDate", "System", "Int"), ("numDays", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "iso8601DateFmt", "com.ligadata.pmml.udfs.Udfs.iso8601DateFmt", ("System", "String"), List(("fmtStr", "System", "String"), ("yyyymmdds", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "timestampFmt", "com.ligadata.pmml.udfs.Udfs.timestampFmt", ("System", "String"), List(("fmtStr", "System", "String"), ("timestamp", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "timeStampFromStr", "com.ligadata.pmml.udfs.Udfs.timeStampFromStr", ("System", "Long"), List(("fmtStr", "System", "String"), ("timestampStr", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateFromStr", "com.ligadata.pmml.udfs.Udfs.dateFromStr", ("System", "Long"), List(("fmtStr", "System", "String"), ("timestampStr", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "timeFromStr", "com.ligadata.pmml.udfs.Udfs.timeFromStr", ("System", "Long"), List(("fmtStr", "System", "String"), ("timestampStr", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateSecondsSinceYear", "com.ligadata.pmml.udfs.Udfs.dateSecondsSinceYear", ("System", "Long"), List(("yr", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateDaysSinceYear", "com.ligadata.pmml.udfs.Udfs.dateDaysSinceYear", ("System", "Long"), List(("yr", "System", "Int")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateMilliSecondsSinceMidnight", "com.ligadata.pmml.udfs.Udfs.dateMilliSecondsSinceMidnight", ("System", "Long"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateSecondsSinceMidnight", "com.ligadata.pmml.udfs.Udfs.dateSecondsSinceMidnight", ("System", "Long"), List(), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateSecondsSinceMidnight", "com.ligadata.pmml.udfs.Udfs.dateSecondsSinceMidnight", ("System", "Long"), List(("fmtStr", "System", "String"), ("timestampStr", "System", "String")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "dateSecondsSinceMidnight", "com.ligadata.pmml.udfs.Udfs.dateSecondsSinceMidnight", ("System", "Long"), List(("timestamp", "System", "Long")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "millisecsBetween", "com.ligadata.pmml.udfs.Udfs.millisecsBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "secondsBetween", "com.ligadata.pmml.udfs.Udfs.secondsBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "minutesBetween", "com.ligadata.pmml.udfs.Udfs.minutesBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "hoursBetween", "com.ligadata.pmml.udfs.Udfs.hoursBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "daysBetween", "com.ligadata.pmml.udfs.Udfs.daysBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "weeksBetween", "com.ligadata.pmml.udfs.Udfs.weeksBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "monthsBetween", "com.ligadata.pmml.udfs.Udfs.monthsBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "yearsBetween", "com.ligadata.pmml.udfs.Udfs.yearsBetween", ("System", "Long"), List(("time1", "System", "Long"), ("time2", "System", "Long"), ("inclusive", "System", "Boolean")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    mgr.AddFunc("Pmml", "Version", "com.ligadata.pmml.udfs.Udfs.Version", ("System", "String"), List(("msg", "System", "MessageInterface")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
    mgr.AddFunc("Pmml", "Version", "com.ligadata.pmml.udfs.Udfs.Version", ("System", "String"), List(("msg", "System", "ContainerInterface")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddFunc("Pmml", "ToString", "com.ligadata.pmml.udfs.Udfs.ToString", ("System", "String"), List(("arg", "System", "Any")), null, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

  }

  def InitFcns = {
    /**
      * NOTE: These functions are variable in nature, more like macros than
      * actual functions.  They actually deploy two
      * functions (in most cases): the outer container function (e.g., Map or Filter) and the inner
      * function that will operate on the members of the container in some way.

      * Since we only know the outer function that will be used, only it is
      * described.  The inner function is specified in the pmml and the arguments
      * and function lookup are separately done for it. The inner functions will be one of the
      * be one of the other udfs that are defined in the core udf lib
      * (e.g., Between(somefield, low, hi, inclusive)

      * Note too that only the "Any" version of these container types are defined.
      * The code generation will utilize the real item type of the container
      * to cast the object "down" to the right type.

      * Note that they all have the "isIterable" boolean set to true.

      * nameSpace: String
      * , name: String
      * , physicalName: String
      * , retTypeNsName: (String, String)
      * , args: List[(String, String, String)]
      * , fmfeatures : Set[FcnMacroAttr.Feature]

      */
    var fcnMacrofeatures: Set[FcnMacroAttr.Feature] = Set[FcnMacroAttr.Feature]()
    fcnMacrofeatures += FcnMacroAttr.ITERABLE
    logger.debug("MetadataLoad...loading container filter functions")
    mgr.AddFunc(MdMgr.sysNS
      , "ContainerFilter"
      , "com.ligadata.pmml.udfs.Udfs.ContainerFilter"
      , (MdMgr.sysNS, "ArrayOfAny")
      , List(("containerId", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    logger.debug("MetadataLoad...loading container map functions")
    mgr.AddFunc(MdMgr.sysNS
      , "ContainerMap"
      , "com.ligadata.pmml.udfs.Udfs.ContainerMap"
      , (MdMgr.sysNS, "ArrayOfAny")
      , List(("containerId", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    logger.debug("MetadataLoad...loading container groupBy functions")
    mgr.AddFunc(MdMgr.sysNS
      , "GroupBy"
      , "com.ligadata.pmml.udfs.Udfs.GroupBy"
      , (MdMgr.sysNS, "ArrayOfAny")
      , List(("containerId", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures, MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)
  }

  /** Initialize the macro definitions used by the pmml compiler.  The private functions called are utilized to
    * prevent excessively large functions that will flummox the compiler.
    */
  def initMacroDefs {
    logger.debug("MetadataLoad...loading Macro functions")
    initMacroDefs1
    initMacroDefs2

  }

  private def initMacroDefs1 {


    /** ************************************************************
      *
      * NOTE: For the Builds portion of the Builds/Does macros that
      * do the class update contexts... make sure the class name is
      * unique.  Do this by qualifying the name with all of the arguments
      * s.t. there is no confusion between 3 and 4 argument macros that
      * have the same class base name.
      *
      * **************************************************************/

    /** catalog the CLASSUPDATE oriented macros:

      */

    var fcnMacrofeatures: Set[FcnMacroAttr.Feature] = Set[FcnMacroAttr.Feature]()
    fcnMacrofeatures += FcnMacroAttr.CLASSUPDATE


    /** Macros Associated with this macro template:
      * "incrementBy(Any,Int,Int)"
      * "incrementBy(Any,Double,Double)"
      * "incrementBy(Any,Long,Long)"

      * Something like the following code would cause the macro to be used were
      * the AlertsToday a FixedField container...
      * <Apply function="incrementBy">
      * <FieldRef field="AlertsToday.Sent"/>
      * <Constant dataType="integer">1</Constant>
      * </Apply>

      */
    val SetFieldMacroStringFixed: String =
      """
	class %1%_%2%_%3%_setField(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def setField  : Boolean = { %1%.%2% = %3%.asInstanceOf[%2_type%]; true }
	} """

    val SetFieldMacroStringMapped: String =
      """
	class %1%_%2%_%3%_setField(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def setField  : Boolean = { %1%.set("%2%", %3%.asInstanceOf[%2_type%]); true }
	} """

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Int"), ("value", MdMgr.sysNS, "Int"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfInt"), ("value", MdMgr.sysNS, "ArrayOfInt"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Double"), ("value", MdMgr.sysNS, "Double"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfDouble"), ("value", MdMgr.sysNS, "ArrayOfDouble"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Long"), ("value", MdMgr.sysNS, "Long"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfLong"), ("value", MdMgr.sysNS, "ArrayOfLong"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Boolean"), ("value", MdMgr.sysNS, "Boolean"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfBoolean"), ("value", MdMgr.sysNS, "ArrayOfBoolean"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfString"), ("value", MdMgr.sysNS, "ArrayOfString"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Int"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfInt"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Double"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfDouble"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Long"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfLong"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Boolean"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfBoolean"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "ArrayOfString"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldMacroStringFixed, SetFieldMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    val SetFieldNullMacroStringFixed: String =
      """
	class %1%_%2%_setFieldNull(val ctx : Context, var %1% : %1_type%)
	{
	  	def setFieldNull  : Boolean = { %1%.%2% = null.asInstanceOf[%2_type%]; true }
	} """

    val SetFieldNullMacroStringMapped: String =
      """
	class %1%_%2%_setFieldNull(val ctx : Context, var %1% : %1_type%)
	{
	  	def setFieldNull  : Boolean = { %1%.set("%2%", null.asInstanceOf[%2_type%]); true }
	} """

    mgr.AddMacro(MdMgr.sysNS
      , "setFieldNull"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (SetFieldNullMacroStringFixed, SetFieldNullMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    val SetFieldMacroContainerStringFixed: String =
      """
	class %1%_%2%_%3%_%4%_setField(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def setField  : Boolean = { %1%.%2% = %3%.%4%; true }
	} """

    val SetFieldMacroContainerStringMapped: String =
      """
	class %1%_%2%_%3%_%4%_setField(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def setField  : Boolean = { %1%.set("%2%", %3%.get("%4%").asInstanceOf[%4_type%]); true }
	} """

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("toContainer", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Int"), ("fromContainer", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "Int"))
      , fcnMacrofeatures
      , (SetFieldMacroContainerStringFixed, SetFieldMacroContainerStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("toContainer", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Double"), ("fromContainer", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "Double"))
      , fcnMacrofeatures
      , (SetFieldMacroContainerStringFixed, SetFieldMacroContainerStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("toContainer", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Long"), ("fromContainer", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "Long"))
      , fcnMacrofeatures
      , (SetFieldMacroContainerStringFixed, SetFieldMacroContainerStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("toContainer", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Boolean"), ("fromContainer", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "Boolean"))
      , fcnMacrofeatures
      , (SetFieldMacroContainerStringFixed, SetFieldMacroContainerStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "setField"
      , (MdMgr.sysNS, "Boolean")
      , List(("toContainer", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "String"), ("fromContainer", MdMgr.sysNS, "Any"), ("value", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (SetFieldMacroContainerStringFixed, SetFieldMacroContainerStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** Macros Associated with this macro template:
      * "incrementBy(Any,Int,Int)"
      * "incrementBy(Any,Double,Double)"
      * "incrementBy(Any,Long,Long)"

      * Something like the following code would cause the macro to be used were
      * the AlertsToday a FixedField container...
      * <Apply function="incrementBy">
      * <FieldRef field="AlertsToday.Sent"/>
      * <Constant dataType="integer">1</Constant>
      * </Apply>

      */
    val incrementByMacroStringFixed: String =
      """
	class %1%_%2%_%3%_incrementBy(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def incrementBy  : Boolean = { %1%.%2% += %3%; true }
	} """

    val incrementByMacroStringMapped: String =
      """
	class %1%_%2%_%3%_incrementBy(val ctx : Context, var %1% : %1_type%, val %3% : %3_type%)
	{
	  	def incrementBy  : Boolean = { %1%.set("%2%", (%1%.get("%2%").asInstanceOf[%2_type%] + %3%)); true }
	} """

    mgr.AddMacro(MdMgr.sysNS
      , "incrementBy"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Int"), ("value", MdMgr.sysNS, "Int"))
      , fcnMacrofeatures
      , (incrementByMacroStringFixed, incrementByMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "incrementBy"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Double"), ("value", MdMgr.sysNS, "Double"))
      , fcnMacrofeatures
      , (incrementByMacroStringFixed, incrementByMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    mgr.AddMacro(MdMgr.sysNS
      , "incrementBy"
      , (MdMgr.sysNS, "Boolean")
      , List(("container", MdMgr.sysNS, "Any"), ("containerField", MdMgr.sysNS, "Long"), ("value", MdMgr.sysNS, "Long"))
      , fcnMacrofeatures
      , (incrementByMacroStringFixed, incrementByMacroStringMapped), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

  }

  private def initMacroDefs2 {

    var fcnMacrofeatures: Set[FcnMacroAttr.Feature] = Set[FcnMacroAttr.Feature]()

    /** **************************************************************************************************************/

    val putGlobalContainerFixedMacroTemplate: String =
      """
	class %1%_%2%_%3%_%4%_Put(val ctx : Context, var %1% : %1_type%, val %2% : %2_type%, val %3% : %3_type%, val %4% : %4_type%)
	{
	  	def Put  : Boolean = { %1%.setObject(ctx.xId, %2%, %3%, %4%); true }
	} """

    val putGlobalContainerMappedMacroTemplate: String =
      """
	class %1%_%2%_%3%_%4%_Put(val ctx : Context, var %1% : %1_type%, val %2% : %2_type%, val %3% : %3_type%, val %4% : %4_type%)
	{
	  	def Put  : Boolean = { %1%.setObject(ctx.xId, %2%, %3%, %4%); true }
	} """

    /** EnvContext write access methods:
      * def setObject(transId: Long, containerName: String, key: String, value: ContainerInterface): Unit
      * def setObject(transId: Long, containerName: String, key: Any, value: ContainerInterface): Unit

      * mgr.AddMacro(MdMgr.sysNS
      * , "Put"
      * , (MdMgr.sysNS, "Boolean")
      * , List(("gCtx", MdMgr.sysNS, "EnvContext")
      * , ("containerName", MdMgr.sysNS, "String")
      * , ("key", MdMgr.sysNS, "ListOfString")
      * , ("value", MdMgr.sysNS, "ContainerInterface"))
      * , fcnMacrofeatures
      * , (putGlobalContainerFixedMacroTemplate,putGlobalContainerMappedMacroTemplate))
      */

    /**
      * val putLongVariableMacroPmmlDict : String =    """
      * class %1%_%2%_PutLong(val ctx : Context, var %1% : %1_type%, val %2% : %2_type%)
      * {
      * //resort to setting the Long value to local variable to insure scala compiler recognizes the appropriate coercion...
      * // 	with a constant as the value present, it will match to Int and fail for large values
      * def Put  : Boolean = { val l : %2_type% = %2%; Put(ctx, %1%, l); true }
      * } """

      * mgr.AddMacro(MdMgr.sysNS
      * , "Put"
      * , (MdMgr.sysNS, "Boolean")
      * , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Long"))
      * , fcnMacrofeatures
      * , (putLongVariableMacroPmmlDict,putLongVariableMacroPmmlDict))
      */

    /** **************************************************************************************************************/

    /** ***********************************************************************
      * Catalog the ITERABLE only macros (no class generation needed for these
      * *************************************************************************/
    fcnMacrofeatures.clear
    fcnMacrofeatures += FcnMacroAttr.ITERABLE

    /**
      * Macros associated with the 'putVariableMacroPmmlDict' macro template:
      * "Put(String,String)"
      * "Put(String,Int)"
      * "Put(String,Long)"
      * "Put(String,Double)"
      * "Put(String,Boolean)"
      * "Put(String,Any)"

      * Notes:
      * 1) No "mapped" version of the template needed for this case.
      * 2) These functions can ONLY be used inside objects that have access to the model's ctx
      * (e.g., inside the 'execute(ctx : Context)' function of a derived field)
      */

    val putVariableMacroPmmlDict: String =
      """Put(ctx, %1%, %2%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfString"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Int"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfInt"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Long"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfLong"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Double"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfDouble"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Float"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfFloat"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Boolean"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfBoolean"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "Put"
      , (MdMgr.sysNS, "Boolean")
      , List(("variableName", MdMgr.sysNS, "String"), ("value", MdMgr.sysNS, "ArrayOfAny"))
      , fcnMacrofeatures
      , (putVariableMacroPmmlDict, putVariableMacroPmmlDict), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /**
      * isMissing and isNotMissing macros
      * no special macro needed for mapped ...
      */
    val isMissingMacro: String =
      """IsMissing(ctx, %1%)"""
    val isNotMissingMacro: String = """IsNotMissing(ctx, %1%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "isMissing"
      , (MdMgr.sysNS, "Boolean")
      , List(("fieldRefName", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (isMissingMacro, isMissingMacro), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "isNotMissing"
      , (MdMgr.sysNS, "Boolean")
      , List(("fieldRefName", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (isNotMissingMacro, isNotMissingMacro), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /**
      * Transaction id access
      * no special macro needed for mapped ...
      */
    val getXidMacro: String =
      """ctx.xId"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "GetXid"
      , (MdMgr.sysNS, "Long")
      , List()
      , fcnMacrofeatures
      , (getXidMacro, getXidMacro), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    /**
      * DowncastArrayMbr Macro used to cast arrays of ContainerInterface to arrays of some specified type
      */
    val DowncastArrayMbrTemplate: String =
      """%1%.map(itm => itm.asInstanceOf[%2%])"""

    mgr.AddMacro(MdMgr.sysNS
      , "DownCastArrayMembers"
      , (MdMgr.sysNS, "ArrayOfAny")
      , List(("arrayExpr", MdMgr.sysNS, "ArrayOfAny"), ("mbrType", MdMgr.sysNS, "Any"))
      , fcnMacrofeatures
      , (DowncastArrayMbrTemplate, DowncastArrayMbrTemplate), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId)


    /**
      * Catalog EnvContext read access macros.  Inject the transaction id as the first arg

      * def getAllObjects(transId: Long, containerName: String): Array[ContainerInterface]
      * def getObject(transId: Long, containerName: String, key: String): ContainerInterface

      * def contains(transId: Long, containerName: String, key: String): Boolean
      * def containsAny(transId: Long, containerName: String, keys: Array[String]): Boolean
      * def containsAll(transId: Long, containerName: String, keys: Array[String]): Boolean
      */

    val getAllObjectsMacroTemplate: String =
      """GetArray(ctx.xId, %1%, %2%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */
    mgr.AddMacro(MdMgr.sysNS
      , "GetArray"
      , (MdMgr.sysNS, "ArrayOfContainerInterface")
      , List(("gCtx", MdMgr.sysNS, "EnvContext")
        , ("containerName", MdMgr.sysNS, "String"))
      , fcnMacrofeatures
      , (getAllObjectsMacroTemplate, getAllObjectsMacroTemplate), MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId
      , -1)

    val getHistoryMacroTemplate: String = """GetHistory(ctx.xId, %1%, %2%, %3%, %4%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */

    val getObjectMacroTemplate: String = """Get(ctx.xId, %1%, %2%, %3%, %4%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */


    val getObjectElseNewMacroTemplate: String = """GetMsgContainerElseNew(ctx.xId, %1%, %2%, %3%, %4%, %5%)"""

    /** @deprecated ("Use <Constant dataType="context">ctx</Constant> as the first arg and match function directly", "2015-Jun-08") */

    val containsMacroTemplate: String = """Contains(ctx.xId, %1%, %2%, %3%, %4%)"""

    /*
        val containsAnyMacroTemplate : String =   """ContainsAny(ctx.xId, %1%, %2%, %3%)"""

        mgr.AddMacro(MdMgr.sysNS
              , "ContainsAny"
              , (MdMgr.sysNS, "Boolean")
              , List(("gCtx", MdMgr.sysNS, "EnvContext")
                  , ("containerName", MdMgr.sysNS, "String")
                  , ("key", MdMgr.sysNS, "ArrayOfString"))
              , fcnMacrofeatures
              , (containsAnyMacroTemplate,containsAnyMacroTemplate)
              ,-1)

        val containsAllMacroTemplate : String =   """ContainsAll(ctx.xId, %1%, %2%, %3%)"""

        mgr.AddMacro(MdMgr.sysNS
              , "ContainsAll"
              , (MdMgr.sysNS, "Boolean")
              , List(("gCtx", MdMgr.sysNS, "EnvContext")
                  , ("containerName", MdMgr.sysNS, "String")
                  , ("key", MdMgr.sysNS, "ArrayOfString"))
              , fcnMacrofeatures
              , (containsAllMacroTemplate,containsAllMacroTemplate)
              ,-1)
    */
  }

}

