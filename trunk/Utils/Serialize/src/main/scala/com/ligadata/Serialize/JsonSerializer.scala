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

package com.ligadata.Serialize

import com.ligadata.kamanja.metadata.MiningModelType
import com.ligadata.kamanja.metadata.ModelRepresentation
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata.MdMgr._
import scala.collection.mutable.{ArrayBuffer}
import org.apache.logging.log4j._

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import com.ligadata.Exceptions._
import com.ligadata.AuditAdapterInfo.AuditRecord

import java.util.Date
import scala.collection.mutable.{HashMap}

case class TypeDef(MetadataType: String, NameSpace: String, Name: String, TypeTypeName: String, TypeNameSpace: String, TypeName: String, PhysicalName: String, var Version: String, JarName: String, DependencyJars: List[String], Implementation: String, OwnerId: String, TenantId: String, UniqueId: Long, MdElementId: Long, Fixed: Option[Boolean], NumberOfDimensions: Option[Int], KeyTypeNameSpace: Option[String], KeyTypeName: Option[String], ValueTypeNameSpace: Option[String], ValueTypeName: Option[String], TupleDefinitions: Option[List[TypeDef]])

case class TypeDefList(Types: List[TypeDef])

case class Argument(ArgName: String, ArgTypeNameSpace: String, ArgTypeName: String)

case class Function(NameSpace: String, Name: String, PhysicalName: String, ReturnTypeNameSpace: String, ReturnTypeName: String, Arguments: List[Argument], Features: List[String], Version: String, JarName: String, DependantJars: List[String], OwnerId: String, TenantId: String, UniqueId: Long, MdElementId: Long)

case class FunctionList(Functions: List[Function])

//case class Concept(NameSpace: String,Name: String, TypeNameSpace: String, TypeName: String,Version: String,Description: String, Author: String, ActiveDate: String)
case class Concept(NameSpace: String, Name: String, TypeNameSpace: String, TypeName: String, Version: String, OwnerId: String, TenantId: String, UniqueId: Long, MdElementId: Long)

case class ConceptList(Concepts: List[Concept])

case class Attr(NameSpace: String, Name: String, Version: Long, CollectionType: Option[String], Type: TypeDef)

case class DerivedConcept(FunctionDefinition: Function, Attributes: List[Attr])

case class MessageStruct(NameSpace: String, Name: String, FullName: String, Version: Long, JarName: String, PhysicalName: String, DependencyJars: List[String], Attributes: List[Attr], OwnerId: String, TenantId: String, UniqueId: Long, MdElementId: Long, SchemaId: Int, AvroSchema:String)

case class MessageDefinition(Message: MessageStruct)

case class ContainerDefinition(Container: MessageStruct)

//case class ModelInfo(NameSpace: String, Name: String, Version: String, ModelType: String, JarName: String, PhysicalName: String, DependencyJars: List[String], InputAttributes: List[Attr], OutputAttributes: List[Attr])

case class InputMsgAndAttributes(message: String, attributes: List[String])

case class ModelInfo(NameSpace: String
                     , Name: String
                     , Version: String
                     , PhysicalName: String
                     , OwnerId: String, TenantId: String
                     , UniqueId: Long
                     , MdElementId: Long
                     , ModelRep: String
                     , ModelType: String
                     , InputMsgAndAttribsSets: List[List[InputMsgAndAttributes]] // Each set is List of MsgName & Full qualified MsgAttributes. And we have multiple sets
                     , OutputMsgs: List[String]
                     , IsReusable: Boolean
                     , ObjectDefinition: String
                     , ObjectFormat: String
                     , JarName: String
                     , DependencyJars: List[String]
                     , Recompile: Boolean
                     , SupportsInstanceSerialization: Boolean)

case class ModelDefinition(Model: ModelInfo)

case class ParameterMap(RootDir: String, GitRootDir: String, Database: String, DatabaseHost: String, JarTargetDir: String, ScalaHome: String, JavaHome: String, ManifestPath: String, ClassPath: String, NotifyEngine: String, ZooKeeperConnectString: String)

case class MetadataApiConfig(ApiConfigParameters: ParameterMap)

case class ZooKeeperNotification(ObjectType: String, Operation: String, NameSpace: String, Name: String, Version: String, PhysicalName: String, JarName: String, DependantJars: List[String], ConfigContnent: Option[String])

case class ZooKeeperTransaction(Notifications: List[ZooKeeperNotification], transactionId: Option[String])
//case class ZooKeeperConfigTransaction()

case class JDataStore(StoreType: String, SchemaName: String, Location: String, AdapterSpecificConfig: Option[String])

case class JZKInfo(ZooKeeperNodeBasePath: String, ZooKeeperConnectString: String, ZooKeeperSessionTimeoutMs: Option[String], ZooKeeperConnectionTimeoutMs: Option[String])

case class JEnvCtxtJsonStr(classname: String, jarname: String, dependencyjars: Option[List[String]])

case class MetadataApiArg(ObjectType: String, NameSpace: String, Name: String, Version: String, FormatType: String)

case class MetadataApiArgList(ArgList: List[MetadataApiArg])

// The implementation class
object JsonSerializer {

  val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  @throws(classOf[Json4sParsingException])
  @throws(classOf[FunctionListParsingException])
  def parseFunctionList(funcListJson: String, formatType: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Array[FunctionDef] = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(funcListJson)

      logger.debug("Parsed the json : " + funcListJson)
      val funcList = json.extract[FunctionList]
      var funcDefList: ArrayBuffer[FunctionDef] = ArrayBuffer[FunctionDef]()

      funcList.Functions.map(fn => {
        try {
          val argList = fn.Arguments.map(arg => (arg.ArgName, arg.ArgTypeNameSpace, arg.ArgTypeName))
          var featureSet: scala.collection.mutable.Set[FcnMacroAttr.Feature] = scala.collection.mutable.Set[FcnMacroAttr.Feature]()
          if (fn.Features != null) {
            fn.Features.foreach(arg => featureSet += FcnMacroAttr.fromString(arg))
          }
          val func = MdMgr.GetMdMgr.MakeFunc(fn.NameSpace, fn.Name, fn.PhysicalName,
            (fn.ReturnTypeNameSpace, fn.ReturnTypeName),
            argList, featureSet, ownerId, tenantId, uniqueId, mdElementId,
            fn.Version.toLong,
            fn.JarName,
            fn.DependantJars.toArray)
          funcDefList += func
        } catch {
          case e: AlreadyExistsException => {
            val funcDef = List(fn.NameSpace, fn.Name, fn.Version)
            val funcName = funcDef.mkString(",")
            logger.error("Failed to add the func: " + funcName, e)
          }
        }
      })
      funcDefList.toArray
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.error("", e)
        throw FunctionListParsingException(e.getMessage(), e)
      }
    }
  }

  def processTypeDef(typ: TypeDef): BaseTypeDef = {
    var typeDef: BaseTypeDef = null
    try {
      typ.MetadataType match {
        case "ScalarTypeDef" => {
          typeDef = MdMgr.GetMdMgr.MakeScalar(typ.NameSpace, typ.Name, ObjType.fromString(typ.TypeName),
            typ.PhysicalName, typ.OwnerId, typ.TenantId, typ.UniqueId, typ.MdElementId, typ.Version.toLong, typ.JarName,
            typ.DependencyJars.toArray, typ.Implementation)
        }
        case "ArrayTypeDef" => {
          typeDef = MdMgr.GetMdMgr.MakeArray(typ.NameSpace, typ.Name, typ.TypeNameSpace,
            typ.TypeName, typ.NumberOfDimensions.get, typ.OwnerId, typ.TenantId, typ.UniqueId, typ.MdElementId,
            typ.Version.toLong)
        }
        case "MapTypeDef" => {
          val mapKeyType = (typ.KeyTypeNameSpace.get, typ.KeyTypeName.get)
          val mapValueType = (typ.ValueTypeNameSpace.get, typ.ValueTypeName.get)
          typeDef = MdMgr.GetMdMgr.MakeMap(typ.NameSpace, typ.Name, mapValueType._1, mapValueType._2, typ.Version.toLong, typ.OwnerId, typ.TenantId, typ.UniqueId, typ.MdElementId)
        }
        case "ContainerTypeDef" => {
          if (typ.TypeName == "Struct") {
            typeDef = MdMgr.GetMdMgr.MakeStructDef(typ.NameSpace, typ.Name, typ.PhysicalName,
              null, typ.Version.toLong, typ.JarName,
              typ.DependencyJars.toArray, null, null, null, typ.OwnerId, typ.TenantId, typ.UniqueId, typ.MdElementId, 0, "", false) //BUGBUG:: Handle Primary Key, Foreign Keys & Partition Key here and also SchemaId, AvroSchema
          }
        }
        case _ => {
          throw TypeDefProcessingException("Internal Error: Unknown Type " + typ.MetadataType, null)
        }
      }
      typeDef

    } catch {
      case e: AlreadyExistsException => {
        val keyValues = List(typ.NameSpace, typ.Name, typ.Version)
        val typeName = keyValues.mkString(",")
        logger.error("Failed to add the type: " + typeName, e)
        throw AlreadyExistsException(e.getMessage(), e)
      }
      case e: Exception => {
        val keyValues = List(typ.NameSpace, typ.Name, typ.Version)
        val typeName = keyValues.mkString(",")
        logger.error("Failed to add the type: " + typeName, e)
        throw TypeDefProcessingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[TypeDefListParsingException])
  def parseTypeList(typeListJson: String, formatType: String): Array[BaseTypeDef] = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(typeListJson)

      logger.debug("Parsed the json : " + typeListJson)
      val typeList = json.extract[TypeDefList]

      logger.debug("Type count  => " + typeList.Types.length)
      var typeDefList: ArrayBuffer[BaseTypeDef] = ArrayBuffer[BaseTypeDef]()

      typeList.Types.map(typ => {

        try {
          val typeDefObj: BaseTypeDef = processTypeDef(typ)
          typeDefList += typeDefObj
        } catch {
          case e: AlreadyExistsException => {
            val keyValues = List(typ.NameSpace, typ.Name, typ.Version)
            val typeName = keyValues.mkString(",")
            logger.error("Failed to add the type: " + typeName, e)
          }
          case e: TypeDefProcessingException => {
            val keyValues = List(typ.NameSpace, typ.Name, typ.Version)
            val typeName = keyValues.mkString(",")
            logger.error("Failed to add the type: " + typeName, e)
          }
        }
      })
      typeDefList.toArray
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeDefListParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ConceptListParsingException])
  def parseConceptList(conceptsStr: String, formatType: String): Array[BaseAttributeDef] = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(conceptsStr)
      val conceptList = json.extract[ConceptList]

      //logger.debug("Parsed the json str " + conceptsStr)
      val attrDefList = new Array[BaseAttributeDef](conceptList.Concepts.length)
      var i = 0;
      conceptList.Concepts.map(o => {
        try {
          //logger.debug("Create Concept for " + o.NameSpace + "." + o.Name)
          val attr = MdMgr.GetMdMgr.MakeConcept(o.NameSpace,
            o.Name,
            o.TypeNameSpace,
            o.TypeName,
            o.OwnerId, o.TenantId, o.UniqueId, o.MdElementId,
            o.Version.toLong,
            false)
          logger.debug("Created AttributeDef for " + o.NameSpace + "." + o.Name)
          attrDefList(i) = attr
          i = i + 1
        } catch {
          case e: AlreadyExistsException => {
            val keyValues = List(o.NameSpace, o.Name, o.Version)
            val fullName = keyValues.mkString(",")
            logger.error("Failed to add the Concept: " + fullName, e)
          }
        }
      })
      //logger.debug("Found " + attrDefList.length + " concepts ")
      attrDefList
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ConceptListParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ZkTransactionParsingException])
  def parseZkTransaction(zkTransactionJson: String, formatType: String): ZooKeeperTransaction = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(zkTransactionJson)

      logger.debug("Parsed the json : " + zkTransactionJson)

      val zkTransaction = json.extract[ZooKeeperTransaction]

      logger.debug("Serialized ZKTransaction => " + zkSerializeObjectToJson(zkTransaction))

      zkTransaction
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ZkTransactionParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ConceptListParsingException])
  def parseDerivedConcept(conceptsStr: String, formatType: String) = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(conceptsStr)
      val concept = json.extract[DerivedConcept]
      val attrList = concept.Attributes.map(attr => (attr.NameSpace, attr.Name, attr.Type.TypeNameSpace, attr.Type.TypeName, false, attr.CollectionType.get))
      val argList = concept.FunctionDefinition.Arguments.map(arg => (arg.ArgName, arg.ArgTypeNameSpace, arg.ArgTypeName))
      var featureSet: scala.collection.mutable.Set[FcnMacroAttr.Feature] = scala.collection.mutable.Set[FcnMacroAttr.Feature]()
      concept.FunctionDefinition.Features.foreach(arg => featureSet += FcnMacroAttr.fromString(arg))

      val ownerId = "" //FIXME: Yet to fix this
      val uniqueId = 0
      val mdElementId = 0
      val tenantId = ""

      val func = MdMgr.GetMdMgr.MakeFunc(concept.FunctionDefinition.NameSpace,
        concept.FunctionDefinition.Name,
        concept.FunctionDefinition.PhysicalName,
        (concept.FunctionDefinition.ReturnTypeNameSpace,
          concept.FunctionDefinition.ReturnTypeName),
        argList,
        featureSet, ownerId, tenantId, uniqueId, mdElementId,
        concept.FunctionDefinition.Version.toLong,
        concept.FunctionDefinition.JarName,
        concept.FunctionDefinition.DependantJars.toArray)

      //val derivedConcept = MdMgr.GetMdMgr.MakeDerivedAttr(func,attrList)
    } catch {
      case e: AlreadyExistsException => {
        logger.error("Failed to add the DerivedConcept", e)
      }
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ConceptListParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ContainerDefParsingException])
  def parseContainerDef(contDefJson: String, formatType: String): ContainerDef = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(contDefJson)

      logger.debug("Parsed the json : " + contDefJson)

      val ContDefInst = json.extract[ContainerDefinition]
      val attrList = ContDefInst.Container.Attributes.map(attr => (attr.NameSpace, attr.Name, attr.Type.TypeNameSpace, attr.Type.TypeName, false, attr.CollectionType.get))
      val contDef = MdMgr.GetMdMgr.MakeFixedContainer(ContDefInst.Container.NameSpace,
        ContDefInst.Container.Name,
        ContDefInst.Container.PhysicalName,
        attrList.toList,
        ContDefInst.Container.OwnerId, ContDefInst.Container.TenantId, ContDefInst.Container.UniqueId, ContDefInst.Container.MdElementId, ContDefInst.Container.SchemaId, ContDefInst.Container.AvroSchema,
        ContDefInst.Container.Version.toLong,
        ContDefInst.Container.JarName,
        ContDefInst.Container.DependencyJars.toArray, null, null, null, false, false)
      contDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ContainerDefParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[TypeParsingException])
  def parseType(typeJson: String, formatType: String): BaseTypeDef = {
    var typeDef: BaseTypeDef = null
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(typeJson)
      logger.debug("Parsed the json : " + typeJson)
      val typ = json.extract[TypeDef]
      typeDef = processTypeDef(typ)
      typeDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: AlreadyExistsException => {
        logger.error("Failed to add the type, json => " + typeJson, e)
        throw AlreadyExistsException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw TypeParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ConceptParsingException])
  def parseConcept(conceptJson: String, formatType: String): BaseAttributeDef = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(conceptJson)

      logger.debug("Parsed the json : " + conceptJson)

      val conceptInst = json.extract[Concept]
      val concept = MdMgr.GetMdMgr.MakeConcept(conceptInst.NameSpace,
        conceptInst.Name,
        conceptInst.TypeNameSpace,
        conceptInst.TypeName,
        conceptInst.OwnerId, conceptInst.TenantId, conceptInst.UniqueId, conceptInst.MdElementId,
        conceptInst.Version.toLong,
        false)
      concept
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ConceptParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[FunctionParsingException])
  def parseFunction(functionJson: String, formatType: String): FunctionDef = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(functionJson)

      logger.debug("Parsed the json : " + functionJson)

      val functionInst = json.extract[Function]
      val argList = functionInst.Arguments.map(arg => (arg.ArgName, arg.ArgTypeNameSpace, arg.ArgTypeName))
      var featureSet: scala.collection.mutable.Set[FcnMacroAttr.Feature] = scala.collection.mutable.Set[FcnMacroAttr.Feature]()
      if (functionInst.Features != null) {
        functionInst.Features.foreach(arg => featureSet += FcnMacroAttr.fromString(arg))
      }

      val function = MdMgr.GetMdMgr.MakeFunc(functionInst.NameSpace,
        functionInst.Name,
        functionInst.PhysicalName,
        (functionInst.ReturnTypeNameSpace, functionInst.ReturnTypeName),
        argList,
        featureSet,
        functionInst.OwnerId, functionInst.TenantId, functionInst.UniqueId, functionInst.MdElementId,
        functionInst.Version.toLong,
        functionInst.JarName,
        functionInst.DependantJars.toArray)
      function
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw FunctionParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[MessageDefParsingException])
  def parseMessageDef(msgDefJson: String, formatType: String): MessageDef = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(msgDefJson)

      logger.debug("Parsed the json : " + msgDefJson)

      val MsgDefInst = json.extract[MessageDefinition]
      val attrList = MsgDefInst.Message.Attributes
      var attrList1 = List[(String, String, String, String, Boolean, String)]()
      for (attr <- attrList) {
        attrList1 ::=(attr.NameSpace, attr.Name, attr.Type.TypeNameSpace, attr.Type.TypeName, false, attr.CollectionType.get)
      }
      val msgDef = MdMgr.GetMdMgr.MakeFixedMsg(MsgDefInst.Message.NameSpace,
        MsgDefInst.Message.Name,
        MsgDefInst.Message.PhysicalName,
        attrList1.toList,
        MsgDefInst.Message.OwnerId, MsgDefInst.Message.TenantId, MsgDefInst.Message.UniqueId, MsgDefInst.Message.MdElementId, MsgDefInst.Message.SchemaId, MsgDefInst.Message.AvroSchema,
        MsgDefInst.Message.Version.toLong,
        MsgDefInst.Message.JarName,
        MsgDefInst.Message.DependencyJars.toArray, null, null, null, false, false)
      msgDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw MessageDefParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ModelDefParsingException])
  def parseModelDef(modDefJson: String, formatType: String): ModelDef = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(modDefJson)

      logger.debug("Parsed the json : " + modDefJson)

      val ModDefInst = json.extract[ModelDefinition]

/*
      val inputMsgsAndAttribs =
        if (ModDefInst.Model.InputMsgsInfo != null) {
          ModDefInst.Model.InputMsgsInfo.map(i => i.map(m => {
            val t = new MessageAndAttributes
            t.message = m.message
            t.attributes = if (m.attributes != null) m.attributes.toArray else Array[String]()
            t
          }).toArray).toArray
        } else {
          Array[Array[MessageAndAttributes]]()
        }
*/
      val inputMsgsAndAttribs = Array[Array[MessageAndAttributes]]()

      val outputMsgs =
        if (ModDefInst.Model.OutputMsgs != null) {
          ModDefInst.Model.OutputMsgs.toArray
        } else {
          Array[String]()
        }

      /**
        * Create a ModelDef from the extracted information found in the JSON string.
        *
        *
        * case class ModelInfo(NameSpace: String
        * , Name: String
        * , Version: String
        * , PhysicalName: String
        * , ModelRep : String
        * , ModelType: String
        * , IsReusable : Boolean
        * , MsgConsumed : String
        * , ObjectDefinition : String
        * , ObjectFormat : String
        * , JarName: String
        * , DependencyJars: List[String]
        * , InputAttributes: List[Attr]
        * , OutputAttributes: List[Attr]
        * , Recompile : Boolean
        * , SupportsInstanceSerialization : Boolean)
        */
      val modDef = MdMgr.GetMdMgr.MakeModelDef(ModDefInst.Model.NameSpace
        , ModDefInst.Model.Name
        , ModDefInst.Model.PhysicalName
        , ModDefInst.Model.OwnerId, ModDefInst.Model.TenantId, ModDefInst.Model.UniqueId, ModDefInst.Model.MdElementId
        , ModelRepresentation.modelRep(ModDefInst.Model.ModelRep)
        , inputMsgsAndAttribs
        , outputMsgs
        , ModDefInst.Model.IsReusable
        , ModDefInst.Model.ObjectDefinition
        , MiningModelType.modelType(ModDefInst.Model.ModelType)
        , ModDefInst.Model.Version.toLong
        , ModDefInst.Model.JarName
        , ModDefInst.Model.DependencyJars.toArray
        , ModDefInst.Model.Recompile
        , ModDefInst.Model.SupportsInstanceSerialization)

      modDef.ObjectDefinition(ModDefInst.Model.ObjectDefinition)
      val objFmt: ObjFormatType.FormatType = ObjFormatType.fromString(ModDefInst.Model.ObjectFormat)
      modDef.ObjectFormat(objFmt)

      modDef
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ModelDefParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[EngineConfigParsingException])
  def parseEngineConfig(configJson: String): Map[String, Any] = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(configJson)
      logger.debug("Parsed the json : " + configJson)

      val fullmap = json.values.asInstanceOf[Map[String, Any]]

      fullmap
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw EngineConfigParsingException(e.getMessage(), e)
      }
    }
  }

  @throws(classOf[Json4sParsingException])
  @throws(classOf[ApiArgListParsingException])
  def parseApiArgList(apiArgListJson: String): MetadataApiArgList = {
    try {
      implicit val jsonFormats: Formats = DefaultFormats
      val json = parse(apiArgListJson)
      logger.debug("Parsed the json : " + apiArgListJson)

      val cfg = json.extract[MetadataApiArgList]
      cfg
    } catch {
      case e: MappingException => {
        logger.debug("", e)
        throw Json4sParsingException(e.getMessage(), e)
      }
      case e: Exception => {
        logger.debug("", e)
        throw ApiArgListParsingException(e.getMessage(), e)
      }
    }
  }

  def zkSerializeObjectToJson(o: ZooKeeperTransaction): String = {
    try {
      val json = ("Notifications" -> o.Notifications.toList.map { n =>
        (
          ("ObjectType" -> n.ObjectType) ~
            ("Operation" -> n.Operation) ~
            ("NameSpace" -> n.NameSpace) ~
            ("Name" -> n.Name) ~
            ("Version" -> n.Version.toLong) ~
            ("PhysicalName" -> n.PhysicalName) ~
            ("JarName" -> n.JarName) ~
            ("DependantJars" -> n.DependantJars.toList))
      })
      pretty(render(json))
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw Json4sSerializationException(e.getMessage(), e)
      }
    }
  }

  def zkSerializeObjectToJson(mdObj: BaseElemDef, operation: String): String = {
    try {
      mdObj match {
        /**
          * Assuming that zookeeper transaction will be different based on type of object
          */
        case o: ModelDef => {
          val json = (("ObjectType" -> "ModelDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("ModelType" -> o.miningModelType.toString) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: MessageDef => {
          val json = (("ObjectType" -> "MessageDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: MappedMsgTypeDef => {
          val json = (("ObjectType" -> "MappedMsgTypeDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: StructTypeDef => {
          val json = (("ObjectType" -> "StructTypeDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: ContainerDef => {
          val json = (("ObjectType" -> "ContainerDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: FunctionDef => {
          val json = (("ObjectType" -> "FunctionDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: ArrayTypeDef => {
          val json = (("ObjectType" -> "ArrayTypeDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: MapTypeDef => {
          val json = (("ObjectType" -> "MapTypeDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: JarDef => {
          val json = (("ObjectType" -> "JarDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> o.ver) ~
            ("PhysicalName" -> o.physicalName) ~
            ("JarName" -> o.jarName) ~
            ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList))
          pretty(render(json))
        }
        case o: ConfigDef => {
          val json = (("ObjectType" -> "ConfigDef") ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.NameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> "0") ~
            ("PhysicalName" -> "") ~
            ("JarName" -> "") ~
            ("DependantJars" -> List[String]()) ~
            ("ConfigContnent" -> o.contents))
          pretty(render(json))
        }
        case o: ClusterConfigDef => {
          val json = (("ObjectType" ->  o.elementType) ~
            ("Operation" -> operation) ~
            ("NameSpace" -> o.NameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> "0") ~
            ("PhysicalName" -> "") ~
            ("JarName" -> "") ~
            ("DependantJars" -> List[String]()) ~
            ("ElementType" -> o.elementType) ~
            ("ClusterId" -> o.clusterId))
          pretty(render(json))
        }
        case _ => {
          throw UnsupportedObjectException("zkSerializeObjectToJson doesn't support the  objects of type objectType of " + mdObj.getClass().getName() + " yet.", null)
        }
      }
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw Json4sSerializationException(e.getMessage(), e)
      }
    }
  }

  def replaceLast(inStr: String, toReplace: String, replacement: String): String = {
    val pos = inStr.lastIndexOf(toReplace);
    if (pos > -1) {
      inStr.substring(0, pos) + replacement + inStr.substring(pos + toReplace.length(), inStr.length());
    } else {
      inStr;
    }
  }

  @throws(classOf[UnsupportedObjectException])
  def SerializeCfgObjectToJson(cfgObj: Object): String = {
    logger.debug("Generating Json for an object of type " + cfgObj.getClass().getName())
    cfgObj match {
      case o: ClusterInfo => {
        val json = (("ClusterId" -> o.clusterId))
        compact(render(json))
      }
      case o: ClusterCfgInfo => {
        val json = (("ClusterId" -> o.clusterId) ~
          ("CfgMap" -> o.cfgMap))
        compact(render(json))
      }
      case o: NodeInfo => {
        val json = (("NodeId" -> o.nodeId) ~
          ("NodePort" -> o.nodePort) ~
          ("NodeIpAddr" -> o.nodeIpAddr) ~
          ("JarPaths" -> o.jarPaths.toList) ~
          ("Scala_home" -> o.scala_home) ~
          ("Java_home" -> o.java_home) ~
          ("Roles" -> o.roles.toList) ~
          ("Classpath" -> o.classpath) ~
          ("ClusterId" -> o.clusterId))
        compact(render(json))
      }
      case o: AdapterInfo => {
        val json = (("Name" -> o.name) ~
          ("TypeString" -> o.typeString) ~
          ("ClassName" -> o.className) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.dependencyJars.toList) ~
          ("AdapterSpecificCfg" -> o.adapterSpecificCfg) ~
          ("TenantId" -> o.TenantId))
        compact(render(json))
      }
      case _ => {
        throw UnsupportedObjectException("SerializeCfgObjectToJson doesn't support the " +
          "objectType of " + cfgObj.getClass().getName() + " yet", null)
      }
    }
  }

  @throws(classOf[UnsupportedObjectException])
  def SerializeObjectToJson(mdObj: BaseElemDef): String = {
    mdObj match {
      case o: FunctionDef => {
        val json = (("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("ReturnTypeNameSpace" -> o.retType.nameSpace) ~
          ("ReturnTypeName" -> o.retType.name) ~
          ("Arguments" -> o.args.toList.map { arg =>
            (
              ("ArgName" -> arg.name) ~
                ("ArgTypeNameSpace" -> arg.Type.nameSpace) ~
                ("ArgTypeName" -> arg.Type.name))
          }) ~
          ("Features" -> o.features.map(_.toString).toList) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependantJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }

      case o: MessageDef => {
        var primaryKeys = List[(String, List[String])]()
        var foreignKeys = List[(String, List[String], String, List[String])]()
        if (o.cType.Keys != null && o.cType.Keys.size != 0) {
          o.cType.Keys.foreach(m => {
            if (m.KeyType == RelationKeyType.tPrimary) {
              val pr = m.asInstanceOf[PrimaryKey]
              primaryKeys ::=(pr.constraintName, pr.key.toList)
            } else {
              val fr = m.asInstanceOf[ForeignKey]
              foreignKeys ::=(fr.constraintName, fr.key.toList, fr.forignContainerName, fr.forignKey.toList)
            }
          })
        }

        val containerDef = o.cType.asInstanceOf[ContainerTypeDef]

        val attribs =
          if (containerDef.IsFixed) {
            containerDef.asInstanceOf[StructTypeDef].memberDefs.toList
          } else {
            containerDef.asInstanceOf[MappedMsgTypeDef].attrMap.map(kv => kv._2).toList
          }

        // Assume o.containerType is checked for not being null
        val json = ("Message" ->
          ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("FullName" -> o.FullName) ~
            ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("ReportingId"-> o.uniqueId) ~
            ("SchemaId" -> o.containerType.schemaId) ~
            ("AvroSchema" -> o.containerType.avroSchema) ~
            ("JarName" -> o.jarName) ~
            ("PhysicalName" -> o.typeString) ~
            ("ObjectDefinition" -> o.objectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.objectFormat)) ~
            ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
            ("MsgAttributes" -> attribs.map(a =>
              ("NameSpace" -> a.NameSpace) ~
                ("Name" -> a.Name) ~
                ("TypNameSpace" -> a.typeDef.NameSpace) ~
                ("TypName" -> a.typeDef.Name) ~
                ("Version" -> a.Version) ~
                ("CollectionType" -> ObjType.asString(a.CollectionType)))) ~
            ("PrimaryKeys" -> primaryKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2))) ~
            ("ForeignKeys" -> foreignKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2) ~ ("forignContainerName" -> m._3) ~ ("forignKey" -> m._4))) ~
            ("TransactionId" -> o.tranId))
        compact(render(json))
      }

      case o: ContainerDef => {
        var primaryKeys = List[(String, List[String])]()
        var foreignKeys = List[(String, List[String], String, List[String])]()
        if (o.cType.Keys != null && o.cType.Keys.size != 0) {
          o.cType.Keys.foreach(m => {
            if (m.KeyType == RelationKeyType.tPrimary) {
              val pr = m.asInstanceOf[PrimaryKey]
              primaryKeys ::=(pr.constraintName, pr.key.toList)
            } else {
              val fr = m.asInstanceOf[ForeignKey]
              foreignKeys ::=(fr.constraintName, fr.key.toList, fr.forignContainerName, fr.forignKey.toList)
            }
          })
        }

        val containerDef = o.cType.asInstanceOf[ContainerTypeDef]

        val attribs =
          if (containerDef.IsFixed) {
            containerDef.asInstanceOf[StructTypeDef].memberDefs.toList
          } else {
            containerDef.asInstanceOf[MappedMsgTypeDef].attrMap.map(kv => kv._2).toList
          }

        val json = ("Container" ->
          ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("FullName" -> o.FullName) ~
            ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
            ("SchemaId" -> o.containerType.schemaId) ~
            ("AvroSchema" -> o.containerType.avroSchema) ~
            ("JarName" -> o.jarName) ~
            ("PhysicalName" -> o.typeString) ~
            ("ObjectDefinition" -> o.objectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.objectFormat)) ~
            ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
            ("MsgAttributes" -> attribs.map(a =>
              ("NameSpace" -> a.NameSpace) ~
                ("Name" -> a.Name) ~
                ("TypNameSpace" -> a.typeDef.NameSpace) ~
                ("TypName" -> a.typeDef.Name) ~
                ("Version" -> a.Version) ~
                ("CollectionType" -> ObjType.asString(a.CollectionType)))) ~
            ("PrimaryKeys" -> primaryKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2))) ~
            ("ForeignKeys" -> foreignKeys.map(m => ("constraintName" -> m._1) ~ ("key" -> m._2) ~ ("forignContainerName" -> m._3) ~ ("forignKey" -> m._4))) ~
            ("TransactionId" -> o.tranId))
        compact(render(json))
      }

      case o: ModelDef => {
        val outputMsgs =
          if (o.outputMsgs != null) {
            o.outputMsgs.toList
          } else {
            List[String]()
          }

        val json = ("Model" ->
          ("NameSpace" -> o.nameSpace) ~
            ("Name" -> o.name) ~
            ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
            ("IsReusable" -> o.isReusable.toString) ~
            ("inputMsgSets" -> o.inputMsgSets.toList.map(m => m.toList.map(f => ("Origin" -> f.origin) ~ ("Message" -> f.message) ~ ("Attributes" -> f.attributes.toList)))) ~
            ("OutputMsgs" -> outputMsgs) ~
            ("ModelRep" -> o.modelRepresentation.toString) ~
            ("ModelType" -> o.miningModelType.toString) ~
            ("JarName" -> o.jarName) ~
            ("PhysicalName" -> o.typeString) ~
            ("ObjectDefinition" -> o.objectDefinition) ~
            ("ObjectFormat" -> ObjFormatType.asString(o.objectFormat)) ~
            ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
            ("Deleted" -> o.deleted) ~
            ("Active" -> o.active) ~
            ("TransactionId" -> o.tranId))
        compact(render(json))
      }

      case o: AttributeDef => {
        val json = (("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("CollectionType" -> ObjType.asString(o.collectionType)) ~
          ("TransactionId" -> o.tranId))
        var jsonStr = pretty(render(json))
        //jsonStr = jsonStr.replaceAll("}","").trim + ",\n  \"Type\": "
        jsonStr = replaceLast(jsonStr, "}", "").trim + ",\n  \"Type\": "
        var memberDefJson = SerializeObjectToJson(o.typeDef)
        memberDefJson = memberDefJson + "}"
        jsonStr += memberDefJson
        jsonStr
      }
      case o: ScalarTypeDef => {
        val json = (("MetadataType" -> "ScalarTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
          ("TypeNameSpace" -> MdMgr.sysNS) ~
          ("TypeName" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case o: MapTypeDef => {
        val json = (("MetadataType" -> "MapTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
          ("TypeNameSpace" -> o.nameSpace) ~
          ("TypeName" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("Fixed" -> o.IsFixed) ~
          ("ValueTypeNameSpace" -> o.valDef.nameSpace) ~
          ("ValueTypeName" -> ObjType.asString(o.valDef.tType)) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case o: ArrayTypeDef => {
        val json = (("MetadataType" -> "ArrayTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.elemDef.tTypeType)) ~
          ("TypeNameSpace" -> o.elemDef.nameSpace) ~
          ("TypeName" -> ObjType.asString(o.elemDef.tType)) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("NumberOfDimensions" -> o.arrayDims) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case o: StructTypeDef => {
        val json = (("MetadataType" -> "StructTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
          ("TypeNameSpace" -> MdMgr.sysNS) ~
          ("TypeName" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case o: MappedMsgTypeDef => {
        val json = (("MetadataType" -> "MappedMsgTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
          ("TypeNameSpace" -> MdMgr.sysNS) ~
          ("TypeName" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case o: AnyTypeDef => {
        val json = (("MetadataType" -> "AnyTypeDef") ~
          ("NameSpace" -> o.nameSpace) ~
          ("Name" -> o.name) ~
          ("TypeTypeName" -> ObjTypeType.asString(o.tTypeType)) ~
          ("TypeNameSpace" -> o.nameSpace) ~
          ("TypeName" -> o.name) ~
          ("PhysicalName" -> o.physicalName) ~
          ("Version" -> MdMgr.Pad0s2Version(o.ver)) ~
          ("JarName" -> o.jarName) ~
          ("DependencyJars" -> o.CheckAndGetDependencyJarNames.toList) ~
          ("Implementation" -> o.implementationName) ~
          ("TransactionId" -> o.tranId))
        compact(render(json))
      }
      case _ => {
        throw UnsupportedObjectException(s"SerializeObjectToJson doesn't support the objectType of " + mdObj.getClass().getName() + "  yet", null)
      }
    }
  }

  def SerializeObjectListToJson[T <: BaseElemDef](objList: Array[T]): String = {
    var json = "[ \n"
    objList.toList.map(obj => {
      var objJson = SerializeObjectToJson(obj); json += objJson; json += ",\n"
    })
    json = json.stripSuffix(",\n")
    json += " ]\n"
    json
  }

  def SerializeCfgObjectListToJson[T <: Object](objList: Array[T]): String = {
    var json = "[ \n"
    objList.toList.map(obj => {
      var objJson = SerializeCfgObjectToJson(obj); json += objJson; json += ",\n"
    })
    json = json.stripSuffix(",\n")
    json += " ]\n"
    json
  }

  def SerializeObjectListToJson[T <: BaseElemDef](objType: String, objList: Array[T]): String = {
    var json = "{\n" + "\"" + objType + "\" :" + SerializeObjectListToJson(objList) + "\n}"
    json
  }

  def SerializeCfgObjectListToJson[T <: Object](objType: String, objList: Array[T]): String = {
    var json = "{\n" + "\"" + objType + "\" :" + SerializeCfgObjectListToJson(objList) + "\n}"
    json
  }

  def zkSerializeObjectListToJson[T <: BaseElemDef](objList: Array[T], operations: Array[String]): String = {
    var json = "[ \n"
    var i = 0
    objList.toList.map(obj => {
      var objJson = zkSerializeObjectToJson(obj, operations(i)); i = i + 1; json += objJson; json += ",\n"
    })
    json = json.stripSuffix(",\n")
    json += " ]\n"
    json
  }

  def zkSerializeObjectListToJson[T <: BaseElemDef](objType: String, objList: Array[T], operations: Array[String]): String = {
    // Insert the highest Transaction ID into the JSON Notification message.
    var max: Long = 0
    objList.foreach(obj => {
      max = scala.math.max(obj.TranId, max)
    })

    var json = "{\n" + "\"transactionId\":\"" + max + "\",\n" + "\"" + objType + "\" :" + zkSerializeObjectListToJson(objList, operations) + "\n}"
    json
  }

  def zkSerializeConfigToJson[T <: Map[String,Any]](tid: Long, objType: String, config: T, operations: Array[String]): String = {
    val json = "{\n" + "\"transactionId\":\"" + tid + "\",\n" + "\"" + objType + "\" :" + SerializeMapToJsonString(config) + "\n}"
    json
  }

  def SerializeApiArgListToJson(o: MetadataApiArgList): String = {
    try {
      val json = ("ArgList" -> o.ArgList.map { n =>
        (
          ("ObjectType" -> n.ObjectType) ~
            ("NameSpace" -> n.NameSpace) ~
            ("Name" -> n.Name) ~
            ("Version" -> n.Version) ~
            ("FormatType" -> n.FormatType))
      })
      pretty(render(json))
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw Json4sSerializationException(e.getMessage(), e)
      }
    }
  }

  def SerializeAuditRecordsToJson(ar: Array[AuditRecord]): String = {
    try {
      val json = (ar.toList.map { a => a.toJson })
      pretty(render(json))
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw Json4sSerializationException(e.getMessage(), e)
      }
    }
  }

  def SerializeMapToJsonString(map: Map[String, Any]): String = {
    implicit val formats = org.json4s.DefaultFormats
    Serialization.write(map)
  }

    /**
      * Translate the supplied json string (typically a flattened rep of either a List[Map[String, Any]] or
      * scala.collection.immutable.Map[String,Any]).  The caller should know what it is (e.g., by looking at the
      * leading character for '[' for list or the '{' for map).
      *
      * @param jsonStr a string rep of a json map or list
      * @return Any (actually either a Map[String,Any] or List[Map[String, Any]]
      */

    @throws(classOf[com.ligadata.Exceptions.Json4sParsingException])
    @throws(classOf[com.ligadata.Exceptions.InvalidArgumentException])
    def jsonStringAsColl(jsonStr: String): Any = {
        val jsonObjs : Any = try {
            implicit val jsonFormats: Formats = DefaultFormats
            val json = parse(jsonStr)
            logger.debug("Parsed the json : " + jsonStr)
            json.values
        } catch {
            case e: MappingException => {
                logger.debug("", e)
                throw Json4sParsingException(e.getMessage, e)
            }
            case e: Exception => {
                logger.debug("", e)
                throw InvalidArgumentException(e.getMessage, e)
            }
        }
        jsonObjs
    }



    def SerializeModelConfigToJson(mcfgKey: String, config: scala.collection.immutable.Map[String, Any]): String = {
    var jsonStr = ""
    try{
      jsonStr = jsonStr + SerializeMapToJsonString(Map(mcfgKey -> config))
      jsonStr
    } catch {
      case e: Exception => {
        logger.debug("", e)
        throw Json4sSerializationException(e.getMessage(), e)
      }
    }
  }
}
