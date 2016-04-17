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

package com.ligadata.kamanja.metadata

import com.ligadata.kamanja.metadata.MiningModelType.MiningModelType
import com.ligadata.kamanja.metadata.ModelRepresentation.ModelRepresentation

import scala.Enumeration
import scala.collection.immutable.List
import scala.collection.mutable.{Map, HashMap, MultiMap, Set, SortedSet, ArrayBuffer}
import scala.io.Source._
import scala.util.control.Breaks._
import com.ligadata.Exceptions._

import java.util._
import java.lang.RuntimeException
import java.util.NoSuchElementException

import org.apache.logging.log4j._

import ObjTypeType._
import ObjType._

/**
  * class MdMgr
  *
  * Accessor functions are provided to access various metadata entities.  These entities include:
  *
  * MessageDef, TypeDef, FunctionDef, ConceptDef/AttributeDef, DerivedConceptDef/ComputedAttributeDef, ModelDef
  *
  * Each metadata entity could be uniquely identified by a key (which includes version number).
  * Each metadata entity could be stored persistently in the underlying persistent store (either key-val or relational schema)
  *
  * There is one container for each type of metadata entity with the exception of FunctionDefs which has two representations:
  * funcDefs uses a string key consisting in the form "fcnNmSpc.functionName(argNmSpc.argType, argNmSpc.argType,...)"
  * funcDefSets uses a simple name key in the form "fcnNmSpc:functionName"
  * The PmmlCompiler is the principal user of the funcDefs, requiring the necessary disambiguation of types with the same base name
  * via the argument qualification.  The funcDefSets will aid the authoring tools to discover what variations of a given named
  * function are cataloged in the metadata.
  *
  */

class MdMgr {

  /** initialize a logger */
  val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  /** maps that hold caches of the metadata */
  /** making every thing is multi map, because we will have multiple versions for the same one */
  private var typeDefs = new HashMap[String, Set[BaseTypeDef]] with MultiMap[String, BaseTypeDef]
  private var funcDefs = new HashMap[String, Set[FunctionDef]] with MultiMap[String, FunctionDef]
  private var msgDefs = new HashMap[String, Set[MessageDef]] with MultiMap[String, MessageDef]
  private var containerDefs = new HashMap[String, Set[ContainerDef]] with MultiMap[String, ContainerDef]
  private var attrbDefs = new HashMap[String, Set[BaseAttributeDef]] with MultiMap[String, BaseAttributeDef]
  private var modelDefs = new HashMap[String, Set[ModelDef]] with MultiMap[String, ModelDef]
  private var factoryOfMdlInstFactories = new HashMap[String, Set[FactoryOfModelInstanceFactoryDef]] with MultiMap[String, FactoryOfModelInstanceFactoryDef]
  private var schemaIdMap = new HashMap[Int, ContainerDef] // For now we consider ContainerDef & MessageDef
  private var schemaIdToElemntIdMap = new HashMap[Int, Long] // For now we consider ContainerDef & MessageDef
  private var elementIdMap = new HashMap[Long, BaseElem] // For now we consider ContainerDef, MessageDef & ModelDef

  // FunctionDefs keyed by function signature nmspc.name(argtyp1,argtyp2,...) map 
  private var compilerFuncDefs = scala.collection.mutable.Map[String, FunctionDef]()
  // Function style macros used by the Pmml Compiler to support code generation. macroDef key is typesig. 
  private var macroDefs = scala.collection.mutable.Map[String, MacroDef]()
  private var macroDefSets = new HashMap[String, Set[MacroDef]] with MultiMap[String, MacroDef]

  // Config objects
  private var clusterCfgs = new HashMap[String, ClusterCfgInfo]
  private var clusters = new HashMap[String, ClusterInfo]
  private var nodes = new HashMap[String, NodeInfo]
  private var adapters = new HashMap[String, AdapterInfo]
  private var modelConfigs = new HashMap[String, scala.collection.immutable.Map[String, Any]]
  private var configurations = new HashMap[String, UserPropertiesInfo]
  private var msgdefSystemCols = List("transactionid", "timepartitiondata", "rownumber")
  private var serializers = new HashMap[String, SerializeDeserializeConfig]
  private var adapterMessageBindings = new HashMap[String, AdapterMessageBinding]

  private var tenantIdMap = new HashMap[String, TenantInfo]

  private var propertyChanged: scala.collection.mutable.ArrayBuffer[(String,Any)] = scala.collection.mutable.ArrayBuffer[(String,Any)]()
  private val lock: Object = new Object

  def truncate {
    typeDefs.clear
    funcDefs.clear
    compilerFuncDefs.clear
    msgDefs.clear
    containerDefs.clear
    attrbDefs.clear
    modelDefs.clear
    compilerFuncDefs.clear
    macroDefs.clear
    macroDefSets.clear
    clusters.clear
    clusterCfgs.clear
    nodes.clear
    adapters.clear
    modelConfigs.clear
    schemaIdMap.clear
    schemaIdToElemntIdMap.clear
    elementIdMap.clear
    tenantIdMap.clear
    factoryOfMdlInstFactories.clear
    serializers.clear
    adapterMessageBindings.clear
  }

  def truncate(objectType: String) {
    objectType match {
      case "TypeDef" => {
        typeDefs.clear
      }
      case "FunctionDef" => {
        funcDefs.clear
        compilerFuncDefs.clear
      }
      case "MessageDef" => {
        msgDefs.clear
      }
      case "ContainerDef" => {
        containerDefs.clear
      }
      case "ConceptDef" => {
        attrbDefs.clear
      }
      case "ModelDef" => {
        modelDefs.clear
      }
      case "FactoryOfMdlInstFactories" => {
        factoryOfMdlInstFactories.clear
      }
      case "CompilerFuncDef" => {
        compilerFuncDefs.clear
      }
      case "MacroDef" => {
        macroDefs.clear
      }
      case "MacroDefSets" => {
        macroDefSets.clear
      }
      case "Clusters" => {
        clusters.clear
        clusterCfgs.clear
      }
      case "Nodes" => {
        nodes.clear
      }
      case "Adapters" => {
        adapters.clear
      }
      case "ModelConfigs" => {
        modelConfigs.clear
      }
      case "SchemaId" => {
        schemaIdMap.clear
      }
      case "SchemaIdElementId" => {
        schemaIdToElemntIdMap.clear
      }
      case "ElementId" => {
        elementIdMap.clear
      }
      case "Tenants" => {
        tenantIdMap.clear
      }
      case _ => {
        logger.error("Unknown object type " + objectType + " in truncate function")
      }
    }
  }

  def dump {
    typeDefs.foreach(obj => {
      logger.debug("Type Key = " + obj._1)
    })
    funcDefs.foreach(obj => {
      logger.debug("Function Key = " + obj._1)
    })
    msgDefs.foreach(obj => {
      logger.debug("Message Key = " + obj._1)
    })
    containerDefs.foreach(obj => {
      logger.debug("Container Key = " + obj._1)
    })
    attrbDefs.foreach(obj => {
      logger.debug("Attribute Key = " + obj._1)
    })
    modelDefs.foreach(obj => {
      logger.debug("Model Key = " + obj._1)
    })
    compilerFuncDefs.foreach(obj => {
      logger.debug("CompilerFunction Key = " + obj._1)
    })
    macroDefs.foreach(obj => {
      logger.debug("Macro Key = " + obj._1)
    })
    macroDefSets.foreach(obj => {
      logger.debug("MacroSet Key = " + obj._1)
    })
    clusters.foreach(obj => {
      logger.debug("MacroSet Key = " + obj._1)
    })
    clusterCfgs.foreach(obj => {
      logger.debug("MacroSet Key = " + obj._1)
    })
    nodes.foreach(obj => {
      logger.debug("MacroSet Key = " + obj._1)
    })
    adapters.foreach(obj => {
      logger.debug("MacroSet Key = " + obj._1)
    })
    factoryOfMdlInstFactories.foreach(obj => {
      logger.trace("factoryOfMdlInstFactoryDef Key = " + obj._1)
    })
    schemaIdMap.foreach(obj => {
      logger.trace("SchemaId:%d => Container:%s Version:%d".format(obj._1, obj._2.FullName, obj._2.Version))
    })
    schemaIdToElemntIdMap.foreach(obj => {
      logger.trace("SchemaId:%d => ElementId:%d".format(obj._1, obj._2))
    })
    elementIdMap.foreach(obj => {
      logger.trace("ElementId:%d => ElementName:%s Version:%d, ElementType:%s".format(obj._1, obj._2.FullName, obj._2.Version, obj._2.MdElementCategory))
    })
    tenantIdMap.foreach(obj => {
      logger.trace("TenantId:" + obj._1)
    })
  }

  private def GetExactVersion[T <: BaseElemDef](elems: Option[scala.collection.immutable.Set[T]], ver: Long): Option[T] = {
    // get exact match
    elems match {
      case None => None
      case Some(es) => {
        var elm: Option[T] = None
        try {
          breakable {
            // We can use this, but it will loop thru all elements 
            // elems.filter(e => e.Version == ver)
            es.foreach(e => {
              if (!e.IsDeleted) {
                if (e.Version == ver) {
                  elm = Some(e)
                  break
                }
              }
            })
          }
        } catch {
          case e: Exception => {
            logger.debug("", e)
          }
        }
        elm
      }
    }
  }

  private def GetLatestVersion[T <: BaseElemDef](elems: Option[scala.collection.immutable.Set[T]]): Option[T] = {
    // get latest one
    elems match {
      case None => None
      case Some(es) => {
        var elm: Option[T] = None
        var maxVer: Long = -1
        try {
          breakable {
            es.foreach(e => {
              // At this point, we only want to consider objects that are not deleted.  Deactivated are ok.
              if (!e.IsDeleted) {
                if (elm == None || maxVer < e.Version) {
                  elm = Some(e)
                  maxVer = e.Version
                }
              }
            })
          }
        } catch {
          case e: Exception => {
            logger.debug("", e)
          }
        }
        elm
      }
    }
  }

  /** Get Matched Value */
  private def GetReqValue[T <: BaseElemDef](elems: Option[scala.collection.immutable.Set[T]], ver: Long): Option[T] = {
    if (ver <= 0) GetLatestVersion(elems) else GetExactVersion(elems, ver)
  }

  /** Get Immutable Set from Mutable Set */
  private def GetImmutableSet[T <: BaseElemDef](elems: Option[scala.collection.mutable.Iterable[T]], onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[T]] = {
    if (latestVersion == false) {
      elems match {
        case None => None
        case Some(es) => {
          if (onlyActive) Some(es.filter(e => {
            e.IsActive && !e.IsDeleted
          }).toSet)
          else Some(es.filter(e => !e.IsDeleted).toSet)
        }
      }
    } else {
      val latestElems =
      // get latest ones
        elems match {
          case None => None
          case Some(es) => {
            var newElems = new scala.collection.mutable.ArrayBuffer[T]()
            var newBaseElemsIdxs = scala.collection.mutable.Map[String, Int]()
            es.foreach(e => {
              if (!e.IsDeleted &&
                (onlyActive == false || (onlyActive && e.IsActive))) {
                val fnm = if (e.isInstanceOf[FunctionDef]) e.asInstanceOf[FunctionDef].typeString else e.FullName
                val existingIdx = newBaseElemsIdxs.getOrElse(fnm, -1)
                if (existingIdx < 0) {
                  newBaseElemsIdxs(fnm) = newElems.size
                  newElems += e
                } else if (newElems(existingIdx).Version < e.Version) {
                  newElems(existingIdx) = e
                }
              }
            })
            Some(newElems.toSet)
          }
        }
      latestElems
    }
  }

  private def GetElem[T <: BaseElemDef](elm: Option[T], noSuchElemErr: String): T = {
    elm match {
      case None => throw new NoSuchElementException(noSuchElemErr)
      case Some(e) => elm.get
    }
  }

  /**
    * Fill in the BaseElement info common to all types.
    *
    * @param be        - the BaseElemDef
    * @param nameSpace - the scalar type namespace
    * @param name      - the scalar type name.
    * @param ver       - version info
    * @param jarNm     -
    * @param depJars   -
    */

  private def SetBaseElem(be: BaseElemDef, nameSpace: String, name: String, ver: Long, jarNm: String, depJars: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
    if (msgdefSystemCols.contains(name.trim.toLowerCase)) {
      be.name = name.trim
    } else {
      be.name = name.trim.toLowerCase
    }
    be.nameSpace = nameSpace.trim.toLowerCase
    be.ver = ver
    // be.uniqueId = MdIdSeq.next
    be.creationTime = System.currentTimeMillis // Taking current local time. May be we need to get GMT time
    be.modTime = be.creationTime
    be.jarName = jarNm
    be.dependencyJarNames = depJars
    be.ownerId = ownerId
    be.tenantId = tenantId
    be.uniqueId = uniqueId
    be.mdElementId = mdElementId
  }

  /**
    * Construct an AttributeDef from the supplied arguments.
    *
    * @param nameSpace    - the namespace in which this structure type
    * @param name         - the name of the structure type.
    * @param typeNameNs   - the namespace of the type for this attribute's type
    * @param typeName     - the name of the attribute's type
    * @param ver          - version info
    * @param findInGlobal - attribute is a explicitly cataloged concept?
    * @return an AttributeDef
    *
    */

  @throws(classOf[NoSuchElementException])
  private def MakeAttribDef(nameSpace: String, name: String, typeNameNs: String, typeName: String, ver: Long, findInGlobal: Boolean, collectionType: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): BaseAttributeDef = {
    if (findInGlobal) {
      val atr = GetElem(Attribute(nameSpace, name, -1, false), s"Attribute $nameSpace.$name does not exist")
      if (atr == null) {
        throw new NoSuchElementException(s"Attribute $nameSpace.$name does not exist")
      }
      return atr
    }

    var ad = new AttributeDef
    val typname: String = typeName.toLowerCase()
    ad.inherited = null
    ad.aType = GetElem(Type(typeNameNs, typname, -1, false), s"Type $typeNameNs.$typeName does not exist")
    if (ad.aType == null) {
      throw new NoSuchElementException(s"Type $typeNameNs.$typeName does not exist")
    }

    val depJarSet = scala.collection.mutable.Set[String]()
    if (ad.aType.JarName != null) depJarSet += ad.aType.JarName
    if (ad.aType.DependencyJarNames != null) depJarSet ++= ad.aType.DependencyJarNames
    val dJars = if (depJarSet.size > 0) depJarSet.toArray else null

    SetBaseElem(ad, nameSpace, name, ver, null, dJars, ownerId, tenantId, uniqueId, mdElementId)
    if (collectionType == null) {
      ad.collectionType = tNone
    } else {
      val ctype = collectionType.trim
      if (ctype.isEmpty() || ctype.compareToIgnoreCase("none") == 0)
        ad.collectionType = tNone
      else if (ctype.compareToIgnoreCase("array") == 0)
        ad.collectionType = tArray
      else if (ctype.compareToIgnoreCase("map") == 0)
        ad.collectionType = tMap
      else
        throw new Throwable(s"Not yet handled collection Type $ctype")
    }
    ad
  }

  private def AddRelationKeys(entity: EntityType, primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])]): Unit = {
    val pKeys = {
      if (primaryKeys == null)
        null
      else
        primaryKeys.map(v => {
          val pk = new PrimaryKey
          pk.constraintName = if (v._1 != null) v._1 else null
          pk.key = v._2.toArray
          pk
        }).toArray
    }

    val fKeys = {
      if (foreignKeys == null)
        null
      else
        foreignKeys.map(v => {
          val fk = new ForeignKey
          fk.constraintName = if (v._1 != null) v._1 else null
          fk.key = v._2.toArray
          fk.forignContainerName = v._3
          fk.forignKey = v._4.toArray
          fk
        }).toArray
    }

    if (pKeys != null && fKeys != null)
      entity.keys = pKeys ++ fKeys
  }

  /**
    * MakeContainerTypeMap participates in the construction of mapped based messages, providing the base type container.
    *
    * @param nameSpace - the container type namespace
    * @param name      - the container name.
    * @param args      - a List of triples (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver       - version info
    * @param jarNm     -
    * @param depJars   -
    * @return an MappedMsgTypeDef
    *
    */

  @throws(classOf[NoSuchElementException])
  private def MakeContainerTypeMap(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, persist: Boolean /* = false */): MappedMsgTypeDef = {
    val st = new MappedMsgTypeDef
    val depJarSet = scala.collection.mutable.Set[String]()

    val msgNm = MdMgr.MkFullName(nameSpace, name)

    args.foreach(elem => {
      val (nsp, nm, typnsp, typenm, isGlobal, collectionType) = elem
      val nmSp = if (nsp != null) nsp else msgNm //BUGBUG:: when nsp != do we need to check for isGlobal is true?????
      val attr = MakeAttribDef(nmSp, nm, typnsp, typenm, ver, isGlobal, collectionType, ownerId, tenantId, uniqueId, mdElementId)
      if (attr.JarName != null) depJarSet += attr.JarName
      if (attr.DependencyJarNames != null) depJarSet ++= attr.DependencyJarNames
      st.attrMap(elem._2) = attr
    })

    if (depJars != null) depJarSet ++= depJars
    val dJars = if (depJarSet.size > 0) depJarSet.toArray else null
    SetBaseElem(st, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    st.PhysicalName(physicalName)

    AddRelationKeys(st, primaryKeys, foreignKeys)

    st.schemaId = schemaId
    st.avroSchema = avroSchema
    st.partitionKey = partitionKey
    st.persist = persist
    st
  }

  /**
    * Construct a StructTypeDef used as the fixed message core type.
    *
    * @param nameSpace - the namespace in which this structure type
    * @param name      - the name of the structure type.
    * @param args      - a List of triples (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver       - version info
    * @param jarNm     -
    * @param depJars   -
    * @return the constructed StructTypeDef
    */

  def MakeStructDef(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, persist: Boolean /* = false */): StructTypeDef = {
    var sd = new StructTypeDef

    val msgNm = MdMgr.MkFullName(nameSpace, name)

    val depJarSet = scala.collection.mutable.Set[String]()

    sd.memberDefs = args.map(elem => {
      val (nsp, nm, typnsp, typenm, isGlobal, collectionType) = elem
      val nmSp = if (nsp != null) nsp else msgNm //BUGBUG:: when nsp != do we need to check for isGlobal is true?????
      val atr = MakeAttribDef(nmSp, nm, typnsp, typenm, ver, isGlobal, collectionType, ownerId, tenantId, uniqueId, mdElementId)
      if (atr.JarName != null) depJarSet += atr.JarName
      if (atr.DependencyJarNames != null) depJarSet ++= atr.DependencyJarNames
      atr
    }).toArray

    if (depJars != null) depJarSet ++= depJars
    val dJars = if (depJarSet.size > 0) depJarSet.toArray else null

    SetBaseElem(sd, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    sd.PhysicalName(physicalName)

    AddRelationKeys(sd, primaryKeys, foreignKeys)

    sd.schemaId = schemaId
    sd.avroSchema = avroSchema
    sd.partitionKey = partitionKey
    sd.persist = persist
    sd
  }

  /**
    * Construct an ArgDef from the supplied arguments.
    *
    * @param name  - the arguments parameter name.
    * @param nmSpc - the type namespace for this argument
    * @param tName - the argument's type name
    * @return an ArgDef
    *
    */

  @throws(classOf[NoSuchElementException])
  private def MakeArgDef(name: String, nmSpc: String, tName: String): ArgDef = {
    val aType = GetElem(Type(nmSpc, tName, -1, false), s"Argument type $nmSpc.$tName does not exist")
    if (aType == null) {
      throw new NoSuchElementException(s"Argument type $nmSpc.$tName does not exist")
    }
    val ad = new ArgDef()
    ad.name = name
    ad.aType = aType
    ad
  }

  // External Functions -- Start 

  // Get Functions
  /** Get All Versions of Types */
  def Types(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseTypeDef]] = {
    GetImmutableSet(Some(typeDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Get All Versions of Types for Key */
  def Types(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseTypeDef]] = {
    GetImmutableSet(typeDefs.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def Types(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseTypeDef]] = Types(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  /** Answer the BaseTypeDef with the supplied namespace and name  */
  def Type(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[BaseTypeDef] = Type(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)

  /** Answer the Active AND Current BaseTypeDef with the supplied namespace and name  */
  def ActiveType(nameSpace: String, name: String): BaseTypeDef = {
    val optContainer: Option[BaseTypeDef] = Type(MdMgr.MkFullName(nameSpace, name), -1, true)
    val container: BaseTypeDef = optContainer match {
      case Some(optContainer) => optContainer
      case _ => null
    }
    container
  }

  /** Answer the BaseTypeDef with the supplied key  */
  def Type(key: String, ver: Long, onlyActive: Boolean): Option[BaseTypeDef] = GetReqValue(Types(key, onlyActive, false), ver)

  /** Answer the BaseTypeDef with the supplied key  */
  def ActiveType(key: String): BaseTypeDef = {
    val typ: Option[BaseTypeDef] = GetReqValue(Types(key.toLowerCase(), true, false), -1)
    val activeType: BaseTypeDef = typ match {
      case Some(typ) => typ
      case _ => null
    }
    activeType
  }

  /** Get All Versions of Messages */
  def Messages(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[MessageDef]] = {
    GetImmutableSet(Some(msgDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Get All Versions of Messages for Key */
  def Messages(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[MessageDef]] = {
    GetImmutableSet(msgDefs.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def Messages(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[MessageDef]] = {
    Messages(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)
  }

  /** Answer the MessageDef with the supplied namespace and name  */
  def Message(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[MessageDef] = {
    Message(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)
  }

  /** Answer the ACTIVE and CURRENT MessageDef with the supplied namespace and name  */
  def ActiveMessage(nameSpace: String, name: String): MessageDef = {
    val optMsg: Option[MessageDef] = Message(MdMgr.MkFullName(nameSpace, name), -1, true)
    val msg: MessageDef = optMsg match {
      case Some(optMsg) => optMsg
      case _ => null
    }
    msg
  }

  /** Answer the MessageDef with the supplied key. */
  def Message(key: String, ver: Long, onlyActive: Boolean): Option[MessageDef] = {
    GetReqValue(Messages(key, onlyActive, false), ver)
  }

  /** Get All Versions of Containers for Key */
  def Containers(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ContainerDef]] = {
    GetImmutableSet(Some(containerDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Get All Versions of Containers for Key */
  def Containers(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ContainerDef]] = {
    GetImmutableSet(containerDefs.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def Containers(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ContainerDef]] = Containers(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  /** Answer the ContainerDef with the supplied namespace and name  */
  def Container(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[ContainerDef] = Container(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)

  /** Answer the Active AND Current ContainerDef with the supplied namespace and name  */
  def ActiveContainer(nameSpace: String, name: String): ContainerDef = {
    val optContainer: Option[ContainerDef] = Container(MdMgr.MkFullName(nameSpace, name), -1, true)
    val container: ContainerDef = optContainer match {
      case Some(optContainer) => optContainer
      case _ => null
    }
    container
  }

  /** Answer the ContainerDef with the supplied key.  Use one of the helper functions described here to form a proper search key.  */
  def Container(key: String, ver: Long, onlyActive: Boolean): Option[ContainerDef] = GetReqValue(Containers(key, onlyActive, false), ver)

  /** Get All Versions of Functions */
  def Functions(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FunctionDef]] = {
    GetImmutableSet(Some(funcDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /**
    * If latestVersion is true it will get only Latest versions of unique function signature (only by positional at this moment, not yet handling named arguments)
    * If latestVersion is false it will get all versions of function name matches to the key
    */
  def Functions(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FunctionDef]] = Functions(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  def Functions(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FunctionDef]] = {
    val allfns = funcDefs.get(key)
    if (latestVersion == false)
      return GetImmutableSet(allfns, onlyActive, latestVersion)

    val latestFns =
    // get latest one
      allfns match {
        case None => None
        case Some(fs) => {
          var newFns = scala.collection.mutable.Map[String, FunctionDef]()
          fs.foreach(f => {
            if (!f.IsDeleted &&
              (onlyActive == false || (onlyActive && f.IsActive))) {
              val fnm = f.FullName // this returns like system.fn1(int, int)
              val existingFn = newFns.getOrElse(fnm, null)
              if (existingFn == null || existingFn.Version < f.Version)
                newFns(fnm) = f
            }
          })
          Some(newFns.map(_._2).toSet)
        }
      }

    latestFns
  }

  /** Answer the ACTIVE and CURRENT function definitions for the supplied namespace.name */
  def FunctionsAvailable(nameSpace: String, name: String): scala.collection.immutable.Set[FunctionDef] = {
    val optFcns: Option[scala.collection.immutable.Set[FunctionDef]] = Functions(nameSpace.toLowerCase(), name.toLowerCase(), true, true)
    val fcns: scala.collection.immutable.Set[FunctionDef] = optFcns match {
      case Some(optFcns) => optFcns
      case _ => null
    }
    fcns
  }

  /* full key name and full arguments names */
  def Function(key: String, args: List[String], ver: Long, onlyActive: Boolean): Option[FunctionDef] = {
    // get functions which match to key & arguments
    val fnMatches =
      funcDefs.get(key) match {
        case None => None
        case Some(fs) => {
          // val signature = key.trim.toLowerCase + "(" + args.foldLeft("")((sig, elem) => sig + "," + elem.trim.toLowerCase) + ")"
          val signature = key.trim.toLowerCase + "(" + args.map(elem => elem.trim.toLowerCase).mkString(",") + ")"
          //logger.debug("signature => " + signature)
          //logger.debug("fs => " + fs.size)
          val matches = fs.filter(f => (onlyActive == false || (onlyActive && f.IsActive)) && (signature == f.typeString))
          //logger.debug("matches => " + matches.toSet.size)
          if (matches.size > 0) Some(matches.toSet) else None
        }
      }

    GetReqValue(fnMatches, ver)
  }

  /* namespace & name for key and fullname for each arg */
  // def Function(nameSpace: String, name: String, args: List[String], ver: Long): Option[FunctionDef] = Function(MkFullName(nameSpace, name), args, ver)

  /* namespace & name for key and namespace & name for each arg */
  def Function(nameSpace: String, name: String, args: List[(String, String)], ver: Long, onlyActive: Boolean): Option[FunctionDef] = Function(MdMgr.MkFullName(nameSpace, name), args.map(a => MdMgr.MkFullName(a._1, a._2)), ver, onlyActive)

  /**
    * Principally used by the Pmml Compiler, answer the FunctionDef with the supplied type signature.
    * Type signatures are in the following form:
    *
    * namespace.functionName(arg.typeString,arg.typeString,...)
    *
    * NOTE: Typestrings are the native scala type representation, not the Ligadata namespace.typename.  For example,
    *
    * Udfs.ContainerMap(Array[SomeMessageType],String,Int,Int,Boolean)
    *
    * @param key : a typestring key
    * @return a FunctionDef matching the type signature key or null if there is no match
    */
  def FunctionByTypeSig(key: String): FunctionDef = {
    compilerFuncDefs.get(key.toLowerCase()).getOrElse(null)
  }

  /**
    * Principally used by the Pmml Compiler, answer the macro that matches the supplied type signature.  A
    * MacroDef is returned.  The MacroDef is similar in most respects to the FunctionDef (in fact a
    * MacroDef isA FunctionDef).  The difference is how they are used by the Pmml compiler.
    *
    * @see <documentation reference> for more details regarding function macros.
    * @see FunctionByTypeSig for details regarding key format.
    * @param key : a typestring key
    * @return a MacroDef matching the type signature key or null if there is no match
    */
  def MacroByTypeSig(key: String): MacroDef = {
    macroDefs.get(key.toLowerCase()).getOrElse(null)
  }

  /**
    * Answer the function macros that match the supplied full name.  See MdMgr.MkFullName for key format.
    * This method is principally for the author and the authoring tools to investigate what is available for
    * use during construction or rule sets and other models.
    *
    * @param key : a typestring key
    * @return a FunctionDef or null if there is no match
    */
  def MacrosAvailable(key: String): Set[MacroDef] = {
    macroDefSets.get(key.toLowerCase()).getOrElse(Set[MacroDef]())
  }

  /** Get All Versions of Attributes */
  def Attributes(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseAttributeDef]] = {
    GetImmutableSet(Some(attrbDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Get All Versions of Attributes for Key */
  def Attributes(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseAttributeDef]] = {
    GetImmutableSet(attrbDefs.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def Attributes(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[BaseAttributeDef]] = Attributes(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  /** Answer the BaseAttributeDef with the supplied namespace and name  */
  def Attribute(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[BaseAttributeDef] = Attribute(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)

  def Attribute(key: String, ver: Long, onlyActive: Boolean): Option[BaseAttributeDef] = GetReqValue(Attributes(key, onlyActive, false), ver)

  /** Answer the Active AND Current ContainerDef with the supplied namespace and name  */
  def ActiveAttribute(nameSpace: String, name: String): BaseAttributeDef = {
    val optAttr: Option[BaseAttributeDef] = Attribute(MdMgr.MkFullName(nameSpace, name), -1, true)
    val attr: BaseAttributeDef = optAttr match {
      case Some(optAttr) => optAttr
      case _ => null
    }
    attr
  }

  def ContainerForSchemaId(schemaId:Int): Option[ContainerDef] = {
    val cont = schemaIdMap.getOrElse(schemaId, null)
    if (cont != null) Some(cont) else None
  }

  def ElementIdForSchemaId(schemaId:Int): Long = {
    schemaIdToElemntIdMap.getOrElse(schemaId, 0)
  }

  def ContainerForElementId(elemId:Long): Option[BaseElem] = {
    val elem = elementIdMap.getOrElse(elemId, null)
    if (elem != null) Some(elem) else None
  }

  /** Get All Versions of FactoryOfModelInstanceFactoryDef */
  def FactoryOfMdlInstFactories(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef]] = {
    GetImmutableSet(Some(factoryOfMdlInstFactories.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Answer ALL Active AND Current FactoryOfModelInstanceFactoryDef  */
  def ActiveFactoryOfMdlInstFactories: scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef] = {
    val optFactories: Option[scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef]] = FactoryOfMdlInstFactories(true, true)
    val active: scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef] = optFactories match {
      case Some(optFactories) => optFactories
      case _ => null
    }
    active
  }

  /** Get All Versions of FactoryOfMdlInstFactories for Key */
  def FactoryOfMdlInstFactories(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef]] = {
    GetImmutableSet(factoryOfMdlInstFactories.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def FactoryOfMdlInstFactories(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[FactoryOfModelInstanceFactoryDef]] = FactoryOfMdlInstFactories(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  /** Answer the FactoryOfModelInstanceFactoryDef with the supplied namespace and name  */
  def FactoryOfMdlInstFactory(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[FactoryOfModelInstanceFactoryDef] = FactoryOfMdlInstFactory(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)

  def FactoryOfMdlInstFactory(key: String, ver: Long, onlyActive: Boolean): Option[FactoryOfModelInstanceFactoryDef] = GetReqValue(FactoryOfMdlInstFactories(key, onlyActive, false), ver)

  /** Get All Versions of Models */
  def Models(onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ModelDef]] = {
    GetImmutableSet(Some(modelDefs.flatMap(x => x._2)), onlyActive, latestVersion)
  }

  /** Answer ALL Active AND Current ModelDefs  */
  def ActiveModels: scala.collection.immutable.Set[ModelDef] = {
    val optModels: Option[scala.collection.immutable.Set[ModelDef]] = Models(true, true)
    val activeModels: scala.collection.immutable.Set[ModelDef] = optModels match {
      case Some(optModels) => optModels
      case _ => null
    }
    activeModels
  }

  /** Get All Versions of Models for Key */
  def Models(key: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ModelDef]] = {
    GetImmutableSet(modelDefs.get(key.trim.toLowerCase), onlyActive, latestVersion)
  }

  def Models(nameSpace: String, name: String, onlyActive: Boolean, latestVersion: Boolean): Option[scala.collection.immutable.Set[ModelDef]] = Models(MdMgr.MkFullName(nameSpace, name), onlyActive, latestVersion)

  /** Answer the ModelDef with the supplied namespace and name  */
  def Model(nameSpace: String, name: String, ver: Long, onlyActive: Boolean): Option[ModelDef] = Model(MdMgr.MkFullName(nameSpace, name), ver, onlyActive)

  def Model(key: String, ver: Long, onlyActive: Boolean): Option[ModelDef] = GetReqValue(Models(key, onlyActive, false), ver)

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyModel(nameSpace: String, name: String, ver: Long, operation: String): ModelDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val model = modelDefs.getOrElse(key, null)
    if (model == null) {
      logger.debug("The model " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The model $key may have been removed already", null)
    } else {
      var versionMatch: ModelDef = null
      modelDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              logger.debug("The model " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The model " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The model " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyMessage(nameSpace: String, name: String, ver: Long, operation: String): MessageDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val message = msgDefs.getOrElse(key, null)
    if (message == null) {
      logger.debug("The message " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The message $key may have been removed already", null)
    } else {
      var versionMatch: MessageDef = null
      msgDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              logger.debug("The message " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The message " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The message " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyContainer(nameSpace: String, name: String, ver: Long, operation: String): ContainerDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val container = containerDefs.getOrElse(key, null)
    if (container == null) {
      logger.debug("The container " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The container $key may have been removed already", null)
    } else {
      var versionMatch: ContainerDef = null
      containerDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              logger.debug("The container " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The container " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The container " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyFunction(nameSpace: String, name: String, ver: Long, operation: String): FunctionDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val function = funcDefs.getOrElse(key, null)
    if (function == null) {
      logger.debug("The function " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The function $key may have been removed already", null)
    } else {
      var versionMatch: FunctionDef = null
      funcDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              if (compilerFuncDefs.contains(m.typeString.toLowerCase)) compilerFuncDefs -= m.typeString.toLowerCase
              logger.debug("The function " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The function " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The function " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyAttribute(nameSpace: String, name: String, ver: Long, operation: String): BaseAttributeDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val attribute = attrbDefs.getOrElse(key, null)
    if (attribute == null) {
      logger.debug("The attribute " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The attribute $key may have been removed already", null)
    } else {
      var versionMatch: BaseAttributeDef = null
      attrbDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              logger.debug("The attribute " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The attribute " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The attribute " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  @throws(classOf[ObjectNolongerExistsException])
  def ModifyType(nameSpace: String, name: String, ver: Long, operation: String): BaseTypeDef = {
    val key = MdMgr.MkFullName(nameSpace, name)
    val typ = typeDefs.getOrElse(key, null)
    if (typ == null) {
      logger.debug("The type " + key + " doesn't exist ")
      throw ObjectNolongerExistsException(s"The type $key may have been removed already", null)
    } else {
      var versionMatch: BaseTypeDef = null
      typeDefs(key).foreach(m =>
        if (m.ver == ver) {
          versionMatch = m
          operation match {
            case "Remove" => {
              m.Deleted
              m.Deactive
              logger.debug("The type " + key + " is removed ")
            }
            case "Activate" => {
              m.Active
              logger.debug("The type " + key + " is activated ")
            }
            case "Deactivate" => {
              m.Deactive
              logger.debug("The type " + key + " is deactivated ")
            }
          }
        })
      versionMatch
    }
  }

  def RemoveMessage(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyMessage(nameSpace, name, ver, "Remove")
  }

  def DeactivateMessage(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyMessage(nameSpace, name, ver, "Deactivate")
  }

  def ActivateMessage(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyMessage(nameSpace, name, ver, "Activate")
  }

  def RemoveContainer(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Remove")
  }

  def DeactivateContainer(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Deactivate")
  }

  def ActivateContainer(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Activate")
  }

  def RemoveFunction(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyFunction(nameSpace, name, ver, "Remove")
  }

  def DeactivateFunction(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyFunction(nameSpace, name, ver, "Deactivate")
  }

  def ActivateFunction(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Activate")
  }

  def RemoveAttribute(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyAttribute(nameSpace, name, ver, "Remove")
  }

  def DeactivateAttribute(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyAttribute(nameSpace, name, ver, "Deactivate")
  }

  def ActivateAttribute(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Activate")
  }

  def RemoveType(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyType(nameSpace, name, ver, "Remove")
  }

  def DeactivateType(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyType(nameSpace, name, ver, "Deactivate")
  }

  def ActivateType(nameSpace: String, name: String, ver: Long): BaseElemDef = {
    ModifyContainer(nameSpace, name, ver, "Activate")
  }

  // Make Functions. These will just make and send back the object

  /**
    * MakeScalar catalogs any of the standard scalar types in the metadata manager's global typedefs map
    *
    * @param nameSpace - the scalar type namespace
    * @param name      - the scalar type name.
    */

  @throws(classOf[IllegalArgumentException])
  @throws(classOf[AlreadyExistsException])
  def MakeScalar(nameSpace: String, name: String, tp: Type, physicalName: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null, implementationName: String = "SomeImplementation"): ScalarTypeDef = {
    if (Type(nameSpace, name, -1, false) != None) {
      throw AlreadyExistsException(s"Scalar $nameSpace.$name already exists.", null)
    }
    if (implementationName == null) {
      throw new IllegalArgumentException(s"Expecting ImplementationName for type $nameSpace.$name with physicalName $physicalName")
    }

    val st: ScalarTypeDef = new ScalarTypeDef
    SetBaseElem(st, nameSpace, name, ver, jarNm, depJars, ownerId, tenantId, uniqueId, mdElementId)
    st.typeArg = tp
    st.PhysicalName(physicalName)
    st.implementationName(implementationName)
    st
  }

  /**
    * MakeTypeDef catalogs the base type def (or one of its subclasses) supplied.
    *
    */
  /*
  // physicalName may be null
  @throws(classOf[AlreadyExistsException])
  def MakeTypeDef(nameSpace: String, name: String, typeType: BaseTypeDef, physicalName: String, ver: Long, jarNm: String, depJars: Array[String]): BaseTypeDef = {
    if (Type(nameSpace, name, -1) != None) {
      throw AlreadyExistsException(s"TypeDef $nameSpace.$name already exists.")
    }
    SetBaseElem(typeType, nameSpace, name, ver, jarNm, depJars)
    typeType.PhysicalName(physicalName)
    typeType
  }
*/

  /**
    * MakeArray catalogs an Array based type in the metadata manager's global typedefs map
    *
    * @param nameSpace - the array type namespace
    * @param name      - the array name.
    * @param tpNameSp  - the namespace of the array element type
    * @param tpName    - the name for the element's type
    * @param numDims   - (not currently used) the number of dimensions for this array type
    * @param ver       - the version info
    *
    */
  // We should not have physicalName. This container type has type inside, which has PhysicalName
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeArray(nameSpace: String, name: String, tpNameSp: String, tpName: String, numDims: Int, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long, recompile: Boolean = false/* , persist: Boolean = false */): ArrayTypeDef = {
    val typ = Type(nameSpace, name, -1, false)
    if (typ != None) {
      if (recompile) {
        //Only make a message if the version is greater then the last known version already in the system.
        if (typ.get.ver > ver) {
          throw AlreadyExistsException(s"Higher active version of Array $nameSpace.$name already exists.", null)
        }
      } else {
        //Only make a message if the version is greater or equal then the last known version already in the system.
        if (typ.get.ver >= ver) {
          throw AlreadyExistsException(s"Higher active version of Array $nameSpace.$name already exists.", null)
        }
      }
    }

    //if (Type(nameSpace, name, -1, false) != None) {
    //   throw AlreadyExistsException(s"Array $nameSpace.$name already exists.")
    //  }
    val elemDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The array's item type $tpNameSp.$tpName does not exist")
    if (elemDef == null) {
      throw new NoSuchElementException(s"The array's item type $tpNameSp.$tpName does not exist")
    }
    val depJarSet = scala.collection.mutable.Set[String]()
    if (elemDef.JarName != null) depJarSet += elemDef.JarName
    if (elemDef.DependencyJarNames != null) depJarSet ++= elemDef.DependencyJarNames
    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
    val st = new ArrayTypeDef
    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
    st.elemDef = elemDef
    st.arrayDims = numDims
    st
  }
//
//  /**
//    * MakeArrayBuffer catalogs an ArrayBuffer based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the array type name space
//    * @param name      - the array name.
//    * @param tpNameSp  - the name space of the array element type
//    * @param tpName    - the name for the element's type
//    * @param numDims   - (not currently used) the number of dimensions for this array type
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeArrayBuffer(nameSpace: String, name: String, tpNameSp: String, tpName: String, numDims: Int, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long, recompile: Boolean = false/* , persist: Boolean = false */): ArrayBufTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (recompile) {
//        //Only make a message if the version is greater then the last known version already in the system.
//        if (typ.get.ver > ver) {
//          throw AlreadyExistsException(s"Higher active version of ArrayBuffer $nameSpace.$name already exists.", null)
//        }
//      } else {
//        //Only make a message if the version is greater or equal then the last known version already in the system.
//        if (typ.get.ver >= ver) {
//          throw AlreadyExistsException(s"Higher active version of ArrayBuffer $nameSpace.$name already exists.", null)
//        }
//      }
//    }
//
//    /*if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"ArrayBuffer $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val elemDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The array buffer's item type $tpNameSp.$tpName does not exist")
//    if (elemDef == null) {
//      throw new NoSuchElementException(s"The array buffer's item type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (elemDef.JarName != null) depJarSet += elemDef.JarName
//    if (elemDef.DependencyJarNames != null) depJarSet ++= elemDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new ArrayBufTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.elemDef = elemDef
//    st
//  }
//
//  /**
//    * MakeList catalogs an List based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the list's type namespace
//    * @param name      - the list name.
//    * @param tpNameSp  - the name space of the list item type
//    * @param tpName    - the name for the element's type
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeList(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): ListTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (typ.get.ver >= ver)
//        throw AlreadyExistsException(s"List $nameSpace.$name already exists.", null)
//    }
//    /*if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"List $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val valDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The list's item type $tpNameSp.$tpName does not exist")
//    if (valDef == null) {
//      throw new NoSuchElementException(s"The list's item type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (valDef.JarName != null) depJarSet += valDef.JarName
//    if (valDef.DependencyJarNames != null) depJarSet ++= valDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new ListTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.valDef = valDef
//    st
//  }
//
//  /**
//    * MakeQueue catalogs an Queue based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the list's type namespace
//    * @param name      - the queue name.
//    * @param tpNameSp  - the name space of the queue item type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    */
//
//  @throws(classOf[NoSuchElementException])
//  def MakeQueue(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long) = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (typ.get.ver >= ver)
//        throw AlreadyExistsException(s"List $nameSpace.$name already exists.", null)
//    }
//    /* if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"List $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val valDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The queue's item type $tpNameSp.$tpName does not exist")
//    if (valDef == null) {
//      throw new NoSuchElementException(s"The queue's item type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (valDef.JarName != null) depJarSet += valDef.JarName
//    if (valDef.DependencyJarNames != null) depJarSet ++= valDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new QueueTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.valDef = valDef
//    st
//  }
//
//  /**
//    * MakeSet catalogs an Set based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, recompile: Boolean = false/* , persist: Boolean = false */): SetTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (recompile) {
//        //Only make a message if the version is greater then the last known version already in the system.
//        if (typ.get.ver > ver) {
//          throw AlreadyExistsException(s"Higher active version of Set $nameSpace.$name already exists.", null)
//        }
//      } else {
//        //Only make a message if the version is greater or equal then the last known version already in the system.
//        if (typ.get.ver >= ver) {
//          throw AlreadyExistsException(s"Higher active version of Set $nameSpace.$name already exists.", null)
//        }
//      }
//    }
//
//    /* if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"Set $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val keyDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The set's key type $tpNameSp.$tpName does not exist")
//    if (keyDef == null) {
//      throw new NoSuchElementException(s"The set's key type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new SetTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st
//  }
//
//  /**
//    * MakeSet catalogs an Set based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeImmutableSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): ImmutableSetTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (typ.get.ver >= ver)
//        throw AlreadyExistsException(s"Set $nameSpace.$name already exists.", null)
//    }
//
//    /* if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"Set $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val keyDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The set's key type $tpNameSp.$tpName does not exist")
//    if (keyDef == null) {
//      throw new NoSuchElementException(s"The set's key type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new ImmutableSetTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st
//  }
//
//  /**
//    * MakeTreeSet catalogs an TreeSet based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeTreeSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, recompile: Boolean = false/* , persist: Boolean = false */): TreeSetTypeDef = {
//
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (recompile) {
//        //Only make a message if the version is greater then the last known version already in the system.
//        if (typ.get.ver > ver) {
//          throw AlreadyExistsException(s"Higher active version of TreeSet $nameSpace.$name already exists.", null)
//        }
//      } else {
//        //Only make a message if the version is greater or equal then the last known version already in the system.
//        if (typ.get.ver >= ver) {
//          throw AlreadyExistsException(s"Higher active version of TreeSet $nameSpace.$name already exists.", null)
//        }
//      }
//    }
//
//    /* if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"TreeSet $nameSpace.$name already exists.")
//    }
//    *
//    */
//    val keyDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The tree set's key type $tpNameSp.$tpName does not exist")
//    if (keyDef == null) {
//      throw new NoSuchElementException(s"The tree set's key type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new TreeSetTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st
//  }
//
//  /**
//    * MakeSortedSet catalogs an SortedSet based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - the type's version
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeSortedSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, recompile: Boolean = false/* , persist: Boolean = false */): SortedSetTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (recompile) {
//        //Only make a message if the version is greater then the last known version already in the system.
//        if (typ.get.ver > ver) {
//          throw AlreadyExistsException(s"Higher active version of Type $nameSpace.$name already exists... unable to add SortedSet with this name.", null)
//        }
//      } else {
//        //Only make a message if the version is greater or equal then the last known version already in the system.
//        if (typ.get.ver >= ver) {
//          throw AlreadyExistsException(s"Higher active version of Type $nameSpace.$name already exists... unable to add SortedSet with this name.", null)
//        }
//      }
//    }
//
//    /*if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"Type $nameSpace.$name already exists... unable to add SortedSet with this name.")
//    }
//    *
//    */
//    val keyDef = GetElem(Type(tpNameSp, tpName, -1, false), s"The tree set's key type $tpNameSp.$tpName does not exist")
//    if (keyDef == null) {
//      throw new NoSuchElementException(s"The tree set's key type $tpNameSp.$tpName does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new SortedSetTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st
//  }

  /**
    * MakeMap catalogs a scala.collection.mutable.Map based type in the metadata manager's global typedefs map
    *
    * @param nameSpace - the map type's namespace
    * @param name      - the map name.
    * @param ver       - the version info
    *
    */
  // We should not have physicalName. This container type has type inside, which has PhysicalName
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeMap(nameSpace: String, name: String, valueNsp: String, valueName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, recompile: Boolean = false/* , persist: Boolean = false */): MapTypeDef = {
    val typ = Type(nameSpace, name, -1, false)
    if (typ != None) {
      if (recompile) {
        //Only make a message if the version is greater then the last known version already in the system.
        if (typ.get.ver > ver) {
          throw AlreadyExistsException(s"Higher active version of Map $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater or equal then the last known version already in the system.
        if (typ.get.ver >= ver) {
          throw AlreadyExistsException(s"Higher active version of Map $nameSpace.$name already exists in the system", null)
        }
      }
    }

    /*if (Type(nameSpace, name, -1, false) != None) {
      throw AlreadyExistsException(s"Map $nameSpace.$name already exists.")
    }
    * 
    */

    val valDef = GetElem(Type(valueNsp, valueName, -1, false), s"Value type $valueNsp.$valueName does not exist")
    if (valDef == null) {
      throw new NoSuchElementException(s"Value type ($valueNsp.$valueName) does not exist")
    }

    val depJarSet = scala.collection.mutable.Set[String]()
    if (valDef.JarName != null) depJarSet += valDef.JarName
    if (valDef.DependencyJarNames != null) depJarSet ++= valDef.DependencyJarNames
    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null

    val st = new MapTypeDef
    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
    st.valDef = valDef
    st
  }

//
//  /**
//    * MakeImmutableMap catalogs an scala.collection.immutable.Map based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param key       - the namespace and name for the map's key
//    * @param value     - the namespace and name for the map's value
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeImmutableMap(nameSpace: String, name: String, key: (String, String), value: (String, String), ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, recompile: Boolean = false/* , persist: Boolean = false */): ImmutableMapTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (recompile) {
//        //Only make a message if the version is greater then the last known version already in the system.
//        if (typ.get.ver > ver) {
//          throw AlreadyExistsException(s"Higher active version of Map $nameSpace.$name already exists in the system", null)
//        }
//      } else {
//        //Only make a message if the version is greater or equal then the last known version already in the system.
//        if (typ.get.ver >= ver) {
//          throw AlreadyExistsException(s"Higher active version of Map $nameSpace.$name already exists in the system", null)
//        }
//      }
//    }
//
//    /* if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"Map $nameSpace.$name already exists.")
//    }*/
//    val (keyNmSp, keyTypeNm) = key
//    val (valNmSp, valTypeNm) = value
//    val keyDef = GetElem(Type(keyNmSp, keyTypeNm, -1, false), s"Key type $keyNmSp.$keyTypeNm does not exist")
//    val valDef = GetElem(Type(valNmSp, valTypeNm, -1, false), s"Value type $valNmSp.$valTypeNm does not exist")
//    if (keyDef == null || valDef == null) {
//      throw new NoSuchElementException(s"Either key type ($keyNmSp.$keyTypeNm) and/or value type ($valNmSp.$valTypeNm) does not exist")
//    }
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    if (valDef.JarName != null) depJarSet += valDef.JarName
//    if (valDef.DependencyJarNames != null) depJarSet ++= valDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val st = new ImmutableMapTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st.valDef = valDef
//    st
//  }
//
//  /**
//    * MakeHashMap catalogs a HashMap based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param key       - the namespace and name for the map's key
//    * @param value     - the namespace and name for the map's value
//    * @param ver       - the version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeHashMap(nameSpace: String, name: String, key: (String, String), value: (String, String), ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): HashMapTypeDef = {
//
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (typ.get.ver >= ver)
//        throw AlreadyExistsException(s"HashMap $nameSpace.$name already exists.", null)
//    }
//    /*if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"HashMap $nameSpace.$name already exists.")
//    }
//    *
//    */
//
//    val (keyNmSp, keyTypeNm) = key
//    val (valNmSp, valTypeNm) = value
//    val keyDef = GetElem(Type(keyNmSp, keyTypeNm, -1, false), s"Key type $keyNmSp.$keyTypeNm does not exist")
//    val valDef = GetElem(Type(valNmSp, valTypeNm, -1, false), s"Value type $valNmSp.$valTypeNm does not exist")
//    if (keyDef == null || valDef == null) {
//      throw new NoSuchElementException(s"Either key type ($keyNmSp.$keyTypeNm) and/or value type ($valNmSp.$valTypeNm) does not exist")
//    }
//
//    val depJarSet = scala.collection.mutable.Set[String]()
//    if (keyDef.JarName != null) depJarSet += keyDef.JarName
//    if (keyDef.DependencyJarNames != null) depJarSet ++= keyDef.DependencyJarNames
//    if (valDef.JarName != null) depJarSet += valDef.JarName
//    if (valDef.DependencyJarNames != null) depJarSet ++= valDef.DependencyJarNames
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//
//    val st = new HashMapTypeDef
//    SetBaseElem(st, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    st.keyDef = keyDef
//    st.valDef = valDef
//    st
//  }
//
//  /**
//    * MakeTupleType catalogs a Tuple based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param tuples    - an array of (namespace,typename) pairs that correspond to each tuple element
//    * @param ver       - the version info
//    *
//    *                  Note: Between one and twenty-two elements may be specified in the tuples array.
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def MakeTupleType(nameSpace: String, name: String, tuples: Array[(String, String)], ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): TupleTypeDef = {
//    val typ = Type(nameSpace, name, -1, false)
//    if (typ != None) {
//      if (typ.get.ver >= ver)
//        throw AlreadyExistsException(s"Tuple $nameSpace.$name already exists.", null)
//    }
//    /*if (Type(nameSpace, name, -1, false) != None) {
//      throw AlreadyExistsException(s"Typle $nameSpace.$name already exists.")
//    }
//    *
//    */
//
//    val depJarSet = scala.collection.mutable.Set[String]()
//
//    var tupleDefs: ArrayBuffer[BaseTypeDef] = new ArrayBuffer[BaseTypeDef]()
//    tuples.foreach(tup => {
//      val (nmspc, nm): (String, String) = tup
//      val tupType: BaseTypeDef = GetElem(Type(nmspc, nm, -1, false), s"Tuple element type $nmspc.$nm does not exist")
//      if (tupType == null) {
//        throw new NoSuchElementException(s"Tuple element type $nmspc.$nm does not exist")
//      }
//      if (tupType.JarName != null) depJarSet += tupType.JarName
//      if (tupType.DependencyJarNames != null) depJarSet ++= tupType.DependencyJarNames
//      tupleDefs += tupType
//    })
//
//    val depJars = if (depJarSet.size > 0) depJarSet.toArray else null
//    val tt: TupleTypeDef = new TupleTypeDef
//    tt.tupleDefs = tupleDefs.toArray
//    SetBaseElem(tt, nameSpace, name, ver, null, depJars, ownerId, tenantId, uniqueId, mdElementId)
//    tt
//  }

  /**
    * Construct a FunctionDef from the supplied arguments.
    *
    * @param nameSpace     - the namespace in which this structure type
    * @param name          - the function name that all are to be cataloged in the nameSpace argument.
    * @param physicalName  - this is the "full" package qualified function name
    * @param retTypeNsName - a tuple (return type namespace and name)
    * @param args          - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param fmfeatures    - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details
    * @param ver           - the version of the function
    * @param jarNm         - where this function is housed
    * @param depJars       - the list of jars that this function depends upon
    * @return a FunctionDef
    *
    *         FIXME: The version, the jarNm, and dependency jars... are these really appropriate for object creation?
    *         Can these be supplied at catalog time (i.e., the Add... function)?  Do we really want the list of all
    *         these jars on every function cataloged in the metadata manager?
    *
    *         I believe with prudent construction of the classpath, the only jar that need to be supplied is the
    *         jar containing the function.  Furthermore, it is really the full object path (e.g.,
    *         com.ligadata.pmml.udfs.Udfs that has many, many, many functions) where this jar association is needed.
    *         Any component then would get the package path of the function to look up the jar for 100s or functions
    *         at one crack.  Detailing this repetitive information on all FunctionDef instances seems TOO MUCH to me.
    *
    *         Similarly, the version information associated with the function should be expressed at the
    *         package level.  Functions as I see it are released a package at a time.  All the functions defined
    *         in that package should have the same version information.
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeFunc(nameSpace: String, name: String, physicalName: String, retTypeNsName: (String, String), args: List[(String, String, String)], fmfeatures: Set[FcnMacroAttr.Feature], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null): FunctionDef = {

    if (Function(nameSpace, name, args.map(a => (a._2, a._3)), -1, false) != None) {
      throw AlreadyExistsException(s"Function $nameSpace.$name already exists.", null)
    }
    val fn: FunctionDef = new FunctionDef
    if (fmfeatures != null && fmfeatures.size > 0) {
      fn.features ++= fmfeatures
    }
    val (retTypeNs, retType) = retTypeNsName
    fn.retType = GetElem(Type(retTypeNs, retType, -1, false), s"Return type $retTypeNs.$retType does not exist")
    if (fn.retType == null) {
      throw new NoSuchElementException(s"Return type $retTypeNs.$retType does not exist")
    }

    val depJarSet = scala.collection.mutable.Set[String]()
    if (fn.retType.JarName != null) depJarSet += fn.retType.JarName
    if (fn.retType.DependencyJarNames != null) depJarSet ++= fn.retType.DependencyJarNames

    fn.args = args.map(elem => {
      val arg = MakeArgDef(elem._1, elem._2, elem._3)
      if (arg.DependencyJarNames != null) depJarSet ++= arg.DependencyJarNames
      arg
    }).toArray

    if (depJars != null) depJarSet ++= depJars
    val dJars = if (depJarSet.size > 0) depJarSet.toArray else null

    SetBaseElem(fn, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    fn.PhysicalName(physicalName)
    fn.active = true
    fn
  }

  /**
    * Construct a list of FunctionDef from the supplied arguments.  All function names share the same namespace, return type and arguments.
    *
    * @param nameSpace     - the namespace in which this structure type
    * @param names         - a list of function names that all are to be cataloged in the nameSpace argument.
    * @param retTypeNsName - a tuple (return type namespace and name) shared by all of the function names
    * @param args          - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param fmfeatures    - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details
    * @param ver           - the version of the function
    * @param jarNm         - where this function is housed
    * @param depJars       - the list of jars that this function depends upon
    * @return a List[FunctionDef]
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeFuncs(nameSpace: String, names: List[(String, String)], retTypeNsName: (String, String), args: List[(String, String, String)], fmfeatures: Set[FcnMacroAttr.Feature], ver: Long, jarNm: String, depJars: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): List[FunctionDef] = {
    val funcs = names.map(name => {
      val (logicalNm, physicalName) = name
      MakeFunc(nameSpace, logicalNm, physicalName, retTypeNsName, args, fmfeatures, ownerId, tenantId, uniqueId, mdElementId, ver, jarNm, depJars)
    }).toList
    funcs
  }

  /**
    * MakeMacro adds a MacroDef to two maps in the metadata manager supplied.  A MacroDef is very similar
    * in many respect to a function definition and is looked up in the same way as a function by the
    * pmml compiler. In fact it is a subclass of FunctionDef.  The macros are used by the pmml compiler
    * to manage several kinds of code generation issues.
    *
    * NOTE: There is a huge effort underway in the Scala community to provide a general macro capability
    * that will vastly expand the power of the Scala programming language, offering powerful meta programming
    * capabilities.  Unfortunately, these language extensions are a work in progress with things being added
    * (and jerked out) from release to release of the language.  At some point, I believe we will be able
    * to adapt their macro generation approach for our purposes.  Until that time arrives, the macros
    * we are supporting here are quite limited (by comparison) but also will nevertheless be useful for
    * extending the pmml code generation process.
    *
    * @param nameSpace        - the namespace of this macro
    * @param name             - the macro name
    * @param retTypeNsName    - a tuple (return type namespace and name)
    * @param args             - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param macrofeatures    - a set of hints that describe the sort of macro being cataloged.  See mdelems.scala for details
    * @param macroTemplateStr - a tuple that has templates for both the fixed message case and the mapped message case.  When
    *                         no containers are in use in the macro, the same template will be used.  See the other MakeMacro function for
    *                         clarification of what was said here.
    * @return a MacroDef
    *
    */
  @throws(classOf[NoSuchElementException])
  def MakeMacro(nameSpace: String, name: String, retTypeNsName: (String, String), args: List[(String, String, String)], macrofeatures: Set[FcnMacroAttr.Feature], macroTemplateStr: (String, String), ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1): MacroDef = {

    val aMacro: MacroDef = new MacroDef
    SetBaseElem(aMacro, nameSpace, name, ver, null, null, ownerId, tenantId, uniqueId, mdElementId)

    if (macrofeatures != null && macrofeatures.size > 0) {
      aMacro.features ++= macrofeatures
    }
    aMacro.macroTemplate = macroTemplateStr

    val (retTypeNs, retType) = retTypeNsName
    aMacro.retType = GetElem(Type(retTypeNs, retType, -1, false), s"Return type $retTypeNs.$retType does not exist")
    if (aMacro.retType == null) {
      throw new NoSuchElementException(s"Return type $retTypeNs.$retType does not exist")
    }

    aMacro.args = args.map(elem => {
      MakeArgDef(elem._1, elem._2, elem._3)
    }).toArray

    aMacro
  }

  /**
    * This version of MakeMacro accepts just one template string.  It is useful for those cases where the macro
    * does not contain a container that might be fixed or mapped.  The same template is used regardless.
    *
    * @param nameSpace        - the namespace of this macro
    * @param name             - the macro name
    * @param retTypeNsName    - a tuple (return type namespace and name)
    * @param args             - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param macrofeatures    - a set of hints that describe the sort of macro being cataloged.  See mdelems.scala for details
    * @param macroTemplateStr - a tuple that has templates for both the fixed message case and the mapped message case.  When
    *                         no containers are in use in the macro, the same template will be used.  See the other MakeMacro function for
    *                         clarification of what was said here.
    * @return a MacroDef
    */
  @throws(classOf[NoSuchElementException])
  def MakeMacro(nameSpace: String, name: String, retTypeNsName: (String, String), args: List[(String, String, String)], macrofeatures: Set[FcnMacroAttr.Feature], macroTemplateStr: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): MacroDef = {
    MakeMacro(nameSpace, name, retTypeNsName, args, macrofeatures, (macroTemplateStr, macroTemplateStr), ownerId, tenantId, uniqueId, mdElementId, ver)
  }

  /**
    * Construct and catalog a "fixed structure" message container with the supplied named attributes. They may be of arbitrary
    * types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @param recompile
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeFixedMsg(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null, primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null, recompile: Boolean = false, persist: Boolean /* = false */): MessageDef = {

    val latestActiveMessage = Message(nameSpace, name, -1, false)
    if (latestActiveMessage != None) {
      if (recompile) {
        //Only make a message if the version is greater then the last known version already in the system.
        if (latestActiveMessage.get.Version > ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater or equal then the last known version already in the system.
        if (latestActiveMessage.get.Version >= ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      }
    }

    var msg: MessageDef = new MessageDef
    msg.containerType = MakeStructDef(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, persist)

    var dJars: Array[String] = depJars
    if (msg.containerType.isInstanceOf[ContainerTypeDef]) // This should match
      dJars = msg.containerType.asInstanceOf[ContainerTypeDef].DependencyJarNames // Taking all dependencies for Container type. That has everything is enough for this

    SetBaseElem(msg, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    msg.PhysicalName(physicalName)
    msg
  }

  /**
    * Construct and catalog a fixed container with the supplied named attributes. They may be of arbitrary
    * types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @param recompile
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeFixedContainer(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null, primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null, recompile: Boolean = false, persist: Boolean /* = false */): ContainerDef = {

    val latestActiveContainer = Container(nameSpace, name, -1, false)
    if (latestActiveContainer != None) {
      if (recompile) {
        //Only make a message if the version is greater then the last known version already in the system.
        if (latestActiveContainer.get.Version > ver) {
          throw AlreadyExistsException(s"Higher active version of Container $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater then the last known version already in the system.
        if (latestActiveContainer.get.Version >= ver) {
          throw AlreadyExistsException(s"Higher active version of Container $nameSpace.$name already exists in the system", null)
        }
      }
    }

    var container = new ContainerDef
    container.containerType = MakeStructDef(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, persist)

    var dJars: Array[String] = depJars
    if (container.containerType.isInstanceOf[ContainerTypeDef]) {
      // This should match
      // Taking all dependencies for Container type. That has everything is enough for this
      dJars = container.containerType.asInstanceOf[ContainerTypeDef].DependencyJarNames
    }
    SetBaseElem(container, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)

    container.PhysicalName(physicalName)
    container
  }

  /**
    * Construct and catalog a "map based" message container with an arbitrary number of named attributes with <b>HETEROGENEOUS</b> types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @param recompile
    * @return a (mapped) MessageDef
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeMappedMsg(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], recompile: Boolean, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, persist: Boolean): MessageDef = {
    val latestActiveMessage = Message(nameSpace, name, -1, false)
    if (latestActiveMessage != None) {
      if (recompile) {
        if (latestActiveMessage.get.Version > ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater or equal then the last known version already in the system.
        if (latestActiveMessage.get.Version >= ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      }
    }

    var msg: MessageDef = new MessageDef
    msg.containerType = MakeContainerTypeMap(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, persist)

    var dJars: Array[String] = depJars
    if (msg.containerType.isInstanceOf[ContainerTypeDef]) // This should match
      dJars = msg.containerType.asInstanceOf[ContainerTypeDef].DependencyJarNames // Taking all dependencies for Container type. That has everything is enough for this

    SetBaseElem(msg, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    msg.PhysicalName(physicalName)
    msg
  }

  /**
    * Construct and catalog a "map based" container with an arbitrary number of named attributes with <b>HETEROGENEOUS</b> types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @param recompile
    * @return a (mapped) ContainerDef
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeMappedContainer(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, recompile: Boolean = false, persist: Boolean /* = false */): ContainerDef = {

    val latestActiveContainer = Container(nameSpace, name, -1, false)
    if (latestActiveContainer != None) {
      if (recompile) {
        //Only make a message if the version is greater then the last known version already in the system.
        if (latestActiveContainer.get.Version > ver) {
          throw AlreadyExistsException(s"Higher active version of Container $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater then the last known version already in the system.
        if (latestActiveContainer.get.Version >= ver) {
          throw AlreadyExistsException(s"Higher active version of Container $nameSpace.$name already exists in the system", null)
        }
      }
    }

    var container = new ContainerDef
    container.containerType = MakeContainerTypeMap(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, persist)

    var dJars: Array[String] = depJars
    if (container.containerType.isInstanceOf[ContainerTypeDef]) // This should match
      dJars = container.containerType.asInstanceOf[ContainerTypeDef].DependencyJarNames // Taking all dependencies for Container type. That has everything is enough for this

    SetBaseElem(container, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)

    container.PhysicalName(physicalName)
    container
  }

  /**
    * Construct and catalog a "map based" message container with an arbitrary number of <b>HOMOGENEOUSLY</b> typed value attributes.
    *
    * @param nameSpace      - the namespace in which this message should be cataloged
    * @param name           - the name of the message.
    * @param physicalName   - the fully qualified name of the class that will represent the runtime instance of the message
    * @param argTypNmSpName - a List of attributes information (attribute type namespace, attribute type name)
    * @param argNames       - a corresponding list of attribute names
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @param recompile
    * @return a (mapped) MessageDef
    */
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def MakeMappedMsg(nameSpace: String, name: String, physicalName: String, argTypNmSpName: (String, String), argNames: List[String], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], recompile: Boolean, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, persist: Boolean): MessageDef = {

    val latestActiveMessage = Message(nameSpace, name, -1, false)
    if (latestActiveMessage != None) {
      if (recompile) {
        if (latestActiveMessage.get.Version > ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      } else {
        //Only make a message if the version is greater or equal then the last known version already in the system.
        if (latestActiveMessage.get.Version >= ver) {
          throw AlreadyExistsException(s"Higher active version of Message $nameSpace.$name already exists in the system", null)
        }
      }
    }

    val msgNm = MdMgr.MkFullName(nameSpace, name)

    val (typeNmSp, typeName) = argTypNmSpName
    val args = argNames.map(elem => (msgNm, elem, typeNmSp, typeName, false, null)) //BUGBUG::Making all local Attributes and Collection Types does not handled

    var msg: MessageDef = new MessageDef
    msg.containerType = MakeContainerTypeMap(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, persist)

    var dJars: Array[String] = depJars
    if (msg.containerType.isInstanceOf[ContainerTypeDef]) // This should match
      dJars = msg.containerType.asInstanceOf[ContainerTypeDef].DependencyJarNames // Taking all dependencies for Container type. That has everything is enough for this

    SetBaseElem(msg, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)
    msg.PhysicalName(physicalName)

    msg
  }

  /**
    * Construct and catalog a model definition.  The model definition represents the essential metadata regarding
    * a PMML generated model, including the identifying information, the model type, and the inputs and output
    * variables used/generated by the model.  The input and output information is used by the online learning engine
    * manager to generate the necessary order execution between the scheduled model working set.
    *
    * @param nameSpace                     - the namespace in which this model should be cataloged
    * @param name                          - the name of the model.
    * @param physicalName                  - the fully qualified className for the compiled model.
    * @param modelRep                      - the sort of model input this is (e.g., a Jar, a PMML src string, et al)
    * @param inputMsgSets                  - Sets of Messages it depends on (attributes referred in this model). Each set must met (all messages should available) to trigger this model
    * @param outputMsgs                    - All possible output messages produced by this model
    * @param isReusable                    - can instances of this model be cached by the engine and reused on subsequent calls for same type?
    * @param objectDefStr                  - model definition String
    * @param miningModelType               :  a visual identifier that can be queried for and/or displayed. Values
    *                                      for this currently include any of the dmg.org model types or our own types... any {BaselineModel, ClusteringModel,
    *                                      GeneralRegressionModel, MiningModel, NaiveBayesModel, NearestNeighborModel, NeuralNetwork, RegressionModel,
    *                                      RuleSetModel, SequenceModel, Scorecard, SupportVectorMachineModel, TextModel, TimeSeriesModel, TreeModel,
    *                                      CustomScala, CustomJava, Unknown}
    * @param ver                           - a long... the version number assigned to this model ... by default '1'
    * @param jarNm                         - the name of the jar sans path.  The path is prescribed by the engine configuration
    * @param depJars                       - the jars upon which the jarNm depends in order to execute
    * @param recompile                     a Boolean flag that when true will verify that the existing model not only exists for this model name, but
    *                                      that it has an older version than the one being supplied.  When false, version equality is ok (i.e., the model
    *                                      is being replaced).
    * @param supportsInstanceSerialization when true, instances of this model are serialized and persisted in the metadata store, saving startup costs
    *                                      required to prepare new instances.  NOTE: this feature is **not** implemented, but will prove useful for reducing
    *                                      cluster startup costs for PMML models in particular when it **is**.
    * @param modelConfig                   - Any extract configuration for this model (like config for JAVA/SCALA models etc)
    * @return the ModelDef instance
    *
    */

  def MakeModelDef(nameSpace: String
                   , name: String
                   , physicalName: String
                   , ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long
                   , modelRep: ModelRepresentation = ModelRepresentation.JAR
                   , inputMsgSets: Array[Array[MessageAndAttributes]] = Array[Array[MessageAndAttributes]]()
                   , outputMsgs: Array[String] = Array[String]()
                   , isReusable: Boolean = false
                   , objectDefStr: String = ""
                   , miningModelType: MiningModelType = MiningModelType.UNKNOWN
                   , ver: Long = 1
                   , jarNm: String = null
                   , depJars: Array[String] = null
                   , recompile: Boolean = false
                   , supportsInstanceSerialization: Boolean = false
                   , modelConfig: String = ""): ModelDef = {

    /** Determine model existence constraints and throw exception if they are not met */
    var modelExists: Boolean = false
    val existingModel = Model(nameSpace, name, -1, false)
    if (existingModel != None) {
      val latestmodel = existingModel.get.asInstanceOf[ModelDef]
      if (recompile == true) {
        // version equality is OK, if we are recompiling
        if (ver < latestmodel.Version) {
          modelExists = true
        }
      } else {
        if (ver <= latestmodel.Version) {
          modelExists = true
        }
      }
    }
    if (modelExists) {
      throw new AlreadyExistsException(s"Model $nameSpace.$name version should be higher than existing Models.", null)
    }

    /** Create the BaseAttributeDef instances for the supplied input and output variable lists */
    val mdlNm = MdMgr.MkFullName(nameSpace, name)
    val depJarSet = scala.collection.mutable.Set[String]()

    /** Instantiate the model definition.  Update the base element with basic id information */
    val mdl: ModelDef = new ModelDef(modelRep
      , miningModelType
      , inputMsgSets
      , outputMsgs
      , isReusable
      , supportsInstanceSerialization
      , modelConfig)

    /** FIXME: All of the statements down to the return of the ModelDef instance really should be just arguments
      * to the constructor that utilizes values instead of the variables now in use... save this work for a refactor
      * effort in the future.
      */
    if (depJars != null) depJarSet ++= depJars
    val dJars = if (depJarSet.size > 0) depJarSet.toArray else null

    if (objectDefStr != null)
      mdl.ObjectDefinition(objectDefStr)

    /** In the case of a PMML model, save the string in the model representation now.  PMML strings are
      * (for the first go) to be injested at the last possible minute by the shim model's factory object.
      * The instances so created will be cached for subsequent calls on the thread with which the instance
      * is associated.
      */
    if (mdl.modelRepresentation == ModelRepresentation.PMML)
      mdl.ObjectFormat(ObjFormatType.fPMML)
    mdl.PhysicalName(physicalName)
    mdl.tenantId = tenantId
    SetBaseElem(mdl, nameSpace, name, ver, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)

    mdl
  }

  // Add Functions
  //
  def MakeFactoryOfModelInstanceFactory(nameSpace: String, name: String, modelRepSupported: ModelRepresentation.ModelRepresentation, physicalName: String, ver: Long, jarNm: String, depJars: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): FactoryOfModelInstanceFactoryDef = {
    val f = new FactoryOfModelInstanceFactoryDef(modelRepSupported)
    SetBaseElem(f, nameSpace, name, ver, jarNm, depJars, ownerId, tenantId, uniqueId, mdElementId)
    f.PhysicalName(physicalName)
    f
  }

  @throws(classOf[AlreadyExistsException])
  def AddFactoryOfModelInstanceFactory(nameSpace: String, name: String, modelRepSupported: ModelRepresentation.ModelRepresentation, physicalName: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null): Unit = {
    AddFactoryOfModelInstanceFactory(MakeFactoryOfModelInstanceFactory(nameSpace, name, modelRepSupported, physicalName, ver, jarNm, depJars, ownerId, tenantId, uniqueId, mdElementId))
  }

  @throws(classOf[AlreadyExistsException])
  def AddFactoryOfModelInstanceFactory(f: FactoryOfModelInstanceFactoryDef): Unit = {
    factoryOfMdlInstFactories.addBinding(f.FullName, f)
  }

  /**
    * MakeScalar catalogs any of the standard scalar types in the metadata manager's global typedefs map
    *
    * @param nameSpace - the scalar type namespace
    * @param name      - the scalar type name.
    */

  @throws(classOf[AlreadyExistsException])
  def AddScalar(nameSpace: String, name: String, tp: Type, physicalName: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = null, implementationName: String = null): Unit = {
    AddScalar(MakeScalar(nameSpace, name, tp, physicalName, ownerId, tenantId, uniqueId, mdElementId, ver, jarNm, depJars, implementationName))
  }

  @throws(classOf[AlreadyExistsException])
  def AddScalar(st: ScalarTypeDef): Unit = {
    if (Type(st.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Scalar ${st.FullName} already exists.", null)
    }
    typeDefs.addBinding(st.FullName, st)
  }

  /**
    * AddArray catalogs an Array based type in the metadata manager's global typedefs map
    *
    * @param nameSpace - the array type namespace
    * @param name      - the array name.
    * @param tpNameSp  - the namespace of the array element type
    * @param tpName    - the name for the element's type
    * @param numDims   - (not currently used) the number of dimensions for this array type
    * @param ver       - version info
    *
    */
  // We should not have physicalName. This container type has type inside, which has PhysicalName
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddArray(nameSpace: String, name: String, tpNameSp: String, tpName: String, numDims: Int, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
    AddArray(MakeArray(nameSpace, name, tpNameSp, tpName, numDims, ownerId, tenantId, uniqueId, mdElementId, ver))
  }

  @throws(classOf[AlreadyExistsException])
  def AddArray(at: ArrayTypeDef): Unit = {
    if (Type(at.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Array ${at.FullName} already exists.", null)
    }
    typeDefs.addBinding(at.FullName, at)
  }

//  /**
//    * AddArrayBuffer catalogs an ArrayBuffer based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the array type name space
//    * @param name      - the array name.
//    * @param tpNameSp  - the name space of the array element type
//    * @param tpName    - the name for the element's type
//    * @param numDims   - (not currently used) the number of dimensions for this array type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddArrayBuffer(nameSpace: String, name: String, tpNameSp: String, tpName: String, numDims: Int, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddArrayBuffer(MakeArrayBuffer(nameSpace, name, tpNameSp, tpName, numDims, ownerId, tenantId, uniqueId, mdElementId, ver))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddArrayBuffer(abt: ArrayBufTypeDef): Unit = {
//    if (Type(abt.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"ArrayBuffer ${abt.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(abt.FullName, abt)
//  }
//
//  /**
//    * AddList catalogs an List based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the list's type namespace
//    * @param name      - the list name.
//    * @param tpNameSp  - the name space of the list item type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddList(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddList(MakeList(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddList(lst: ListTypeDef): Unit = {
//    if (Type(lst.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"List ${lst.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(lst.FullName, lst)
//  }
//
//  /**
//    * AddQueue catalogs an Queue based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the list's type namespace
//    * @param name      - the list name.
//    * @param tpNameSp  - the name space of the list item type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddQueue(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddQueue(MakeQueue(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddQueue(queue: QueueTypeDef): Unit = {
//    if (Type(queue.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"A type with queue's name ${queue.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(queue.FullName, queue)
//  }
//
//  /**
//    * AddSet catalogs an Set based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddSet(MakeSet(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddSet(set: SetTypeDef): Unit = {
//    if (Type(set.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"Set ${set.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(set.FullName, set)
//  }
//
//  /**
//    * AddImmutableSet catalogs an immutable Set based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddImmutableSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddImmutableSet(MakeImmutableSet(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddImmutableSet(set: ImmutableSetTypeDef): Unit = {
//    if (Type(set.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"Set ${set.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(set.FullName, set)
//  }
//
//  /**
//    * MakeTreeSet catalogs an TreeSet based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddTreeSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddTreeSet(MakeTreeSet(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddTreeSet(tree: TreeSetTypeDef): Unit = {
//    if (Type(tree.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"TreeSet ${tree.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(tree.FullName, tree)
//  }
//
//  /**
//    * AddSortedSet catalogs an SortedSetTypeDef based upon the supplied parameters.
//    *
//    * @param nameSpace - the set's type namespace
//    * @param name      - the set name.
//    * @param tpNameSp  - the namespace of the set's key element type
//    * @param tpName    - the name for the element's type
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddSortedSet(nameSpace: String, name: String, tpNameSp: String, tpName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddSortedSet(MakeSortedSet(nameSpace, name, tpNameSp, tpName, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddSortedSet(set: SortedSetTypeDef): Unit = {
//    if (Type(set.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"SortedSet ${set.FullName} cannot be created... a type by that name already exists.", null)
//    }
//    typeDefs.addBinding(set.FullName, set)
//  }

  /**
    * AddMap catalogs a scala.collection.mutable.Map based type in the metadata manager's global typedefs map
    *
    * @param nameSpace - the map type's namespace
    * @param name      - the map name.
    * @param ver       - version info
    *
    */
  // We should not have physicalName. This container type has type inside, which has PhysicalName
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddMap(nameSpace: String, name: String, valueNsp: String, valueName: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
    AddMap(MakeMap(nameSpace, name, valueNsp, valueName, ver, ownerId, tenantId, uniqueId, mdElementId))
  }

  @throws(classOf[AlreadyExistsException])
  def AddMap(map: MapTypeDef): Unit = {
    if (Type(map.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Map ${map.FullName} already exists.", null)
    }
    typeDefs.addBinding(map.FullName, map)
  }

//  /**
//    * AddImmutableMap catalogs a scala.collection.immutable.Map based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param key       - the namespace and name for the map's key
//    * @param value     - the namespace and name for the map's value
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddImmutableMap(nameSpace: String, name: String, key: (String, String), value: (String, String), ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddImmutableMap(MakeImmutableMap(nameSpace, name, key, value, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddImmutableMap(map: ImmutableMapTypeDef): Unit = {
//    if (Type(map.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"Map ${map.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(map.FullName, map)
//  }
//
//  /**
//    * MakeHashMap catalogs a HashMap based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param key       - the namespace and name for the map's key
//    * @param value     - the namespace and name for the map's value
//    * @param ver       - version info
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddHashMap(nameSpace: String, name: String, key: (String, String), value: (String, String), ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddHashMap(MakeHashMap(nameSpace, name, key, value, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddHashMap(hmap: HashMapTypeDef): Unit = {
//    if (Type(hmap.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"HashMap ${hmap.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(hmap.FullName, hmap)
//  }
//
//  /**
//    * MakeTupleType catalogs a Tuple based type in the metadata manager's global typedefs map
//    *
//    * @param nameSpace - the map type's namespace
//    * @param name      - the map name.
//    * @param tuples    - an array of (namespace,typename) pairs that correspond to each tuple element
//    * @param ver       - version info
//    *
//    *                  Note: Between one and twenty-two elements may be specified in the tuples array.
//    *
//    */
//  // We should not have physicalName. This container type has type inside, which has PhysicalName
//  @throws(classOf[AlreadyExistsException])
//  @throws(classOf[NoSuchElementException])
//  def AddTupleType(nameSpace: String, name: String, tuples: Array[(String, String)], ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
//    AddTupleType(MakeTupleType(nameSpace, name, tuples, ver, ownerId, tenantId, uniqueId, mdElementId))
//  }
//
//  @throws(classOf[AlreadyExistsException])
//  def AddTupleType(tt: TupleTypeDef): Unit = {
//    if (Type(tt.FullName, -1, false) != None) {
//      throw AlreadyExistsException(s"Typle ${tt.FullName} already exists.", null)
//    }
//    typeDefs.addBinding(tt.FullName, tt)
//  }

  /**
    * Construct a FunctionDef from the supplied arguments and add it to this MdMgr instance
    *
    * @param nameSpace     - the namespace in which this structure type
    * @param name          - the function name that all are to be cataloged in the nameSpace argument.
    * @param physicalName  - FIXME: find out what this is and document it .... is this full class name??????????????????
    * @param retTypeNsName - a tuple (return type namespace and name)
    * @param args          - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param fmfeatures    - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details.
    * @param ver           - the version of the function
    * @param jarNm         - where this function is housed
    * @param depJars       - the list of jars that this function depends upon
    *
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddFunc(nameSpace: String, name: String, physicalName: String, retTypeNsName: (String, String), args: List[(String, String, String)], fmfeatures: Set[FcnMacroAttr.Feature], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String]()): Unit = {
    AddFunc(MakeFunc(nameSpace, name, physicalName, retTypeNsName, args, fmfeatures, ownerId, tenantId, uniqueId, mdElementId, ver, jarNm, depJars))
  }

  /**
    * Catalog the supplied FunctionDef in this MdMgr instance.  Should it already exist, the catalog
    * operation is rejected with an AlreadyExistsException.
    *
    * @param fn the FunctionDef
    *
    */

  @throws(classOf[AlreadyExistsException])
  def AddFunc(fn: FunctionDef): Unit = {
    val args = fn.args.map(a => a.aType.FullName).toList
    if (Function(fn.FullName, args, -1, false) != None) {
      val argsStr = args.mkString(",")
      throw AlreadyExistsException(s"Function ${fn.FullName} with arguments \'${argsStr}\' already exists.", null)
    }
    val fcnSig: String = fn.typeString
    if (FunctionByTypeSig(fcnSig) != null) {
      throw AlreadyExistsException(s"Function ${fn.FullName} with signature \'${fcnSig}\' already exists.", null)
    }
    if (MacroByTypeSig(fcnSig) != null) {
      throw AlreadyExistsException(s"Macro ${fn.FullName} with signature \'${fcnSig}\' will be hidden should this function be cataloged.", null)
    }

    funcDefs.addBinding(fn.FullName, fn)

    /** add it to the pmml compiler's typesignature based map as well */
    compilerFuncDefs(fcnSig.toLowerCase()) = fn

  }

  /**
    * Catalog the supplied AttributeDef in this MdMgr instance.  Should it already exist, the catalog
    * operation is rejected with an AlreadyExistsException.
    *
    * @param attr the AttributeDef
    *
    */

  @throws(classOf[AlreadyExistsException])
  def AddAttribute(attr: BaseAttributeDef): Unit = {
    attrbDefs.addBinding(attr.FullName, attr)
  }

  @throws(classOf[AlreadyExistsException])
  def MakeConcept(nameSpace: String, name: String, typeNameNs: String, typeName: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, isGlobal: Boolean): BaseAttributeDef = {
    val attr = MakeAttribDef(nameSpace, name, typeNameNs, typeName, ver, isGlobal, null, ownerId, tenantId, uniqueId, mdElementId) // BUGBUG:: Considering no CollectionType for Concecept
    attr
  }

  /**
    * Construct a list of FunctionDef from the supplied arguments.  All function names share the same namespace, return type and arguments.
    *
    * @param nameSpace     - the namespace in which this structure type
    * @param names         - a list of function names that all are to be cataloged in the nameSpace argument.
    * @param retTypeNsName - a tuple (return type namespace and name) shared by all of the function names
    * @param args          - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param fmfeatures    - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details
    * @param ver           - the version of the function
    * @param jarNm         - where this function is housed
    * @param depJars       - the list of jars that this function depends upon
    * @return a List[FunctionDef]
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddFuncs(nameSpace: String, names: List[(String, String)], retTypeNsName: (String, String), args: List[(String, String, String)], fmfeatures: Set[FcnMacroAttr.Feature], ver: Long, jarNm: String, depJars: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
    AddFuncs(MakeFuncs(nameSpace, names, retTypeNsName, args, fmfeatures, ver, jarNm, depJars, ownerId, tenantId, uniqueId, mdElementId))
  }

  /**
    * Construct a list of FunctionDef from the supplied arguments.
    *
    * @param fns - a List[FunctionDef] to catalog in this MdMgr instance
    */

  @throws(classOf[AlreadyExistsException])
  def AddFuncs(fns: List[FunctionDef]): Unit = {
    fns.foreach(fn => {
      val args = fn.args.map(a => a.aType.FullName).toList
      if (Function(fn.FullName, args, -1, false) != None) {
        val argsStr = args.mkString(",")
        throw AlreadyExistsException(s"Function ${fn.FullName} with arguments \'${argsStr}\' already exists.", null)
      }
      val fcnSig: String = fn.typeString
      if (FunctionByTypeSig(fcnSig) != null) {
        throw AlreadyExistsException(s"Function ${fn.FullName} with signature \'${fcnSig}\' already exists.", null)
      }
      if (MacroByTypeSig(fcnSig) != null) {
        throw AlreadyExistsException(s"Macro ${fn.FullName} with signature \'${fcnSig}\' will be hidden should this function be cataloged.", null)
      }

    })
    fns.foreach(fn => {
      funcDefs.addBinding(fn.FullName, fn)
      val key = fn.typeString

      /** add it to the pmml compiler's typesignature based map as well */
      compilerFuncDefs(key.toLowerCase()) = fn
    })
  }

  /**
    * Construct a MacroDef from the supplied arguments and add it to this MdMgr instance
    *
    * @param nameSpace         - the namespace in which this structure type
    * @param name              - the function name that all are to be cataloged in the nameSpace argument.
    * @param retTypeNsName     - a tuple (return type namespace and name)
    * @param args              - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param macrofeatures     - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details
    * @param macroTemplateStrs - a Tuple2.  when the function has CLASSUPDATE feature, the first macro in the pair
    *                          has the "builds" template has a template that supports "fixed" container field update.  The second string
    *                          in the tuple contains the "mapped" container template for same function.  When the CLASSUPDATE feature is
    *                          not specified, the same macro template can be specified for both tuple._1 and ._2.  A convenience
    *                          method is available that handles this for you.
    * @param ver               - the version of the function
    *
    */

  def AddMacro(nameSpace: String, name: String, retTypeNsName: (String, String), args: List[(String, String, String)], macrofeatures: Set[FcnMacroAttr.Feature], macroTemplateStrs: (String, String), ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1): Unit = {
    AddMacro(MakeMacro(nameSpace, name, retTypeNsName, args, macrofeatures, macroTemplateStrs, ownerId, tenantId, uniqueId, mdElementId, ver))
  }

  /**
    * Construct a MacroDef from the supplied arguments and add it to this MdMgr instance
    *
    * @param nameSpace        - the namespace in which this structure type
    * @param name             - the function name that all are to be cataloged in the nameSpace argument.
    * @param retTypeNsName    - a tuple (return type namespace and name)
    * @param args             - a List of triples (argument name, argument type namespace and type name), one for each argument
    * @param macrofeatures    - a set of hints that describe the sort of function being cataloged.  See mdelems.scala for details
    * @param macroTemplateStr - a Tuple2.  when the function does not support CLASSUPDATE feature, supply just
    *                         one template to this method.
    * @param ver              - the version of the function
    *
    */

  def AddMacro(nameSpace: String, name: String, retTypeNsName: (String, String), args: List[(String, String, String)], macrofeatures: Set[FcnMacroAttr.Feature], macroTemplateStr: String, ver: Long, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): Unit = {
    AddMacro(MakeMacro(nameSpace, name, retTypeNsName, args, macrofeatures, macroTemplateStr, ver, ownerId, tenantId, uniqueId, mdElementId))
  }

  /**
    * Catalog the supplied MacroDef to this MdMgr instance.  Should one exist with the identical
    * type string, the catalog operation is rejected with an AlreadyExistsException.
    *
    * @param mac - the MacroDef to be cataloged FunctionByTypeSig(key : String): FunctionDef = { compilerFuncDefs.get(key).getOrElse(null) }
    *            MacroByTypeSig(key : String)
    */

  @throws(classOf[AlreadyExistsException])
  def AddMacro(mac: MacroDef): Unit = {
    val macroSignature: String = mac.typeString
    if (MacroByTypeSig(macroSignature) != null) {
      throw AlreadyExistsException(s"Macro ${mac.FullName} with signature \'${macroSignature}\' already exists.", null)
    }
    if (FunctionByTypeSig(macroSignature) != null) {
      throw AlreadyExistsException(s"Warning! Macro ${mac.FullName} with signature \'${macroSignature}\' is hidden by a function with the same signature.", null)
    }

    macroDefSets.addBinding(mac.FullName, mac)
    macroDefs(macroSignature.toLowerCase()) = mac
  }

  /**
    * Construct and catalog a "fixed structure" message with the supplied named attributes. They may be of arbitrary
    * types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddFixedMsg(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String](), primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null): Unit = {
    AddMsg(MakeFixedMsg(nameSpace, name, physicalName, args, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, false, false))
  }

  /**
    * Construct and catalog a "map based" message with an arbitrary number of named attributes with  <b>HETEROGENEOUS</b> types.
    *
    * @param nameSpace - the namespace in which this message should be cataloged
    * @param name      - the name of the message.
    * @param args      - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @return Unit
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddMappedMsg(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String](), primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null): Unit = {
    AddMsg(MakeMappedMsg(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, false, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, false))
  }

  /**
    * Construct and catalog a "map based" message container with an arbitrary number of <b>HOMOGENEOUSLY</b> typed value attributes.
    *
    * @param nameSpace      - the namespace in which this message should be cataloged
    * @param name           - the name of the message.
    * @param argTypNmSpName - a List of triples (attribute type namspace, attribute type name)
    * @param argNames       - a corresponding list of the attribute names for the types
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @return Unit
    */
  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddMappedMsg(nameSpace: String, name: String, physicalName: String, argTypNmSpName: (String, String), argNames: List[String], ver: Long, jarNm: String, depJars: Array[String], primaryKeys: List[(String, List[String])], foreignKeys: List[(String, List[String], String, List[String])], partitionKey: Array[String], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String): Unit = {
    AddMsg(MakeMappedMsg(nameSpace, name, physicalName, argTypNmSpName, argNames, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, false, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, false))
  }

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddMsg(msg: MessageDef): Unit = {
    if (Type(msg.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Message type ${msg.FullName} already exists.", null)
    }
    if (Message(msg.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Message ${msg.FullName} already exists.", null)
    }
    if (msg.containerType == null) {
      throw new NoSuchElementException(s"The containerType of the Message ${msg.FullName} can not be null.")
    }
    val typ = msg.containerType.asInstanceOf[ContainerTypeDef]
    typeDefs.addBinding(typ.FullName, typ)
      msgDefs.addBinding(msg.FullName, msg)
    if (msg.containerType != null && msg.containerType.schemaId > 0) {
      schemaIdToElemntIdMap(msg.containerType.schemaId) = msg.MdElementId
      schemaIdMap(msg.containerType.schemaId) = msg
    }
    else
      logger.error("SchemaId not found for Container:%s with Version:%d".format(msg.FullName, msg.Version))
    elementIdMap(msg.MdElementId) = msg
  }

  /**
    * Construct and catalog a "fixed structure" container with the supplied named attributes. They may be of arbitrary
    * types.
    *
    * @param nameSpace    - the namespace in which this message should be cataloged
    * @param name         - the name of the message.
    * @param physicalName - the fully qualified name of the class that will represent the runtime instance of the message
    * @param args         - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @return Unit
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddFixedContainer(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String](), primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null): Unit = {
    AddContainer(MakeFixedContainer(nameSpace, name, physicalName, args, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, false, false))
  }

  /**
    * Construct and catalog a "map based" container with an arbitrary number of named attributes with HETEROGENEOUS types.
    *
    * @param nameSpace - the namespace in which this message should be cataloged
    * @param name      - the name of the message.
    * @param args      - a List of attributes information (attribute namespace, attribute name, attribute type namespace, attribute type name, isGlobal)
    * @param ver
    * @param jarNm
    * @param depJars
    * @param primaryKeys
    * @param foreignKeys
    * @param partitionKey
    * @return Unit
    */

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddMappedContainer(nameSpace: String, name: String, physicalName: String, args: List[(String, String, String, String, Boolean, String)], ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, schemaId: Int, avroSchema:String, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String](), primaryKeys: List[(String, List[String])] = null, foreignKeys: List[(String, List[String], String, List[String])] = null, partitionKey: Array[String] = null): Unit = {
    AddContainer(MakeMappedContainer(nameSpace, name, physicalName, args, ver, jarNm, depJars, primaryKeys, foreignKeys, partitionKey, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, false, false))
  }

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddContainer(container: ContainerDef): Unit = {
    if (Type(container.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Container type ${container.FullName} already exists.", null)
    }
    if (Container(container.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Container ${container.FullName} already exists.", null)
    }
    if (container.containerType == null) {
      throw new NoSuchElementException(s"The containerType of container ${container.FullName} can not be null.")
    }
    val typ = container.containerType.asInstanceOf[ContainerTypeDef]
    typeDefs.addBinding(typ.FullName, typ)
    containerDefs.addBinding(container.FullName, container)
    if (container.containerType != null && container.containerType.schemaId > 0) {
      schemaIdMap(container.containerType.schemaId) = container
      schemaIdToElemntIdMap(container.containerType.schemaId) = container.MdElementId
    }
    else
      logger.error("SchemaId not found for Container:%s with Version:%d".format(container.FullName, container.Version))
    elementIdMap(container.MdElementId) = container
  }

  @throws(classOf[AlreadyExistsException])
  @throws(classOf[NoSuchElementException])
  def AddContainerType(containerType: ContainerTypeDef): Unit = {
    if (Type(containerType.FullName, -1, false) != None) {
      throw AlreadyExistsException(s"Container type ${containerType.FullName} already exists.", null)
    }
    typeDefs.addBinding(containerType.FullName, containerType)
  }

  /**
    * Construct and catalog a model definition.  The model definition represents the essential metadata regarding
    * a PMML generated model, including the identifying information, the model type, and the inputs and output
    * variables used/generated by the model.  The input and output information is used by the online learning engine
    * manager to generate the necessary order execution between the scheduled model working set.
    *
    * @param nameSpace                     - the namespace in which this model should be cataloged
    * @param name                          - the name of the model.
    * @param physicalName                  - the fully qualified className for the compiled model.
    * @param modelRep                      - the sort of model input this is (e.g., a Jar, a PMML src string, et al)
    * @param inputMsgSets                  - Sets of Messages it depends on (attributes referred in this model). Each set must met (all messages should available) to trigger this model
    * @param outputMsgs                    - All possible output messages produced by this model
    * @param isReusable                    - can instances of this model be cached by the engine and reused on subsequent calls for same type?
    * @param objectDefStr                  - model definition String
    * @param miningModelType               :  a visual identifier that can be queried for and/or displayed. Values
    *                                      for this currently include any of the dmg.org model types or our own types... any {BaselineModel, ClusteringModel,
    *                                      GeneralRegressionModel, MiningModel, NaiveBayesModel, NearestNeighborModel, NeuralNetwork, RegressionModel,
    *                                      RuleSetModel, SequenceModel, Scorecard, SupportVectorMachineModel, TextModel, TimeSeriesModel, TreeModel,
    *                                      CustomScala, CustomJava, Unknown}
    * @param ver                           - a long... the version number assigned to this model ... by default '1'
    * @param jarNm                         - the name of the jar sans path.  The path is prescribed by the engine configuration
    * @param depJars                       - the jars upon which the jarNm depends in order to execute
    * @return the ModelDef instance as a measure of convenience
    *
    */
  def AddModelDef(nameSpace: String, name: String, physicalName: String, modelRep: ModelRepresentation.ModelRepresentation, inputMsgSets: Array[Array[MessageAndAttributes]], outputMsgs: Array[String],
                  isReusable: Boolean, objectDefStr: String, miningModelType: MiningModelType.MiningModelType, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long, ver: Long = 1, jarNm: String = null, depJars: Array[String] = Array[String](), modelConfig: String = ""): Unit = {
    AddModelDef(MakeModelDef(nameSpace, name, physicalName, ownerId, tenantId, uniqueId, mdElementId, modelRep,  inputMsgSets, outputMsgs, isReusable, objectDefStr, miningModelType, ver, jarNm, depJars, false, false, modelConfig), false)
  }

  def AddModelDef(mdl: ModelDef, allowLatestVersion: Boolean): Unit = {
    var modelExists: Boolean = false
    val existingModel = Model(mdl.FullName, -1, false)
    if (existingModel != None) {
      val latesmodel = existingModel.get.asInstanceOf[ModelDef]
      if (allowLatestVersion) {
        if (mdl.Version < latesmodel.Version) {
          modelExists = true
        }
      }
      else {
        if (mdl.Version <= latesmodel.Version) {
          modelExists = true
        }
      }
    }

    if (modelExists) {
      throw AlreadyExistsException(s"Model ${mdl.FullName}  should be higher than existing models.", null)
    }

    // if (Model(mdl.FullName, -1, false) != None) {
    //    throw AlreadyExistsException(s"Model ${mdl.FullName} already exists.")
    // }
    modelDefs.addBinding(mdl.FullName, mdl)
    elementIdMap(mdl.MdElementId) = mdl
  }

  def MakeJarDef(nameSpace: String, name: String, version: String, ownerId: String, tenantId: String, uniqueId: Long, mdElementId: Long): JarDef = {
    val jd = new JarDef
    var depJars = new Array[String](0)
    SetBaseElem(jd, nameSpace, name, version.toLong, name, depJars, ownerId, tenantId, uniqueId, mdElementId)
    jd
  }

  def MakeNode(nodeId: String, nodePort: Int, nodeIpAddr: String,
               jarPaths: List[String],
               scala_home: String,
               java_home: String,
               classpath: String,
               clusterId: String,
               power: Int,
               roles: Array[String],
               description: String): NodeInfo = {
    val ni = new NodeInfo
    ni.nodeId = nodeId
    ni.nodePort = nodePort
    ni.nodeIpAddr = nodeIpAddr
    if (jarPaths != null) {
      ni.jarPaths = jarPaths.toArray
    }
    ni.scala_home = scala_home
    ni.java_home = java_home
    ni.classpath = classpath
    ni.clusterId = clusterId
    ni.power = power
    ni.roles = roles
    ni.description = description
    ni
  }

  def AddNode(ni: NodeInfo): Option[String] = {
    // Enforce the presence of clusterId int he node
    if (ni.clusterId == null) {
      throw InvalidArgumentException("Failed to Add the node, ClusterId Can not be null", null)
    }

    if (nodes.contains(ni.nodeId.trim.toLowerCase)) {
      var isSame = nodes.get(ni.nodeId.trim.toLowerCase).get.asInstanceOf[NodeInfo].equals(ni)
      nodes(ni.nodeId.toLowerCase) = ni
      if (!isSame) return Some("Update") else return None
    } else {
      nodes(ni.nodeId.toLowerCase) = ni
      return Some("Add")
    }
  }

  def GetNode(nodeId: String): NodeInfo = {
    return nodes.getOrElse(nodeId,null)
  }

  def RemoveNode(nodeId: String): Unit = {
    val ni = nodes.getOrElse(nodeId, null)
    if (ni != null) {
      nodes -= nodeId
    }
  }

  def DumpModelConfigs: Unit = {
    modelConfigs.keys.foreach(key => {
      logger.debug("----" + key + "-----")
      logger.debug(modelConfigs(key))
    })
    logger.debug("----")
  }

  def GetModelConfigKeys: Array[String] = {
    modelConfigs.keySet.toArray[String]
  }

  def AddModelConfig(key: String, inCfg: scala.collection.immutable.Map[String, Any]): Unit = {
    modelConfigs(key.toLowerCase) = inCfg
  }

  def GetModelConfig(key: String): scala.collection.immutable.Map[String, Any] = {
    modelConfigs.getOrElse(key.toLowerCase, scala.collection.immutable.Map[String, Any]())
  }

  /**
    * AddUserProperty - add UserPropertiesMap to a local cache
    *
    * @param upi : UserPropertiesInfo
    */
  def AddUserProperty(upi: UserPropertiesInfo): Option[String] = {
    if (configurations.contains(upi.ClusterId.trim.toLowerCase)) {
      var isSame = configurations.get(upi.ClusterId.trim.toLowerCase).get.asInstanceOf[UserPropertiesInfo].equals(upi)
      configurations(upi.ClusterId) = upi
      if (!isSame) return Some("Update") else return None
    } else {
      configurations(upi.ClusterId) = upi
      return Some("Add")
    }
  }

  /**
    * GetUserProperty - return a String value of a User Property
    *
    * @param clusterId : String
    * @param key       : String
    */
  def GetUserProperty(clusterId: String, key: String): String = {
    if (configurations.contains(clusterId)) {
      val upi: scala.collection.mutable.HashMap[String, String] = configurations(clusterId).Props
      return upi.get(key).getOrElse("")
    }
    return ""
  }

  /* TenantId functions */
  def AddTenantInfo(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): Unit = {
    AddTenantInfo(MakeTenantInfo(tenantId, description, primaryDataStore, cacheConfig))
  }

  def AddTenantInfo(tenant : TenantInfo) : Option[String] = {
    // Update the tenantId in this cache, but also see if there was a meaniful change
    // in the existing element - it will not need to be sent to other nodes.
    if (tenantIdMap.contains(tenant.tenantId.trim.toLowerCase)) {
      var isSame = tenant.equals(tenantIdMap.get(tenant.tenantId.trim.toLowerCase).get.asInstanceOf[TenantInfo])
      tenantIdMap(tenant.tenantId.trim.toLowerCase) = tenant
      if (!isSame) return Some("Update") else return None
    } else {
      tenantIdMap(tenant.tenantId.trim.toLowerCase) = tenant
      return Some("Add")
    }
  }

  def UpdateTenantInfo(tenant : TenantInfo) : Boolean = {
    if (tenant.tenantId.trim.equalsIgnoreCase("System") && tenantIdMap.contains("System".toLowerCase)) {
      throw new AlreadyExistsException(s"System is system tenant. you can not repalce it.", null)
    }
    tenantIdMap(tenant.tenantId.toLowerCase) = tenant // Not really checking for existance
    true
  }

  def MakeTenantInfo(tenantId: String, description: String, primaryDataStore: String, cacheConfig: String): TenantInfo = {
    new TenantInfo(tenantId.trim, description, primaryDataStore, cacheConfig)
  }

  def GetTenantInfo(tenantId: String) : TenantInfo = {
    tenantIdMap.getOrElse(tenantId.trim.toLowerCase, null)
  }

  def GetAllTenantInfos : Array[TenantInfo] = {
    tenantIdMap.values.toArray
  }

  def RemoveTenantInfo(tenantId: String): Unit = {
    if (tenantIdMap.contains(tenantId.trim.toLowerCase())) {
      tenantIdMap -= tenantId.trim.toLowerCase()
    }
  }

  /**
      * Construct a SerializeDeserializeConfig instance and add it to the metadata.
      * @param nameSpace the namespace for this SerializeDeserializeConfig
      * @param name its serializer name
      * @param version its serializer version
      * @param serializerType the serializer type
      * @param physicalName the fqClassname that contains the behavior (found in the jarNm)
      * @param ownerId the perpetrator of this serializer
      * @param uniqueId a unique identifier that uniquely describes this object (reserved)
      * @param mdElementId another unique identifer (reserved)
      * @param jarNm the simple jar that is to be loaded in order to use the described SerializeDeserialize implementation
      *              this config describes
      * @param depJars the array of jars that are to be loaded so the SerializeDeserialize implementation this config
      *                describes can function
      * @return Unit
      */
    @throws(classOf[AlreadyExistsException])
    def AddSerializer(nameSpace: String
                      , name: String
                      , version: Long = 1
                      , serializerType: SerializeDeserializeType.SerDeserType
                      , physicalName: String
                      , ownerId: String, tenantId: String
                      , uniqueId: Long
                      , mdElementId: Long
                      , jarNm: String = null
                      , depJars: Array[String] = null): Unit = {
        AddSerializer(MakeSerializer(nameSpace
                    , name
                    , version
                    , serializerType
                    , physicalName
                    , ownerId
                    , tenantId
                    , uniqueId
                    , mdElementId
                    , jarNm
                    , depJars))
    }

    /**
      * Add a SerializeDeserializeConfig instance to the map designated to hold them.
      * @param config the prepared SerializeDeserializeConfig object
      * @return true if the object was added to the map (exception is thrown if one exists with this name)
      */
    @throws(classOf[AlreadyExistsException])
    def AddSerializer(config : SerializeDeserializeConfig) : Boolean = {
        val added : Boolean = if (serializers.contains(config.FullName.toLowerCase)) {
            throw new AlreadyExistsException(s"a SerializeDeserializeConfig already exists with the name ${config.FullName}.", null)
        } else {
            serializers(config.FullName.toLowerCase) = config
            true
        }
        added
    }

    /**
      * Construct a SerializeDeserializeConfig from the supplied arguments.
      * @param nameSpace the namespace for this SerializeDeserializeConfig
      * @param name its serializer name
      * @param version its serializer version
      * @param serializerType the serializer type
      * @param physicalName the fqClassname that contains the behavior (found in the jarNm)
      * @param ownerId the perpetrator of this serializer
      * @param uniqueId a unique identifier that uniquely describes this object (reserved)
      * @param mdElementId another unique identifer (reserved)
      * @param jarNm the simple jar that is to be loaded in order to use the described SerializeDeserialize implementation
      *              this config describes
      * @param depJars the array of jars that are to be loaded so the SerializeDeserialize implementation this config
      *                describes can function
      * @return a SerializeDeserializeConfig
      */
    def MakeSerializer(nameSpace: String
                    , name: String
                    , version: Long = 1
                    , serializerType: SerializeDeserializeType.SerDeserType
                    , physicalName: String
                    , ownerId: String, tenantId: String
                    , uniqueId: Long
                    , mdElementId: Long
                    , jarNm: String = null
                    , depJars: Array[String] = null): SerializeDeserializeConfig = {

        val depJarSet = scala.collection.mutable.Set[String]()

        /** Instantiate the model definition.  Update the base element with basic id information */
        val cfg: SerializeDeserializeConfig = new SerializeDeserializeConfig(serializerType)

        if (depJars != null) depJarSet ++= depJars
        val dJars : Array[String] = if (depJarSet.nonEmpty) depJarSet.toArray else null

        cfg.PhysicalName(physicalName)
        SetBaseElem(cfg, nameSpace, name, version, jarNm, dJars, ownerId, tenantId, uniqueId, mdElementId)

        cfg
    }

    /**
      * Add an adapter message binding to the metadata.  An AdapterMessageBinding describes a triple: the adapter,
      * a message it either consumes or produces, and a serializer that can interpret a stream represention of an
      * instance of this message or produce a serialized representation of same.
      *
      * @param adapterName the name of the adapter that will have this message binding
      * @param namespaceMsgName the message that can be consumed by the specified serializer
      * @param namespaceSerializerName the serializer that can deserialize and serialize the message
      * @param serializerOptions (optional) options used by the serializer to configure itself
      */
    @throws(classOf[AlreadyExistsException])
    def AddAdapterMessageBinding(adapterName: String
                      , namespaceMsgName : String
                      , namespaceSerializerName: String
                      , serializerOptions : scala.collection.immutable.Map[String,Any]
                                 = scala.collection.immutable.Map[String,Any]()): Unit = {

        AddAdapterMessageBinding(MakeAdapterMessageBinding(adapterName
                                                        , namespaceMsgName
                                                        , namespaceSerializerName
                                                        , serializerOptions))
    }

    /**
      * Add a AdapterMessageBinding instance to the map designated to hold them.  The map key is constructed from
      * the triple:
      *
      *     binding.adapterName.binding.messageName.binding.serializer
      *
      * folded to lower case.
      *
      * @param binding the prepared SerializeDeserializeConfig object
      * @return true if the object was added to the map (exception is thrown if one exists with this name)
      */

    @throws(classOf[AlreadyExistsException])
    def AddAdapterMessageBinding(binding : AdapterMessageBinding) : Boolean = {
        val key : String = binding.FullBindingName.toLowerCase
        val added : Boolean = if (adapterMessageBindings.contains(key)) {
            throw AlreadyExistsException(s"an AdapterMessageBinding with key $key already exists... binding could not be added.", null)
        } else {
            adapterMessageBindings(key) = binding
            true
        }
        added
    }

    /**
      * Make an AdapterMessageBinding instance
      * @param adapterName the adapter's name
      * @param namespaceMsgName the message that can be consumed by the specified serializer
      * @param namespaceSerializerName the serializer that can deserialize and serialize the message
      * @param serializerOptions (optional) options that should be used by the serializer to configure itself
      * @return AdapterMessageBinding instance
      */
    def MakeAdapterMessageBinding(  adapterName: String
                                  , namespaceMsgName : String
                                  , namespaceSerializerName: String
                                  , serializerOptions : scala.collection.immutable.Map[String,Any]
                                        = scala.collection.immutable.Map[String,Any]())
                : AdapterMessageBinding = {

        /** Instantiate the AdapterMessageBinding.  Update its base element with basic id information */
        val binding: AdapterMessageBinding = new AdapterMessageBinding(  adapterName
                                                                       , namespaceMsgName
                                                                       , namespaceSerializerName
                                                                       , serializerOptions)
        binding
    }

    /**
      * Remove the AdapterMessageBinding from the metadata answering the removed instance.
      *
      * @param fqBindingName the fully qualified name (adapterName.namespace.msgname.namespace.serializername)
      * @return the removed AdapterMessageBinding (or null if not present).
      */
    def RemoveAdapterMessageBinding(fqBindingName : String) : AdapterMessageBinding = {
        val key : String = fqBindingName.toLowerCase
        val binding = adapterMessageBindings.getOrElse(key, null)
        if (binding != null) {
            adapterMessageBindings -= key
        }
        binding
    }

    /**
      * Answer all of the  AdapaterMessageBindings defined.
      *
      * @return a Map[String, AapterMessageBinding] with 0 or more kv pairs.
      */
    def AllAdapterMessageBindings : scala.collection.immutable.Map[String,AdapterMessageBinding] = {
        adapterMessageBindings.toMap
    }

    /**
      * Answer a map of AdapaterMessageBindings that are used by the supplied adapter name.
      *
      * @param adapterName the adapter name that has the AdapterMessageBinding instances of interest
      * @return a Map[String, AapterMessageBinding] with 0 or more kv pairs.
      */
    def BindingsForAdapter(adapterName : String) : scala.collection.immutable.Map[String,AdapterMessageBinding] = {
        val adapterNameKey = adapterName.trim.toLowerCase + ","
        val bindingMap :  scala.collection.immutable.Map[String,AdapterMessageBinding] = adapterMessageBindings.filterKeys(key => {
            key.startsWith(adapterNameKey)
        }).toMap
        bindingMap
    }

    /**
      * Answer a map of AdapaterMessageBindings that operate on the supplied message name.
      *
      * @param namespaceMsgName the namespace.name of the message of interest
      * @return a Map[String, AapterMessageBinding] with 0 or more kv pairs.
      */
    def BindingsForMessage(namespaceMsgName : String) : scala.collection.immutable.Map[String,AdapterMessageBinding] = {
        val adapterNameKey = "," + namespaceMsgName.trim.toLowerCase + ","
      val bindingMap :  scala.collection.immutable.Map[String,AdapterMessageBinding] = adapterMessageBindings.filterKeys(key => {
            key.contains(adapterNameKey)
        }).toMap
        bindingMap
    }

    /**
      * Answer a map of AdapaterMessageBindings that are used by the serializer with the supplied name.
      *
      * @param namespaceSerializerName the adapter name that has the AdapterMessageBinding instances of interest
      * @return a Map[String, AapterMessageBinding] with 0 or more kv pairs.
      */
    def BindingsUsingSerializer(namespaceSerializerName : String) : scala.collection.immutable.Map[String,AdapterMessageBinding] = {
        val adapterNameKey = "," + namespaceSerializerName.trim.toLowerCase
        val bindingMap :  scala.collection.immutable.Map[String,AdapterMessageBinding] = adapterMessageBindings.filterKeys(key => {
            key.endsWith(adapterNameKey)
        }).toMap
        bindingMap
    }

  def Binding(adapterName: String, messageName : String, serializer : String) : AdapterMessageBinding = {
    val adapName : String = adapterName.trim.toLowerCase
    val msgName : String = messageName.trim.toLowerCase
    val serName : String = serializer.trim.toLowerCase

    val key = s"$adapName,$msgName,$serName"

    adapterMessageBindings.getOrElse(key, null)
  }

    /** Retrieve the SerializeDeserializerConfig with the supplied namespace.name
      *
      * @param fullName the serializer sought
      * @return the corresponding SerializeDeserializeConfig or null if not found
      */
    def GetSerializer(fullName : String) : SerializeDeserializeConfig = {
        serializers.getOrElse(fullName.toLowerCase,null)
    }

    /**
      * Answer an array of the known SerializeDeserializeConfig
      *
      * @return an Array[SerializeDeserializeConfig]
      */
    def GetAllSerializers : Array[SerializeDeserializeConfig] = {
        serializers.values.toArray
    }


  /**
    * GetUserProperty - return a String value of a User Property
    * @param key: String
    */
  def GetUserProperty(key: String): UserPropertiesInfo = {
    configurations.getOrElse(key.toLowerCase(),null)
  }

  def MakeUPProps(clusterId: String): UserPropertiesInfo = {
    var upi = new UserPropertiesInfo
    upi.clusterId = clusterId
    upi.props = new scala.collection.mutable.HashMap[String, String]
    upi
  }

  def MakeCluster(clusterId: String, description: String, privileges: String): ClusterInfo = {
    val ci = new ClusterInfo
    ci.clusterId = clusterId
    ci.description = description
    ci.privileges = privileges
    ci
  }

  def AddCluster(ci: ClusterInfo): Option[String] = {
    if (clusters.contains(ci.clusterId.toLowerCase)) {
      val isSame = clusters.get(ci.clusterId.toLowerCase).get.asInstanceOf[ClusterInfo].equals(ci)
      clusters(ci.clusterId.toLowerCase) = ci
      if (!isSame) return Some("Update") else return None
    } else {
      clusters(ci.clusterId.toLowerCase) = ci
      return Some("Add")
    }
  }

  def GetCluster(clusterId: String): ClusterInfo ={
    return clusters.getOrElse(clusterId.toLowerCase,null)
  }

  def RemoveCluster(clusterId: String): Unit = {
    val ni = clusters.getOrElse(clusterId, null)
    if (ni != null) {
      clusters -= clusterId
    }
  }

  def MakeClusterCfg(clusterId: String, cfgMap: scala.collection.mutable.HashMap[String, String],
                     modifiedTime: Date, createdTime: Date): ClusterCfgInfo = {
    val ci = new ClusterCfgInfo
    ci.clusterId = clusterId
    ci.cfgMap = cfgMap
    ci.usrConfigs = scala.collection.mutable.HashMap[String,String]()
    ci.modifiedTime = modifiedTime
    ci.createdTime = createdTime
    ci
  }

  def AddClusterCfg(ci: ClusterCfgInfo): Option[String] = {
    if (clusterCfgs.contains(ci.clusterId.toLowerCase)) {
      var isSame = clusterCfgs.get(ci.clusterId.trim.toLowerCase).get.asInstanceOf[ClusterCfgInfo].equals(ci)
      clusterCfgs(ci.clusterId.toLowerCase) = ci
      if (!isSame) return Some("Update") else return None
    } else {
      clusterCfgs(ci.clusterId.toLowerCase) = ci
      return Some("Add")
    }
  }

  def GetClusterCfg(key: String): ClusterCfgInfo = {
    return clusterCfgs.getOrElse(key, null)
  }

  def RemoveClusterCfg(clusterCfgId: String): Unit = {
    val ni = clusterCfgs.getOrElse(clusterCfgId, null)
    if (ni != null) {
      clusterCfgs -= clusterCfgId
    }
  }

  def MakeAdapter(name: String, typeString: String, className: String, jarName: String, dependencyJars: List[String], adapterSpecificCfg: String, tenantId: String, fullAdapterConfig: String): AdapterInfo = {
    val ai = new AdapterInfo
    ai.name = name
    ai.typeString = typeString
    ai.className = className
    ai.jarName = jarName
    ai.tenantId = tenantId
    if (dependencyJars != null) {
      ai.dependencyJars = dependencyJars.toArray
    }
    ai.adapterSpecificCfg = adapterSpecificCfg
    ai.fullAdapterConfig = fullAdapterConfig
    ai
  }

  def AddAdapter(ai: AdapterInfo): Option[String] = {
    if (adapters.contains(ai.name.trim.toLowerCase)) {
      var isSame = ai.equals(adapters.get(ai.name.trim.toLowerCase).get.asInstanceOf[AdapterInfo])
      adapters(ai.name.trim.toLowerCase) = ai
      if (!isSame) return Some("Update") else return None
    } else {
      adapters(ai.name.trim.toLowerCase) = ai
      return Some("Add")
    }

  }

  def GetAdapter(adapterName: String): AdapterInfo = {
    val key : String = adapterName.toLowerCase
    val aInfo : AdapterInfo = adapters.getOrElse(key,null)
    aInfo
  }

  def RemoveAdapter(name: String): Unit = {
    val ni = adapters.getOrElse(name, null)
    if (ni != null) {
      adapters -= name
    }
  }

  def getConfigChanges: Array[(String, Any)] = {
    lock.synchronized {
      if (propertyChanged.isEmpty) return new Array[(String, Any)](0)
      val changes =  propertyChanged.toArray
      propertyChanged.clear
      return changes
    }
  }

  def addConfigChange (in: (String, Any)) = {
    lock.synchronized {
      propertyChanged += in
    }
  }

  def Nodes: scala.collection.immutable.Map[String, NodeInfo] = {
    nodes.toMap
  }

  def NodesForCluster(clusterId: String): Array[NodeInfo] = {
    val id: String = if (clusterId != null) clusterId.toLowerCase else null
    val nodesForThisCluster: Array[NodeInfo] = if (id != null) {
      val cNodes: ArrayBuffer[NodeInfo] = ArrayBuffer[NodeInfo]()
      nodes.values.foreach(node => {
        if (id == node.ClusterId.toLowerCase) cNodes += node
      })
      cNodes.toArray
    } else {
      Array[NodeInfo]()
    }
    nodesForThisCluster
  }

  def Adapters: scala.collection.immutable.Map[String, AdapterInfo] = {
    adapters.toMap
  }

  def Clusters: scala.collection.immutable.Map[String, ClusterInfo] = {
    clusters.toMap
  }

  def ClusterCfgs: scala.collection.immutable.Map[String, ClusterCfgInfo] = {
    clusterCfgs.toMap
  }
  // External Functions -- End

}

/*
object MdIdSeq {
  var increment: Long = 1
  var minValue: Long = 1
  var maxValue: Long = 2 ^ 63 - 1
  var cacheSize: Long = 100
  var start: Long = 1

  var current: Long = start
  var consumeLimit: Long = start

  def next: Long = {
    val nxt = current
    current += 1
    nxt
  }
}
*/

trait LogTrait {
  val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
}

object MdMgr extends LogTrait {

  /** Static variables */
  var mdMgr = new MdMgr;
  val sysNS = "System"
  // system name space
  var IdCntr = 0;

  // metadata elem unique id counter; later use proper class and make it persistent

  /**
    * Answer the default metadata manager advertised in this object.
    *
    * @return mdMgr
    */
  def GetMdMgr = mdMgr

  // Reset MdMgr -- Create new MdMgr
  // NOTE NOTE:: All previous hold mdmgr will not update any more
  // Mainly used for test suite
  def ResetMdMgr: Unit = {
    mdMgr = new MdMgr;
  }

  def SysNS = sysNS

  /** Helper function to form a proper search key */
  def MkFullName(nameSpace: String, name: String): String = (nameSpace.trim + "." + name.trim).toLowerCase // Ignoring version for now

  /** Helper function to form a proper search key */
  def MkFullNameWithVersion(nameSpace: String, name: String): String = MkFullNameWithVersion(nameSpace, name, -1)

  /** Helper function to form a proper search key */
  def MkFullNameWithVersion(nameSpace: String, name: String, ver: Long): String = (nameSpace.trim + "." + name.trim + "." + ver).toLowerCase // Ignoring version for now

  private def CheckVerDigits(value: Int, orgVerInfo: String): Unit = {
    if (value < 0 || value > 999999)
      throw new Exception("Expecting only 0 to 999999 in major, minor & micro versions, but got %d from %s".format(value, orgVerInfo))
  }

  // Make sure the version is in the format of "%06d.%06d.%06d"
  def FormatVersion(verInfo: String): String = {
    /*
	    //BUGBUG:: This is returning non found matches, may be better to go with split
		val numPattern = "[0-9]+".r
		val verParts = numPattern.findAllIn(verInfo).toList
	*/
    val verParts = verInfo.split('.')
    val major = (if (verParts.size > 0) verParts(0).toInt else 0)
    val mini = (if (verParts.size > 1) verParts(1).toInt else 0)
    val micro = (if (verParts.size > 2) verParts(2).toInt else 0)

    CheckVerDigits(major, verInfo)
    CheckVerDigits(mini, verInfo)
    CheckVerDigits(micro, verInfo)

    val retVerInfo = "%06d.%06d.%06d".format(major, mini, micro)
    retVerInfo
  }

  /**
    * Convert the supplied version string to a Long.  Should it have '.' in it, they are squeezed out.
    *
    * @param verInfo a version string (possibly with '.' ... e.g., 000000.000001.000001 -> 1000001)
    * @return long formed from decimal digits in the string
    */
  def ConvertVersionToLong(verInfo: String): Long = {
    val hasDots: Boolean = (verInfo != null && verInfo.contains('.'))
    val longVer: Long = if (hasDots) {
      FormatVersion(verInfo).replaceAll("[.]", "").toLong
    } else {
      if (IsNumeric(verInfo)) {
        verInfo.toLong
      } else {
        // oh oh
        0
      }
    }
    longVer
  }

  /** Answer if the string contains only decimal digits
    *
    */
  def IsNumeric(str: String): Boolean = {
    (str != null && str.filter(c => c >= '0' && c <= '9').length == str.length)
  }

  /**
    * Convert the supplied version to a formatted string in three parts with form "%06d.%06d.%06d"
    *
    * @param verInfo  a long version
    * @param withDots if true (the default) string returned is of form "%06d.%06d.%06d"
    * @return a formatted string from the long in the form "%06d.%06d.%06d" or %06d%06d%06d"
    */
  def ConvertLongVersionToString(verInfo: Long, withDots: Boolean = true): String = {
    var remVer = verInfo

    val major = remVer / 1000000000000L // Not expecting more than 6 digits here. Do we need to add check for that?????
    remVer = remVer % 1000000000000L
    val mini = remVer / 1000000L
    val micro = remVer % 1000000L

    val desiredFmt: String = if (withDots) "%06d.%06d.%06d" else "%06d%06d%06d"
    val retVerInfo = desiredFmt.format(major, mini, micro)
    retVerInfo
  }

  /** Answer the unknown version as a string.
    *
    * @return "000000000000000000000"
    */
  def UnknownVersion: String = "000000000000000000000"

  /**
    * Answer the "latest" version key as a string.
    *
    * @return ConvertLongVersionToString(-1)
    */
  def LatestVersion: String = ConvertLongVersionToString(-1)

  /**
    * @deprecated ("Use MdMgr.ConvertLongVersionToString(Long) instead.","2015-Nov-11")
    *
    *             Convert the supplied version to a formatted string in three parts with form "%06d.%06d.%06d"
    * @param verInfo a long version
    * @return a string from the long in the form "%06d.%06d.%06d"
    */
  def Pad0s2Version(verInfo: Long): String = {
    var remVer = verInfo

    val major = remVer / 1000000000000L // Not expecting more than 6 digits here. Do we need to add check for that?????
    remVer = remVer % 1000000000000L
    val mini = remVer / 1000000L
    val micro = remVer % 1000000L

    val retVerInfo = "%06d%06d%06d".format(major, mini, micro)
    retVerInfo
  }

  /**
    * Split a three part name (namespace.name.version) into its respective parts. A null input parameter or strings with
    * less than three '.' delimited nodes produce (null,null,null). The name and version consume the last two '.'
    * delimited nodes with the remaining nodes prefixing them considered the namespace.  That is, the namespace can
    * have multiple '.' delimited nodes in it.
    *
    * For Namespace.Name type nodes, use the SplitFullName(String) function
    *
    * @see SplitFullName(String)
    * @param mdName a Namespace.Name.Version name.
    * @return (namespace, name, version) triple
    */
  def SplitFullNameWithVersion(mdName: String): (String, String, String) = {
    val nameparts = if (mdName != null) mdName.split('.') else null
    val split_names: (String, String, String) = if (nameparts == null || nameparts.length < 3) {
      (null, null, null)
    } else {
      val buffer: StringBuilder = new StringBuilder
      val nameNodes: Array[String] = mdName.split('.')
      val ver: String = nameNodes.last
      val nameNodesSansVer: Array[String] = nameNodes.dropRight(1)
      val name: String = nameNodesSansVer.last
      nameNodesSansVer.take(nameNodesSansVer.size - 1).addString(buffer, ".")
      val nmSpace: String = buffer.toString
      (nmSpace, name, ver)
    }
    split_names
  }

  /** Split a '.' delimited Namespace.Name string into its namespace and name.  The last name is considered as the name and
    * the remaining '.' delimited nodes that prefix it are considered the namespace.  Arguments that are null, have no length
    * or no '.' are rejected and a (null,null) pair is returned.
    *
    * If the name supplied has a version suffix, use SplitFullNameWithVersion(String) instead of this function.
    *
    * @see SplitFullNameWithVersion(String)
    * @param mdName a "full" name (namespace.name) string.
    * @return (namespace, name) pair
    *
    */

  def SplitFullName(mdName: String): (String, String) = {
    val buffer: StringBuilder = new StringBuilder
    val nameNodes: Array[String] = mdName.split('.')
    val modelNm: String = nameNodes.last
    nameNodes.take(nameNodes.size - 1).addString(buffer, ".")
    val modelNmSpace: String = buffer.toString
    (modelNmSpace, modelNm)
  }

}



