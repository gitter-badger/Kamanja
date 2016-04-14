package org.kamanja.serdeser.json

import org.json4s.jackson.JsonMethods._
import org.json4s.{MappingException, DefaultFormats, Formats}
import scala.reflect.runtime.{universe => ru}

import scala.collection.mutable.{ArrayBuffer, Map }
import scala.collection.JavaConverters._

import java.io.{DataInputStream, ByteArrayInputStream, DataOutputStream, ByteArrayOutputStream}

import org.apache.logging.log4j._
import com.fasterxml.jackson.databind.ObjectMapper

import com.ligadata.kamanja.metadata.MiningModelType
import com.ligadata.kamanja.metadata.ModelRepresentation
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata.MdMgr._

import com.ligadata.Exceptions._
import com.ligadata.KamanjaBase._

import scala.reflect.runtime._

/**
  * Meta fields found at the beginning of each JSON representation of a ContainerInterface object
  */
object JsonContainerInterfaceKeys extends Enumeration {
    type JsonKeys = Value
    val typename, version, physicalname = Value
}


/**
  * JSONSerDeser instance can serialize a ContainerInterface to a byte array and deserialize a byte array to form
  * an instance of the ContainerInterface encoded in its bytes.
  *
  * Pre-condition: The JSONSerDes must be initialized with the metadata manager, object resolver and class loader
  * before it can be used.
  */

class JSONSerDes extends SerializeDeserialize with LogTrait {

    var _objResolver : ObjectResolver = null
    var _config : Map[String, String] = Map[String,String]()
    var _isReady : Boolean = false

    /**
      * Serialize the supplied container to a byte array
      *
      * @param v a ContainerInterface (describes a standard kamanja container)
      */
    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    def serialize(v : ContainerInterface) : Array[Byte] = {
        val bos: ByteArrayOutputStream = new ByteArrayOutputStream(8 * 1024)
        val dos = new DataOutputStream(bos)

        val withComma : Boolean = true
        val withoutComma :Boolean = false
        val containerName : String = v.getFullTypeName
        val containerVersion :String = v.getTypeVersion
        // BUGBUG::if type is needed we need function to get type information from object resolver
        //FIXME:- if type is needed we need function to get type information from object resolver
        val container : ContainerTypeDef = null; // _mgr.ActiveType(containerName).asInstanceOf[ContainerTypeDef]
        val className : String = container.PhysicalName

        val containerJsonHead : String = "{ "
        val containerJsonTail : String = " }"
        val containerNameJson : String = nameValueAsJson(JsonContainerInterfaceKeys.typename.toString, containerName, true, withComma)
        val containerVersionJson : String = nameValueAsJson(JsonContainerInterfaceKeys.version.toString, containerVersion,  true, withComma)
        val containerPhyNameJson : String = nameValueAsJson(JsonContainerInterfaceKeys.physicalname.toString, className,  true, withComma)

        dos.writeUTF(containerJsonHead)
        dos.writeUTF(containerNameJson)
        dos.writeUTF(containerVersionJson)
        dos.writeUTF(containerPhyNameJson)

        val containerType : ContainerTypeDef = if (container != null) container.asInstanceOf[ContainerTypeDef] else null
        if (containerType == null) {
            throw new ObjectNotFoundException(s"type name $containerName is not a container type... serialize fails.",null)
        }
        val mappedMsgType : MappedMsgTypeDef = if (containerType.isInstanceOf[MappedMsgTypeDef]) containerType.asInstanceOf[MappedMsgTypeDef] else null
        val fixedMsgType : StructTypeDef = if (containerType.isInstanceOf[StructTypeDef]) containerType.asInstanceOf[StructTypeDef] else null
        if (mappedMsgType == null && fixedMsgType == null) {
            throw new UnsupportedObjectException(s"type name $containerNameJson is not a fixed or mapped message container type... serialize fails.",null)
        }

        /** Fixme: were we to support more than the "current" type, the version key above would be used to discern which type is to be deserialized */

        /**
          * Note:
          * The fields from the ContainerInstance are unordered, a java.util.HashMap.  Similarly the fields found in a
          * a MappedMsg are unordered.
          *
          * On the other hand, the fields from a FixedMsg are ordered.
          *
          * The fields will be processed in the order of the StructTypeDef's memberDefs array for fixed messages. For
          * the fixed ones, all of the ContainerInterface's fields will be emitted.
          *
          * The fields will be processed in the order of the MappedMsgTypeDef's attrMap map for mapped messages. For
          * these mapped ones, only the ContainerInterface's fields that have values will be emitted.
          *
          */
        val fieldsToConsider : Array[BaseAttributeDef] = if (mappedMsgType != null) {
            mappedMsgType.attrMap.values.toArray
        } else {
            if (fixedMsgType != null) {
                fixedMsgType.memberDefs
            } else {
                Array[BaseAttributeDef]()
            }
        }
        if (fieldsToConsider.isEmpty) {
            throw new ObjectNotFoundException(s"The container ${containerName} surprisingly has no fields...serialize fails", null)
        }

       /* val fields : java.util.HashMap[String,com.ligadata.KamanjaBase.AttributeValue] = v.getAllAttributeValues
        var processCnt : Int = 0
        val fieldCnt : Int = fields.size()
        fieldsToConsider.foreach(fldname => {
            processCnt += 1
            val attr : com.ligadata.KamanjaBase.AttributeValue = fields.get(fldname)
            if (attr != null) {
                val valueType: String = attr.getValueType
                val rawValue: Any = attr.getValue
                val useComma: Boolean = if (processCnt < fieldCnt) withComma else withoutComma

                val typedef: BaseTypeDef = _mgr.ActiveType(valueType)
                val typeName: String = typedef.FullName
                val isContainerType: Boolean = isContainerTypeDef(typedef)
                val fldRep: String = if (isContainerType) {
                    processContainerTypeAsJson(typedef.asInstanceOf[ContainerTypeDef]
                        , rawValue
                        , useComma)
                } else {
                    val quoteValue: Boolean = useQuotesOnValue(typedef)
                    nameValueAsJson(typeName, rawValue, quoteValue, useComma)
                }
                dos.writeBytes(fldRep)
            } else {
                if (mappedMsgType == null) {
                    *//** is permissible that a mapped field has no value in the container. *//*
                } else {
                    *//** blow it up when fixed msg field is missing *//*
                    throw new ObjectNotFoundException(s"The fixed container $containerName field $fldname has no value",null)
                }
            }
        })*/
        dos.writeUTF(containerJsonTail)

        val strRep : String = dos.toString
        logger.debug(s"container $containerName as JSON:\n$strRep")

        val byteArray : Array[Byte] = bos.toByteArray
        dos.close()
        bos.close()
        byteArray
    }

    /**
      * Process the supplied ContainerTypeDef.  There are essentially two paths through this method.
      * One path recognizes the container to be MappedMsgTypeDef or StructTypeDef (i.e., instanceOf[ContainerInterface]).
      * In that event there is recursion back to the serialize(v : ContainerInterface) method.
      *
      * The other path pertains to the Array and Map type Containers.  Those with simple scalar types (scalars, String, Boolean)
      * are handled locally.  Array elements that have ContainerTypeDef elements are iterated and recursion happens
      * for each element.  For maps, if either the key or value member type is a ContainerTypeDef, each is similarly
      * managed.
      *
      * Fixme: There is no reason that Stacks, Queues and other data types we support cannot be implemented here.  For
      * now just Array, ArrayBuffer, Map, and ImmutableMap
      *
      * @param aContainerType the ContainerTypeDef that describes the supplied instance.
      * @param rawValue the actual container instance in raw form
      * @param withComma when true, a ',' is appended to the ContainerTypeDef Json representation
      * @return a Json String representation for the supplied ContainterTypeDef
      */
    private def processContainerTypeAsJson(aContainerType : ContainerTypeDef
                                         , rawValue : Any
                                         , withComma : Boolean) : String = {
        /** ContainerInterface instance? */
        val isContainerInterface : Boolean = rawValue.isInstanceOf[ContainerInterface]
        val stringRep : String = if (isContainerInterface) {
            val containerBytes : Array[Byte] = serialize(rawValue.asInstanceOf[ContainerInterface])
            val containerStr : String = new String(containerBytes)
            containerStr
        } else { /** Check for collection that is currently supported */
            val strrep : String = aContainerType match {
                case a : ArrayTypeDef =>  {
                    val array : Array[Any] = rawValue.asInstanceOf[Array[Any]]
                    arrayAsJson(aContainerType, array)
                }
                case m : MapTypeDef => {
                    val map : scala.collection.immutable.Map[Any,Any] = rawValue.asInstanceOf[scala.collection.immutable.Map[Any,Any]]
                    mapAsJson(aContainerType, map)
                }
                case _ => throw new UnsupportedObjectException(s"container type ${aContainerType.typeString} not currently serializable",null)
            }

            strrep
        }
        stringRep
    }

    /**
      * Answer if the supplied BaseTypeDef is a ContainerTypeDef.
      *
      * @param aType a BaseTypeDef
      * @return true if a ContainerTypeDef
      */
    private def isContainerTypeDef(aType : BaseTypeDef) : Boolean = {
        aType.isInstanceOf[ContainerTypeDef]
    }

    /**
      * Answer if the supplied BaseTypeDef is a MappedMsgTypeDef.
      *
      * @param aType a BaseTypeDef
      * @return true if a MappedMsgTypeDef
      */
    private def isMappedMsgTypeDef(aType : BaseTypeDef) : Boolean = {
        aType.isInstanceOf[MappedMsgTypeDef]
    }

    /**
      * Answer if the supplied BaseTypeDef is a StructTypeDef (used for fixed messages).
      *
      * @param aType a BaseTypeDef
      * @return true if a StructTypeDef
      */
    private def isFixedMsgTypeDef(aType : BaseTypeDef) : Boolean = {
        aType.isInstanceOf[StructTypeDef]
    }

    /**
      * Answer a string consisting of "name" : "value" with/without comma suffix.  When quoteValue parameter is false
      * the value is not quoted (for the scalars and boolean
      *
      * @param name json key
      * @param value json value
      * @param quoteValue when true value is quoted
      * @param withComma when true suffix string with comma
      * @return decorated map element string suitable for including in json map string
      */
    private def nameValueAsJson(name : String, value : Any, quoteValue : Boolean, withComma : Boolean) : String = {
        val comma : String = if (withComma) "," else ""
        val quote : String = if (quoteValue) s"\\${'"'}" else ""
        s" {'\'}${'"'}$name{'\'}${'"'} : $quote$value$quote$comma"
    }


    /**
      * Ascertain if the supplied type is one that does not require quotes. The scalars and boolean do not.
      *
      * @param fieldTypeDef a BaseTypeDef
      * @return true or false if quotes should be used on the json value
      */
    private def useQuotesOnValue(fieldTypeDef : BaseTypeDef) : Boolean = {
        val usequotes : Boolean = if (fieldTypeDef == null) {
            true
        } else {
            /** This does not account for user defined versions of the scalars */
            fieldTypeDef.Name.toLowerCase match {
                case "int" | "integer" | "short" | "long" | "float" | "double" => false
                case "boolean" => false
                case _ => true
            }
        }
        usequotes
    }

    /**
      * Create a Json string from the supplied immutable map.  Note that either/both map elements can in turn be
      * containers.
      *
      * @param aContainerType The container type def for the supplied map
      * @param map the map instance
      * @return a Json string representation
      */
    private def mapAsJson(aContainerType : ContainerTypeDef, map : scala.collection.immutable.Map[Any,Any]) : String = {
        val memberTypes : Array[BaseTypeDef] = aContainerType.asInstanceOf[MapTypeDef].ElementTypes
        val keyType : BaseTypeDef = memberTypes.head
        val valType : BaseTypeDef = memberTypes.last
        val itemAtATime : Boolean = (keyType.isInstanceOf[ContainerTypeDef] || valType.isInstanceOf[ContainerTypeDef])
        val mapAsJsonStr : String = if (itemAtATime) {
            val buffer : StringBuilder = new StringBuilder
            val lastVal : Any  = map.values.last
            val noComma : Boolean = false
            map.foreach(pair => {
                val keyRep : String =  if (keyType.isInstanceOf[ContainerTypeDef]) {
                    processContainerTypeAsJson(keyType.asInstanceOf[ContainerTypeDef], pair._1, noComma)
                } else {
                    val quoteValue : Boolean = true
                    nameValueAsJson(keyType.FullName, pair._1, quoteValue, noComma)
                }
                val printComma : Boolean = lastVal != pair._2
                val valRep : String =  if (valType.isInstanceOf[ContainerTypeDef]) {
                    processContainerTypeAsJson(valType.asInstanceOf[ContainerTypeDef], pair._2, printComma)
                } else {
                    val quoteValue : Boolean = useQuotesOnValue(valType)
                    nameValueAsJson(valType.FullName, pair._2, quoteValue, printComma)
                }
                buffer.append(s"$keyRep : $valRep")
            })
            buffer.toString
        } else {
            new ObjectMapper().writeValueAsString(map)
        }
        mapAsJsonStr
    }

    /**
      * Create a Json string from the supplied array.  Note that the array elements can themselves
      * be containers.
      *
      * @param aContainerType The container type def for the supplied array
      * @param array the array instance
      * @return a Json string representation
      */
    private def arrayAsJson(aContainerType : ContainerTypeDef, array : Array[Any]) : String = {
        val memberTypes : Array[BaseTypeDef] = aContainerType.asInstanceOf[ArrayTypeDef].ElementTypes
        val itmType : BaseTypeDef = memberTypes.last
        val itemAtATime : Boolean = itmType.isInstanceOf[ContainerTypeDef]
        val arrAsJsonStr : String = if (itemAtATime) {
            val buffer : StringBuilder = new StringBuilder
            val lastItm : Any  = array.last
            buffer.append("[ ")
            array.foreach(itm => {
                val printComma : Boolean = lastItm != itm
                val itmRep : String =  if (itmType.isInstanceOf[ContainerTypeDef]) {
                    processContainerTypeAsJson(itmType.asInstanceOf[ContainerTypeDef], itm, printComma)
                } else {
                    val quoteValue : Boolean = useQuotesOnValue(itmType)
                    nameValueAsJson(itmType.FullName, itm, quoteValue, printComma)
                }
                buffer.append(s"$itm")
            })
            buffer.append(" ]")
            buffer.toString
        } else {
            new ObjectMapper().writeValueAsString(array)
        }
        arrAsJsonStr
    }


    /**
      * Set the object resolver to be used for this serializer
 *
      * @param objRes an ObjectResolver
      */
    def setObjectResolver(objRes : ObjectResolver) : Unit = {
        _objResolver = objRes
    }

    /**
      * Configure the SerializeDeserialize adapter.  This must be done before the adapter implementation can be used.
      *
      * @param objResolver the ObjectResolver instance that can instantiate ContainerInterface instances
      * @param config a map of options that might be used to configure the execution of this SerializeDeserialize instance. This may
      *               be null if it is not necessary for the SerializeDeserialize implementation
      */
    def configure(objResolver: ObjectResolver, config : java.util.Map[String,String]): Unit = {
        _objResolver = objResolver
        _config = if (config != null) config.asScala else Map[String,String]()
        _isReady = _objResolver != null && _config != null
    }

    /**
      * Deserialize the supplied byte array into some ContainerInterface instance.
      *
      * @param b the byte array containing the serialized ContainerInterface instance
      * @return a ContainerInterface
      */
    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    def deserialize(b: Array[Byte], containerName: String) : ContainerInterface = {

        val rawJsonContainerStr : String = new String(b)
        val containerInstanceMap : Map[String, Any] = jsonStringAsMap(rawJsonContainerStr)

        /** Decode the map to produce an instance of ContainerInterface */

        /** get the container key information.. the top level object must be a ContainerInterface... if these
          * are not present, nothing good will come of it */
        val containerNameJson : String = containerInstanceMap.getOrElse(JsonContainerInterfaceKeys.typename.toString, "").asInstanceOf[String]
        val containerVersionJson : String = containerInstanceMap.getOrElse(JsonContainerInterfaceKeys.version.toString, "").asInstanceOf[String]
        val containerPhyNameJson : String = containerInstanceMap.getOrElse(JsonContainerInterfaceKeys.physicalname.toString, "").asInstanceOf[String]

        if (containerNameJson.isEmpty) {
            throw new MissingPropertyException("the supplied byte array to deserialize does not have a known container name.", null)
        }

        /** Fixme: were we to support more than the "current" type, the version key above would be used to discern which type is to be deserialized */

        /** get an empty ContainerInterface instance for this type name from the _objResolver */
        val ci : ContainerInterface = _objResolver.getInstance(containerNameJson)
        if (ci == null) {
            throw new ObjectNotFoundException(s"type name $containerNameJson could not be resolved and built for deserialize",null)
        }

        /** get the fields information */
            // BUGBUG::if type is needed we need function to get type information from object resolver
            //FIXME:- if type is needed we need function to get type information from object resolver
        val containerBaseType : BaseTypeDef = null // _mgr.ActiveType(containerNameJson)
        val containerType : ContainerTypeDef = if (containerBaseType != null) containerBaseType.asInstanceOf[ContainerTypeDef] else null
        if (containerType == null) {
            throw new ObjectNotFoundException(s"type name $containerNameJson is not a container type... deserialize fails.",null)
        }
        val mappedMsgType : MappedMsgTypeDef = if (containerType.isInstanceOf[MappedMsgTypeDef]) containerType.asInstanceOf[MappedMsgTypeDef] else null
        val fixedMsgType : StructTypeDef = if (containerType.isInstanceOf[StructTypeDef]) containerType.asInstanceOf[StructTypeDef] else null
        if (mappedMsgType == null && fixedMsgType == null) {
            throw new UnsupportedObjectException(s"type name $containerNameJson is not a fixed or mapped message container type... deserialize fails.",null)
        }

        val fieldsToConsider : Array[BaseAttributeDef] = if (mappedMsgType != null) {
            mappedMsgType.attrMap.values.toArray
        } else {
            if (fixedMsgType != null) {
                fixedMsgType.memberDefs
            } else {
                Array[BaseAttributeDef]()
            }
        }
        if (fieldsToConsider.isEmpty) {
            throw new ObjectNotFoundException(s"The container $containerNameJson surprisingly has no fields...deserialize fails", null)
        }


        /** The fields are considered in the order given in their mapped or fixed message type def collection (the values of the map for the
          * mapped message and the array for the struct def type (fixed))
          */
        fieldsToConsider.foreach(attr => {

            val fieldsJson : Any = containerInstanceMap.getOrElse(attr.FullName, null)
            if (fieldsJson != null) {
                val isContainerType: Boolean = (attr.typeDef != null && isContainerTypeDef(attr.typeDef))
                val fld: Any = if (isContainerType) {
                    val containerTypeInfo: ContainerTypeDef = attr.typeDef.asInstanceOf[ContainerTypeDef]
                    createContainerType(containerTypeInfo, fieldsJson)
                } else {
                    /** currently assumed to be one of the scalars or simple types supported by json/avro */
                    fieldsJson
                }
                ci.set(attr.FullName, fld)
            } else {
                if (mappedMsgType == null) {
                    /** is permissible that a mapped field has no value in the container. */
                } else {
                    /** blow it up when fixed msg field is missing */
                    throw new ObjectNotFoundException(s"The fixed container $containerNameJson field ${attr.typeDef} has no value.. deserialize abandoned",null)
                }
            }
        })

        val container : ContainerInterface = null
        container
    }

    /**
      * The current json describes one of the ContainerTypeDefs.  Decode the json building the correct container.
      *
      * @param containerTypeInfo a ContainerTypeDef (e.g., a StructTypeDef, MappedMsgTypeDef, ArrayTypeDef, et al)
      * @param fieldsJson the json container (a map or array) that contains the content
      * @return
      */
    def createContainerType(containerTypeInfo : ContainerTypeDef, fieldsJson : Any) : Any = {
        /** ContainerInterface instance? */
        val isContainerInterface : Boolean = containerTypeInfo.isInstanceOf[MappedMsgTypeDef] || containerTypeInfo.isInstanceOf[StructTypeDef]
        val containerInst : Any = if (isContainerInterface) {
            /** recurse to obtain the subcontainer */
            val containerBytes : Array[Byte] = fieldsJson.toString.toCharArray.map(_.toByte)
            val container : ContainerInterface = deserialize(containerBytes, null) // BUGBUG:: FIX this type string
            container
        } else { /** Check for collection that is currently supported */

            val coll : Any = containerTypeInfo match {
                case a : ArrayTypeDef =>  {
                    val collElements : List[Map[String, Any]] = fieldsJson.asInstanceOf[List[Map[String, Any]]]
                    jsonAsArray(containerTypeInfo, collElements)
                }
                case m : MapTypeDef => {
                    val collElements : Map[String, Any] = fieldsJson.asInstanceOf[Map[String, Any]]
                    jsonAsMap(containerTypeInfo, collElements)
                }
                case _ => throw new UnsupportedObjectException(s"container type ${containerTypeInfo.typeString} not currently serializable",null)
            }

            coll
        }
        containerInst
    }

    /**
      * Coerce the list of mapped elements to an array of the mapped elements' values
      *
      * @param arrayTypeInfo the metadata that describes the array
      * @param collElements the list of json elements for the array buffer
      * @return an array instance
      */
    def jsonAsArray(arrayTypeInfo : ContainerTypeDef, collElements : List[Map[String,Any]]) : Array[Any] = {

        /**
         * FIXME: if we intend to support arrays of hetergeneous items (i.e, Array[Any]), this has to change.  At the
         * moment only arrays of homogeneous types are supported.
         */

        val memberTypes : Array[BaseTypeDef] = arrayTypeInfo.asInstanceOf[MapTypeDef].ElementTypes
        val itmType : BaseTypeDef = memberTypes.last
        val arrayMbrTypeIsContainer : Boolean = itmType.isInstanceOf[ContainerTypeDef]
        val array : Array[Any] = if (collElements.size > 0) {
            val list : List[Any] = collElements.map(itm => {
                if (arrayMbrTypeIsContainer) {
                    val itmType : ContainerTypeDef = itm.asInstanceOf[ContainerTypeDef]
                    createContainerType(itmType, itm)
                } else {
                    itm
                }
            })
            list.toArray
        } else {
            Array[Any]()
        }
        array
    }

    /**
      * Coerce the list of mapped elements to an immutable map of the mapped elements' values
      *
      * @param mapTypeInfo
      * @param collElements
      * @return
      */
    def jsonAsMap(mapTypeInfo : ContainerTypeDef, collElements : Map[String,Any]) : scala.collection.immutable.Map[Any,Any] = {
        val memberTypes : Array[BaseTypeDef] = mapTypeInfo.asInstanceOf[MapTypeDef].ElementTypes
        val sanityChk : Boolean = memberTypes.length == 2
        val keyType : BaseTypeDef = memberTypes.head
        val valType : BaseTypeDef = memberTypes.last
        val map : scala.collection.immutable.Map[Any,Any] = collElements.map(pair => {
            val key : String = pair._1
            val value : Any = pair._2
            val keyRep : Any =  if (keyType.isInstanceOf[ContainerTypeDef]) {
                val itmType : ContainerTypeDef = key.asInstanceOf[ContainerTypeDef]
                createContainerType(itmType, key)
            } else {
                key
            }

            val valRep : Any =  if (valType.isInstanceOf[ContainerTypeDef]) {
                val itmType : ContainerTypeDef = value.asInstanceOf[ContainerTypeDef]
                createContainerType(itmType, value)
            } else {
                value
            }
            (keyRep,valRep)
        }).toMap

        map
    }

    /**
      * Translate the supplied json string to a Map[String, Any]
      *
      * @param configJson
      * @return Map[String, Any]
      */

    @throws(classOf[com.ligadata.Exceptions.Json4sParsingException])
    @throws(classOf[com.ligadata.Exceptions.EngineConfigParsingException])
    def jsonStringAsMap(configJson: String): Map[String, Any] = {
        try {
            implicit val jsonFormats: Formats = DefaultFormats
            val json = parse(configJson)
            logger.debug("Parsed the json : " + configJson)

            val fullmap = json.values.asInstanceOf[Map[String, Any]]

            fullmap
        } catch {
            case e: MappingException => {
                logger.debug("", e)
                throw Json4sParsingException(e.getMessage, e)
            }
            case e: Exception => {
                logger.debug("", e)
                throw EngineConfigParsingException(e.getMessage, e)
            }
        }
    }



}

