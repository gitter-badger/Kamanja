package com.ligadata.kamanja.serializer

import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Formats, MappingException}

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.JavaConverters._
import java.io.{ByteArrayOutputStream, DataOutputStream}

import com.ligadata.kamanja.metadata._
import com.ligadata.Exceptions._
import com.ligadata.KamanjaBase.AttributeTypeInfo.TypeCategory
import com.ligadata.KamanjaBase.AttributeTypeInfo.TypeCategory._
import com.ligadata.KamanjaBase._
import org.apache.logging.log4j.LogManager

/**
  * Meta fields found at the beginning of each JSON representation of a ContainerInterface object
  */
//@@TODO: move this into utils and use for all logging
object Log {
    private val log = LogManager.getLogger(getClass);

    def Trace(str: String) = if(log.isTraceEnabled())  log.trace(str)
    def Warning(str: String) = if(log.isWarnEnabled()) log.warn(str)
    def Info(str: String) = if(log.isInfoEnabled())    log.info(str)
    def Error(str: String) = if(log.isErrorEnabled())  log.error(str)
    def Debug(str: String) = if(log.isDebugEnabled())  log.debug(str)

    def Trace(str: String, e: Throwable) = if(log.isTraceEnabled())  log.trace(str, e)
    def Warning(str: String, e: Throwable) = if(log.isWarnEnabled()) log.warn(str, e)
    def Info(str: String, e: Throwable) = if(log.isInfoEnabled())    log.info(str, e)
    def Error(str: String, e: Throwable) = if(log.isErrorEnabled())  log.error(str, e)
    def Debug(str: String, e: Throwable) = if(log.isDebugEnabled())  log.debug(str, e)

    def Trace(e: Throwable) = if(log.isTraceEnabled())  log.trace("", e)
    def Warning(e: Throwable) = if(log.isWarnEnabled()) log.warn("", e)
    def Info(e: Throwable) = if(log.isInfoEnabled())    log.info("", e)
    def Error(e: Throwable) = if(log.isErrorEnabled())  log.error("", e)
    def Debug(e: Throwable) = if(log.isDebugEnabled())  log.debug("", e)

    def isTraceEnabled = log.isTraceEnabled()
    def isWarnEnabled = log.isWarnEnabled()
    def isInfoEnabled = log.isInfoEnabled()
    def isErrorEnabled = log.isErrorEnabled()
    def isDebugEnabled = log.isDebugEnabled()
}


object JSONSerDes {
    val indents = ComputeIndents
    val strLF = "\n"
    val maxIndentLevel = 64
    def getIndentStr(indentLevel: Int) = if(indentLevel > maxIndentLevel) indents(maxIndentLevel) else if (indentLevel < 0) indents(0) else indents(indentLevel)

    private
    def ComputeIndents : Array[String] = {
        val indentsTemp = ArrayBuffer[String]()
        indentsTemp.append("")
        val indent = "  "
        for (idx <- 1 to maxIndentLevel) indentsTemp.append(indent+indentsTemp(idx-1))
        indentsTemp.toArray
    }
}

import JSONSerDes._
import Log._
/**
  * JSONSerDeser instance can serialize a ContainerInterface to a byte array and deserialize a byte array to form
  * an instance of the ContainerInterface encoded in its bytes.
  *
  * Pre-condition: The JSONSerDes must be initialized with the metadata manager, object resolver and class loader
  * before it can be used.
  */

class JSONSerDes extends SerializeDeserialize with LogTrait {
    var _objResolver : ObjectResolver = null
    var _config = Map[String,String]()
    var _isReady : Boolean = false
    var _emitSchemaId = true
    var _schemaIdKeyPrefix = "@@"

    def SchemaIDKeyName = _schemaIdKeyPrefix + "SchemaId"
    /**
      * Serialize the supplied container to a byte array
      *
      * @param v a ContainerInterface (describes a standard kamanja container)
      */
    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    def serialize(v : ContainerInterface) : Array[Byte] = {
        val sb = new StringBuilder(8*1024)
        containerAsJson(sb, 0, v)
        val strRep = sb.toString()
        if (isDebugEnabled) {
            Debug(s"Serialized as JSON, data: $strRep")
        }
        strRep.getBytes
    }

    /**
      * Serialize the supplied container to a byte array
      *
      * @param v a ContainerInterface (describes a standard kamanja container)
      */
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    def containerAsJson(sb: StringBuilder, indentLevel: Int, v : ContainerInterface) : Unit = {
        val fields = v.getAllAttributeValues
        val fieldCnt : Int = fields.length

        val indentStr = getIndentStr(indentLevel)
        val schemaId = v.getSchemaId
        val containerJsonHead = indentStr + "{ "
        val containerJsonTail = indentStr + " }"
        sb.append(containerJsonHead)
        if(_emitSchemaId) {
            // sb.append(strLF)
            nameValueAsJson(sb, indentLevel+1, SchemaIDKeyName, schemaId.toString, false)
            if(fieldCnt > 0)
                sb.append(", ")
        }
        var processCnt : Int = 0
        fields.foreach(fld => {
            processCnt += 1
            val valueType = fld.getValueType
            val rawValue: Any = fld.getValue
            val commaSuffix = if (processCnt < fieldCnt) "," else ""
            val quoteValue = useQuotesOnValue(valueType)
            valueType.getTypeCategory match {
                case MAP => { keyAsJson(sb, indentLevel+1, valueType.getName); mapAsJson(sb, indentLevel+1, valueType, rawValue.asInstanceOf[Map[Any, Any]]) }
                case ARRAY => { keyAsJson(sb, indentLevel+1, valueType.getName); arrayAsJson(sb, indentLevel+1, valueType, rawValue.asInstanceOf[Array[Any]]) }
                case (MESSAGE | CONTAINER) => { keyAsJson(sb, indentLevel+1, valueType.getName); containerAsJson(sb, indentLevel+1, rawValue.asInstanceOf[ContainerInterface]) }
                case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => nameValueAsJson(sb, indentLevel+1, valueType.getName, rawValue, quoteValue)
                case _ => throw new UnsupportedObjectException(s"container type ${valueType.getName} not currently serializable", null)
            }
            sb.append(commaSuffix)
        })
        sb.append(containerJsonTail)
    }

    /**
      * Answer a string consisting of "name" : "value" with/without comma suffix.  When quoteValue parameter is false
      * the value is not quoted (for the scalars and boolean
      *
      * @param name json key
      * @param value json value
      * @param quoteValue when true value is quoted
      * @return decorated map element string suitable for including in json map string
      */
    private def nameValueAsJson(sb: StringBuilder, indentLevel: Int, name : String, value : Any, quoteValue: Boolean) = {
        keyAsJson(sb, indentLevel, name)
        valueAsJson(sb, indentLevel, value, quoteValue)
    }

    private def valueAsJson(sb: StringBuilder, indentLevel: Int, value : Any, quoteValue: Boolean)  = {
        val quote = if (quoteValue) s"\\${'"'}" else ""
        // @TODO: need to encode string as proper json string
        sb.append(quote+value+quote)
    }

    /**
      * Answer a string consisting of "name" : "value" with/without comma suffix.  When quoteValue parameter is false
      * the value is not quoted (for the scalars and boolean
      *
      * @param key json key
      */
    private def keyAsJson(sb: StringBuilder, indentLevel: Int, key : String) = sb.append(getIndentStr(indentLevel)+"\""+key+"\": ")


    /**
      * Ascertain if the supplied type is one that does not require quotes. The scalars and boolean do not.
      *
      * @param fieldTypeDef a BaseTypeDef
      * @return true or false if quotes should be used on the json value
      */
    private def useQuotesOnValue(fieldTypeDef : AttributeTypeInfo) : Boolean =  if (fieldTypeDef == null) true else useQuotesOnValue(fieldTypeDef.getTypeCategory)

    private def useQuotesOnValue(typeCategory: TypeCategory) : Boolean = {
        typeCategory match {
            case (MAP | ARRAY | MESSAGE | CONTAINER | BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT ) => false
            case _ => true
        }
    }

    /**
      * Create a Json string from the supplied immutable map.  Note that either/both map elements can in turn be
      * containers.
      *
      * @param attribType The container type def for the supplied map
      * @param map the map instance
      * @return a Json string representation
      */
    private def mapAsJson(sb: StringBuilder, indentLevel: Int, attribType : AttributeTypeInfo, map : scala.collection.mutable.Map[Any,Any]) = {
        val keyType = attribType.getKeyTypeCategory
        val valType = attribType.getValTypeCategory
        val quoteValue = useQuotesOnValue(valType)

        keyType match {
            case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => ;
            case _ => throw new UnsupportedObjectException(s"json serialize doesn't support maps as with complex key types, keyType: ${keyType.name}", null)
        }
        val indentStr = getIndentStr(indentLevel)

        // @TODO: for now, write entire map as a single line.. later it can be done in multi line using the passed in indentation as basis
        val mapJsonHead = "{ "
        val mapJsonTail = " }"
        sb.append(mapJsonHead)
        var idx = 0
        map.foreach(pair => {
            val k = pair._1
            val v = pair._2
            if(idx > 0) sb.append(", ")
            idx += 1
            sb.append(mapJsonHead)
            keyAsJson(sb, 0, k.toString)
            valType match {
                case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => valueAsJson(sb, 0, v, quoteValue);
                case MAP => mapGenericAsJson(sb, indentLevel, v.asInstanceOf[scala.collection.mutable.Map[Any, Any]])
                case ARRAY => arrayGenericAsJson(sb, indentLevel, v.asInstanceOf[Array[Any]])
                case (CONTAINER | MESSAGE) => containerAsJson(sb, 0, v.asInstanceOf[ContainerInterface])
            }
            sb.append(mapJsonTail)
        })
        sb.append(mapJsonTail)
    }

    private def mapGenericAsJson(sb: StringBuilder, indentLevel: Int, map : scala.collection.mutable.Map[Any,Any]) = {
        val indentStr = getIndentStr(indentLevel)
        // @TODO: for now, write entire map as a single line.. later it can be done in multi line using the passed in indentation as basis
        val mapJsonHead = "{ "
        val mapJsonTail = " }"
        sb.append(mapJsonHead)
        var idx = 0
        map.foreach(pair => {
            val k = pair._1
            val v = pair._2
            if(idx > 0) sb.append(", ")
            idx += 1
            sb.append(mapJsonHead)
            keyAsJson(sb, 0, k.toString)
            valueAsJson(sb, 0, v, isInstanceOf[String])
            sb.append(mapJsonTail)
        })
        sb.append(mapJsonTail)
    }

    private def arrayGenericAsJson(sb: StringBuilder, indentLevel: Int, array : Array[Any]) = {
        val indentStr = getIndentStr(indentLevel)
        // @TODO: for now, write entire map as a single line.. later it can be done in multi line using the passed in indentation as basis
        val mapJsonHead = "[ "
        val mapJsonTail = " ]"
        sb.append(mapJsonHead)
        var idx = 0
        array.foreach(elem => {
            if(idx > 0) sb.append(", ")
            idx += 1
            valueAsJson(sb, 0, elem, isInstanceOf[String])
        })
        sb.append(mapJsonTail)
    }

    /**
      * Create a Json string from the supplied array.  Note that the array elements can themselves
      * be containers.
      *
      * @param attribType The container type def for the supplied array
      * @param array the array instance
      * @return a Json string representation
      */
    private def arrayAsJson(sb: StringBuilder, indentLevel: Int, attribType : AttributeTypeInfo, array : Array[Any]) = {
        val itemType = attribType.getValTypeCategory
        val quoteValue = useQuotesOnValue(itemType)
        val mapJsonHead = "[ "
        val mapJsonTail = " ]"
        sb.append(mapJsonHead)
        var idx = 0
        array.foreach(itm => {
            if(idx > 0) sb.append(", ")
            idx += 1
            itemType match {
                case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => valueAsJson(sb, 0, itm, quoteValue);
                case MAP => mapGenericAsJson(sb, indentLevel, itm.asInstanceOf[scala.collection.mutable.Map[Any, Any]])
                case ARRAY => arrayGenericAsJson(sb, indentLevel, itm.asInstanceOf[Array[Any]])
                case (CONTAINER | MESSAGE) => containerAsJson(sb, 0, itm.asInstanceOf[ContainerInterface])
            }
        })
        sb.append(mapJsonTail)
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
        val rawJsonContainerStr: String = new String(b)
        val containerInstanceMap: Map[String, Any] = jsonStringAsMap(rawJsonContainerStr)
        deserializeContainerFromJsonMap(containerInstanceMap)
    }

    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    private def deserializeContainerFromJsonMap(containerInstanceMap : Map[String, Any]) : ContainerInterface = {
        /** Decode the map to produce an instance of ContainerInterface */

        val schemaIdJson = containerInstanceMap.getOrElse(SchemaIDKeyName, "-1").asInstanceOf[Long]

        if (schemaIdJson == -1) {
            throw new MissingPropertyException(s"the supplied map (from json) to deserialize does not have a known schemaid, id: $schemaIdJson", null)
        }
        /** get an empty ContainerInterface instance for this type name from the _objResolver */
        val ci : ContainerInterface = _objResolver.getInstance(schemaIdJson)
        if (ci == null) {
            throw new ObjectNotFoundException(s"container interface with schema id: $schemaIdJson could not be resolved and built for deserialize",null)
        }

        containerInstanceMap.foreach(pair => {
            val k = pair._1
            val v = pair._2

            val at = ci.getAttributeType(k)
            if(at == null)
                ci.set(k, v)
            else {
                // @@TODO: check the type compatibility between "value" field v with the target field
                val valType = at.getValTypeCategory
                val fld = valType match {
                    case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => v
                    case MAP => jsonAsMap(at, v.asInstanceOf[Map[String, Any]])
                    case (CONTAINER | MESSAGE) => deserializeContainerFromJsonMap(v.asInstanceOf[Map[String,Any]])
                    case ARRAY => jsonAsArray(at, v.asInstanceOf[List[Any]])
                }
                ci.set(k, fld)
            }
        })
        ci
    }

    /**
      * Coerce the list of mapped elements to an array of the mapped elements' values
      *
      * @param arrayTypeInfo the metadata that describes the array
      * @param collElements the list of json elements for the array buffer
      * @return an array instance
      */
    def jsonAsArray(arrayTypeInfo : AttributeTypeInfo, collElements : List[Any]) : Array[Any] = {
        /**
          * FIXME: if we intend to support arrays of hetergeneous items (i.e, Array[Any]), this has to change.  At the
          * moment only arrays of homogeneous types are supported.
          */

        val itmType = arrayTypeInfo.getValTypeCategory
        val array : Array[Any] = if (collElements.nonEmpty) {
            val list : List[Any] = collElements.map(itm => {
                val fld = itmType match {
                    case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => itm
                    case MAP => itm.asInstanceOf[Map[String, Any]]
                    case (CONTAINER | MESSAGE) => deserializeContainerFromJsonMap(itm.asInstanceOf[Map[String,Any]])
                    case ARRAY => itm.asInstanceOf[List[Any]].toArray
                    case _ => throw new ObjectNotFoundException(s"jsonAsArray: invalid value type: ${itmType.getValue}, fldName: ${itmType.name} could not be resolved",null)
                }
                fld
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
    def jsonAsMap(mapTypeInfo : AttributeTypeInfo, collElements : Map[String,Any]) : scala.collection.immutable.Map[Any,Any] = {
        val keyType = mapTypeInfo.getKeyTypeCategory
        // check if keyType is STRING or other, for now, only STRING is supported
        val valType = mapTypeInfo.getValTypeCategory
        val map : scala.collection.immutable.Map[Any,Any] = collElements.map(pair => {
            val key : String = pair._1
            val value : Any = pair._2
            val fld = valType match {
                case (BOOLEAN | BYTE | LONG | DOUBLE | FLOAT | INT | STRING) => value
                case MAP => value.asInstanceOf[Map[String, Any]]
                case (CONTAINER | MESSAGE) => deserializeContainerFromJsonMap(value.asInstanceOf[Map[String,Any]])
                case ARRAY => value.asInstanceOf[List[Any]].toArray
                case _ => throw new ObjectNotFoundException(s"jsonAsMap: invalid value type: ${valType.getValue}, fldName: ${valType.name} could not be resolved",null)
            }
            fld
            (key, fld)
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
