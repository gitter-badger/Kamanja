package org.kamanja.serdeser.kbinary

import com.ligadata.BaseTypes._

import scala.reflect.runtime.{universe => ru}

import scala.collection.mutable.{ArrayBuffer, Map }
import scala.collection.JavaConverters._

import java.io._

import org.apache.logging.log4j._

import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MiningModelType
import com.ligadata.kamanja.metadata.ModelRepresentation
import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.Exceptions._
import com.ligadata.KamanjaBase._
import com.ligadata.BaseTypes

/**
  * Meta fields found at the beginning of each JSON representation of a ContainerInterface object
  */
object KBinaryContainerInterfaceKeys extends Enumeration {
    type JsonKeys = Value
    val typename, version, physicalname = Value
}


/**
  * KBinarySerDeser instance can serialize a ContainerInterface to a byte array and deserialize a byte array to form
  * an instance of the ContainerInterface encoded in its bytes.
  *
  * Pre-condition: The JSONSerDes must be initialized with the metadata manager, object resolver and class loader
  * before it can be used.
  */
class KBinarySerDeser extends SerializeDeserialize with LogTrait {

    var _objResolver : ObjectResolver = null
    var _config : Map[String,String] = Map[String,String]()
    var _isReady : Boolean = false

    /**
      * Serialize the supplied container to a byte array using Binary format
      *
      * @param v a ContainerInterface (describes a standard kamanja container)
      */
    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    def serialize(v : ContainerInterface) : Array[Byte] = {
        val bos: ByteArrayOutputStream = new ByteArrayOutputStream(8 * 1024)
        val dos = new DataOutputStream(bos)

        val containerTypeName : String = v.getFullTypeName
        val containerVersion :String = v.getTypeVersion
        // BUGBUG::if type is needed we need function to get type information from object resolver
        //FIXME:- if type is needed we need function to get type information from object resolver
        val container : ContainerTypeDef = null // _mgr.ActiveType(containerTypeName).asInstanceOf[ContainerTypeDef]
        val className : String = container.PhysicalName

        dos.writeUTF(containerTypeName)
        dos.writeUTF(containerVersion)
        dos.writeUTF(className)

        val containerType : ContainerTypeDef = if (container != null) container.asInstanceOf[ContainerTypeDef] else null
        if (containerType == null) {
            throw new ObjectNotFoundException(s"type name $containerTypeName is not a container type... serialize fails.",null)
        }
        val mappedMsgType : MappedMsgTypeDef = if (containerType.isInstanceOf[MappedMsgTypeDef]) containerType.asInstanceOf[MappedMsgTypeDef] else null
        val fixedMsgType : StructTypeDef = if (containerType.isInstanceOf[StructTypeDef]) containerType.asInstanceOf[StructTypeDef] else null
        if (mappedMsgType == null && fixedMsgType == null) {
            throw new UnsupportedObjectException(s"type name $containerTypeName is not a fixed or mapped message container type... serialize fails.",null)
        }

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
            throw new ObjectNotFoundException(s"The container ${containerTypeName} surprisingly has no fields...serialize fails", null)
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

                val typedef: BaseTypeDef = _mgr.ActiveType(valueType)
                val typeName: String = typedef.FullName
                val isContainerType: Boolean = isContainerTypeDef(typedef)
                if (isContainerType) {
                    streamOutContainerBinary(dos, typedef.asInstanceOf[ContainerTypeDef], rawValue)
                } else {
                    streamOutSimpleBinary(dos, typeName, rawValue)
                }
            } else {
                if (mappedMsgType == null) {
                    *//** is permissible that a mapped field has no value in the container. *//*
                } else {
                    *//** blow it up when fixed msg field is missing *//*
                    throw new ObjectNotFoundException(s"The fixed container $containerTypeName field $fldname has no value",null)
                }
            }
        })*/

        val strRep : String = dos.toString
        logger.debug(s"container $containerTypeName as JSON:\n$strRep")

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
      * @param dos the stream to receive the serialized values
      * @param aContainerType the ContainerTypeDef that describes the supplied instance.
      * @param rawValue the actual container instance in raw form
      * @return a Json String representation for the supplied ContainterTypeDef
      */
    @throws(classOf[IOException])
    private def streamOutContainerBinary(dos : DataOutputStream, aContainerType : ContainerTypeDef, rawValue : Any) : Unit = {

        /** ContainerInterface instance? */
        if (rawValue != null) {
            val isContainerInterface: Boolean = rawValue.isInstanceOf[ContainerInterface]
            if (isContainerInterface) {
                val containerBytes: Array[Byte] = serialize(rawValue.asInstanceOf[ContainerInterface])
                dos.writeBytes(new String(containerBytes))
            } else {
                /** Check for collection that is currently supported */
                aContainerType match {
                    case a: ArrayTypeDef => {
                        val array: Array[Any] = rawValue.asInstanceOf[Array[Any]]
                        arrayAsBinary(dos, aContainerType, array)
                    }
                    case m: MapTypeDef => {
                        val map: scala.collection.immutable.Map[Any, Any] = rawValue.asInstanceOf[scala.collection.immutable.Map[Any, Any]]
                        mapAsBinary(dos, aContainerType, map)
                    }
                    case _ => throw new UnsupportedObjectException(s"container type ${aContainerType.typeString} not currently serializable", null)
                }
            }
        }
    }

    /**
      * Serialize the supplied value to the the supplied stream.
      *
      * @param dos the output stream to receive the value
      * @param typename the typename
      * @param value a simple value (scalar, boolean, string, char, etc.)
      */
    @throws(classOf[IOException])
    private def streamOutSimpleBinary(dos : DataOutputStream, typename : String, value : Any) : Unit = {
        if (value != null) {
            typename.toLowerCase match {
                case "int" => {
                    IntImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Int])
                }
                case "long" => {
                    LongImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Long])
                }
                case "float" => {
                    FloatImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Long])
                }
                case "double" => {
                    DoubleImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Long])
                }
                case "boolean" => {
                    BoolImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Boolean])
                }
                case "string" => {
                    StringImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[String])
                }
                case "char" => {
                    CharImpl.SerializeIntoDataOutputStream(dos, value.asInstanceOf[Char])
                }
                case _ => {
                    // Fixme:
                    /**
                      * It should be possible to lookup the class implementation of the supplied type in the
                      * metadata, obtain its physical name (i.e., the fqClassname), and use reflection to instantiate
                      * a SerializeIntoDataOutputStream method, and then invoke it.
                      *
                      * This is not so easily done with the multiple Scala targets (2.10.x and 2.11.x) using the same code base.
                      * The reflection apis are slightly different.
                      *
                      * Perhaps if we insisted that java reflection was used...the base trait currently used should be then
                      * made a java interface (i.e., class TypeImplementation[T] {... methods ....})
                      *
                      * Then code would be:
                      * val aClass : Class = ...//obtain class object
                      * val method : Method  = aClass.getMethod("SerializeIntoDataOutputStream", new Class[]{DataOutputStream.class, aClass});
                      *
                      * Assuming the method is part of a static class then the invoke looks like this:
                      * method.invoke(null, dos, value);
                      */
                    throw new UnsupportedObjectException("Kamanja does not have a serialization implementation for the supplied type '$typename'", null)
                }

            }
        }
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
      * Answer if the supplied BaseTypeDef is a StructTypeDef (used for fixed messages).
      *
      * @param aType a BaseTypeDef
      * @return true if a StructTypeDef
      */
    private def isFixedMsgTypeDef(aType : BaseTypeDef) : Boolean = {
        aType.isInstanceOf[StructTypeDef]
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
      * Create a Json string from the supplied immutable map.  Note that either/both map elements can in turn be
      * containers.  As a pre-condition, the map cannot be null and can have zero to N key/value pairs.
      *
      * @param dos the output stream to receive the value
      * @param aContainerType The container type def for the supplied map
      * @param map the map instance
      * @return Unit
      */
    @throws(classOf[IOException])
    private def mapAsBinary(dos : DataOutputStream, aContainerType : ContainerTypeDef, map : scala.collection.immutable.Map[Any,Any]) : Unit = {
        val memberTypes : Array[BaseTypeDef] = aContainerType.asInstanceOf[MapTypeDef].ElementTypes
        val keyType : BaseTypeDef = memberTypes.head
        val valType : BaseTypeDef = memberTypes.last

        val memberCnt : Int = map.size
        dos.write(memberCnt) /** write the count of the map elements before writing the elements */

        map.foreach(pair => {
            val (keyData, valData): (Any, Any) = pair
            if (keyType.isInstanceOf[ContainerTypeDef]) {
                streamOutContainerBinary(dos, keyType.asInstanceOf[ContainerTypeDef], keyData)
            } else {
                streamOutSimpleBinary(dos, keyType.FullName, valData)
            }

            if (valType.isInstanceOf[ContainerTypeDef]) {
                streamOutContainerBinary(dos, valType.asInstanceOf[ContainerTypeDef], valData)
            } else {
                streamOutSimpleBinary(dos, valType.FullName, valData)
            }
        })
     }

    /**
      * Create a Json string from the supplied array.  Note that the array elements can themselves
      * be containers.  As a pre-condition, the array cannot be null and can have zero to N items.
      *
      * @param dos the output stream to receive the value
      * @param aContainerType The container type def for the supplied array
      * @param array the array instance
      * @return Unit
      */
    @throws(classOf[IOException])
    private def arrayAsBinary(dos : DataOutputStream, aContainerType : ContainerTypeDef, array : Array[Any]) : Unit = {
        val memberTypes : Array[BaseTypeDef] = aContainerType.asInstanceOf[MapTypeDef].ElementTypes
        val itmType : BaseTypeDef = memberTypes.last

        val memberCnt : Int = array.size
        dos.write(memberCnt) /** write the count of the array elements before writing the elements */

        array.foreach(itm => {
            if (itmType.isInstanceOf[ContainerTypeDef]) {
                streamOutContainerBinary(dos, itmType.asInstanceOf[ContainerTypeDef], itm)
            } else {
                streamOutSimpleBinary(dos, itmType.FullName, itm)
            }
        })

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
    def deserialize(b: Array[Byte], containerName: String) : ContainerInterface = {
        var dis = new DataInputStream(new ByteArrayInputStream(b));
        val container : ContainerInterface = deserialize(dis)
        container
    }

    /**
      * Deserialize the supplied byte array into some ContainerInterface instance.
      *
      * @param dis the DataInputStream that contains the container
      * @return a ContainerInterface
      */
    @throws(classOf[com.ligadata.Exceptions.ObjectNotFoundException])
    @throws(classOf[com.ligadata.Exceptions.MissingPropertyException])
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    @throws(classOf[IOException])
    private def deserialize(dis : DataInputStream) : ContainerInterface = {

        val containerTypeName : String = dis.readUTF
        val containerVersion : String = dis.readUTF
        val containerPhyName : String = dis.readUTF

        if (containerTypeName.isEmpty) {
            throw new MissingPropertyException("the supplied byte array to deserialize does not have a known container name.", null)
        }
        if (containerVersion.isEmpty) {
            throw new MissingPropertyException("the supplied byte array to deserialize does not have a version number", null)
        }
        if (containerPhyName.isEmpty) {
            throw new MissingPropertyException("the supplied byte array to deserialize does not have a fully qualified class name.", null)
        }

        /** Fixme: were we to support more than the "current" type, the version key would be used here */

        /** get an empty ContainerInterface instance for this type name from the _objResolver */
        val ci : ContainerInterface = _objResolver.getInstance(containerTypeName)
        if (ci == null) {
            throw new ObjectNotFoundException(s"type name $containerTypeName could not be resolved and built for deserialize",null)
        }

        /** get the fields information */
        // BUGBUG::if type is needed we need function to get type information from object resolver
        //FIXME:- if type is needed we need function to get type information from object resolver
        val containerBaseType : BaseTypeDef = null // _mgr.ActiveType(containerTypeName)
        val containerType : ContainerTypeDef = if (containerBaseType != null) containerBaseType.asInstanceOf[ContainerTypeDef] else null
        if (containerType == null) {
            throw new ObjectNotFoundException(s"type name $containerTypeName is not a container type... deserialize fails.",null)
        }
        val mappedMsgType : MappedMsgTypeDef = if (containerType.isInstanceOf[MappedMsgTypeDef]) containerType.asInstanceOf[MappedMsgTypeDef] else null
        val fixedMsgType : StructTypeDef = if (containerType.isInstanceOf[StructTypeDef]) containerType.asInstanceOf[StructTypeDef] else null
        if (mappedMsgType == null && fixedMsgType == null) {
            throw new UnsupportedObjectException(s"type name $containerTypeName is not a fixed or mapped message container type... deserialize fails.",null)
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
            throw new ObjectNotFoundException(s"The container $containerTypeName surprisingly has no fields...deserialize fails", null)
        }


        /** The fields are considered in the order given in their mapped or fixed message type def collection (the values of the map for the
          * mapped message and the array for the struct def type (fixed))
          */
        fieldsToConsider.foreach(attr => {
            val typeName : String = attr.typeDef.FullName
            val fieldTypeDef : BaseTypeDef = mdMgr.ActiveType(typeName)

            if (fieldTypeDef != null) {
                val isContainerType: Boolean = (attr.typeDef != null && isContainerTypeDef(attr.typeDef))
                val fld: Any = if (isContainerType) {
                    val containerTypeInfo: ContainerTypeDef = attr.typeDef.asInstanceOf[ContainerTypeDef]
                    streamInContainerBinary(dis, containerTypeInfo)
                } else {
                    /** currently assumed to be one of the scalars or simple types supported by json/avro */
                    val itm : Any = streamInSimpleBinary(dis, fieldTypeDef)
                    itm
                }
                ci.set(attr.FullName, fld)
            } else {
                if (mappedMsgType == null) {
                    /** is permissible that a mapped field has no value in the container. */
                } else {
                    /** blow it up when fixed msg field is missing */
                    throw new ObjectNotFoundException(s"The fixed container $containerTypeName field ${attr.typeDef} has no value.. deserialize abandoned",null)
                }
            }
        })

        val container : ContainerInterface = ci
        container
    }

    /**
      * The current json describes one of the ContainerTypeDefs.  Decode the json building the correct container.
      *
      * @param dis the DataInputStream that contains the container type content
      * @param containerTypeInfo a ContainerTypeDef (e.g., a StructTypeDef, MappedMsgTypeDef, ArrayTypeDef, et al)
      * @return
      */
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    @throws(classOf[IOException])
    def streamInContainerBinary(dis : DataInputStream, containerTypeInfo : ContainerTypeDef) : Any = {
        /** ContainerInterface instance? */
        val isContainerInterface : Boolean = containerTypeInfo.isInstanceOf[MappedMsgTypeDef] || containerTypeInfo.isInstanceOf[StructTypeDef]
        val containerInst : Any = if (isContainerInterface) {
            /** recurse to obtain the subcontainer */
            val container : ContainerInterface = deserialize(dis)
            container
        } else { /** Check for collection that is currently supported */
            val coll : Any = containerTypeInfo match {
                case a : ArrayTypeDef =>  {
                    collectArrayFromInputStream(dis, containerTypeInfo)
                }
                case m : MapTypeDef => {
                    collectMutableMapFromInputStream(dis, containerTypeInfo)
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
      * @param dis the DataInputStream that contains the container type content
      * @param arrayTypeInfo the metadata that describes the array
      * @return an array instance
      */
    @throws(classOf[IOException])
    def collectArrayFromInputStream(dis : DataInputStream, arrayTypeInfo : ContainerTypeDef) : Array[Any] = {

        /**
          * FIXME: if we intend to support arrays of hetergeneous items (i.e, Array[Any]), this has to change.  At the
          * moment only arrays of homogeneous types are supported.
          */

        val memberTypes : Array[BaseTypeDef] = arrayTypeInfo.asInstanceOf[MapTypeDef].ElementTypes
        val itmType : BaseTypeDef = memberTypes.last
        val arrayMbrTypeIsContainer : Boolean = itmType.isInstanceOf[ContainerTypeDef]

        val itmCnt : Int = dis.readInt()
        val wrArray : ArrayBuffer[Any] = ArrayBuffer[Any]()
        if (itmCnt > 0) {
            for (idx <- 0 to itmCnt - 1) {
                if (arrayMbrTypeIsContainer) {
                    val container : Any = streamInContainerBinary(dis, itmType.asInstanceOf[ContainerTypeDef])
                    wrArray += container
                } else {
                    val itm : Any = streamInSimpleBinary(dis, itmType)
                    wrArray += itm
                }
            }
        }
        val array : Array[Any] = wrArray.toArray
        array
    }

    /**
      * Coerce the list of mapped elements to an mutable map of the mapped elements' values
      *
      * @param dis the DataInputStream that contains the container type content
      * @param mapTypeInfo the type def for the supplied map
      * @return
      */
    @throws(classOf[IOException])
    def collectMutableMapFromInputStream(dis : DataInputStream, mapTypeInfo : ContainerTypeDef) : scala.collection.mutable.Map[Any,Any] = {

        val memberTypes : Array[BaseTypeDef] = mapTypeInfo.asInstanceOf[MapTypeDef].ElementTypes
        val sanityChk : Boolean = memberTypes.length == 2
        val keyType : BaseTypeDef = memberTypes.head
        val valType : BaseTypeDef = memberTypes.last

        val itmCnt : Int = dis.readInt()
        val wrKeyArray : ArrayBuffer[Any] = ArrayBuffer[Any]()
        val wrValArray : ArrayBuffer[Any] = ArrayBuffer[Any]()

        val mutMap : scala.collection.mutable.Map[Any,Any] = scala.collection.mutable.Map[Any,Any]()

        if (itmCnt > 0) {
            for (idx <- 0 to itmCnt - 1) {
                val newKey : Any = if (keyType.isInstanceOf[ContainerTypeDef]) {
                    val container : Any = streamInContainerBinary(dis, keyType.asInstanceOf[ContainerTypeDef])
                    container
                } else {
                    streamInSimpleBinary(dis, keyType)
                }
                val newVal : Any = if (valType.isInstanceOf[ContainerTypeDef]) {
                    val container : Any = streamInContainerBinary(dis, valType.asInstanceOf[ContainerTypeDef])
                    container
                } else {
                    streamInSimpleBinary(dis, valType)
                }
                mutMap(newKey) = newVal
            }
        } 

        mutMap
    }

    /**
      * Deserialize an object with identified with the supplied type from the input stream.
      *
      * @param dis the input stream to receive the value
      * @param itmType the typename
      * @return instance of the scalar, string, bool, char, that was deserialized from the input stream
      */
    @throws(classOf[com.ligadata.Exceptions.UnsupportedObjectException])
    @throws(classOf[IOException])
    private def streamInSimpleBinary(dis : DataInputStream, itmType : BaseTypeDef) : Any = {
        val simple_type_name : String = if (itmType != null) itmType.Name else null
        val value : Any = if (simple_type_name != null) {
            val simpleValue : Any = simple_type_name.toLowerCase match {
                case "int" => {
                    IntImpl.DeserializeFromDataInputStream(dis)
                }
                case "long" => {
                    LongImpl.DeserializeFromDataInputStream(dis)
                }
                case "float" => {
                    FloatImpl.DeserializeFromDataInputStream(dis)
                }
                case "double" => {
                    DoubleImpl.DeserializeFromDataInputStream(dis)
                }
                case "boolean" => {
                    BoolImpl.DeserializeFromDataInputStream(dis)
                }
                case "string" => {
                    StringImpl.DeserializeFromDataInputStream(dis)
                }
                case "char" => {
                    CharImpl.DeserializeFromDataInputStream(dis)
                }
                case _ => {
                    // Fixme:
                    /**
                      * See comment in streamOutSimpleBinary.  What holds for the serialization case holds for
                      * this deserialization case as well.
                      */
                    throw new UnsupportedObjectException("Kamanja does not have a de-serialization implementation for the supplied type '$typename'", null)
                }
            }
            simpleValue
        }
        value
    }
    
}

