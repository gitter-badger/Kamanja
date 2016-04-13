
package com.ligadata.messages.V1000000;

import org.json4s.jackson.JsonMethods._;
import org.json4s.DefaultFormats;
import org.json4s.Formats;
import com.ligadata.KamanjaBase._;
import com.ligadata.BaseTypes._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date;
import java.io.{ DataInputStream, DataOutputStream, ByteArrayOutputStream }

object HL7Fixed extends RDDObject[HL7Fixed] with MessageFactoryInterface {
  type T = HL7Fixed;
  override def getFullTypeName: String = "com.ligadata.messages.HL7Fixed";
  override def getTypeNameSpace: String = "com.ligadata.messages";
  override def getTypeName: String = "HL7Fixed";
  override def getTypeVersion: String = "000000.000001.000000";
  override def getSchemaId: Int = 0;
  override def createInstance: HL7Fixed = new HL7Fixed(HL7Fixed);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerTypes.ContainerType = ContainerTypes.ContainerType.MESSAGE
  override def getFullName = getFullTypeName;
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);

  def build = new T(this)
  def build(from: T) = new T(from)
  override def getPartitionKeyNames: Array[String] = Array("desynpuf_id");

  override def getPrimaryKeyNames: Array[String] = Array("desynpuf_id", "clm_id");

  override def getTimePartitionInfo: TimePartitionInfo = { return null; } // FieldName, Format & Time Partition Types(Daily/Monthly/Yearly)

  override def hasPrimaryKey(): Boolean = {
    val pKeys = getPrimaryKeyNames();
    return (pKeys != null && pKeys.length > 0);
  }

  override def hasPartitionKey(): Boolean = {
    val pKeys = getPartitionKeyNames();
    return (pKeys != null && pKeys.length > 0);
  }

  override def hasTimePartitionInfo(): Boolean = {
    val tmInfo = getTimePartitionInfo();
    return (tmInfo != null && tmInfo.getTimePartitionType != TimePartitionInfo.TimePartitionType.NONE);
  }

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.messages" , "name" : "hl7fixed" , "fields":[{ "name" : "desynpuf_id" , "type" : "string"},{ "name" : "clm_id" , "type" : "long"},{ "name" : "clm_from_dt" , "type" : "int"},{ "name" : "chestpain" , "type" : "string"},{ "name" : "aatdeficiency" , "type" : "int"},{ "name" : "chroniccough" , "type" : "int"},{ "name" : "chronicsputum" , "type" : "int"}]}""";

  override def FullName: String = getFullTypeName
  override def NameSpace: String = getTypeNameSpace
  override def Name: String = getTypeName
  override def Version: String = getTypeVersion
  override def CreateNewMessage: BaseMsg = createInstance.asInstanceOf[BaseMsg];
  override def CreateNewContainer: BaseContainer = null;
  override def IsFixed: Boolean = true
  override def IsKv: Boolean = false
  override def CanPersist: Boolean = true
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  override def PartitionKeyData(inputdata: InputData): Array[String] = createInstance.getPartitionKey();
  override def PrimaryKeyData(inputdata: InputData): Array[String] = createInstance.getPrimaryKey();
  override def TimePartitionData(inputdata: InputData): Long = createInstance.getTimePartitionData;
  override def NeedToTransformData: Boolean = false
}

class HL7Fixed(factory: MessageFactoryInterface, other: HL7Fixed) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)

  var attributeTypes = generateAttributeTypes;

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](7);
    attributeTypes(0) = new AttributeTypeInfo("desynpuf_id", 0, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes(1) = new AttributeTypeInfo("clm_id", 1, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(2) = new AttributeTypeInfo("clm_from_dt", 2, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(3) = new AttributeTypeInfo("chestpain", 3, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes(4) = new AttributeTypeInfo("aatdeficiency", 4, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(5) = new AttributeTypeInfo("chroniccough", 5, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(6) = new AttributeTypeInfo("chronicsputum", 6, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)

    return attributeTypes
  }

  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* HL7Fixed.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { HL7Fixed.build(this) }

  override def getPartitionKey: Array[String] = {
    var partitionKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      partitionKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").asInstanceOf[String]);
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };
    partitionKeys.toArray;

  }

  override def getPrimaryKey: Array[String] = {
    var primaryKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      primaryKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").asInstanceOf[String]);
      primaryKeys += com.ligadata.BaseTypes.LongImpl.toString(get("clm_id").asInstanceOf[Long]);
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };
    primaryKeys.toArray;

  }

  override def getAttributeType(name: String): AttributeTypeInfo = {
    if (name == null || name.trim() == "") return null;
    attributeTypes.foreach(attributeType => {
      if (attributeType.getName == name.toLowerCase())
        return attributeType
    })
    return null;
  }

  var desynpuf_id: String = _;
  var clm_id: Long = _;
  var clm_from_dt: Int = _;
  var chestpain: String = _;
  var aatdeficiency: Int = _;
  var chroniccough: Int = _;
  var chronicsputum: Int = _;

  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    if (attributeTypes == null) return null;
    return attributeTypes
  }

  private def getWithReflection(key: String): AnyRef = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[HL7Fixed].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    return fmX.get.asInstanceOf[AnyRef];
  }

  override def get(key: String): AnyRef = {
    try {
      // Try with reflection
      return getByName(key.toLowerCase())
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        // Call By Name
        return getWithReflection(key.toLowerCase())
      }
    }
  }

  private def getByName(key: String): AnyRef = {
    if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message/container hl7Fixed ");
    return get(keyTypes(key).getIndex)
  }

  override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return (value, type)
    try {
      val value = get(key.toLowerCase())
      if (value == null) return defaultVal.asInstanceOf[AnyRef]; else return value;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def get(index: Int): AnyRef = { // Return (value, type)
    try {
      index match {
        case 0 => return this.desynpuf_id.asInstanceOf[AnyRef];
        case 1 => return this.clm_id.asInstanceOf[AnyRef];
        case 2 => return this.clm_from_dt.asInstanceOf[AnyRef];
        case 3 => return this.chestpain.asInstanceOf[AnyRef];
        case 4 => return this.aatdeficiency.asInstanceOf[AnyRef];
        case 5 => return this.chroniccough.asInstanceOf[AnyRef];
        case 6 => return this.chronicsputum.asInstanceOf[AnyRef];

        case _ => throw new Exception(s"$index is a bad index for message HL7Fixed");
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def getOrElse(index: Int, defaultVal: Any): AnyRef = { // Return (value,  type)
    try {
      val value = get(index)
      if (value == null) return defaultVal.asInstanceOf[AnyRef]; else return value;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def getAttributeNames(): Array[String] = {
    try {
      if (keyTypes.isEmpty) {
        return null;
      } else {
        return keyTypes.keySet.toArray;
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def getAllAttributeValues(): Array[AttributeValue] = { // Has ( value, attributetypeinfo))
    var attributeVals = new Array[AttributeValue](7);
    try {
      attributeVals(0) = new AttributeValue(this.desynpuf_id, keyTypes("desynpuf_id"))
      attributeVals(1) = new AttributeValue(this.clm_id, keyTypes("clm_id"))
      attributeVals(2) = new AttributeValue(this.clm_from_dt, keyTypes("clm_from_dt"))
      attributeVals(3) = new AttributeValue(this.chestpain, keyTypes("chestpain"))
      attributeVals(4) = new AttributeValue(this.aatdeficiency, keyTypes("aatdeficiency"))
      attributeVals(5) = new AttributeValue(this.chroniccough, keyTypes("chroniccough"))
      attributeVals(6) = new AttributeValue(this.chronicsputum, keyTypes("chronicsputum"))

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

    return attributeVals;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[AttributeValue] = {
    //getAllAttributeValues.iterator.asInstanceOf[java.util.Iterator[AttributeValue]];
    return null; // Fix - need to test to make sure the above iterator works properly

  }

  override def set(key: String, value: Any) = {
    try {

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message HL7Fixed")
      set(keyTypes(key).getIndex, value);

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  def set(index: Int, value: Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message HL7Fixed ")
    try {
      index match {
        case 0 => {
          if (value.isInstanceOf[String])
            this.desynpuf_id = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 1 => {
          if (value.isInstanceOf[Long])
            this.clm_id = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 2 => {
          if (value.isInstanceOf[Int])
            this.clm_from_dt = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 3 => {
          if (value.isInstanceOf[String])
            this.chestpain = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 4 => {
          if (value.isInstanceOf[Int])
            this.aatdeficiency = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 5 => {
          if (value.isInstanceOf[Int])
            this.chroniccough = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }
        case 6 => {
          if (value.isInstanceOf[Int])
            this.chronicsputum = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message HL7Fixed")
        }

        case _ => throw new Exception(s"$index is a bad index for message HL7Fixed");
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def set(key: String, value: Any, valTyp: String) = {
    throw new Exception("Set Func for Value and ValueType By Key is not supported for Fixed Messages")
  }

  private def fromFunc(other: HL7Fixed): HL7Fixed = {
    this.desynpuf_id = com.ligadata.BaseTypes.StringImpl.Clone(other.desynpuf_id);
    this.clm_id = com.ligadata.BaseTypes.LongImpl.Clone(other.clm_id);
    this.clm_from_dt = com.ligadata.BaseTypes.IntImpl.Clone(other.clm_from_dt);
    this.chestpain = com.ligadata.BaseTypes.StringImpl.Clone(other.chestpain);
    this.aatdeficiency = com.ligadata.BaseTypes.IntImpl.Clone(other.aatdeficiency);
    this.chroniccough = com.ligadata.BaseTypes.IntImpl.Clone(other.chroniccough);
    this.chronicsputum = com.ligadata.BaseTypes.IntImpl.Clone(other.chronicsputum);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }

  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: HL7Fixed) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}