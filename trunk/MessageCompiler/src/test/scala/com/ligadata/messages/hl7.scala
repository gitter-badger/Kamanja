
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

object HL7 extends RDDObject[HL7] with MessageFactoryInterface {
  type T = HL7;
  override def getFullTypeName: String = "com.ligadata.messages.HL7";
  override def getTypeNameSpace: String = "com.ligadata.messages";
  override def getTypeName: String = "HL7";
  override def getTypeVersion: String = "000000.000001.000000";
  override def getSchemaId: Int = 0;
  override def createInstance: HL7 = new HL7(HL7);
  override def isFixed: Boolean = false;
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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.messages" , "name" : "hl7" , "fields":[{ "name" : "desynpuf_id" , "type" : "string"},{ "name" : "clm_id" , "type" : "long"},{ "name" : "clm_from_dt" , "type" : "int"},{ "name" : "chestpain" , "type" : "string"},{ "name" : "aatdeficiency" , "type" : "int"},{ "name" : "chroniccough" , "type" : "int"},{ "name" : "chronicsputum" , "type" : "int"}]}""";

  override def FullName: String = getFullTypeName
  override def NameSpace: String = getTypeNameSpace
  override def Name: String = getTypeName
  override def Version: String = getTypeVersion
  override def CreateNewMessage: BaseMsg = createInstance.asInstanceOf[BaseMsg];
  override def CreateNewContainer: BaseContainer = null;
  override def IsFixed: Boolean = false
  override def IsKv: Boolean = true
  override def CanPersist: Boolean = true
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  override def PartitionKeyData(inputdata: InputData): Array[String] = createInstance.getPartitionKey();
  override def PrimaryKeyData(inputdata: InputData): Array[String] = createInstance.getPrimaryKey();
  override def TimePartitionData(inputdata: InputData): Long = createInstance.getTimePartitionData;
  override def NeedToTransformData: Boolean = false
}

class HL7(factory: MessageFactoryInterface, other: HL7) extends MessageInterface(factory) {

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

  override def save: Unit = { /* HL7.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { HL7.build(this) }

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

  var valuesMap = scala.collection.mutable.Map[String, AttributeValue]()

  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    val attributeTyps = valuesMap.map(f => f._2.getValueType).toArray;
    if (attributeTyps == null) return null else return attributeTyps
  }

  override def get(key: String): AnyRef = { // Return (value, type)
    try {
      val value = valuesMap(key).getValue
      if (value == null) return null; else return value.asInstanceOf[AnyRef];
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return (value, type)
    var attributeValue: AttributeValue = new AttributeValue();
    try {
      val value = valuesMap(key).getValue
      if (value == null) return defaultVal.asInstanceOf[AnyRef];
      return value.asInstanceOf[AnyRef];
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def getAttributeNames(): Array[String] = {
    try {
      if (valuesMap.isEmpty) {
        return null;
      } else {
        return valuesMap.keySet.toArray;
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def get(index: Int): AnyRef = { // Return (value, type)
    throw new Exception("Get By Index is not supported in mapped messages");
  }

  override def getOrElse(index: Int, defaultVal: Any): AnyRef = { // Return (value,  type)
    throw new Exception("Get By Index is not supported in mapped messages");
  }

  override def getAllAttributeValues(): Array[AttributeValue] = { // Has (name, value, type))
    return valuesMap.map(f => f._2).toArray;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[AttributeValue] = {
    //valuesMap.iterator.asInstanceOf[java.util.Iterator[AttributeValue]];
    return null;
  }

  override def set(key: String, value: Any) = {
    try {
      val keyName: String = key.toLowerCase();
      if (keyTypes.contains(key)) {
        valuesMap(key) = new AttributeValue(value, keyTypes(keyName))
      } else {
        valuesMap(key) = new AttributeValue(ValueToString(value), new AttributeTypeInfo(key, -1, AttributeTypeInfo.TypeCategory.STRING, 0, 0, 0))
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def set(key: String, value: Any, valTyp: String) = {
    try {
      val keyName: String = key.toLowerCase();
      if (keyTypes.contains(key)) {
        valuesMap(key) = new AttributeValue(value, keyTypes(keyName))
      } else {
        val typeCategory = AttributeTypeInfo.TypeCategory.valueOf(valTyp.toUpperCase())
        val keytypeId = typeCategory.getValue.toShort
        val valtypeId = typeCategory.getValue.toShort
        valuesMap(key) = new AttributeValue(value, new AttributeTypeInfo(key, -1, typeCategory, valtypeId, keytypeId, 0))
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def set(index: Int, value: Any) = {
    throw new Exception("Set By Index is not supported in mapped messages");
  }

  private def ValueToString(v: Any): String = {
    if (v.isInstanceOf[Set[_]]) {
      return v.asInstanceOf[Set[_]].mkString(",")
    }
    if (v.isInstanceOf[List[_]]) {
      return v.asInstanceOf[List[_]].mkString(",")
    }
    if (v.isInstanceOf[Array[_]]) {
      return v.asInstanceOf[Array[_]].mkString(",")
    }
    v.toString
  }

  private def fromFunc(other: HL7): HL7 = {
    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }

  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: HL7) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}