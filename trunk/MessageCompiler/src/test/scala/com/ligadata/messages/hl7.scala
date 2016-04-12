package com.ligadata.messages

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase._;
import com.ligadata.BaseTypes._
import java.io.{ DataInputStream, DataOutputStream, ByteArrayOutputStream }
import com.ligadata.Exceptions.StackTrace
import org.apache.logging.log4j.{ Logger, LogManager }
import scala.collection.mutable.ArrayBuffer;
import java.util.Date

object hl7 extends RDDObject[hl7] with ContainerFactoryInterface {
  type T = hl7;

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

  def getTimePartitionInfo: TimePartitionInfo = {
    var timePartitionInfo: TimePartitionInfo = new TimePartitionInfo();
    timePartitionInfo.setFieldName("clm_from_dt");
    timePartitionInfo.setFormat("epochtime");
    timePartitionInfo.setTimePartitionType(TimePartitionInfo.TimePartitionType.DAILY)
    return timePartitionInfo
  }

  override def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE
  override def isFixed: Boolean = false
  override def getAvroSchema: String = ""
  override def getSchemaId = 0;
  override def getPrimaryKeyNames: Array[String] = {
    var primaryKeyNames: Array[String] = Array("desynpuf_id", "clm_id");
    return primaryKeyNames;
  }

  override def getPartitionKeyNames: Array[String] = {
    var partitionKeyNames: Array[String] = Array("desynpuf_id");
    return partitionKeyNames;
  }

  override def createInstance: ContainerInterface = null
  override def getFullTypeName: String = "System.HL7"
  override def getTypeNameSpace: String = "System"
  override def getTypeName: String = "HL7"
  override def getTypeVersion: String = "000000.000001.000000"
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);
  override def getFullName: String = ""
  override def build: T = null
  override def build(from: T): T = null
}

class hl7(factory: ContainerFactoryInterface) extends ContainerInterface(factory) {
  private val log = LogManager.getLogger(getClass)

  var valuesMap = scala.collection.mutable.Map[String, AttributeValue]()

  var attributeTypes = generateAttributeTypes

  val keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](7)
    attributeTypes :+ new AttributeTypeInfo("desynpuf_id", 0, AttributeTypeInfo.TypeCategory.STRING, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("clm_id", 1, AttributeTypeInfo.TypeCategory.LONG, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("clm_from_dt", 2, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("clm_thru_dt", 3, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("bene_birth_dt", 4, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("bene_death_dt", 5, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes :+ new AttributeTypeInfo("bene_sex_ident_cd", 6, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    return attributeTypes
  }
  
  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    val attributeTyps = valuesMap.map(f => f._2.getValueType).toArray;
    if (attributeTyps == null) return null else return attributeTyps
  }

  override def getPrimaryKey(): Array[String] = {
    var primaryKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      primaryKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").asInstanceOf[String]);
      primaryKeys += com.ligadata.BaseTypes.LongImpl.toString(get("clm_id").asInstanceOf[Long]);
    } catch {
      case e: Exception => {
        log.debug("", e.getStackTrace)
        throw e
      }
    }
    primaryKeys.toArray;
  }

  override def getPartitionKey(): Array[String] = {
    var partitionKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      partitionKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").asInstanceOf[String]);
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    partitionKeys.toArray;
  }

  override def save() = {}
  override def Clone(): ContainerOrConcept = { hl7.build(this) }

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

  override def get(key: String): AnyRef = { // Return (value, type)
    val value = valuesMap(key).getValue
    if (value == null) return null; else return value;
  }

  override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return (value, type)
    try {
      val value = valuesMap(key).getValue
      if (value == null) return defaultVal.asInstanceOf[AnyRef];
      return value;
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
        valuesMap = valuesMap + (key -> new AttributeValue(value, keyTypes(keyName)))
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
}

