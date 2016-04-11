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
  override def getSchema: String = ""

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
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var valuesMap = new java.util.HashMap[String, AttributeValue]()
  private val keyTypes = Map("desynpuf_id" -> "String", "clm_id" -> "Long", "clm_from_dt" -> "Int")

  override def getPrimaryKey(): Array[String] = {
    var primaryKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      primaryKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").getValue.asInstanceOf[String]);
      primaryKeys += com.ligadata.BaseTypes.LongImpl.toString(get("clm_id").getValue.asInstanceOf[Long]);
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
      partitionKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").getValue.toString());
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
    var attributeNames: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      val iter = valuesMap.entrySet().iterator();
      while (iter.hasNext()) {
        val attributeName = iter.next().getKey;
        if (attributeName != null && attributeName.trim() != "")
          attributeNames += attributeName;
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return attributeNames.toArray;
  }

  override def get(key: String): AttributeValue = { // Return (value, type)
    try {
      return valuesMap.get(key.toLowerCase())
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def getOrElse(key: String, defaultVal: Any): AttributeValue = { // Return (value, type)
    var attributeValue: AttributeValue = new AttributeValue();

    try {
      val value = valuesMap.get(key.toLowerCase())
      if (value == null) {
        attributeValue.setValue(defaultVal);
        attributeValue.setValueType("Any");
        return attributeValue;
      } else {

        return value;
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def get(index: Int): AttributeValue = { // Return (value, type)
    throw new Exception("Get By Index is not supported in mapped messages");
  }

  override def getOrElse(index: Int, defaultVal: Any): AttributeValue = { // Return (value,  type)
    throw new Exception("Get By Index is not supported in mapped messages");
  }

  override def getAllAttributeValues(): java.util.HashMap[String, AttributeValue] = { // Has (name, value, type))
    return valuesMap;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[java.util.Map.Entry[String, AttributeValue]] = {
    valuesMap.entrySet().iterator();
  }

  override def set(key: String, value: Any) = {
    var attributeValue: AttributeValue = new AttributeValue();
    try {
      val keyName: String = key.toLowerCase();
      val valType = keyTypes.getOrElse(keyName, "Any")
      attributeValue.setValue(value)
      attributeValue.setValueType(valType)
      valuesMap.put(keyName, attributeValue)
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def set(key: String, value: Any, valTyp: String) = {
    var attributeValue: AttributeValue = new AttributeValue();
    try {
      attributeValue.setValue(value)
      attributeValue.setValueType(valTyp)
      valuesMap.put(key.toLowerCase(), attributeValue)
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
}

