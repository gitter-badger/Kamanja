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

object hl7Fixed extends RDDObject[hl7Fixed] with ContainerFactoryInterface {
  type T = hl7Fixed;

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
  override def isFixed: Boolean = true
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
  override def getFullTypeName: String = "System.hl7Fixed"
  override def getTypeNameSpace: String = "System"
  override def getTypeName: String = "hl7Fixed"
  override def getTypeVersion: String = "000000.000001.000000"
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);

  override def getFullName: String = ""
  override def build: T = null
  override def build(from: T): T = null
}

class hl7Fixed(factory: ContainerFactoryInterface) extends ContainerInterface(factory) {
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var desynpuf_id: String = _;
  var clm_id: Long = _;
  var clm_from_dt: Int = _;
  var clm_thru_dt: Int = _;
  var bene_birth_dt: Int = _;
  var bene_death_dt: Int = _;
  var bene_sex_ident_cd: Int = _;
  var bene_race_cd: Int = _;

  //AttributeTypeInfo -- 0 -> index, 1 -> typeCategary (standard), 0 -> valueTypeId (since scalar type field), 0 - SchemaId (since not message/container type element), "string" -> keyTypeId

  var attributeTypes = generateAttributeTypes

  //(new AttributeTypeInfo(0, 1, 0, 0, "string"), new AttributeTypeInfo(1, 1, 0, 0, "long"), new AttributeTypeInfo(2, 1, 0, 0, "int"), new AttributeTypeInfo(3, 1, 0, 0, "int"), new AttributeTypeInfo(4, 1, 0, 0, "int"), new AttributeTypeInfo(5, 1, 0, 0, "int"), new AttributeTypeInfo(6, 1, 0, 0, "int"), new AttributeTypeInfo(7, 1, 0, 0, "int"));

  private val keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap

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
    if (attributeTypes == null) return null;
    return attributeTypes
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
      partitionKeys += com.ligadata.BaseTypes.StringImpl.toString(get("desynpuf_id").toString());
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    partitionKeys.toArray;
  }

  override def save() = {}
  override def Clone(): ContainerOrConcept = { hl7Fixed.build(this) }

  override def getAttributeNames(): Array[String] = {
    var attributeNames: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
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
  override def get(index: Int): AnyRef = { // Return (value, type)
    try {
      index match {
        case 0 => return this.desynpuf_id.asInstanceOf[AnyRef];
        case 1 => return this.clm_id.asInstanceOf[AnyRef];
        case 2 => return this.clm_from_dt.asInstanceOf[AnyRef]
        case 3 => return this.clm_thru_dt.asInstanceOf[AnyRef]
        case 4 => return this.bene_birth_dt.asInstanceOf[AnyRef]
        case 5 => return this.bene_death_dt.asInstanceOf[AnyRef]
        case 6 => return this.bene_sex_ident_cd.asInstanceOf[AnyRef]
        case _ => throw new Exception("Bad index");
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
  }

  override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return value
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

  private def getWithReflection(key: String): AnyRef = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[hl7Fixed].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    return fmX.get.asInstanceOf[AnyRef]

  }

  private def getByName(key: String): AnyRef = {
    if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message/container hl7Fixed ");
    return get(keyTypes(key).getIndex)

  }

  override def getAllAttributeValues(): Array[AttributeValue] = { // Has (name, value, type))
    var attributeVals = new Array[AttributeValue](7);
    attributeVals :+ new AttributeValue(this.desynpuf_id, keyTypes("desynpuf_id"))
    attributeVals :+ new AttributeValue(this.clm_id, keyTypes("clm_id"))
    attributeVals :+ new AttributeValue(this.clm_from_dt, keyTypes("clm_from_dt"))
    attributeVals :+ new AttributeValue(this.clm_thru_dt, keyTypes("clm_thru_dt"))
    attributeVals :+ new AttributeValue(this.bene_birth_dt, keyTypes("bene_birth_dt"))
    attributeVals :+ new AttributeValue(this.bene_death_dt, keyTypes("bene_death_dt"))
    attributeVals :+ new AttributeValue(this.bene_sex_ident_cd, keyTypes("bene_sex_ident_cd"))
    return attributeVals;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[AttributeValue] = {
    //getAllAttributeValues.iterator.asInstanceOf[java.util.Iterator[AttributeValue]];
    return null; // Fix - need to test to mae sure the above iterator works properly
  }

  override def set(key: String, value: Any) = {
    try {
      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message hl7fixed");
      set(keyTypes(key).getIndex, value)
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def set(index: Int, value: Any) = {
    if (value == null) throw new Exception(s"Value is null for index $index in message hl7fixed")
    index match {
      case 0 => {
        if (value.isInstanceOf[String])
          this.desynpuf_id = value.asInstanceOf[String];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 1 => {
        if (value.isInstanceOf[Long])
          this.clm_id = value.asInstanceOf[Long];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 2 => {
        if (value.isInstanceOf[Int])
          this.clm_from_dt = value.asInstanceOf[Int];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 3 => {
        if (value.isInstanceOf[Int])
          this.clm_thru_dt = value.asInstanceOf[Int];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 4 => {
        if (value.isInstanceOf[Int])
          this.bene_birth_dt = value.asInstanceOf[Int];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 5 => {
        if (value.isInstanceOf[Int])
          this.bene_death_dt = value.asInstanceOf[Int];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case 6 => {
        if (value.isInstanceOf[Int])
          this.bene_sex_ident_cd = value.asInstanceOf[Int];
        else throw new Exception("Value is the not the correct type for index $index in message hl7fixed")
      }
      case _ => throw new Exception(s"$index is a bad index for message hl7Fixed");
    }
  }

  override def set(key: String, value: Any, valTyp: String) = {}
}

