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

  private val keyTypes = Map("desynpuf_id" -> "String", "clm_id" -> "Long", "clm_from_dt" -> "Int", "clm_thru_dt" -> "Int", "bene_birth_dt" -> "Int", "bene_death_dt" -> "Int", "bene_sex_ident_cd" -> "Int", "bene_race_cd" -> "Int")

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

  override def get(key: String): AttributeValue = {
    try {
      // Try with reflection
      return getWithReflection(key.toLowerCase())
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        // Call By Name
        return getByName(key.toLowerCase())
      }
    }
  }

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[hl7Fixed].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    attributeValue.setValue(fmX.get);
    attributeValue.setValueType(keyTypes(key))
    attributeValue

  }

  private def getByName(key: String): AttributeValue = {
    try {
      if (!keyTypes.contains(key)) throw new Exception("Key does not exists");
      var attributeValue = new AttributeValue();
      if (key.equals("desynpuf_id")) { attributeValue.setValue(this.desynpuf_id); }
      if (key.equals("clm_id")) { attributeValue.setValue(this.clm_id); }
      if (key.equals("clm_from_dt")) { attributeValue.setValue(this.clm_from_dt); }
      if (key.equals("clm_thru_dt")) { attributeValue.setValue(this.clm_thru_dt); }
      if (key.equals("bene_birth_dt")) { attributeValue.setValue(this.bene_birth_dt); }
      if (key.equals("bene_death_dt")) { attributeValue.setValue(this.bene_death_dt); }
      if (key.equals("bene_sex_ident_cd")) { attributeValue.setValue(this.bene_sex_ident_cd); }
      attributeValue.setValueType(keyTypes(key.toLowerCase()));
      return attributeValue;
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
  }

  override def get(index: Int): AttributeValue = { // Return (value, type)
    var attributeValue = new AttributeValue();
    try {
      index match {
        case 0 => {
          attributeValue.setValue(this.desynpuf_id);
          attributeValue.setValueType(keyTypes("desynpuf_id"));
        }
        case 1 => {
          attributeValue.setValue(this.clm_id);
          attributeValue.setValueType(keyTypes("clm_id"));
        }
        case 2 => {
          attributeValue.setValue(this.clm_from_dt);
          attributeValue.setValueType(keyTypes("clm_from_dt"));
        }
        case 3 => {
          attributeValue.setValue(this.clm_thru_dt);
          attributeValue.setValueType(keyTypes("clm_thru_dt"));
        }
        case 4 => {
          attributeValue.setValue(this.bene_birth_dt);
          attributeValue.setValueType(keyTypes("bene_birth_dt"));
        }
        case 5 => {
          attributeValue.setValue(this.bene_death_dt);
          attributeValue.setValueType(keyTypes("bene_death_dt"));
        }
        case 6 => {
          attributeValue.setValue(this.bene_sex_ident_cd);
          attributeValue.setValueType(keyTypes("bene_sex_ident_cd"));
        }
        case _ => throw new Exception("Bad index");
      }

      return attributeValue;
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
  }

  override def getOrElse(key: String, defaultVal: Any): AttributeValue = { // Return (value, type)
    var attributeValue: AttributeValue = new AttributeValue();
    try {
      val value = get(key.toLowerCase())
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

  override def getOrElse(index: Int, defaultVal: Any): AttributeValue = { // Return (value,  type)
    var attributeValue: AttributeValue = new AttributeValue();
    try {
      val value = get(index)
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
    return null; ;
  }

  override def getAllAttributeValues(): java.util.HashMap[String, AttributeValue] = { // Has (name, value, type))
    var attributeValsMap = new java.util.HashMap[String, AttributeValue];
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(desynpuf_id)
      attributeVal.setValueType(keyTypes("desynpuf_id"))
      attributeValsMap.put("desynpuf_id", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(clm_id)
      attributeVal.setValueType(keyTypes("clm_id"))
      attributeValsMap.put("clm_id", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(clm_from_dt)
      attributeVal.setValueType(keyTypes("clm_from_dt"))
      attributeValsMap.put("clm_from_dt", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(clm_from_dt)
      attributeVal.setValueType(keyTypes("clm_thru_dt"))
      attributeValsMap.put("clm_thru_dt", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(bene_birth_dt)
      attributeVal.setValueType(keyTypes("bene_birth_dt"))
      attributeValsMap.put("bene_birth_dt", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(bene_death_dt)
      attributeVal.setValueType(keyTypes("bene_death_dt"))
      attributeValsMap.put("bene_death_dt", attributeVal)
    };
    {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(bene_sex_ident_cd)
      attributeVal.setValueType(keyTypes("bene_sex_ident_cd"))
      attributeValsMap.put("bene_sex_ident_cd", attributeVal)
    };

    return attributeValsMap;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[java.util.Map.Entry[String, AttributeValue]] = {
    getAllAttributeValues.entrySet().iterator();
  }

  override def set(key: String, value: Any) = {
    try {
      if (key.equals("desynpuf_id")) { this.desynpuf_id = value.asInstanceOf[String]; }
      if (key.equals("clm_id")) { this.clm_id = value.asInstanceOf[Long]; }
      if (key.equals("clm_from_dt")) { this.clm_from_dt = value.asInstanceOf[Int]; }
      if (key.equals("clm_thru_dt")) { this.clm_thru_dt = value.asInstanceOf[Int]; }
      if (key.equals("bene_birth_dt")) { this.bene_birth_dt = value.asInstanceOf[Int]; }
      if (key.equals("bene_death_dt")) { this.bene_death_dt = value.asInstanceOf[Int]; }
      if (key.equals("bene_sex_ident_cd")) { this.bene_sex_ident_cd = value.asInstanceOf[Int]; }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
  }

  override def set(index: Int, value: Any) = {
    index match {
      case 0 => { this.desynpuf_id = value.asInstanceOf[String]; }
      case 1 => { this.clm_id = value.asInstanceOf[Long]; }
      case 2 => { this.clm_from_dt = value.asInstanceOf[Int]; }
      case 3 => { this.clm_thru_dt = value.asInstanceOf[Int]; }
      case 4 => { this.bene_birth_dt = value.asInstanceOf[Int]; }
      case 5 => { this.bene_death_dt = value.asInstanceOf[Int]; }
      case 6 => { this.bene_sex_ident_cd = value.asInstanceOf[Int]; }
      case _ => throw new Exception("Bad index");
    }
  }

  override def set(key: String, value: Any, valTyp: String) = {}
}

