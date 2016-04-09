
package com.ligadata.KamanjaBase.V1000005000000;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeTypeInfo, AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept }
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date

object KamanjaMessageEvent extends RDDObject[KamanjaMessageEvent] with MessageFactoryInterface {
  type T = KamanjaMessageEvent;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaMessageEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaMessageEvent";
  override def getTypeVersion: String = "000001.000005.000000";
  override def getSchemaId: Int = 1000002;
  override def createInstance: KamanjaMessageEvent = new KamanjaMessageEvent(KamanjaMessageEvent);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE
  override def getFullName = getFullTypeName;
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);

  def build = new T(this)
  def build(from: T) = new T(from)
  override def getPartitionKeyNames: Array[String] = Array[String]();

  override def getPrimaryKeyNames: Array[String] = Array[String]();

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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.kamanjabase" , "name" : "kamanjamessageevent" , "fields":[{ "name" : "messageid" , "type" : "long"},{ "name" : "modelinfo" ,"type": {"type": "array", "items": { "type": "record",  "namespace" : "com.ligadata.kamanjabase" , "name" : "kamanjamodelevent" , "fields":[{ "name" : "modelid" , "type" : "long"},{ "name" : "elapsedtimeinms" , "type" : "float"},{ "name" : "eventepochtime" , "type" : "long"},{ "name" : "isresultproduced" ,},{ "name" : "producedmessages" , "type" :  {"type" : "array", "items" : "long"}},{ "name" : "error" , "type" : "string"}]}}},{ "name" : "elapsedtimeinms" , "type" : "float"},{ "name" : "messagekey" , "type" : "string"},{ "name" : "messagevalue" , "type" : "string"},{ "name" : "error" , "type" : "string"}]}""";
}

class KamanjaMessageEvent(factory: MessageFactoryInterface, other: KamanjaMessageEvent) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)

  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* KamanjaMessageEvent.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { KamanjaMessageEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var attributeTypes = getAttributeTypes;

  private def getAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](6);
    attributeTypes :+ new AttributeTypeInfo("messageid", 0, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes :+ new AttributeTypeInfo("modelinfo", 1, AttributeTypeInfo.TypeCategory.MESSAGE, -1, -1, 2000020)
    attributeTypes :+ new AttributeTypeInfo("elapsedtimeinms", 2, AttributeTypeInfo.TypeCategory.FLOAT, 2, 2, 0)
    attributeTypes :+ new AttributeTypeInfo("messagekey", 3, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes :+ new AttributeTypeInfo("messagevalue", 4, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes :+ new AttributeTypeInfo("error", 5, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)

    return attributeTypes
  }

  var messageid: Long = _;
  var modelinfo: scala.Array[com.ligadata.KamanjaBase.V1000003000000.KamanjaModelEvent] = _;
  var elapsedtimeinms: Float = _;
  var messagekey: String = _;
  var messagevalue: String = _;
  var error: String = _;

  private def getWithReflection(key: String): Any = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaMessageEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    return fmX.get;
  }

  override def get(key: String): Any = {
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

  private def getByName(key: String): Any = {
    if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message/container hl7Fixed ");
    return get(keyTypes(key).getIndex)
  }

  override def getOrElse(key: String, defaultVal: Any): Any = { // Return (value, type)
    try {
      val value = get(key.toLowerCase())
      if (value == null) return defaultVal; else return value;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return null;
  }

  override def getOrElse(index: Int, defaultVal: Any): Any = { // Return (value,  type)
    try {
      val value = get(index)
      if (value == null) return defaultVal; else return value;
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
    var attributeVals = new Array[AttributeValue](6);
    try {
      attributeVals :+ new AttributeValue(this.messageid, keyTypes("messageid"))
      attributeVals :+ new AttributeValue(this.modelinfo, keyTypes("modelinfo"))
      attributeVals :+ new AttributeValue(this.elapsedtimeinms, keyTypes("elapsedtimeinms"))
      attributeVals :+ new AttributeValue(this.messagekey, keyTypes("messagekey"))
      attributeVals :+ new AttributeValue(this.messagevalue, keyTypes("messagevalue"))
      attributeVals :+ new AttributeValue(this.error, keyTypes("error"))

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

  def get(index: Int): Any = { // Return (value, type)
    try {
      index match {
        case 0 => return this.messageid;
        case 1 => return this.modelinfo;
        case 2 => return this.elapsedtimeinms;
        case 3 => return this.messagekey;
        case 4 => return this.messagevalue;
        case 5 => return this.error;

        case _ => throw new Exception(s"$index is a bad index for message KamanjaMessageEvent");
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def set(key: String, value: Any) = {
    try {

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message KamanjaMessageEvent")
      set(keyTypes(key).getIndex, value);

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  def set(index: Int, value: Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message KamanjaMessageEvent ")
    try {
      index match {
        case 0 => {
          if (value.isInstanceOf[Long])
            this.messageid = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }
        case 1 => {
          if (value.isInstanceOf[scala.Array[com.ligadata.KamanjaBase.V1000003000000.KamanjaModelEvent]])
            this.modelinfo = value.asInstanceOf[scala.Array[com.ligadata.KamanjaBase.V1000003000000.KamanjaModelEvent]];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }
        case 2 => {
          if (value.isInstanceOf[Float])
            this.elapsedtimeinms = value.asInstanceOf[Float];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }
        case 3 => {
          if (value.isInstanceOf[String])
            this.messagekey = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }
        case 4 => {
          if (value.isInstanceOf[String])
            this.messagevalue = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }
        case 5 => {
          if (value.isInstanceOf[String])
            this.error = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaMessageEvent")
        }

        case _ => throw new Exception(s"$index is a bad index for message KamanjaMessageEvent");
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

  private def fromFunc(other: KamanjaMessageEvent): KamanjaMessageEvent = {
    this.messageid = com.ligadata.BaseTypes.LongImpl.Clone(other.messageid);
    if (other.modelinfo != null) {
      modelinfo = new scala.Array[com.ligadata.KamanjaBase.V1000003000000.KamanjaModelEvent](other.modelinfo.length)
      modelinfo = other.modelinfo.map(f => f.Clone.asInstanceOf[com.ligadata.KamanjaBase.V1000003000000.KamanjaModelEvent]);
    } else modelinfo = null;
    this.elapsedtimeinms = com.ligadata.BaseTypes.FloatImpl.Clone(other.elapsedtimeinms);
    this.messagekey = com.ligadata.BaseTypes.StringImpl.Clone(other.messagekey);
    this.messagevalue = com.ligadata.BaseTypes.StringImpl.Clone(other.messagevalue);
    this.error = com.ligadata.BaseTypes.StringImpl.Clone(other.error);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }

  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaMessageEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}