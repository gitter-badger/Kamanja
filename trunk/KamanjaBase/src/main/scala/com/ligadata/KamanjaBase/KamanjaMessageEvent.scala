package com.ligadata.KamanjaBase;
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ KamanjaModelEvent, AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object KamanjaMessageEvent extends RDDObject[KamanjaMessageEvent] with MessageFactoryInterface {
  type T = KamanjaMessageEvent ;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaMessageEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaMessageEvent";
  override def getTypeVersion: String = "000001.000005.000000";
  override def getSchemaId: Int = 2000017;
  override def createInstance: KamanjaMessageEvent = new KamanjaMessageEvent(KamanjaMessageEvent);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE
  override def getFullName = getFullTypeName;
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);

  def build = new T(this)
  def build(from: T) = new T(from)
  override def getPartitionKeyNames: Array[String] = Array[String]();

  override def getPrimaryKeyNames: Array[String] = Array[String]();


  override def getTimePartitionInfo: TimePartitionInfo = { return null;}  // FieldName, Format & Time Partition Types(Daily/Monthly/Yearly)


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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanjabase\",\"name\" : \"kamanjamessageevent\",\"fields\":[{\"name\" : \"messageid\",\"type\" : \"long\"},{\"name\" : \"modelinfo\",\"type\" : {\"type\" : \"array\", \"items\" : \"string\"}},{\"name\" : \"elapsedtimeinms\",\"type\" : \"float\"},{\"name\" : \"messagekey\",\"type\" : \"string\"},{\"name\" : \"messagevalue\",\"type\" : \"string\"},{\"name\" : \"error\",\"type\" : \"string\"}]}";
}

class KamanjaMessageEvent(factory: MessageFactoryInterface, other: KamanjaMessageEvent) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("messageid"-> "Long","modelinfo"-> "scala.Array[String]","elapsedtimeinms"-> "Float","messagekey"-> "String","messagevalue"-> "String","error"-> "String");

  override def save: Unit = { KamanjaMessageEvent.saveOne(this) }

  def Clone(): ContainerOrConcept = { KamanjaMessageEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var messageid: Long = _;
  var modelinfo: scala.Array[com.ligadata.KamanjaBase.KamanjaModelEvent] = _;
  var elapsedtimeinms: Float = _;
  var messagekey: String = _;
  var messagevalue: String = _;
  var error: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaMessageEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    attributeValue.setValue(fmX.get);
    attributeValue.setValueType(keyTypes(key))
    attributeValue
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

  private def getByName(key: String): AttributeValue = {
    try {
      if (!keyTypes.contains(key)) throw new Exception("Key does not exists");
      var attributeValue = new AttributeValue();
      if (key.equals("messageid")) { attributeValue.setValue(this.messageid); }
      if (key.equals("modelinfo")) { attributeValue.setValue(this.modelinfo); }
      if (key.equals("elapsedtimeinms")) { attributeValue.setValue(this.elapsedtimeinms); }
      if (key.equals("messagekey")) { attributeValue.setValue(this.messagekey); }
      if (key.equals("messagevalue")) { attributeValue.setValue(this.messagevalue); }
      if (key.equals("error")) { attributeValue.setValue(this.error); }


      attributeValue.setValueType(keyTypes(key.toLowerCase()));
      return attributeValue;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

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

  override def getAllAttributeValues(): java.util.HashMap[String, AttributeValue] = { // Has (name, value, type))
  var attributeValsMap = new java.util.HashMap[String, AttributeValue];
    try{
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(messageid)
        attributeVal.setValueType(keyTypes("messageid"))
        attributeValsMap.put("messageid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(modelinfo)
        attributeVal.setValueType(keyTypes("modelinfo"))
        attributeValsMap.put("modelinfo", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(elapsedtimeinms)
        attributeVal.setValueType(keyTypes("elapsedtimeinms"))
        attributeValsMap.put("elapsedtimeinms", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(messagekey)
        attributeVal.setValueType(keyTypes("messagekey"))
        attributeValsMap.put("messagekey", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(messagevalue)
        attributeVal.setValueType(keyTypes("messagevalue"))
        attributeValsMap.put("messagevalue", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(error)
        attributeVal.setValueType(keyTypes("error"))
        attributeValsMap.put("error", attributeVal)
      };

    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

    return attributeValsMap;
  }

  override def getAttributeNameAndValueIterator(): java.util.Iterator[java.util.Map.Entry[String, AttributeValue]] = {
    getAllAttributeValues.entrySet().iterator();
  }


  def get(index : Int) : AttributeValue = { // Return (value, type)
  var attributeValue = new AttributeValue();
    try{
      index match {
        case 0 => {
          attributeValue.setValue(this.messageid);
          attributeValue.setValueType(keyTypes("messageid"));
        }
        case 1 => {
          attributeValue.setValue(this.modelinfo);
          attributeValue.setValueType(keyTypes("modelinfo"));
        }
        case 2 => {
          attributeValue.setValue(this.elapsedtimeinms);
          attributeValue.setValueType(keyTypes("elapsedtimeinms"));
        }
        case 3 => {
          attributeValue.setValue(this.messagekey);
          attributeValue.setValueType(keyTypes("messagekey"));
        }
        case 4 => {
          attributeValue.setValue(this.messagevalue);
          attributeValue.setValueType(keyTypes("messagevalue"));
        }
        case 5 => {
          attributeValue.setValue(this.error);
          attributeValue.setValueType(keyTypes("error"));
        }

        case _ => throw new Exception("Bad index");
      }
      return attributeValue;
    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def set(key: String, value: Any) = {
    try {

      if (key.equals("messageid")) { this.messageid = value.asInstanceOf[Long]; }
      if (key.equals("modelinfo")) { this.modelinfo = value.asInstanceOf[scala.Array[com.ligadata.KamanjaBase.KamanjaModelEvent]]; }
      if (key.equals("elapsedtimeinms")) { this.elapsedtimeinms = value.asInstanceOf[Float]; }
      if (key.equals("messagekey")) { this.messagekey = value.asInstanceOf[String]; }
      if (key.equals("messagevalue")) { this.messagevalue = value.asInstanceOf[String]; }
      if (key.equals("error")) { this.error = value.asInstanceOf[String]; }

    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }


  def set(index : Int, value :Any): Unit = {
    try{
      index match {
        case 0 => {this.messageid = value.asInstanceOf[Long];}
        case 1 => {this.modelinfo = value.asInstanceOf[scala.Array[com.ligadata.KamanjaBase.KamanjaModelEvent]];}
        case 2 => {this.elapsedtimeinms = value.asInstanceOf[Float];}
        case 3 => {this.messagekey = value.asInstanceOf[String];}
        case 4 => {this.messagevalue = value.asInstanceOf[String];}
        case 5 => {this.error = value.asInstanceOf[String];}

        case _ => throw new Exception("Bad index");
      }
    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def set(key: String, value: Any, valTyp: String) = {
    throw new Exception ("Set Func for Value and ValueType By Key is not supported for Fixed Messages" )
  }

  private def fromFunc(other: KamanjaMessageEvent): KamanjaMessageEvent = {
    this.messageid = com.ligadata.BaseTypes.LongImpl.Clone(other.messageid);
    if (other.modelinfo != null ) {
      modelinfo = new scala.Array[com.ligadata.KamanjaBase.KamanjaModelEvent](other.modelinfo.length);
      modelinfo = other.modelinfo.map(v => v);
    }
    else this.modelinfo = null;
    this.elapsedtimeinms = com.ligadata.BaseTypes.FloatImpl.Clone(other.elapsedtimeinms);
    this.messagekey = com.ligadata.BaseTypes.StringImpl.Clone(other.messagekey);
    this.messagevalue = com.ligadata.BaseTypes.StringImpl.Clone(other.messagevalue);
    this.error = com.ligadata.BaseTypes.StringImpl.Clone(other.error);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaMessageEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}