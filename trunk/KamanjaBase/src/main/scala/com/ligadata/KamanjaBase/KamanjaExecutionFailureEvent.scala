package com.ligadata.KamanjaBase;
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object KamanjaExecutionFailureEvent extends RDDObject[KamanjaExecutionFailureEvent] with MessageFactoryInterface {
  type T = KamanjaExecutionFailureEvent ;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaExecutionFailureEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaExecutionFailureEvent";
  override def getTypeVersion: String = "000001.000002.000000";
  override def getSchemaId: Int = 2000021;
  override def createInstance: KamanjaExecutionFailureEvent = new KamanjaExecutionFailureEvent(KamanjaExecutionFailureEvent);
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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanjabase\",\"name\" : \"kamanjaexecutionfailureevent\",\"fields\":[{\"name\" : \"msgid\",\"type\" : \"long\"},{\"name\" : \"timeoferrorepochms\",\"type\" : \"long\"},{\"name\" : \"msgcontent\",\"type\" : \"string\"},{\"name\" : \"errordetail\",\"type\" : \"string\"}]}";
}

class KamanjaExecutionFailureEvent(factory: MessageFactoryInterface, other: KamanjaExecutionFailureEvent) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("msgid"-> "Long","timeoferrorepochms"-> "Long","msgcontent"-> "String","errordetail"-> "String");

  override def save: Unit = { KamanjaExecutionFailureEvent.saveOne(this) }

  def Clone(): ContainerOrConcept = { KamanjaExecutionFailureEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var msgid: Long = _;
  var timeoferrorepochms: Long = _;
  var msgcontent: String = _;
  var errordetail: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaExecutionFailureEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
      if (key.equals("msgid")) { attributeValue.setValue(this.msgid); }
      if (key.equals("timeoferrorepochms")) { attributeValue.setValue(this.timeoferrorepochms); }
      if (key.equals("msgcontent")) { attributeValue.setValue(this.msgcontent); }
      if (key.equals("errordetail")) { attributeValue.setValue(this.errordetail); }


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
        attributeVal.setValue(msgid)
        attributeVal.setValueType(keyTypes("msgid"))
        attributeValsMap.put("msgid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(timeoferrorepochms)
        attributeVal.setValueType(keyTypes("timeoferrorepochms"))
        attributeValsMap.put("timeoferrorepochms", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(msgcontent)
        attributeVal.setValueType(keyTypes("msgcontent"))
        attributeValsMap.put("msgcontent", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(errordetail)
        attributeVal.setValueType(keyTypes("errordetail"))
        attributeValsMap.put("errordetail", attributeVal)
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
          attributeValue.setValue(this.msgid);
          attributeValue.setValueType(keyTypes("msgid"));
        }
        case 1 => {
          attributeValue.setValue(this.timeoferrorepochms);
          attributeValue.setValueType(keyTypes("timeoferrorepochms"));
        }
        case 2 => {
          attributeValue.setValue(this.msgcontent);
          attributeValue.setValueType(keyTypes("msgcontent"));
        }
        case 3 => {
          attributeValue.setValue(this.errordetail);
          attributeValue.setValueType(keyTypes("errordetail"));
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

      if (key.equals("msgid")) { this.msgid = value.asInstanceOf[Long]; }
      if (key.equals("timeoferrorepochms")) { this.timeoferrorepochms = value.asInstanceOf[Long]; }
      if (key.equals("msgcontent")) { this.msgcontent = value.asInstanceOf[String]; }
      if (key.equals("errordetail")) { this.errordetail = value.asInstanceOf[String]; }

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
        case 0 => {this.msgid = value.asInstanceOf[Long];}
        case 1 => {this.timeoferrorepochms = value.asInstanceOf[Long];}
        case 2 => {this.msgcontent = value.asInstanceOf[String];}
        case 3 => {this.errordetail = value.asInstanceOf[String];}

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

  private def fromFunc(other: KamanjaExecutionFailureEvent): KamanjaExecutionFailureEvent = {
    this.msgid = com.ligadata.BaseTypes.LongImpl.Clone(other.msgid);
    this.timeoferrorepochms = com.ligadata.BaseTypes.LongImpl.Clone(other.timeoferrorepochms);
    this.msgcontent = com.ligadata.BaseTypes.StringImpl.Clone(other.msgcontent);
    this.errordetail = com.ligadata.BaseTypes.StringImpl.Clone(other.errordetail);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaExecutionFailureEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}
