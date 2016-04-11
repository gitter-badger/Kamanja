package com.ligadata.KamanjaBase;
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object KamanjaExceptionEvent extends RDDObject[KamanjaExceptionEvent] with MessageFactoryInterface {
  type T = KamanjaExceptionEvent ;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaExceptionEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaExceptionEvent";
  override def getTypeVersion: String = "000001.000002.000000";
  override def getSchemaId: Int = 2000020;
  override def createInstance: KamanjaExceptionEvent = new KamanjaExceptionEvent(KamanjaExceptionEvent);
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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanjabase\",\"name\" : \"kamanjaexceptionevent\",\"fields\":[{\"name\" : \"componentname\",\"type\" : \"string\"},{\"name\" : \"timeoferrorepochms\",\"type\" : \"long\"},{\"name\" : \"errortype\",\"type\" : \"string\"},{\"name\" : \"errorstring\",\"type\" : \"string\"}]}";
}

class KamanjaExceptionEvent(factory: MessageFactoryInterface, other: KamanjaExceptionEvent) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("componentname"-> "String","timeoferrorepochms"-> "Long","errortype"-> "String","errorstring"-> "String");

  override def save: Unit = { KamanjaExceptionEvent.saveOne(this) }

  def Clone(): ContainerOrConcept = { KamanjaExceptionEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var componentname: String = _;
  var timeoferrorepochms: Long = _;
  var errortype: String = _;
  var errorstring: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaExceptionEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
      if (key.equals("componentname")) { attributeValue.setValue(this.componentname); }
      if (key.equals("timeoferrorepochms")) { attributeValue.setValue(this.timeoferrorepochms); }
      if (key.equals("errortype")) { attributeValue.setValue(this.errortype); }
      if (key.equals("errorstring")) { attributeValue.setValue(this.errorstring); }


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
        attributeVal.setValue(componentname)
        attributeVal.setValueType(keyTypes("componentname"))
        attributeValsMap.put("componentname", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(timeoferrorepochms)
        attributeVal.setValueType(keyTypes("timeoferrorepochms"))
        attributeValsMap.put("timeoferrorepochms", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(errortype)
        attributeVal.setValueType(keyTypes("errortype"))
        attributeValsMap.put("errortype", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(errorstring)
        attributeVal.setValueType(keyTypes("errorstring"))
        attributeValsMap.put("errorstring", attributeVal)
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
          attributeValue.setValue(this.componentname);
          attributeValue.setValueType(keyTypes("componentname"));
        }
        case 1 => {
          attributeValue.setValue(this.timeoferrorepochms);
          attributeValue.setValueType(keyTypes("timeoferrorepochms"));
        }
        case 2 => {
          attributeValue.setValue(this.errortype);
          attributeValue.setValueType(keyTypes("errortype"));
        }
        case 3 => {
          attributeValue.setValue(this.errorstring);
          attributeValue.setValueType(keyTypes("errorstring"));
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

      if (key.equals("componentname")) { this.componentname = value.asInstanceOf[String]; }
      if (key.equals("timeoferrorepochms")) { this.timeoferrorepochms = value.asInstanceOf[Long]; }
      if (key.equals("errortype")) { this.errortype = value.asInstanceOf[String]; }
      if (key.equals("errorstring")) { this.errorstring = value.asInstanceOf[String]; }

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
        case 0 => {this.componentname = value.asInstanceOf[String];}
        case 1 => {this.timeoferrorepochms = value.asInstanceOf[Long];}
        case 2 => {this.errortype = value.asInstanceOf[String];}
        case 3 => {this.errorstring = value.asInstanceOf[String];}

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

  private def fromFunc(other: KamanjaExceptionEvent): KamanjaExceptionEvent = {
    this.componentname = com.ligadata.BaseTypes.StringImpl.Clone(other.componentname);
    this.timeoferrorepochms = com.ligadata.BaseTypes.LongImpl.Clone(other.timeoferrorepochms);
    this.errortype = com.ligadata.BaseTypes.StringImpl.Clone(other.errortype);
    this.errorstring = com.ligadata.BaseTypes.StringImpl.Clone(other.errorstring);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaExceptionEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}