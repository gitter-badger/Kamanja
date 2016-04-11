package com.ligadata.KamanjaBase;
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object KamanjaModelEvent extends RDDObject[KamanjaModelEvent] with MessageFactoryInterface {
  type T = KamanjaModelEvent ;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaModelEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaModelEvent";
  override def getTypeVersion: String = "000001.000001.000000";
  override def getSchemaId: Int = 2000001;
  override def createInstance: KamanjaModelEvent = new KamanjaModelEvent(KamanjaModelEvent);
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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanjabase\",\"name\" : \"kamanjamodelevent\",\"fields\":[{\"name\" : \"modelid\",\"type\" : \"long\"},{\"name\" : \"elapsedtimeinms\",\"type\" : \"float\"},{\"name\" : \"eventepochtime\",\"type\" : \"long\"},{\"name\" : \"isresultproduced\",},{\"name\" : \"producedmessages\",\"type\" : {\"type\" : \"array\", \"items\" : \"long\"}},{\"name\" : \"error\",\"type\" : \"string\"}]}";
}

class KamanjaModelEvent(factory: MessageFactoryInterface, other: KamanjaModelEvent) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("modelid"-> "Long","elapsedtimeinms"-> "Float","eventepochtime"-> "Long","isresultproduced"-> "Boolean","producedmessages"-> "scala.Array[Long]","error"-> "String");

  override def save: Unit = { KamanjaModelEvent.saveOne(this) }

  def Clone(): ContainerOrConcept = { KamanjaModelEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var modelid: Long = _;
  var elapsedtimeinms: Float = _;
  var eventepochtime: Long = _;
  var isresultproduced: Boolean = _;
  var producedmessages: scala.Array[Long] = _;
  var error: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaModelEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
      if (key.equals("modelid")) { attributeValue.setValue(this.modelid); }
      if (key.equals("elapsedtimeinms")) { attributeValue.setValue(this.elapsedtimeinms); }
      if (key.equals("eventepochtime")) { attributeValue.setValue(this.eventepochtime); }
      if (key.equals("isresultproduced")) { attributeValue.setValue(this.isresultproduced); }
      if (key.equals("producedmessages")) { attributeValue.setValue(this.producedmessages); }
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
        attributeVal.setValue(modelid)
        attributeVal.setValueType(keyTypes("modelid"))
        attributeValsMap.put("modelid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(elapsedtimeinms)
        attributeVal.setValueType(keyTypes("elapsedtimeinms"))
        attributeValsMap.put("elapsedtimeinms", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(eventepochtime)
        attributeVal.setValueType(keyTypes("eventepochtime"))
        attributeValsMap.put("eventepochtime", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(isresultproduced)
        attributeVal.setValueType(keyTypes("isresultproduced"))
        attributeValsMap.put("isresultproduced", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(producedmessages)
        attributeVal.setValueType(keyTypes("producedmessages"))
        attributeValsMap.put("producedmessages", attributeVal)
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
          attributeValue.setValue(this.modelid);
          attributeValue.setValueType(keyTypes("modelid"));
        }
        case 1 => {
          attributeValue.setValue(this.elapsedtimeinms);
          attributeValue.setValueType(keyTypes("elapsedtimeinms"));
        }
        case 2 => {
          attributeValue.setValue(this.eventepochtime);
          attributeValue.setValueType(keyTypes("eventepochtime"));
        }
        case 3 => {
          attributeValue.setValue(this.isresultproduced);
          attributeValue.setValueType(keyTypes("isresultproduced"));
        }
        case 4 => {
          attributeValue.setValue(this.producedmessages);
          attributeValue.setValueType(keyTypes("producedmessages"));
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

      if (key.equals("modelid")) { this.modelid = value.asInstanceOf[Long]; }
      if (key.equals("elapsedtimeinms")) { this.elapsedtimeinms = value.asInstanceOf[Float]; }
      if (key.equals("eventepochtime")) { this.eventepochtime = value.asInstanceOf[Long]; }
      if (key.equals("isresultproduced")) { this.isresultproduced = value.asInstanceOf[Boolean]; }
      if (key.equals("producedmessages")) { this.producedmessages = value.asInstanceOf[scala.Array[Long]]; }
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
        case 0 => {this.modelid = value.asInstanceOf[Long];}
        case 1 => {this.elapsedtimeinms = value.asInstanceOf[Float];}
        case 2 => {this.eventepochtime = value.asInstanceOf[Long];}
        case 3 => {this.isresultproduced = value.asInstanceOf[Boolean];}
        case 4 => {this.producedmessages = value.asInstanceOf[scala.Array[Long]];}
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

  private def fromFunc(other: KamanjaModelEvent): KamanjaModelEvent = {
    this.modelid = com.ligadata.BaseTypes.LongImpl.Clone(other.modelid);
    this.elapsedtimeinms = com.ligadata.BaseTypes.FloatImpl.Clone(other.elapsedtimeinms);
    this.eventepochtime = com.ligadata.BaseTypes.LongImpl.Clone(other.eventepochtime);
    this.isresultproduced = com.ligadata.BaseTypes.BoolImpl.Clone(other.isresultproduced);
    if (other.producedmessages != null ) {
      producedmessages = new scala.Array[Long](other.producedmessages.length);
      producedmessages = other.producedmessages.map(v => com.ligadata.BaseTypes.LongImpl.Clone(v));
    }
    else this.producedmessages = null;
    this.error = com.ligadata.BaseTypes.StringImpl.Clone(other.error);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaModelEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}