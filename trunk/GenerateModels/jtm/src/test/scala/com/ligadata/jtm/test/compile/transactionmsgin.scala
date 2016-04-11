package com.ligadata.kamanja.samples.messages.V1000000;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object TransactionMsgIn extends RDDObject[TransactionMsgIn] with MessageFactoryInterface {
  type T = TransactionMsgIn ;
  override def getFullTypeName: String = "com.ligadata.kamanja.samples.messages.TransactionMsgIn";
  override def getTypeNameSpace: String = "com.ligadata.kamanja.samples.messages";
  override def getTypeName: String = "TransactionMsgIn";
  override def getTypeVersion: String = "000000.000001.000000";
  override def getSchemaId: Int = 0;
  override def createInstance: TransactionMsgIn = new TransactionMsgIn(TransactionMsgIn);
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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanja.samples.messages\",\"name\" : \"transactionmsgin\",\"fields\":[{\"name\" : \"data\",\"type\" : \"string\"}]}";
}

class TransactionMsgIn(factory: MessageFactoryInterface, other: TransactionMsgIn) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("data" -> "String");

  override def save: Unit = {
    /* TransactionMsgIn.saveOne(this) */
  }

  def Clone(): ContainerOrConcept = {
    TransactionMsgIn.build(this)
  }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  var data: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[TransactionMsgIn].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
      if (key.equals("data")) {
        attributeValue.setValue(this.data);
      }


      attributeValue.setValueType(keyTypes(key.toLowerCase()));
      return attributeValue;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def getOrElse(key: String, defaultVal: Any): AttributeValue = {
    // Return (value, type)
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

  override def getOrElse(index: Int, defaultVal: Any): AttributeValue = {
    // Return (value,  type)
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
    return null;
    ;
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

  override def getAllAttributeValues(): java.util.HashMap[String, AttributeValue] = {
    // Has (name, value, type))
    var attributeValsMap = new java.util.HashMap[String, AttributeValue];
    try { {
      var attributeVal = new AttributeValue();
      attributeVal.setValue(data)
      attributeVal.setValueType(keyTypes("data"))
      attributeValsMap.put("data", attributeVal)
    };

    } catch {
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


  def get(index: Int): AttributeValue = {
    // Return (value, type)
    var attributeValue = new AttributeValue();
    try {
      index match {
        case 0 => {
          attributeValue.setValue(this.data);
          attributeValue.setValueType(keyTypes("data"));
        }

        case _ => throw new Exception("Bad index");
      }
      return attributeValue;
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  override def set(key: String, value: Any) = {
    try {

      if (key.equals("data")) {
        this.data = value.asInstanceOf[String];
      }

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }


  def set(index: Int, value: Any): Unit = {
    try {
      index match {
        case 0 => {
          this.data = value.asInstanceOf[String];
        }

        case _ => throw new Exception("Bad index");
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

  private def fromFunc(other: TransactionMsgIn): TransactionMsgIn = {
    this.data = com.ligadata.BaseTypes.StringImpl.Clone(other.data);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: TransactionMsgIn) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }
}
