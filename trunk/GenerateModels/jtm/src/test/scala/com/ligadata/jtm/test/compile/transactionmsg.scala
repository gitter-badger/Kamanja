package com.ligadata.kamanja.samples.messages.V1000000;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object TransactionMsg extends RDDObject[TransactionMsg] with MessageFactoryInterface {
  type T = TransactionMsg ;
  override def getFullTypeName: String = "com.ligadata.kamanja.samples.messages.TransactionMsg";
  override def getTypeNameSpace: String = "com.ligadata.kamanja.samples.messages";
  override def getTypeName: String = "TransactionMsg";
  override def getTypeVersion: String = "000000.000001.000000";
  override def getSchemaId: Int = 0;
  override def createInstance: TransactionMsg = new TransactionMsg(TransactionMsg);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE
  override def getFullName = getFullTypeName;
  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this);

  def build = new T(this)
  def build(from: T) = new T(from)
  override def getPartitionKeyNames: Array[String] = Array("custid");

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

  override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanja.samples.messages\",\"name\" : \"transactionmsg\",\"fields\":[{\"name\" : \"custid\",\"type\" : \"long\"},{\"name\" : \"branchid\",\"type\" : \"int\"},{\"name\" : \"accno\",\"type\" : \"long\"},{\"name\" : \"amount\",\"type\" : \"double\"},{\"name\" : \"balance\",\"type\" : \"double\"},{\"name\" : \"date\",\"type\" : \"int\"},{\"name\" : \"time\",\"type\" : \"int\"},{\"name\" : \"locationid\",\"type\" : \"int\"},{\"name\" : \"transtype\",\"type\" : \"string\"}]}";
}

class TransactionMsg(factory: MessageFactoryInterface, other: TransactionMsg) extends MessageInterface(factory) {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  private var keyTypes = Map("custid"-> "Long","branchid"-> "Int","accno"-> "Long","amount"-> "Double","balance"-> "Double","date"-> "Int","time"-> "Int","locationid"-> "Int","transtype"-> "String");

  override def save: Unit = { /* TransactionMsg.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { TransactionMsg.build(this) }

  override def getPartitionKey: Array[String] = {
    var partitionKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      partitionKeys += com.ligadata.BaseTypes.LongImpl.toString(get("custid").getValue.asInstanceOf[Long]);
    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };
    partitionKeys.toArray;

  }


  override def getPrimaryKey: Array[String] = Array[String]()

  var custid: Long = _;
  var branchid: Int = _;
  var accno: Long = _;
  var amount: Double = _;
  var balance: Double = _;
  var date: Int = _;
  var time: Int = _;
  var locationid: Int = _;
  var transtype: String = _;

  private def getWithReflection(key: String): AttributeValue = {
    var attributeValue = new AttributeValue();
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[TransactionMsg].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
      if (key.equals("custid")) { attributeValue.setValue(this.custid); }
      if (key.equals("branchid")) { attributeValue.setValue(this.branchid); }
      if (key.equals("accno")) { attributeValue.setValue(this.accno); }
      if (key.equals("amount")) { attributeValue.setValue(this.amount); }
      if (key.equals("balance")) { attributeValue.setValue(this.balance); }
      if (key.equals("date")) { attributeValue.setValue(this.date); }
      if (key.equals("time")) { attributeValue.setValue(this.time); }
      if (key.equals("locationid")) { attributeValue.setValue(this.locationid); }
      if (key.equals("transtype")) { attributeValue.setValue(this.transtype); }


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
        attributeVal.setValue(custid)
        attributeVal.setValueType(keyTypes("custid"))
        attributeValsMap.put("custid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(branchid)
        attributeVal.setValueType(keyTypes("branchid"))
        attributeValsMap.put("branchid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(accno)
        attributeVal.setValueType(keyTypes("accno"))
        attributeValsMap.put("accno", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(amount)
        attributeVal.setValueType(keyTypes("amount"))
        attributeValsMap.put("amount", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(balance)
        attributeVal.setValueType(keyTypes("balance"))
        attributeValsMap.put("balance", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(date)
        attributeVal.setValueType(keyTypes("date"))
        attributeValsMap.put("date", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(time)
        attributeVal.setValueType(keyTypes("time"))
        attributeValsMap.put("time", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(locationid)
        attributeVal.setValueType(keyTypes("locationid"))
        attributeValsMap.put("locationid", attributeVal)
      };
      {
        var attributeVal = new AttributeValue();
        attributeVal.setValue(transtype)
        attributeVal.setValueType(keyTypes("transtype"))
        attributeValsMap.put("transtype", attributeVal)
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
          attributeValue.setValue(this.custid);
          attributeValue.setValueType(keyTypes("custid"));
        }
        case 1 => {
          attributeValue.setValue(this.branchid);
          attributeValue.setValueType(keyTypes("branchid"));
        }
        case 2 => {
          attributeValue.setValue(this.accno);
          attributeValue.setValueType(keyTypes("accno"));
        }
        case 3 => {
          attributeValue.setValue(this.amount);
          attributeValue.setValueType(keyTypes("amount"));
        }
        case 4 => {
          attributeValue.setValue(this.balance);
          attributeValue.setValueType(keyTypes("balance"));
        }
        case 5 => {
          attributeValue.setValue(this.date);
          attributeValue.setValueType(keyTypes("date"));
        }
        case 6 => {
          attributeValue.setValue(this.time);
          attributeValue.setValueType(keyTypes("time"));
        }
        case 7 => {
          attributeValue.setValue(this.locationid);
          attributeValue.setValueType(keyTypes("locationid"));
        }
        case 8 => {
          attributeValue.setValue(this.transtype);
          attributeValue.setValueType(keyTypes("transtype"));
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

      if (key.equals("custid")) { this.custid = value.asInstanceOf[Long]; }
      if (key.equals("branchid")) { this.branchid = value.asInstanceOf[Int]; }
      if (key.equals("accno")) { this.accno = value.asInstanceOf[Long]; }
      if (key.equals("amount")) { this.amount = value.asInstanceOf[Double]; }
      if (key.equals("balance")) { this.balance = value.asInstanceOf[Double]; }
      if (key.equals("date")) { this.date = value.asInstanceOf[Int]; }
      if (key.equals("time")) { this.time = value.asInstanceOf[Int]; }
      if (key.equals("locationid")) { this.locationid = value.asInstanceOf[Int]; }
      if (key.equals("transtype")) { this.transtype = value.asInstanceOf[String]; }

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
        case 0 => {this.custid = value.asInstanceOf[Long];}
        case 1 => {this.branchid = value.asInstanceOf[Int];}
        case 2 => {this.accno = value.asInstanceOf[Long];}
        case 3 => {this.amount = value.asInstanceOf[Double];}
        case 4 => {this.balance = value.asInstanceOf[Double];}
        case 5 => {this.date = value.asInstanceOf[Int];}
        case 6 => {this.time = value.asInstanceOf[Int];}
        case 7 => {this.locationid = value.asInstanceOf[Int];}
        case 8 => {this.transtype = value.asInstanceOf[String];}

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

  private def fromFunc(other: TransactionMsg): TransactionMsg = {
    this.custid = com.ligadata.BaseTypes.LongImpl.Clone(other.custid);
    this.branchid = com.ligadata.BaseTypes.IntImpl.Clone(other.branchid);
    this.accno = com.ligadata.BaseTypes.LongImpl.Clone(other.accno);
    this.amount = com.ligadata.BaseTypes.DoubleImpl.Clone(other.amount);
    this.balance = com.ligadata.BaseTypes.DoubleImpl.Clone(other.balance);
    this.date = com.ligadata.BaseTypes.IntImpl.Clone(other.date);
    this.time = com.ligadata.BaseTypes.IntImpl.Clone(other.time);
    this.locationid = com.ligadata.BaseTypes.IntImpl.Clone(other.locationid);
    this.transtype = com.ligadata.BaseTypes.StringImpl.Clone(other.transtype);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: TransactionMsg) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}
