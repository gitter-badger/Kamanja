package com.ligadata.kamanja.samples.messages.V1000000;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeTypeInfo, AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.kamanja.samples.messages" , "name" : "transactionmsg" , "fields":[{ "name" : "custid" , "type" : "long"},{ "name" : "branchid" , "type" : "int"},{ "name" : "accno" , "type" : "long"},{ "name" : "amount" , "type" : "double"},{ "name" : "balance" , "type" : "double"},{ "name" : "date" , "type" : "int"},{ "name" : "time" , "type" : "int"},{ "name" : "locationid" , "type" : "int"},{ "name" : "transtype" , "type" : "string"}]}""";
}

class TransactionMsg(factory: MessageFactoryInterface, other: TransactionMsg) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)

  var attributeTypes = generateAttributeTypes;

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](9);
    attributeTypes(0) = new AttributeTypeInfo("custid", 0, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(1) = new AttributeTypeInfo("branchid", 1, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(2) = new AttributeTypeInfo("accno", 2, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(3) = new AttributeTypeInfo("amount", 3, AttributeTypeInfo.TypeCategory.DOUBLE, 3, 3, 0)
    attributeTypes(4) = new AttributeTypeInfo("balance", 4, AttributeTypeInfo.TypeCategory.DOUBLE, 3, 3, 0)
    attributeTypes(5) = new AttributeTypeInfo("date", 5, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(6) = new AttributeTypeInfo("time", 6, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(7) = new AttributeTypeInfo("locationid", 7, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(8) = new AttributeTypeInfo("transtype", 8, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)


    return attributeTypes
  }

  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* TransactionMsg.saveOne(this) */}

  def Clone(): ContainerOrConcept = { TransactionMsg.build(this) }

  override def getPartitionKey: Array[String] = {
    var partitionKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();
    try {
      partitionKeys += com.ligadata.BaseTypes.LongImpl.toString(get("custid").asInstanceOf[Long]);
    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };
    partitionKeys.toArray;

  }


  override def getPrimaryKey: Array[String] = Array[String]()

  override def getAttributeType(name: String): AttributeTypeInfo = {
    if (name == null || name.trim() == "") return null;
    attributeTypes.foreach(attributeType => {
      if(attributeType.getName == name.toLowerCase())
        return attributeType
    })
    return null;
  }


  var custid: Long = _;
  var branchid: Int = _;
  var accno: Long = _;
  var amount: Double = _;
  var balance: Double = _;
  var date: Int = _;
  var time: Int = _;
  var locationid: Int = _;
  var transtype: String = _;

  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    if (attributeTypes == null) return null;
    return attributeTypes
  }

  private def getWithReflection(key: String): AnyRef = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[TransactionMsg].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    return fmX.get.asInstanceOf[AnyRef];
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

  private def getByName(key: String): AnyRef = {
    if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message/container hl7Fixed ");
    return get(keyTypes(key).getIndex)
  }

  override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return (value, type)
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


  override def get(index : Int) : AnyRef = { // Return (value, type)
    try{
      index match {
        case 0 => return this.custid.asInstanceOf[AnyRef];
        case 1 => return this.branchid.asInstanceOf[AnyRef];
        case 2 => return this.accno.asInstanceOf[AnyRef];
        case 3 => return this.amount.asInstanceOf[AnyRef];
        case 4 => return this.balance.asInstanceOf[AnyRef];
        case 5 => return this.date.asInstanceOf[AnyRef];
        case 6 => return this.time.asInstanceOf[AnyRef];
        case 7 => return this.locationid.asInstanceOf[AnyRef];
        case 8 => return this.transtype.asInstanceOf[AnyRef];

        case _ => throw new Exception(s"$index is a bad index for message TransactionMsg");
      }
    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

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
  var attributeVals = new Array[AttributeValue](9);
    try{
      attributeVals(0) = new AttributeValue(this.custid, keyTypes("custid"))
      attributeVals(1) = new AttributeValue(this.branchid, keyTypes("branchid"))
      attributeVals(2) = new AttributeValue(this.accno, keyTypes("accno"))
      attributeVals(3) = new AttributeValue(this.amount, keyTypes("amount"))
      attributeVals(4) = new AttributeValue(this.balance, keyTypes("balance"))
      attributeVals(5) = new AttributeValue(this.date, keyTypes("date"))
      attributeVals(6) = new AttributeValue(this.time, keyTypes("time"))
      attributeVals(7) = new AttributeValue(this.locationid, keyTypes("locationid"))
      attributeVals(8) = new AttributeValue(this.transtype, keyTypes("transtype"))

    }catch {
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

  override def set(key: String, value: Any) = {
    try {

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message TransactionMsg")
      set(keyTypes(key).getIndex, value);

    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }


  def set(index : Int, value :Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message TransactionMsg ")
    try{
      index match {
        case 0 => {
          if(value.isInstanceOf[Long])
            this.custid = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 1 => {
          if(value.isInstanceOf[Int])
            this.branchid = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 2 => {
          if(value.isInstanceOf[Long])
            this.accno = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 3 => {
          if(value.isInstanceOf[Double])
            this.amount = value.asInstanceOf[Double];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 4 => {
          if(value.isInstanceOf[Double])
            this.balance = value.asInstanceOf[Double];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 5 => {
          if(value.isInstanceOf[Int])
            this.date = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 6 => {
          if(value.isInstanceOf[Int])
            this.time = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 7 => {
          if(value.isInstanceOf[Int])
            this.locationid = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }
        case 8 => {
          if(value.isInstanceOf[String])
            this.transtype = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message TransactionMsg")
        }

        case _ => throw new Exception(s"$index is a bad index for message TransactionMsg");
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
