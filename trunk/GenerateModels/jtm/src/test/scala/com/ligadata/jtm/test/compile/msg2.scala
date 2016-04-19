package com.ligadata.kamanja.test.V1000000;

import org.json4s.jackson.JsonMethods._;
import org.json4s.DefaultFormats;
import org.json4s.Formats;
import com.ligadata.KamanjaBase._;
import com.ligadata.BaseTypes._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date;
import java.io.{ DataInputStream, DataOutputStream, ByteArrayOutputStream }



object msg2 extends RDDObject[msg2] with MessageFactoryInterface {
  type T = msg2 ;
  override def getFullTypeName: String = "com.ligadata.kamanja.test.msg2";
  override def getTypeNameSpace: String = "com.ligadata.kamanja.test";
  override def getTypeName: String = "msg2";
  override def getTypeVersion: String = "000000.000001.000000";
  override def getSchemaId: Int = 0;
  override def createInstance: msg2 = new msg2(msg2);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerTypes.ContainerType = ContainerTypes.ContainerType.MESSAGE
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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.kamanja.test" , "name" : "msg2" , "fields":[{ "name" : "out1" , "type" : "int"},{ "name" : "out2" , "type" : "string"},{ "name" : "out3" , "type" : "int"},{ "name" : "out4" , "type" : "int"}]}""";

  override def FullName: String = getFullTypeName
  override def NameSpace: String = getTypeNameSpace
  override def Name: String = getTypeName
  override def Version: String = getTypeVersion
  override def CreateNewMessage: BaseMsg= createInstance.asInstanceOf[BaseMsg];
  override def CreateNewContainer: BaseContainer= null;
  override def IsFixed: Boolean = true
  override def IsKv: Boolean = false
  override def CanPersist: Boolean = false
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  override def PartitionKeyData(inputdata: InputData): Array[String] = createInstance.getPartitionKey();
  override def PrimaryKeyData(inputdata: InputData): Array[String] = createInstance.getPrimaryKey();
  override def TimePartitionData(inputdata: InputData): Long = createInstance.getTimePartitionData;
  override def NeedToTransformData: Boolean = false
}

class msg2(factory: MessageFactoryInterface, other: msg2) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)

  var attributeTypes = generateAttributeTypes;

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](4);
    attributeTypes(0) = new AttributeTypeInfo("out1", 0, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(1) = new AttributeTypeInfo("out2", 1, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes(2) = new AttributeTypeInfo("out3", 2, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)
    attributeTypes(3) = new AttributeTypeInfo("out4", 3, AttributeTypeInfo.TypeCategory.INT, 0, 0, 0)


    return attributeTypes
  }

  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* msg2.saveOne(this) */}

  def Clone(): ContainerOrConcept = { msg2.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  override def getAttributeType(name: String): AttributeTypeInfo = {
    if (name == null || name.trim() == "") return null;
    attributeTypes.foreach(attributeType => {
      if(attributeType.getName == name.toLowerCase())
        return attributeType
    })
    return null;
  }


  var out1: Int = _;
  var out2: String = _;
  var out3: Int = _;
  var out4: Int = _;

  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    if (attributeTypes == null) return null;
    return attributeTypes
  }

  private def getWithReflection(key: String): AnyRef = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[msg2].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
        case 0 => return this.out1.asInstanceOf[AnyRef];
        case 1 => return this.out2.asInstanceOf[AnyRef];
        case 2 => return this.out3.asInstanceOf[AnyRef];
        case 3 => return this.out4.asInstanceOf[AnyRef];

        case _ => throw new Exception(s"$index is a bad index for message msg2");
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
  var attributeVals = new Array[AttributeValue](4);
    try{
      attributeVals(0) = new AttributeValue(this.out1, keyTypes("out1"))
      attributeVals(1) = new AttributeValue(this.out2, keyTypes("out2"))
      attributeVals(2) = new AttributeValue(this.out3, keyTypes("out3"))
      attributeVals(3) = new AttributeValue(this.out4, keyTypes("out4"))

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

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message msg2")
      set(keyTypes(key).getIndex, value);

    }catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }


  def set(index : Int, value :Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message msg2 ")
    try{
      index match {
        case 0 => {
          if(value.isInstanceOf[Int])
            this.out1 = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message msg2")
        }
        case 1 => {
          if(value.isInstanceOf[String])
            this.out2 = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message msg2")
        }
        case 2 => {
          if(value.isInstanceOf[Int])
            this.out3 = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message msg2")
        }
        case 3 => {
          if(value.isInstanceOf[Int])
            this.out4 = value.asInstanceOf[Int];
          else throw new Exception(s"Value is the not the correct type for index $index in message msg2")
        }

        case _ => throw new Exception(s"$index is a bad index for message msg2");
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

  private def fromFunc(other: msg2): msg2 = {
    this.out1 = com.ligadata.BaseTypes.IntImpl.Clone(other.out1);
    this.out2 = com.ligadata.BaseTypes.StringImpl.Clone(other.out2);
    this.out3 = com.ligadata.BaseTypes.IntImpl.Clone(other.out3);
    this.out4 = com.ligadata.BaseTypes.IntImpl.Clone(other.out4);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }


  def this(factory:MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: msg2) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}
