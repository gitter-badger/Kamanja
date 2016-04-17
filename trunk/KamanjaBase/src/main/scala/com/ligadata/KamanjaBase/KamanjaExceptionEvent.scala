
package com.ligadata.KamanjaBase;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase._;
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date

object KamanjaExceptionEvent extends RDDObject[KamanjaExceptionEvent] with MessageFactoryInterface {
  type T = KamanjaExceptionEvent;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaExceptionEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaExceptionEvent";
  override def getTypeVersion: String = "000001.000002.000000";
  override def getSchemaId: Int = 1000004;
  override def createInstance: KamanjaExceptionEvent = new KamanjaExceptionEvent(KamanjaExceptionEvent);
  override def isFixed: Boolean = true;
  override def getContainerType: ContainerTypes.ContainerType = ContainerTypes.ContainerType.MESSAGE
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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.kamanjabase" , "name" : "kamanjaexceptionevent" , "fields":[{ "name" : "componentname" , "type" : "string"},{ "name" : "timeoferrorepochms" , "type" : "long"},{ "name" : "errortype" , "type" : "string"},{ "name" : "errorstring" , "type" : "string"}]}""";

//  final override def convertFrom(srcObj: Any): T = convertFrom(createInstance(), srcObj);
//
//  override def convertFrom(destObj: Any, srcObj: Any): ContainerInterface = {
//    try {
//      srcObj match {
//
//        case oldVerobj: com.ligadata.KamanjaBase.KamanjaExceptionEvent => { return convertToNewVer(oldVerobj); }
//        case _ => {
//          throw new Exception("Unhandled Version Found");
//        }
//      }
//    } catch {
//      case e: Exception => {
//        throw e
//      }
//    }
//    return null;
//  }
  private def convertToNewVer(oldVerobj: com.ligadata.KamanjaBase.KamanjaExceptionEvent): com.ligadata.KamanjaBase.KamanjaExceptionEvent = {
    return oldVerobj
  }

  def NeedToTransformData: Boolean = false
  override def FullName: String = getFullTypeName
  override def NameSpace: String = getTypeNameSpace
  override def Name: String = getTypeName
  override def Version: String = getTypeVersion
  def CreateNewMessage: BaseMsg = createInstance.asInstanceOf[BaseMsg];
  def CreateNewContainer: BaseContainer = null;
  def IsFixed: Boolean = true
  def IsKv: Boolean = false
  override def CanPersist: Boolean = false
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  override def PartitionKeyData(inputdata: InputData): Array[String] = { throw new Exception("Deprecated method PartitionKeyData in obj IdCodeDimFixedTest") };
  override def PrimaryKeyData(inputdata: InputData): Array[String] = throw new Exception("Deprecated method PrimaryKeyData in obj IdCodeDimFixedTest");
  override def TimePartitionData(inputdata: InputData): Long = throw new Exception("Deprecated method TimePartitionData in obj IdCodeDimFixedTest");

}

class KamanjaExceptionEvent(factory: MessageFactoryInterface, other: KamanjaExceptionEvent) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)

  var attributeTypes = generateAttributeTypes;
  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* KamanjaExceptionEvent.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { KamanjaExceptionEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](4);
    attributeTypes(0) = new AttributeTypeInfo("componentname", 0, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes(1) = new AttributeTypeInfo("timeoferrorepochms", 1, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(2) = new AttributeTypeInfo("errortype", 2, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)
    attributeTypes(3) = new AttributeTypeInfo("errorstring", 3, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)

    return attributeTypes
  }

  var componentname: String = _;
  var timeoferrorepochms: Long = _;
  var errortype: String = _;
  var errorstring: String = _;

  override def getAttributeTypes(): Array[AttributeTypeInfo] = {
    if (attributeTypes == null) return null;
    return attributeTypes
  }
  override def getAttributeType(name: String): AttributeTypeInfo = {
    if (name == null || name.trim() == "") return null;
    attributeTypes.foreach(attributeType => {
      if (attributeType.getName == name.toLowerCase())
        return attributeType
    })
    return null;
  }

  private def getWithReflection(key: String): AnyRef = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[KamanjaExceptionEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
    try {
      attributeVals(0) = new AttributeValue(this.componentname, keyTypes("componentname"))
      attributeVals(1) = new AttributeValue(this.timeoferrorepochms, keyTypes("timeoferrorepochms"))
      attributeVals(2) = new AttributeValue(this.errortype, keyTypes("errortype"))
      attributeVals(3) = new AttributeValue(this.errorstring, keyTypes("errorstring"))

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

  override def get(index: Int): AnyRef = { // Return (value, type)
    try {
      index match {
        case 0 => return this.componentname.asInstanceOf[AnyRef];
        case 1 => return this.timeoferrorepochms.asInstanceOf[AnyRef];
        case 2 => return this.errortype.asInstanceOf[AnyRef];
        case 3 => return this.errorstring.asInstanceOf[AnyRef];

        case _ => throw new Exception(s"$index is a bad index for message KamanjaExceptionEvent");
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

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message KamanjaExceptionEvent")
      set(keyTypes(key).getIndex, value);

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  def set(index: Int, value: Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message KamanjaExceptionEvent ")
    try {
      index match {
        case 0 => {
          if (value.isInstanceOf[String])
            this.componentname = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaExceptionEvent")
        }
        case 1 => {
          if (value.isInstanceOf[Long])
            this.timeoferrorepochms = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaExceptionEvent")
        }
        case 2 => {
          if (value.isInstanceOf[String])
            this.errortype = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaExceptionEvent")
        }
        case 3 => {
          if (value.isInstanceOf[String])
            this.errorstring = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaExceptionEvent")
        }

        case _ => throw new Exception(s"$index is a bad index for message KamanjaExceptionEvent");
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

  private def fromFunc(other: KamanjaExceptionEvent): KamanjaExceptionEvent = {
    this.componentname = com.ligadata.BaseTypes.StringImpl.Clone(other.componentname);
    this.timeoferrorepochms = com.ligadata.BaseTypes.LongImpl.Clone(other.timeoferrorepochms);
    this.errortype = com.ligadata.BaseTypes.StringImpl.Clone(other.errortype);
    this.errorstring = com.ligadata.BaseTypes.StringImpl.Clone(other.errorstring);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }

  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaExceptionEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}