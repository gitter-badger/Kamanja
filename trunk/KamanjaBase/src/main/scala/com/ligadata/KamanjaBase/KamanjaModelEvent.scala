
package com.ligadata.KamanjaBase;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase._;
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date

object KamanjaModelEvent extends RDDObject[KamanjaModelEvent] with MessageFactoryInterface {
  type T = KamanjaModelEvent;
  override def getFullTypeName: String = "com.ligadata.KamanjaBase.KamanjaModelEvent";
  override def getTypeNameSpace: String = "com.ligadata.KamanjaBase";
  override def getTypeName: String = "KamanjaModelEvent";
  override def getTypeVersion: String = "000001.000003.000000";
  override def getSchemaId: Int = 1000002;
  override def createInstance: KamanjaModelEvent = new KamanjaModelEvent(KamanjaModelEvent);
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

  override def getAvroSchema: String = """{ "type": "record",  "namespace" : "com.ligadata.kamanjabase" , "name" : "kamanjamodelevent" , "fields":[{ "name" : "modelid" , "type" : "long"},{ "name" : "elapsedtimeinms" , "type" : "float"},{ "name" : "eventepochtime" , "type" : "long"},{ "name" : "isresultproduced" ,},{ "name" : "producedmessages" , "type" :  {"type" : "array", "items" : "long"}},{ "name" : "error" , "type" : "string"}]}""";
 final override def convertFrom(srcObj: Any): T = convertFrom(createInstance(), srcObj);

  override def convertFrom(destObj: Any, srcObj: Any): ContainerInterface = {
    try {
      srcObj match {

        case oldVerobj: com.ligadata.KamanjaBase.KamanjaModelEvent => { return convertToNewVer(oldVerobj); }
        case _ => {
          throw new Exception("Unhandled Version Found");
        }
      }
    } catch {
      case e: Exception => {
        throw e
      }
    }
    return null;
  }
  private def convertToNewVer(oldVerobj: com.ligadata.KamanjaBase.KamanjaModelEvent): com.ligadata.KamanjaBase.KamanjaModelEvent = {
    return oldVerobj
  }

  /** Deprecated Methods **/
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
  override def PartitionKeyData(inputdata: InputData): Array[String] = { throw new Exception("Deprecated method PartitionKeyData in obj KamanjaModelEvent") };
  override def PrimaryKeyData(inputdata: InputData): Array[String] = throw new Exception("Deprecated method PrimaryKeyData in obj KamanjaModelEvent");
  override def TimePartitionData(inputdata: InputData): Long = throw new Exception("Deprecated method TimePartitionData in obj KamanjaModelEvent");
}

class KamanjaModelEvent(factory: MessageFactoryInterface, other: KamanjaModelEvent) extends MessageInterface(factory) {

  private val log = LogManager.getLogger(getClass)
  var attributeTypes = generateAttributeTypes;
  var keyTypes: Map[String, AttributeTypeInfo] = attributeTypes.map { a => (a.getName, a) }.toMap;

  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  override def save: Unit = { /* KamanjaModelEvent.saveOne(this) */ }

  def Clone(): ContainerOrConcept = { KamanjaModelEvent.build(this) }

  override def getPartitionKey: Array[String] = Array[String]()

  override def getPrimaryKey: Array[String] = Array[String]()

  private def generateAttributeTypes(): Array[AttributeTypeInfo] = {
    var attributeTypes = new Array[AttributeTypeInfo](6);
    attributeTypes(0) = new AttributeTypeInfo("modelid", 0, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(1) = new AttributeTypeInfo("elapsedtimeinms", 1, AttributeTypeInfo.TypeCategory.FLOAT, 2, 2, 0)
    attributeTypes(2) = new AttributeTypeInfo("eventepochtime", 2, AttributeTypeInfo.TypeCategory.LONG, 4, 4, 0)
    attributeTypes(3) = new AttributeTypeInfo("isresultproduced", 3, AttributeTypeInfo.TypeCategory.BOOLEAN, 7, 7, 0)
    attributeTypes(4) = new AttributeTypeInfo("producedmessages", 4, AttributeTypeInfo.TypeCategory.ARRAY, 4, 1003, 0)
    attributeTypes(5) = new AttributeTypeInfo("error", 5, AttributeTypeInfo.TypeCategory.STRING, 1, 1, 0)

    return attributeTypes
  }

  var modelid: Long = _;
  var elapsedtimeinms: Float = _;
  var eventepochtime: Long = _;
  var isresultproduced: Boolean = _;
  var producedmessages: scala.Array[Long] = _;
  var error: String = _;

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
    val fieldX = ru.typeOf[KamanjaModelEvent].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
    var attributeVals = new Array[AttributeValue](6);
    try {
      attributeVals(0) = new AttributeValue(this.modelid, keyTypes("modelid"))
      attributeVals(1) = new AttributeValue(this.elapsedtimeinms, keyTypes("elapsedtimeinms"))
      attributeVals(2) = new AttributeValue(this.eventepochtime, keyTypes("eventepochtime"))
      attributeVals(3) = new AttributeValue(this.isresultproduced, keyTypes("isresultproduced"))
      attributeVals(4) = new AttributeValue(this.producedmessages, keyTypes("producedmessages"))
      attributeVals(5) = new AttributeValue(this.error, keyTypes("error"))

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
        case 0 => return this.modelid.asInstanceOf[AnyRef];
        case 1 => return this.elapsedtimeinms.asInstanceOf[AnyRef];
        case 2 => return this.eventepochtime.asInstanceOf[AnyRef];
        case 3 => return this.isresultproduced.asInstanceOf[AnyRef];
        case 4 => return this.producedmessages.asInstanceOf[AnyRef];
        case 5 => return this.error.asInstanceOf[AnyRef];

        case _ => throw new Exception(s"$index is a bad index for message KamanjaModelEvent");
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

      if (!keyTypes.contains(key)) throw new Exception(s"Key $key does not exists in message KamanjaModelEvent")
      set(keyTypes(key).getIndex, value);

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    };

  }

  def set(index: Int, value: Any): Unit = {
    if (value == null) throw new Exception(s"Value is null for index $index in message KamanjaModelEvent ")
    try {
      index match {
        case 0 => {
          if (value.isInstanceOf[Long])
            this.modelid = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }
        case 1 => {
          if (value.isInstanceOf[Float])
            this.elapsedtimeinms = value.asInstanceOf[Float];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }
        case 2 => {
          if (value.isInstanceOf[Long])
            this.eventepochtime = value.asInstanceOf[Long];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }
        case 3 => {
          if (value.isInstanceOf[Boolean])
            this.isresultproduced = value.asInstanceOf[Boolean];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }
        case 4 => {
          if (value.isInstanceOf[scala.Array[Long]])
            this.producedmessages = value.asInstanceOf[scala.Array[Long]];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }
        case 5 => {
          if (value.isInstanceOf[String])
            this.error = value.asInstanceOf[String];
          else throw new Exception(s"Value is the not the correct type for index $index in message KamanjaModelEvent")
        }

        case _ => throw new Exception(s"$index is a bad index for message KamanjaModelEvent");
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

  private def fromFunc(other: KamanjaModelEvent): KamanjaModelEvent = {
    this.modelid = com.ligadata.BaseTypes.LongImpl.Clone(other.modelid);
    this.elapsedtimeinms = com.ligadata.BaseTypes.FloatImpl.Clone(other.elapsedtimeinms);
    this.eventepochtime = com.ligadata.BaseTypes.LongImpl.Clone(other.eventepochtime);
    this.isresultproduced = com.ligadata.BaseTypes.BoolImpl.Clone(other.isresultproduced);
    if (other.producedmessages != null) {
      producedmessages = new scala.Array[Long](other.producedmessages.length);
      producedmessages = other.producedmessages.map(v => com.ligadata.BaseTypes.LongImpl.Clone(v));
    } else this.producedmessages = null;
    this.error = com.ligadata.BaseTypes.StringImpl.Clone(other.error);

    //this.timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this;
  }

  def this(factory: MessageFactoryInterface) = {
    this(factory, null)
  }

  def this(other: KamanjaModelEvent) = {
    this(other.getFactory.asInstanceOf[MessageFactoryInterface], other)
  }

}