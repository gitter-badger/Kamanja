package com.ligadata.kamanja.test001.v1000000

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import scala.xml.XML
import scala.xml.Elem
import com.ligadata.KamanjaBase.{InputData, DelimitedData, JsonData, XmlData, KvData}
import com.ligadata.BaseTypes._
import com.ligadata.KamanjaBase.SerializeDeserialize
import java.io.{DataInputStream, DataOutputStream, ByteArrayOutputStream}
import com.ligadata.Exceptions.StackTrace
import org.apache.logging.log4j.{Logger, LogManager}
import java.util.Date
import com.ligadata.KamanjaBase.{BaseMsg, BaseMsgObj, TransformMessage, BaseContainer, MdBaseResolveInfo, MessageContainerBase, RDDObject, RDD, JavaRDDObject}

object msg1 extends RDDObject[msg1] with BaseMsgObj {
  override def TransformDataAttributes: TransformMessage = null

  override def NeedToTransformData: Boolean = false

  override def FullName: String = "com.ligadata.kamanja.test001.msg1"

  override def NameSpace: String = "com.ligadata.kamanja.test001"

  override def Name: String = "msg1"

  override def Version: String = "000000.000001.000000"

  override def CreateNewMessage: BaseMsg = new msg1()

  override def IsFixed: Boolean = true;

  override def IsKv: Boolean = false;

  override def CanPersist: Boolean = false;


  type T = msg1

  override def build = new T

  override def build(from: T) = new T(from.transactionId, from)

  override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this)


  val partitionKeys: Array[String] = Array[String]();

  override def PartitionKeyData(inputdata: InputData): Array[String] = Array[String]()

  val primaryKeys: Array[String] = Array[String]();

  override def PrimaryKeyData(inputdata: InputData): Array[String] = Array[String]()

  def getTimePartitionInfo: (String, String, String) = {
    // Column, Format & Types
    return (null, null, null)
  }

  override def TimePartitionData(inputdata: InputData): Long = 0


  override def hasPrimaryKey(): Boolean = {
    if (primaryKeys == null) return false;
    (primaryKeys.size > 0);
  }

  override def hasPartitionKey(): Boolean = {
    if (partitionKeys == null) return false;
    (partitionKeys.size > 0);
  }

  override def hasTimeParitionInfo(): Boolean = {
    val tmPartInfo = getTimePartitionInfo
    (tmPartInfo != null && tmPartInfo._1 != null && tmPartInfo._2 != null && tmPartInfo._3 != null);
  }


  override def getFullName = FullName

}

class msg1(var transactionId: Long, other: msg1) extends BaseMsg {
  override def IsFixed: Boolean = msg1.IsFixed;

  override def IsKv: Boolean = msg1.IsKv;

  override def CanPersist: Boolean = msg1.CanPersist;

  override def FullName: String = msg1.FullName

  override def NameSpace: String = msg1.NameSpace

  override def Name: String = msg1.Name

  override def Version: String = msg1.Version


  var in1: Int = _;
  var in2: Int = _;
  var in3: Int = _;

  override def PartitionKeyData: Array[String] = Array[String]()

  override def PrimaryKeyData: Array[String] = Array[String]()

  override def set(key: String, value: Any): Unit = {
    throw new Exception("set function is not yet implemented")
  }

  override def get(key: String): Any = {
    try {
      // Try with reflection
      return getWithReflection(key)
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("StackTrace:" + stackTrace)
        // Call By Name
        return getByName(key)
      }
    }
  }

  override def getOrElse(key: String, default: Any): Any = {
    throw new Exception("getOrElse function is not yet implemented")
  }

  private def getByName(key: String): Any = {
    try {
      if (key.equals("in1")) return in1;
      if (key.equals("in2")) return in2;
      if (key.equals("in3")) return in3;
      if (key.equals("transactionId")) return transactionId;

      if (key.equals("timePartitionData")) return timePartitionData;
      return null;
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
  }

  private def getWithReflection(key: String): Any = {
    val ru = scala.reflect.runtime.universe
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val im = m.reflect(this)
    val fieldX = ru.typeOf[msg1].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
    val fmX = im.reflectField(fieldX)
    fmX.get
  }

  private val LOG = LogManager.getLogger(getClass)

  override def AddMessage(childPath: Array[(String, String)], msg: BaseMsg): Unit = {}


  override def GetMessage(childPath: Array[(String, String)], primaryKey: Array[String]): com.ligadata.KamanjaBase.BaseMsg = {
    return null
  }


  override def Save: Unit = {
    msg1.saveOne(this)
  }

  var nativeKeyMap = scala.collection.mutable.Map[String, String](("in1", "in1"), ("in2", "in2"), ("in3", "in3"))

  override def getNativeKeyValues(): scala.collection.immutable.Map[String, (String, Any)] = {

    var keyValues: scala.collection.mutable.Map[String, (String, Any)] = scala.collection.mutable.Map[String, (String, Any)]()

    try {
      keyValues("in1") = ("in1", in1);
      keyValues("in2") = ("in2", in2);
      keyValues("in3") = ("in3", in3);

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return keyValues.toMap
  }

  def populate(inputdata: InputData) = {
    if (inputdata.isInstanceOf[DelimitedData])
      populateCSV(inputdata.asInstanceOf[DelimitedData])
    else if (inputdata.isInstanceOf[JsonData])
      populateJson(inputdata.asInstanceOf[JsonData])
    else if (inputdata.isInstanceOf[XmlData])
      populateXml(inputdata.asInstanceOf[XmlData])
    else if (inputdata.isInstanceOf[KvData])
      populateKvData(inputdata.asInstanceOf[KvData])
    else throw new Exception("Invalid input data")
    timePartitionData = ComputeTimePartitionData

  }

  private def populateCSV(inputdata: DelimitedData): Unit = {
    val list = inputdata.tokens
    val arrvaldelim = inputdata.delimiters.valueDelimiter
    try {
      if (list.size < 3) throw new Exception("Incorrect input data size")
      in1 = com.ligadata.BaseTypes.IntImpl.Input(list(inputdata.curPos));
      inputdata.curPos = inputdata.curPos + 1;
      in2 = com.ligadata.BaseTypes.IntImpl.Input(list(inputdata.curPos));
      inputdata.curPos = inputdata.curPos + 1;
      in3 = com.ligadata.BaseTypes.IntImpl.Input(list(inputdata.curPos));
      inputdata.curPos = inputdata.curPos + 1;

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("Stacktrace:" + stackTrace)
        throw e
      }
    }
  }

  private def populateJson(json: JsonData): Unit = {
    try {
      if (json == null || json.cur_json == null || json.cur_json == None) throw new Exception("Invalid json data")
      assignJsonData(json)
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("Stacktrace:" + stackTrace)
        throw e
      }
    }
  }

  def CollectionAsArrString(v: Any): Array[String] = {
    if (v.isInstanceOf[Set[_]]) {
      return v.asInstanceOf[Set[String]].toArray
    }
    if (v.isInstanceOf[List[_]]) {
      return v.asInstanceOf[List[String]].toArray
    }
    if (v.isInstanceOf[Array[_]]) {
      return v.asInstanceOf[Array[String]].toArray
    }
    throw new Exception("Unhandled Collection")
  }

  private def assignJsonData(json: JsonData): Unit = {
    type tList = List[String]
    type tMap = Map[String, Any]
    var list: List[Map[String, Any]] = null
    try {
      val mapOriginal = json.cur_json.get.asInstanceOf[Map[String, Any]]
      if (mapOriginal == null)
        throw new Exception("Invalid json data")

      val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
      mapOriginal.foreach(kv => {
        map(kv._1.toLowerCase()) = kv._2
      })

      in1 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in1", 0).toString);
      in2 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in2", 0).toString);
      in3 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in3", 0).toString);

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("Stacktrace:" + stackTrace)
        throw e
      }
    }
  }

  private def populateXml(xmlData: XmlData): Unit = {
    try {
      val xml = XML.loadString(xmlData.dataInput)
      if (xml == null) throw new Exception("Invalid xml data")
      val _in1val_ = (xml \\ "in1").text.toString
      if (_in1val_ != "") in1 = com.ligadata.BaseTypes.IntImpl.Input(_in1val_) else in1 = 0;
      val _in2val_ = (xml \\ "in2").text.toString
      if (_in2val_ != "") in2 = com.ligadata.BaseTypes.IntImpl.Input(_in2val_) else in2 = 0;
      val _in3val_ = (xml \\ "in3").text.toString
      if (_in3val_ != "") in3 = com.ligadata.BaseTypes.IntImpl.Input(_in3val_) else in3 = 0;

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("Stacktrace:" + stackTrace)
        throw e
      }
    }
  }

  private def populateKvData(kvData: KvData): Unit = {
    try {

      if (kvData == null)
        throw new Exception("Invalid KvData")

      val map: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map[String, String]();
      kvData.dataMap.foreach(kv => {
        map(kv._1.toLowerCase()) = kv._2
      });

      in1 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in1", 0).toString);
      in2 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in2", 0).toString);
      in3 = com.ligadata.BaseTypes.IntImpl.Input(map.getOrElse("in3", 0).toString);

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("Stacktrace:" + stackTrace)
        throw e
      }
    }
  }

  override def Serialize(dos: DataOutputStream): Unit = {
    try {
      com.ligadata.BaseTypes.IntImpl.SerializeIntoDataOutputStream(dos, in1);
      com.ligadata.BaseTypes.IntImpl.SerializeIntoDataOutputStream(dos, in2);
      com.ligadata.BaseTypes.IntImpl.SerializeIntoDataOutputStream(dos, in3);
      com.ligadata.BaseTypes.LongImpl.SerializeIntoDataOutputStream(dos, transactionId);

      com.ligadata.BaseTypes.LongImpl.SerializeIntoDataOutputStream(dos, timePartitionData);
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("StackTrace:" + stackTrace)
      }
    }
  }

  override def Deserialize(dis: DataInputStream, mdResolver: MdBaseResolveInfo, loader: java.lang.ClassLoader, savedDataVersion: String): Unit = {
    try {
      if (savedDataVersion == null || savedDataVersion.trim() == "")
        throw new Exception("Please provide Data Version")

      val prevVer = savedDataVersion.replaceAll("[.]", "").toLong
      val currentVer = Version.replaceAll("[.]", "").toLong


      if (prevVer == currentVer) {
        in1 = com.ligadata.BaseTypes.IntImpl.DeserializeFromDataInputStream(dis);
        in2 = com.ligadata.BaseTypes.IntImpl.DeserializeFromDataInputStream(dis);
        in3 = com.ligadata.BaseTypes.IntImpl.DeserializeFromDataInputStream(dis);
        transactionId = com.ligadata.BaseTypes.LongImpl.DeserializeFromDataInputStream(dis);

        timePartitionData = com.ligadata.BaseTypes.LongImpl.DeserializeFromDataInputStream(dis)


      } else throw new Exception("Current Message/Container Version " + currentVer + " should be greater than Previous Message Version " + prevVer + ".")

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        LOG.debug("StackTrace:" + stackTrace)
      }
    }
  }

  def ConvertPrevToNewVerObj(obj: Any): Unit = {}

  def within1(value: Int): msg1 = {
    this.in1 = value
    return this
  }

  def within2(value: Int): msg1 = {
    this.in2 = value
    return this
  }

  def within3(value: Int): msg1 = {
    this.in3 = value
    return this
  }

  private def fromFunc(other: msg1): msg1 = {
    in1 = com.ligadata.BaseTypes.IntImpl.Clone(other.in1);
    in2 = com.ligadata.BaseTypes.IntImpl.Clone(other.in2);
    in3 = com.ligadata.BaseTypes.IntImpl.Clone(other.in3);


    timePartitionData = com.ligadata.BaseTypes.LongImpl.Clone(other.timePartitionData);
    return this
  }

  def ComputeTimePartitionData: Long = {
    val tmPartInfo = msg1.getTimePartitionInfo
    if (tmPartInfo == null) return 0;
    msg1.ComputeTimePartitionData(" ", tmPartInfo._2, tmPartInfo._3)
  }

  def getTimePartition(): msg1 = {
    timePartitionData = ComputeTimePartitionData
    return this;
  }


  if (other != null && other != this) {
    // call copying fields from other to local variables
    fromFunc(other)
  }

  def this(txnId: Long) = {
    this(txnId, null)
  }

  def this(other: msg1) = {
    this(0, other)
  }

  def this() = {
    this(0, null)
  }

  override def Clone(): MessageContainerBase = {
    msg1.build(this)
  }


  override def hasPrimaryKey(): Boolean = {
    msg1.hasPrimaryKey;
  }

  override def hasPartitionKey(): Boolean = {
    msg1.hasPartitionKey;
  }

  override def hasTimeParitionInfo(): Boolean = {
    msg1.hasTimeParitionInfo;
  }

}
