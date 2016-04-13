
/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.KamanjaBase
import java.net.URL
import java.net.URLClassLoader
import java.io.{ ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream }
import com.ligadata.Exceptions.KamanjaException
import com.ligadata.Utils.KamanjaClassLoader
import com.ligadata.kamanja.metadata.MdMgr
import org.apache.logging.log4j._
import java.util.Date
import java.util.Calendar
import java.text.SimpleDateFormat

import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock;

/*trait MessageContainerBase {
  // System Columns
  var transactionId: Long
  var timePartitionData: Long = 0 // By default we are taking Java date with 0 milliseconds.
  var rowNumber: Int = 0 // This is unique value with in transactionId

  // System Attributes Functions
  final def TransactionId(transId: Long): Unit = { transactionId = transId }
  final def TransactionId(): Long = transactionId

  final def TimePartitionData(timeInMillisecs: Long): Unit = { timePartitionData = timeInMillisecs }
  final def TimePartitionData(): Long = timePartitionData

  final def RowNumber(rno: Int): Unit = { rowNumber = rno }
  final def RowNumber(): Int = rowNumber

  def isMessage: Boolean
  def isContainer: Boolean
  def IsFixed: Boolean
  def IsKv: Boolean
  def CanPersist: Boolean
  def populate(inputdata: InputData): Unit
  def set(key: String, value: Any): Unit
  def get(key: String): Any
  def getOrElse(key: String, default: Any): Any
  def AddMessage(childPath: Array[(String, String)], msg: BaseMsg): Unit
  def GetMessage(childPath: Array[(String, String)], primaryKey: Array[String]): BaseMsg
  def Version: String // Message or Container Version
  def PartitionKeyData: Array[String] // Partition key data
  def PrimaryKeyData: Array[String] // Primary key data
  def FullName: String // Message or Container Full Name
  def NameSpace: String // Message or Container NameSpace
  def Name: String // Message or Container Name
  def Deserialize(dis: DataInputStream, mdResolver: MdBaseResolveInfo, loader: java.lang.ClassLoader, savedDataVersion: String): Unit
  def Serialize(dos: DataOutputStream): Unit
  def Save(): Unit
  def Clone(): MessageContainerBase
  def hasPrimaryKey: Boolean
  def hasPartitionKey: Boolean
  def hasTimeParitionInfo: Boolean
  def getNativeKeyValues: scala.collection.immutable.Map[String, (String, Any)]
}

trait MessageContainerObjBase {
  def isMessage: Boolean
  def isContainer: Boolean
  def IsFixed: Boolean
  def IsKv: Boolean
  def CanPersist: Boolean
  def FullName: String // Message or Container FullName
  def NameSpace: String // Message or Container NameSpace
  def Name: String // Message or Container Name
  def Version: String // Message or Container Version
  def PartitionKeyData(inputdata: InputData): Array[String] // Partition key data
  def PrimaryKeyData(inputdata: InputData): Array[String] // Primary key data
  def getTimePartitionInfo: (String, String, String) // FieldName, Format & Time Partition Types(Daily/Monthly/Yearly)
  def TimePartitionData(inputdata: InputData): Long
  def hasPrimaryKey: Boolean
  def hasPartitionKey: Boolean
  def hasTimeParitionInfo: Boolean

  private def extractTime(fieldData: String, timeFormat: String): Long = {
    if (fieldData == null || fieldData.trim() == "") return 0

    if (timeFormat == null || timeFormat.trim() == "") return 0

    if (timeFormat.compareToIgnoreCase("epochtimeInMillis") == 0)
      return fieldData.toLong

    if (timeFormat.compareToIgnoreCase("epochtimeInSeconds") == 0 || timeFormat.compareToIgnoreCase("epochtime") == 0)
      return fieldData.toLong * 1000

    // Now assuming Date partition format exists.
    val dtFormat = new SimpleDateFormat(timeFormat);
    val tm =
      if (fieldData.size == 0) {
        new Date(0)
      } else {
        dtFormat.parse(fieldData)
      }
    tm.getTime()
  }

  def ComputeTimePartitionData(fieldData: String, timeFormat: String, timePartitionType: String): Long = {
    val fldTimeDataInMs = extractTime(fieldData, timeFormat)

    // Align to Partition
    var cal: Calendar = Calendar.getInstance();
    cal.setTime(new Date(fldTimeDataInMs));

    if (timePartitionType == null || timePartitionType.trim() == "") return 0

    timePartitionType.toLowerCase match {
      case "yearly" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(Calendar.YEAR, cal.get(Calendar.YEAR));
        return newcal.getTime().getTime()
      }
      case "monthly" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(Calendar.YEAR, cal.get(Calendar.YEAR))
        newcal.set(Calendar.MONTH, cal.get(Calendar.MONTH))
        return newcal.getTime().getTime()
      }
      case "daily" => {
        var newcal: Calendar = Calendar.getInstance();
        newcal.setTimeInMillis(0)
        newcal.set(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH), cal.get(Calendar.DAY_OF_MONTH))
        return newcal.getTime().getTime()
      }
    }
    return 0
  }

}
*/

trait MdBaseResolveInfo {
  def getMessgeOrContainerInstance(typName: String): ContainerInterface
  // Get Latest SchemaId For Type
  // getMessgeOrContainerInstance for SchemaId
  // Convert to LatestVersion (Take any object and tries to covert to new version)
}


/*
object SerializeDeserialize {

  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)
  def Serialize(inst: MessageContainerBase): Array[Byte] = {
    val bos: ByteArrayOutputStream = new ByteArrayOutputStream(1024 * 1024)
    val dos = new DataOutputStream(bos)

    try {
      dos.writeUTF(inst.FullName)
      dos.writeUTF(inst.Version)
      dos.writeUTF(inst.getClass.getName)
      inst.Serialize(dos)
      val arr = bos.toByteArray
      dos.close
      bos.close
      return arr

    } catch {
      case e: Exception => {
        //LOG.error("Failed to get classname :" + clsName, e)
        logger.debug("Failed to Serialize", e)
        dos.close
        bos.close
        throw e
      }
    }
    null
  }

  def Serialize(inst: MessageContainerBase, dos: DataOutputStream): Unit = {
    try {
      dos.writeUTF(inst.FullName)
      dos.writeUTF(inst.Version)
      dos.writeUTF(inst.getClass.getName)
      inst.Serialize(dos)
    } catch {
      case e: Exception => {
        //LOG.error("Failed to get classname :" + clsName, e)
        logger.debug("Failed to Serialize", e)
        throw e
      }
    }
  }

  def Deserialize(bytearray: Array[Byte], mdResolver: MdBaseResolveInfo, loader: java.lang.ClassLoader, isTopObject: Boolean, desClassName: String): MessageContainerBase = {
    var dis = new DataInputStream(new ByteArrayInputStream(bytearray));

    val typName = dis.readUTF
    val version = dis.readUTF
    val classname = dis.readUTF
    try {
      // Expecting type name
      // get class instance for this type
      val typ =
        if (isTopObject) {
          mdResolver.getMessgeOrContainerInstance(typName)
        } else {
          try {
            Class.forName(desClassName, true, loader)
          } catch {
            case e: Exception => {
              logger.error("Failed to load Message/Container class %s".format(desClassName), e)
              throw e // Rethrow
            }
          }
          var curClz = Class.forName(desClassName, true, loader)
          curClz.newInstance().asInstanceOf[MessageContainerBase]
        }
      if (typ == null) {
        throw new Exception("Message/Container %s not found to deserialize".format(typName))
      }
      typ.Deserialize(dis, mdResolver, loader, version.toString)
      dis.close
      return typ
    } catch {
      case e: Exception => {
        // LOG.error("Failed to get classname :" + clsName, e)
        logger.debug("Failed to Deserialize", e)
        dis.close
        throw e
      }
    }
    null
  }
}
*/

/*trait BaseContainer extends MessageContainerBase {
  override def isMessage: Boolean = false
  override def isContainer: Boolean = true
}

trait BaseContainerObj extends MessageContainerObjBase {
  override def isMessage: Boolean = false
  override def isContainer: Boolean = true
  def CreateNewContainer: BaseContainer
}

trait BaseMsg extends MessageContainerBase {
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
}

trait BaseMsgObj extends MessageContainerObjBase {
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  def NeedToTransformData: Boolean // Filter & Rearrange input attributes if needed
  def CreateNewMessage: BaseMsg
}
*/
trait AdaptersSerializeDeserializers {
  var mdMgr: MdMgr = _
  var classLoader: KamanjaClassLoader = _
  private var msgBindings = scala.collection.mutable.Map[String, String]() // For now we are mapping to one message to one serializer. Later may be we need to handle multiple serializers
  private val reent_lock = new ReentrantReadWriteLock(true)

  private def ReadLock(reent_lock: ReentrantReadWriteLock): Unit = {
    if (reent_lock != null)
      reent_lock.readLock().lock()
  }

  private def ReadUnlock(reent_lock: ReentrantReadWriteLock): Unit = {
    if (reent_lock != null)
      reent_lock.readLock().unlock()
  }

  private def WriteLock(reent_lock: ReentrantReadWriteLock): Unit = {
    if (reent_lock != null)
      reent_lock.writeLock().lock()
  }

  private def WriteUnlock(reent_lock: ReentrantReadWriteLock): Unit = {
    if (reent_lock != null)
      reent_lock.writeLock().unlock()
  }

  final def setMdMgrAndClassLoader(mdMgr: MdMgr, classLoader: KamanjaClassLoader): Unit = {
    this.mdMgr = mdMgr
    this.classLoader = classLoader
//    resolveBinding()
  }

//  private def resolveBinding(): Unit = {
//    //FIXME:- Do we need to lock here & resolve Bindings
//  }

  final def getAllMessageBindings: Map[String, String] = {
    var retVal: Map[String, String] = null
    ReadLock(reent_lock)
    try {
      retVal = msgBindings.toMap
    } catch {
      case e: Throwable => {
        throw e
      }
    }
    finally {
      ReadUnlock(reent_lock)
    }
    return retVal
  }

  final def getMessageBinding(msgName: String): String = {
    if (msgName == null) return null

    var retVal: String = null
    ReadLock(reent_lock)
    try {
      retVal = msgBindings.getOrElse(msgName, null)
    } catch {
      case e: Throwable => {
        throw e
      }
    }
    finally {
      ReadUnlock(reent_lock)
    }
    return retVal
  }

  // This tuple has Message Name, Serializer Name.
  final def addMessageBinding(msgName: String, serName: String): Unit = {
    if (msgName != null && serName != null)
      addMessageBinding(Array((msgName, serName)))
  }

  // This tuple has Message Name, Serializer Name.
  final def addMessageBinding(binding: (String, String)): Unit = {
    if (binding != null)
      addMessageBinding(Array(binding))
  }

  // This tuple has Message Name, Serializer Name.
  final def addMessageBinding(bindings: Array[(String, String)]): Unit = {
    if (bindings == null) return

    WriteLock(reent_lock)
    try {
      msgBindings ++= bindings
    } catch {
      case e: Throwable => {
        throw e
      }
    }
    finally {
      WriteUnlock(reent_lock)
    }
  }

  // This tuple has Message Name, Serializer Name.
  final def removeMessageBinding(msgName: String, serName: String): Unit = {
    if (msgName != null && serName != null)
      removeMessageBinding(Array((msgName, serName)))
  }

  // This tuple has Message Name, Serializer Name.
  final def removeMessageBinding(binding: (String, String)): Unit = {
    if (binding != null)
      removeMessageBinding(Array(binding))
  }

  // This tuple has Message Name, Serializer Name.
  final def removeMessageBinding(bindings: Array[(String, String)]): Unit = {
    if (bindings == null) return

    WriteLock(reent_lock)
    try {
      msgBindings --= bindings.map(b => b._1)
    } catch {
      case e: Throwable => {
        throw e
      }
    }
    finally {
      WriteUnlock(reent_lock)
    }
  }

  // This tuple has Message Name, Serializer Name.
  final def removeMessageBinding(msgName: String): Unit = {
    if (msgName != null)
      removeMessageBinding(Array(msgName))
  }

  // This tuple has Message Name, Serializer Name.
  final def removeMessageBinding(msgNames: Array[String]): Unit = {
    if (msgNames == null) return

    WriteLock(reent_lock)
    try {
      msgBindings --= msgNames
    } catch {
      case e: Throwable => {
        throw e
      }
    }
    finally {
      WriteUnlock(reent_lock)
    }
  }

  // Returns serialized msgs, serialized msgs data & serializers names applied on these messages.
  final def serialize(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface]): (Array[ContainerInterface], Array[Array[Byte]], Array[String]) = {
    if (outputContainers == null || outputContainers.size == 0) return (Array[ContainerInterface](), Array[Array[Byte]](), Array[String]())

    val serOutputContainers = ArrayBuffer[ContainerInterface]()
    val serializedContainerData = ArrayBuffer[Array[Byte]]()
    val usedSerializersNames = ArrayBuffer[String]()

    val allMsgBindings = getAllMessageBindings

    outputContainers.map(c => {
      val ser = allMsgBindings.getOrElse(c.getFullTypeName, null)
      if (ser != null) {


      }
    })

    (serOutputContainers.toArray, serializedContainerData.toArray, usedSerializersNames.toArray)
  }

  // Returns serialized msgs, serialized msgs data & serializers names applied on these messages.
  final def serialize(tnxCtxt: TransactionContext, outputContainers: Array[ContainerInterface], serializersNames: Array[String]): (Array[ContainerInterface], Array[Array[Byte]], Array[String]) = {
    if ((outputContainers == null || outputContainers.size == 0) && (serializersNames == null || serializersNames.size == 0)) return (Array[ContainerInterface](), Array[Array[Byte]](), Array[String]())

    if (((outputContainers == null || outputContainers.size == 0) && !(serializersNames == null || serializersNames.size == 0)) ||
      (!(outputContainers == null || outputContainers.size == 0) && (serializersNames == null || serializersNames.size == 0)))
        throw new KamanjaException("Invalid input sizes", null)

    val serOutputContainers = ArrayBuffer[ContainerInterface]()
    val serializedContainerData = ArrayBuffer[Array[Byte]]()
    val usedSerializersNames = ArrayBuffer[String]()

    //FIXME:- yet to fix it

    (serOutputContainers.toArray, serializedContainerData.toArray, usedSerializersNames.toArray)
  }

  // Returns deserialized msg, deserialized msg data & deserializer name applied.
  final def deserialize(data: Array[Byte]): (ContainerInterface, String) = {

    var container: ContainerInterface = null
    var deserializerName: String = null

    //FIXME:- Convert incoming data into message using deserializer

    (container, deserializerName)
  }

  // Returns deserialized msg, deserialized msg data & deserializer name applied.
  final def deserialize(data: Array[Byte], deserializerName: String): (ContainerInterface, String) = {

    var container: ContainerInterface = null

    //FIXME:- Convert incoming data into message using deserializer

    (container, deserializerName)
  }
}

case class ContainerInterfaceWithModFlag(modified: Boolean, value: ContainerInterface)
