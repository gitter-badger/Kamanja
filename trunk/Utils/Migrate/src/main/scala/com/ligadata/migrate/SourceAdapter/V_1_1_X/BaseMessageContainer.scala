
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

package com.ligadata.Migrate.SourceAdapter.V_1_1_X

import java.net.URL
import java.net.URLClassLoader
import java.io.{ ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream }
import com.ligadata.Exceptions.StackTrace
import org.apache.log4j._
import com.ligadata.KamanjaBase.InputData

trait MessageContainerBase_V_1_1_X {
  var transactionId: Long
  def isMessage: Boolean
  def isContainer: Boolean
  def IsFixed: Boolean
  def IsKv: Boolean
  def CanPersist: Boolean
  def populate(inputdata: InputData): Unit
  def set(key: String, value: Any): Unit
  def get(key: String): Any
  def getOrElse(key: String, default: Any): Any
  def AddMessage(childPath: Array[(String, String)], msg: BaseMsg_V_1_1_X): Unit
  def GetMessage(childPath: Array[(String, String)], primaryKey: Array[String]): BaseMsg_V_1_1_X
  def Version: String // Message or Container Version
  def PartitionKeyData: Array[String] // Partition key data
  def PrimaryKeyData: Array[String] // Primary key data
  def FullName: String // Message or Container Full Name
  def NameSpace: String // Message or Container NameSpace
  def Name: String // Message or Container Name
  def Deserialize(dis: DataInputStream, mdResolver: MdBaseResolveInfo_V_1_1_X, loader: java.lang.ClassLoader, savedDataVersion: String): Unit
  def Serialize(dos: DataOutputStream): Unit
  def Save(): Unit
  def Clone(): MessageContainerBase_V_1_1_X
  final def TransactionId(transId: Long): Unit = { transactionId = transId }
  final def TransactionId(): Long = transactionId
}

trait MessageContainerObjBase_V_1_1_X {
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
}

trait MdBaseResolveInfo_V_1_1_X {
  def getMessgeOrContainerInstance(typName: String): MessageContainerBase_V_1_1_X
}

object SerializeDeserialize {
  val loggerName = this.getClass.getName
  val logger = Logger.getLogger(loggerName)
  def Serialize(inst: MessageContainerBase_V_1_1_X): Array[Byte] = {
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
        //LOG.error("Failed to get classname :" + clsName)
        logger.debug("StackTrace:" + StackTrace.ThrowableTraceString(e))
        dos.close
        bos.close
        throw e
      }
    }
    null
  }

  def Deserialize(bytearray: Array[Byte], mdResolver: MdBaseResolveInfo_V_1_1_X, loader: java.lang.ClassLoader, isTopObject: Boolean, desClassName: String): MessageContainerBase_V_1_1_X = {
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
              logger.error("Failed to load Message/Container class %s with Reason:%s Message:%s".format(desClassName, e.getCause, e.getMessage))
              throw e // Rethrow
            }
          }
          var curClz = Class.forName(desClassName, true, loader)
          curClz.newInstance().asInstanceOf[MessageContainerBase_V_1_1_X]
        }
      if (typ == null) {
        throw new Exception("Message/Container %s not found to deserialize".format(typName))
      }
      typ.Deserialize(dis, mdResolver, loader, version.toString)
      dis.close
      return typ
    } catch {
      case e: Exception => {
        // LOG.error("Failed to get classname :" + clsName)
        logger.debug("StackTrace:" + StackTrace.ThrowableTraceString(e))
        dis.close
        throw e
      }
    }
    null
  }
}

trait BaseContainer_V_1_1_X extends MessageContainerBase_V_1_1_X {
  override def isMessage: Boolean = false
  override def isContainer: Boolean = true
}

trait BaseContainerObj_V_1_1_X extends MessageContainerObjBase_V_1_1_X {
  override def isMessage: Boolean = false
  override def isContainer: Boolean = true
  def CreateNewContainer: BaseContainer_V_1_1_X
}

trait BaseMsg_V_1_1_X extends MessageContainerBase_V_1_1_X {
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
}

trait BaseMsgObj_V_1_1_X extends MessageContainerObjBase_V_1_1_X {
  override def isMessage: Boolean = true
  override def isContainer: Boolean = false
  def NeedToTransformData: Boolean // Filter & Rearrange input attributes if needed
  def TransformDataAttributes: TransformMessage_V_1_1_X // Filter & Rearrange input columns if needed
  def CreateNewMessage: BaseMsg_V_1_1_X
}

// BUGBUG:: for now handling only CSV input data.
// Assuming this is filled properly, we are not checking whether outputFields are subset of inputFields or not.
// Assuming the field names are all same case (lower or upper). Because we don't want to convert them every time.
class TransformMessage_V_1_1_X {
  var messageType: String = null // Type of the message (first field from incoming data)
  var inputFields: Array[String] = null // All input fields
  var outputFields: Array[String] = null // All output fields filters from input field. These are subset of input fields.
  var outputKeys: Array[String] = null // Output Key field names from input fields.
}

