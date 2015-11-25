package com.ligadata.msgcompiler

class MessageConstants {

  val newline: String = "\n";
  val pad1: String = "\t";
  val pad2: String = "\t\t";
  val closeBrace: String = "}";
  val True: String = "true";
  val False: String = "false";
  val packageStr: String = "package %s; %s";
  val messageStr: String = "message";
  val containerStr: String = "container";
  val baseMsgObj: String = "BaseMsgObj";
  val baseMsg: String = "BaseMsg";
  val baseContainerObj: String = "BaseContainerObj";
  val baseContainer: String = "BaseContainer";
  val msgObjectStr: String = "object %s extends RDDObject[%s] with %s { %s";
  val classStr: String = "class %s(var transactionId: Long, other: %s) extends %s { %s";

  val template: String = "%s type T = %s ;%s";

  val fullName: String = "%s override def FullName: String = \"%s.%s\"; %s"; //com.ligadata.samples.messages.CustAlertHistory"
  val namespace: String = "%s override def NameSpace: String = \"%s\"; %s"; //com.ligadata.samples.messages"
  val name: String = "%s override def Name: String = \"%s\"; %s"; //CustAlertHistory"
  val version: String = "%s override def Version: String = \"%s\"; %s"; //000000.000001.000000"
  val createNewContainer = "%s override def CreateNewContainer: %s = new %s(); %s"; //BaseContainer = new CustAlertHistory()
  val createNewMessage = "%s override def CreateNewMessage: %s = new %s(); %s"; //BaseContainer = new CustAlertHistory()
  val isFixed: String = "%s override def IsFixed: Boolean = %s; %s"; //true;
  val isKV: String = "%s override def IsKv: Boolean = %s; %s"; //false;
  val canPersist: String = "%s override def CanPersist: Boolean = %s; %s"; //true;
  val getFullName: String = "%s override def getFullName = FullName; %s";
  val toJavaRDD: String = "%s override def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this); %s";

  val paritionKeyData: String = "override def PartitionKeyData: Array[String] = %s ";
  val primaryKeyData: String = "override def PrimaryKeyData: Array[String] = %s ";
  val partitionKeys: String = "%s val partitionKeys : Array[String] = Array%s; %s";
  val primaryKeys: String = "%s val primaryKeys : Array[String] = Array%s; %s";
  val partitionKeyVar: String = "%s var partitionKeysData: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();%s"
  val primaryKeyVar: String = "%s var primaryKeysData: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();%s"


  def importStatements() = {
    """
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import scala.xml.XML
import scala.xml.Elem
import com.ligadata.KamanjaBase.{ InputData, DelimitedData, JsonData, XmlData, KvData }
import com.ligadata.BaseTypes._
import com.ligadata.KamanjaBase.SerializeDeserialize
import java.io.{ DataInputStream, DataOutputStream, ByteArrayOutputStream }
import com.ligadata.Exceptions.StackTrace
import org.apache.log4j.Logger
import java.util.Date
import com.ligadata.KamanjaBase.{ BaseMsg, BaseMsgObj, TransformMessage, BaseContainer, BaseContainerObj, MdBaseResolveInfo, MessageContainerBase, RDDObject, RDD, JavaRDDObject }
    
    """
  }

  def msgObjectBuildStmts = {
    """
  def build = new T
  def build(from: T) = new T(from)

    """
  }

}