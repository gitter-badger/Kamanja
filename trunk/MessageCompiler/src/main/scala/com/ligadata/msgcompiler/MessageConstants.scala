package com.ligadata.msgcompiler

import org.apache.logging.log4j.{ Logger, LogManager }

class MessageConstants {
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  val primitives = List("string", "int", "boolean", "float", "double", "long", "char");

  val newline: String = "\n";
  val pad1: String = "\t";
  val pad2: String = "\t\t";
  val pad3: String = "\t\t\t";
  val pad4: String = "\t\t\t\t";
  val closeBrace: String = "}";
  val True: String = "true";
  val False: String = "false";
  val packageStr: String = "package %s; %s";

  val messageStr: String = "message";
  val containerStr: String = "container";
  val baseMsgObj: String = "MessageFactoryInterface";
  val baseMsg: String = "MessageInterface(factory)";
  val baseContainerObj: String = "ContainerFactoryInterface";
  val baseContainer: String = "ContainerInterface(factory)";
  val msgObjectStr: String = "object %s extends RDDObject[%s] with %s { %s";
  val classStr: String = "class %s(factory: %s, other: %s) extends %s { %s";

  val template: String = "%stype T = %s ;%s";

  val fullName: String = "%soverride def getFullTypeName: String = \"%s.%s\"; %s"; //com.ligadata.samples.messages.CustAlertHistory"
  val namespace: String = "%soverride def getTypeNameSpace: String = \"%s\"; %s"; //com.ligadata.samples.messages"
  val name: String = "%soverride def getTypeName: String = \"%s\"; %s"; //CustAlertHistory"
  val version: String = "%soverride def getTypeVersion: String = \"%s\"; %s"; //000000.000001.000000"
  val schemaId: String = "%soverride def getSchemaId: Int = %s; %s"; //10
  val createInstance = "%soverride def createInstance: %s = new %s(%s); %s"; //ContainerInterface = new CustAlertHistory()
  val containerInstanceType = "ContainerInterface";
  val messageInstanceType = "MessageInterface";
  val getContainerTypeMsg = "%soverride def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE";
  val getContainerTypeContainer = "%soverride def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.CONTAINER";

  val createNewContainer = "%soverride def CreateNewContainer: %s = new %s(); %s"; //ContainerInterface = new CustAlertHistory()
  val createNewMessage = "%soverride def CreateNewMessage: %s = new %s(); %s"; //ContainerInterface = new CustAlertHistory()
  val isFixed: String = "%soverride def isFixed: Boolean = %s; %s"; //true;
  val isKV: String = "%soverride def IsKv: Boolean = %s; %s"; //false;
  val canPersist: String = "%soverride def CanPersist: Boolean = %s; %s"; //true;
  val getFullName: String = "%soverride def getFullName = getFullTypeName; %s";
  val toJavaRDD: String = "%soverride def toJavaRDDObject: JavaRDDObject[T] = JavaRDDObject.fromRDDObject[T](this); %s";

  val paritionKeyData: String = "%soverride def getPartitionKey: Array[String] = %s ";
  val primaryKeyData: String = "%soverride def getPrimaryKey: Array[String] = %s ";
  val getPartitionKeyNames: String = "Array%s; %s";
  val getPrimaryKeyNames: String = "Array%s; %s";
  val partitionKeyVar: String = "%svar partitionKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();%s"
  val primaryKeyVar: String = "%svar primaryKeys: scala.collection.mutable.ArrayBuffer[String] = scala.collection.mutable.ArrayBuffer[String]();%s"

  val rddFactoryClass: String = "%spublic final class %sFactory { %s";
  val rddObj: String = "%spublic static JavaRDDObject<%s> rddObject = %s$.MODULE$.toJavaRDDObject(); %s";
  val rddContainerFactoryInterface = "%spublic static ContainerFactoryInterface baseObj = (ContainerFactoryInterface) %s$.MODULE$; %s";
  val rddMessageFactoryInterface = "%spublic static MessageFactoryInterface baseObj = (MessageFactoryInterface) %s$.MODULE$; %s";
  val fieldsForMappedVar = "%svar fields: scala.collection.mutable.Map[String, (Int, Any)] = new scala.collection.mutable.HashMap[String, (Int, Any)];"

  def isFixedFunc(message: Message): Boolean = {
    if (message.Fixed.equalsIgnoreCase("true"))
      return true;
    else return false;
  }

  def isMessageFunc(message: Message): Boolean = {
    if (message.MsgType.equalsIgnoreCase("message"))
      return true;
    else return false;
  }

  def catchStmt = {
    """catch {
          case e: Exception => {
          log.debug("", e)
          throw e
        }
      };
      """
  }
  def rddObjImportStmts() = {
    """
  import com.ligadata.KamanjaBase.JavaRDDObject;
	import com.ligadata.KamanjaBase.MessageFactoryInterface;
	import com.ligadata.KamanjaBase.ContainerFactoryInterface;
    """
  }

  def importStatements() = {
    """
import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeTypeInfo, AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date
    
 """
  }

  def msgObjectBuildStmts = {
    """
    def build = new T(this)
    def build(from: T) = new T(from)
"""
  }
  /*
   * Get By Key method for Mapped Messages
   */

  private def getByNameFuncForMapped = {
    """
    override def get(key: String): AnyRef = { // Return (value, type)
      try {
        val value = valuesMap(key).getValue
        if (value == null) return null; else return value.asInstanceOf[AnyRef];  
       } catch {
        case e: Exception => {
          log.debug("", e)
          throw e
        }
      }      
    }
"""
  }

  /*
   * GetOrElse method by Key for mapped messages
   */
  private def getOrElseFuncForMapped = {
    """
    override def getOrElse(key: String, defaultVal: Any): AnyRef = { // Return (value, type)
      var attributeValue: AttributeValue = new AttributeValue();
      try {
        val value = valuesMap(key).getValue
        if (value == null) return defaultVal.asInstanceOf[AnyRef];
        return value.asInstanceOf[AnyRef];   
      } catch {
        case e: Exception => {
          log.debug("", e)
          throw e
        }
      }      
    }     
 """
  }

  /*
   * Get Attribute Names method for Mapped messages
   */
  private def getAttibuteNamesMapped = {
    """
    override def getAttributeNames(): Array[String] = {
      try {
        if (valuesMap.isEmpty) {
          return null;
        } else {
          return valuesMap.keySet.toArray;
        }
      } catch {
        case e: Exception => {
          log.debug("", e)
          throw e
        }
      }
    }  
"""
  }

  /*
   * Get By Index method for Mapped Messages
   */
  private def getByIndexMapped = {
    """
    override def get(index: Int): AnyRef = { // Return (value, type)
      throw new Exception("Get By Index is not supported in mapped messages");
    }

    override def getOrElse(index: Int, defaultVal: Any): AnyRef = { // Return (value,  type)
      throw new Exception("Get By Index is not supported in mapped messages");
    }  
    """
  }
  /*
   * Get all AttributeVlaues method for Mapped Messages
   */

  private def getAllAttributes = {
    """
    override def getAllAttributeValues(): Array[AttributeValue] = { // Has (name, value, type))
      return valuesMap.map(f => f._2).toArray;
    }  
    """
  }

  /*
   * Get Attribute Name and Value Iterator Method
   */
  private def getAttributeNameAndValueIteratorMapped = {
    """
    override def getAttributeNameAndValueIterator(): java.util.Iterator[AttributeValue] = {
      //valuesMap.iterator.asInstanceOf[java.util.Iterator[AttributeValue]];
      return null;
    }  
   """
  }

  /*
   * SetByKey method for Mapped Messages
   */

  private def setByNameFuncForMappedMsgs() = {
    """
    override def set(key: String, value: Any) = {
      try {
       val keyName: String = key.toLowerCase();
        if (keyTypes.contains(key)) {
          valuesMap(key) = new AttributeValue(value, keyTypes(keyName))
        } else {
          valuesMap(key) = new AttributeValue(ValueToString(value), new AttributeTypeInfo(key, -1, AttributeTypeInfo.TypeCategory.STRING, 0, 0, 0))
        }
      } catch {
        case e: Exception => {
          log.debug("", e)
          throw e
        }
      }
    }
"""
  }

  /*
   * Set Value and ValueType By Key method for Mapped msgs
   */
  private def setValueAndValTypByKeyMapped = {
    """
    override def set(key: String, value: Any, valTyp: String) = {
       try{
         val keyName: String = key.toLowerCase();
         if (keyTypes.contains(key)) {
           valuesMap(key) = new AttributeValue(value, keyTypes(keyName))
         } else {
           val typeCategory = AttributeTypeInfo.TypeCategory.valueOf(valTyp.toUpperCase())
           val keytypeId = typeCategory.getValue.toShort
           val valtypeId = typeCategory.getValue.toShort
           valuesMap(key) = new AttributeValue(value, new AttributeTypeInfo(key, -1, typeCategory, valtypeId, keytypeId, 0))
          }
        } catch {
          case e: Exception => {
            log.debug("", e)
            throw e
          }
        }
      }  
    """
  }

  /*
   * Set By Index method for mapped messages
   */
  private def setByIndex = {
    """
    override def set(index: Int, value: Any) = {
      throw new Exception("Set By Index is not supported in mapped messages");
    } 
  """
  }

  def valuesMapMapped = {
    """
    var valuesMap = scala.collection.mutable.Map[String, AttributeValue]()
 """
  }

  /*
   * ValueToString method for mapped messages
   */
  private def ValueToString = {
    """
    private def ValueToString(v: Any): String = {
      if (v.isInstanceOf[Set[_]]) {
        return v.asInstanceOf[Set[_]].mkString(",")
      }
      if (v.isInstanceOf[List[_]]) {
        return v.asInstanceOf[List[_]].mkString(",")
      }
      if (v.isInstanceOf[Array[_]]) {
        return v.asInstanceOf[Array[_]].mkString(",")
      }
      v.toString
    }  
    """
  }

  /*
   * All Get Set methods for mapped messages
   */

  def getGetSetMethods: String = {
    var getSetMapped = new StringBuilder(8 * 1024)
    try {
      getSetMapped.append(valuesMapMapped);
      getSetMapped.append(getAttributeTypesMethodMapped)
      getSetMapped.append(getByNameFuncForMapped)
      getSetMapped.append(getOrElseFuncForMapped)
      getSetMapped.append(getAttibuteNamesMapped)
      getSetMapped.append(getByIndexMapped)
      getSetMapped.append(getAllAttributes)
      getSetMapped.append(getAttributeNameAndValueIteratorMapped)
      getSetMapped.append(setByNameFuncForMappedMsgs)
      getSetMapped.append(setValueAndValTypByKeyMapped)
      getSetMapped.append(setByIndex)
      getSetMapped.append(ValueToString)
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    getSetMapped.toString()
  }

  /*
   * Get the index for fields in mapped messages
   */
  def getScalarFieldindex(fields: List[Element]): Map[String, Int] = {
    var fldsSclarIndex: Map[String, Int] = Map(("String", 0))
    var setFields: Set[String] = Set[String]()
    var a = 0;
    var index: Int = 0
    if (fields != null) {
      for (a <- fields) {
        setFields = setFields + a.FieldTypePhysicalName
      }
    }

    var b = 0;
    for (b <- setFields) {
      if (primitives.contains(b.toLowerCase())) {
        if (!b.equalsIgnoreCase("string"))
          fldsSclarIndex = fldsSclarIndex + ((b, index + 1))
        index = index + 1
      } else fldsSclarIndex = fldsSclarIndex + ((b, -1))
    }

    log.info("fldsSclarIndex  fields " + fldsSclarIndex.toList)
    log.info("set fields " + setFields.toList)
    return fldsSclarIndex
  }

  /*
   * CollectioAsString method for mapped messages
   */

  def CollectionAsArrString = {
    """
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
  """
  }
  /*
   * type conversion
   */
  def typeConversion = {
    """
    private def typeConv(valueType: String, value: Any): AttributeValue = {
      var attributeValue: AttributeValue = new AttributeValue();
      attributeValue.setValueType(valueType)

      valueType match {
        case "string" => { attributeValue.setValue(com.ligadata.BaseTypes.StringImpl.Input(value.asInstanceOf[String])) }
        case "int" => { attributeValue.setValue(com.ligadata.BaseTypes.IntImpl.Input(value.asInstanceOf[String])) }
        case "float" => { attributeValue.setValue(com.ligadata.BaseTypes.FloatImpl.Input(value.asInstanceOf[String])) }
        case "double" => { attributeValue.setValue(com.ligadata.BaseTypes.DoubleImpl.Input(value.asInstanceOf[String])) }
        case "boolean" => { attributeValue.setValue(com.ligadata.BaseTypes.BoolImpl.Input(value.asInstanceOf[String])) }
        case "long" => { attributeValue.setValue(com.ligadata.BaseTypes.LongImpl.Input(value.asInstanceOf[String])) }
        case "char" => { attributeValue.setValue(com.ligadata.BaseTypes.CharImpl.Input(value.asInstanceOf[String])) }
        case "any" => { attributeValue.setValue(com.ligadata.BaseTypes.StringImpl.Input(value.asInstanceOf[String])) }
        case _ => {} // do nothhing
      }
      attributeValue
    }  
    """
  }

  def getAttributeTypesMethodFixed = {

    """
    override def getAttributeTypes(): Array[AttributeTypeInfo] = {
      if (attributeTypes == null) return null;
      return attributeTypes
    }
    """
  }

  def getAttributeTypesMethodMapped = {
    """
    override def getAttributeTypes(): Array[AttributeTypeInfo] = {
      val attributeTyps = valuesMap.map(f => f._2.getValueType).toArray;
      if (attributeTyps == null) return null else return attributeTyps
    }   
 """
  }

}