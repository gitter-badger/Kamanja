package com.ligadata.kamanja.test.v1000000;

import org.json4s.jackson.JsonMethods._
import org.json4s.DefaultFormats
import org.json4s.Formats
import com.ligadata.KamanjaBase.{ AttributeValue, ContainerFactoryInterface, ContainerInterface, MessageFactoryInterface, MessageInterface, TimePartitionInfo, ContainerOrConceptFactory, RDDObject, JavaRDDObject, ContainerOrConcept}
import com.ligadata.BaseTypes._
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import java.util.Date


object msg2 extends RDDObject[msg2] with MessageFactoryInterface {
	type T = msg2 ;
	override def getFullTypeName: String = "com.ligadata.kamanja.test.msg2";
	override def getTypeNameSpace: String = "com.ligadata.kamanja.test";
	override def getTypeName: String = "msg2";
	override def getTypeVersion: String = "000000.000001.000000";
	override def getSchemaId: Int = 0;
	override def createInstance: msg2 = new msg2(msg2);
	override def isFixed: Boolean = true;
	override def getContainerType: ContainerFactoryInterface.ContainerType = ContainerFactoryInterface.ContainerType.MESSAGE
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

	override def getSchema: String = " {\"type\": \"record\", \"namespace\" : \"com.ligadata.kamanja.test\",\"name\" : \"msg2\",\"fields\":[{\"name\" : \"out1\",\"type\" : \"int\"},{\"name\" : \"out2\",\"type\" : \"string\"},{\"name\" : \"out3\",\"type\" : \"int\"},{\"name\" : \"out4\",\"type\" : \"int\"}]}";
}

class msg2(factory: MessageFactoryInterface, other: msg2) extends MessageInterface(factory) {

	val logger = this.getClass.getName
	lazy val log = LogManager.getLogger(logger)

	private var keyTypes = Map("out1"-> "Int","out2"-> "String","out3"-> "Int","out4"-> "Int");

	override def save: Unit = { msg2.saveOne(this) }

	def Clone(): ContainerOrConcept = { msg2.build(this) }

	override def getPartitionKey: Array[String] = Array[String]()

	override def getPrimaryKey: Array[String] = Array[String]()

	var out1: Int = _;
	var out2: String = _;
	var out3: Int = _;
	var out4: Int = _;

	private def getWithReflection(key: String): AttributeValue = {
		var attributeValue = new AttributeValue();
		val ru = scala.reflect.runtime.universe
		val m = ru.runtimeMirror(getClass.getClassLoader)
		val im = m.reflect(this)
		val fieldX = ru.typeOf[msg2].declaration(ru.newTermName(key)).asTerm.accessed.asTerm
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
			if (key.equals("out1")) { attributeValue.setValue(this.out1); }
			if (key.equals("out2")) { attributeValue.setValue(this.out2); }
			if (key.equals("out3")) { attributeValue.setValue(this.out3); }
			if (key.equals("out4")) { attributeValue.setValue(this.out4); }


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
				attributeVal.setValue(out1)
				attributeVal.setValueType(keyTypes("out1"))
				attributeValsMap.put("out1", attributeVal)
			};
			{
				var attributeVal = new AttributeValue();
				attributeVal.setValue(out2)
				attributeVal.setValueType(keyTypes("out2"))
				attributeValsMap.put("out2", attributeVal)
			};
			{
				var attributeVal = new AttributeValue();
				attributeVal.setValue(out3)
				attributeVal.setValueType(keyTypes("out3"))
				attributeValsMap.put("out3", attributeVal)
			};
			{
				var attributeVal = new AttributeValue();
				attributeVal.setValue(out4)
				attributeVal.setValueType(keyTypes("out4"))
				attributeValsMap.put("out4", attributeVal)
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
					attributeValue.setValue(this.out1);
					attributeValue.setValueType(keyTypes("out1"));
				}
				case 1 => {
					attributeValue.setValue(this.out2);
					attributeValue.setValueType(keyTypes("out2"));
				}
				case 2 => {
					attributeValue.setValue(this.out3);
					attributeValue.setValueType(keyTypes("out3"));
				}
				case 3 => {
					attributeValue.setValue(this.out4);
					attributeValue.setValueType(keyTypes("out4"));
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

			if (key.equals("out1")) { this.out1 = value.asInstanceOf[Int]; }
			if (key.equals("out2")) { this.out2 = value.asInstanceOf[String]; }
			if (key.equals("out3")) { this.out3 = value.asInstanceOf[Int]; }
			if (key.equals("out4")) { this.out4 = value.asInstanceOf[Int]; }

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
				case 0 => {this.out1 = value.asInstanceOf[Int];}
				case 1 => {this.out2 = value.asInstanceOf[String];}
				case 2 => {this.out3 = value.asInstanceOf[Int];}
				case 3 => {this.out4 = value.asInstanceOf[Int];}

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
