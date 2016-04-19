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

package com.ligadata.pmml.syntaxtree.raw.common

import scala.collection.immutable.{List}
import scala.collection.mutable._
import org.xml.sax.Attributes

class PmmlNodeBase(val namespaceURI: String, val localName: String , val qName:String
					, val lineNumber : Int, val columnNumber : Int) {} 
class PmmlNode(namespaceURI: String, localName: String , qName:String
					, lineNumber : Int, columnNumber : Int) 
		extends PmmlNodeBase(namespaceURI, localName, qName, lineNumber, columnNumber) 
{	 
	var children : ArrayBuffer[PmmlNodeBase] = ArrayBuffer[PmmlNodeBase]()
	def Children : ArrayBuffer[PmmlNodeBase] = children
	
	def addChild(child : PmmlNodeBase) = {
		children += child
	}
	
	/** By default only element names found in the map are collected.  There are cases where 
	 *  the children are self defined and at the collection time not understood by the 
	 *  element collection tools.  See the PmmlRow for an example.  The row tuples are 
	 *  blindly collected when the following function answers true
	 */
	def CollectChildTuples : Boolean = { false }
}

class PmmlConstant(namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , val dataType : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	var value : String = ""
	def Value(v : String) { value = v }
	def Value() : String = value
}

class PmmlHeader(namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , val copyright : String
			    , val description : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	
}

class PmmlApplication(namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , val name : String
			    , val version : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlDataDictionary(namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , numberOfFields : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}	 


class PmmlDataField(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val name : String
				, val displayName : String
				, val optype : String
				, val dataType : String
				, val taxonomy : String
				, val isCyclic : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlInterval(namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , val closure : String
			    , val leftMargin : String
			    , val rightMargin : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlValue(  namespaceURI: String
			    , localName: String 
			    , qName:String
				, lineNumber : Int
				, columnNumber : Int
			    , val value : String
			    , val displayValue : String
			    , val property : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlTransformationDictionary (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
		 
}

class PmmlDerivedField(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val name : String
				, val displayName : String
				, val optype : String
				, val dataType : String
				, val cacheHint : String)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}	 

class PmmlDefineFunction(namespaceURI: String
						, localName: String 
						, qName:String
						, lineNumber : Int
						, columnNumber : Int
						, val name : String
						, val optype : String
						, val dataType : String)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {	
}

class PmmlParameterField(namespaceURI: String
						, localName: String 
						, qName:String
						, lineNumber : Int
						, columnNumber : Int
						, val name : String
						, val optype : String
						, val dataType : String)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {	
}

class PmmlApply(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val function : String
				, val mapMissingTo : String
				, val invalidValueTreatment : String)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
  
	var categorizedValues : ArrayBuffer[Tuple2[String,String]] = ArrayBuffer[Tuple2[String,String]]()
	def CategorizedValue(idx : Int) : Tuple2[String,String] = categorizedValues.apply(idx)
	def addCategorizedValue(one : String, two : String)  : ArrayBuffer[Tuple2[String,String]] = { categorizedValues += (one -> two) }
	
}

class PmmlFieldRef(namespaceURI: String
				, localName : String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val field : String
				, val mapMissingTo : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlMapValues(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val mapMissingTo : String
				, val defaultValue : String 
				, val outputColumn :String 
				, val dataType :String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	var containerStyle : String = "map" /** By default "map", an xMapValuesMap is created; else "array" for an xMapValuesVector */
	var dataSource : String = "inline" /** By default "inline", the table/map content from InlineTable; else "locator" for a TableLocator */
	  
	def ContainerStyle(style : String) { containerStyle = style }
	def DataSource(src : String) { dataSource = src }
}    

class PmmlFieldColumnPair (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val field : String
				, val column : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
} 
/**
 
	<xs:element name="TableLocator">
	  <xs:complexType>
	    <xs:sequence>
	      <xs:element ref="Extension" minOccurs="0" maxOccurs="unbounded"/>
	    </xs:sequence>
	  </xs:complexType>
	</xs:element> 
	
    <TableLocator>
      <Extension name="dbname" value="myDB"/>
    </TableLocator>
    
  <xs:element name="Extension">
    <xs:complexType>
      <xs:complexContent mixed="true">
        <xs:restriction base="xs:anyType">
          <xs:sequence>
            <xs:any processContents="skip" minOccurs="0" maxOccurs="unbounded"/>
          </xs:sequence>
          <xs:attribute name="extender" type="xs:string" use="optional"/>
          <xs:attribute name="name" type="xs:string" use="optional"/>
          <xs:attribute name="value" type="xs:string" use="optional"/>
        </xs:restriction>
      </xs:complexContent>
    </xs:complexType>
  </xs:element>
 
*/

class PmmlTableLocator(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
} 

class PmmlExtension(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val extender : String
				, val name : String
				, val value : String)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
} 

class PmmlInlineTable (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
} 

class PmmlRow(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val collectRowTuples : Boolean = true) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	override def CollectChildTuples : Boolean = { collectRowTuples }
}

class PmmlRowTuple(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int)  extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	var value : String = ""
	def Value(v : String) {
		value = v
	}
	def Value() : String ={
		value
	}
} 

class PmmlRuleSetModel(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val modelName: String
				, val functionName : String
				, val algorithmName : String
				, val isScorable : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {

}

class PmmlSimpleRule(namespaceURI: String
				, localName: String 
				, qName:String 
				, lineNumber : Int
				, columnNumber : Int
				, val id: String
				, val score : String
				, val recordCount : String
				, val nbCorrect : String
				, val confidence : String
				, val weight : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlScoreDistribution(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val value: String
				, val recordCount : String
				, val confidence : String
				, val probability : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}


class PmmlCompoundPredicate (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val booleanOperator : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}


class PmmlSimplePredicate(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val field : String
				, val operator : String
				, val value : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

/** 
 *  <Array n="3" type="string">ab  "a b"   "with \"quotes\" "</Array>
 */
class PmmlArray(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val n : String
				, val arrayType : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	var valueString : String = ""
	
	def SetArrayValue(value : String) {
	  	valueString = value
	}
}

class PmmlSimpleSetPredicate(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val field : String
				, val booleanOperator : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlMiningSchema(namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	
}

class PmmlMiningField(namespaceURI: String, localName: String , qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val name : String
			    , val usageType : String
			    , val optype : String = ""
			    , val importance : String = ""
			    , val outliers : String = "asIs"
				, val lowValue : String = "-4294967295"
				, val highValue : String = "4294967296"
				, val missingValueReplacement : String = "0"
				, val missingValueTreatment : String = "asIs"
				, val invalidValueTreatment : String = "asIs") extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlRuleSet (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val recordCount : String
				, val nbCorrect : String
				, val defaultScore : String
				, val defaultConfidence : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
}

class PmmlRuleSelectionMethod (namespaceURI: String
				, localName: String 
				, qName:String
				, lineNumber : Int
				, columnNumber : Int
				, val criterion : String) extends PmmlNode(namespaceURI, localName, qName, lineNumber, columnNumber) {
	
 }

object PmmlNode {
  
	/** 
	 *  hlpOrganizeAttributes is called from each of the mkPmml* 'make' fcns in order to get 
	 *  the attributes from the PMML in the order of the constructor.  If the attribute, especially
	 *  common on the optional attributes, is not present, a value of "" is returned in its place.
	 *  
	 *  At some point, we can dress this up by utilizing the defaults from the xsd instead (meaning that 
	 *  another ArrayBuffer would be supplied here with the appropriate defaults for each attribute).
	 *  
	 *  @param atts the attributes collected from the xml
	 *  @param ofInterest those attributes that are needed for the constructor in the order they are needed
	 *  @return the array buffer of constructor values to be used
	 */
	def hlpOrganizeAttributes(atts: Attributes, ofInterest : ArrayBuffer[String]) : Any = {

		var attributes:ArrayBuffer[String] = ArrayBuffer()
		var values:ArrayBuffer[String] = ArrayBuffer()
		var idx : Int = 0
		for (i <- 0 to atts.getLength()) { 
			attributes+=atts.getQName(idx)
			values+=atts.getValue(idx)
			idx = idx + 1
		}
		val attrValPairs:ArrayBuffer[(String,String)] = attributes.zip(values)
		val selectedValues : ArrayBuffer[_] = for (ctorArg <- ofInterest) yield {
			val matches = attrValPairs.filter(_._1 == ctorArg)
			if (matches.length > 0) matches.apply(0)._2 else "" /** take the first (should be ONLY) one */
		}
		
		selectedValues
	}
	

}

