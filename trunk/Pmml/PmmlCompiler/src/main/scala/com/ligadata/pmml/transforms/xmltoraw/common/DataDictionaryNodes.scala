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

package com.ligadata.pmml.transforms.xmltoraw.common

import scala.collection.mutable.ArrayBuffer
import org.xml.sax.Attributes
import com.ligadata.pmml.compiler._
import com.ligadata.pmml.traits._
import com.ligadata.pmml.support._
import com.ligadata.pmml.syntaxtree.raw.common._
import com.ligadata.pmml.transforms.xmltoraw.common._

class DataDictionaryPmmlNodeGenerator extends PmmlNodeGenerator {

	/**
	    With the supplied xml arguments build a PmmlNode and return it to the dispatcher that is calling.
 	    @param namespaceURI: String 
 	    @param localName: String 
 	    @param qName:String
 	    @param atts: Attributes
 	    @param lineNumber : Int
 	    @param columnNumber : Int
 	    @return a PmmlNode
	 */
	 
	def make(namespaceURI: String
			, localName: String 
			, qName:String 
			, atts: Attributes
			, lineNumber : Int
			, columnNumber : Int) : PmmlNode =  {

		val ofInterest : ArrayBuffer[String] = ArrayBuffer("numberOfFields")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val numberOfFields : String = selectedValues.apply(0).asInstanceOf[String]
		new PmmlDataDictionary(namespaceURI, localName , qName, lineNumber, columnNumber, numberOfFields)
	}
}

class DataFieldPmmlNodeGenerator extends PmmlNodeGenerator {

	/**
	    With the supplied xml arguments build a PmmlNode and return it to the dispatcher that is calling.
 	    @param namespaceURI: String 
 	    @param localName: String 
 	    @param qName:String
 	    @param atts: Attributes
 	    @param lineNumber : Int
 	    @param columnNumber : Int
 	    @return a PmmlNode
	 */
	 
	def make(namespaceURI: String
			, localName: String 
			, qName:String 
			, atts: Attributes
			, lineNumber : Int
			, columnNumber : Int) : PmmlNode =  {

		val ofInterest : ArrayBuffer[String] = ArrayBuffer("name", "displayName", "optype", "dataType", "taxonomy", "isCyclic")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val name : String = selectedValues.apply(0).asInstanceOf[String]
		val displayName : String = selectedValues.apply(1).asInstanceOf[String]
		val optype : String = selectedValues.apply(2).asInstanceOf[String]
		val dataType : String = selectedValues.apply(3).asInstanceOf[String]
		val taxonomy : String = selectedValues.apply(4).asInstanceOf[String]
		val isCyclic : String = selectedValues.apply(5).asInstanceOf[String]
		new PmmlDataField(namespaceURI, localName , qName, lineNumber, columnNumber, name, displayName, optype, dataType, taxonomy, isCyclic)
	}
}

class ValuePmmlNodeGenerator extends PmmlNodeGenerator {

	/**
	    With the supplied xml arguments build a PmmlNode and return it to the dispatcher that is calling.
 	    @param namespaceURI: String 
 	    @param localName: String 
 	    @param qName:String
 	    @param atts: Attributes
 	    @param lineNumber : Int
 	    @param columnNumber : Int
 	    @return a PmmlNode
	 */
	 
	def make(namespaceURI: String
			, localName: String 
			, qName:String 
			, atts: Attributes
			, lineNumber : Int
			, columnNumber : Int) : PmmlNode =  {

		val ofInterest : ArrayBuffer[String] = ArrayBuffer("value", "displayValue", "property")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val value : String = selectedValues.apply(0).asInstanceOf[String]
		val displayValue : String = selectedValues.apply(1).asInstanceOf[String]
		val property : String = selectedValues.apply(2).asInstanceOf[String]
		new PmmlValue(namespaceURI, localName , qName, lineNumber, columnNumber, value, displayValue, property)
	}
}


