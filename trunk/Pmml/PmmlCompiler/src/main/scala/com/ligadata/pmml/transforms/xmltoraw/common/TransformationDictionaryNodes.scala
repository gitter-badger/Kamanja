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

class TransformationDictionaryPmmlNodeGenerator extends PmmlNodeGenerator {

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

		new PmmlTransformationDictionary(namespaceURI, localName , qName, lineNumber, columnNumber)
	}
}

class DerivedFieldPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("name", "displayName", "optype", "dataType", "cacheHint")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val name : String = selectedValues.apply(0).asInstanceOf[String]
		val displayName : String = selectedValues.apply(1).asInstanceOf[String]
		val optype : String = selectedValues.apply(2).asInstanceOf[String]
		val dataType : String = selectedValues.apply(3).asInstanceOf[String]
    val cacheHint : String = selectedValues.apply(4).asInstanceOf[String]
 		new PmmlDerivedField(namespaceURI, localName , qName, lineNumber, columnNumber, name, displayName, optype, dataType, cacheHint)
	}
}


class ApplyPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("function", "mapMissingTo", "invalidValueTreatment")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val function : String = selectedValues.apply(0).asInstanceOf[String]
		val mapMissingTo : String = selectedValues.apply(1).asInstanceOf[String]
		val invalidValueTreatment : String = selectedValues.apply(2).asInstanceOf[String]
		new PmmlApply(namespaceURI, localName , qName, lineNumber, columnNumber, function, mapMissingTo, invalidValueTreatment)
	}
}

class FieldRefPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("field", "mapMissingTo")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val field : String = selectedValues.apply(0).asInstanceOf[String]
		val mapMissingTo : String = selectedValues.apply(1).asInstanceOf[String]
		new PmmlFieldRef(namespaceURI, localName , qName, lineNumber, columnNumber, field, mapMissingTo)
	}
}


class ConstantPmmlNodeGenerator() extends PmmlNodeGenerator {

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
			, columnNumber : Int) : PmmlNode = {

		val ofInterest : ArrayBuffer[String] = ArrayBuffer("dataType")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		var dataType : String = selectedValues.apply(0).asInstanceOf[String]
		if (dataType == "") dataType = "string"
		new PmmlConstant(namespaceURI, localName , qName, lineNumber, columnNumber, dataType)
	}
}

class ArrayPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("n","type")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val n : String = selectedValues.apply(0).asInstanceOf[String]
		val arrayType = selectedValues.apply(1).asInstanceOf[String]
		new PmmlArray(namespaceURI, localName , qName, lineNumber, columnNumber, n, arrayType)
	}
}
	
