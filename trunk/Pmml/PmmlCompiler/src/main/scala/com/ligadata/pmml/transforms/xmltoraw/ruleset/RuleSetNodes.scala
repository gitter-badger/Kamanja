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

package com.ligadata.pmml.transforms.xmltoraw.ruleset

import scala.collection.mutable.ArrayBuffer
import org.xml.sax.Attributes
import com.ligadata.pmml.compiler._
import com.ligadata.pmml.traits._
import com.ligadata.pmml.support._
import com.ligadata.pmml.syntaxtree.raw.common._
import com.ligadata.pmml.transforms.xmltoraw.common._

class RuleSetModelPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("modelName", "functionName", "algorithmName", "isScorable")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val modelName : String = selectedValues.apply(0).asInstanceOf[String]
		val functionName : String = selectedValues.apply(1).asInstanceOf[String]
		val algorithmName : String = selectedValues.apply(2).asInstanceOf[String]
		val isScorable : String = selectedValues.apply(3).asInstanceOf[String]
		new PmmlRuleSetModel(namespaceURI, localName , qName, lineNumber, columnNumber, modelName, functionName, algorithmName, isScorable)
	}
}

class SimpleRulePmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("id", "score", "recordCount", "nbCorrect", "confidence", "weight")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val id : String = selectedValues.apply(0).asInstanceOf[String]
		val score : String = selectedValues.apply(1).asInstanceOf[String]
		val recordCount : String = selectedValues.apply(2).asInstanceOf[String]
		val nbCorrect : String = selectedValues.apply(3).asInstanceOf[String]
		val confidence : String = selectedValues.apply(4).asInstanceOf[String]
		val weight : String = selectedValues.apply(5).asInstanceOf[String]
		new PmmlSimpleRule(namespaceURI, localName , qName, lineNumber, columnNumber, id, score, recordCount, nbCorrect, confidence, weight)
	}
}


class RuleSetPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("recordCount", "nbCorrect", "defaultScore", "defaultConfidence")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val recordCount : String = selectedValues.apply(0).asInstanceOf[String]
		val nbCorrect : String = selectedValues.apply(1).asInstanceOf[String]
		val defaultScore : String = selectedValues.apply(2).asInstanceOf[String]
		val defaultConfidence : String = selectedValues.apply(3).asInstanceOf[String]
		new PmmlRuleSet(namespaceURI, localName , qName, lineNumber, columnNumber, recordCount, nbCorrect, defaultScore, defaultConfidence)
	}
}
	
class RuleSelectionMethodPmmlNodeGenerator extends PmmlNodeGenerator {

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
		val ofInterest : ArrayBuffer[String] = ArrayBuffer("criterion")
		val selectedValues = PmmlNode.hlpOrganizeAttributes(atts, ofInterest).asInstanceOf[ArrayBuffer[_]]
		val criterion : String = selectedValues.apply(0).asInstanceOf[String]
		new PmmlRuleSelectionMethod(namespaceURI, localName , qName, lineNumber, columnNumber, criterion)
	}
}
	

