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

package com.ligadata.pmml.traits

import org.xml.sax.Attributes
import com.ligadata.pmml.syntaxtree.raw.common._


/**
 * The PmmlNodeGeneratorDispatch interface accepts the PMML element values collected by the Sax parse.
 * Based upon the xml "qName" supplied, the appropriate PmmlNodeGenerator is dispatched.  The returned node is added
 * to the syntax tree.
 */
trait PmmlNodeGeneratorDispatch {

	/** 
		Select the appropriate PmmlNode generator for the supplied xml values, locate its
	    PmmlNodeGenerator (the 'qName' is the key), and dispatch it. The returned node is added to the 
	    syntax tree owned by (or addressable by) the PmmlNodeGeneratorDispatch implementation.
 	    
 	    @param namespaceURI: String
 	    @param localName: String 
 	    @param qName:String
 	    @param atts: Attributes
 	    @param lineNumber : Int
 	    @param columnNumber : Int
 	    @return Unit
	 */

	def dispatch(namespaceURI: String, localName: String , qName:String , atts: Attributes, lineNumber : Int, columnNumber : Int) 
	
}

/**
 * The PmmlNodeGenerator is an an interface that consumes the xml content supplied in the parameter list
 * of the generator itself 
 * 
 */
trait PmmlNodeGenerator {

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
			, columnNumber : Int) : PmmlNode
	
}

