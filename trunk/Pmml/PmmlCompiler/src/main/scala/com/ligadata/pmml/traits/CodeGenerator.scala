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

import com.ligadata.pmml.compiler
import com.ligadata.pmml.support._
import com.ligadata.pmml.syntaxtree.cooked.common._

/**
 * The CodePrinterDispatch interface accepts some kind of PmmlExecNode, a buffer to fill 
 * with generated output, an instance of the CodeGeneratorDispatch so that recursive calls back to 
 * the code printer dispatch can be made, and the sort of code fragment to generate.
 * 
 * This interface acts as the manager or central control that is called back when recursion 
 * to a sub-node is required.
 * 
 * The intention here is to allow for the generation of alternate outputs either in strategy 
 * or even in the language used.  Currently there is a Scala printer that implements this 
 * interface.  A C++ version is planned.  Conceivably alternate versions of a given 
 * syntax tree node instance could be generated in the same language.
 * 
 */
trait CodePrinterDispatch {

	/** Select the appropriate code generator for the supplied PmmlExecNode, invoke
	    its CodePrinter, and (presumably) add the generated code string to a
	    StringBuilder owned by the CodePrinterDispatch implementation.

	    The CodeFragment.Kind describes what kind of string should be produced.  Note 
	    that many nodes can produce different representations. For example, is a 
	    declaration or an "use" expression required for a DataField.
 	    
 	    @param node the PmmlExecNode
 	    @param buffer the caller's buffer to which the print for the node (and below it) will be left
 	    @param the kind of code fragment to generate...any 
 	    	{VARDECL, VALDECL, FUNCCALL, DERIVEDCLASS, RULECLASS, RULESETCLASS , MININGFIELD, MAPVALUE, AGGREGATE, USERFUNCTION}
	 */
	def generate(node : Option[PmmlExecNode]
				, buffer: StringBuilder
				, kind : CodeFragment.Kind) 
	
}

/**
 * The CodePrinter is an an interface that describes a common print behavior that is 
 * implemented by all PmmlExecNode instances.  Its method, print, answers a 
 * string representation of itself according to the code fragment requested and the semantics
 * of the generator itself (e.g., will a scala code fragment be generated or a c++ one?).
 * 
 */
trait CodePrinter {

	/**
	 *  Answer a string (code representation) for the supplied node.
	 *  @param node the PmmlExecNode
	 *  @param the CodePrinterDispatch to use should recursion to child nodes be required.
 	 *  @param the kind of code fragment to generate...any 
 	 *  	{VARDECL, VALDECL, FUNCCALL, DERIVEDCLASS, RULECLASS, RULESETCLASS , MININGFIELD, MAPVALUE, AGGREGATE, USERFUNCTION}
	 *  @order the order to traverse this node...any {Traversal.INORDER, Traversal.PREORDER, Traversal.POSTORDER} 
	 *  
	 *  @return some string representation of this node appropriate for the code fragment requested.
	 */
	def print(node : Option[PmmlExecNode]
			, generator : CodePrinterDispatch
			, kind : CodeFragment.Kind
			, order : Traversal.Order) : String
	
}

