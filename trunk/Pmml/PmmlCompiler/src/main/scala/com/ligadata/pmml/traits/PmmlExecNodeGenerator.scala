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

import com.ligadata.pmml.compiler._
import com.ligadata.pmml.syntaxtree.raw.common._
import com.ligadata.pmml.syntaxtree.cooked.common._


/**
 * The PmmlExecNodeGeneratorDispatch interface accepts an instance of this dispatcher, the element 'qName' of the
 * node that is to be built, and the 'raw' PmmlNode that was constructed from the corresponding PMML and from which
 * the PmmlExecNode (i.e., the 'cooked' node) will be built.
 */
trait PmmlExecNodeGeneratorDispatch {

	/** Select the appropriate PmmlExecNode generator (a PmmlExecNodeGenerator) for the supplied xml element name by locating its
	    PmmlExecNodeGenerator (the 'qName' is the element name key), and dispatch it. The returned node is added to the syntax tree
	    being constructed.  The dispatcher is passed as an argument so that the generators are free to access the
	    'raw' and 'cooked' syntax trees being built via the Context method. For example, this is useful to pull up information from children to parent as part of the 'cooking' process as is the case with DataFields and DerivedFields and their
	    respective dictionaries.
 	    
 	    @param qName: String 
 	    @param pmmlnode:PmmlNode
  	    @return Unit
	 */
	def dispatch(qName : String, pmmlnode : PmmlNode) : Unit


	/**
		Answer the global context that contains the syntax tree and other useful state information needed to 
		properly build the syntax tree.
		@return the PmmlContext singleton
	 */
	def context : PmmlContext
	
}

/**
 * The PmmlExecNodeGenerator is an an interface that transforms PmmlNodes (so-called raw nodes) and produces
 * a decorated "cooked" node (i.e., a PmmlExecCode).  In some cases, no node is returned.
 * 
 */
trait PmmlExecNodeGenerator {

	/**
	    Construct a PmmlExecNode appropriate for the PmmlNode supplied. The supplied dispatcher is used principally
	    to gain access to the PmmlContext singleton managing the compilation.  In some cases no node is returned
	    (i.e., None).  This can happen when the PmmlNode content is subsumed by the parent node.  See DataField
	    handling for an example where the DataNode content is added to the parent DataDictionary.

 	    @param dispatcher: PmmlExecNodeGeneratorDispatch
 	    @param qName: String (the original element name from the PMML)
 	    @param pmmlnode:PmmlNode
 	    @return optionally an appropriate PmmlExecNode or None
	 */
	 
	def make(dispatcher : PmmlExecNodeGeneratorDispatch, qName : String, pmmlnode : PmmlNode) : Option[PmmlExecNode]
	
}

