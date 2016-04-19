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

package com.ligadata.pmml.transforms.rawtocooked.common

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata._
import com.ligadata.pmml.compiler._
import com.ligadata.pmml.support._
import com.ligadata.pmml.traits._
import com.ligadata.pmml.syntaxtree.raw.common._
import com.ligadata.pmml.syntaxtree.cooked.common._

class SimplePredicatePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

	/**
	    Construct a PmmlExecNode appropriate for the PmmlNode supplied  In some cases no node is returned
	    (i.e., None).  This can happen when the PmmlNode content is subsumed by the parent node.  See DataField
	    handling for an example where the DataNode content is added to the parent DataDictionary.

 	    @param dispatcher: PmmlExecNodeGeneratorDispatch
 	    @param qName: String (the original element name from the PMML)
 	    @param pmmlnode:PmmlNode
 	    @return optionally an appropriate PmmlExecNode or None
	 */
	 
	def make(dispatcher : PmmlExecNodeGeneratorDispatch, qName : String, pmmlnode : PmmlNode) : Option[PmmlExecNode] = {
		val node : PmmlSimplePredicate =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlSimplePredicate]) {
				pmmlnode.asInstanceOf[PmmlSimplePredicate] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlSimplePredicate... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			Some(new xSimplePredicate(node.lineNumber, node.columnNumber, node.field, node.operator, node.value))
		} else {
			None
		}
		xnode
	}	
}

class SimpleSetPredicatePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

	/**
	    Construct a PmmlExecNode appropriate for the PmmlNode supplied  In some cases no node is returned
	    (i.e., None).  This can happen when the PmmlNode content is subsumed by the parent node.  See DataField
	    handling for an example where the DataNode content is added to the parent DataDictionary.

 	    @param dispatcher: PmmlExecNodeGeneratorDispatch
 	    @param qName: String (the original element name from the PMML)
 	    @param pmmlnode:PmmlNode
 	    @return optionally an appropriate PmmlExecNode or None
	 */
	 
	def make(dispatcher : PmmlExecNodeGeneratorDispatch, qName : String, pmmlnode : PmmlNode) : Option[PmmlExecNode] = {
		val node : PmmlSimpleSetPredicate =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlSimpleSetPredicate]) {
				pmmlnode.asInstanceOf[PmmlSimpleSetPredicate] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlSimpleSetPredicate... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			Some(new xSimpleSetPredicate(node.lineNumber, node.columnNumber, node.field, node.booleanOperator))
		} else {
			None
		}
		xnode
	}	
}

class CompoundPredicatePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

	/**
	    Construct a PmmlExecNode appropriate for the PmmlNode supplied  In some cases no node is returned
	    (i.e., None).  This can happen when the PmmlNode content is subsumed by the parent node.  See DataField
	    handling for an example where the DataNode content is added to the parent DataDictionary.

 	    @param dispatcher: PmmlExecNodeGeneratorDispatch
 	    @param qName: String (the original element name from the PMML)
 	    @param pmmlnode:PmmlNode
 	    @return optionally an appropriate PmmlExecNode or None
	 */
	 
	def make(dispatcher : PmmlExecNodeGeneratorDispatch, qName : String, pmmlnode : PmmlNode) : Option[PmmlExecNode] = {
		val node : PmmlCompoundPredicate =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlCompoundPredicate]) {
				pmmlnode.asInstanceOf[PmmlCompoundPredicate] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlCompoundPredicate... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			Some(new xCompoundPredicate(node.lineNumber, node.columnNumber, node.booleanOperator))
		} else {
			None
		}
		xnode
	}	
}
