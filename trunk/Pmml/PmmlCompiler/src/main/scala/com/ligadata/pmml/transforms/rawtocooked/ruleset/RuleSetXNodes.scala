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

package com.ligadata.pmml.transforms.rawtocooked.ruleset

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata._
import com.ligadata.pmml.compiler._
import com.ligadata.pmml.support._
import com.ligadata.pmml.traits._
import com.ligadata.pmml.syntaxtree.raw.common._
import com.ligadata.pmml.syntaxtree.cooked.common._

class RuleSetModelPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlRuleSetModel =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlRuleSetModel]) {
				pmmlnode.asInstanceOf[PmmlRuleSetModel] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlRuleSetModel... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			/** Gather content from RuleSetModel attributes for later use by the Scala code generator */
			ctx.pmmlTerms("ModelName") = Some(node.modelName)
			ctx.pmmlTerms("FunctionName") = Some(node.functionName)
			
			Some(new xRuleSetModel(node.lineNumber, node.columnNumber, node.modelName, node.functionName, node.algorithmName, node.isScorable))
		} else {
			None
		}
		xnode
	}	
}

class RuleSetPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlRuleSet =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlRuleSet]) {
				pmmlnode.asInstanceOf[PmmlRuleSet] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlRuleSet... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  	var rsm : xRuleSetModel = top.asInstanceOf[xRuleSetModel]
					rsm.DefaultScore(node.defaultScore)
			  }
			  case _ => None
			}
			Some(new xRuleSet(node.lineNumber, node.columnNumber, node.recordCount, node.nbCorrect, node.defaultScore, node.defaultConfidence))
		} else {
			None
		}
		xnode
	}	
}

class SimpleRulePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlSimpleRule =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlSimpleRule]) {
				pmmlnode.asInstanceOf[PmmlSimpleRule] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlSimpleRule... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			var rsm : Option[xRuleSetModel] = ctx.pmmlExecNodeStack.apply(1).asInstanceOf[Option[xRuleSetModel]]
			
			val id : Option[String] = Some(node.id)
			var rule : xSimpleRule = new xSimpleRule( node.lineNumber, node.columnNumber, id
														    , node.score
														    , 0.0 /** recordCount */
														    , 0.0 /** nbCorrect */
														    , 0.0 /** confidence */
														    , 0.0) /** weight */
			rsm match {
				case Some(rsm) => {		
					try {
						rule.RecordCount(node.recordCount.toDouble)
						rule.CorrectCount(node.nbCorrect.toDouble)
						rule.Confidence(node.confidence.toDouble)
						rule.Weight(node.weight.toDouble)
					} catch {
						case e : Throwable => {ctx.logger.debug (s"Unable to coerce one or more mining 'double' fields... name = $id", e)}
					}
				
					rsm.addRule (rule) 
				}
				case _ => None
			}
			Some(rule)
		} else {
			None
		}
		xnode
	}	
}

class RuleSelectionMethodPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlRuleSelectionMethod =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlRuleSelectionMethod]) {
				pmmlnode.asInstanceOf[PmmlRuleSelectionMethod] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlRuleSelectionMethod... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.apply(1)   // xRuleSetModel is the grandparent
			top match {
			  case Some(top) => {
				  	var mf : xRuleSetModel = top.asInstanceOf[xRuleSetModel]
				  	var rsm : xRuleSelectionMethod = new xRuleSelectionMethod(node.lineNumber, node.columnNumber, node.criterion)
					mf.addRuleSetSelectionMethod(rsm)
			  }
			  case _ => None
			}
			None
		} else {
			None
		}
		xnode
	}	
}
	
