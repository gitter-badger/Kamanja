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

class MiningSchemaPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlMiningSchema =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlMiningSchema]) {
				pmmlnode.asInstanceOf[PmmlMiningSchema] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlMiningSchema... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			node.Children.foreach((child) => {
				val ch = child.asInstanceOf[PmmlMiningField]
				dispatcher.dispatch(ch.qName, ch)
			})
			None
		} else {
			None
		}
		xnode
	}	
}

class MiningFieldPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlMiningField =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlMiningField]) {
				pmmlnode.asInstanceOf[PmmlMiningField] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlMiningField... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  	var mf : xRuleSetModel = top.asInstanceOf[xRuleSetModel]
					mf.addMiningField (node.name , hlpMkMiningField(ctx, node)) 
			  }
			  case _ => None
			}
			None
		} else {
			None
		}
		xnode
	}

	def hlpMkMiningField(ctx : PmmlContext, d : PmmlMiningField) : xMiningField = {
		val name : String = d.name
		var fld : xMiningField = new xMiningField(d.lineNumber, d.columnNumber, d.name
											    , d.usageType
											    , d.optype
											    , 0.0
											    , d.outliers
											    , 0.0
											    , 0.0
											    , d.missingValueReplacement
											    , d.missingValueTreatment
											    , d.invalidValueTreatment)
		try {
			fld.Importance(d.importance.toDouble)
			fld.LowValue(d.lowValue.toDouble)
			fld.HighValue(d.highValue.toDouble)
		} catch {
			case e : Throwable => {
        ctx.logger.debug (s"Unable to coerce one or more of the mining field doubles... name = $name", e)}
		}
	  	fld
	}
	

}

