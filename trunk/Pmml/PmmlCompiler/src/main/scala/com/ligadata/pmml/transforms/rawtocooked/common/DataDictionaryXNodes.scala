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

class DataDictionaryPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val dictNode : PmmlDataDictionary =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlDataDictionary]) {
				pmmlnode.asInstanceOf[PmmlDataDictionary] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlDataDictionary... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val node : Option[PmmlExecNode] = if (dictNode != null) {
			Some(new xDataDictionary(dictNode.lineNumber, dictNode.columnNumber)) 
		} else {
			None
		}
		node
	}
	
}

class DataFieldPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val dfld : PmmlDataField =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlDataField]) {
				pmmlnode.asInstanceOf[PmmlDataField] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlDataField... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val node : Option[PmmlExecNode] = if (dfld != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			var datafld : xDataField = null
			top match {
			  case Some(top) => {
				  var dict : xDataDictionary = top.asInstanceOf[xDataDictionary]
				  datafld = new xDataField(dfld.lineNumber, dfld.columnNumber, dfld.name, dfld.displayName, dfld.optype, dfld.dataType, dfld.taxonomy, dfld.isCyclic)
				  dict.add(datafld)
				  ctx.dDict(datafld.name) = datafld
				  if (dfld.Children.size > 0) {
					  /** pick up the values for DataFields */
					  dfld.Children.foreach( aNode => {
						  if (aNode.isInstanceOf[PmmlValue]) {
						  val vNode : PmmlValue = aNode.asInstanceOf[PmmlValue]
							  datafld.addValuePropertyPair(vNode.value, vNode.property)
						  }
					  })
				  }
			  }
			  case _ => None
			}
			None
		} else {
			None
		}
		node
	}	
}

class ValuePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlValue =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlValue]) {
				pmmlnode.asInstanceOf[PmmlValue] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlValue... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  if (top.isInstanceOf[xDataField]) {
					  var d : xDataField = top.asInstanceOf[xDataField]
					  d.addValuePropertyPair(node.value, node.property)
				  } else {
					  var df : xDerivedField = top.asInstanceOf[xDerivedField]
					  df.addValuePropertyPair(node.value, node.property)
				  }
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

