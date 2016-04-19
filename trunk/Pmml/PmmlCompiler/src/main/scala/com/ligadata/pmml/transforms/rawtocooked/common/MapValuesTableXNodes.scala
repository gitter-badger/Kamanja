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

class MapValuesPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlMapValues =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlMapValues]) {
				pmmlnode.asInstanceOf[PmmlMapValues] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlMapValues... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val typ = node.dataType
			val containerStyle = node.containerStyle
			val dataSrc = node.dataSource
			
			var mapvalues = if (containerStyle == "map") {
				if (dataSrc == "inline") {
					new xMapValuesMapInline(node.lineNumber, node.columnNumber, node.mapMissingTo, node.defaultValue, node.outputColumn, node.dataType, containerStyle, dataSrc)
				  
				} else { /** locator */
					new xMapValuesMapExternal(node.lineNumber, node.columnNumber, node.mapMissingTo, node.defaultValue, node.outputColumn, node.dataType, containerStyle, dataSrc) 
				}
			} else { /** array */
				if (dataSrc == "inline") {
					new xMapValuesArrayInline(node.lineNumber, node.columnNumber, node.mapMissingTo, node.defaultValue, node.outputColumn, node.dataType, containerStyle, dataSrc)
				} else { /** locator */
					new xMapValuesArrayExternal(node.lineNumber, node.columnNumber, node.mapMissingTo, node.defaultValue, node.outputColumn, node.dataType, containerStyle, dataSrc) 
				}
			}
			Some(mapvalues)
		} else {
			None
		}
		xnode
	}	
}


class FieldColumnPairPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlFieldColumnPair =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlFieldColumnPair]) {
				pmmlnode.asInstanceOf[PmmlFieldColumnPair] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlFieldColumnPair... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			/** update the value map node with the field col pair; don't create node*/
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  	var mv :xMapValuesMap = top.asInstanceOf[xMapValuesMap]
					mv.addFieldColumnPair (node.field , node.column) 
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

class RowColumnPairPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlRow =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlRow]) {
				pmmlnode.asInstanceOf[PmmlRow] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlRow... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  	var mv :xMapValuesMap = top.asInstanceOf[xMapValuesMap]

					/** Each PmmlRow has children (in this implementation we expect two values...
					 *  a key and a value.  The value maps to the MapValues output and the 
					 *  there is but one key sine we represent the inline table as a Map.
					 *  NOTE that more than one key can be present according to the spec. 
					 *  For this revision, this is not supported.  Tuple2 only.
					 */
					val tuples : ArrayBuffer[Option[PmmlRowTuple]] = node.children.filter(_.isInstanceOf[PmmlRowTuple]).asInstanceOf[ArrayBuffer[Option[PmmlRowTuple]]]
			
					val valuelen = tuples.length
					if (valuelen == 2) {
						val t0 = tuples(0)
						val k : String = t0 match {
						  case Some(t0) => t0.Value()
						  case _ => ""
						}
						val t1 = tuples(1)
						t1 match {
						  case Some(t1) => {
							  if (k.length() > 0)
								  mv.add(k,t1.Value())
						  }
						  case _ => None
						}
					} else {
						PmmlError.logError(ctx, s"Currently only 2 tuple rows (simple maps) are supported for InlineTables... row has $valuelen elements")
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


class TableLocatorPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlTableLocator =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlTableLocator]) {
				pmmlnode.asInstanceOf[PmmlTableLocator] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlTableLocator... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val pmmlxtensions : Queue[Option[PmmlExtension]] = node.children.filter(_.isInstanceOf[PmmlExtension]).asInstanceOf[Queue[Option[PmmlExtension]]]

			if (pmmlxtensions.length > 0) {
				val extens  = pmmlxtensions.map(x => {
					x match {
					  	case Some(x) => new xExtension(node.lineNumber, node.columnNumber, x.extender, x.name, x.value)
					  	case _ => new xExtension(node.lineNumber, node.columnNumber, "", "", "")
					}
				}).toArray
			  
				val extensions = extens.filter(_.name.length > 0)
				if (! ctx.pmmlExecNodeStack.isEmpty) {
					val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
					top match {
						case Some(top) => {
							top match {
							  	case xMap : xMapValuesMap => xMap.addExtensions(extensions) 
							  	case xArr : xMapValuesArray => xArr.addExtensions(extensions) 
							  	case _ => PmmlError.logError(ctx, "Parent of TableLocator is not a kind MapValues...discarded")
							}
						}
						case _ => PmmlError.logError(ctx, "TableLocator cannot be added")
					}
				}
			}
		  
			None
		} else {
			None
		}
		xnode
	}	
}

	
class InlineTablePmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val node : PmmlInlineTable =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlInlineTable]) {
				pmmlnode.asInstanceOf[PmmlInlineTable] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlInlineTable... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val xnode : Option[PmmlExecNode] = if (node != null) {
			val pmmlxtensions : Queue[Option[PmmlExtension]] = node.children.filter(_.isInstanceOf[PmmlExtension]).asInstanceOf[Queue[Option[PmmlExtension]]]

			if (pmmlxtensions.length > 0) {
				val extenss  = pmmlxtensions.map(x => {
					x match {
					  	case Some(x) => new xExtension(node.lineNumber, node.columnNumber, x.extender, x.name, x.value)
					  	case _ => new xExtension(node.lineNumber, node.columnNumber, "", "", "")
					}
				}).toArray
				val extensions = extenss.filter(_.name.length > 0)
				if (! ctx.pmmlExecNodeStack.isEmpty) {
					val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
					top match {
						case Some(top) => {
							top match {
							  	case xMap : xMapValuesMap => xMap.addExtensions(extensions) 
							  	case xArr : xMapValuesArray => xArr.addExtensions(extensions) 
							  	case _ => PmmlError.logError(ctx, "Parent of InlineTable is not a kind MapValues...discarded")
							}
						}
						case _ => PmmlError.logError(ctx, "inline table cannot be added")
					}
				}
			}
			None
		} else {
			None
		}
		xnode
	}	
}


