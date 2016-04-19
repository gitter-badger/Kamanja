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


class HeaderPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val headerNode : PmmlHeader =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlHeader]) {
				pmmlnode.asInstanceOf[PmmlHeader] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlHeader... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val node : Option[PmmlExecNode] = if (headerNode != null) {
			/** Collect the copyright and description if present for later use during code generation */
			ctx.pmmlTerms("Copyright") = Some(headerNode.copyright)
			ctx.pmmlTerms("Description") = Some(headerNode.description)
		  
			Some(new xHeader(headerNode.lineNumber, headerNode.columnNumber, headerNode.copyright, headerNode.description))
		} else {
			None
		}
		node
	}
	
}

class ApplicationPmmlExecNodeGenerator(val ctx : PmmlContext) extends PmmlExecNodeGenerator with com.ligadata.pmml.compiler.LogTrait {

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
		val headerNode : PmmlApplication =  if (pmmlnode != null && pmmlnode.isInstanceOf[PmmlApplication]) {
				pmmlnode.asInstanceOf[PmmlApplication] 
			} else {
				if (pmmlnode != null) {
					PmmlError.logError(ctx, s"For $qName, expecting a PmmlApplication... got a ${pmmlnode.getClass.getName}... check PmmlExecNode generator initialization")
				}
				null
			}
		val node : Option[PmmlExecNode] = if (headerNode != null) {
		  
			/** split the namespace given in the application name attribute (if any) from the name... when no 
			 *  namespace prefix, default to "System" */		  
			val (nmspc, name) : (String, String) = if (headerNode.name != null && headerNode.name.size > 0) {
				val buffer : StringBuilder = new StringBuilder
				val nmNodes : Array[String] = headerNode.name.split('.').map(_.trim)
				if (nmNodes.size > 1) {
					val nmPart : String = nmNodes.last
					val nmspcPart : String = nmNodes.takeWhile(_ != nmPart).addString(buffer, ".").toString
					buffer.clear
					(nmspcPart,nmPart)	
				} else {
					("System", headerNode.name)
				}
			} else {
				PmmlError.logError(ctx, s"Incredibly the PmmlApplication name attribute is not specified!")
				("","")
			}
		  
			/** Collect the Application name and the version if it is present */
			ctx.pmmlTerms("ModelNamespace") = Some(nmspc)
			ctx.pmmlTerms("ApplicationName") = Some(name)
			ctx.pmmlTerms("Version") = Some(MdMgr.FormatVersion(if (headerNode.version == null || headerNode.version.trim.isEmpty) "1.1" else headerNode.version))

			/** update the header parent node with the application name and version, don't create node*/
			
			val top : Option[PmmlExecNode] = ctx.pmmlExecNodeStack.top
			top match {
			  case Some(top) => {
				  var header : xHeader = top.asInstanceOf[xHeader]
				  header.ApplicationNameVersion(name, headerNode.version)
				  header.ModelNamespace(nmspc)
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

