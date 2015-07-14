package com.ligadata.pmml.compiler

import scala.collection.mutable._
import scala.math._
import scala.collection.immutable.StringLike
import scala.util.control.Breaks._
import com.ligadata.pmml.runtime._
import org.apache.log4j.Logger
import com.ligadata.fatafat.metadata._

class SimpleSetPredicateCodePrinter(ctx : PmmlContext) {

	/**
	 *  Answer a string (code representation) for the supplied node.
	 *  @param node the PmmlExecNode
	 *  @param the CodePrinterDispatch to use should recursion to child nodes be required.
 	 *  @param the kind of code fragment to generate...any 
 	 *   	{VARDECL, VALDECL, FUNCCALL, DERIVEDCLASS, RULECLASS, RULESETCLASS , MININGFIELD, MAPVALUE, AGGREGATE, USERFUNCTION}
	 *  @order the traversalOrder to traverse this node...any {INORDER, PREORDER, POSTORDER} 
	 *  
	 *  @return some string representation of this node
	 */
	def print(node : Option[PmmlExecNode]
			, generator : CodePrinterDispatch
			, kind : CodeFragment.Kind
			, traversalOrder : Traversal.Order) : String = {

		val xnode : xSimpleSetPredicate = node match {
			case Some(node) => {
				if (node.isInstanceOf[xSimpleSetPredicate]) node.asInstanceOf[xSimpleSetPredicate] else null
			}
			case _ => null
		}

		val printThis = if (xnode != null) {
			codeGenerator(xnode, generator, kind, traversalOrder)
		} else {
			if (node != null) {
				PmmlError.logError(ctx, s"For ${node.qName}, expecting an xSimpleSetPredicate... got a ${node.getClass.getName}... check CodePrinter dispatch map initialization")
			}
			""
		}
		printThis
	}
	

	private def codeGenerator(node : xSimpleSetPredicate
							, generator : CodePrinterDispatch
							, kind : CodeFragment.Kind
							, traversalOrder : Traversal.Order) : String = 	{

	  	val fcnBuffer : StringBuilder = new StringBuilder()
		val simplePredStr : String = order match {
			case Traversal.INORDER => { "" }
			case Traversal.POSTORDER => { "" }
			case Traversal.PREORDER => {
				val opFcn : String = PmmlTypes.scalaBuiltinNameFcnSelector(node.booleanOperator)
				val sPred = s"$opFcn("
				fcnBuffer.append(sPred)
				var cnt = 0
				node.Children.foreach((child : PmmlExecNode) => {
			  		generator.generate(child.asInstanceOf[Option[PmmlExecNode]], fcnBuffer, CodeFragment.FUNCCALL)
			  		cnt += 1
			  		if (cnt < Children.length) { 
			  			fcnBuffer.append(", ")
			  		}
		  		})
		  		val closingParen : String = s")\n"
		  		fcnBuffer.append(closingParen)
		  		fcnBuffer.toString
			}
		}
		simplePredStr
	}
}
