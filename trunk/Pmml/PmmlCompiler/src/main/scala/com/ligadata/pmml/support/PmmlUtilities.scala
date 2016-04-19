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

package com.ligadata.pmml.support

import scala.collection.mutable._
import scala.math._
import scala.collection.immutable.StringLike
import scala.collection.immutable.List
import scala.collection.immutable.Map

import org.joda.time.base
import org.joda.time.chrono
import org.joda.time.convert
import org.joda.time.field
import org.joda.time.format
import org.joda.time.tz
import org.joda.time.LocalDate
import org.joda.time.Years

import com.ligadata.pmml.runtime._
import com.ligadata.pmml.compiler._ 
import com.ligadata.pmml.support._ 

object PmmlError extends com.ligadata.pmml.compiler.LogTrait {
  
	/** 
	 *  Process the supplied error, first printing the geo location info in the element stack.
	 *  Leave the stack intact so other error or warning messages that may follow also have this information.
	 *  PmmlError.logError
	 *  
	 *  @param ctx the execution context for the compiler
	 *  @param errorMsg the error message to print after printing the location information
	 */
	def logError(ctx : PmmlContext, errorMsg : String, e: Throwable = null) : Unit = {
		
		if (ctx.elementStack.nonEmpty) { 
			val buffer : StringBuilder = new StringBuilder
			
			val sizeofStack : Int = ctx.elementStack.size
			var i : Int = 0
			ctx.elementStack.foreach( elem => {
				val elemStr : String = elem.toString
				if (elemStr != null && elemStr.size > 0) {
					buffer.append(elem.toString)
					i += 1
					if (i < sizeofStack) {
						buffer.append(" found in ")
					} 
				}
			})
			val errorVicinity : String = buffer.toString
			logger.error(s"While processing $errorVicinity...")
		}

		if (e != null)
			logger.error(errorMsg, e)
		else
			logger.error(errorMsg)

		/** count the errors */
		ctx.IncrErrorCounter
		
	}

	/** 
	 *  Process the supplied warning, first printing the geo location info in the element stack.
	 *  Leave the stack intact so other warning or error messages that may follow also have this information.
	 *  PmmlError.logError
	 *  
	 *  @param ctx the execution context for the compiler
	 *  @param warningMsg the warning message to print after printing the location information
	 */
	def logWarning(ctx : PmmlContext, warningMsg : String) : Unit = {
		
		if (ctx.elementStack.nonEmpty) { 
			val buffer : StringBuilder = new StringBuilder
			
			val sizeofStack : Int = ctx.elementStack.size
			var i : Int = 0
			ctx.elementStack.foreach( elem => {
				val elemStr : String = elem.toString
				if (elemStr != null && elemStr.size > 0) {
					buffer.append(elem.toString)
					i += 1
					if (i < sizeofStack) {
						buffer.append(" found in ")
					} 
				}
			})
			val warnVicinity : String = buffer.toString
			logger.warn(s"While processing $warnVicinity...")
			logger.warn(warningMsg)
		}
	  
	}
}


object PmmlTypes extends com.ligadata.pmml.compiler.LogTrait {
  
  	def scalaDataType(dataType : String) : String = {
	  		val typ = dataType.toLowerCase match {
		      case "string" => "String"
		      case "long" => "Long"
		      case "int" => "Int"
		      case "float" => "Float"
		      case "double" => "Double"
		      case "integer" => "Int" 
		      case "real" => "Double" 
		      case "boolean" => "Boolean" 
		      case "date" => "Long" 
		      case "time" => "Long" 
		      case "datetime" => "Long" 
		      case "datedayssince[0]" => "Long" 
		      case "datedayssince[1960]" => "Long" 
		      case "datedayssince[1970]" => "Long" 
		      case "datedayssince[1980]" => "Long" 
		      case "timeseconds" => "Long" 
		      case "datetimesecondssince[0]" => "Long" 
		      case "datetimesecondssince[1960]" => "Long" 
		      case "datetimesecondssince[1970]" => "Long" 
		      case "datetimesecondssince[1980]" => "Long" 
		  
		      case _ => {
		    	  //println(s"scalaDataType($dataType)... unknown type... Any returned")
		    	  "Any"
		      }
			} 
	  		typ
	}
  	
  	/** Wrap quotes if needed around supplied string acorrding to the type */ 
  	def QuoteString(fcnNameStr : String, dataType : String) : String = {
  		val str = dataType match {
	      case "string" | "date" | "time" | "dateTime" => s"${'"'}$fcnNameStr${'"'}" 
	      case _  => fcnNameStr
		} 
  		str
	}

  	def scalaDerivedDataType(dataType : String) : String = {
  			//println(s"scalaDerivedDataType arg = $dataType")
	  		val typ = dataType match {
		      case "String" => "StringDataValue"
		      case "Int" => "IntDataValue" 
		      case "Integer" => "IntDataValue" 
		      case "Long" => "LongDataValue" 
		      case "Float" => "FloatDataValue" 
		      case "Double" => "DoubleDataValue" 
		      case "Boolean" => "BooleanDataValue" 
		      case "LocalDate" => "DateDataValue" 
		      case "LocalTime" => "TimeDataValue" 
		      case "DateTime" => "DateTimeDataValue"
		  
		      case _ => {
		    	  //println(s"scalaDerivedDataType($dataType)... unknown type... Any returned")
		    	  "Any"
		      }
			} 
	  		typ
	}

 	def scalaDerivedDataTypeToScalaType(derivedDataType : String) : String = {
	  		val typ = derivedDataType match {
		      case "StringDataValue" => "String"
		      case "IntDataValue" => "Int" 
		      case "LongDataValue" => "Long" 
		      case "FloatDataValue" => "Float" 
		      case "DoubleDataValue" => "Double" 
		      case "BooleanDataValue" => "Boolean" 
		      case "DateDataValue" => "LocalDate" 
		      case "TimeDataValue" => "LocalTime" 
		      case "DateTimeDataValue" => "DateTime"
		  
		      case _ =>  "Any" 
			} 
	  		typ
	}

 	def scalaTypeToDataValueType(scalaType : String) : String = {
	  		val typ = scalaType match {
		      case "String" => "StringDataValue"
		      case "Int" => "IntDataValue" 
		      case "Integer" => "IntDataValue" 
		      case "Long" =>  "LongDataValue"
		      case "Float" => "FloatDataValue" 
		      case "Double" => "DoubleDataValue" 
		      case "Boolean" => "BooleanDataValue" 
		      case "LocalDate" => "DateDataValue" 
		      case "LocalTime" => "TimeDataValue" 
		      case "DateTime" => "DateTimeDataValue"
		  
		      case _ =>  "AnyDataValue" 
			} 
	  		typ
	}

  	def dataValueFromString(dataType : String, strRep : String) : DataValue = {
	  		val obj : DataValue = dataType match {
		      case "String" => StringDataValue.fromString(strRep)
		      case "Int" => IntDataValue.fromString(strRep)
		      case "Integer" => IntDataValue.fromString(strRep)
		      case "Long" => LongDataValue.fromString(strRep)
		      case "Float" => FloatDataValue.fromString(strRep)
		      case "Double" => DoubleDataValue.fromString(strRep)
		      case "Boolean" => BooleanDataValue.fromString(strRep)
		      case "LocalDate" => DateDataValue.fromString(strRep)
		      case "LocalTime" => TimeDataValue.fromString(strRep)
		      case "DateTime" => DateTimeDataValue.fromString(strRep)
		  
		      case _ => {
		    	  //println(s"dataValueFromString($dataType, $strRep)... unknown type ... coercion to String")
		    	  StringDataValue.fromString(strRep)
		      }
			} 
	  		obj
	}

  	
  	def scalaBuiltinNameFcnSelector(opname : String) : String = {
		val fcnName = opname match {
		      case "if" => "if"
		      case "or" => "or"
		      case "and" => "and"
		      case "IntOr" => "IntOr"  /** equivalent to 'or' but with int args ... renamed to avoid dup type due to type erasure */
		      case "IntAnd" => "IntAnd"   /** equivalent to 'and' but with int args ... renamed to avoid dup type due to type erasure */
		      case "xor" => "Xor"
		      case "equal" => "Equal"
		      case "notEqual" => "NotEqual"
		      case "-" => "Minus"
		      case "+" => "Plus"
		      case "*" => "Multiply"
		      case "/" => "Divide"
		      case "not" => "Not"
		      case "lessThan" => "LessThan"
		      case "lessOrEqual" => "LessOrEqual"
		      case "greaterThan" => "GreaterThan"
		      case "greaterOrEqual" => "GreaterOrEqual"
		      /**case "isMissing" => "IsMissing"   these are not simple functions ... they are macros ... need to go through macro logic 
		         case "isNotMissing" => "IsNotMissing"*/
		      case "isIn" => "IsIn"
		      case "isNotIn" => "IsNotIn"
		      case _  => "Unknown Operator"
		}
		fcnName
  	}
  	
  	def translateBuiltinNameIfNeeded(fcnName : String) : String = {
  		val fcnNameX : String = scalaBuiltinNameFcnSelector(fcnName)
  		if (fcnNameX != "Unknown Operator") fcnNameX else fcnName
  	}
  	
   	def scalaNameForIterableFcnName(iterableFcnName : String) : String = {
		val fcnName = iterableFcnName.toLowerCase() match {
		      case "containermap" => "map"
		      case "containerfilter" => "filter"
		      case "groupby" => "groupBy"
		      case _  => null
		}
		fcnName
  	}
 
}

/** 
 *  This enumeration describes the type of traversal to perform for 
 *  PmmlExecNode tree traversal.  By default (since we are doing functional language,
 *  the order is PREORDER
 */
object Traversal extends Enumeration {
	type Order = Value 
	val INORDER, PREORDER, POSTORDER = Value
}

/** 
 *  CodeFragment enumeration describes which kind of string to generate for a given
 *  PmmlExecNode.  This enumeration can be used to control if desired which aspect
 *  of a given PMML tree to print.  For example, the TransformationDictionary DerivedField
 *  has both a declaration (as part of the dictionary) and a declaration of a class that has
 *  the apply functions for computing the DerivedField's value.  
 */
object CodeFragment extends Enumeration {
	type Kind = Value
	val VARDECL, VALDECL, FUNCCALL, DERIVEDCLASS, RULECLASS, RULESETCLASS , MININGFIELD, MAPVALUE, AGGREGATE, USERFUNCTION = Value
}


