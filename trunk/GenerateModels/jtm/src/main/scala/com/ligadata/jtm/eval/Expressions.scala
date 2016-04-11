/*
 * Copyright 2016 ligaDATA
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
package com.ligadata.jtm.eval

import com.ligadata.jtm.{Datatypes, eval}
import com.ligadata.runtime.Conversion

import scala.util.matching.Regex

// Track details of any used element
case class Tracker(variableName: String, className: String, typeName: String, isInput: Boolean, accessor: String, expression: String) {
  def getExpression(): String = {
    if(expression.isEmpty)
      variableName
    else
      expression
  }
}

/**
  *
  */
object Expressions {

  /** Split a fully qualified object name into namspace and class
    *
    * @param name is a fully qualified class name
    * @return tuple with namespace and class name
    */
  def splitNamespaceClass(name: String): (String, String) = {
    val elements = name.split('.')
    (elements.dropRight(1).mkString("."), elements.last)
  }

  def IsExpressionVariable(expr: String, mapNameSource: Map[String, Tracker]): Boolean = {
    val regex1 = """^\$([a-zA-Z0-9_]+)$""".r
    val regex2 = """^\$\{([a-zA-Z0-9_]+\.[a-zA-Z0-9_]+)\}$""".r
    regex1.findFirstMatchIn(expr).isDefined || regex1.findFirstMatchIn(expr).isDefined
  }

  /** Evaluates if a expression is a variable
    *
    * @param expr
    * @param mapNameSource
    * @return
    */
  def isVariable(expr: String, mapNameSource: Map[String, Tracker], dictMessages: Map[String, String], aliaseMessages: Map[String, String]): (eval.Tracker) = {

    val Element = """([a-zA-Z][a-zA-Z0-9_]+)"""
    val Index = """(\([0-9]+\))"""
    val Begin = """^"""
    val End = """$"""
    val Open = """\{"""
    val Close = """\}"""
    val Marker = """\$"""
    val Separator = """\."""

    // name or ${name} -> points to a variable
    {
      val regex = s"$Begin$Element$End|$Begin$Marker$Open$Element$Close$End".r
      val elements = regex.findFirstMatchIn(expr)
      if (elements.isDefined) {
        val m1 = elements.get.group(1)
        if (mapNameSource.contains(m1)) {
          return mapNameSource.get(m1).get
        }
        return null
      }
    }

    // name.accessor or ${name.accessor} or {name.key} or name.key or $name.key
    {
      //      val regex = s"$Begin$Element$Separator$Element$End|$Begin$Marker$Open$Element$Separator$Element$Close$End|$Begin$Marker$Element$Separator$Element$End".r
      val regex = s"$Begin$Marker$Element$Separator$Element$End".r
      expr match {
        case regex(m1, m2) =>
          if (mapNameSource.contains(s"$m1.$m2")) {
            val t = mapNameSource.get(s"$m1.$m2").get
            return null
          } else if (dictMessages.contains(m1)) {
            val expression = "%s.get(\"%s\")".format(dictMessages.get(m1).get, m2)
            val variableName = "%s.%s".format(dictMessages.get(m1).get, m2)
            return eval.Tracker(variableName, m1, "Any", true, m2, expression)
          } else if (mapNameSource.contains(m1)) {
            val t = mapNameSource.get(m1).get
            if (Datatypes.isStringDictionary(t.typeName)) {
              val expression = "%s.get(\"%s\")".format(t.getExpression, m2)
              val variableName = "%s.%s".format(t.getExpression, m2)
              return eval.Tracker(variableName, m1, "String", true, m2, expression)
            } else {
              return null
            }
          }
        case _ => ;
      }
    }

    // name(<number>) or ${name}(<number>) or ${name(<number>)}
    {
      // val regex = s"$Begin$Element$Index$End|$Begin$Marker$Open$Element$Index$Close$End|$Begin$Marker$Open$Element$Close$Index$End".r
      val regex = s"$Begin$Marker$Element$Index$End".r
      expr match {
        case regex(m1, m2) =>
          if (mapNameSource.contains(m1)) {
            val t = mapNameSource.get(m1).get
            if (Datatypes.isStringArray(t.typeName)) {
              val expression = "%s%s".format(m1, m2)
              val variableName = "%s%s".format(m1, m2)
              return eval.Tracker(variableName, m1, "String", true, "", expression)
            }
          }
        case _ => ;
      }
    }

    null
  }
  /** Find all logical column names that are encode in this expression $name
    *
    * $var
    * $ns.$var
    *
    * \$([a-zA-Z0-9_]+)
    * \$\{([a-zA-Z0-9_]+\.[a-zA-Z0-9_]+)\}
    *
    * @param expression
    * @return
    */
  def ExtractColumnNames(expression: String): Set[String] = {

    // Extract single and multiple components names
    val regex1 = """\$([a-zA-Z0-9_]+)""".r
    val regex2 = """\$\{([a-zA-Z0-9_]+\.[a-zA-Z0-9_]+)\}""".r
    val m1 = regex1.findAllMatchIn(expression).toArray
    val m2 = regex2.findAllMatchIn(expression).toArray
    m1.map(m => m.group(1)).toSet ++  m2.map(m => m.group(1)).toSet
  }

  /** Replace all logical column names with the variables
    *
    * @param expression expression to update
    * @param mapNameSource name to variable mapping
    * @return string with the result
    */
  def FixupColumnNames(expression: String, mapNameSource: Map[String, Tracker], aliaseMessages: Map[String, String]): String = {

    val regex1 = """\$([a-zA-Z0-9_]+)""".r
    val regex2 = """\$\{([a-zA-Z0-9_]+\.[a-zA-Z0-9_]+)\}""".r

    def ReplaceWithResolve(regex: Regex, expression: String): String = {
      val m = regex.pattern.matcher(expression)
      val sb = new StringBuffer
      var i = 0
      while (m.find) {
        val name = m.group(1)
        val resolvedName = ResolveName(name, aliaseMessages)
        m.appendReplacement(sb, mapNameSource.get(resolvedName).get.getExpression)
        i = i + 1
      }
      m.appendTail(sb)
      sb.toString
    }

    val expression1 = ReplaceWithResolve(regex1, expression)
    val expression2 = ReplaceWithResolve(regex2, expression1)
    return expression2
  }

  def ResolveNames(names: Set[String], aliaseMessages: Map[String, String] ) : Map[String, String] =  {

    names.map ( n => {
      val (alias, name) = splitAlias(n)
      if(alias.length>0) {
        val a = aliaseMessages.get(alias)
        if(a.isEmpty) {
          throw new Exception("Missing alias %s for %s".format(alias, n))
        } else {
          n -> "%s.%s".format(a.get, name)
        }
      } else {
        n -> n
      }
    }).toMap
  }

  def ResolveName(n: String, aliaseMessages: Map[String, String] ) : String =  {

    val (alias, name) = splitAlias(n)
    if(alias.length>0) {
      val a = aliaseMessages.get(alias)
      if(a.isEmpty) {
        throw new Exception("Missing alias %s for %s".format(alias, n))
      } else {
        "%s.%s".format(a.get, name)
      }
    } else {
      n
    }
  }

  def ResolveAlias(n: String, aliaseMessages: Map[String, String] ) : String =  {

    val a = aliaseMessages.get(n)
    if(a.isEmpty) {
      throw new Exception("Missing alias %s".format(n))
    } else {
      a.get
    }
  }

  /** Split a name into alias and field name
    *
    * @param name Name
    * @return
    */
  def splitAlias(name: String): (String, String) = {
    val elements = name.split('.')
    if(elements.length==1)
      ("", name)
    else
      ( elements.head, elements.slice(1, elements.length).mkString(".") )
  }

  def Coerce(outType: String, inType: String, expr: String): String = {
    if(outType!=inType && outType.nonEmpty) {
      // Find the conversion and wrap the call
      if(Conversion.builtin.contains(inType) && Conversion.builtin.get(inType).get.contains(outType))
      {
        val conversionExpr = Conversion.builtin.get(inType).get.get(outType).get
        "conversion.%s(%s)\n".format(conversionExpr, expr)
      }
      else
      {
        expr
      }
    } else {
      expr
    }
  }
}
