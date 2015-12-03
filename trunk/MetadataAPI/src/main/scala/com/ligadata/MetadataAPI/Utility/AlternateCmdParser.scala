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

package com.ligadata.MetadataAPI.Utility

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

object AlternateCmdParser extends JavaTokenParsers {

    /** Match a run of any characters between '(' and ')' that do not include either '(' or ')' ...
      * i.e., no recursion yet
      * @return Parser[Regex]
      */
    private def valueRegex: Parser[Regex] = "(" ~> "([^()]*)".r <~ ")" ^^ {_.r}

    /** Obtain the value of the run of characters
      * @return Parser[String] that has the enclosed value in it
      */
    private def value: Parser[String] =  { valueRegex ^^ (_.toString) }

    /** Answer the parameter name/value pair
      * @param key the identifier preceding the value regexpr
      * @return Parser[(String,String)] that has the parameter name in _._1 and value in _._2
      */
    private def valueExpr(key:String): Parser[(String,String)] =
        value ^^ {
            case expr => (key,expr)
        }

    /** Parse "ident (some run of characters)" returning (String,String) for (ident's value,"some run of characters")
      @return Parser[(String,String)] that has the parameter name in _._1 and value in _._2
      */
    private def namedParameter: Parser[(String,String)] = ident >> valueExpr

    /** Parse an arbitrary number of "ident (some run of characters)" patterns found in the string supplied to apply.
      * @see apply(String) for more information.
      * @return Parser[List[(String,String)] representing a List of the parameter name/value pairs
      */
    private def namedParameters:Parser[List[(String,String)]] = namedParameter.*

    /** match the command name and put remainder of the named elements in a list of tuple2 (to produce map)
      * @return a tuple containing the command name and a list of parameter name/value pairs
      */
    private def cmdWithParameters: Parser[(String,List[(String,String)])] = {
        ident ~ namedParameters ^^ {
            case name ~ values => (name, values)
        }
    }

    /** Invoke the top level rule, cmdWithParameters, with the supplied argument string. Produce a cmd and list of Tuple2[String,String]
      * @param args the command string to parse
      * @return a ParseResult containing a tuple with the command name and a list of parameter name/value pairs
      *
      */
    private def apply(args: String):ParseResult[(String, List[(String,String)])] = parseAll(cmdWithParameters, args)

    /**
     * Parse the supplied command string with the AlternateCmdParser. Answer the name of the command and a
     * map of name/value pairs for the arguments found if successful.  Parse failures produce NO command name (None) and
     * an empty Map[String,String] of command arguments.
     * @param cmdStr the command to parser (without the MetadataAPI config)
     * @return the command name and its args
     */
    def parse(cmdStr : String) : (Option[String], Map[String, String]) = {
        val (cmd, argMap) : (Option[String], Map[String, String]) = if (cmdStr != null && cmdStr.size > 0) {
            val (cmdName, resultsMap): (Option[String], Map[String, String]) = AlternateCmdParser(cmdStr) match {
                case Success(parms, _) => {
                    val (cmdName, keyValPairs): (String, List[(String, String)]) = parms

                    val m: Map[String, String] = keyValPairs.map(pair => (pair._1.trim.toLowerCase, pair._2.trim)).toMap
                    (Some(cmdName), m)
                }
                case NoSuccess(msg, remaining) => {
                    println(s"We have a problem: $msg at: ${remaining.pos}")
                    (None, Map[String,String]())
                }
            }
            (cmdName, resultsMap)
        } else {
            (None, Map[String,String]())
        }
        (cmd, argMap)
    }

    /**
     *
     * AlternateCmdParser accepts a single string that looks like a kamanja command, without shell command and metadata api config.  For example,
     *
     * '''
     *      "addModel type (jpmml) name ( com.anotherCo.jpmml.DahliaRandomForest ) version( 000000.000001.000001 ) message (com.botanicalCo.jpmml.DahliaMsg                   ) pmml(/anotherpath/dahliaRandomForest.xml)"
     * '''
     *
     * The parser will extract the command name and a List[(String,String)] of argName/argValue tuples. For the test, these are printed as a Map.
     *
     * @param args
     */
    def main(args: Array[String]) {

        val cmdParms =
            """addModel type (jpmml) name ( com.botanicalCo.jpmml.IrisRandomForest ) version( 000000.000001.000000 ) message (com.botanicalCo.jpmml.IrisMsg                   ) pmml(/somepath/irisRandomForest.xml)""".stripMargin
        val cmdParms1 =
            """addModel type jpmml) name ( com.botanicalCo.jpmml.IrisRandomForest ) version( 000000.000001.000000 ) message (com.botanicalCo.jpmml.IrisMsg   ) pmml(/somepath/irisRandomForest.xml)""".stripMargin

        val doBuiltinTest : Boolean = false

        /** if there is an argument (should be a kamanja like command), use it */
        if (args != null && args.size == 1 && ! doBuiltinTest) {
            val (cmdName, resultsMap): (String, Map[String, String]) = AlternateCmdParser(args(0)) match {
                case Success(parms, _) => {
                    val (cmdName, keyValPairs) : (String, List[(String,String)]) = parms

                    val m: Map[String, String] = keyValPairs.map(pair => (pair._1.trim, pair._2.trim)).toMap
                    (cmdName,m)
                }
                case NoSuccess(msg, remaining) => {
                    println(s"We have a problem: $msg at: ${remaining.pos}")
                    ("cmd failed", Map[String,String]())
                }
            }
            println(s"command = $cmdName(parameters = $resultsMap)")
        }  else { /** otherwise demonstrate a successful parse and a failure with the builtin test cases */
            println(s"processing ... ${'"'}$cmdParms${'"'}")
            val (cmdName, resultsMap): (String, Map[String, String]) = AlternateCmdParser(cmdParms) match {
                case Success(parms, _) => {
                    val (cmdName, keyValPairs) : (String, List[(String,String)]) = parms

                    val m: Map[String, String] = keyValPairs.map(pair => (pair._1.trim, pair._2.trim)).toMap
                    (cmdName,m)
                }
                case NoSuccess(msg, remaining) => {
                    println(s"We have a problem: $msg at: ${remaining.pos}")
                    ("cmd failed", Map[String,String]())
                }
            }
            println(s"command = $cmdName(parameters = $resultsMap)")

            println(s"processing ... ${'"'}$cmdParms1${'"'}")
            val (cmdName1, resultsMap1): (String, Map[String, String]) = AlternateCmdParser(cmdParms1) match {
                case Success(parms, _) => {
                    val (cmdName, keyValPairs) : (String, List[(String,String)]) = parms

                    val m: Map[String, String] = keyValPairs.map(pair => (pair._1.trim, pair._2.trim)).toMap
                    (cmdName,m)
                }
                case NoSuccess(msg, remaining) => {
                    println(s"Parse error: $msg at: ${remaining.pos}")
                    ("cmd failed", Map[String,String]())
                }
            }
            println(s"command = $cmdName1(parameters = $resultsMap1)")
        }
    }

}

