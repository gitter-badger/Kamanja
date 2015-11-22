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

#!/bin/bash
exec scala "$0" "$@"
!#

/***************************************************************************************
 * sbtProjDependencies.scala extracts the jars from the supplied string (an invocation of
 * sbt 'show someProject/fullClasspath')
 *
 **************************************************************************************/

import scala.collection.mutable._
import scala.collection.immutable.Seq
import scala.io.Source
import sys.process._
import java.util.regex.Pattern
import java.util.regex.Matcher


class sbtProjDependencies(rawDepsListStr: String) {

    def transform : Array[String] = {
 
        val depList = """(Attributed.*\))""".r
        val depsCloser = """\((.*)\)""".r
        var argNames : Array[String] = null
        val reTypes = "[a-zA-Z0-9_]+:|, [a-zA-Z0-9_]+: "
        var argTypes : Array[String] = null
      
        var attributedElems : ArrayBuffer[String] = ArrayBuffer[String]()
        var attributedElemsSansAttr : ArrayBuffer[String] = ArrayBuffer[String]()
        try {
            depList.findAllIn(rawDepsListStr).matchData.foreach { m => attributedElems ++= m.matched.split(", ") }
            attributedElems.map( chunk => {
                val itm : String = chunk
                depsCloser.findAllIn(chunk).matchData.foreach(mc => {
                    val mcMatched = mc.matched
                    attributedElemsSansAttr += mcMatched
            })})
        } catch {
            case e : Exception => { }
        }
        val implClassesAndDepJars : Array[String] = attributedElemsSansAttr.map(itm => trimBothEndsOfParens(itm)).toArray
        val depJars : Array[String] = implClassesAndDepJars.filter(itm => itm.endsWith(".jar"))
        val depClasses : Array[String] = implClassesAndDepJars.filter(itm => (! itm.endsWith(".jar")))
        val depJarsFromClasses : Array[String] = jarsInsteadOfClassesFor(depClasses)
        val combined : ArrayBuffer[String] = ArrayBuffer[String]()
        depJarsFromClasses.foreach(jarpath => combined += jarpath )
        depJars.foreach(jarpath => combined += jarpath )
        combined.toArray
    }
    
    def trimBothEndsOfParens(str : String) : String = { 
      str.split('(').last.split(')').head
    }
    
    def jarsInsteadOfClassesFor(depClasses : Array[String]) : Array[String] = {
      
        val classDirRemoved : Array[String] = depClasses.map(itm => itm.split("classes").head)
        val jarNames : Array[String] = classDirRemoved.map(itm => {
            val dircmd : String = "ls " + itm.trim + "*jar"
            
            val cmdSeq : Seq[String] = Seq("bash", "-c", dircmd)
            val result : String = Process(cmdSeq).!!.trim
            result
        })
      
      
        jarNames
    }
    
    def cleanDeps : String = { 
        val contents  : Array[String] = transform
        contents.toString
    }
  
}

object sbtProjDependencies extends App {
  
    def usage : String = {
"""
sbtProjectDependencies --sbtDeps <deps from "`sbt 'show <proj>/fullClasspath' | grep 'List(Attributed'`" 
                       --emitCp <t | 1 | f | 0> 
                       --emitCpWin <t | 1 | f | 0> 
                       --emitFQJars <t | 1 | f | 0>
                       --emitJarNamesOnly <t | 1 | f | 0>
                       --emitJarNamesOnlyList <t | 1 | f | 0>
                       --emitJsonJarNamesOnlyList <t | 1 | f | 0>
                       --emitAll <t | 1 | f | 0>
        where sbtDeps (Required) contains the 'money' result (output line containing 'List(Attributed') from an sbt project fullClasspath task 
              emitCp will cause a ':' separated class path style string to be printed 
              emitCpWin will cause a ';' separated class path style string to be printed 
              emitFQJars will print the jars with full path names (one jar per line)
              emitJarNamesOnly will print the jar names without paths (one jar per line)
              emitJarNamesOnlyList will print the jar names without paths as a comma delimited list
              emitJsonJarNamesOnlyList will print the jar names without paths delimited by quote marks and spaces suitable for inclusion in json string
              emitAll will print all of the above. 
      
        Note: If multiple emits are requested, there will be one blank line between each emission to stdout.  At least
        one 'emit' named parameter must be supplied with a 't' or '1' value.
      
"""
    }

    def testPaths : String = { 
"""
[info] List(Attributed(/home/rich/github/Med/Kamanja/trunk/Pmml/PmmlUdfs/target/scala-2.11/classes), Attributed(/home/rich/github/Med/Kamanja/trunk/Metadata/target/scala-2.11/classes), Attributed(/home/rich/github/Med/Kamanja/trunk/Pmml/PmmlRuntime/target/scala-2.11/classes), Attributed(/home/rich/github/Med/Kamanja/trunk/KamanjaBase/target/scala-2.11/classes), Attributed(/home/rich/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.7.jar), Attributed(/home/rich/.ivy2/cache/org.joda/joda-convert/jars/joda-convert-1.6.jar), Attributed(/home/rich/.ivy2/cache/joda-time/joda-time/jars/joda-time-2.3.jar), Attributed(/home/rich/.ivy2/cache/log4j/log4j/bundles/log4j-1.2.17.jar), Attributed(/home/rich/.ivy2/cache/org.json4s/json4s-native_2.11/jars/json4s-native_2.11-3.2.9.jar), Attributed(/home/rich/.ivy2/cache/org.json4s/json4s-core_2.11/jars/json4s-core_2.11-3.2.9.jar), Attributed(/home/rich/.ivy2/cache/org.json4s/json4s-ast_2.11/jars/json4s-ast_2.11-3.2.9.jar), Attributed(/home/rich/.ivy2/cache/com.thoughtworks.paranamer/paranamer/jars/paranamer-2.6.jar), Attributed(/home/rich/.ivy2/cache/org.scala-lang/scalap/jars/scalap-2.11.0.jar), Attributed(/home/rich/.ivy2/cache/org.scala-lang/scala-compiler/jars/scala-compiler-2.11.0.jar), Attributed(/home/rich/.ivy2/cache/org.scala-lang/scala-reflect/jars/scala-reflect-2.11.0.jar))
"""
    }
      
    override def main (args : Array[String]) {

        if (args.length == 0) println(usage)
        val arglist = args.toList
        type OptionMap = Map[Symbol, String]
        //println(arglist)
        def nextOption(map: OptionMap, list: List[String]): OptionMap = {
          list match {
            case Nil => map
            case "--sbtDeps" :: value :: tail =>
              nextOption(map ++ Map('sbtDeps -> value), tail)
            case "--emitCp" :: value :: tail =>
              nextOption(map ++ Map('emitCp -> value), tail)
            case "--emitCpWin" :: value :: tail =>
              nextOption(map ++ Map('emitCpWin -> value), tail)
            case "--emitFQJars" :: value :: tail =>
              nextOption(map ++ Map('emitFQJars -> value), tail)
            case "--emitJarNamesOnly" :: value :: tail =>
              nextOption(map ++ Map('emitJarNamesOnly -> value), tail)
            case "--emitJarNamesOnlyList" :: value :: tail =>
              nextOption(map ++ Map('emitJarNamesOnlyList -> value), tail)
            case "--emitJsonJarNamesOnlyList" :: value :: tail =>
              nextOption(map ++ Map('emitJsonJarNamesOnlyList -> value), tail)
            case "--emitAll" :: value :: tail =>
              nextOption(map ++ Map('emitAll -> value), tail)
            case option :: tail =>
              println("Unknown option " + option)
              println(usage)
              sys.exit(1)
          }
        }
    
        val options = nextOption(Map(), arglist)
        
        val sbtDeps = if (options.contains('sbtDeps)) options.apply('sbtDeps) else null
        val emitCp = if (options.contains('emitCp)) options.apply('emitCp) else null
        val emitCpWin = if (options.contains('emitCpWin)) options.apply('emitCpWin) else null
        val emitFQJars = if (options.contains('emitFQJars)) options.apply('emitFQJars) else null
        val emitJarNamesOnly = if (options.contains('emitJarNamesOnly)) options.apply('emitJarNamesOnly) else null
        val emitJarNamesOnlyList = if (options.contains('emitJarNamesOnlyList)) options.apply('emitJarNamesOnlyList) else null
        val emitJsonJarNamesOnlyList = if (options.contains('emitJsonJarNamesOnlyList)) options.apply('emitJsonJarNamesOnlyList) else null
        val emitAll = if (options.contains('emitAll)) options.apply('emitAll) else null
        
        val emitClassPath : Boolean = (emitCp == "1" || emitCp == "t" || emitAll == "1" || emitAll == "t")
        val emitWinClassPath : Boolean = (emitCpWin == "1" || emitCpWin == "t" || emitAll == "1" || emitAll == "t")
        val emitJarsWithPath : Boolean = (emitFQJars == "1" || emitFQJars == "t" || emitAll == "1" || emitAll == "t")
        val emitJarsNamesSansPath : Boolean = (emitJarNamesOnly == "1" || emitJarNamesOnly == "t" || emitAll == "1" || emitAll == "t")
        val emitJarsNamesSansPathList : Boolean = (emitJarNamesOnlyList == "1" || emitJarNamesOnlyList == "t" || emitAll == "1" || emitAll == "t")
        val emitJsonJarsNamesSansPathList : Boolean = (emitJsonJarNamesOnlyList == "1" || emitJsonJarNamesOnlyList == "t" || emitAll == "1" || emitAll == "t")
        
//        val reasonableArguments : Boolean = (sbtDeps != null && sbtDeps.size > 0 && //sbtDeps.startsWith("[info] List(Attributed") && 
        val reasonableArguments : Boolean = (sbtDeps != null && sbtDeps.size > 0 && 
            (emitClassPath || emitWinClassPath || emitJarsWithPath  || emitJarsNamesSansPath || emitJarsNamesSansPathList || emitJsonJarsNamesSansPathList))
        if (! reasonableArguments) {
            println("Your arguments are not satisfactory...Usage:")
            println(usage)
            sys.exit(1)
        }
            
        /**
         *  Extract an array of the jar dependencies as full path names 
         */
        val depsCleaner = new sbtProjDependencies(sbtDeps)
        val depJarPaths : Array[String] = depsCleaner.transform
        /** produce another array from the jarPaths with just the jar names (sans path) */
        val depJars : Array[String] = depJarPaths.map(jarPath => jarPath.split('/').last)
        
        /** Print */
        if (emitClassPath) {
            var i : Int = 0
            depJarPaths.foreach(path => {
                if (i != 0) print(":")
                i += 1
                print(path)
            })
            println
            println
        }
        
        if (emitWinClassPath) {
            var i : Int = 0
            depJarPaths.foreach(path => {
                if (i != 0) print(";")
                i += 1
                print(path)
            })
            println
            println
        }
        
        if (emitJarsWithPath) {
            depJarPaths.foreach(path => println(path))
            println
        }
        
        if (emitJarsNamesSansPath) {
            depJars.foreach(path => println(path))
            println
        }
        
        if (emitJarsNamesSansPathList) {
            var i : Int = 0
            depJars.foreach(path => {
                if (i != 0) print(", ")
                i += 1
                print(path)
            })
            println
        }
        
        if (emitJsonJarsNamesSansPathList) {
            var i : Int = 0
            depJars.foreach(path => {
                if (i != 0) print(", ")
                i += 1
                print(s"${'\\'}${'"'}$path${'\\'}${'"'}")
            })
            println
        }
        
        println
    }
}

sbtProjDependencies.main(args)


