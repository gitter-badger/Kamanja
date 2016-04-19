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
import java.util.regex.Pattern
import java.util.regex.Matcher
import scala.io.Source

class FcnSubstitution {

    def findAndReplace(m: Matcher, vars: scala.collection.mutable.Map[String, String])(callback: String => String):String = {
        val sb = new StringBuffer
        while (m.find) { 
            val replStr = vars(m.group(1))
            m.appendReplacement(sb, callback(replStr)) 
        }
        m.appendTail(sb)
        sb.toString
    }

    def makePostitionalSubstitutions(template : String, replacementMap : scala.collection.mutable.Map[String, String]) = { 
         val m = Pattern.compile("""(%[0-9]+%)""").matcher(template)

        findAndReplace(m, replacementMap){ x => x }
    }

    def makeSubstitutions(template : String, replacementMap : scala.collection.mutable.Map[String, String]) = { 
         val m = Pattern.compile("""(%[A-Za-z0-9_]+%)""").matcher(template)

        findAndReplace(m, replacementMap){ x => x }
    }

    /** 
     *  Make substitutions on the contents of the supplied file, using the key/replacement value pairs in the supplied
     *  Map.  Keys in the file are expected to be of the form "%some1_keytobereplaced%"
     */
    def makeFileSubstitutions(templateFile : String, replacementMap : scala.collection.mutable.Map[String, String]) = { 
        var contents = Source.fromFile(templateFile, "ASCII").mkString
        val m = Pattern.compile("""(%[A-Za-z0-9_]+%)""").matcher(contents)
        findAndReplace(m, replacementMap){ x => x }
    }
  
}


object FcnSubstitution extends App {

	override def main (args : Array[String]) {
 
		var patternMap : HashMap[String,String] = HashMap[String,String]() 
		var substitutionMap : HashMap[String,String] = HashMap[String,String]() 

		substitutionMap += ("%1%" -> "mbr.Inpatient_Claims")
		substitutionMap += ("%2%" -> "Icd9_Dgns_Cds")
		substitutionMap += ("%3%" -> "49300")
		substitutionMap += ("%4%" -> "49392")
		substitutionMap += ("%5%" -> "true")
		
		patternMap += ("Builtins.ContainerFilterAnyBetween" -> "%1%.filter((itm) => Builtins.AnyBetween(itm.%2%, %3%, %4%, %5%))")
      
		/** do the port map substitutions */
        var varSub = new FcnSubstitution()
        println(varSub.makeSubstitutions(patternMap.apply("Builtins.ContainerFilterAnyBetween"),substitutionMap))  

     }
}

