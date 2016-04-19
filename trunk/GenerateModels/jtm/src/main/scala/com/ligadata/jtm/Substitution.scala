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
package com.ligadata.jtm

/**
  * Created by joerg on 1/20/16.
  */

import javax.annotation.RegEx

import org.scalatest.matchers.Matcher
import scala.collection.mutable.Map

// Note this can create an endless recursion Run( "{Hello}" with "Hello"-> "{World}", World->"{Hello}"
class Substitution {

  def Add(name: String, value: String) = {
    subts += (name -> value)
  }

  def Run(value: String): String = {

      val r = Replace(value)
      if(r!=value)
        Run(r)
      else
        r
  }

  def Replace(value: String): String = {
    val r = subts.foldLeft(value)((s: String, x:(String,String)) => ({
      val regextmp = "\\{" + x._1 + "\\}"
      val regex = regextmp.r
      regex.replaceAllIn( s,  java.util.regex.Matcher.quoteReplacement (x._2))
    }))
    r
  }

  var subts = Map.empty[String, String]
}
