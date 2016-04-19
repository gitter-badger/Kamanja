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

import java.io.{File, StringReader}

import com.ligadata.jtm.LogTrait
import com.ligadata.jtm.nodes.{Grok, Root}
import org.aicer.grok.dictionary.GrokDictionary

/**
  *
  */
object GrokHelper extends LogTrait {


  /** Extract the key from an expressions
    *
    * @param expression
    * @return
    */
  def ExtractDictionaryKeys(expression: String): Set[String] = {

    val r = """\{[\s]*([A-Za-z][A-Za-z0-9]*)[\s]*:[\s]*([A-Za-z][A-Za-z0-9]*)[\s]*\}""".r
    val m = r.findAllMatchIn(expression)
    m.foldLeft(Set.empty[String])( (r, m) => {
      r + m.group(2)
    })
  }

  /** Extract the key from an expressions
    *
    * @param expression
    * @return
    */
  def ConvertToGrokPattern( expression: String): String = {

    val r = """\{[\s]*([A-Za-z][A-Za-z0-9]*)[\s]*:[\s]*([A-Za-z][A-Za-z0-9]*)[\s]*\}""".r
    val m = r.findAllMatchIn(expression)
    m.map(m => {
       "%%{%s:%s}".format(m.group(1), m.group(2))
    }).mkString(" ")
  }

  /** Returns the list of files after validating
    *
    * @param root
    * @return
    */
  def FileList(root: Root): Array[String] = {
      FileList(root.grok)
  }

  /**
    *
    * @param root
    */
  def Validate(root: Root): Unit = {
    Validate(root.grok)
  }

  /**
    *
    * @param grok
    * @return
    */
  def Validate(grok: Grok): GrokDictionary = {

    val dict = new GrokDictionary

    if(grok.builtInDictionary) {
      dict.addBuiltInDictionaries()
    }

    // Add all the grok dictionaries
    val filecheck = grok.file.foldLeft(Array.empty[String])( (r, f) => {
      val file = new File(f)
      if( file.exists) {
        dict.addDictionary(file)
        r
      } else {
        r :+ "Grok file %s not found".format(f)
      }
    })

    if(filecheck.length > 0) {
      //throw new Exception(filecheck.mkString("\n"))
    }

    // Walk through the dictionay
    grok.patterns.foreach( p =>
      dict.addDictionary(new StringReader(p._1 + " " + p._2))
    )

    dict.bind()
    dict
  }

  /** Extracts the set with the unique files names to load
    *
    * @param groks
    * @return
    */
  private def FileList(groks: scala.collection.Map[String, Grok]): Array[String] = {

    val filelist = groks.foldLeft(Set.empty[String]) ( (r, grok) => {
        grok._2.file.foldLeft(r) ((r, f) => {
          r + f
      })
    })

    filelist.toArray
  }

  /** Validates all the element in the map with the grok configuration
    *
    * @param groks
    * @return
    */
  private def Validate(groks: scala.collection.Map[String, Grok]): Unit= {
      groks.map(grok => Validate(grok._2))
  }

}
