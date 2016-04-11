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
package com.ligadata.jtm.nodes

import com.google.gson.annotations.SerializedName

/**
  *
  */
class Transformation {

  /** Lists of lists with dependencies
    * If a list of dependency is statisfied processing will be comenced
    */
  val dependsOn: Array[Array[String]] = Array.empty[Array[String]]

  /** List with grok matches
    *
    */
  @SerializedName("grok match")
  val grokMatch: scala.collection.Map[String, String] = scala.collection.Map.empty[String, String]

  /** Map with computes that apply to all outputs
    *
    */
  val computes: scala.collection.Map[String, Compute] = scala.collection.Map.empty[String, Compute]

  /** Specialized processing for outputs
    *
    */
  val outputs: scala.collection.Map[String, Output] = scala.collection.Map.empty[String, Output]

  /** Single comment, to be output to code
    *
    */
  val comment: String = ""

  /** Multiple comments, to be output to code
    *
    */
  val comments: Array[String] = Array.empty[String]

  def Comment() : String = {
    if (comment.length() > 0)
        "// " + comment
    else
      comments.map(comment => "// " + comment).mkString("\n")
  }
}
