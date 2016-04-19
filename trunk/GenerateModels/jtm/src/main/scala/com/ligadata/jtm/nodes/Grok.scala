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
  * Grok expression support
  */
class Grok {

  /** true, indicates to use the internal dictornary
    *
    */
  val builtInDictionary: Boolean = true

  /** match string in the form of {EMAIL: email}, used if not provided in
    * the transformation
    */
  @SerializedName("match")
  val matchstring: String = ""

  /** List of files to load into the matcher
    *
    */
  val file: Array[String] = Array.empty[String]

  /** Inline specification for additional patterns
    *
    */
  val patterns: scala.collection.Map[String, String] = Map.empty[String, String]
}
