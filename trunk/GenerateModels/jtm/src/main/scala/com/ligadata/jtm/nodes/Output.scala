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

/**
  *
  */
class Output {

  /** Mapping variables / expressions to output attributes
    *
    */
  val mapping: scala.collection.Map[String, String] = scala.collection.Map.empty[String, String]

  /** Filter to be checked
    * expression of the target language expected
    */
  val where: String = ""

  /** Map with computations
    *
    */
  val computes: scala.collection.Map[String, Compute] = scala.collection.Map.empty[String, Compute]

  /** If true, map by name the  outputs if not provided
    *
    */
  val mappingByName: Boolean = false
}
