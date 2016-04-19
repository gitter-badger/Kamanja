
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

package com.ligadata.KamanjaBase

trait InputData {
  val dataInput: String // Just for Debugging/Logging purpose. Engine will parse the data to corresponding type and give it to populate
}

class DelimitedData(val dataInput: String, val delimiters: DataDelimiters) extends InputData {
  var tokens: Array[String] = _
  var curPos: Int = _
}

class JsonData(val dataInput: String) extends InputData {
  var root_json: Option[Any] = _
  var cur_json: Option[Any] = _
}

class XmlData(val dataInput: String) extends InputData {
  var root_xml: scala.xml.Elem = _
  var cur_xml: scala.xml.Elem = _
}

class KvData(val dataInput: String, val delimiters: DataDelimiters) extends InputData {
  var dataMap: scala.collection.immutable.Map[String, String] = _
}

