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
package com.ligadata.runtime

import java.sql.Timestamp
import java.util.Date
import scala.math.Ordering.BigIntOrdering

/*
 jtm locations

 // Each step can cause an error
 // track back to the originating instruction
 transformation -> grok instance
 transformation -> [dependency set] -> compute
 transformation -> [dependency set] -> output -> compute|where|mapping

a) string
b) id -> to map

*/
object Conversion {

  // Source -> Map[Target, Function]  // Source -> Map[Target, Function]
  val builtin: Map[String, Map[String, String]] = Map(
    "Int" -> Map("String" -> "ToString"),
    "Double" -> Map("String" -> "ToString"),
    "Boolean" -> Map("String" -> "ToString"),
    "Date" -> Map("String" -> "ToString"),
    "BigDecimal" -> Map("String" -> "ToString"),
    "Long" -> Map("String" -> "ToString"),
    "String" -> Map("Int" -> "ToInteger",
      "Double" -> "ToDouble",
      "Boolean" -> "ToBoolean",
      "Date" -> "ToDate",
      "Timestamp" -> "ToTimestamp",
      "BigDecimal" -> "ToBigDecimal",
      "Long" -> "ToLong"),
    "Any" -> Map("Int" -> "ToInteger",
      "Double" -> "ToDouble",
      "Boolean" -> "ToBoolean",
      "Date" -> "ToDate",
      "Timestamp" -> "ToTimestamp",
      "BigDecimal" -> "ToBigDecimal",
      "Long" -> "ToBLong")
  )
}

class Conversion {

  val formatDate = new java.text.SimpleDateFormat("yyyy-MM-dd")
  val formatTs = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  //var errors : Map[Integer, String] = Map.empty[Integer, String]

  def ToInteger(v: Any): Int = {
    v match {
      case y: String => y.toInt
      case y: Int => y
    }
  }

  def ToDouble(v: Any): Double = {
    v match {
      case y: String => y.toDouble
      case y: Double => y
    }
  }

  def ToBoolean(v: Any): Boolean = {
    v match {
      case y: String => y.toBoolean
      case y: Boolean => y
    }
  }

  def ToDate(v: Any): Date = {
    v match {
      case y: String => formatDate.parse(y)
      case y: Date => y
    }
  }

  def ToTimestamp(v: Any): Timestamp = {
    v match {
      case y: String => new Timestamp(formatTs.parse(y).getTime)
      case y: Timestamp => y
    }
  }

  def ToBigDecimal(v: Any): BigDecimal = {
    v match {
      case y: String => BigDecimal.apply(y)
      case y: BigDecimal => y
    }
  }

  def ToLong(v: Any): Long = {
    v match {
      case y: String => y.toLong
      case y: Long => y
    }
  }

  def ToString(v: Any): String = {
    v match {
      case y: String => y
      case y: Int => y.toString
      case y: Double => y.toString
      case y: Boolean => y.toString
      case y: Timestamp => formatTs.format(y)
      case y: Date => formatDate.format(y)
      case y: BigDecimal => y.toString
      case y: Long => y.toString
    }
  }
}
