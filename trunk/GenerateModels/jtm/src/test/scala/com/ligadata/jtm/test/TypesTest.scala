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
package com.ligadata.jtm.test

import java.io.File

import com.ligadata.jtm.nodes.Root
import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfter, FunSuite}
import com.ligadata.jtm.eval.Types

/**
  *
  */
class TypesTest extends FunSuite with BeforeAndAfter {

  test("test01") {
    val fileInput = getClass.getResource("/test003.jtm/test1.jtm").getPath
    val t = Root.fromJson(fileInput)

    val actual = Types.CollectTypes(t)
    val expected : Map[String, Set[String]] = Map(
      "Int" -> Set("test1/zipcode", "test1/zipcode1"),
      "IpInfo" -> Set("test1/ipinfo"),
      "String" -> Set("test1/tmp8")
    )
    assert(expected == actual)
  }

  test("test02") {
    val fileInput = getClass.getResource("/test003.jtm/test1.jtm").getPath
    val t = Root.fromJson(fileInput)

    val actual = Types.CollectMessages(t)
    val expected : Map[String, Set[String]] = Map(
      "c3" -> Set("c3"),
      "c4" -> Set("c4"),
      "com.ligadata.kamanja.samples.concepts.c1" -> Set("c1", "c2"),
      "com.ligadata.kamanja.samples.messages.msg1" -> Set("m1"),
      "com.ligadata.kamanja.samples.messages.msg2" -> Set("m2"),
      "com.ligadata.kamanja.samples.messages.msg4" -> Set("com.ligadata.kamanja.samples.messages.msg4"),
      "com.ligadata.kamanja.samples.messages.omsg2" -> Set("omsg2"),
      "m3" -> Set("m3"),
      "m4" -> Set("m4"),
      "m5" -> Set("m5"),
      "omsg5" -> Set("omsg5"),
      "omsg4" -> Set("omsg4")
    )

    assert(expected == actual)
  }

}
