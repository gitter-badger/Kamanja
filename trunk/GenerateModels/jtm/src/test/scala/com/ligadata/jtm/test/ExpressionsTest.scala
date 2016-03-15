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

import com.ligadata.jtm._
import org.apache.commons.io.FileUtils
import org.apache.logging.log4j.LogManager
import com.ligadata.jtm.nodes._
import org.skyscreamer.jsonassert._
import org.scalatest.{BeforeAndAfter, FunSuite}

/**
  *
  */
class ExpressionsTest  extends FunSuite with BeforeAndAfter {

  test("test01") {
    val actual = eval.Expressions.ExtractColumnNames("")
    assert(Set.empty[String] == actual)
  }

  test("test02") {
    val actual = eval.Expressions.ExtractColumnNames("$na1_me")
    assert(Set("na1_me") == actual)
  }

  test("test03") {
    val actual = eval.Expressions.ExtractColumnNames("${ns_3.na1_me}")
    assert(Set("ns_3.na1_me") == actual)
  }

}
