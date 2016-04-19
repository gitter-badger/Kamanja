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

package com.ligadata.pmml.udfs
import org.scalatest.FlatSpec

class TestCustomUdfs extends FlatSpec {

  "Concat" should "return one String with no space" in {
    //input
    val input1: String = "textOne"
    val input2: String = "textTwo"
    //expected
    val expected: String = "textOnetextTwo"
    //actual
    val actual: String = CustomUdfs.Concat(input1, input2)
    assert(expected === actual)
  }

  it should "concat Ints" in {
    //input
    val input1: Int = 1
    val input2: Int = 5
    //expected
    val expected: String = "15"
    //actual
    val actual: String = CustomUdfs.Concat(input1, input2)
    assert(expected === actual)
  }

  it should "replace null values with empty string" in {
    //input
    val input1: String = "textOne"
    val input2: String = null
    //expected
    val expected: String = "textOne"
    //actual
    val actual: String = CustomUdfs.Concat(input1, input2)
    assert(expected === actual)
  }

  "matchTermsetBoolean" should "ignore cases" in {
    //input
    val inputString = "ligadata company"
    val context = Array("Apple", "Google", "LIGADATA")
    val degree = 1
    //actual
    val actual = CustomUdfs.matchTermsetBoolean(inputString, context, degree)
    assert(actual)
  }

  it should "return false if inputString is null" in {
    //input
    val inputString = null
    val context = Array("Apple", "Google", "LIGADATA")
    val degree = 1
    val expected = false
    //actual
    val actual = CustomUdfs.matchTermsetBoolean(inputString, context, degree)
    assertResult(expected)(actual)
  }

  it should "return false if context is null" in {
    //input
    val inputString = "ligadata"
    val context = null
    val degree = 1
    //expected
    val expected = false
    //actual
    val actual = CustomUdfs.matchTermsetBoolean(inputString, context, degree)
    assertResult(expected)(actual)
  }

  "getMatchingTokens" should "matched string delimited by ." in {
    //input
    val inputString = "ligadata company"
    val context = Array("Apple", "Google", "LIGADATA")
    //expected
    val expected = ".LIGADATA"
    //actual
    val actual = CustomUdfs.getMatchingTokens(inputString, context)
    assert(expected === actual)
  }

  it should "return false if inputString is null" in {
    //input
    val inputString = "ligadata"
    val context = null
    //expected
    val expected = ""
    //actual
    val actual = CustomUdfs.getMatchingTokens(inputString, context)
    assert(expected === actual)
  }

  it should "return false if context is null" in {
    //input
    val inputString = null
    val context = Array("Apple", "Google", "LIGADATA")
    //expected
    val expected = ""
    //actual
    val actual = CustomUdfs.getMatchingTokens(inputString, context)
    assert(expected === actual)
  }

}