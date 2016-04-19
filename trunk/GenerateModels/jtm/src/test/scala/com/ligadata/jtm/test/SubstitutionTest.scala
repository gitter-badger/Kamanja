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

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.ligadata.jtm.Substitution

/**
  * Created by joerg on 1/21/16.
  */
class SubstitutionTest extends FunSuite with BeforeAndAfter {

  test("test01") {
    var s = new Substitution
    s.Add("xx", "zz")
    val result = s.Run("{xx}")
    assert(result == "zz")
  }

  test("test02") {
    var s = new Substitution
    s.Add("xx", "zzxx{yy}")
    s.Add("yy", "zz")

    val result = s.Run("yy{xx}zz")
    assert(result == "yyzzxxzzzz")
  }
}
