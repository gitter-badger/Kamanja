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

import com.ligadata.jtm
import com.ligadata.jtm.nodes.Root
import com.ligadata.kamanja.metadata.ModelDef
import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.skyscreamer.jsonassert.JSONAssert

/**
  * Created by joerg on 3/9/16.
  */
class ModelDefTest  extends FunSuite with BeforeAndAfter {

  test("test01") {
    val fileInput = getClass.getResource("/test002.jtm/test.jtm").getPath
    val md: ModelDef =  jtm.MakeModelDef(fileInput)
    assert("com.ligadata.kamanja.test.msg1,com.ligadata.kamanja.test.msg3" == md.msgConsumed)
  }
}
