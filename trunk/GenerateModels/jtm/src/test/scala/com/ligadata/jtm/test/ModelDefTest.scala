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
import com.ligadata.jtm.CompilerBuilder
import com.ligadata.jtm.nodes.Root
import com.ligadata.kamanja.metadata.{MiningModelType, ModelRepresentation, ModelDef}
import org.apache.commons.io.FileUtils
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.skyscreamer.jsonassert.JSONAssert

/**
  * Created by joerg on 3/9/16.
  */
class ModelDefTest  extends FunSuite with BeforeAndAfter {

  test("test01") {
    val fileInput = getClass.getResource("/modeldeftest/test.jtm").getPath
    val metadataLocation = getClass.getResource("/metadata").getPath

    val compiler = CompilerBuilder.create().
      setSuppressTimestamps().
      setInputFile(fileInput).
      setMetadataLocation(metadataLocation).
      build()

    compiler.Execute()
    val md: ModelDef =  compiler.MakeModelDef()

    assert(ModelRepresentation.JAR == md.modelRepresentation)
    assert(MiningModelType.JTM == md.miningModelType)
    assert("MDName" == md.Name)
    assert("Description Test" == md.Description)
    assert("com.ligadata.jtm.test.modeldeftest"==md.NameSpace)
    assert(Array("com.ligadata.kamanja.test.msg2", "com.ligadata.kamanja.test.msg4").deep == md.outputMsgs.deep)
    //assert(""== md.inputMsgSets.toString)
  }
}
