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

import org.scalatest.{BeforeAndAfter, FunSuite}
/**
  *
  */
class Test002Test  extends FunSuite with BeforeAndAfter {

  val logger = LogManager.getLogger(this.getClass.getName)

  test("test") {

    val fileInput = getClass.getResource("/test002.jtm/test.jtm").getPath
    val fileOutput = getClass.getResource("/test002.jtm/test.scala.result").getPath
    val fileExpected = getClass.getResource("/test002.jtm/test.scala.expected").getPath
    val metadataLocation = getClass.getResource("/metadata").getPath

    val compiler = CompilerBuilder.create().
      setSuppressTimestamps().
      setInputFile(fileInput).
      setOutputFile(fileOutput).
      setMetadataLocation(metadataLocation).
      build()

    compiler.Execute()

    val expected = FileUtils.readFileToString(new File(fileExpected))
    val actual = FileUtils.readFileToString(new File(fileOutput))
    logger.info("actual path={}", fileOutput)
    logger.info("expected path={}", fileExpected)

    assert(actual == expected)
  }

}
