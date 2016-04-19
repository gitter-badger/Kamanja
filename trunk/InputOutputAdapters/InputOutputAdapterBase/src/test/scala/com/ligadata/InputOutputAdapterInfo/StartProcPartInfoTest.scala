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

package com.ligadata.InputOutputAdapterInfo

/**
  * Created by will on 2/9/16.
  */
import org.scalatest._

class StartProcPartInfoTests extends FlatSpec with BeforeAndAfter {
  private var startProcPartInfo: StartProcPartInfo = null

  before {
    startProcPartInfo = new StartProcPartInfo
  }

  "StartProcPartInfo" should "be instantiated with _key of type PartitionUniqueRecordKey set to null" in {
    assert(startProcPartInfo._key == null)
  }

  it should "be instantiated with _val of type PartitionUniqueRecordValue set to null" in {
    assert(startProcPartInfo._val == null)
  }

  it should "be instantiated with _validateInfoVal of type PartitionUniqueRecord set to null" in {
    assert(startProcPartInfo._validateInfoVal == null)
  }
}
