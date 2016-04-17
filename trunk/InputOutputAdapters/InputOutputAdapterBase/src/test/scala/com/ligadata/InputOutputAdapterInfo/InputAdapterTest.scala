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

import com.ligadata.HeartBeat.MonitorComponentInfo
import com.ligadata.KamanjaBase.NodeContext
import org.scalatest._
/**
  * Created by will on 2/9/16.
  */

private class MockInputAdapter extends InputAdapter {
  override val inputConfig: AdapterConfiguration = new AdapterConfiguration
  inputConfig.Name = "MockInputAdapter"

  override def DeserializeKey(k: String): PartitionUniqueRecordKey = null

  override def DeserializeValue(v: String): PartitionUniqueRecordValue = null

  override def getAllPartitionBeginValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = null

  override def Shutdown: Unit = ???

  // each value in partitionInfo is (PartitionUniqueRecordKey, PartitionUniqueRecordValue, Long, PartitionUniqueRecordValue). // key, processed value, Start transactionid, Ignore Output Till given Value (Which is written into Output Adapter) & processing Transformed messages (processing & total)
  override def GetAllPartitionUniqueRecordKey: Array[PartitionUniqueRecordKey] = null

  override def getAllPartitionEndValues: Array[(PartitionUniqueRecordKey, PartitionUniqueRecordValue)] = null

  override def StartProcessing(partitionInfo: Array[StartProcPartInfo], ignoreFirstMsg: Boolean): Unit = ???

  override def StopProcessing: Unit = ???

  override def getComponentStatusAndMetrics: MonitorComponentInfo = null

  override def getComponentSimpleStats: String = ""

  override val nodeContext: NodeContext = null
}

class InputAdapterTests extends FlatSpec with BeforeAndAfter with Matchers {

  private def inputAdapter: InputAdapter = new MockInputAdapter

  "InputAdapter" should "be instantiated with UniqueName set to Name: {adapter config name}" in {
    assert(inputAdapter.UniqueName == "{\"Name\" : \"MockInputAdapter\"}")
  }

  it should "be instantiated with Category set to Input" in {
    inputAdapter.Category should equal ("Input")
  }

}
