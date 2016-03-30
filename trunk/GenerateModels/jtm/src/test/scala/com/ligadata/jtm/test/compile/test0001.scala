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
package com.ligadata.jtm.test.filter
import com.ligadata.KamanjaBase._
import com.ligadata.KvBase.TimeRange
import com.ligadata.kamanja.metadata.ModelDef
import com.ligadata.Utils._
class Factory(modelDef: ModelDef, nodeContext: NodeContext) extends ModelInstanceFactory(modelDef, nodeContext) {
  //override def isValidMessage(msg: MessageContainerBase): Boolean = {
  //  msg.isInstanceOf[com.ligadata.kamanja.test.V1000000.msg1]
  //}
  override def createModelInstance(): ModelInstance = return new Model(this)
  override def getModelName: String = "com.ligadata.jtm.test.filter"
  override def getVersion: String = "0.0.1"
  override def createResultObject(): ModelResultBase = new MappedModelResults()
}
class Model(factory: ModelInstanceFactory) extends ModelInstance(factory) {
  /*override*/ def run(txnCtxt: TransactionContext, outputDefault: Boolean): Array[BaseMsg]  = {
    //
    //
    def exeGenerated_test1_1(msg1: com.ligadata.kamanja.test.V1000000.msg1): Array[BaseMsg] = {
      // in scala, type could be optional
      val out3: Int = msg1.in1 + 1000
      def process_o1(): Array[BaseMsg] = {
        if (!(msg1.in2 != -1 && msg1.in2 < 100)) return Array.empty[BaseMsg]
        val t1: String = "s:" + msg1.in2.toString()
        val result : com.ligadata.kamanja.test.V1000000.msg2 = null //new com.ligadata.kamanja.test.V1000000.msg2
        result.out2 = t1
        //result.rowNumber = msg1.rowNumber
        result.out3 = msg1.in2
        //result.timePartitionData = msg1.timePartitionData
        result.out4 = msg1.in3
        result.out1 = msg1.in1
        //result.transactionId = msg1.transactionId
        Array(result.asInstanceOf[BaseMsg])
      }
      process_o1()
    }
    // Evaluate messages
    //val msg1 = txnCtxt.getMessages("com.ligadata.kamanja.test.msg1").headOption.getOrElse(null).asInstanceOf[com.ligadata.kamanja.test.V1000000.msg1]
    val msg1: com.ligadata.kamanja.test.V1000000.msg1 = null
    // Main dependency -> execution check
    //
    val results: Array[BaseMsg] =
      if(msg1!=null) {
        exeGenerated_test1_1(msg1)
      } else {
        Array.empty[BaseMsg]
      }
    results
  }
}
