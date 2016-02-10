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
import scala.reflect.runtime.universe._
class Factory(modelDef: ModelDef, nodeContext: NodeContext) extends ModelInstanceFactory(modelDef, nodeContext) {
  // if more than 1 input we have to find the correct instance
  override def isValidMessage(msg: MessageContainerBase): Boolean = {
    msg.isInstanceOf[com.ligadata.kamanja.test001.v1000000.msg1]
  }
  override def createModelInstance(): ModelInstance = return new Model(this)
  // Provided in json
  override def getModelName: String = "com.ligadata.jtm.test.filter"
  override def getVersion: String = "0.0.1"
  override def createResultObject(): ModelResultBase = new MappedModelResults()
}
class Model(factory: ModelInstanceFactory) extends ModelInstance(factory) {

  def exeGenerated_test1_1(msg1: com.ligadata.kamanja.test001.v1000000.msg1): Array[Result] = {

    val out3 = msg1.in1 + 1000
    
    def process_o1(): Array[Result] = {
      if (!(msg1.in2 != -1 && msg1.in2 < 100)) return Array.empty[Result]
      val t1: String = "s:" + (msg1.in2).toString()
      Array[Result](new Result("transactionId", msg1.transactionId), new Result("out1", msg1.in1), new Result("out4", msg1.in3), new Result("out2", t1), new Result("out3", out3))
    }

    process_o1() // ++ process_*()
  }

  override def execute(txnCtxt: TransactionContext, outputDefault: Boolean): ModelResultBase = {

    // ToDo we expect an array of messages
    val msgs =  Array(txnCtxt.getMessage())

    var msg1: com.ligadata.kamanja.test001.v1000000.msg1 = null

    // Match all the messages
    msgs.foreach( _ match {
        case msg: com.ligadata.kamanja.test001.v1000000.msg1 => msg1 = msg
        case _: AnyRef => ;
    })

    val results: Array[Result] =
      if(msg1!=null) {
        exeGenerated_test1_1(msg1)
      }
      else {
        Array.empty[Result]
      }

    factory.createResultObject().asInstanceOf[MappedModelResults].withResults(results)
  }
}
