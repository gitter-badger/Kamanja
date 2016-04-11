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

package com.ligadata.samples.models

import com.ligadata.KamanjaBase._
import com.ligadata.kamanja.metadata.ModelDef;

class HelloWorld2ModelFactory(modelDef: ModelDef, nodeContext: NodeContext) extends ModelInstanceFactory(modelDef, nodeContext) {
  override def isValidMessage(msg: ContainerInterface): Boolean = return ( msg.isInstanceOf[helloworld_msg_def] || msg.isInstanceOf[helloworld_msg_def_2] )
  override def createModelInstance(): ModelInstance = return new HelloWorld2Model(this)
  override def getModelName: String = "HelloWorld2Model" 
  override def getVersion: String = "0.0.1"
  override def createResultObject(): ModelResultBase = new MappedModelResults()
}

class HelloWorld2Model(factory: ModelInstanceFactory) extends ModelInstance(factory) {
  
   override def execute(txnCtxt: TransactionContext, outputDefault: Boolean):ModelResultBase = {
     
     var helloWorld : helloworld_msg_def =  txnCtxt.getMessage().asInstanceOf[helloworld_msg_def]
     
        if(helloWorld.score!=1)
          return null;
     
     var actualResults: Array[Result] = Array[Result](new Result("Id",helloWorld.id),
                                                        new Result("Name",helloWorld.Name),
                                                        new Result("Score",helloWorld.score))
     return factory.createResultObject().asInstanceOf[MappedModelResults].withResults(actualResults)
   }
}
