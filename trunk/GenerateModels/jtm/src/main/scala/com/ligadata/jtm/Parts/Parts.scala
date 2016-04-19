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
package com.ligadata.jtm

/**
  * Created by joerg on 1/20/16.
  */
object Parts {

  val header =
    """|/*
       | * Copyright 2016 ligaDATA
       | *
       | * Licensed under the Apache License, Version 2.0 (the "License");
       | * you may not use this file except in compliance with the License.
       | * You may obtain a copy of the License at
       | *
       | *     http://www.apache.org/licenses/LICENSE-2.0
       | *
       | * Unless required by applicable law or agreed to in writing, software
       | * distributed under the License is distributed on an "AS IS" BASIS,
       | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       | * See the License for the specific language governing permissions and
       | * limitations under the License.
       | */""".stripMargin

  val imports =
    """|import com.ligadata.KamanjaBase._
       |import com.ligadata.KvBase.TimeRange
       |import com.ligadata.kamanja.metadata.ModelDef""".stripMargin

  val factory =
    """|class {factoryclass.name}(modelDef: ModelDef, nodeContext: NodeContext) extends ModelInstanceFactory(modelDef, nodeContext) {
       |  override def createModelInstance(): ModelInstance = return new {modelclass.name}(this)
       |  override def getModelName: String = "{model.name}"
       |  override def getVersion: String = "{model.version}"
       |
       |  override def createResultObject(): ModelResultBase = new MappedModelResults()
       |}""".stripMargin

  val model =
    """|class {modelclass.name}(factory: ModelInstanceFactory) extends ModelInstance(factory) {
       |  val conversion = new com.ligadata.runtime.Conversion
       |  override def execute(txnCtxt: TransactionContext, execMsgsSet: Array[ContainerOrConcept], triggerdSetIndex: Int, outputDefault: Boolean): Array[ContainerOrConcept] = {
       |    //
       |    {model.grok}
       |    //
       |    {model.methods}
       |    // Evaluate messages
       |    {model.message}
       |    // Main dependency -> execution check
       |    //
       |    {model.code}
       |  }
       |}""".stripMargin
}
