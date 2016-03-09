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
package com.ligadata

import com.ligadata.jtm.eval.Types
import com.ligadata.jtm.nodes.Root
import com.ligadata.kamanja.metadata.{BaseAttributeDef, MiningModelType, ModelRepresentation, ModelDef}

/**
  *
  */
package object jtm {

  def MakeModelDef(inputFile: String ) : ModelDef = {

    // Load Json
    val root = Root.fromJson(inputFile)

    val inputVars : Array[BaseAttributeDef] = null
    val outputVars: Array[BaseAttributeDef] = null
    val isReusable: Boolean = true

    // Collect all messages consumed
    //
    val dependencyToTransformations = Types.ResolveDependencies(root)

    // Return tru if we accept the message, flatten the messages into a list
    //
    val msgs = dependencyToTransformations.foldLeft(Set.empty[String]) ( (r, d) => {
      d._1.foldLeft(r) ((r, n) => {
        r ++ Set(n)
      })
    })

    val msgConsumed: String = msgs.mkString(",")

    val supportsInstanceSerialization : Boolean = false

    /*
    val modelRepresentation: ModelRepresentation = ModelRepresentation.JAR
    val miningModelType : MiningModelType = MiningModelType.UNKNOWN
    val inputVars : Array[BaseAttributeDef] = null
    val outputVars: Array[BaseAttributeDef] = null
    val isReusable: Boolean = false
    val msgConsumed: String = ""
    val supportsInstanceSerialization : Boolean = false
    */
    new ModelDef(ModelRepresentation.UNKNOWN, MiningModelType.UNKNOWN, inputVars, outputVars, isReusable, msgConsumed, supportsInstanceSerialization)
  }
}
