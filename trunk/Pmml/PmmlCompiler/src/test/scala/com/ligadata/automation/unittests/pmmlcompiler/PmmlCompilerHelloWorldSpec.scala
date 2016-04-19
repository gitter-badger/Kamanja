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

package com.ligadata.automation.unittests.pmmlcompiler

import com.ligadata.kamanja.metadata.MdMgr
import com.ligadata.kamanja.metadataload.MetadataLoad
import com.ligadata.pmml.compiler.PmmlCompiler
import org.scalatest._

import org.apache.logging.log4j._

class PmmlCompilerHelloWorldSpec extends FlatSpec with BeforeAndAfter {
  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  before {
    try {
      logger.info("Adding HelloWorld message metadata...");
      val argsList = List(("system", "id", "system", "int", false, null), ("system", "name", "system", "string", false, null), ("system", "score", "system", "int", false, null))
      val mdLoader = new MetadataLoad(MdMgr.mdMgr, "", "", "", "")
      mdLoader.initialize

      val ownerId: String = "kamanja"
      val uniqueId: Long = 1
      val mdElementId: Long = 1
      val schemaId: Int = 1
      val avroSchema:String = "kamanja"
      val version : Long = 1
      val tenantId : String = "kamanja"
      MdMgr.GetMdMgr.AddFixedMsg("com.ligadata.kamanja.samples.messages", "msg1", "com.ligadata.kamanja.samples.messages.V1.msg1", argsList, ownerId, tenantId, uniqueId, mdElementId, schemaId, avroSchema, version)
    }
    catch {
      case e: Exception => throw new Exception("Failed to add messagedef", e)
    }
  }

  it should "compile HelloWorld kPMML model" in {
    val kPmmlXmlStr =

      """
      <PMML version="4.1" xmlns="http://www.dmg.org/PMML-4_1">
        <Header copyright="LigaDATA. Copyright 2014" description="Hello World">
          <Application name="HelloWorld" version="00.01.00"/>
        </Header>
        <DataDictionary numberOfFields="3">
          <DataField dataType="msg1" displayName="msg" name="msg" optype="categorical"/>
          <DataField dataType="EnvContext" displayName="globalContext" name="gCtx" optype="categorical"/>
          <DataField dataType="container" displayName="parameters" name="parameters">
            <Value property="valid" value="gCtx"/>
            <Value property="valid" value="msg"/>
          </DataField>
          <DataField dataType="string" displayName="name" name="name" optype="categorical"/>
          <DataField dataType="integer" displayName="predictedField" name="predictedField" optype="categorical"/>
          <DataField name="NamespaceSearchPath" displayName="NamespaceSearchPath" dataType="container">
            <Value value="Messages, com.ligadata.kamanja.samples.messages" property="valid"/>
          </DataField>
        </DataDictionary>
        <TransformationDictionary>
          <DerivedField dataType="boolean" name="ScoreCheck" optype="categorical">
              <Apply function="Equal">
                <FieldRef field="msg.score"/>
                <Constant dataType="integer">1</Constant>
              </Apply>
          </DerivedField>
        </TransformationDictionary>
        <RuleSetModel algorithmName="RuleSet" functionName="classification" modelName="HelloWorldModel">
          <MiningSchema>
            <MiningField name="name" usageType="supplementary"/>
            <MiningField name="predictedField" usageType="predicted"/>
          </MiningSchema>
          <RuleSet defaultScore="0">
            <RuleSelectionMethod criterion="firstHit"/>
            <SimpleRule id="ScoreCheck" score="1b">
              <SimplePredicate field="ScoreCheck" operator="equal" value="true"/>
            </SimpleRule>
          </RuleSet>
        </RuleSetModel>
      </PMML>
      """

    val compiler = new PmmlCompiler(MdMgr.GetMdMgr, "ligadata", logger, false, Array[String]())
    val (classStr, modDef) = compiler.compile(kPmmlXmlStr, "/tmp", false)

    assert(null != classStr)
    assert(null != modDef)
    val sep = "==============================================================================================="
    // info("Generated scala code\n" + sep + ">\n" + classStr + "\n<" + sep)

    logger.info("Generated scala code\n" + sep + ">\n" + classStr + "\n<" + sep)
  }
}
