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

package com.ligadata.MetadataAPI


import com.ligadata.kamanja.metadata.MiningModelType._

import scala.collection.JavaConverters._
import scala.collection.immutable.List

import java.io.{ByteArrayInputStream, PushbackInputStream, InputStream}
import java.nio.charset.StandardCharsets
import javax.xml.bind.{ValidationEvent, ValidationEventHandler}
import javax.xml.transform.sax.SAXSource
import java.util.{List => JList}

import com.ligadata.kamanja.metadata.{MdMgr, MessageDef, ModelDef, MiningModelType, ModelRepresentation}
import org.jpmml.model.{JAXBUtil, ImportFilter}
import org.dmg.pmml._
import org.jpmml.evaluator._
import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory




/**
 * JpmmlSupport add, rebuild, and remove of JPMML based models from the Kamanja metadata store.
 *
 * It builds an instance of the shim model with a JPMML evaluator appropriate for the supplied InputStream
 * containing the pmml model text.
 *
 * @param mgr the active metadata manager instance
 * @param modelNamespace the namespace for the model
 * @param modelName the name of the model
 * @param version the version of the model in the form "MMMMMM.NNNNNN.mmmmmmm"
 * @param msgNamespace the message namespace of the message that will be consumed by this model
 * @param msgName the message name
 * @param pmmlText the pmml to be ingested.
 */
class JpmmlSupport(mgr : MdMgr
                   , val modelNamespace : String
                   , val modelName : String
                   , val version: String
                   , val msgNamespace : String
                   , val msgName: String
                   , val msgVersion : String
                   , val pmmlText: String) extends LogTrait {

    def CreateModel : ModelDef = {
        val reasonable : Boolean = (
                    mgr != null &&
                    modelNamespace != null && modelNamespace.nonEmpty &&
                    modelName != null && modelName.nonEmpty &&
                    version != null && version.nonEmpty &&
                    msgNamespace != null && msgNamespace.nonEmpty &&
                    msgName != null && msgName.nonEmpty &&
                    pmmlText != null && pmmlText.nonEmpty
                )
        val modelDef : ModelDef = if (reasonable) {
            val inputStream: InputStream = new ByteArrayInputStream(pmmlText.getBytes(StandardCharsets.UTF_8))
            val is = new PushbackInputStream(inputStream)

            val reader = XMLReaderFactory.createXMLReader()
            reader.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
            val filter = new ImportFilter(reader)
            val source = new SAXSource(filter, new InputSource(is))
            val unmarshaller = JAXBUtil.createUnmarshaller
            unmarshaller.setEventHandler(SimpleValidationEventHandler)

            val pmml: PMML = unmarshaller.unmarshal(source).asInstanceOf[PMML]
            val modelEvaluatorFactory = ModelEvaluatorFactory.newInstance()
            val modelEvaluator = modelEvaluatorFactory.newModelManager(pmml)

            val m : ModelDef = if (modelEvaluator != null) {
                /**
                 *
                 * Construct the ModelDef.  Note that the jpmmlAdapter is the "shim" model for ALL JPMML models, For
                 * this reason, we get the shim model from the metadata to get the model def's jarName and jarDeps.
                 *
                 * The "com.ligadata.jpmml.jpmmlAdapter" object gets instantiated at cluster initialization and it
                 * is able to discern if an incoming message can be processed by the one that the engine has referred
                 * from its ModelInfo record.
                 *
                 */
                val shimModelNamespace : String = "com.ligadata.jpmml"
                val shimModelName : String = "jpmmlAdapter"
                val onlyActive : Boolean = true
                val modelVersion : Long = MdMgr.ConvertVersionToLong(version)
                val optShimModel : Option[ModelDef] = mgr.Model(shimModelNamespace, shimModelName, modelVersion, onlyActive)
                val shimModel : ModelDef = optShimModel.getOrElse(null)

                val jarName : String = if (shimModel != null) shimModel.jarName else null
                val jarDeps : scala.Array[String] = if (shimModel != null) shimModel.dependencyJarNames else null
                val phyName : String = if (shimModel != null) shimModel.typeString else null

                val messageVersion : Long = MdMgr.ConvertVersionToLong(msgVersion)
                val optInputMsg : Option[MessageDef] = mgr.Message(msgNamespace, msgName, messageVersion, onlyActive)
                val inputMsg : MessageDef = mgr.ActiveMessage(msgNamespace, msgName)
                val activeFieldNames : JList[FieldName] = modelEvaluator.getActiveFields
                val outputFieldNames : JList[FieldName] = modelEvaluator.getOutputFields

                /** NOTE: activeFields are not used at this point... for jpmml models, only the message will be
                  * available as an input variable
                  */
                val activeFields : scala.Array[DataField] = {
                    activeFieldNames.asScala.map(nm => modelEvaluator.getDataField(nm))
                }.toArray
                val inVars : List[(String,String,String,String,Boolean,String)]  =
                    List[(String,String,String,String,Boolean,String)](("msg"
                                                                      , inputMsg.typeString
                                                                      , inputMsg.NameSpace
                                                                      , inputMsg.Name
                                                                      , false
                                                                      , null))

                val outputFields : scala.Array[OutputField] = {
                    outputFieldNames.asScala.map(nm => modelEvaluator.getOutputField(nm))
                }.toArray
                val outVars : List[(String, String, String)] = outputFields.map(fld => {
                    val fldName : String = fld.getName.getValue
                    val dataType : String = fld.getDataType.value
                    (fldName, "System", dataType)
                }).toList


                val isReusable : Boolean = true
                val supportsInstanceSerialization : Boolean = false // FIXME: not yet
                val recompile : Boolean = false
                val modelDef : ModelDef = mgr.MakeModelDef(modelNamespace
                                                        , modelName
                                                        , phyName
                                                        , ModelRepresentation.JPMML
                                                        , isReusable
                                                        , s"$msgNamespace.$msgName"
                                                        , pmmlText
                                                        , DetermineMiningModelType(modelEvaluator)
                                                        , inVars
                                                        , outVars
                                                        , MdMgr.ConvertVersionToLong(version)
                                                        , jarName
                                                        , jarDeps
                                                        , recompile
                                                        , supportsInstanceSerialization)
                modelDef
            } else {
                null
            }
            m
        } else {
            logger.error(s"One or more arguments to JpmmlSupport.CreateModel were bad .. model name = $modelNamespace.$modelName, message name=$msgNamespace.$msgName, version=$version, pmmlText=$pmmlText")
            null
        }
        modelDef
    }

    /**
     * Answer the kind of model that this is based upon the factory returned
     * @param evaluator a ModelEvaluator
     * @return the MiningModelType
     */
    private def DetermineMiningModelType(evaluator : ModelEvaluator[_]) : MiningModelType= {

        val modelType : MiningModelType = evaluator match {
            case a:AssociationModelEvaluator => MiningModelType.AssociationModel
            case c:ClusteringModelEvaluator => MiningModelType.ClusteringModel
            case g:GeneralRegressionModelEvaluator => MiningModelType.GeneralRegressionModel
            case m:MiningModelEvaluator => MiningModelType.MiningModel
            case n:NaiveBayesModelEvaluator => MiningModelType.NaiveBayesModel
            case nn:NearestNeighborModelEvaluator => MiningModelType.NearestNeighborModel
            case nn1:NeuralNetworkEvaluator => MiningModelType.NeuralNetwork
            case r:RegressionModelEvaluator => MiningModelType.RegressionModel
            case rs:RuleSetModelEvaluator => MiningModelType.RuleSetModel
            case sc:ScorecardEvaluator => MiningModelType.Scorecard
            case svm:SupportVectorMachineModelEvaluator => MiningModelType.SupportVectorMachineModel
            case sc:TreeModelEvaluator => MiningModelType.TreeModel
            case _ => MiningModelType.Unknown
        }
        modelType
    }

    private object SimpleValidationEventHandler extends ValidationEventHandler {
        def handleEvent(event: ValidationEvent): Boolean = {
            val severity: Int = event.getSeverity
            severity match {
                case ValidationEvent.ERROR => false
                case ValidationEvent.FATAL_ERROR => false
                case _ => true
            }
        }
    }
}

