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

import com.ligadata.Serialize._

import scala.collection.JavaConverters._
import scala.collection.immutable.List

import java.io.{ByteArrayInputStream, PushbackInputStream, InputStream}
import java.nio.charset.StandardCharsets
import javax.xml.bind.{ValidationEvent, ValidationEventHandler}
import javax.xml.transform.sax.SAXSource
import java.util.{List => JList}

import com.ligadata.kamanja.metadata._
import com.ligadata.jpmml.JpmmlAdapter
import org.jpmml.model.{JAXBUtil, ImportFilter}
import org.dmg.pmml._
import org.jpmml.evaluator._
import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory




/**
 * JpmmlSupport - Add, rebuild, and remove of JPMML based models from the Kamanja metadata store.
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
 * @param msgVersion the version of the message to be used for this model
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

    /** Answer a ModelDef based upon the arguments supplied to the class constructor.
      * @param recompile certain callers are creating a model to recompile the model when the message it consumes changes.
      *                  pass this flag as true in those cases to avoid com.ligadata.Exceptions.AlreadyExistsException
      * @return a ModelDef
      */
    def CreateModel(recompile: Boolean = false) : ModelDef = {
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

            val modelDe : ModelDef = if (modelEvaluator != null) {
                /**
                 * Construct a ModelDef instance of com.ligadata.jpmml.JpmmlAdapter and the JPMML evaluator that will
                 * be used to interpret messages for the modelNamespace.modelName.version supplied here.  The supplied
                 * pmml is parsed to an org.dmg.pmml tree
                 *
                 * The adapter is needed to get the appropriate jars and jar dependencies for the adapter.  The
                 * evaluator is created now to obtain the output dependencies for the model.
                 *
                 */
                val shimModelNamespaceName : String = JpmmlAdapter.ModelName()
                val shimVersion : String = JpmmlAdapter.Version()
                val onlyActive : Boolean = true
                val modelVersion : Long = MdMgr.ConvertVersionToLong(shimVersion)
                val optShimModel : Option[ModelDef] = mgr.Model(shimModelNamespaceName, modelVersion, onlyActive)
                val shimModel : ModelDef = optShimModel.orNull

                val jarName : String = if (shimModel != null) shimModel.jarName else null
                val jarDeps : scala.Array[String] = if (shimModel != null) shimModel.dependencyJarNames else null
                val phyName : String = if (shimModel != null) shimModel.typeString else null

                /** make sure new msg is there. */
                val msgver : Long = MdMgr.ConvertVersionToLong(msgVersion)
                val optInputMsg : Option[MessageDef] = mgr.Message(msgNamespace, msgName, msgver, onlyActive)
                val inputMsg : MessageDef = optInputMsg.orNull
                val activeFieldNames : JList[FieldName] = modelEvaluator.getActiveFields
                val outputFieldNames : JList[FieldName] = modelEvaluator.getOutputFields
                val targetFieldNames : JList[FieldName] = modelEvaluator.getTargetFields; /** target|predicted usage types */

                /** NOTE: activeFields are not used at this point... for jpmml models, only the message will be
                  * available as an input variable
                  */
                val activeFields : scala.Array[DataField] = {
                    activeFieldNames.asScala.map(nm => modelEvaluator.getDataField(nm))
                }.toArray
                val modelD : ModelDef = if (inputMsg != null) {
                    val inVars: List[(String, String, String, String, Boolean, String)] =
                        List[(String,String,String,String,Boolean,String)](("msg"
                                                                      , inputMsg.typeString
                                                                      , inputMsg.NameSpace
                                                                      , inputMsg.Name
                                                                      , false
                                                                      , null))

                    /** fields found in the output section */
                    val outputFields: scala.Array[OutputField] = {
                        outputFieldNames.asScala.map(nm => modelEvaluator.getOutputField(nm))
                    }.toArray
                    val outputFieldVars: List[(String, String, String)] = outputFields.map(fld => {
                        val fldName: String = fld.getName.getValue
                        val dataType: String = fld.getDataType.value
                        (fldName, MdMgr.SysNS, dataType)
                    }).toList
                    /** get the concrete data fields for either 'target' or 'predicted' ... type info found there. */
                    val targetDataFields: scala.Array[DataField] = {
                        targetFieldNames.asScala.map(nm => {
                            modelEvaluator.getDataField(nm)
                        })
                    }.toArray
                    val targVars: List[(String, String, String)] = targetDataFields.map(fld => {
                        val fldName: String = fld.getName.getValue
                        val dataType: String = fld.getDataType.value
                        (fldName, MdMgr.SysNS, dataType)
                    }).toList

                    /**
                     * Model output fields will consist of the target variables (either target or predicted fields from mining
                     * schema) and the fields found in the output section (if any)
                     */
                    val outVars: List[(String, String, String)] = (targVars ++ outputFieldVars).distinct

                    val isReusable: Boolean = true
                    val supportsInstanceSerialization: Boolean = false // FIXME: not yet

                    val withDots : Boolean = false
                    val msgVersionFormatted : String = MdMgr.ConvertLongVersionToString(msgver, withDots)
                    val model: ModelDef = mgr.MakeModelDef(modelNamespace
                                                        , modelName
                                                        , phyName
                                                        , ModelRepresentation.JPMML
                                                        , isReusable
                                                        , s"$msgNamespace.$msgName.$msgVersionFormatted"
                                                        , pmmlText
                                                        , DetermineMiningModelType(modelEvaluator)
                                                        , inVars
                                                        , outVars
                                                        , MdMgr.ConvertVersionToLong(version)
                                                        , jarName
                                                        , jarDeps
                                                        , recompile
                                                        , supportsInstanceSerialization)

                    /** dump the model def to the log for time being */
                    logger.info(modelDefToString(model))
                    model
                } else {
                    logger.error(s"The supplied message def is not available in the metadata... msgName=$msgNamespace.$msgName.$msgVersion ... a model definition will not be created for model name=$modelNamespace.$modelName.$version")
                    null
                }
                modelD
            } else {
                logger.error(s"The JPMML evaluator could not be created for model $modelNamespace.$modelName.$version ... a model definition will not be created for model name=$modelNamespace.$modelName.$version")
                null
            }
            modelDe
        } else {
            logger.error(s"One or more arguments to JpmmlSupport.CreateModel were bad .. model name = $modelNamespace.$modelName, message name=$msgNamespace.$msgName, version=$version, pmmlText=$pmmlText")
            null
        }
        modelDef
    }

    /** Prepare a new model with the new PMML source supplied in the constructor.
      * @return a newly constructed model def that reflects the new PMML source
      */
    def UpdateModel : ModelDef = {
        logger.debug("UpdateModel is a synonym for CreateModel")
        val recompile : Boolean = false
        CreateModel(recompile)
     }

    /**
     * Answer the kind of model that this is based upon the factory returned
     * @param evaluator a ModelEvaluator
     * @return the MiningModelType
     */
    private def DetermineMiningModelType(evaluator : ModelEvaluator[_]) : MiningModelType= {

        val modelType : MiningModelType = evaluator match {
            case a:AssociationModelEvaluator => ASSOCIATIONMODEL
            case c:ClusteringModelEvaluator => CLUSTERINGMODEL
            case g:GeneralRegressionModelEvaluator => GENERALREGRESSIONMODEL
            case m:MiningModelEvaluator => MININGMODEL
            case n:NaiveBayesModelEvaluator => NAIVEBAYESMODEL
            case nn:NearestNeighborModelEvaluator => NEARESTNEIGHBORMODEL
            case nn1:NeuralNetworkEvaluator => NEURALNETWORK
            case r:RegressionModelEvaluator => REGRESSIONMODEL
            case rs:RuleSetModelEvaluator => RULESETMODEL
            case sc:ScorecardEvaluator => SCORECARD
            case svm:SupportVectorMachineModelEvaluator => SUPPORTVECTORMACHINEMODEL
            case sc:TreeModelEvaluator => TREEMODEL
            case _ => UNKNOWN
        }
        modelType
    }

    /**
     * SimpleValidationEventHandler used by the JAXB Util that decomposes the PMML string supplied to CreateModel.
     */
    private object SimpleValidationEventHandler extends ValidationEventHandler {
        /**
         * Answer false whenever the validation event severity is ERROR or FATAL_ERROR.
         * @param event a ValidationEvent issued by the JAXB SAX utility that is parsing the PMML source text.
         * @return flag to indicate whether to continue with the parse or not.
         */
        def handleEvent(event: ValidationEvent): Boolean = {
            val severity: Int = event.getSeverity
            severity match {
                case ValidationEvent.ERROR => false
                case ValidationEvent.FATAL_ERROR => false
                case _ => true
            }
        }
    }

    /** diagnostic... generate a JSON string to print to the log for the supplied ModelDef.
      *
      * @param modelDef the model def of interest
      * @return a JSON string representation of the ModelDef almost suitable for printing to log or console.
      */
    def modelDefToString(modelDef : ModelDef) : String = {
        val abbreviatedModelSrc : String = if (modelDef.objectDefinition != null && modelDef.objectDefinition.length > 100) {
            modelDef.objectDefinition.take(99)
        } else {
            if (modelDef.objectDefinition != null) {
                modelDef.objectDefinition
            } else {
                "no source"
            }
        }

        var jsonStr : String  = JsonSerializer.SerializeObjectToJson(modelDef)
        jsonStr = JsonSerializer.replaceLast(jsonStr, "}\n}", "").trim
        jsonStr = jsonStr + ",\n\"InputVariableTypes\": "
        var memberDefJson = JsonSerializer.SerializeObjectListToJson(modelDef.inputVars)
        jsonStr += memberDefJson

        jsonStr = jsonStr + ",\n\"OutputVariableTypes\": "
        memberDefJson = JsonSerializer.SerializeObjectListToJson(modelDef.outputVars)
        memberDefJson = memberDefJson + "}\n}"
        jsonStr += memberDefJson
        jsonStr
    }

}

