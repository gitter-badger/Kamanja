package com.ligadata.jpmml

import java.io.{PushbackInputStream, ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.{List => JList}
import javax.xml.bind.{ValidationEvent, ValidationEventHandler}
import javax.xml.transform.sax.SAXSource

import com.ligadata.KamanjaBase._
import com.ligadata.kamanja.metadata._
import org.dmg.pmml.{PMML, FieldName}
import org.jpmml.evaluator._
import org.jpmml.model.{JAXBUtil, ImportFilter}
import org.xml.sax.InputSource
import org.xml.sax.helpers.XMLReaderFactory

import scala.collection.JavaConverters._
import scala.collection.mutable.{Map => MutableMap}
import scala.collection.concurrent.TrieMap


/**
 * JpmmlAdapter serves a "shim" between the engine and a JPMML evaluator that will perform the actual message
 * scoring.
 */
class JpmmlAdapter( modelContext: ModelContext, factory : ModelBaseObj, modelEvaluator: ModelEvaluator[_]) extends ModelBase(modelContext,factory) {

    /**
     * The engine will call this method to have the model evaluate the input message and produce a ModelResultBase with results.
     * @param outputDefault when true, a model result will always be produced with default values.  If false (ordinary case), output is
     *                      emitted only when the model deems this message worthy of report.  If desired the model may return a 'null'
     *                      for the execute's return value and the engine will not proceed with output processing
     * @return a ModelResultBase derivative or null (if there is nothing to report and outputDefault is false).
     */
    override def execute(outputDefault: Boolean): ModelResultBase = {
        val msg = modelContext.msg
        evaluateModel(msg)
    }

    /**
     * Prepare the active fields, call the model evaluator, and emit results.
     * @param msg the incoming message containing the values of interest to be assigned to the active fields in the
     *            model's data dictionary.
     * @return
     */
    private def evaluateModel(msg : MessageContainerBase): ModelResultBase = {
        val activeFields = modelEvaluator.getActiveFields
        val preparedFields = prepareFields(activeFields, msg, modelEvaluator)
        val evalResultRaw : MutableMap[FieldName, _] = modelEvaluator.evaluate(preparedFields.asJava).asScala
        val evalResults = replaceNull(evalResultRaw)
        val results = EvaluatorUtil.decode(evalResults.asJava).asScala
        new MappedModelResults().withResults(results.toArray)
    }


    /** Since the decode util for the map results shows that at least one key can possibly be null,
      * let's give a name to it for our results.  This is likely just careful coding, but no harm
      * taking precaution.  This is the tag for the null field:
      */
    val DEFAULT_NAME : FieldName = FieldName.create("Anon_Result")

    /**
     * If there is a missing key (an anonymous result), try to manufacture a key for the map so that 
     * all of the result fields returned are decoded and returned.
     * 
     * @param evalResults a mutable map of the results from an evaluator's evaluate function
     * @tparam V the result type
     * @return a map with any null key replaced with a manufactured field name
     */
    private def replaceNull[V](evalResults: MutableMap[FieldName, V]): MutableMap[FieldName, V] = {
         evalResults.get(null.asInstanceOf[FieldName]).fold(evalResults)(v => {
             evalResults - null.asInstanceOf[FieldName] + (DEFAULT_NAME -> v)
        })
    }

    /**
     * Prepare the active fields in the data dictionary of the model with corresponding values from the incoming
     * message.  Note that currently all field names in the model's data dictionary must exactly match the field names
     * from the message.  There is no mapping capability metadata at this point.
     *
     * NOTE: It is possible to have missing inputs in the message.  The model, if written robustly, has accounted
     * for missingValue and other strategies needed to produce results even with imperfect results. See
     * http://dmg.org/pmml/v4-2-1/MiningSchema.html for a discussion about this.
     *
     * @param activeFields a List of the FieldNames
     * @param msg the incoming message instance
     * @param evaluator the JPMML evaluator that the factory as arranged for this instance that can process the
     *                  input values.
     * @return
     */
    private def prepareFields(activeFields: JList[FieldName]
                              , msg: MessageContainerBase
                              , evaluator: ModelEvaluator[_]) : Map[FieldName, FieldValue] = {
        activeFields.asScala.foldLeft(Map.empty[FieldName, FieldValue])((map, activeField) => {
            val key = activeField.getValue
            Option(msg.get(key)).fold(map)(value => {
                val fieldValue : FieldValue = EvaluatorUtil.prepare(evaluator, activeField, value)
                map.updated(activeField, fieldValue)

            })
        })
    }
}

/**
 * JPMMLInfo contains the additional information needed to both recognize the need to instantiate a model for a given message presented to the
 * model factory's IsValidMessage method as well as that information to instantiate an instance of the model that will process that message.
 * This state information is collected and maintained for JPMML models only.
 * @see ModelInfo
 * @see ModelContext
 *
 * @param jpmmlText the pmml source that will be sent to the JPMML evaluator factory to create a suitable evaluator to process the message.
 * @param msgConsumed the incoming message's namespace.name.version that this pmml model will consume.  It is mapped to the input fields in the
 *                    PMML model by the ModelAdapter instance.
 * @param modelName the modelNamespace.name of the model that will process the message.  Unlike most model factories, JPMML's model factory handles
 *                  all JPMML models.  We need to know which of the JPMML ModelDef instances is being used.
 * @param modelVersion the model version.
 */
case class JPMMLInfo(val jpmmlText : String, val msgConsumed : String, val modelName : String, val modelVersion : Long)


object JpmmlAdapter extends ModelBaseObj {

    /** Key for modelMsgMap */
    private case class ModelNameVersion(name: String, version: String)
    /** Key for instanceMap */
    private case class ModelMessageThreadId(modelName: String, modelVer: String, msgName : String, msgVer : String, threadId : String)

    /** modelMsgMap is utilized to construct new instances of JPMML models.
      *
      * FIXME: When there are updates to models, or a new model is added to the metadata and activated, or perhaps
      * FIXME: a model is deactivated. the JpmmlModelFactory here needs to be notified of this change.  Perhaps an
      * FIXME: Event is sent here such that the maps can be amended.
      */
    private var modelMsgMap : TrieMap[ModelNameVersion, ModelDef] = TrieMap.empty[ModelNameVersion, ModelDef]

    /** modelMsgMap is utilized to construct new instances of JPMML models.
      *
      * FIXME: When a thread terminates, it might be useful to eliminate any instances that were allocated for that thread
      */
    private var instanceMap : TrieMap[ModelMessageThreadId, Option[ModelBase]] = TrieMap.empty[ModelMessageThreadId,Option[ModelBase]]

    /**
     * Called at cluster startup, this method collects the metadata for the JPMML model definitions.  These definitions
     * contain the source
     * @param modelDefinitions
     * @return true if initialization was successful
     *
     * FIXME: if this map gets replaced when the engine is active, there needs to be a lock on this function to prevent
     * FIXME: unpredictable things from occurring.
     */
    def initializeModelMsgMap(modelDefinitions : Array[ModelDef]) : Boolean = {
        modelDefinitions.foreach(modelDef => {
            modelMsgMap.put(ModelNameVersion(modelDef.FullName,modelDef.Version.toString), modelDef)
        })

        (modelMsgMap != null && modelMsgMap.size >= 0)
    }

    private var mdMgr : MdMgr = null
    private var factoryName : String = null
    private var factoryVersion : String = null

    /**
     * Set the mdmgr needed to manage the instances and answer IsValidMessage question.
     * ''pre-condition: this function is called before any message processing commences.''
     *
     * @param mdmgr the Kamanja engine's MdMgr
     */
    def FactoryInitialize(mdmgr : MdMgr)  : Unit = {
        mdMgr = mdmgr
        val optModel : Option[ModelDef] = if (mdMgr != null) {
            val currentVersion : Long = -1
            mdMgr.Model("com.ligadata.jpmml.JpmmlAdapter", currentVersion, true)
        } else {
            None
        }
        val factoryModel : ModelDef = optModel.orNull
        if (factoryModel != null) {
            factoryName = factoryModel.FullName
            factoryVersion = MdMgr.ConvertLongVersionToString(factoryModel.Version)
        }
    }

    /** See if any of the models consume the supplied message */
    def IsValidMessage(msg: MessageContainerBase, modelName : String, modelVersion : String): Boolean = {
        val msgFullName : String = msg.FullName
        val msgVersion : String = msg.Version
        //val completeName : String = s"$fullName.$versionStr"
        val completeName : ModelNameVersion = ModelNameVersion(modelName,modelVersion)
        val modelDef : ModelDef = modelMsgMap.getOrElse(completeName, null)
        val yum : Boolean = if (modelDef != null) {
            (msgFullName == modelDef.FullName && msgVersion == MdMgr.ConvertLongVersionToString(modelDef.Version))
        } else {
            false
        }
        yum
    }

    /**
     * Answer a model instance to the caller, obtaining a pre-existing one in the cache if possible.
     * @param mCtx the ModelContext object supplied by the engine with the message and model name to instantiate/fetch
     * @return an instance of the Model that can process the message found in the ModelContext
     */
    override def CreateNewModel(mCtx: ModelContext): ModelBase = {
        /** First determine if we have an instance of this model for this thread */
        val threadId : String = Thread.currentThread.getId.toString
        val msgName : String = mCtx.msg.FullName
        val msgVer : String = mCtx.msg.Version
        val modelName : String = mCtx.modelName
        val modelVer : String = mCtx.modelVersion.toString

        /** Instantiate the instance cache key */
        val modelInstanceKey : ModelMessageThreadId = ModelMessageThreadId(modelName, modelVer, msgName, msgVer, threadId)

        /** if an instance has already been constructed of this model for this thread, use it */
        val optOptModel : Option[Option[ModelBase]] = instanceMap.get(modelInstanceKey)
        val optModel : Option[ModelBase] = optOptModel.getOrElse(None)
        val model : ModelBase = optModel.orNull
        val useThisModel : ModelBase = if (model != null) {
            model
        } else {
            val optModelDef : Option[ModelDef] = modelMsgMap.get(ModelNameVersion(msgVer,msgVer))
            val modelDef : ModelDef = optModelDef.orNull

            if (modelDef != null) {
                val modelName : String = modelDef.FullName
                val version : String = MdMgr.ConvertLongVersionToString(modelDef.Version)

                /** Ingest the pmml here and build an evaluator */
                val modelEvaluator: ModelEvaluator[_] = CreateEvaluator(modelDef.jpmmlText)
                val builtModel : ModelBase = new JpmmlAdapter( mCtx, this, modelEvaluator)
                /** store the instance in the instance map for future reference */
                instanceMap.put(modelInstanceKey, Some(builtModel))

                builtModel
            } else {

                //logger.error(s"model could not be built for modelName=$modelName")  /// FIXME add logger to classes
                null
            }
        }
        useThisModel
    }

    def ModelName(): String = {
        factoryName
    }
    def Version(): String = {
        factoryVersion
    }

    /**
     * Answer a ModelBaseResult from which to give the model results.
     * @return a ModelResultBase derivative appropriate for the model
     */
    def CreateResultObject(): ModelResultBase = new MappedModelResults

    /**
     * Create the appropriate JPMML evaluator based upon the pmml text supplied.
     * 
     * @param pmmlText the PMML (xml) text for a JPMML consumable model
     * @return the appropriate JPMML ModelEvaluator
     */
    private def CreateEvaluator(pmmlText : String) : ModelEvaluator[_] = {

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
        modelEvaluator
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
