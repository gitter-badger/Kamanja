package com.ligadata.jpmml

import java.io.{PushbackInputStream, ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.{List => JList}
import javax.xml.bind.{ValidationEvent, ValidationEventHandler}
import javax.xml.transform.sax.SAXSource

import com.ligadata.KamanjaBase._
import com.ligadata.kamanja.metadata._
import org.apache.log4j.Logger
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
 * The JpmmlAdapter factory object manages all JPMML models that are active in the system.  It expects the message
 * namespace.name and version to be properly filled in by the Kamanja engine when its IsValidMessage is called.
 *
 * Should the JpmmlAdapter be changed out during message processing (a notification has been received by the engine
 * from a MetadataAPI service serving the cluster), it is incumbent upon the engine to quiesce the JPMML models that
 * are in RUN state before removing and replacing the JpmmlAdapter.
 *
 * ''FIXME: Afaik, there is no mechanism for tracking model execution by MetadataAPI.ModelType.  For factories that
 * don't manage multiple models, this is not an issue.  It will possibly be an issue only for the factories that
 * are managing multiple models (like JpmmlAdapter).''
 *
 * When a replacement is made like this during message processing the FactoryInitialize(mdmgr : MdMgr, mutexLockNeeded : Boolean)
 * method should be called with the flag set to true.  Initialization at cluster startup before the messages are flying, this flag
 * should be false.
 */
object JpmmlAdapter extends ModelBaseObj {

    val logger = Logger.getLogger(getClass)

    /** Key for modelMsgMap (name is namespace.name, version is the canonical form returned by MdMgr.ConvertLongVersionToString */
    private case class ModelNameVersion(name: String, version: String)
    /** Key for instanceMap (both names are namespace.name, both versions are the canonical form returned by
      * MdMgr.ConvertLongVersionToString and threadId is the Thread.currentThread.getId.toString */
    private case class ModelMessageThreadId(modelName: String, modelVer: String, msgName : String, msgVer : String, threadId : String)

    /** modelMsgMap is utilized to construct new instances of JPMML models.
      *
      * When there are updates to models, or a new model is added to the metadata and activated, or perhaps
      * a model is deactivated. the JpmmlModelFactory here needs to be notified of this change.  To do this, call
      * method JpmmlModelsHaveChanged.
      * @see JpmmlModelsHaveChanged
      *
      */
    private val modelMsgMap : TrieMap[ModelNameVersion, ModelDef] = TrieMap.empty[ModelNameVersion, ModelDef]

    /** The instanceMap serves as a cache of JPMML model instances. JPMML models are relatively expensive to build, but since
      * they are idempotent, a cache of instances is maintained to service subsequent calls. If the engine is configured to
      * execute models on multiple threads, there will be one JPMML model instance for each thread for any given JPMML model.
      * @see ModelMessageThreadId
      */
    private val instanceMap : TrieMap[ModelMessageThreadId, Option[ModelBase]] = TrieMap.empty[ModelMessageThreadId,Option[ModelBase]]

    /** The variable mutex serves as the synchronization variable for updating the modelMsgMap */
    private val mutex : Integer = 0
    /**
     * Called at cluster startup, this method collects the metadata for the JPMML model definitions.  These definitions
     * contain the source
     * @param modelDefinitions - an Array[ModelDef] containing only JPMML models
     * @param lockNeeded - indicate if synchronized is required.  It is only required when the engine is processing
     *                   messages.  At cluster startup, it is not needed.
     * @return true if initialization was successful
     */
    private def InitializeModelMsgMap(modelDefinitions : Array[ModelDef], lockNeeded : Boolean) : Boolean = {

        if (lockNeeded) {
            mutex.synchronized {
                modelDefinitions.foreach(modelDef => {
                    modelMsgMap.put(ModelNameVersion(modelDef.FullName, MdMgr.ConvertLongVersionToString(modelDef.Version)), modelDef)
                })
            }
        } else {
            modelDefinitions.foreach(modelDef => {
                modelMsgMap.put(ModelNameVersion(modelDef.FullName, MdMgr.ConvertLongVersionToString(modelDef.Version)), modelDef)
            })
        }

        (modelMsgMap != null && modelMsgMap.size >= 0) /** it's ok to have no models though unlikely */
    }

    /**
     * Called when the engine has been notified that the JPMML models (possibly) have changed by the MetadataAPI,
     * this function causes the models to be refetched
     */
    def JpmmlModelsHaveChanged : Unit = {
        val mutexLockNeeded : Boolean = true
        val onlyActive : Boolean = true
        val latestVersionsOnly : Boolean = false
        val optModelDefs: Option[scala.collection.immutable.Set[ModelDef]] = mdMgr.Models(onlyActive, latestVersionsOnly)
        val modelDefs : scala.collection.immutable.Set[ModelDef] = optModelDefs.orNull
        val initialized : Boolean = if (modelDefs != null) {
            val jpmmlModelDefs: Array[ModelDef] = modelDefs.filter(_ == ModelRepresentation.JPMML).toArray
            InitializeModelMsgMap(jpmmlModelDefs, mutexLockNeeded)
        } else {
            InitializeModelMsgMap(Array[ModelDef](), mutexLockNeeded)
        }
        if (! initialized) {
            logger.error(s"Factory Initialization result = $initialized ... unable to load the model defs from the metadata manager")
        } else {
            logger.debug(s"Factory Initialization result = $initialized ... load successful")
        }

    }

    /** JpmmlAdapter has access to the engine metadata through this reference */
    private var mdMgr : MdMgr = null
    /** JpmmlAdapter factory namespace.name */
    private var factoryName : String = null
    /** JpmmlAdapter factory's version */
    private var factoryVersion : String = null

    /**
     * Called at cluster startup, set the mdmgr needed to manage the instances and answer IsValidMessage question.
     *
     * @param mdmgr - the Kamanja engine's MdMgr
     * @param mutexLockNeeded - this value should be 'true' if the messages are being processed.  The 'false' value
     *                        is acceptable during cluster initialization before message processing.
     */
    def FactoryInitialize(mdmgr : MdMgr, mutexLockNeeded : Boolean) : Boolean = {
        mdMgr = mdmgr
        val optModel : Option[ModelDef] = if (mdMgr != null) {
            val currentVersion : Long = -1
            val active : Boolean = true
            mdMgr.Model("com.ligadata.jpmml.JpmmlAdapter", currentVersion, active)
        } else {
            None
        }
        val factoryModel : ModelDef = optModel.orNull
        val factoryInitialized : Boolean = if (factoryModel != null) {
            factoryName = factoryModel.FullName
            factoryVersion = MdMgr.ConvertLongVersionToString(factoryModel.Version)

            /** Gather the Jpmml models and put them in a Map so it can be discerned which of the models is being
              * asked it it is able to service the current supplied message. */

            val onlyActive : Boolean = true
            val latestVersionsOnly : Boolean = false
            val optModelDefs: Option[scala.collection.immutable.Set[ModelDef]] = mdMgr.Models(onlyActive, latestVersionsOnly)
            val modelDefs : scala.collection.immutable.Set[ModelDef] = optModelDefs.orNull
            val initialized : Boolean = if (modelDefs != null) {
                val jpmmlModelDefs: Array[ModelDef] = modelDefs.filter(_ == ModelRepresentation.JPMML).toArray
                InitializeModelMsgMap(jpmmlModelDefs, mutexLockNeeded)
            } else {
                InitializeModelMsgMap(Array[ModelDef](), mutexLockNeeded)
            }
            initialized
        } else {
            logger.error(s"Factory Initialization failed.. there is no model named ${'"'}com.ligadata.jpmml.JpmmlAdapter${'"'}")
            false
        }
        factoryInitialized
    }

    /**
     * Determine if the supplied message can be consumed by the model mentioned in the argument list.  The engine will
     * call this method when a new messages has arrived and been prepared.  It is passed to each of the active models
     * in the working set.  Each model has the opportunity to indicate its interest in the message.
     *
     * NOTE: For many model factories that implement this interface, there is only one model to be concerned with and the
     * namespace.name.version can be ignored. Some factories, however, are responsible for servicing many models, so
     * the Kamanja engine's intentions are made known explicitly as to which active model it is currently concerned.
     *
     * @param msg  - the message instance that is currently being processed
     * @param modelName - the namespace.name of the model that the engine wishes to know if it can process this message
     * @param modelVersion - the canonical version of the model (string form) that the engine wishes to know if it can
     *                     process this message
     * @return true if this model can process the message.
     */
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
     * Answer a model instance, obtaining a pre-existing one in the cache if possible.
     * @param mCtx - the ModelContext object supplied by the engine with the message and model name to instantiate/fetch
     * @return - an instance of the Model that can process the message found in the ModelContext
     */
    def CreateNewModel(mCtx: ModelContext): ModelBase = {
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
                /** Ingest the pmml here and build an evaluator */
                val modelEvaluator: ModelEvaluator[_] = CreateEvaluator(modelDef.jpmmlText)
                val builtModel : ModelBase = new JpmmlAdapter( mCtx, this, modelEvaluator)
                /** store the instance in the instance map for future reference */
                instanceMap.put(modelInstanceKey, Some(builtModel))

                builtModel
            } else {
                logger.error(s"ModelDef not found...instance could not be built for modelName=$modelName modelVersion=$modelVer, msgName=$msgName, msgVersion=$msgVer")
                null
            }
        }
        useThisModel
    }

    /**
     * Answer the model name.
     * @return the model name
     */
    def ModelName(): String = {
        factoryName
    }

    /**
     * Answer the model version.
     * @return the model version
     */
    def Version(): String = {
        factoryVersion
    }

    /**
     * Answer a ModelResultBase from which to give the model results.
     * @return - a ModelResultBase derivative appropriate for the model
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
