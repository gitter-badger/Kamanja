package com.ligadata.MetadataAPI.Utility

import com.ligadata.Exceptions.{InvalidArgumentException, EngineConfigParsingException, Json4sParsingException}
import com.ligadata.MetadataAPI.AdapterMessageBindingUtils
import org.apache.logging.log4j.LogManager
import org.json4s.jackson.JsonMethods._
import org.json4s.{MappingException, DefaultFormats, Formats}

import scala.collection.mutable.Map
import scala.io.Source

object AdapterMessageBindingService {
    val loggerName = this.getClass.getName
    lazy val logger = LogManager.getLogger(loggerName)

    /**
      * Add an adapter message binding to the metadata
      *
      * @param input the binding specification, either in the form of a JSON string specifying the binding(s)
      *              or
      *              a path specification of a file that contains the binding(s).
      * @param userId the user that is performing the add (currently optional)
      * @return a JSON message result
      */
    def addAdapterMessageBinding(input: String, userId : Option[String]) : String = {

        val userIdentifier : String = userId.getOrElse(null)
        if (userIdentifier != null) {
            /** FIXME: discern (when implemented) if this user is authorized to execute this command */
        } else {
            /** FIXME: complain that there is no user id when that day comes that the user must be specified */
        }

        val inputTrimmed : String = if (input != null) input.trim else null
        if (input == null) {
            throw InvalidArgumentException("attempting to add adapter message binding with bogus input text", null)
        }
        val isInlineSpec : Boolean = (inputTrimmed.startsWith("{") || inputTrimmed.startsWith("["))
        val result : String = if (isInlineSpec) {
            addAdapterMessageBindingFromJson(inputTrimmed, userId)
        } else {
            val jsonText: String = Source.fromFile(input).mkString
            addAdapterMessageBindingFromJson(jsonText, userId)
        }
        result
    }

    /**
      * Process the supplied json string.  One (starts with '{') or more (starts with '[') adapters may be present
      *
      * @param input a Json string ... either a  { k:v, k:v,...} or [ { k:v, k:v,...}, { k:v, k:v,...}, ...]
      * @param userId the user requesting this operation
      * @return
      */
    @throws(classOf[com.ligadata.Exceptions.InvalidArgumentException])
    private def addAdapterMessageBindingFromJson(input: String, userId : Option[String]) : String = {

        val trimmedInput : String = input.trim
        val isMap : Boolean = trimmedInput.startsWith("{")
        val isList : Boolean = trimmedInput.startsWith("[")

        val reasonable : Boolean = isMap || isList
        if (! reasonable) {
            throw InvalidArgumentException("the adapter string specified must be either a json map or json array.", null)
        }
        val result : String = if (isMap) {
            val bindingSpec : scala.collection.immutable.Map[String,Any] = jsonStringAsColl(input).asInstanceOf[scala.collection.immutable.Map[String,Any]]
            val mutableBindingSpec : Map[String,Any] = Map[String,Any]()
            bindingSpec.foreach(pair => mutableBindingSpec(pair._1) = pair._2)
            AdapterMessageBindingUtils.AddAdapterMessageBinding(mutableBindingSpec, userId)
        } else {
            val bindingSpecList : List[scala.collection.immutable.Map[String,Any]] = jsonStringAsColl(input).asInstanceOf[List[scala.collection.immutable.Map[String,Any]]]
            val mutableMapsList : List[Map[String,Any]] = bindingSpecList.map(aMap => {
                val mutableBindingSpec : Map[String,Any] = Map[String,Any]()
                aMap.foreach(pair => mutableBindingSpec(pair._1) = pair._2)
                mutableBindingSpec
            })
            AdapterMessageBindingUtils.AddAdapterMessageBinding(mutableMapsList, userId)
        }
        result
    }

    /**
      * Translate the supplied json string to a List[Map[String, Any]]
      *
      * @param configJson
      * @return Map[String, Any]
      */

    @throws(classOf[com.ligadata.Exceptions.Json4sParsingException])
    @throws(classOf[com.ligadata.Exceptions.InvalidArgumentException])
    def jsonStringAsColl(configJson: String): Any = {
        val jsonObjs : Any = try {
            implicit val jsonFormats: Formats = DefaultFormats
            val json = parse(configJson)
            logger.debug("Parsed the json : " + configJson)
            json.values
        } catch {
            case e: MappingException => {
                logger.debug("", e)
                throw Json4sParsingException(e.getMessage, e)
            }
            case e: Exception => {
                logger.debug("", e)
                throw InvalidArgumentException(e.getMessage, e)
            }
        }
        jsonObjs
    }

    /**
      * Update an existing adapter message binding to the metadata.
      *
      * @param input the binding specification, either in the form of a JSON string specifying the binding
      *              or
      *              a path specification of a file that contains the binding(s).
      * @param userId the user that is performing the update (currently optional)
      * @return a JSON message result
      */
    def updateAdapterMessageBinding(input: String, userId : Option[String]) : String  = {

        ""
    }

    /**
      * Remove an existing adapter message binding from the metadata.
      *
      * @param input the name of the binding to remove.  This name is a namespace qualified name.
      * @param userId the user that is performing the remove (currently optional)
      * @return a JSON message result
      */
    def removeAdapterMessageBinding(input: String, userId : Option[String]) : String  = {

        ""
    }

}
