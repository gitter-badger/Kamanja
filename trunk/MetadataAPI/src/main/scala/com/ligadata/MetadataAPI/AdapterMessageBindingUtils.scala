package com.ligadata.MetadataAPI

import com.ligadata.Serialize.SerializerManager
import com.ligadata.kamanja.metadata.{SerializeDeserializeConfig, MessageDef, AdapterMessageBinding, MdMgr}

import scala.collection.mutable.Map

object AdapterMessageBindingUtils {

    lazy val serializerType = "kryo"
    lazy val serializer = SerializerManager.GetSerializer(serializerType)


    val AdapterNameKey : String =  "AdapterName"
    val MessageNameKey : String =  "MessageName"
    val MessageNamesKey : String =  "MessageNames"
    val SerializerKey : String =  "Serializer"
    val OptionsKey : String =  "Options"

    val mdMgr : MdMgr = MdMgr.GetMdMgr

    /**
      * Add an AdapterMessageBinding to the metadata for the supplied values.
      * @param adapterName the adapter's name
      * @param messageName the namespace.name of the message
      * @param serializerName the serializer's namespace.name to use
      * @param options serializer options
      * @param userId the user making this request
      * @return a result string as to whether it succeeded or not
      */
    def AddAdapterMessageBinding(adapterName : String
                                 , messageName : String
                                 , serializerName : String
                                 , options : scala.collection.immutable.Map[String,String]
                                 , userId : Option[String]) : String = {
        val (acceptableBinding, errorMsgs) : (Boolean, String) = SemanticChecks(adapterName, messageName, serializerName, options)
        val result : String = if (acceptableBinding) {
            /** Create an AdapterMessageBinding from the supplied map values and catalog it to mdmgr */
            val binding: AdapterMessageBinding = mdMgr.MakeAdapterMessageBinding(adapterName
                                                                                , messageName
                                                                                , serializerName
                                                                                , options)
            mdMgr.AddAdapterMessageBinding(binding)

            /** persist the binding for next session */
            val key: String = s"$adapterName.$messageName.$serializerName"
            val value = serializer.SerializeObjectToByteArray(binding)
            MetadataAPIImpl.SaveObject(key.toLowerCase, value, "adapter_message_bindings", serializerType)

            /** format a json string for return result */
            val res: ApiResult = new ApiResult(0, "AddAdapterMessageBinding", "success!", s"binding for $key was successfully added; ")
            res.toString
        } else {
            val res: ApiResult = new ApiResult(-1, "AddAdapterMessageBinding", "failure!", s"$errorMsgs; ")
            res.toString
        }
        result
    }

    /**
      * Add AdapterMessageBinding(s) to the metadata for the supplied values in the map.  More than one may be added if
      * multiple messages were supplied in a MessageNames array (vs. simple MessageName value),
      *
      * @param bindingMap binding values for adapter name, message name, serializer name, and optionally serializer options.
      * @param userId the user requesting this operation
      * @return json result string.
      */
    def AddAdapterMessageBinding(bindingMap : Map[String,Any], userId : Option[String]) : String = {

        val result : String = if (bindingMap != null) {
            val adapterName: String = bindingMap.getOrElse(AdapterNameKey, "**invalid adapter name**").asInstanceOf[String].trim
            val messageName: String = bindingMap.getOrElse(MessageNameKey, "**invalid message name**").asInstanceOf[String].trim
            val messageNames: List[String] = bindingMap.getOrElse(MessageNamesKey, List[String]()).asInstanceOf[List[String]]
            val serializerName: String = bindingMap.getOrElse(SerializerKey, "**invalid serializer name**").asInstanceOf[String].trim
            val options: scala.collection.immutable.Map[String, String] = if (bindingMap.contains(OptionsKey)) {
                bindingMap(OptionsKey).asInstanceOf[scala.collection.immutable.Map[String, String]]
            } else {
                scala.collection.immutable.Map[String, String]()
            }

            val multipleMessages : Boolean = messageNames.length > 0
            val rslt : String = if (multipleMessages) {
                val results : List[String] = messageNames.map(msg => {
                    AddAdapterMessageBinding(adapterName, msg, serializerName, options, userId)
                })
                results.mkString("; ")
            } else {
                AddAdapterMessageBinding(adapterName, messageName, serializerName, options, userId)
            }
            rslt
        } else {
            new ApiResult(-1, "AddAdapterMessageBinding", "failed!", "json map was null; ").toString
        }
        result
    }

    /**
      * Check out the AdapterMessageBinding values to be used are acceptable.  Checks are:
      *
      * 1) if the names start with "**" it means that the name was not supplied and this is itself an error message
      *    substitution value
      * 2) the message and serializerName names should be checked for namespace.name form
      * 3) the adapter name should exist in MdMgr's adapters map
      * 4) the message should exist in the MdMgr's message map
      * 5) the serializer should exist in the MdMgr's serializers map
      * 6) if the serializer is the csv serializer, verify that none of the attributes presented in the
      * message are ContainerTypeDefs.  These are not handled by csv.  Fixed msgs with simple types only.
      *
      * @param adapterName the name of the adapter
      * @param messageName the namespace.name of the message to be bound
      * @param serializerName the serializer that will be used to flatten/resurrect the message on binder's behalf
      * @param options the serializer options that may be used to configure the serializer
      * @return (boolean, string) If true returned, arguments are acceptable.  If false, the operation will be abandoned
      *         and the returned string contains the issues found.
      */
    private def SemanticChecks(adapterName: String
                               , messageName: String
                               , serializerName: String
                               , options: scala.collection.immutable.Map[String,String]) : (Boolean, String) = {
        val buffer : StringBuilder = new StringBuilder

        /** 1) if the names start with "**" it means that the name was not supplied ... issue name error */
        val adapterStartsWithAstsk : Boolean = (adapterName != null && adapterName.startsWith("**"))
        val messageStartsWithAstsk : Boolean = (messageName != null && messageName.startsWith("**"))
        val serializerStartsWithAstsk : Boolean = (serializerName != null && serializerName.startsWith("**"))
        if (adapterStartsWithAstsk) {
            buffer.append(s"the adapter name was not supplied ... $adapterName...; ")
        }
        if (messageStartsWithAstsk) {
            buffer.append(s"the message name was not supplied ... $messageName...; ")
        }
        if (serializerStartsWithAstsk) {
            buffer.append(s"the serlializer name was not supplied ... $serializerName...; ")
        }

        /** 2) the message and serializerName are checked for namespace.name form */
        val messageNameHasDots : Boolean = (! messageStartsWithAstsk && messageName.size > 2 && messageName.contains('.'))
        val serializerNameHasDots : Boolean = (! serializerStartsWithAstsk && serializerName.size > 2 && serializerName.contains('.'))
        if (! (messageNameHasDots && serializerNameHasDots)) {
            buffer.append(s"$messageName and/or $serializerName do not conform to namespace.name form...; ")
        }
        /** 3) the adapter name should exist in MdMgr's adapters map */
        val adapterPresent : Boolean = mdMgr.GetAdapter(adapterName) != null
        if (! adapterPresent) {
            buffer.append(s"the adapter $adapterName cannot be found in the metadata...; ")
        }
        val msgDef : MessageDef = if (messageNameHasDots) {
            /** 4) the message should exist in the MdMgr's message map */
            val msgNamespace : String = messageName.split('.').dropRight(1).mkString(".")
            val msgName : String = messageName.split('.').last
            val msgD : MessageDef = mdMgr.ActiveMessage(msgNamespace, msgName)
            val msgPresent: Boolean = msgD != null
            if (! msgPresent) {
                buffer.append(s"the message $messageName cannot be found in the metadata...; ")
            }
            msgD
        } else {
            null
        }
        val serializer : SerializeDeserializeConfig = if (serializerNameHasDots) {
            /** 5) he serializer should exist in the MdMgr's serializers map */
            val slzer : SerializeDeserializeConfig = mdMgr.GetSerializer(serializerName)
            val serializerPresent: Boolean = slzer != null
            if (! serializerPresent) {
                buffer.append(s"the message $messageName cannot be found in the metadata...; ")
            }
            slzer
        } else {
            null
        }
        /** 6) csv check ... if csv verify msg is fixed with simple (non-ContainerTypeDef types).  The nested
          * types (ContainerTypeDef fields) are only supported by Json and KBinary at this time.
          */
        // Fixme: implement check 6)
        // Fixme: implement check 6)
        // Fixme: implement check 6)

        val ok = buffer.isEmpty
        val errorMsgs : String = buffer.toString
        (ok,errorMsgs)
    }

    /**
      * Add multiple AdapterMessageBindings in the supplied map.
      *
      * @param bindingMaps a list of AdapaterMessageBindings specs as a list of json maps.
      * @param userId the user requesting this operation
      * @return result string for all of the AdapterMessageBindings added.
      */
    def AddAdapterMessageBinding(bindingMaps : List[Map[String,Any]], userId : Option[String]) : String = {

        var resultBuffer : StringBuilder = new StringBuilder
        bindingMaps.foreach(bindingMap => {
            val result : String = if (bindingMap != null) {
                AddAdapterMessageBinding(bindingMap, userId)
            } else {
                new ApiResult(-1, "AddAdapterMessageBinding", "failed!", "json map was null.").toString
            }
            resultBuffer.append(result)
        })
        val results : String = resultBuffer.toString
        results
    }

}