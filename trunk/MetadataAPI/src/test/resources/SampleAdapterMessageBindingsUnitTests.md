****************************
**Adapter Message Bindings**

Adapter message bindings are independently cataloged metadata objects that associate three other metadata objects together:

- An adapter (input, output, or storage)
- A message that this adapter either deserializes from its source or serializes to send to its sink.
- The serializer to use that understands how to serialize and deserialize the message.

There are currently three builtin serializers provided in the Kamanja distribution that can be used: a JSON serializer, a CSV serializer and a KBinary serializer.  The KBinary is used principally by the Kamanja platform to manage data in its stores and caches.

The adapter message bindings can be ingested one at a time (see the [input adapter example below]).  If there are multiple messages that are managed by an adapter that all share the same serializer, a compact representation is possible (see the [output adapter example below]).  It is also possible to organize one or more adapter message binding specifications in a file and have kamanja consume that.

************************************
**Adapter Message Binding Examples**

**_NOTE_: All of the adapters are defined in trunk/MetadataAPI/src/test/resources/Metadata/config/ClusterConfig.1.4.0.json.  The com.botanical.* messages mentioned are defined in trunk/MetadataAPI/src/test/resources/Metadata/message/. See the _Test Setup_ below.**

What follows is a number of annotated examples that illustrate the bindings and how one might use them to configure a cluster.  The local kamanja command is used here.  See [The wiki page for the MetadataAPI service] to see the service rendition of commands like these.


**Input Adapter Message Binding**

{
  "AdapterName": "kafkaAdapterInput1",
  "MessageName": "com.botanical.json.ordermsg",
  "Serializer": " org.kamanja.serializer.json.JsonSerDeser"
}

	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMSTRING '{"AdapterName": "kafkaAdapterInput1", "MessageName": "com.botanical.json.ordermsg", "Serializer": " org.kamanja.serializer.json.JsonSerDeser"}'

_The keys in the JSON specification are case sensitive.  For example, use "AdapterName", not "ADAPTERname" or "adaptername"._

Note that the adapter message binding sent to the kamanja script is just a flattened version of the json map structure presented above wrapped with the <json>..</json> tokens.  These tokens allow the kamanja command processor to recognize the beginning and end of the json specified configuration for the adapter message binding.  They are _significant_. The _space_ following start <json> and preceding the closing </json> tokens are currently _required_.  

The serializer used in this case is the name of the builtin CSV deserializer.  Note too that it has an options map that is used to configure it.  See the [description of CSV serializer] for what the options mean.


**Output Adapter Message Binding**
{
  "AdapterName": "kafkaAdapterOutput2",
  "MessageNames": ["com.botanical.csv.emailmsg"],
  "Serializer": "org.kamanja.serializer.csv.CsvSerDeser",
  "Options": {
    "lineDelimiter": "\r\n",
    "fieldDelimiter": ",",
    "produceHeader": true,
    "alwaysQuoteFields": false
  }
}

	$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMSTRING '{"AdapterName": "kafkaAdapterOutput2", "MessageNames": ["com.botanical.csv.emailmsg"], "Serializer": "org.kamanja.serializer.csv.CsvSerDeser", "Options": {"lineDelimiter": "\r\n", "fieldDelimiter": ",", "produceHeader": true, "alwaysQuoteFields": false } }'

This output adapter also uses the CSV builtin adapter, but notice too that there are two messages mentioned in the MessageNames key value.  What actually happens is that binding metadata is built for each adapter/message/serializer combination.  Think of this as a short hand.

If the key for the message is "MessageName", then only message name is given.  If "MessageNames" (plural), an array of names is expected.

**Storage Adapter Message Binding**
{
  "AdapterName": "hBaseStore1",
  "MessageNames": ["com.botanical.json.audit.ordermsg", "com.botanical.json.audit.shippingmsg"],
  "Serializer": "org.kamanja.serializer.json.JsonSerDeser"
}

	$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMSTRING '{"AdapterName": "hBaseStore1", "MessageNames": ["com.botanical.json.audit.ordermsg", "com.botanical.json.audit.shippingmsg"], "Serializer": "org.kamanja.serializer.json.JsonSerDeser"}'

In this storage adapter binding, the use of the builtin JSON adapter is illustrated.  Again there are multiple messages specified.  The JSON serializer doesn't currently have any options, so it can be omitted.

**Storage Adapter Message Binding Specifications in a File**

A file can have multiple binding specifications in it.  For example,

[
	{
	  "AdapterName": "kafkaAdapterInput1",
	  "MessageNames": ["com.botanical.json.ordermsg", "com.botanical.json.shippingmsg"],
	  "Serializer": "org.kamanja.serializer.json.JsonSerDeser"
	},
	{
	  "AdapterName": "kafkaAdapterOutput2",
	  "MessageNames": ["com.botanical.csv.emailmsg"],
	  "Serializer": "org.kamanja.serializer.csv.CsvSerDeser",
	  "Options": {
		"lineDelimiter": "\r\n",
		"fieldDelimiter": ",",
		"produceHeader": "true",
		"alwaysQuoteFields": "false"
	  }
	},
	{
	  "AdapterName": "hBaseStore1",
	  "MessageNames": ["com.botanical.json.audit.ordermsg", "com.botanical.json.audit.shippingmsg"],
	  "Serializer": "org.kamanja.serializer.json.JsonSerDeser"
	}
]


Using a file to ingest all (or at least the key adapter bindings) for a new cluster has its attraction.  A binding is prepared for each map and, as can be seen in the kafkaAdapterOutput2 and hBaseStore1 adapters, the multiple message shorthand is used that will cause a binding for each unique triple (adapter, message, serializer).

If the above file was called $MetadataDir/config/AdapterMessageBindingsForClusterConfig1.4.0.json, a Kamanja command can directly ingest it:

	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMFILE $MetadataDir/config/AdapterMessageBindingsForClusterConfig1.4.0.json

	$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMFILE $MetadataDir/config/AdapterMessageBindingsForClusterConfig1.4.0.json

Like the other examples, you can also push this directly on the command line too (it is useful to have an editor that can pretty print/flatten the JSON text to do these more complicated structures for the command line):

	$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add adaptermessagebinding BINDINGFROMSTRING '[{"AdapterName": "kafkaAdapterInput1", "MessageNames": ["com.botanical.json.ordermsg", "com.botanical.json.shippingmsg"], "Serializer": "org.kamanja.serializer.json.JsonSerDeser"}, {"AdapterName": "kafkaAdapterOutput2", "MessageNames": ["com.botanical.csv.emailmsg"], "Serializer": "org.kamanja.serializer.csv.CsvSerDeser", "Options": {"lineDelimiter": "\r\n", "fieldDelimiter": ",", "produceHeader": "true", "alwaysQuoteFields": "false"} }, {"AdapterName": "hBaseStore1", "MessageNames": ["com.botanical.json.audit.ordermsg", "com.botanical.json.audit.shippingmsg"], "Serializer": "org.kamanja.serializer.json.JsonSerDeser"} ]'



**List Adapter Message Bindings**

_List them all_
	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties list adaptermessagebindings 
_List bindings for the supplied adapter_
	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties list adaptermessagebindings ADAPTERFILTER hBaseStore1
_List bindings for a given message_
	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties list adaptermessagebindings MESSAGEFILTER com.botanical.csv.emailmsg
_List bindings for given serializer_
	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties list adaptermessagebindings SERIALIZERFILTER org.kamanja.serializer.json.JsonSerDeser

**List Adapter Message Bindings**

_Remove the supplied binding key_
	$KAMANJA_HOME/bin/kamanja debug $KAMANJA_HOME/config/MetadataAPIConfig.properties remove adaptermessagebindings hBaseStore1.com.botanical.json.audit.ordermsg.org.kamanja.serializer.json.JsonSerDeser

**Test Setup**

To run the examples above (and expect them to succeed), the cluster configuration with the adapters defined in it as well as the com.botanical* messages need to be defined.  

_Define the path locations (adjust these)_

export KAMANJA_HOME=/tmp/drdigital/Kamanja-1.4.0_2.11
export KAMANJA_SRCDIR=~/github/dev/1.4.0.Base/kamanja/trunk
export MetadataDir=$KAMANJA_SRCDIR/MetadataAPI/src/test/resources/Metadata

_Install the parameterized version of SetPaths.sh called setPathsFor.scala (optional)_

This step uses a script called setPathsFor.scala that will do the same thing that the SampleApplication/EasyInstall/SetPaths.sh script does for some of the fixed examples we have in the distribution.  This does it for arbitrary files... one at a time.  The script is somewhat useful for setting up testing configurations.  Install it if you like on your PATH.  It can be found at trunk/Utils/Script/setPathsFor.scala 

Alternatively the template may be edited by hand if you're really old school.

_Configure the cluster configuration json file_ (make location substitutions)

setPathsFor.scala --installDir $KAMANJA_HOME --templateFile $MetadataDir/config/ClusterConfig.1.4.0.json --scalaHome `which scala` --javaHome `which java` > $KAMANJA_HOME/config/adapterMessageBindingClusterConfig.json

_Define the cluster info_

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties upload cluster config $KAMANJA_HOME/config/adapterMessageBindingClusterConfig.json

_Define the messages_

$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $MetadataDir/message/com.botanical.csv.emailmsg.json tenantid "botanical"
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $MetadataDir/message/com.botanical.json.audit.ordermsg.json tenantid "botanical"
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $MetadataDir/message/com.botanical.json.audit.shippingmsg.json tenantid "botanical"
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $MetadataDir/message/com.botanical.json.ordermsg.json tenantid "botanical"
$KAMANJA_HOME/bin/kamanja $KAMANJA_HOME/config/MetadataAPIConfig.properties add message $MetadataDir/message/com.botanical.json.shippingmsg.json tenantid "botanical"

Once complete return to the **Adapter Message Binding Examples** section and try out the adapter messasge binding add / remove commands.

