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

object SampleData {

  val sampleScalarTypeStr = """
  {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "my_char",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "Char",
  "PhysicalName" : "Char",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.CharImpl"
  }
"""

  val sampleNewScalarTypeStr = """
  {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "my_char",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "Char",
  "PhysicalName" : "Char",
  "Version" : 101,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.CharImpl"
  }
"""

  val sampleTupleTypeStr = """
  {
    "MetadataType" : "TupleTypeDef",
    "NameSpace" : "system",
    "Name" : "my_tupleofstringstring",
    "TypeTypeName" : "tTupleN",
    "TypeNameSpace" : "System",
    "TypeName" : "Any",
    "PhysicalName" : null,
    "Version" : 1,
    "JarName" : null,
    "DependencyJars" : [ "basetypes_2.11-0.1.0.jar", "metadata_2.11-1.0.jar" ],
    "Implementation" : null,
    "TupleDefinitions": [ 
      {
	"MetadataType" : "ScalarTypeDef",
	"NameSpace" : "system",
	"Name" : "string",
	"TypeTypeName" : "tScalar",
	"TypeNameSpace" : "System",
	"TypeName" : "String",
	"PhysicalName" : "String",
	"Version" : 100,
	"JarName" : "basetypes_2.11-0.1.0.jar",
	"DependencyJars" : [ "metadata_2.11-1.0.jar" ],
	"Implementation" : "com.ligadata.BaseTypes.StringImpl"
      },
      {
	"MetadataType" : "ScalarTypeDef",
	"NameSpace" : "system",
	"Name" : "string",
	"TypeTypeName" : "tScalar",
	"TypeNameSpace" : "System",
	"TypeName" : "String",
	"PhysicalName" : "String",
	"Version" : 100,
	"JarName" : "basetypes_2.11-0.1.0.jar",
	"DependencyJars" : [ "metadata_2.11-1.0.jar" ],
	"Implementation" : "com.ligadata.BaseTypes.StringImpl"
      } ]
  }
"""

  val sampleConceptStr = """
  {"Concepts" : [
  {"NameSpace":"Ligadata",
  "Name":"ProviderId",
  "TypeNameSpace":"System",
  "TypeName" : "String",
  "Version"  : "100"} ]
  }
"""

  val sampleFunctionStr = """
  {
  "NameSpace" : "pmml",
  "Name" : "my_min",
  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
  "ReturnTypeNameSpace" : "system",
  "ReturnTypeName" : "double",
  "Arguments" : [ {
  "ArgName" : "expr1",
  "ArgTypeNameSpace" : "system",
  "ArgTypeName" : "int"
  }, {
  "ArgName" : "expr2",
  "ArgTypeNameSpace" : "system",
  "ArgTypeName" : "double"
  } ],
  "Version" : 1,
  "JarName" : null,
  "DependantJars" : [ "basetypes_2.11-0.1.0.jar", "metadata_2.11-1.0.jar" ]
  }
"""

  val sampleDerivedConceptStr = """
  {
  "FunctionDefinition" : {
  "NameSpace" : "pmml",
  "Name" : "my_min",
  "PhysicalName" : "com.ligadata.pmml.udfs.Udfs.Min",
  "ReturnTypeNameSpace" : "system",
  "ReturnTypeName" : "double",
  "Arguments" : [ {
  "ArgName" : "expr1",
  "ArgTypeNameSpace" : "system",
  "ArgTypeName" : "int"
  }, {
  "ArgName" : "expr2",
  "ArgTypeNameSpace" : "system",
  "ArgTypeName" : "double"
  } ],
  "Version" : 1,
  "JarName" : null,
  "DependantJars" : [ "basetypes_2.11-0.1.0.jar", "metadata_2.11-1.0.jar" ]
  },
  "Attributes": [ 
{
  "NameSpace" : "desynpuf_id",
  "Name" : "desynpuf_id",
  "Version" : 1,
  "CollectionType" : "None",
  "Type": {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "string",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "string",
  "PhysicalName" : "String",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.StringImpl"
}},
{
  "NameSpace" : "medicationidentifier",
  "Name" : "medicationidentifier",
  "Version" : 1,
  "CollectionType" : "None",
  "Type": {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "string",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "string",
  "PhysicalName" : "String",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.StringImpl"
}},
{
  "NameSpace" : "medicationdetails_name",
  "Name" : "medicationdetails_name",
  "Version" : 1,
  "CollectionType" : "None",
  "Type": {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "string",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "string",
  "PhysicalName" : "String",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.StringImpl"
}},
{
  "NameSpace" : "encounterid",
  "Name" : "encounterid",
  "Version" : 1,
  "CollectionType" : "None",
  "Type": {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "long",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "long",
  "PhysicalName" : "Long",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.LongImpl"
}},
{
  "NameSpace" : "administration_dosage_quantity",
  "Name" : "administration_dosage_quantity",
  "Version" : 1,
  "CollectionType" : "None",
  "Type": {
  "MetadataType" : "ScalarTypeDef",
  "NameSpace" : "system",
  "Name" : "double",
  "TypeTypeName" : "tScalar",
  "TypeNameSpace" : "System",
  "TypeName" : "double",
  "PhysicalName" : "Double",
  "Version" : 100,
  "JarName" : "basetypes_2.11-0.1.0.jar",
  "DependencyJars" : [ "metadata_2.11-1.0.jar" ],
  "Implementation" : "com.ligadata.BaseTypes.DoubleImpl"
}} ]
  }
"""

}


