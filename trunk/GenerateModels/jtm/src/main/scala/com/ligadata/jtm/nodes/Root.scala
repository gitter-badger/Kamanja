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
package com.ligadata.jtm.nodes

import java.io.{File, StringReader}
import java.lang.reflect.Type

import com.google.gson.{Gson, GsonBuilder, JsonDeserializer, JsonElement, JsonSerializer, _}
import com.google.gson.reflect.TypeToken
import com.google.gson.stream.JsonReader
import org.apache.commons.io.FileUtils

import scala.collection.JavaConversions._
import scala.reflect.{ClassTag, _}

/** Object to parse the jtm "language"
  *
  */
object Root {

  /** Class to parse string to string Map[String, String]
    * like "map": { "name1" : "value1", "name2" : "value2", "name3" : "value3"},
    */
  class MapToString extends JsonDeserializer[scala.collection.Map[String, String]] with JsonSerializer[scala.collection.Map[String, String]] {

    def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): scala.collection.Map[String, String] = {
      var collectMap = scala.collection.Map.empty[String, String]
      val entrySet = json.getAsJsonObject().entrySet()
      entrySet.map( entry =>  collectMap += ( entry.getKey() -> entry.getValue.getAsString()) )
      collectMap
    }

    def serialize(src: scala.collection.Map[String, String], typeOfT: Type, context: JsonSerializationContext) :  JsonObject = {
      val json = new JsonObject
      src.foreach( p => json.addProperty(p._1, p._2) )
      json
    }
  }

  /** Class to parse string to string Map[String, T] where T is is an class
    *
    * @tparam T type of the value of the map
    */
  class MapToType[T:ClassTag] extends JsonDeserializer[scala.collection.Map[String, T]] with JsonSerializer[scala.collection.Map[String, T]] {

    def deserialize(json: JsonElement, typeOfT1: Type, context: JsonDeserializationContext): scala.collection.Map[String, T] = {
      var map = scala.collection.Map.empty[String, T]
      val p = json.getAsJsonObject()
      p.entrySet().map( e => {
        val o = e.getValue
        val t = classTag[T].runtimeClass
        val v: T = context.deserialize[T](o, t)
        map ++= Map( e.getKey -> v)
      })
      map
    }

    def serialize(src: scala.collection.Map[String, T], typeOfT: Type, context: JsonSerializationContext):  JsonObject = {
      val json = new JsonObject
      src.foreach( p => {
        val jo = context.serialize(p._2).getAsJsonObject()
        json.add(p._1, jo)
      })
      json
    }
  }

  /** Setup the gson object to be used
    *
    * @return a configured Gson object
    */
  def buildGson(): Gson = {
    val mapToString = new TypeToken[scala.collection.Map[String, String]](){}.getType()
    val mapToTransformation = new TypeToken[scala.collection.Map[String, Transformation]](){}.getType()
    val mapToOutput = new TypeToken[scala.collection.Map[String, Output]](){}.getType()
    val mapToCompute = new TypeToken[scala.collection.Map[String, Compute]](){}.getType()
    val mapToGrok = new TypeToken[scala.collection.Map[String, Grok]](){}.getType()

    new GsonBuilder().
      registerTypeAdapter(mapToString, new MapToString).
      registerTypeAdapter(mapToTransformation, new MapToType[Transformation]).
      registerTypeAdapter(mapToOutput, new MapToType[Output]).
      registerTypeAdapter(mapToCompute, new MapToType[Compute]).
      registerTypeAdapter(mapToGrok, new MapToType[Grok]).
      create()
  }

  val gson = buildGson()

  /** Create root object from json string
    *
    * @param config json string to parse
    * @return root of the processing instructions
    */
  def fromJsonString(config : String) : Root = {
    val reader = new JsonReader(new StringReader(config))
    reader.setLenient(true)
    gson.fromJson(reader, classOf[Root])
  }

  /** Parse a provided file
    *
    * @param file file name to be read and parsed
    * @return root of the processing instructions
    */
  def fromJson(file : String) : Root = {
    val config = FileUtils.readFileToString(new File(file), null:String)
    fromJsonString(config)
  }

  /** Take a Root object and output to a json file
    *
    * @param file to output too
    * @param c Root node
    */
  def toJson(file: String, c: Root) = {
    val dataJson = gson.toJson(c)
    scala.tools.nsc.io.File(file).writeAll(dataJson)
  }

  /** Take a Root object and output to a json string
    *
    * @param c Root node
    * @return string with the json program
    */
  def toJson(c: Root) : String = {
    gson.toJson(c)
  }
}

/**
  * Created by joerg on 1/20/16.
  */
class Root {

  /** Header information of jtm
    *
    */
  val header: Header = null

  /** List of dependencies, should be empty
    *
    */
  val dependencies: Array[String] = Array.empty[String]

  /** List of imports to be added
    *
    */
  val imports: Imports = new Imports

  /** Map with "name" to transformations
    *
    */
  val transformations: scala.collection.Map[String, Transformation] = scala.collection.Map.empty[String, Transformation]

  /** Map with aliases
    *
    */
  val aliases: Aliases = new Aliases

  /** List of Grok configurations, currently we only expect to
    * find only a single one
    */
  val grok: scala.collection.Map[String, Grok] = scala.collection.Map.empty[String, Grok]
}
