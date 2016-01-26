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

package com.ligadata.AdaptersConfiguration

import com.ligadata.InputOutputAdapterInfo.{ AdapterConfiguration, PartitionUniqueRecordKey, PartitionUniqueRecordValue }
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._

class FileAdapterConfiguration extends AdapterConfiguration {
  var Files: Array[String] = _ // Array of files and each execute one by one
  var CompressionString: String = _ // If it is null or empty we treat it as TEXT file
  var MessagePrefix: String = _ // This is the first String in the message
  var IgnoreLines: Int = _ // number of lines to ignore in each file
  var AddTS2MsgFlag: Boolean = false // Add TS after the Prefix Msg
  var append: Boolean = false // To append the data to file. Used only for output adapter
}

object FileAdapterConfiguration {
  def GetAdapterConfig(inputConfig: AdapterConfiguration): FileAdapterConfiguration = {
    if (inputConfig.adapterSpecificCfg == null || inputConfig.adapterSpecificCfg.size == 0) {
      val err = "Not found File Adapter Config:" + inputConfig.Name
      throw new Exception(err)
    }

    val fc = new FileAdapterConfiguration
    fc.Name = inputConfig.Name
    fc.formatName = inputConfig.formatName
    fc.validateAdapterName = inputConfig.validateAdapterName
    fc.failedEventsAdapterName = inputConfig.failedEventsAdapterName
    fc.className = inputConfig.className
    fc.jarName = inputConfig.jarName
    fc.dependencyJars = inputConfig.dependencyJars
    fc.associatedMsg = if (inputConfig.associatedMsg == null) null else inputConfig.associatedMsg.trim
    fc.keyAndValueDelimiter = if (inputConfig.keyAndValueDelimiter == null) null else inputConfig.keyAndValueDelimiter.trim
    fc.fieldDelimiter = if (inputConfig.fieldDelimiter == null) null else inputConfig.fieldDelimiter.trim
    fc.valueDelimiter = if (inputConfig.valueDelimiter == null) null else inputConfig.valueDelimiter.trim

    val adapCfg = parse(inputConfig.adapterSpecificCfg)
    if (adapCfg == null || adapCfg.values == null) {
      val err = "Not found File Adapter Config:" + inputConfig.Name
      throw new Exception(err)
    }
    val values = adapCfg.values.asInstanceOf[Map[String, String]]

    values.foreach(kv => {
      if (kv._1.compareToIgnoreCase("CompressionString") == 0) {
        fc.CompressionString = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("MessagePrefix") == 0) {
        fc.MessagePrefix = kv._2.trim
      } else if (kv._1.compareToIgnoreCase("IgnoreLines") == 0) {
        fc.IgnoreLines = kv._2.trim.toInt
      } else if (kv._1.compareToIgnoreCase("append") == 0) {
        fc.append = kv._2.trim.toBoolean
      } else if (kv._1.compareToIgnoreCase("AddTS2MsgFlag") == 0) {
      } else if (kv._1.compareToIgnoreCase("Files") == 0) {
        fc.Files = kv._2.split(",").map(str => str.trim).filter(str => str.size > 0)
      }
    })

    fc
  }
}

case class FileKeyData(Version: Int, Type: String, Name: Option[String]) // Using most of the values as optional values. Just thinking about future changes. Don't know the performance issues.

class FilePartitionUniqueRecordKey extends PartitionUniqueRecordKey {
  val Version: Int = 1
  val Type: String = "File"
  var Name: String = _

  override def Serialize: String = { // Making String from key
    val json =
      ("Version" -> Version) ~
        ("Type" -> Type) ~
        ("Name" -> Name)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = { // Making Key from Serialized String
    implicit val jsonFormats: Formats = DefaultFormats
    val keyData = parse(key).extract[FileKeyData]
    if (keyData.Version == Version && keyData.Type.compareTo(Type) == 0) {
      Name = keyData.Name.get
    }
    // else { } // Not yet handling other versions
  }
}

case class FileRecData(Version: Int, FileFullPath: Option[String], Offset: Option[Long]) // Using most of the values as optional values. Just thinking about future changes. Don't know the performance issues.

class FilePartitionUniqueRecordValue extends PartitionUniqueRecordValue {
  val Version: Int = 1
  var FileFullPath: String = _
  var Offset: Long = _ // Current Record File Offset

  override def Serialize: String = { // Making String from Value
    val json =
      ("Version" -> Version) ~
        ("FileFullPath" -> FileFullPath) ~
        ("Offset" -> Offset)
    compact(render(json))
  }

  override def Deserialize(key: String): Unit = { // Making Value from Serialized String
    implicit val jsonFormats: Formats = DefaultFormats
    val recData = parse(key).extract[FileRecData]
    if (recData.Version == Version) {
      FileFullPath = recData.FileFullPath.get
      Offset = recData.Offset.get
    }
    // else { } // Not yet handling other versions
  }
}

