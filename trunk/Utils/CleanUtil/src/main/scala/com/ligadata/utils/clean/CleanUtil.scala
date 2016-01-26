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

package com.ligadata.utils.clean

import java.io.InputStream

import com.ligadata.Exceptions._

import scala.io.Source

object CleanUtil {
  private val logger = org.apache.logging.log4j.LogManager.getLogger(this.getClass)
  private type OptionMap = Map[Symbol, Any]

  private val stream: InputStream = getClass.getResourceAsStream("/help.txt")
  private val helpStr: String = Source.fromInputStream(stream).mkString

  private val usage =
    """Usage: CleanUtil --config /path/to/MetadataAPIConfig.properties [--clean-kafka] [--clean-zookeeper] [--clean-testdata [List of messages/containers]] [--clean-metadata] 
       or CleanUtil --config /path/to/MetadataAPIConfig.properties [--clean-all [List of messages/containers]]
       or CleanUtil --help
    """.stripMargin



  private def nextOption(map: OptionMap, list: List[String]): OptionMap = {
    def isSwitch(s: String) = (s(0) == '-')
    list match {
      case Nil => map
      case "--help" :: tail =>
        nextOption(map ++ Map('help -> true), tail)
      case "--config" :: value :: tail if !isSwitch(value) =>
        nextOption(map ++ Map('config -> value), tail)
      case "--clean-testdata" :: value :: tail if !isSwitch(value)=>
        nextOption(map ++ Map('cleantestdata -> value), tail)
      case "--clean-testdata" :: tail =>
        nextOption(map ++ Map('cleantestdata -> true), tail)
      case "--clean-kafka" :: tail =>
        nextOption(map ++ Map('cleankafka -> true), tail)
      case "--clean-zookeeper" :: tail =>
        nextOption(map ++ Map('cleanzookeeper -> true), tail)
      case "--clean-metadata" :: tail =>
        nextOption(map ++ Map('cleanmetadata -> true), tail)
      case "--clean-all" :: value :: tail if !isSwitch(value) =>
        nextOption(map ++ Map('cleanall -> value), tail)
      case "--clean-all" :: tail =>
        nextOption(map ++ Map('cleanall -> true), tail)
      case option :: tail => {
        throw CleanUtilException(s"CLEAN-UTIL: Unknown option or invalid usage of option $option.\n$usage", null)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var config: CleanerConfiguration = null
    try{
      if(args.length == 0) {
        logger.error("CLEAN-UTIL: No parameters were given.")
        logger.error(usage)
        return
      }
      else {
        val options = nextOption(Map(), args.toList)
        val helpOpt = options.getOrElse('help, false).asInstanceOf[Boolean]
        if (helpOpt) {
          println(helpStr)
          return
        }

        val configFile = options.getOrElse('config, "").asInstanceOf[String]
        val cleanAll = options.getOrElse('cleanall, false)

        if (cleanAll.isInstanceOf[String] || (cleanAll.isInstanceOf[Boolean] && cleanAll.asInstanceOf[Boolean])) {
          config = new CleanerConfiguration(configFile)
          if(CleanZookeeper.isKamanjaClusterRunning(config.zookeeperInfo))
            throw CleanUtilException("CLEAN-UTIL: The Kamanja cluster is running. Aborting cleaning operation.", null)
          config.topicList.foreach(topic => {
            CleanKafka.deleteTopic(topic, config.zookeeperInfo.connStr)
          })
          CleanZookeeper.deletePath(config.zookeeperInfo)
          CleanStores.cleanMetadata(config.metadataStore)

          if (cleanAll.isInstanceOf[Boolean]) {
            CleanStores.cleanDatastore(config.dataStore, None)
          }
          else {
            CleanStores.cleanDatastore(config.dataStore, Some(cleanAll.asInstanceOf[String].split(',')))
          }
        }
        else {
          val cleanKafka = options.getOrElse('cleankafka, false).asInstanceOf[Boolean]
          val cleanZookeeper = options.getOrElse('cleanzookeeper, false).asInstanceOf[Boolean]
          val cleanMetadata = options.getOrElse('cleanmetadata, false).asInstanceOf[Boolean]
          // Not converting cleanTestdata to boolean since it could be a string as well.
          val cleanTestdata = options.getOrElse('cleantestdata, false)

          // Determining if any options were given. If none are given, error out and print usage.
          if(!cleanKafka && !cleanZookeeper && !cleanMetadata) {
            // If the previous 3 were true, check testData's type. If it's a boolean and that boolean is false
            // then error out with usage. Otherwise, it should be a string, indicating it's been provided by the user.
            if (cleanTestdata.isInstanceOf[Boolean]) {
              if (!cleanTestdata.asInstanceOf[Boolean]) {
                logger.error("CLEAN-UTIL: No options given exception --config. Please give at least one clean option.")
                logger.error(usage)
                return
              }
            }
          }
          config = new CleanerConfiguration(configFile)
          if(CleanZookeeper.isKamanjaClusterRunning(config.zookeeperInfo))
            throw CleanUtilException("CLEAN-UTIL: The Kamanja cluster is running. Aborting cleaning operation.", null)

          if (cleanKafka) {
            config.topicList.foreach(topic => {
              CleanKafka.deleteTopic(topic, config.zookeeperInfo.connStr)
            })
          }

          if (cleanZookeeper) {
            CleanZookeeper.deletePath(config.zookeeperInfo)
          }

          if (cleanMetadata) {
            CleanStores.cleanMetadata(config.metadataStore)
          }

          if (cleanTestdata.isInstanceOf[Boolean]) {

            if (cleanTestdata.asInstanceOf[Boolean]) {
              CleanStores.cleanDatastore(config.dataStore, None)
            }
          }
          else {
            CleanStores.cleanDatastore(config.dataStore, Some(cleanTestdata.asInstanceOf[String].split(',')))
          }
        }
      }
    }
    catch {
      case e: MissingArgumentException => logger.error("", e)
      case e: CleanUtilException => logger.error("", e)
      case e: Exception => logger.error("Unexpected Exception caught", e)
    }
    finally {
      if(config != null /* && config != "" */ ) {
        config.shutdown
      }
    }
  }
}
