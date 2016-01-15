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

package com.ligadata.keyvaluestore

import com.ligadata._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.ligadata.StorageBase.{ DataStore }
import org.apache.log4j._
import com.ligadata.keyvaluestore._
// import com.ligadata.Utils.Utils._
import com.ligadata.Utils.{ KamanjaClassLoaderFrom, KamanjaLoaderInfoFrom }
import com.ligadata.StorageBase.StorageAdapterObj

object KeyValueManager {
  private val loggerName = this.getClass.getName
  private val logger = Logger.getLogger(loggerName)
  private val kvManagerLoader = new KamanjaLoaderInfoFrom
  // We will add more implementations here 
  // so we can test  the system characteristics
  //
  def Get(jarPaths: collection.immutable.Set[String], datastoreConfig: String, tableName: String): DataStore = {
    val adapterConfig = if (datastoreConfig != null) datastoreConfig.trim else ""

    if (adapterConfig.size == 0) {
      throw new Exception("Not found valid Storage Configuration.")
    }

    logger.debug("Storage configuration:" + adapterConfig)
    var parsed_json: Map[String, Any] = null
    try {
      val json = parse(adapterConfig)
      if (json == null || json.values == null) {
        logger.error("Failed to parse Storage JSON configuration string:" + adapterConfig)
        throw new Exception("Failed to parse Storage JSON configuration string:" + adapterConfig)
      }
      parsed_json = json.values.asInstanceOf[Map[String, Any]]
    } catch {
      case e: Exception => {
        logger.error("Failed to parse Storage JSON configuration string:%s. Reason:%s Message:%s".format(adapterConfig, e.getCause, e.getMessage))
        throw e
      }
    }

    val storeType = parsed_json.getOrElse("StoreType", "").toString.trim.toLowerCase

    storeType.toLowerCase match {

      // Other KV stored
      case "cassandra" => return KeyValueCassandra.CreateStorageAdapter(kvManagerLoader, datastoreConfig, tableName)
      case "hbase"     => return KeyValueHBase.CreateStorageAdapter(kvManagerLoader, datastoreConfig, tableName)
      // Simple file base implementations
      //      case "treemap" => return KeyValueTreeMap.CreateStorageAdapter(kvManagerLoader, datastoreConfig, tableName)
      //      case "hashmap" => return KeyValueHashMap.CreateStorageAdapter(kvManagerLoader, datastoreConfig, tableName)
      //      case "redis" => return KeyValueRedis.CreateStorageAdapter(kvManagerLoader, datastoreConfig, tableName)
    }
    
    throw new Exception("Unhandled Database type" + storeType)
    return null
  }
}
