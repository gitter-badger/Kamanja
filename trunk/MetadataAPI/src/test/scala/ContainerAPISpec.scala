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

package com.ligadata.automation.unittests.api

import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import com.ligadata.automation.unittests.api.setup._
import org.scalatest._
import Matchers._

import com.ligadata.MetadataAPI._
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadata.MdMgr._

import com.ligadata.Utils._
import util.control.Breaks._
import scala.io._
import java.util.Date
import java.io._

import sys.process._
import org.apache.logging.log4j._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import com.ligadata.Serialize._

import com.ligadata.kamanja.metadataload.MetadataLoad

class ContainerAPISpec extends FunSpec with LocalTestFixtures with BeforeAndAfter with BeforeAndAfterAll with GivenWhenThen {
	var res: String = null;
	var statusCode: Int = -1;
	var apiResKey: String = "\"Status Code\" : 0"
	var objName: String = null
	var contStr: String = null
	var version: String = null
	var o: Option[ContainerDef] = None
	var dirName: String = null
	var iFile: File = null
	var fileList: List[String] = null
	var newVersion: String = null
	val userid: Option[String] = Some("test")

	private val loggerName = this.getClass.getName
	private val logger = LogManager.getLogger(loggerName)

	private def TruncateDbStore = {
		val db = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE")
		assert(null != db)
		db match {
			case "sqlserver" | "mysql" | "hbase" | "cassandra" | "hashmap" | "treemap" => {
				var ds = MetadataAPIImpl.GetMainDS
				var containerList: Array[String] = Array("config_objects", "jar_store", "model_config_objects", "metadata_objects", "transaction_id")
				ds.TruncateContainer(containerList)
			}
			case _ => {
				logger.info("TruncateDbStore is not supported for database " + db)
			}
		}
	}

	private def DropDbStore = {
		val db = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE")
		assert(null != db)
		db match {
			case "sqlserver" | "mysql" | "hbase" | "cassandra" | "hashmap" | "treemap" => {
				var ds = MetadataAPIImpl.GetMainDS
				var containerList: Array[String] = Array("config_objects", "jar_store", "model_config_objects", "metadata_objects", "transaction_id")
				ds.DropContainer(containerList)
			}
			case _ => {
				logger.info("DropDbStore is not supported for database " + db)
			}
		}
	}

	override def beforeAll = {
		try {

			logger.info("starting...");

			logger.info("resource dir => " + getClass.getResource("/").getPath)

			logger.info("Initialize MetadataManager")
			mdMan.config.classPath = ConfigDefaults.metadataClasspath
			mdMan.initMetadataCfg

			logger.info("Initialize MdMgr")
			MdMgr.GetMdMgr.truncate
			val mdLoader = new MetadataLoad(MdMgr.mdMgr, "", "", "", "")
			mdLoader.initialize

			val zkServer = EmbeddedZookeeper
			zkServer.instance.startup

			logger.info("Initialize zooKeeper connection")
			MetadataAPIImpl.initZkListeners(false)

			logger.info("Initialize datastore")
			var tmpJarPaths = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
			logger.info("jarPaths => " + tmpJarPaths)
			val jarPaths = if (tmpJarPaths != null) tmpJarPaths.split(",").toSet else scala.collection.immutable.Set[String]()
			MetadataAPIImpl.OpenDbStore(jarPaths, MetadataAPIImpl.GetMetadataAPIConfig.getProperty("METADATA_DATASTORE"))

			var jp = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
			logger.info("jarPaths => " + jp)


			logger.info("Truncating dbstore")
			TruncateDbStore

			And("PutTranId updates the tranId")
			noException should be thrownBy {
				MetadataAPIImpl.PutTranId(0)
			}

			logger.info("Load All objects into cache")
			MetadataAPIImpl.LoadAllObjectsIntoCache(false)

			// The above call is resetting JAR_PATHS based on nodeId( node-specific configuration)
			// This is causing unit-tests to fail
			// restore JAR_PATHS value
			MetadataAPIImpl.GetMetadataAPIConfig.setProperty("JAR_PATHS", tmpJarPaths)
			jp = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("JAR_PATHS")
			logger.info("jarPaths => " + jp)

			logger.info("Initialize security adapter")
			MetadataAPIImpl.InitSecImpl

			MetadataAPIImpl.TruncateAuditStore
			MetadataAPIImpl.isInitilized = true
			logger.info(MetadataAPIImpl.GetMetadataAPIConfig)
		}
		catch {
			case e: EmbeddedZookeeperException => {
				throw new EmbeddedZookeeperException("EmbeddedZookeeperException detected")
			}
			case e: Exception => throw new Exception("Failed to execute set up properly", e)
		}
	}

	/**
		* extractNameFromPMML - pull the Application name="xxx" from the PMML doc and construct
		* a name  string from it, cloned from APIService.scala
		*/
	def extractNameFromPMML(pmmlObj: String): String = {
		var firstOccurence: String = "unknownModel"
		val pattern = """Application[ ]*name="([^ ]*)"""".r
		val allMatches = pattern.findAllMatchIn(pmmlObj)
		allMatches.foreach(m => {
			if (firstOccurence.equalsIgnoreCase("unknownModel")) {
				firstOccurence = (m.group(1))
			}
		})
		return firstOccurence
	}

	describe("Unit Tests for Container MetadataAPI operations") {

		// validate property setup
		it("Validate properties for MetadataAPI") {
			And("MetadataAPIImpl.GetMetadataAPIConfig should have been initialized")
			val cfg = MetadataAPIImpl.GetMetadataAPIConfig
			assert(null != cfg)

			And("The property DATABASE must have been defined")
			val db = cfg.getProperty("DATABASE")
			assert(null != db)
			if (db == "cassandra") {
				And("The property MetadataLocation must have been defined for store type " + db)
				val loc = cfg.getProperty("DATABASE_LOCATION")
				assert(null != loc)
				And("The property MetadataSchemaName must have been defined for store type " + db)
				val schema = cfg.getProperty("DATABASE_SCHEMA")
				assert(null != schema)
			}
			And("The property NODE_ID must have been defined")
			assert(null != cfg.getProperty("NODE_ID"))


			And("The property JAR_TRAGET_DIR must have been defined")
			val d = cfg.getProperty("JAR_TARGET_DIR")
			assert(null != d)

			And("Make sure the Directory " + d + " exists")
			val f = new File(d)
			assert(null != f)

			And("The property SCALA_HOME must have been defined")
			val sh = cfg.getProperty("SCALA_HOME")
			assert(null != sh)

			And("The property JAVA_HOME must have been defined")
			val jh = cfg.getProperty("SCALA_HOME")
			assert(null != jh)

			And("The property CLASSPATH must have been defined")
			val cp = cfg.getProperty("CLASSPATH")
			assert(null != cp)

			And("The property ZNODE_PATH must have been defined")
			val zkPath = cfg.getProperty("ZNODE_PATH")
			assert(null != zkPath)

			And("The property ZOOKEEPER_CONNECT_STRING must have been defined")
			val zkConnStr = cfg.getProperty("ZOOKEEPER_CONNECT_STRING")
			assert(null != zkConnStr)

			And("The property SERVICE_HOST must have been defined")
			val shost = cfg.getProperty("SERVICE_HOST")
			assert(null != shost)

			And("The property SERVICE_PORT must have been defined")
			val sport = cfg.getProperty("SERVICE_PORT")
			assert(null != sport)

			And("The property JAR_PATHS must have been defined")
			val jp = cfg.getProperty("JAR_PATHS")
			assert(null != jp)
			logger.info("jar_paths => " + jp)

			And("The property SECURITY_IMPL_JAR  must have been defined")
			val sij = cfg.getProperty("SECURITY_IMPL_JAR")
			assert(null != sij)

			And("The property SECURITY_IMPL_CLASS  must have been defined")
			val sic = cfg.getProperty("SECURITY_IMPL_CLASS")
			assert(null != sic)

			And("The property DO_AUTH  must have been defined")
			val da = cfg.getProperty("DO_AUTH")
			assert(null != da)

			And("The property AUDIT_IMPL_JAR  must have been defined")
			val aij = cfg.getProperty("AUDIT_IMPL_JAR")
			assert(null != sij)

			And("The property AUDIT_IMPL_CLASS  must have been defined")
			val aic = cfg.getProperty("AUDIT_IMPL_CLASS")
			assert(null != sic)

			And("The property DO_AUDIT  must have been defined")
			val dau = cfg.getProperty("DO_AUDIT")
			assert(null != dau)

			And("The property SSL_CERTIFICATE  must have been defined")
			val sc = cfg.getProperty("SSL_CERTIFICATE")
			assert(null != sc)
		}

		// CRUD operations on container objects
		it("Container Tests") {
			And("Check whether CONTAINER_FILES_DIR defined as property")
			dirName = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("CONTAINER_FILES_DIR")
			assert(null != dirName)

			And("Check Directory Path")
			iFile = new File(dirName)
			assert(true == iFile.exists)

			And("Check whether " + dirName + " is a directory ")
			assert(true == iFile.isDirectory)

			And("Make sure there are few JSON container files in " + dirName);
			val contFiles = new java.io.File(dirName).listFiles.filter(_.getName.endsWith(".json"))
			assert(0 != contFiles.length)

			//fileList = List("CoughCodes.json","EnvCodes.json","DyspnoeaCodes.json","SmokeCodes.json","SputumCodes.json")
			fileList = List("EnvCodes.json")
			fileList.foreach(f1 => {
				And("Add the Container From " + f1)
				And("Make Sure " + f1 + " exist")
				var exists = false
				var file: java.io.File = null
				breakable {
					contFiles.foreach(f2 => {
						if (f2.getName() == f1) {
							exists = true
							file = f2
							break
						}
					})
				}
				assert(true == exists)

				And("GetContainerDef API to fetch the container that may not even exist, check for Status Code of -1")
				objName = f1.stripSuffix(".json").toLowerCase
				version = "0000000000001000000"
				res = MetadataAPIImpl.GetContainerDef("system", objName, "JSON", version, None)
				res should include regex ("\"Status Code\" : -1")

				And("AddContainer first time from " + file.getPath)
				contStr = Source.fromFile(file).mkString
				println("HERE IS MY METADATA CONFIGURATION!!!!!!!!!!!")
				MetadataAPIImpl.dumpMetadataAPIConfig

				res = MetadataAPIImpl.AddContainer(contStr, "JSON", None)
				res should include regex ("\"Status Code\" : 0")

				And("GetAllContainerDefs API to fetch the containers that were just added")
				res = MetadataAPIImpl.GetAllContainerDefs("JSON")
			        logger.info("Containers => " + res)

				And("GetContainerDef API to fetch the container that was just added")
				res = MetadataAPIImpl.GetContainerDef("system", objName, "JSON", version, None)
				res should include regex ("\"Status Code\" : 0")

				And("AddContainer second time from " + file.getPath + ",should result in error")
				contStr = Source.fromFile(file).mkString
				res = MetadataAPIImpl.AddContainer(contStr, "JSON", None)
				res should include regex ("\"Status Code\" : -1")

				And("RemoveContainer API for the container that was just added")
				res = MetadataAPIImpl.RemoveContainer(objName, 1000000, None)
				res should include regex ("\"Status Code\" : 0")

				And("GetContainerDef API to fetch the container that was just removed, should fail, check for Status Code of -1")
				res = MetadataAPIImpl.GetContainerDef("system", objName, "JSON", version, None)
				res should include regex ("\"Status Code\" : -1")

				And("AddContainer again to add Container from " + file.getPath)
				contStr = Source.fromFile(file).mkString
				res = MetadataAPIImpl.AddContainer(contStr, "JSON", None)
				res should include regex ("\"Status Code\" : 0")

				And("GetContainerDef API to fetch  the container that was just added")
				res = MetadataAPIImpl.GetContainerDef("system", objName, "JSON", version, None)
				res should include regex ("\"Status Code\" : 0")

				And("Get the container object from the cache")
				o = MdMgr.GetMdMgr.Container("system", objName, version.toLong, true)
				assert(o != None)

				And("Deactivate container that was just added")
				MetadataAPIImpl.DeactivateObject(o.get.asInstanceOf[BaseElemDef])

				And("Get the active container object from the cache after deactivating")
				o = MdMgr.GetMdMgr.Container("system", objName, version.toLong, true)
				assert(o == None)

				And("Make sure the container object from the cache nolonger active ")
				o = MdMgr.GetMdMgr.Container("system", objName, version.toLong, false)
				assert(o != None)

				And("Activate container that was just deactivated")
				MetadataAPIImpl.ActivateObject(o.get.asInstanceOf[BaseElemDef])

				And("Make sure the container object from the cache is active")
				o = MdMgr.GetMdMgr.Container("system", objName, version.toLong, true)
				assert(o != None)

				And("Update the container without changing version number, should fail ")
				res = MetadataAPIImpl.UpdateContainer(contStr, "JSON")
				res should include regex ("\"Status Code\" : -1")

				And("Clone the input json and update the version number to simulate a container for an update operation")
				contStr = contStr.replaceFirst("01.00", "01.01")
				assert(contStr.indexOf("\"00.01.01\"") >= 0)
				res = MetadataAPIImpl.UpdateContainer(contStr, "JSON")
				res should include regex ("\"Status Code\" : 0")

				And("GetContainerDef API to fetch the container that was just updated")
				newVersion = "0000000000001000001"
				res = MetadataAPIImpl.GetContainerDef("system", objName, "JSON", newVersion, None)
				res should include regex ("\"Status Code\" : 0")

				And("Get the active container object from the cache after updating")
				o = MdMgr.GetMdMgr.Container("system", objName, newVersion.toLong, true)
				assert(o != None)

				And("Make sure old(pre update version) container object nolonger active after the update")
				o = MdMgr.GetMdMgr.Container("system", objName, version.toLong, true)
				assert(o == None)
			})
		}
	}


	override def afterAll = {
		logger.info("Truncating dbstore")
		var file = new java.io.File("logs")
		if (file.exists()) {
			TestUtils.deleteFile(file)
		}

	  //file = new java.io.File("lib_managed")
	  //if(file.exists()){
	  //TestUtils.deleteFile(file)
	  //}

		val db = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("DATABASE")
		assert(null != db)
		db match {
			case "hashmap" | "treemap" => {
				DropDbStore
			}
			case _ => {
				logger.info("cleanup...")
			}
		}
		TruncateDbStore
		MetadataAPIImpl.shutdown
	}

	if (zkServer != null) {
		zkServer.instance.shutdown
	}
}
