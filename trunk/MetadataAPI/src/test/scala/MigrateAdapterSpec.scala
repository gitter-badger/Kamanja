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

class MigrateAdapterSpec extends FunSpec with LocalTestFixtures with BeforeAndAfter with BeforeAndAfterAll with GivenWhenThen {
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
      MetadataAPIImpl.LoadAllObjectsIntoCache

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


  describe("Unit Tests for Migration of Adapter objects to 1.4") {
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


    it("Load Cluster Config ") {

      And("Check whether CONFIG_FILES_DIR defined as property")
      dirName = MetadataAPIImpl.GetMetadataAPIConfig.getProperty("CONFIG_FILES_DIR")
      assert(null != dirName)

      And("Check Directory Path")
      iFile = new File(dirName)
      assert(true == iFile.exists)

      And("Check whether " + dirName + " is a directory ")
      assert(true == iFile.isDirectory)

      And("Make sure there are few JSON config files in " + dirName);
      val cfgFiles = new java.io.File(dirName).listFiles.filter(_.getName.endsWith(".json"))
      assert(0 != cfgFiles.length)

      fileList = List("ClusterConfig.json")
      fileList.foreach(f1 => {
	And("Add the Config From " + f1)
	And("Make Sure " + f1 + " exist")
	var exists = false
	var file: java.io.File = null
	breakable {
	  cfgFiles.foreach(f2 => {
	    if (f2.getName() == f1) {
	      exists = true
	      file = f2
	      break
	    }
	  })
	}
	assert(true == exists)

	And("AddConfig  from " + file.getPath)
	var cfgStr = Source.fromFile(file).mkString
	res = MetadataAPIImpl.UploadConfig(cfgStr, None, "testConfig")
	res should include regex ("\"Status Code\" : 0")

	And("Check number of the adapters")
	var adapters = MdMgr.GetMdMgr.Adapters
	assert(adapters.size == 4)

	And("Convert map adapters to json")
	val jsonStr = JsonSerializer.SerializeCfgObjectListToJson("Adapters",adapters.toArray.map(x => x._2))
	logger.info(jsonStr)
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
