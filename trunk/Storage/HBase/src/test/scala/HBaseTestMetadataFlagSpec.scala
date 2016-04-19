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

package com.ligadata.automation.unittests.hbaseadapter

import org.scalatest._
import Matchers._

import java.util.{Date, Calendar, TimeZone}
import java.text.{SimpleDateFormat}
import java.io._

import org.apache.logging.log4j._

import com.ligadata.KvBase._
import com.ligadata.StorageBase._
import com.ligadata.Serialize._
import com.ligadata.Utils.KamanjaLoaderInfo
import com.ligadata.keyvaluestore.HBaseAdapter

import com.ligadata.Exceptions._

class HBaseTestMetadataFlagSpec extends FunSpec with BeforeAndAfter with BeforeAndAfterAll with GivenWhenThen {
  var adapter: DataStore = null
  var serializer: Serializer = null

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
  val dateFormat1 = new SimpleDateFormat("yyyy/MM/dd")
  // set the timezone to UTC for all time values
  TimeZone.setDefault(TimeZone.getTimeZone("UTC"))

  private val kvManagerLoader = new KamanjaLoaderInfo
  private var hbaseAdapter: HBaseAdapter = null
  serializer = SerializerManager.GetSerializer("kryo")
  val dataStoreInfo = s"""{"StoreType": "hbase","SchemaName": "unit_tests","Location":"localhost","autoCreateTables":"YES"}"""

  private val maxConnectionAttempts = 10;
  var cnt: Long = 0
  private val containerName = "sys.customer1"
  private var readCount = 0

  private def RoundDateToSecs(d: Date): Date = {
    var c = Calendar.getInstance()
    if (d == null) {
      c.setTime(new Date(0))
      c.getTime
    }
    else {
      c.setTime(d)
      c.set(Calendar.MILLISECOND, 0)
      c.getTime
    }
  }

  def readCallBack(key: Key, value: Value) {
    logger.info("timePartition => " + key.timePartition)
    logger.info("bucketKey => " + key.bucketKey.mkString(","))
    logger.info("transactionId => " + key.transactionId)
    logger.info("rowId => " + key.rowId)
    logger.info("serializerType => " + value.serializerType)
    logger.info("serializedInfo length => " + value.serializedInfo.length)
    val cust = serializer.DeserializeObjectFromByteArray(value.serializedInfo).asInstanceOf[Customer]
    logger.info("serializedObject => " + cust)
    logger.info("----------------------------------------------------")
    readCount = readCount + 1
  }

  def readKeyCallBack(key: Key) {
    logger.info("timePartition => " + key.timePartition)
    logger.info("bucketKey => " + key.bucketKey.mkString(","))
    logger.info("transactionId => " + key.transactionId)
    logger.info("rowId => " + key.rowId)
    logger.info("----------------------------------------------------")
    readCount = readCount + 1
  }

  def deleteFile(path: File): Unit = {
    if (path.exists()) {
      if (path.isDirectory) {
        for (f <- path.listFiles) {
          deleteFile(f)
        }
      }
      path.delete()
    }
  }

  private def CreateAdapter: DataStore = {
    var connectionAttempts = 0
    while (connectionAttempts < maxConnectionAttempts) {
      try {
        adapter = HBaseAdapter.CreateStorageAdapter(kvManagerLoader, dataStoreInfo)
        return adapter
      } catch {
        case e: Exception => {
          logger.error("will retry after one minute ...", e)
          Thread.sleep(60 * 1000L)
          connectionAttempts = connectionAttempts + 1
        }
      }
    }
    return null;
  }

  override def beforeAll = {
    try {
      logger.info("starting...");
      logger.info("Initialize HBaseAdapter")
      adapter = CreateAdapter
    }
    catch {
      case e: Exception => throw new Exception("Failed to execute set up properly", e)
    }
  }

  describe("Unit Tests for hbaseadapter use of MetadataContainer flag") {

    // validate property setup
    it("Validate api operations") {
      val containerName = "sys.customer1"
      val metadataContainerName = "sys.meta_customer1"

      hbaseAdapter = adapter.asInstanceOf[HBaseAdapter]

      And("create namespace")
      noException should be thrownBy {
        hbaseAdapter.CreateNameSpace("unit_tests")
      }

      And("drop container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.DropContainer(containers)
      }

      And("drop isMetadata table")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ "isMetadata"
        adapter.DropContainer(containers)
      }

      And("create container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.CreateContainer(containers)
      }

      val containerCount = 10
      And("Test Put api for container")
      var keys = new Array[Key](0) // to be used by a delete operation later on
      for (i <- 1 to containerCount) {
        var currentTime = new Date()
        //var currentTime = null
        var keyArray = new Array[String](0)
        var custName = "customer-" + i
        keyArray = keyArray :+ custName
        var key = new Key(currentTime.getTime(), keyArray, i, i)
        keys = keys :+ key
        var custAddress = "1000" + i + ",Main St, Redmond WA 98052"
        var custNumber = "425666777" + i
        var obj = new Customer(custName, custAddress, custNumber)
        var v = serializer.SerializeObjectToByteArray(obj)
        var value = new Value(1,"kryo",v)
        noException should be thrownBy {
          adapter.put(containerName, key, value)
        }
      }

      And("Get all the rows that were just added")
      noException should be thrownBy {
        adapter.get(containerName, readCallBack _)
      }

      hbaseAdapter = adapter.asInstanceOf[HBaseAdapter]

      And("Check the row count after adding a bunch")
      cnt = hbaseAdapter.getRowCount(containerName)
      assert(cnt == containerCount)


      And("drop metadata container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ metadataContainerName
        adapter.DropContainer(containers)
      }

      And("create metadata containers")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ metadataContainerName
        adapter.CreateMetadataContainer(containers)
      }

      var metadataContainerCount = 4
      And("Test Put api for metadata container")
      keys = new Array[Key](0) // to be used by a delete operation later on
      for (i <- 1 to 4) {
        var currentTime = new Date()
        //var currentTime = null
        var keyArray = new Array[String](0)
        var custName = "meta_customer-" + i
        keyArray = keyArray :+ custName
        var key = new Key(currentTime.getTime(), keyArray, i, i)
        keys = keys :+ key
        var custAddress = "1000" + i + ",Main St, Redmond WA 98052"
        var custNumber = "425666777" + i
        var obj = new Customer(custName, custAddress, custNumber)
        var v = serializer.SerializeObjectToByteArray(obj)
        var value = new Value(1,"kryo",v)
        noException should be thrownBy {
          adapter.put(metadataContainerName, key, value)
        }
      }

      And("Get all the rows that were just added")
      noException should be thrownBy {
        adapter.get(metadataContainerName, readCallBack _)
      }

      hbaseAdapter = adapter.asInstanceOf[HBaseAdapter]

      And("Check the row count after adding a bunch")
      cnt = hbaseAdapter.getRowCount(metadataContainerName)
      assert(cnt == metadataContainerCount)

      And("Shutdown hbase session")
      noException should be thrownBy {
        adapter.Shutdown
      }

    }
  }

  override def afterAll = {
    var logFile = new java.io.File("logs")
    if (logFile != null) {
      deleteFile(logFile)
    }
  }
}
