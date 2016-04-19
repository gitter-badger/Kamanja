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

import com.ligadata.Utils._
import util.control.Breaks._
import scala.io._
import java.util.{ Date, Calendar, TimeZone }
import java.text.{ SimpleDateFormat }
import java.io._

import sys.process._
import org.apache.logging.log4j._

import com.ligadata.keyvaluestore._
import com.ligadata.KvBase._
import com.ligadata.StorageBase._
import com.ligadata.Serialize._
import com.ligadata.Utils.Utils._
import com.ligadata.Utils.{ KamanjaClassLoader, KamanjaLoaderInfo }
import com.ligadata.StorageBase.StorageAdapterFactory
import com.ligadata.keyvaluestore.HBaseAdapter

import com.ligadata.Exceptions._

@Ignore
class HBasePerfTestSpec extends FunSpec with BeforeAndAfter with BeforeAndAfterAll with GivenWhenThen {
  var res: String = null;
  var statusCode: Int = -1;
  var adapter: DataStore = null
  var serializer: Serializer = null

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
  val dateFormat1 = new SimpleDateFormat("yyyy/MM/dd")
  // set the timezone to UTC for all time values
  TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
  
  val dataStoreInfo = """{"StoreType": "hbase","SchemaName": "unit_tests","Location":"localhost"}"""
  private val kvManagerLoader = new KamanjaLoaderInfo
  private val maxConnectionAttempts = 10;
  var cnt:Long = 0

  private val containerName = "sys.customer1"

  private def CreateAdapter: DataStore = {
    var connectionAttempts = 0
    while (connectionAttempts < maxConnectionAttempts) {
      try {
        adapter = HBaseAdapter.CreateStorageAdapter(kvManagerLoader, dataStoreInfo)
        return adapter
      } catch {
        case e: StorageConnectionException => {
          logger.error("will retry after one minute ...", e)
          connectionAttempts = connectionAttempts + 1
          Thread.sleep(60 * 1000L)
        }
        case e: Exception => {
          logger.error("Failed to connect", e)
          logger.error("retrying ...")
        }
      }
    }
    return null;
  }

  override def beforeAll = {
    try {
      logger.info("starting...");

      serializer = SerializerManager.GetSerializer("kryo")
      logger.info("Initialize HBaseAdapter")
      adapter = CreateAdapter
    } catch {
      case e: StorageConnectionException => {
        logger.error("", e)
      }
      case e: Exception => {
        logger.error("Failed to connect", e)
      }
    }
  }

  private def RoundDateToSecs(d: Date): Date = {
    var c = Calendar.getInstance()
    if (d == null) {
      c.setTime(new Date(0))
      c.getTime
    } else {
      c.setTime(d)
      c.set(Calendar.MILLISECOND, 0)
      c.getTime
    }
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

  object ReadCount {
    var rec_count = 0
    def increment: Unit ={
      rec_count = rec_count + 1
      if(rec_count % 1000 == 0 ){
	logger.info("Read " + rec_count + " so far ...")
      }
    }
  }

  def readCallBack(key:Key, value: Value) {
    logger.info("timePartition => " + key.timePartition)
    logger.info("bucketKey => " + key.bucketKey.mkString(","))
    logger.info("transactionId => " + key.transactionId)
    logger.info("rowId => " + key.rowId)
    logger.info("serializerType => " + value.serializerType)
    logger.info("serializedInfo length => " + value.serializedInfo.length)
    val cust = serializer.DeserializeObjectFromByteArray(value.serializedInfo).asInstanceOf[Customer]
    logger.info("serializedObject => " + cust)
    logger.info("----------------------------------------------------")
  }


  def readCallBack1(key:Key, value: Value) {
    ReadCount.increment
  }

  private def GetCurDtTmStr: String = {
    new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(new java.util.Date(System.currentTimeMillis))
  }

  describe("Load Tests for the adapter") {

    // validate property setup
    it("Load data operations") {

      And("Test drop container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.DropContainer(containers)
      }

      And("Test create container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.CreateContainer(containers)
      }

      And("Test Bulk Put api")

      logger.info(GetCurDtTmStr + ": Start Loading  1 million records 1000 at a time")

      for (batch <- 1 to 10) {
	var successful = false
	while ( ! successful ){
          var keyValueList = new Array[(Key, Value)](0)
          var keyStringList = new Array[Array[String]](0)
          for (i <- 1 to 1000) {
            var cal = Calendar.getInstance();
            cal.add(Calendar.DATE, -i);
            var currentTime = cal.getTime()
            var keyArray = new Array[String](0)
            var custName = "batch-" + batch + "-customer-" + i
            keyArray = keyArray :+ custName
            keyStringList = keyStringList :+ keyArray
            var key = new Key(currentTime.getTime(), keyArray, i, i)
            var custAddress = "1000" + batch * i + ",Main St, Redmond WA 98052"
            var custNumber = "4256667777" + batch * i
            var obj = new Customer(custName, custAddress, custNumber)
            var v = serializer.SerializeObjectToByteArray(obj)
            var value = new Value(1,"kryo",v)
            keyValueList = keyValueList :+ (key, value)
          }
          var dataList = new Array[(String, Array[(Key, Value)])](0)
          dataList = dataList :+ (containerName, keyValueList)
	  try{
	    adapter.put(dataList)
            logger.info(GetCurDtTmStr + ": Loaded " + batch * 1000 + " objects ")
	    successful = true
	  }
	  catch{
	    case e: Exception => {
	      logger.info("", e)
	      Thread.sleep(10000)
	      successful = false
	    }
	  }
	}
      }

      val hbaseAdapter = adapter.asInstanceOf[HBaseAdapter]

      And("Check the row count after adding a bunch")
      cnt = hbaseAdapter.getRowCount(containerName)
      assert(cnt == 10000)
    }

    it("Bulk Read Operations"){
      And("Read 1000 records at a time")
      for (batch <- 1 to 10) {
	var successful = false
	while ( ! successful ){
          var keyStringList = new Array[Array[String]](0)
          for (i <- 1 to 1000) {
            var keyArray = new Array[String](0)
            var custName = "batch-" + batch + "-customer-" + i
            keyArray = keyArray :+ custName
            keyStringList = keyStringList :+ keyArray
	  }
	  try{
	    adapter.get(containerName,keyStringList,readCallBack1 _)
	    logger.info(GetCurDtTmStr + ": Fetched " + batch * 1000 + " objects ")
	    successful = true
	  }
	  catch{
	    case e: Exception => {
	      logger.info("", e)
	      successful = false
	    }
	  }
	}
      }
    }
    
    ignore("Cleanup Operations"){
      And("Test drop container again, cleanup")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.DropContainer(containers)
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
