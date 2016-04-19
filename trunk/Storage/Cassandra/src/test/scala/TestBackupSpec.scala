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

package com.ligadata.automation.unittests.cassandraadapter

import org.scalatest._
import Matchers._

import com.ligadata.Utils._
import util.control.Breaks._
import scala.io._
import java.util.{Date,Calendar,TimeZone}
import java.text.{SimpleDateFormat}
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
import com.ligadata.keyvaluestore.CassandraAdapter

import com.ligadata.Exceptions._

@ Ignore
class TestBackupSpec extends FunSpec with BeforeAndAfter with BeforeAndAfterAll with GivenWhenThen {
  var res : String = null;
  var statusCode: Int = -1;
  var adapter:DataStore = null
  var serializer:Serializer = null

  var stackTrace = ""

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
  val dateFormat1 = new SimpleDateFormat("yyyy/MM/dd")
  // set the timezone to UTC for all time values
  TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

  private val kvManagerLoader = new KamanjaLoaderInfo
  private var cassandraAdapter:CassandraAdapter = null
  serializer = SerializerManager.GetSerializer("kryo")
  val dataStoreInfo = """{"StoreType": "cassandra","SchemaName": "unit_tests","Location":"localhost","autoCreateTables":"YES"}"""

  private val maxConnectionAttempts = 10;
  var cnt:Long = 0
  private var containerName = "sys.customer1"


  private def RoundDateToSecs(d:Date): Date = {
    var c = Calendar.getInstance()
    if( d == null ){
      c.setTime(new Date(0))
      c.getTime
    }
    else{
      c.setTime(d)
      c.set(Calendar.MILLISECOND,0)
      c.getTime
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

  def readKeyCallBack(key:Key) {
    logger.info("timePartition => " + key.timePartition)
    logger.info("bucketKey => " + key.bucketKey.mkString(","))
    logger.info("transactionId => " + key.transactionId)
    logger.info("rowId => " + key.rowId)
    logger.info("----------------------------------------------------")
  }

  def deleteFile(path:File):Unit = {
    if(path.exists()){
      if (path.isDirectory){
	for(f <- path.listFiles) {
          deleteFile(f)
	}
      }
      path.delete()
    }
  }

  private def ProcessException(e: Exception) = {
      e match {
	case e1: StorageDMLException => {
	  logger.error("Inner Message:%s: Message:%s".format(e1.cause.getMessage,e1.getMessage))
	}
	case e2: StorageDDLException => {
	  logger.error("Innser Message:%s: Message:%s".format(e2.cause.getMessage,e2.getMessage))
	}
	case e3: StorageConnectionException => {
	  logger.error("Inner Message:%s: Message:%s".format(e3.cause.getMessage,e3.getMessage))
	}
	case _ => {
	  logger.error("Message:%s".format(e.getMessage))
	}
      }
      var stackTrace = StackTrace.ThrowableTraceString(e)
      logger.info("StackTrace:"+stackTrace)
  }	  

  private def CreateAdapter: DataStore = {
    var connectionAttempts = 0
    while (connectionAttempts < maxConnectionAttempts) {
      try {
        adapter = CassandraAdapter.CreateStorageAdapter(kvManagerLoader, dataStoreInfo)
        return adapter
      } catch {
        case e: Exception => {
	  ProcessException(e)
          logger.error("will retry after one minute ...")
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
      logger.info("Initialize CassandraAdapter")
      adapter = CreateAdapter
   }
    catch {
      case e: Exception => throw new Exception("Failed to execute set up properly\n" + e)
    }
  }

  describe("Unit Tests for all cassandraadapter operations") {

    // validate property setup
    it ("Validate api operations") {
      containerName = "sys.customer1"

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

      And("Test Put api")
      var keys = new Array[Key](0) // to be used by a delete operation later on
      for( i <- 1 to 10 ){
	var currentTime = new Date()
	//var currentTime = null
	var keyArray = new Array[String](0)
	var custName = "customer-" + i
	keyArray = keyArray :+ custName
	var key = new Key(currentTime.getTime(),keyArray,i,i)
	keys = keys :+ key
	var custAddress = "1000" + i + ",Main St, Redmond WA 98052"
	var custNumber = "425666777" + i
	var obj = new Customer(custName,custAddress,custNumber)
	var v = serializer.SerializeObjectToByteArray(obj)
	var value = new Value(1,"kryo",v)
	noException should be thrownBy {
	  adapter.put(containerName,key,value)
	}
      }

      And("Get all the rows that were just added")
      noException should be thrownBy {
	adapter.get(containerName,readCallBack _)
      }

      cassandraAdapter = adapter.asInstanceOf[CassandraAdapter]

      And("Check the row count after adding a bunch")
      var cnt = cassandraAdapter.getRowCount(containerName,null)
      assert(cnt == 10)

      And("Get all the keys for the rows that were just added")
      noException should be thrownBy {
	adapter.getKeys(containerName,readKeyCallBack _)
      }
      
      And("Test backup container")
      noException should be thrownBy {
	adapter.backupContainer(containerName)
      }

      And("Shutdown cassandra session")
      noException should be thrownBy {
	adapter.Shutdown
      }

    }
  }
  override def afterAll = {
    var logFile = new java.io.File("logs")
    if( logFile != null ){
      deleteFile(logFile)
    }
  }
}
