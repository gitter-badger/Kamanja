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

package com.ligadata.automation.unittests.sqlserveradapter

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
import com.ligadata.keyvaluestore.SqlServerAdapter

import com.ligadata.Exceptions._


object SimulateDeadlockSpec {
  private var adapter:DataStore = null
  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)
  private[this] val lock = new Object

  val dataStoreInfo = """{"StoreType": "sqlserver","hostname": "192.168.56.1","instancename":"KAMANJA","portnumber":"1433","database": "bofa","user":"bofauser","SchemaName":"bofauser","password":"bofauser","jarpaths":"/media/home2/jdbc","jdbcJar":"sqljdbc4-2.0.jar","clusteredIndex":"YES","autoCreateTables":"YES"}"""

  private val kvManagerLoader = new KamanjaLoaderInfo
  private val maxConnectionAttempts = 10;

  private def CreateAdapter: DataStore = lock.synchronized {
    logger.info("Creating a new adapter")
    var connectionAttempts = 0
    while (connectionAttempts < maxConnectionAttempts) {
      try {
        adapter = SqlServerAdapter.CreateStorageAdapter(kvManagerLoader, dataStoreInfo)
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
  private def getAdapter: DataStore = lock.synchronized {
    if( adapter != null ){
      return adapter
    }
    else{
      return CreateAdapter
    }
  }
}

@Ignore
class SimulateDeadlockSpec extends FunSuite with BeforeAndAfter with BeforeAndAfterAll with ParallelTestExecution {
  var adapter: DataStore = null
  var serializer: Serializer = null
  private[this] val lock = new Object

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)
  TimeZone.setDefault(TimeZone.getTimeZone("UTC"));
  
  val containerName = "sys.uniquedata"
  var exitImmediately = true

  private def CreateAdapter: DataStore = lock.synchronized {
    return SimulateDeadlockSpec.getAdapter
  }

  before {
    try {
      logger.info("starting...");
      serializer = SerializerManager.GetSerializer("kryo")
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

  private def GetCurDtTmStr: String = {
    new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS").format(new java.util.Date(System.currentTimeMillis))
  }

  private def SimulateDML(keyIndex: Int): Unit = {
    for (batch <- 1 to 100 ) {
      logger.info("put for batch " + batch + ",row : " + keyIndex)
      var successful = false
      while ( ! successful ){
	try{
          var keyValueList = new Array[(Key, Value)](0)
	  var dataList = new Array[(String, Array[(Key, Value)])](0)
          var keyArray = new Array[String](0)
          var custName = "customer-" + keyIndex
          keyArray = keyArray :+ custName
          var key = new Key(0, keyArray, 0, 0)
          var custAddress = "1000" + batch * keyIndex + ",Main St, Redmond WA 98052"
          var custNumber = "4256667777" + batch * keyIndex
          var obj = new Customer(custName, custAddress, custNumber)
          var v = serializer.SerializeObjectToByteArray(obj)
          var value = new Value(1,"kryo", v)
          keyValueList = keyValueList :+ (key, value)
          dataList = dataList :+ (containerName, keyValueList)
	  adapter.put(dataList)
	  successful = true
	}
	catch{
	  case e: Exception => {
	    logger.info("", e)
	    successful = exitImmediately
	  }
	}
      }
    }
  }

  test("thread1") {
    SimulateDML(1)
  }

  test("thread2") {
    SimulateDML(2)
  }

  test("thread3") {
    SimulateDML(3)
  }

  test("thread4") {
    SimulateDML(4)
  }

  test("thread5") {
    SimulateDML(5)
  }

  test("thread6") {
    SimulateDML(6)
  }

  test("thread7") {
    SimulateDML(7)
  }

  test("thread8") {
    SimulateDML(8)
  }

  after {
    var logFile = new java.io.File("logs")
    if (logFile != null) {
      deleteFile(logFile)
    }
  }
}
