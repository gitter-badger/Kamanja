package com.ligadata.tools.containersutility

import com.ligadata.KamanjaBase.{MessageContainerBase, MdBaseResolveInfo, SerializeDeserialize}
import org.scalatest.{BeforeAndAfterAll, Matchers, GivenWhenThen, FeatureSpec}
import Matchers._
import java.util.{Properties, Date, Calendar, TimeZone}
import java.text.{SimpleDateFormat}
import java.io._
import org.apache.logging.log4j._

import com.ligadata.keyvaluestore._
import com.ligadata.KvBase._
import com.ligadata.StorageBase._
import com.ligadata.Serialize._
import com.ligadata.Utils.{Utils, KamanjaClassLoader, KamanjaLoaderInfo}
import com.ligadata.StorageBase.StorageAdapterObj
import com.ligadata.keyvaluestore.HashMapAdapter

/**
  * Created by Yousef on 3/30/2016.
  */
case class Customer(name:String, address: String, homePhone: String)

class UtilityForContainersTest extends FeatureSpec with GivenWhenThen with BeforeAndAfterAll {
  var res : String = null;
  var statusCode: Int = -1;
  var adapter:DataStore = null
  var serializer:Serializer = null

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
  val dateFormat1 = new SimpleDateFormat("yyyy/MM/dd")
  val dateFormat2 = new SimpleDateFormat("yyyyMMddKKmmss")
  // set the timezone to UTC for all time values
  TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

  private val kvManagerLoader = new KamanjaLoaderInfo
  private var hashmapAdapter:HashMapAdapter = null
  //private var dataDirectory = getClass.getResource("/DataDirectory")
  private var dataDirectory = "."

  override def beforeAll = {
    try {
      logger.info("starting...");

      serializer = SerializerManager.GetSerializer("kryo")
      logger.info("Initialize HashMapAdapter")
      var dataStoreInfo="{\"StoreType\": \"" + "hashmap" + "\",\"SchemaName\": \"" + "unit_tests" + "\",\"Location\": \"" + dataDirectory + "\"}"
      adapter = HashMapAdapter.CreateStorageAdapter(kvManagerLoader, dataStoreInfo)
    }
    catch {
      case e: Exception => throw new Exception("Failed to execute set up properly", e)
    }
  }

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
    val cust = serializer.DeserializeObjectFromByteArray(value.serializedInfo)//.asInstanceOf[Customer]
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

  feature("test all case in UtilityForContainers") {
    scenario("test create container") {
      Given("initiate values")
      var cfgfile = "C:\\Users\\Yousef\\Downloads\\Engine1Config.properties"

      val containerName = "sys.customer1"
      Then("Test drop container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.DropContainer(containers)
      }

      Then("Test create container")
      noException should be thrownBy {
        var containers = new Array[String](0)
        containers = containers :+ containerName
        adapter.CreateContainer(containers)
      }

      Then("Test Put api")
      var keys = new Array[Key](0) // to be used by a delete operation later on
      for (i <- 1 to 10) {
        var currentTime = new Date()
        var keyArray = new Array[String](0)
        var custName = "customer-" + i
        keyArray = keyArray :+ custName
        var key = new Key(currentTime.getTime(), keyArray, i, i)
        keys = keys :+ key
        var custAddress = "1000" + i + ",Main St, Redmond WA 98052"
        var custNumber = "425666777" + i
        var obj = new Customer(custName, custAddress, custNumber)
        var v = serializer.SerializeObjectToByteArray(obj)
        var value = new Value("kryo", v)
        noException should be thrownBy {
          adapter.put(containerName, key, value)
        }
      }

      Then("Test truncate container")
      noException should be thrownBy {
        adapter.TruncateContainer(Array(containerName))
      }


      var keyValueList = new Array[(Key, Value)](0)
      var keyStringList = new Array[Array[String]](0)
      for( i <- 1 to 10 ){
        var  cal = Calendar.getInstance();
        cal.add(Calendar.DATE, -i);
        var currentTime = cal.getTime()
        var keyArray = new Array[String](0)
        var custName = "customer-" + i
        keyArray = keyArray :+ custName
        // keyStringList is only used to test a del operation later
        keyStringList = keyStringList :+ keyArray
        var key = new Key(currentTime.getTime(),keyArray,i,i)
        var custAddress = "1000" + i + ",Main St, Redmond WA 98052"
        var custNumber = "4256667777" + i
        var obj = new Customer(custName,custAddress,custNumber)
        var v = serializer.SerializeObjectToByteArray(obj)
        var value = new Value("kryo",v)
        keyValueList = keyValueList :+ (key,value)
      }
      var dataList = new Array[(String, Array[(Key,Value)])](0)
      dataList = dataList :+ (containerName,keyValueList)
      noException should be thrownBy {
        adapter.put(dataList)
      }

      Then("Get all the rows that were just added")
      noException should be thrownBy {
        adapter.get(containerName,readCallBack _)
      }

      Then("Test Delete for a time range")
      var  cal = Calendar.getInstance();
      cal.add(Calendar.DATE, -10);
      var beginTime = cal.getTime()
      logger.info("begin time => " + dateFormat.format(beginTime))
      cal = Calendar.getInstance();
      cal.add(Calendar.DATE, -1);
      var endTime = cal.getTime()
      logger.info("end time => " + dateFormat.format(endTime))
      logger.info("time ==> " + endTime.getTime())
      val date:Date  = new Date("2016/03/20 15:54:55")
      cal.setTime(date) ///1458489295000
      logger.info("time date ==> " + cal.getTime().getTime())

      var timeRange = new TimeRange(beginTime.getTime(),endTime.getTime())
      noException should be thrownBy {
        adapter.del(containerName,timeRange)
      }

      noException should be thrownBy {
        adapter.del(containerName,timeRange,keyStringList)
      }

//      noException should be thrownBy {
//        adapter.del(containerName,keyStringList)
//      }


      Then("Get all the rows after delete")
      noException should be thrownBy {
        adapter.get(containerName,readCallBack _)
      }

    }
  }

}
