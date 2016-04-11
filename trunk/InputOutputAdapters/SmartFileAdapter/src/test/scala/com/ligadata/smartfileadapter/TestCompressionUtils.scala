package com.ligadata.smartfileadapter

import com.ligadata.AdaptersConfiguration.SmartFileAdapterConfiguration
import com.ligadata.InputAdapters.{SmartFileHandlerFactory, CompressionUtil}
import com.ligadata.InputOutputAdapterInfo.AdapterConfiguration
import org.scalatest._

/**
  * Created by Yasser on 3/30/2016.
  */
class TestCompressionUtils extends  FunSpec with BeforeAndAfter with ShouldMatchers with BeforeAndAfterAll {

  describe("getFileType test of a file") {

    //comment for now, used library(magic mime) is causing too many log messages on debug level for some file types
    /*it("should detect file type of msg_test5_not_compressed as (text/plain)") {
      val plainFilePath = getResourceFullPath("/msg_test5_not_compressed")
      println("textFilePath=" + plainFilePath)
      val handler = SmartFileHandlerFactory.createSmartFileHandler(createDefaultAdapterConfig, plainFilePath)
      println("handler created successfully for file " + handler.getFullPath)

      val fileType = CompressionUtil.getFileType(handler, null)
      fileType shouldEqual "text/plain"
    }*/

    it("should detect file type of msg_test5_gzip as (application/gzip)") {
      val gzipFilePath = getResourceFullPath("/msg_test5_gzip")
      //println("textFilePath="+gzipFilePath)
      val handler = SmartFileHandlerFactory.createSmartFileHandler(createDefaultAdapterConfig, gzipFilePath)

      val fileType = CompressionUtil.getFileType(handler, null)
      fileType shouldEqual "application/gzip"
    }

    it("should detect file type of msg_test5_bz2 as (application/x-bzip2)") {
      val bz2FilePath = getResourceFullPath("/msg_test5_bz2")
      //println("textFilePath="+bz2FilePath)
      val handler = SmartFileHandlerFactory.createSmartFileHandler(createDefaultAdapterConfig, bz2FilePath)

      val fileType = CompressionUtil.getFileType(handler, null)
      fileType shouldEqual "application/x-bzip2"
    }

    //comment for now, used library(magic mime) is causing too many log messages on debug level for some file types
    /*it("should detect file type of msg_test5_1x_lzop as (application/x-lzop)") {
      val lzopFilePath = getResourceFullPath("/msg_test5_1x_lzop")
      println("textFilePath="+lzopFilePath)
      val handler = SmartFileHandlerFactory.createSmartFileHandler(createDefaultAdapterConfig, lzopFilePath)
      println("handler created successfully for file " + handler.getFullPath)

      val fileType = CompressionUtil.getFileType(handler, null)
      fileType shouldEqual "application/x-lzop"
    }*/

  }

  private def getResourceFullPath(resourcePath : String): String ={
    val os = System.getProperty("os.name")
    val isWidnows = os.toLowerCase.contains("windows")
    val path = getClass.getResource(resourcePath).getPath
    val finalPath = if(isWidnows) path.substring(1) else path
    finalPath
  }

  //config is needed to create a file handler object
  private def createDefaultAdapterConfig :SmartFileAdapterConfiguration = {
    val inputConfig = new AdapterConfiguration()
    inputConfig.Name = "TestInput_2"
    inputConfig.className = "com.ligadata.InputAdapters.SamrtFileInputAdapter$"
    inputConfig.jarName = "smartfileinputoutputadapters_2.10-1.0.jar"
    //inputConfig.dependencyJars = new Set()
    inputConfig.adapterSpecificCfg = adapterSpecificCfgJson

    SmartFileAdapterConfiguration.getAdapterConfig(inputConfig)
  }

  val adapterSpecificCfgJson =
    """
      |{
      |  "Type": "das/nas",
      |  "ConnectionConfig": {
      |  },
      |  "MonitoringConfig": {
      |     "Locations": "/data/input",
      |      "TargetMoveDir": "/data/processed",
      |    "MaxTimeWait": "5000"
      |  }
      |}
    """.stripMargin
}
