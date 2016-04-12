package com.ligadata.testmsgcompiler

import org.scalatest.FunSuite
import com.ligadata.msgcompiler.MessageCompiler
import com.ligadata.msgcompiler.Message
import scala.io.Source
import java.io.File
import com.ligadata.kamanja.metadata.MdMgr
import com.ligadata.kamanja.metadata.ContainerDef
import com.ligadata.kamanja.metadata.MessageDef
import com.ligadata.kamanja.metadataload.MetadataLoad
import scala.collection.mutable._
import java.io.File
import java.io.PrintWriter

class MessageCompilerTestSuite extends FunSuite {

  test("Process Message Compiler") {
    val mdLoader: MetadataLoad = new MetadataLoad(MdMgr.GetMdMgr, "", "", "", "")
    mdLoader.initialize
    var msgDfType: String = "JSON"

    var messageCompiler = new MessageCompiler;
    val jsonstr: String = Source.fromFile("./MessageCompiler/src/test/resources/fixedmsgs/product.json").getLines.mkString
     val ((verScalaMsg, verJavaMsg), containerDef, (nonVerScalaMsg, nonVerJavaMsg), rawMsg) = messageCompiler.processMsgDef(jsonstr, msgDfType, MdMgr.GetMdMgr, 0)
    //createScalaFile(verScalaMsg, containerDef.Version.toString, containerDef.FullName, ".scala")

    assert(verScalaMsg === verScalaMsg)
    println("==========verScalaMsg  ")
    assert(containerDef.Name === "product")
  }

  private def createScalaFile(scalaClass: String, version: String, className: String, clstype: String): Unit = {
    try {
      val writer = new PrintWriter(new File("./MessageCompiler/src/test/resources/GeneratedMsgs/" + className + "_" + version + clstype))
      // val writer = new PrintWriter(new File("src/test/resources/GeneratedMsgs/" + className + "_" + version + clstype))
      writer.write(scalaClass.toString)
      writer.close()
      println("Done")
    } catch {
      case e: Exception => {
        e.printStackTrace()
        throw e
      }
    }
  }
}
