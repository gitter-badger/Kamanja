/*
 * Copyright 2016 ligaDATA
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
package com.ligadata

import java.io.File

import com.ligadata.jtm.eval.Types
import com.ligadata.jtm.nodes.Root
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadataload.MetadataLoad
import com.ligadata.msgcompiler._
import org.apache.commons.io.FileUtils
import org.json4s.jackson.JsonMethods._

/**
  *
  */
package object jtm {

  /** Creates a metadata instance with defaults and json objects located on the file system
    *
    * @return Metadata manager
    */
  def loadMetadata(metadataLocation: String): MdMgr= {

    val typesPath : String = ""
    val fcnPath : String = ""
    val attrPath : String = ""
    val msgCtnPath : String = ""
    val mgr : MdMgr = MdMgr.GetMdMgr

    // If the compiler is called again this will throw
    // To Do: move the metadata and improve handling
    try {
      val mdLoader = new MetadataLoad(mgr, typesPath, fcnPath, attrPath, msgCtnPath)
      mdLoader.initialize

      def getRecursiveListOfFiles(dir: File): Array[File] = {
        val these = dir.listFiles.filter(_.isFile)
        val those = dir.listFiles.filter(_.isDirectory)
        these ++ those.flatMap(getRecursiveListOfFiles)
      }

      val files = getRecursiveListOfFiles(new File(metadataLocation))

      // Load all json files for the metadata directory
      files.map ( jsonFile => {
        val json = FileUtils.readFileToString(jsonFile, null:String)
        val map = parse(json).values.asInstanceOf[Map[String, Any]]
        val msg = new MessageCompiler()
        val ((classStrVer, classStrVerJava), msgDef, (classStrNoVer, classStrNoVerJava), rawMsgStr) = msg.processMsgDef(json, "JSON", mgr, 0, false)
        val msg1 = msgDef.asInstanceOf[com.ligadata.kamanja.metadata.MessageDef]
        mgr.AddMsg(msg1)
      })
    } catch {
      case _ : Throwable => ;
    }

    mgr
  }


}
