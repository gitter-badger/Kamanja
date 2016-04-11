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

package com.ligadata.jsonutility

import org.apache.commons.io.FilenameUtils
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import scala.collection.mutable.HashMap
import java.io.File
import java.io.PrintWriter
import org.apache.logging.log4j._
import scala.io.Source.fromFile

trait LogTrait {
  val loggerName = this.getClass.getName()
  val logger = LogManager.getLogger(loggerName)
}


object JsonChecker {

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private type OptionMap = Map[Symbol, Any]

  private def nextOption(map: OptionMap, list: List[String]): OptionMap = {
    def isSwitch(s: String) = (s.charAt(0) == '-')
    list match {
      case Nil => map
      case "--inputfile" :: value :: tail =>
        nextOption(map ++ Map('inputfile -> value), tail)
      case option :: tail => {
        logger.error("Unknown option " + option)
        sys.exit(1)
      }
    }
  }

   def main(args: Array[String]) {

    logger.debug("JsonChecker.main begins")

    if (args.length == 0) {
      logger.error("Please pass the input file as parameter")
      sys.exit(1)
    }
    val options = nextOption(Map(), args.toList)

    val inputfile = options.getOrElse('inputfile, null).toString.trim
    if (inputfile == null && inputfile.toString().trim() == "") {
      logger.error("Please pass the input file as parameter")
      sys.exit(1)
    }

    var jsonBen: JsonChecker = new JsonChecker()
    val jsonFileFlag = jsonBen.FindFileExtension(inputfile)
    if(jsonFileFlag == false){
      logger.error("The file extension is not .json. We only accept json files.")
      sys.exit(1)
    }else {
      val fileExistFlag = jsonBen.FileExist(inputfile)
      if (fileExistFlag == true) {
        val fileContent = jsonBen.ReadFile(inputfile)
        if (fileContent.equalsIgnoreCase(null) || fileContent.size == 0) {
          logger.error("The file does not include data. Check your file please.")
          sys.exit(1)
        } else {
          jsonBen.ParseFile(fileContent)
        }
      }else {
        logger.error("The file does not exist. Check the name of the file.")
        sys.exit(1)
    }
    }

    logger.info("Json file parsed successfully");
  }

}

class JsonChecker {

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  def FindFileExtension (filePath: String) : Boolean = {
    val ext = FilenameUtils.getExtension(filePath);
    if (ext.equalsIgnoreCase("json")){
      return true
    } else {
      return false
    }
  }

  def ParseFile(filePath: String): Unit ={
    try{
      val parsedFile = parse(filePath)
    } catch{
      case e: Exception => logger.error("there is an error in the format of file \n ErrorMsg : ", e)
        sys.exit(1)
    }
  }

  def ReadFile(filePath: String): String ={
    return fromFile(filePath).mkString // read file (JSON file)
  }

  def FileExist(filePath: String): Boolean={
    return new java.io.File(filePath).exists
  }
}

