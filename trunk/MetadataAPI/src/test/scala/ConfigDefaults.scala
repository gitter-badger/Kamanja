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

package com.ligadata.automation.unittests.api.setup

import scala.io._
import java.util.Date
import java.io._
import java.nio.channels._
import sys.process._
import org.apache.logging.log4j._

/**
 * Created by wtarver on 1/28/15.
 * Some basic defaults for use in manual creation of configuration for instantiation of certain classes. Meant for typical usage.
 * Custom extensions need custom configuration.
 * This may be deprecated later.
 */
object ConfigDefaults {

  private val loggerName = this.getClass.getName
  private val logger = LogManager.getLogger(loggerName)

  val scalaVersionFull = scala.util.Properties.versionNumberString
  val scalaVersion = scalaVersionFull.substring(0, scalaVersionFull.lastIndexOf('.'))

  private val RootDir = s"./MetadataAPI/target/scala-$scalaVersion/test-classes"
  private val targetLibDir = RootDir + "/jars/lib/system"
  private val appLibDir = RootDir + "/jars/lib/application"
  private val workDir = RootDir + "/jars/lib/workingdir"

  private val IgnoreDir = "MetadataAPI/target"

  private def copyFile(sourceFile:File, destFile:File)  {
    try{
    var source:FileChannel = null;
    var destination:FileChannel = null;

    source = new FileInputStream(sourceFile).getChannel();
    destination = new FileOutputStream(destFile).getChannel();
    destination.transferFrom(source, 0, source.size());
    source.close()
    destination.close()
  }
    catch {
      case e: Exception => throw new Exception("Failed to copy file: " + sourceFile.getName(),e)
    }
  }

  private def createDirectory(dirName:String){
    var dir = new File(dirName)
    if( ! dir.exists() ){
      dir.mkdirs()
    }
  }

  private def copy(path: File): Unit = {
    if(path.isDirectory ){
      if( path.getPath.contains(IgnoreDir) ){
	//logger.debug("We don't copy any files from directory that contains " + IgnoreDir)
	return
      }
      Option(path.listFiles).map(_.toList).getOrElse(Nil).foreach(f => {
        if (f.isDirectory){
          copy(f)
	}
        else if (f.getPath.endsWith(".jar")) {
          try {
	    logger.debug("Copying " + f + "," + "(file size => " + f.length() + ") to " + targetLibDir + "/" + f.getName)
	    val d = new File(targetLibDir + "/" + f.getName)
	    if( ! d.exists() ){
	      d.createNewFile()
          }
	    copyFile(f,d)
          }
          catch {
            case e: Exception => throw new Exception("Failed to copy file: " + f,e)
          }
        }
      })
    }
  }

  createDirectory(targetLibDir)
  createDirectory(appLibDir)
  createDirectory(workDir)

  //copy(new File("lib_managed"))
  copy(new File("."))

  def jarResourceDir = getClass.getResource("/jars/lib/system").getPath

  logger.info("jarResourceDir " + jarResourceDir)

  def envContextClassName: String = "com.ligadata.SimpleEnvContextImpl.SimpleEnvContextImpl$"
  def envContextDependecyJarList: List[String] = List(s"ExtDependencyLibs_$scalaVersion-1.4.0.jar", s"KamanjaInternalDeps_$scalaVersion-1.4.0.jar", s"ExtDependencyLibs2_$scalaVersion-1.4.0.jar")
  def envContextJarName = s"simpleenvcontextimpl_$scalaVersion-1.0.jar"

  def nodeClassPath: String = s".:$jarResourceDir/ExtDependencyLibs_$scalaVersion-1.4.0.jar:$jarResourceDir/KamanjaInternalDeps_$scalaVersion-1.4.0.jar:$jarResourceDir/ExtDependencyLibs2_$scalaVersion-1.4.0.jar"

  def adapterDepJars: List[String] = List(s"ExtDependencyLibs_$scalaVersion-1.4.0.jar", s"KamanjaInternalDeps_$scalaVersion-1.4.0.jar", s"ExtDependencyLibs2_$scalaVersion-1.4.0.jar")

  val scala_home = System.getenv("SCALA_HOME")

  def scalaJarsClasspath = s"$scala_home/lib/typesafe-config.jar:$scala_home/lib/scala-actors.jar:$scala_home/lib/akka-actors.jar:$scala_home/lib/scalap.jar:$scala_home/lib/jline.jar:$scala_home/lib/scala-swing.jar:$scala_home/lib/scala-library.jar:$scala_home/lib/scala-actors-migration.jar:$scala_home/lib/scala-reflect.jar:$scala_home/lib/scala-compiler.jar"

  def dataDirectory = getClass.getResource("/DataDirectories").getPath
  logger.info("dataDirectory => " + dataDirectory)

  def metadataDirectory = getClass.getResource("/Metadata").getPath
  logger.info("metadataDirectory => " + metadataDirectory)

  def dataStorePropertiesFile:String = metadataDirectory + "/config/DataStore.properties"

  val metadataClasspath: String = List(
    s"ExtDependencyLibs_$scalaVersion-1.4.0.jar",
    s"KamanjaInternalDeps_$scalaVersion-1.4.0.jar",
    s"ExtDependencyLibs2_$scalaVersion-1.4.0.jar"
  ).mkString(s""""$jarResourceDir/""", s":$jarResourceDir/", "\"")
}
