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

package com.ligadata.clusterInstaller

import scala.collection.immutable.{ Set, HashMap }
import scala.io.Source

import java.io._

import org.apache.logging.log4j.{ Logger, LogManager }
import org.joda.time._
import org.joda.time.format._
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import com.ligadata.kamanja.metadata._
import com.ligadata.Exceptions.StackTrace
import com.ligadata.kamanja.metadata._

/** 
  This application installs and upgrades Kamanaja.  It does these essential things:

    1)  It verifies that a valid environment is available on the cluster nodes specified in the configuration.  The validation includes determination
    if the nodes mentioned in the node configuration exist, whether they are configured with the apporopriate scala and java, can access a valid
    zookeeper cluster, can access a valid kafka cluster and can use the hBase kv store configured for this cluster.
    2)  The cluster installation script is used to install a new version of the Kamanja release binariesa in the new installation directory on each node.
    3)  If an existing cluster installation is being upgraded, the appropriate migration script is invoked.  A migration json file is prepared with the essential
      details required for this migration.
      
  Notes:
    1) The prior installation directory, if upgrading, and the new installation directory are determined from the apiConfig property, ROOT_DIR.  To 
    make sure we no where the new and old installations are located (they are sibling directories in the same parent folder), the parent folder is 
    supplied to the cluster install script.
    2) The migration script requires a JSON file that contains information about the upgrade.  It looks like this:

          {
            "clusterConfigFile": "{NewPackageInstallPath}/config/ClusterConfig.json",
            "apiConfigFile": "{NewPackageInstallPath}/config/MetadataAPIConfig.properties",
            "unhandledMetadataDumpDir": "{UnhandledMetadataDumpDir}",
            "dataSaveThreshold": 1000,
            "migratingFrom": {
              "version": "1.1",
              "versionInstallPath": "{OldPackageInstallPath}",
              "implemtedClass": "com.ligadata.Migrate.MigrateFrom_V_1_1",
              "jars": [
                "{NewPackageInstallPath}/lib/system/migratebase-1.0.jar",
                "{NewPackageInstallPath}/lib/system/migratefrom_v_1_1_2.10-1.0.jar",
                "{OldPackageInstallPath}/bin/KamanjaManager-1.0"
              ]
            },
            "migratingTo": {
              "version": "1.3",
              "versionInstallPath": "{NewPackageInstallPath}",
              "implemtedClass": "com.ligadata.Migrate.MigrateTo_V_1_3",
              "jars": [
                "{NewPackageInstallPath}/lib/system/migratebase-1.0.jar",
                "{NewPackageInstallPath}/lib/system/migrateto_v_1_3_2.11-1.0.jar",
                "{NewPackageInstallPath}/bin/KamanjaManager-1.0"
              ]
            },
            "excludeMetadata": [
              "ModelDef",
              "MessageDef",
              "ContainerDef",
              "ConfigDef",
              "FunctionDef",
              "JarDef",
              "OutputMsgDef"
            ],
            "excludeData": false
          }

      where {NewPackageInstallPath} will be substituted with the new installation directory found in the parent folder.
          {OldPackageInstallPath} will be substituted with the existing installation directory in the parent folder.
    3) This installation process will be discretely logged in a file whose file path is displayed to the console.  All critical details about 
    the steps taken in the installation/upgrade will be noted.  Should failures occur, the log should be consulted for what went wrong and instruction 
    on how to roll back the an upgrade to the prior state if appropriate.  For new installations, simply rerunning the script after correcting the 
    issues in the environment that possibly created the failure should be sufficient.
 */

class InstallDriverLog(logPath : String) {

  val bufferedWriter = new BufferedWriter(new FileWriter(new File(logPath)))

  /** open the discrete log file if possible.  If open fails (file must not previously exist) false is returned */
  def initialize : Boolean = {
      (bufferedWriter != null)
  }

  def log.emit(msg : String) : Unit = {
      /** write the string as a line to the file */
      bufferedWriter.write(cfgString)

  } 

  def finalize : Unit = {
     bufferedWriter.close
  }

}

object InstallDriver extends App {

  def usage: String = {
    """
com.ligadata.clusterInstaller.Driver_1_3 --{upgrade|install} --apiConfig <MetadataAPIConfig.properties file> --clusterConfig <ClusterConig.json file> --fromKamanja "1.1" [--fromScala "2.10"] [--toScala "2.11"] --workingDir <workingdirectory>

    where 
        --upgrade explicitly specifies that the intent to upgrade an existing cluster installation with the latest release.
        --install explicitly specifies that the intent that this is to be a new installation.  If there is an existing implementation and 
            the --install option is given, the installation will fail.  Similarly, if there is NO existing implementation
            and the --upgrade option has been given, the upgrade will fail. If both --updgrade and --install are given, the install fails.)
        --apiConfig <MetadataAPIConfig.properties file> specifies the path of the properties file that (among others) specifies the location of the installation on each
            cluster node given in the cluster configuration.  Note that all installations exist in the same location on all nodes.
        --clusterConfig <ClusterConig.json file> gives the file that describes the node information for the cluster, including the IP address of each node, et al.
        --fromKamanja "N.N" where "N.N" can be either "1.1" or "1.2"
        [--fromScala "2.10"] an optional parameter that, for the 1.3 InstallDriver, simply documents the version of Scala that the current 1.1. or 1.2 is using.  The value
            "2.10" is the only possible value for this release.  
        [--toScala "2.11"] an optional parameter that when given declares the intent to upgrade the Scala compiler used by the Kamanja engine to 2.11.x instead of using the 
            2.10.x compiler.  Note that this controls the compiler version that the Kamanja message compiler, Kamanja pmml compiler, and other future Kamanja 
            components will use when building their respective objects.
        --workingDir <workingdirectory> a CRUD directory path that should be addressable on every node specified in the cluster configuration file.  It is used by the script
            to create intermediate files used during the installation process.
      
    The ClusterInstaller Driver_1_3 is the cluster installer driver for Kamanja 1.3.  It is capable of installing a new version of 1.3 or given the appropriate arguments, installing a new version of Kamanja 1.3 *and* upgrading a 1.1 or 1.2 installation to the 1.3 version. 
      
"""
  }

  override def main(args: Array[String]): Unit = {

    if (args.length == 0) { println(usage); sys.exit(1) }
    val arglist = args.toList
    type OptionMap = Map[Symbol, String]

    println(s"arguments supplied are:\n $arglist")
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "--clusterId" :: value :: tail =>
          nextOption(map ++ Map('clusterId -> value), tail)
        case "--apiConfig" :: value :: tail =>
          nextOption(map ++ Map('apiConfig -> value), tail)
        case "--clusterConfig" :: value :: tail =>
          nextOption(map ++ Map('clusterConfig -> value), tail)
        case "--tarballPath" :: value :: tail =>
          nextOption(map ++ Map('tarballPath -> value), tail)
        case "--fromKamanja" :: value :: tail =>
          nextOption(map ++ Map('fromKamanja -> value), tail)
        case "--fromScala" :: value :: tail =>
          nextOption(map ++ Map('fromScala -> value), tail)
        case "--toScala" :: value :: tail =>
          nextOption(map ++ Map('toScala -> value), tail)
        case "--workingDir" :: value :: tail =>
          nextOption(map ++ Map('workingDir -> value), tail)
        case "--upgrade" :: tail =>
                               nextOption(map ++ Map('upgrade -> "true"), tail)
        case "--install" :: tail =>
                               nextOption(map ++ Map('install -> "true"), tail)
        case option :: tail =>
          println("Unknown option " + option)
          println(usage)
          sys.exit(1)
      }
    }

    val options = nextOption(Map(), arglist)

    val clusterId : String = if (options.contains('clusterId)) options.apply('clusterId) else null
    val apiConfigPath : String = if (options.contains('apiConfig)) options.apply('apiConfig) else null
    val nodeConfigPath : String = if (options.contains('clusterConfig)) options.apply('clusterConfig) else null
    val tarballPath : String = if (options.contains('tarballPath)) options.apply('tarballPath) else null
    val fromKamanja : String = if (options.contains('fromKamanja)) options.apply('fromKamanja) else null
    val fromScala : String = if (options.contains('fromScala)) options.apply('fromScala) else "2.10"
    val toScala : String = if (options.contains('toScala)) options.apply('toScala) else "2.11"
    val workingDir : String = if (options.contains('workingDir)) options.apply('workingDir) else null
    val upgrade : Boolean = if (options.contains('upgrade)) options.apply('upgrade) == "true" else false
    val install : Boolean = if (options.contains('install)) options.apply('install) == "true" else false

    /** FIXME: we might want to create more meaningful failure messages here that pinpoint the complaint instead
        of the "your arguments are not satisfactory...Usage:"   */
    val confusedIntention : Boolean = (upgrade && install)
    val reasonableArguments: Boolean = (
        clusterId != null && clusterId.size > 0
        && apiConfigPath != null && apiConfigPath.size > 0
        && nodeConfigPath != null && nodeConfigPath.size > 0
        && tarballPath != null && tarballPath.size > 0
        && fromKamanja != null && fromKamanja.size > 0 && (fromKamanja == "1.1" || fromKamanja == "1.2")
        && fromScala != null && fromScala.size > 0 && fromScala == "2.10"
        && toScala != null && toScala.size > 0 && (toScala == "2.10" || toScala == "2.11")
        && workingDir != null && workingDir.size > 0 
        && ! confusedIntention)

    if (! reasonableArguments) {
        println("Your arguments are not satisfactory...Usage:")
        println(usage)
        sys.exit(1)
    }

    /** make a log ... FIXME: generate a timestamp for the "SomeDate" in the file path below... maybe make better configurable path */
    val log : InstallDriverLog = new InstallDriverLog("/tmp/installationLog/LogDriverResults.SomeDate.log")

    /** Convert the content of the property file into a map.  If the path is bad, an empty map is returned and processing stops */
    val apiConfigMap : Map[String,String] = mapProperties(log, apiConfigPath)
    if (apiConfigMap.isEmpty) {
        println("The configuration file is messed up... it needs to be lines of key=value pairs")
        log.emit("The configuration file is messed up... it needs to be lines of key=value pairs")
        println(usage)
        log.emit(usage)
        log.finalize
        sys.exit(1)
    }

    /** Ascertain what the name of the new installation directory will be, what the name of the prior installation would be (post install), and the parent 
    directory in which both of them will live on each cluster node. Should there be no existing installation, the prior installation value will be null. */
    val (parentPath, priorInstallDirName, newInstallDirName) : (String, String, String) = CreateInstallationNames(apiConfigMap)

    /** Run the node info extract on the supplied file and garner all of the information needed to conduct the cluster environment validatation */
    val installDir : String = s"$parentPath/$newInstallDirName"
    val (ips, ipIdTargPaths, ipPathPairs) : (Array[String], Array[(String, String, String, String)], Array[(String, String)]) = 
                extractNodeInfo(log
                              , clusterId
                              , apiConfigMap
                              , apiConfigPath
                              , nodeConfigPath
                              , installDir
                              , workingDir)

    /** Validate that the proposed installation has the requisite characteristics (scala, java, zookeeper, kafka, and hbase) */
    val proposedClusterEnvironmentIsSuitable  : Boolean = validateClusterEnvironment(log
                                                                                  , apiConfigMap
                                                                                  , nodeConfigPath
                                                                                  , fromKamanja
                                                                                  , fromScala
                                                                                  , toScala
                                                                                  , upgrade
                                                                                  , install
                                                                                  , parentPath,
                                                                                  , priorInstallDirName
                                                                                  , newInstallDirName
                                                                                  , ips, ipIdTargPaths, ipPathPairs)
    if (proposedClusterEnvironmentIsSuitable) {
        
        /** Install the new installation */
        val nodes : String = clusterNodes(ips)
        log.emit(s"Begin cluster installation... installation found on each cluster node(any {$nodes}) at $installDir")
        val installOk : Boolean = installCluster(log, apiConfigPath, nodeConfigPath, parentPath, priorInstallDirName, newInstallDirName)
        if (installOk) {
            /** Do upgrade if necessary */
            if (upgrade) {
                log.emit(s"Upgrade required... upgrade from version $fromKamanja")
                val upgradeOk : Boolean = doMigration(log
                                                    , apiConfigMap
                                                    , nodeConfigPath
                                                    , fromKamanja
                                                    , fromScala
                                                    , toScala
                                                    , parentPath
                                                    , priorInstallDirName
                                                    , newInstallDirName)
                log.emit(s"Upgrade completed...successful?  ${if (upgradeOk) true else false}")
            } else {
                log.emit("Migration not required... new installation was selected")
            }
        } else {
            log.emit("The cluster installation has failed")
            sys.exit(1)
        }
    } else {
        log.emit("The cluster environment is not suitable for an installation or upgrade... look at the prior log entries for more information.  Corrections are needed.")
    }
    
    log.emit("Processing is Complete!")
    log.finalize
  }


  def validateClusterEnvironment(log : InstallDriverLog
                                  , apiConfigMap : Map[String,String]
                                  , nodeConfigPath : String
                                  , fromKamanja  : String
                                  , fromScala : String
                                  , toScala : String
                                  , upgrade : Boolean
                                  , install : Boolean
                                  , parentPath : String
                                  , priorInstallDirName : String
                                  , newInstallDirName : String
                                  , ips : Array[String],
                                  , ipIdTargPaths :  Array[(String, String, String, String)] 
                                  , ipPathPairs : Array[(String, String)])
  {
      val proposedClusterEnvironmentIsSuitable  : Boolean = true

      /** Check Java on the cluster */
      /** Check Scala on the cluster */
      /** Check if zookeeper is workable */
      /** Check for suitable kafka environment */
      /** Check the hBase */

      proposedClusterEnvironmentIsSuitable
  }



  def clusterNodes(ips : Array[String]) : String = {
      val buffer : StringBuilder = new StringBuilder
  }


  def mapProperties(log : InstallDriverLog, apiConfigPath : String) : Map[String,String] = {

      val mapPropertyLines : List[String] = Source.fromFile(apiConfigPath).mkString.split('\n').toList
      val mapProperties : Map[String,String] = mapPropertyLines.filter( l => (l != null && l.size > 0 && l.contains("="))).map(line => {
          val key : String = line.head('=').trim.toLowerCase
          val value : String = line.last.trim
          (key,value) 
      }).toMap

      mapProperties
  }

  def extractNodeInfo(log : InstallDriverLog
                    , clusterId : String
                    , apiConfigMap : Map[String,String]
                    , apiConfigPath : String
                    , nodeConfigPath : String
                    , installDir : String
                    , workDir : String) : (Array[String], Array[(String, String, String, String)], Array[(String, String)]) = {

      val ipFile : String = "ip.txt"
      val ipPathPairFile : String = "ipPath.txt"
      val ipIdCfgTargPathQuartetFileName : String = "ipIdCfgTarg.txt"

      val installDir : String = s"$parentPath.$newInstallDirName"

      log.emit("...Command = NodeInfoExtract-1.0 --MetadataAPIConfig \"$metadataAPIConfig\" --NodeConfigPath \"$nodeConfigPath\"  --workDir \"$workDir\" --ipFileName \"$ipFile\" --ipPathPairFileName \"$ipPathPairFile\" --ipIdCfgTargPathQuartetFileName \"$ipIdCfgTargPathQuartetFileName\" --installDir \"$installDir\" --clusterId \"$clusterId\"")

      val extractCmd = Seq("bash", "-c", s"NodeInfoExtract-1.0 --MetadataAPIConfig "$metadataAPIConfig" --NodeConfigPath "$nodeConfigPath" --workDir "$workDir" --ipFileName "$ipFile" --ipPathPairFileName "$ipPathPairFile" --ipIdCfgTargPathQuartetFileName "$ipIdCfgTargPathQuartetFileName"  --installDir "$installDir" --clusterId "$clusterId"")
        log.emit(s"NodeInfoExtract cmd used: $extractCmd")
      val extractCmdRc = Process(extractCmd).!
      val (ips, ipIdTargPaths, ipPathPairs) : (Array[String], Array[(String, String, String, String)], Array[(String, String)]) = if (extractCmdRc != 0) {
          log.emit(s"NodeInfoExtract has failed...rc = $extractCmdRc")
          log.emit(s"Command used: $extractCmd")
          (null,null,null)
      } else {
          reconstituteNodeInfoContent(workDir, ipFile, ipPathPairFile, ipIdCfgTargPathQuartetFileName)
      }

      (ips, ipIdTargPaths, ipPathPairs)
  }

  def reconstituteNodeInfoContent(log : InstallDriverLog, workDir : String, ipFile : String, ipPathPairFile : String, ipIdCfgTargPathQuartetFileName : String) 
                    :  (Array[String], Array[(String, String, String, String)], Array[(String, String)]) = {

      val nodeLines : List[String] = Source.fromFile(s"$workDir/$ipFile").mkString.split('\n').toList
      val nodeList : List[String] = nodeLines.filter( l => (l != null && l.size > 0 && l.contains("="))).map(line => line.trim)
      val ipNodes : Array[String] = nodesList.toArray

      val ipPathPairLines : List[String] = Source.fromFile(s"$workDir/$ipPathPairFile").mkString.split('\n').toList
      val ipTargPathPairs : List[(String,String)] = ipPathPairLines.filter( l => (l != null && l.size > 0 && l.contains(","))).map(line => {
        val pair : Tuple2[String] = line.split(',').map(_ => _.trim)
        pair
      })

      val ipIdCfgTarPathQuartetLines : List[String] = Source.fromFile(s"$workDir/$ipPathPairFile").mkString.split('\n').toList
      val ipIdCfgTarPathQuartets : List[(String,String,String,String)] = ipIdCfgTarPathQuartetLines.filter( l => (l != null && l.size > 0 && l.contains(","))).map(line => {
        val quartet : Tuple4[String] = line.split(',').map(_ => _.trim)
        quartet
      })

      (ipNodes, ipTargPathPairs, ipIdCfgTarPathQuartets)
  }


  def CreateInstallationNames(log : InstallDriverLog, apiConfigMap : Map[String,String]) : (String, String, String) = {

      val (parentPath, priorInstallDirName, newInstallDirName) : (String, String, String) = ("","","")

      (parentPath, priorInstallDirName, newInstallDirName)
  }

  def installCluster(log : InstallDriverLog, apiConfigPath : String, nodeConfigPath : String, parentPath : String, priorInstallDirName : String, newInstallDirName : String) : Boolean = {

      val installCmd = Seq("bash", "-c", s"KamanjaClusterInstall.sh  --MetadataAPIConfig $apiConfigPath, --NodeConfigPath $nodeConfigPath, --ParentPath $parentPath, --PriorInstallDirName $priorInstallDirName, --NewInstallDirName $newInstallDirName --TarballPath $tarballPath")
      log.emit(s"KamanjaClusterInstall cmd used: $installCmd")
      val installCmdRc = Process(installCmd).!
      if (installCmdRc != 0) {
        log.emit(s"KamanjaClusterInstall has failed...rc = $installCmdRc")
        log.emit(s"Command used: $installCmd")
      }
  }

  def doMigration(log : InstallDriverLog
                , apiConfigMap : Map[String,String]
                , nodeConfigPath : String
                , fromKamanja : String
                , fromScala : String
                , toScala : String
                , parentPath : String
                , priorInstallDirName : String
                , newInstallDirName : String) : Boolean = {

      val migrationToBeDone : String = selectMigration(fromKamanja, fromScala, toScala)
      
      val cfgPath : String = "/tmp/migrateCfg.json"
      val upgradeOk : Boolean = migrationToBeDone match {
          case "1.1=>1.3" => {
              val migrateConfigJSON : String = createMigrationConfig("1.1", "1_1", "parentPath", s"$parentPath/$priorInstallDirName, $parentPath/$newInstallDirName")
              writeCfgFile(migrateConfigJSON, cfgPath)
              val migrateCmd = Seq("bash", "-c", s"MigrateManager-1.0 --config $cfgPath")
              log.emit(s"Migrate command used: bash -c MigrateManager-1.0 --config $cfgPath")
              val migrateCmdRc = Process(installCmd).!
              if (migrateCmdRc != 0) {
                  log.emit(s"migration has failed has failed...rc = $migrateCmdRc")
              } else {
                  log.emit("migration was successful")
              }
          }
          case "1.2=>1.3" =>  {
              val migrateConfigJSON : String = createMigrationConfig("1.2", "1_2", "parentPath", s"$parentPath/$priorInstallDirName, $parentPath/$newInstallDirName")
              writeCfgFile(migrateConfigJSON, cfgPath)
              val migrateCmd = Seq("bash", "-c", s"MigrateManager-1.0 --config $cfgPath")
              log.emit(s"Migrate command used: bash -c MigrateManager-1.0 --config $cfgPath")
              val migrateCmdRc = Process(installCmd).!
              if (migrateCmdRc != 0) {
                  log.emit(s"migration has failed has failed...rc = $migrateCmdRc")
              } else {
                  log.emit("migration was successful")
              }
          }
          case _ => 
      }
      val upgradeOk : Boolean = false
      upgradeOk;
  }

  /** 
      Write the supplied configuration string to the supplied path.
      @param cfgString some configuration
      @param filePath the path of the file that will be created.  It should be valid... no checks here.

   */
  private def writeCfgFile(cfgString : String, filePath : String) {
    val file = new File(filePath);
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(cfgString)
    bufferedWriter.close
  }
  


  /** Answer a migration configuration json structure that contains the requisite migration info required for one of the migration/upgrade paths.

      FIXME: This has a hard coded version for the migrationFrom.  Don't we want to substitute that ?????? I am doing it below for now... Pokuri, please fix as appopriate. We need
      to change the lib version as well I think... 

   */
  def createMigrationConfig(log : InstallDriverLog, fromKamanjaVersion : String, libVersion : String, unhandledMetadataDumpDir : String, priorInstallationPath : String, newInstallationPath : String) : String = {

s"{
      \"clusterConfigFile\": \"$newInstallationPath/config/ClusterConfig.json\",
      \"apiConfigFile\": \"$newInstallationPath/config/MetadataAPIConfig.properties\",
      \"unhandledMetadataDumpDir\": \"$unhandledMetadataDumpDir\",
      \"dataSaveThreshold\": 1000,
      \"migratingFrom\": {
        \"version\": \"fromKamanjaVersion\",
        \"versionInstallPath\": \"$priorInstallationPath\",
        \"implemtedClass\": \"com.ligadata.Migrate.MigrateFrom_V_1_1\",
        \"jars\": [
          \"$newInstallationPath/lib/system/migratebase-1.0.jar\",
          \"$newInstallationPath/lib/system/migratefrom_v_${libVersion}_2.10-1.0.jar\",
          \"$priorInstallationPath/bin/KamanjaManager-1.0\"
        ]
      },
      \"migratingTo\": {
        \"version\": \"1.3\",
        \"versionInstallPath\": \"$newInstallationPath\",
        \"implemtedClass\": \"com.ligadata.Migrate.MigrateTo_V_1_3\",
        \"jars\": [
          \"$newInstallationPath/lib/system/migratebase-1.0.jar\",
          \"$newInstallationPath/lib/system/migrateto_v_1_3_2.11-1.0.jar\",
          \"$newInstallationPath/bin/KamanjaManager-1.0\"
        ]
      },
      \"excludeMetadata\": [
        \"ModelDef\",
        \"MessageDef\",
        \"ContainerDef\",
        \"ConfigDef\",
        \"FunctionDef\",
        \"JarDef\",
        \"OutputMsgDef\"
      ],
      \"excludeData\": false
}"
  }



}
