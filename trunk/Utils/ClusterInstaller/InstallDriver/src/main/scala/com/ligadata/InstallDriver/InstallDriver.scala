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

package com.ligadata.InstallDriver

import java.util.Properties

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.sys.process._
import sys.process._

import java.io._
import java.util.regex.{Matcher, Pattern}

import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger;
import org.json4s._

import com.ligadata.InstallDriverBase.InstallDriverBase

import com.ligadata.Utils.Utils
import com.ligadata.Serialize.JsonSerializer

/*
import com.ligadata.Migrate.{Migrate, StatusCallback}
*/

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import com.ligadata.KamanjaVersion.KamanjaVersion

/**
  * This application installs and upgrades Kamanaja.  It does these essential things:

  * 1)  It verifies that a valid environment is available on the cluster nodes specified in the configuration.  The validation includes determination
  * if the nodes mentioned in the node configuration exist, whether they are configured with the apporopriate scala and java, can access a valid
  * zookeeper cluster, can access a valid kafka cluster and can use the hBase kv store configured for this cluster.
  * 2)  The cluster installation script is used to install a new version of the Kamanja release binariesa in the new installation directory on each node.
  * 3)  If an existing cluster installation is being upgraded, the appropriate migration script is invoked.  A migration json file is prepared with the essential
  * details required for this migration.

  * Notes:
  * 1) The prior installation directory, if upgrading, and the new installation directory are determined from the apiConfig property, ROOT_DIR.  To
  * make sure we no where the new and old installations are located (they are sibling directories in the same parent folder), the parent folder is
  * supplied to the cluster install script.
  * 2) The migration script requires a JSON file that contains information about the upgrade.  There are substitution symbols ("macros") in the
  * file that are substituted with the appropriate values, either supplied in the script parameters or from one of the configuation files
  * whose path was supplied.  The substitution symbols have the form """({[A-Za-z0-9_.-]+})""" ... that is, some run 1 or more Alphamerics and punctuation
  * enclosed in {} braces.  For example,
  *
  * "clusterConfigFile": "{NewPackageInstallPath}/config/ClusterConfig.json",
  *
  * See trunk/SampleApplication/clusterInstallerDriver/src/main/resources/MigrateConfigTemplate.json in the github dev repo for an example template.
  */

class InstallDriverLog(val logPath: String) {
  var bufferedWriter = new BufferedWriter(new FileWriter(new File(logPath)))
  var isReady: Boolean = true
  val emptyStrOf5Chars = "     "

  /**
    * open the discrete log file if possible.  If open fails (file must not previously exist) false is returned */
  /**
    * Answer if this object can write messages
    *
    * @return
    */
  def ready: Boolean = {
    isReady
  }

  /**
    * Emit the supplied msg to the buffered FileWriter contained in this InstallDriverLog instance.
    *
    * @param msg a message of importance
    */
  def emit(msg: String, typ: String): Unit = {
    if (ready) {
      val dateTime: DateTime = new DateTime
      val fmt: DateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss.SSS")
      val dateStr: String = fmt.print(dateTime);
      val prntTyp = if (typ != null) (typ + emptyStrOf5Chars).take(5) else emptyStrOf5Chars

      bufferedWriter.write(dateStr + " - " + prntTyp + " - " + msg + "\n")
      bufferedWriter.flush();
    }
  }

  /**
    * Close the buffered FileWriter to flush last buffer and close file.
    */
  def close: Unit = {
    if (bufferedWriter != null)
      bufferedWriter.close
    bufferedWriter = null
    isReady = false
  }
}

class InstallDriver extends InstallDriverBase {
  private lazy val loggerName = this.getClass.getName
  private lazy val logger = LogManager.getLogger(loggerName)
  private var log: InstallDriverLog = null
  private var migratePending = false
  private var migrateConfig = ""

  override def migrationPending: Boolean = migratePending

  override def migrationConfig: String = migrateConfig

  def usage: String = {
    """
Usage:
    java -Dlog4j.configurationFile=file:./log4j2.xml -jar <some path> ClusterInstallerDriver-1.4.0
            /** Mandatory parameters (always) */
            --{upgrade|install}
            --apiConfig <MetadataAPIConfig.properties file>
            --clusterConfig <ClusterConfig.json file>
            --tarballPath <tarball path>
            --toScala <"2.11" or "2.10">
            /** Mandatory parameters iff --upgrade chosen */
            [--fromKamanja "1.1"]
            [--fromScala "2.10"]
            /** Optional parameters */
            [--workingDir <workingdirectory>]
            [--clusterId <id>]
            [--logDir <logDir>]
            [--migrationTemplate <MigrationTemplate>]
            [--skipPrerequisites "scala,java,hbase,kafka,zookeeper,all"]
            [--preRequisitesCheckOnly]
            [--externalJarsDir <external jars directory to be copied to installation lib/application>]
            [--tenantId <a tenantId is applied to all metadata objects>]

    where
        --upgrade explicitly specifies that the intent to upgrade an existing cluster installation with the latest release.
        --install explicitly specifies that the intent that this is to be a new installation.  If there is an existing implementation and
            the --install option is given, the installation will fail.  Similarly, if there is NO existing implementation
            and the --upgrade option has been given, the upgrade will fail. If both --upgrade and --install are given, the install fails.
        --apiConfig <MetadataAPIConfig.properties file> specifies the path of the properties file that (among others) specifies the location
            of the installation on each cluster node given in the cluster configuration.  Note that all installations exist in the same
            location on all nodes.
        --clusterConfig <ClusterConig.json file> gives the file that describes the node information for the cluster, including the
            IP address of each node, et al.
        --tarballPath <tarball path> this is the location of the Kamanja 1.3 tarball of the installation directory to be installed
            on the cluster.  The tarball is copied to each of the cluster nodes, extracted and installed there.
        --toScala which Scala version the engine is to use. Note that this controls the compiler
            version that the Kamanja message compiler, Kamanja pmml compiler, and other future Kamanja components will use when
            building their respective objects. If the requested version has not been installed on the cluster nodes in question,
            the installation will fail.

        [--fromKamanja] optional for install but required for upgrade..."N.N" where "N.N" can be either "1.1" or "1.2" or "1.3"
        [--fromScala "2.10"] an optional parameter that, for the 1.3 InstallDriver, simply documents the version of Scala that
            the current 1.1. or 1.2 is using.  The value "2.10" is the only possible value for this release.

        [--logDir <logDir>] Logs that contain information for both unhandled metadata, and installation and upgrade processing is stored here.
            If this value is not specified, it defaults to /tmp.  It is highly recommended that you specify a more permanent path so that the
            installations you do can be documented in a location not prone to being deleted. By default, the logDir is set to /tmp.
        [--clusterId <id>] describes the key that should be used to extract the cluster metadata from the node configuration.  If no id is
            given by default the name will be "kamanjacluster_1_3_2_2_11".
        [--workingDir <workingdirectory>] a CRUD directory path that should be addressable on each node specified in the cluster configuration
            file as well as on the machine that this program is executing.  It is used by this program and scripts it invokes to create
            intermediate files used during the installation process. If not specified, /tmp/work is used.
       [--migrationTemplate <MigrationTemplate>] A migration template file is required
            when a migration is to be performed.  If one is not given on the command line, the migration template found in the sibling directory, config,
            called MigrateConfig_template.json is used.  This file has several configurable values as well as a number of values that are automatically
            filled with information gleaned from other files in this list.  See the [cluster installation page <web page addr>] for more information
        [--skipPrerequisites "scala,java,hbase,kafka,zookeeper,all"] if specified, the check for those core requirements will be skipped.  This is a bit
            risky unless you are confident your cluster is well appointed with the required support software.
        [--preRequisitesCheckOnly] When specified, the prerequisite software components are checked, but the installation and possible migration are not done.
            If both --skipPrerequisites and --preRequisitesOnly are specified, only the prerequisites not given in the skip list will be performed.
            Processing stops after the checks; installation and upgrade are not done.
        [--externalJarsDir <external jars directory to be copied to installation lib/application] External jars to be copied while installing/upgrading new package.
        [--tenantId <Tenant Id to be applied to all the meta data objects being migrated.

    The ClusterInstallerDriver-1.4.0 is the cluster installer driver for Kamanja 1.3.  It is capable of installing a new version of 1.3
    or given the appropriate arguments, installing a new version of Kamanja 1.3 *and* upgrading a 1.1 or 1.2 installation to the 1.3 version.

    A log of the installation and optional upgrade is collected in a log file.  This log file is automatically generated and will be found in the
    logDir you supply to the installer driver program.  The name will be of the form: InstallDriver.yyyyMMdd_HHmmss.log (e.g., InstallDriver.20160201_231101.log)  Should issues be encountered (missing components, connectivity issues, etc.) this log should be consulted as to how to proceed.

    Using the content of the log as a guide, the administrator will see what fixes must be made to their environment to push on and complete the install.  It will also be the guide for backing off/abandoning an upgrade so a prior Kamanja cluster
    can be restored.

    """
  }

  override def closeLog(): Unit = {
    if (log != null)
      log.close
    log = null
  }

  private def openLog(fl: String): Unit = {
    if (log != null)
      log.close
    log = new InstallDriverLog(fl)
  }

  private def print(msg: String, typ: String, log: InstallDriverLog = null, printToConsole: Boolean = true): Unit = {
    typ.toUpperCase match {
      case "ERROR" => {
        logger.error(msg);
        if (printToConsole && !logger.isErrorEnabled())
          println(msg)
        if (log != null)
          log.emit(msg, typ)
      }
      case "WARN" => {
        logger.warn(msg);
        if (printToConsole && !logger.isWarnEnabled())
          println(msg)
        if (log != null)
          log.emit(msg, typ)
      }
      case "INFO" => {
        logger.info(msg);
        if (printToConsole && !logger.isInfoEnabled())
          println(msg)
        if (log != null)
          log.emit(msg, typ)
      }
      case _ => {
        logger.debug(msg);
        if (printToConsole && !logger.isDebugEnabled())
          println(msg)
        if (log != null)
          log.emit(msg, typ)
      }
    }
  }

  private def printAndLogDebug(msg: String, log: InstallDriverLog = null, printToConsole: Boolean = true): Unit = {
    logger.debug(msg);
    if (printToConsole && !logger.isDebugEnabled())
      println(msg)
    if (log != null)
      log.emit(msg, "DEBUG")
  }

  private def printAndLogError(msg: String, log: InstallDriverLog = null, printToConsole: Boolean = true, e: Throwable = null): Unit = {
    logger.error(msg);
    if (printToConsole)
      println(msg) // This may go out twice if user set the logger to console
    if (log != null)
      log.emit(msg, "ERROR")
  }

  override def run(args: Array[String]): Unit = {
    if (args.length == 0) {
      printAndLogError("No arguments provided", log);
      printAndLogDebug(usage, log);
      closeLog
      sys.exit(1)
    }

    // locate the clusterInstallerDriver app ... need its working directory to refer to others... this function
    // returns this form:  file:/tmp/drdigital/KamanjaInstall-1.3.3_2.11/bin/clusterInstallerDriver-1.4.0

    /** Obtain location of the clusterInstallerDriver fat jar.  Its directory contains the scripts we use to
      * obtain component info for the env check and the lower level cluster install script that actually does the
      * install.
      */
    var thisFatJarsLocationAbs: String = ""
    var clusterInstallerDriversLocation: String = ""
    try {
      val fl = new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
      thisFatJarsLocationAbs = fl.getAbsolutePath
      clusterInstallerDriversLocation = new File(fl.getParent).getAbsolutePath
    }
    catch {
      case e: Exception => {
        logger.error("Failed to get InstallerDriver Jar Absolute Path", e)
        println("Failed to get InstallerDriver Jar Absolute Path")
        System.exit(1)
      }
    }

    logger.info("Jar Absolute Path:" + thisFatJarsLocationAbs + ", InstallerDriversLocation:" + clusterInstallerDriversLocation)
    println("Jar Absolute Path:" + thisFatJarsLocationAbs + ", InstallerDriversLocation:" + clusterInstallerDriversLocation)

    val arglist = args.toList
    type OptionMap = Map[Symbol, String]

    //printAndLogDebug(s"arguments supplied are:\n $arglist")
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
        case "--migrationTemplate" :: value :: tail =>
          nextOption(map ++ Map('migrationTemplate -> value), tail)
        case "--logDir" :: value :: tail =>
          nextOption(map ++ Map('logDir -> value), tail)
        case "--upgrade" :: tail =>
          nextOption(map ++ Map('upgrade -> "true"), tail)
        case "--install" :: tail =>
          nextOption(map ++ Map('install -> "true"), tail)
        case "--preRequisitesCheckOnly" :: tail =>
          nextOption(map ++ Map('preRequisitesCheckOnly -> "true"), tail)
        case "--skipPrerequisites" :: value :: tail =>
          nextOption(map ++ Map('skipPrerequisites -> value), tail)
        case "--externalJarsDir" :: value :: tail =>
          nextOption(map ++ Map('externalJarsDir -> value), tail)
        case "--tenantId" :: value :: tail =>
          nextOption(map ++ Map('tenantId -> value), tail)
        case "--version" :: tail =>
          nextOption(map ++ Map('version -> "true"), tail)
        case option :: tail =>
          printAndLogError("Unknown option " + option, log)
          printAndLogDebug(usage, log);
          closeLog
          sys.exit(1)
      }
    }

    val options = nextOption(Map(), arglist)
    val version = options.getOrElse('version, "false").toString
    if (version.equalsIgnoreCase("true")) {
      KamanjaVersion.print
      return
    }

    // 1st Set of options
    // Mandatory options
    val apiConfigPath: String = if (options.contains('apiConfig)) options.apply('apiConfig) else null
    val nodeConfigPath: String = if (options.contains('clusterConfig)) options.apply('clusterConfig) else null
    val tarballPath: String = if (options.contains('tarballPath)) options.apply('tarballPath) else null
    val toScala: String = if (options.contains('toScala)) options.apply('toScala) else "" // FIXME: Insist to have this
    val upgrade: Boolean = if (options.contains('upgrade)) options.apply('upgrade) == "true" else false
    val install: Boolean = if (options.contains('install)) options.apply('install) == "true" else false

    // 2nd Set of Options
    // Mandatory for Upgrade
    val fromKamanja: String = if (options.contains('fromKamanja)) options.apply('fromKamanja) else null
    val fromScala: String = if (options.contains('fromScala)) options.apply('fromScala) else null

    // 3rd Set of Options
    // Optional
    val clusterId_opt: String = if (options.contains('clusterId)) options.apply('clusterId) else null
    val workingDir: String = (if (options.contains('workingDir)) options.apply('workingDir) else "/tmp").trim // Default /tmp if not given
    val logDir: String = (if (options.contains('logDir)) options.apply('logDir) else "/tmp").trim // Default /tmp if not given
    val migrateTemplate_opt: String = if (options.contains('migrationTemplate)) options.apply('migrationTemplate) else null
    val skipPrerequisites_opt: String = if (options.contains('skipPrerequisites)) options.apply('skipPrerequisites) else null
    val preRequisitesCheckOnly: Boolean = if (options.contains('preRequisitesCheckOnly)) options.apply('preRequisitesCheckOnly) == "true" else false
    val externalJarsDir_opt: String = if (options.contains('externalJarsDir)) options.apply('externalJarsDir) else null
    val tenantId_opt: String = if (options.contains('tenantId)) options.apply('tenantId) else null

    val toKamanja: String = "1.4"

    // Check whether logDir is valid or not
    if (!isFileExists(logDir, false, true)) {
      // If logDir does not exists
      printAndLogError(s"logDir:$logDir is not valid path", log)
      printAndLogDebug(usage, log);
      closeLog
      sys.exit(1)
    }

    /** make a log ... Warning... if logDir not supplied, logDir defaults to /tmp */
    val dateTime: DateTime = new DateTime
    val flfmt: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMdd_HHmmss")
    val datestr: String = flfmt.print(dateTime);

    val installDriverLogFl = s"$logDir/InstallDriver.$datestr.log"
    openLog(installDriverLogFl)
    printAndLogDebug(s"The installation log file can be found in $installDriverLogFl")

    val hasBoth = (upgrade && install)
    val hasNone = (!upgrade && !install)

    val bothTxt = "\nYou have specified both 'upgrade' and 'install' as your action.  It must be one or the other.\n"
    val noneTxt = "\nYou have not specified either 'upgrade' or 'install' as your action.  It must be one or the other.\n"

    val commonTxt =
      """
Use 'upgrade' if you have an existing Kamanja installation and wish to install the new version of the
software and upgrade your application metadata to the new system.
Use 'install' if the desire is to simply create a new installation.  However, if the 'install'
detects an existing installation, the installation will fail.
Try again.

      """

    if (hasBoth || hasNone) {
      if (hasBoth) {
        printAndLogError(bothTxt + commonTxt, log)
      }
      if (hasNone) {
        printAndLogError(noneTxt + commonTxt, log)
      }
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    val operation = if (install) "Installing" else if (upgrade) "Upgrading"

    printAndLogDebug(s"Given Arguments:$operation with clusterId:$clusterId_opt, apiConfigPath:$apiConfigPath, nodeConfigPath:$nodeConfigPath, tarballPath:$tarballPath, fromKamanja:$fromKamanja, fromScala:$fromScala, toScala:$toScala, workingDir:$workingDir, migrateTemplate:$migrateTemplate_opt, logDir:$logDir, toKamanja:$toKamanja, skipPrerequisites:$skipPrerequisites_opt, preRequisitesCheckOnly:$preRequisitesCheckOnly")

    val apiConfigPathOk: Boolean = apiConfigPath != null && apiConfigPath.nonEmpty
    val nodeConfigPathOk: Boolean = apiConfigPath != null && apiConfigPath.nonEmpty
    val tarballPathOk: Boolean = tarballPath != null && tarballPath.nonEmpty
    val fromKamanjaOk: Boolean = install || (upgrade && fromKamanja != null && fromKamanja.nonEmpty && (fromKamanja == "1.1" || fromKamanja == "1.2") || fromKamanja == "1.3" )
    val fromScalaOk: Boolean = install || (upgrade && fromScala != null && fromScala.nonEmpty && (fromScala == "2.10" || fromScala == "2.11"))
    val toScalaOk: Boolean = (toScala != null && toScala.nonEmpty && (toScala == "2.10" || toScala == "2.11"))
    val workingDirOk: Boolean = workingDir != null && workingDir.nonEmpty
    val logDirOk: Boolean = logDir != null && logDir.nonEmpty
    val reasonableArguments: Boolean =
      if (install) (apiConfigPathOk
        && nodeConfigPathOk
        && tarballPathOk
        && toScalaOk
        && workingDirOk
        && logDirOk
        )
      else
        (apiConfigPathOk
          && nodeConfigPathOk
          && tarballPathOk
          && fromKamanjaOk
          && fromScalaOk
          && toScalaOk
          && workingDirOk
          && logDirOk
          )

    if (!reasonableArguments) {
      val installOrUpgrade: String = if (upgrade) "your upgrade" else "your install"
      printAndLogError(s"One or more arguments for $installOrUpgrade are not set or have bad values...", log)
      if (!apiConfigPathOk) printAndLogError("\tapiConfigPath", log)
      if (!nodeConfigPathOk) printAndLogError("\t--apiConfigPath <path to the metadata api properties file that contains the ROOT_DIR property location>", log)
      if (!tarballPathOk) printAndLogError("\t--tarballPath <location of the prepared 1.3 installation tarball to be installed>", log)
      if (upgrade && !fromKamanjaOk) printAndLogError("\t--fromKamanja <the prior installation version being upgraded... either '1.1' or '1.2'>", log)
      if (upgrade && !fromScalaOk) printAndLogError("\t--fromScala <either scala version '2.10' or '2.11'", log)
      if (!logDirOk) printAndLogError("\t--logDir <the directory path where the Cluster logs (InstallDriver.yyyyMMdd_HHmmss.log) is to be written ", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    var majorVer = KamanjaVersion.getMajorVersion
    var minVer = KamanjaVersion.getMinorVersion
    var microVer = KamanjaVersion.getMicroVersion
    val scalaVersionFull = scala.util.Properties.versionNumberString
    val scalaVersion = scalaVersionFull.substring(0, scalaVersionFull.lastIndexOf('.'))

    val componentVersionScriptAbsolutePath = s"$clusterInstallerDriversLocation/GetComponentsVersions.sh"
    val componentVersionJarAbsolutePath = s"$clusterInstallerDriversLocation/GetComponent_${scalaVersion}-${majorVer}.${minVer}.${microVer}.jar"
    val kamanjaClusterInstallPath = s"$clusterInstallerDriversLocation/KamanjaClusterInstall.sh"

    var cnt: Int = 0

    /** validate the paths before proceeding */
    if (!isFileExists(componentVersionScriptAbsolutePath, true)) {
      printAndLogError("GetComponentsVersions.sh script is not installed in path " + clusterInstallerDriversLocation, log)
      cnt += 1
    }

    if (!isFileExists(componentVersionJarAbsolutePath, true)) {
      printAndLogError("GetComponent_2.11-1.4.0.jar is not installed in path " + clusterInstallerDriversLocation, log)
      cnt += 1
    }

    if (!isFileExists(kamanjaClusterInstallPath, true)) {
      printAndLogError("KamanjaClusterInstall.sh script is not installed in path " + clusterInstallerDriversLocation, log)
      cnt += 1
    }

    if (!isFileExists(apiConfigPath, true)) {
      printAndLogError(s"The apiConfigPath ($apiConfigPath) does not exist", log)
      cnt += 1
    }

    if (!isFileExists(nodeConfigPath, true)) {
      printAndLogError(s"The nodeConfigPath ($nodeConfigPath) does not exist", log)
      cnt += 1
    }

    if (!isFileExists(tarballPath, true)) {
      printAndLogError(s"The tarballPath ($tarballPath) does not exist", log)
      cnt += 1
    }

    if (!isFileExists(workingDir, false, true)) {
      printAndLogError(s"The workingDir ($workingDir) does not exist", log)
      cnt += 1
    }

    if (externalJarsDir_opt != null && !isFileExists(externalJarsDir_opt, false, true)) {
      printAndLogError(s"The externalJarsDir ($externalJarsDir_opt) does not exist", log)
      cnt += 1
    }

    var migrateTemplate: String = null
    if (upgrade) {
      var givenTemplate = false
      migrateTemplate = if (migrateTemplate_opt != null && migrateTemplate_opt.nonEmpty) {
        givenTemplate = true
        migrateTemplate_opt.trim
      } else {
        s"$clusterInstallerDriversLocation/MigrateConfig_template.json"
      }

      if (!isFileExists(migrateTemplate, true)) {
        if (givenTemplate)
          printAndLogError(s"Given migrateTemplate $migrateTemplate is not valid file", log)
        else
          printAndLogError(s"MigrateConfig_template.json is not installed in path ${clusterInstallerDriversLocation}", log)
        cnt += 1
      }

      // Validate all arguments
      var validMigrationPaths : scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()
      validMigrationPaths.add("1.1 => 1.4") 
      validMigrationPaths.add("1.2 => 1.4") 
      validMigrationPaths.add("1.3 => 1.4") 

      if ( ! validMigrationPaths.contains(fromKamanja + " => " + toKamanja) ) {
        printAndLogError(s"The upgrade path ($fromKamanja => $toKamanja) is not valid with this release... ", log)
        cnt += 1
      }
    }

    if (cnt > 0) {
      printAndLogError(s"Installation is aborted. Consult the log file (${log.logPath}) for details.", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    /** Convert the content of the property file into a map.  If the path is bad, an empty map is returned and processing stops */
    val apiConfigMap: Properties = mapProperties(log, apiConfigPath)
    if (apiConfigMap == null || apiConfigMap.isEmpty) {
      printAndLogError("The configuration file is messed up... it needs to be lines of key=value pairs", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    /** Ascertain what the name of the new installation directory will be, what the name of the prior installation would be
      * (post install), and the parent directory in which both of them will live on each cluster node. Should there be no
      * existing installation, the prior installation value will be null. */
    val tmpRootDirPath: String = apiConfigMap.getProperty("root_dir").trim

    /** use root dir value for the base name */
    if (tmpRootDirPath == null || tmpRootDirPath.size == 0) {
      printAndLogError("Found ROOT_DIR as empty", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    val lastChar = tmpRootDirPath.charAt(tmpRootDirPath.size - 1)

    // Trim / at the end if we have it
    val rootDirPath = if (lastChar == '/' || lastChar == '\\') tmpRootDirPath.substring(0, tmpRootDirPath.size - 2) else tmpRootDirPath
    val (parentPath, priorInstallDirName, newInstallDirName): (String, String, String) = CreateInstallationNames(log
      , rootDirPath
      , fromKamanja
      , toKamanja)

    // FIXME: Note that this check must be done on each cluster node.  This work is implemented in the validateClusterEnvironment method below.
    // Check priorInstallDirName is link or dir. If it is DIR take an action (renaming on all nodes) and LOG the task done.
    // IF priorInstallDirName is link, unlink it. But log the Actual dir & link to make sure for Reverting the installation.

    /** Run the node info extract on the supplied file and garner all of the information needed to conduct the cluster environment validatation */
    val installDir: String = s"$parentPath/$newInstallDirName"

    val clusterConfig: String = Source.fromFile(nodeConfigPath).mkString
    var clusterConfigMap: ClusterConfigMap = null
    try {
      clusterConfigMap = new ClusterConfigMap(clusterConfig, clusterId_opt)
    } catch {
      case e: Exception => {
        printAndLogError("Failed to extract cluster information. ErrorMessage:" + e.getMessage, log, true, e)
        printAndLogDebug(usage, log)
        closeLog
        sys.exit(1)
      }
      case e: Throwable => {
        printAndLogError("Failed to extract cluster information. ErrorMessage:" + e.getMessage, log, true, e)
        printAndLogDebug(usage, log)
        closeLog
        sys.exit(1)
      }
    }

    val clusterMap: Map[String, Any] = clusterConfigMap.ClusterMap
    var clusterId = clusterConfigMap.clusterIdOfInterest
    if (clusterMap == null || clusterMap.isEmpty) {
      if (clusterId != null)
        printAndLogError(s"There is no cluster info for the supplied clusterId, $clusterId", log)
      else
        printAndLogError(s"There is no cluster info found", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    printAndLogDebug("Processing cluster information for clusterId:" + clusterId, log)

    /** Create the cluster config map and pull out the top level objects... either Maps or Lists of Maps */
    val clusterIdFromConfig: String = clusterConfigMap.ClusterId
    val dataStore: Map[String, Any] = clusterConfigMap.DataStore
    val zooKeeperInfo: Map[String, Any] = clusterConfigMap.ZooKeeperInfo
    val environmentContext: Map[String, Any] = clusterConfigMap.EnvironmentContext
    val clusterNodes: List[Map[String, Any]] = clusterConfigMap.ClusterNodes
    val adapters: List[Map[String, Any]] = clusterConfigMap.Adapters

    /* Collect the node information needed to valididate the implied cluster environment and serve as basic
     * parameters for the new cluster installation */
    val (ips, ipIdTargPaths, ipPathPairs): (Array[String], Array[(String, String, String, String)], Array[(String, String)]) =
      collectNodeInfo(log, clusterId, clusterNodes, installDir, rootDirPath)

    /** Validate that the proposed installation has the requisite characteristics (scala, java, zookeeper, kafka, and hbase) */
    val shouldUpgrade: Boolean = upgrade
    val (proposedClusterEnvironmentIsSuitable, physicalRootDir): (Boolean, String) =
      validateClusterEnvironment(log
        , clusterInstallerDriversLocation
        , clusterConfigMap
        , componentVersionScriptAbsolutePath
        , componentVersionJarAbsolutePath
        , apiConfigMap
        , nodeConfigPath
        , fromKamanja
        , fromScala
        , toScala
        , shouldUpgrade
        , parentPath
        , priorInstallDirName
        , newInstallDirName
        , ips, ipIdTargPaths, ipPathPairs, skipPrerequisites_opt)

    if (upgrade && (physicalRootDir == null || physicalRootDir.isEmpty)) {
      printAndLogError(s"For upgrade, not found valid directory/link at $rootDirPath on any node.", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    } else if (install && physicalRootDir != null && !physicalRootDir.isEmpty) {
      printAndLogError(s"For fresh install, found valid directory/link at $rootDirPath at least on one node.", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    if (!preRequisitesCheckOnly) {
      if (proposedClusterEnvironmentIsSuitable) {
        /** if so... */

        val metadataDataStore: String =
          if (apiConfigMap.getProperty("METADATA_DATASTORE".toLowerCase()) != null) {
            apiConfigMap.getProperty("METADATA_DATASTORE".toLowerCase())
          } else if (apiConfigMap.getProperty("MetadataDataStore".toLowerCase()) != null) {
            apiConfigMap.getProperty("MetadataDataStore".toLowerCase())
          } else {
            val dbType = apiConfigMap.getProperty("DATABASE".toLowerCase())
            val dbHost = if (apiConfigMap.getProperty("DATABASE_HOST".toLowerCase()) != null) apiConfigMap.getProperty("DATABASE_HOST".toLowerCase()) else apiConfigMap.getProperty("DATABASE_LOCATION".toLowerCase())
            val dbSchema = apiConfigMap.getProperty("DATABASE_SCHEMA".toLowerCase())
            val dbAdapterSpecific = apiConfigMap.getProperty("ADAPTER_SPECIFIC_CONFIG".toLowerCase())

            val dbType1 = if (dbType == null) "" else dbType.trim
            val dbHost1 = if (dbHost == null) "" else dbHost.trim
            val dbSchema1 = if (dbSchema == null) "" else dbSchema.trim

            val jsonStr =
              if (dbAdapterSpecific != null) {
                val json = ("StoreType" -> dbType1) ~
                  ("SchemaName" -> dbSchema1) ~
                  ("Location" -> dbHost1) ~
                  ("AdapterSpecificConfig" -> dbAdapterSpecific)
                pretty(render(json))
              } else {
                val json = ("StoreType" -> dbType1) ~
                  ("SchemaName" -> dbSchema1) ~
                  ("Location" -> dbHost1)
                pretty(render(json))
              }
            jsonStr
          }

        val externalJarsDir =
          if (externalJarsDir_opt != null) {
            val fl = new File(externalJarsDir_opt)
            fl.getAbsolutePath
          } else {
            ""
          }

        val tenantId = if (tenantId_opt == null) {""} else{ tenantId_opt }
	logger.info("tenantId => " + tenantId)
	

        /** Install the new installation */
        val nodes: String = ips.mkString(",")
        printAndLogDebug(s"Begin cluster installation... installation found on each cluster node(any {$nodes}) at $installDir", log)
        val installOk: Boolean = installCluster(log
          , kamanjaClusterInstallPath
          , rootDirPath
          , apiConfigPath
          , nodeConfigPath
          , priorInstallDirName
          , newInstallDirName
          , tarballPath
          , ips
          , ipIdTargPaths
          , ipPathPairs
          , workingDir
          , clusterId
          , metadataDataStore
          , externalJarsDir
          , tenantId)
        if (installOk) {
          /** Do upgrade if necessary */
          if (upgrade) {
            printAndLogDebug(s"Upgrade required... upgrade from version $fromKamanja", log)
            val migratePreparationOk: Boolean = prepareForMigration(log
              , apiConfigPath
              , apiConfigMap
              , nodeConfigPath
              , migrateTemplate
              , logDir
              , fromKamanja
              , fromScala
              , toScala
              , parentPath
              , priorInstallDirName
              , newInstallDirName
              , physicalRootDir
              , rootDirPath
              , tenantId)
            printAndLogDebug("Migration preparation " + (if (migratePreparationOk) "Succeeded" else "Failed"), log)
            if (!migratePreparationOk) {
              printAndLogError(s"Some thing failed to prepare migration configuration. The parameters for the migration may be incorrect... aborting installation", log)
              printAndLogDebug(usage, log)
              closeLog
              sys.exit(1)
            }
          } else {
            printAndLogDebug("Migration not required... new installation was selected", log)
          }
        } else {
          printAndLogError("The cluster installation has failed", log)
          printAndLogDebug(usage, log)
          closeLog
          sys.exit(1)
        }
      } else {
        printAndLogError("The cluster environment is not suitable for an installation or upgrade... look at the prior log entries for more information.  Corrections are needed.", log)
        printAndLogDebug(usage, log)
        closeLog
        sys.exit(1)
      }

      if (!migrationPending) {
        printAndLogDebug("Processing is Complete!", log)
        closeLog
      }
    } else {
      printAndLogDebug("Requested to check preRequisitesCheckOnly.\nProcessing is Complete!", log)
      closeLog
    }
  }

  /**
    *
    * The scala version on each cluster node is tested against the toScala version supplied as a parameter value here.
    * The java must be 1.7x or 1.8x.
    * The zk, kafka and hbase are tested against their status.  The test is assumed to be that the client protocols can
    * access their respective servers.
    * There is a test of the physical root dirs returned.  They should all be the same path (or so I think).
    *
    * @param log                                the InstallDriverLog instance that records important processing events and information needed if mal configurations detected
    * @param clusterInstallerDriversLocation    the location of the script that calls GetComponent
    * @param componentVersionScriptAbsolutePath the GetComponent script that invokes the GetComponent program on each node
    * @param componentVersionJarAbsolutePath    the GetComponent tarball that is shipped to each cluster node
    * @param apiConfigMap                       the metadata api properties (including the ROOT_DIR
    * @param nodeConfigPath                     the node configuration file for the cluster
    * @param fromKamanja                        the version of an existing Kamanja installation that is present on this cluster (if any)
    * @param fromScala                          the version of Scala used on the existing cluster (if any)
    * @param toScala                            the version desired of Scala desired on the new installation
    * @param upgrade                            the user's intent that this is to be an upgrade else it is new install
    * @param priorInstallDirName                the name of the prior installation name
    * @param newInstallDirName                  the new installation name
    * @param ips                                the ip addresses of the nodes participating in this cluster
    * @param ipIdTargPaths                      a quartet (ip, node id, install path, node roles)
    * @param ipPathPairs                        an array of pair (ip, install path)
    * @return true if the environment is acceptable
    *
    *         NOTE: Some of the parameters may be eliminated if they can be conclusively ruled out as being
    *         worthy of inspection or testing for this cluster environment worthiness check.
    */

  def validateClusterEnvironment(log: InstallDriverLog
                                 , clusterInstallerDriversLocation: String
                                 , clusterConfigMap: ClusterConfigMap
                                 , componentVersionScriptAbsolutePath: String
                                 , componentVersionJarAbsolutePath: String
                                 , apiConfigMap: Properties
                                 , nodeConfigPath: String
                                 , fromKamanja: String
                                 , fromScala: String
                                 , toScala: String
                                 , upgrade: Boolean
                                 , parentPath: String
                                 , priorInstallDirName: String
                                 , newInstallDirName: String
                                 , ips: Array[String]
                                 , ipIdTargPaths: Array[(String, String, String, String)]
                                 , ipPathPairs: Array[(String, String)]
                                 , skipPrerequisites_opt: String): (Boolean, String) = {

    var phyDirLast: String = null
    val rootDirPath: String = apiConfigMap.getProperty("root_dir")

    val (proposedClusterEnvironmentIsSuitable): Boolean = try {
      val hbaseConnections: String = clusterConfigMap.DataStoreConnections.trim //
      val kafkaConnections: String = clusterConfigMap.KafkaConnections.trim
      val zkConnections: String = clusterConfigMap.ZooKeeperConnectionString.trim

      var skipComponents =
        if (skipPrerequisites_opt != null && skipPrerequisites_opt.nonEmpty)
          skipPrerequisites_opt.split(",").map(s => s.trim.toLowerCase()).filter(s => s.size > 0).toSet
        else
          Set[String]()
      var skipedAll = false

      val jsonParm: String =
        if (!skipComponents.contains("all")) {
          val sb = new StringBuilder()
          sb.append("[")
          var addedComp = 0
          if (!skipComponents.contains("zookeeper") && zkConnections.nonEmpty) {
            if (addedComp > 0)
              sb.append(",")
            sb.append("""{ "component" : "zookeeper", "hostslist" : "%s" }""".stripMargin.format(zkConnections))
            addedComp += 1
          }
          if (!skipComponents.contains("kafka") && kafkaConnections.nonEmpty) {
            if (addedComp > 0)
              sb.append(",")
            sb.append("""{ "component" : "kafka", "hostslist" : "%s" }""".stripMargin.format(kafkaConnections))
            addedComp += 1
          }
          if ((!skipComponents.contains("hbase")) && (!skipComponents.contains("storage")) && hbaseConnections.nonEmpty) {
            if (addedComp > 0)
              sb.append(",")
            sb.append(hbaseConnections)
            addedComp += 1
          }
          if (!skipComponents.contains("scala")) {
            if (addedComp > 0)
              sb.append(",")
            sb.append("""{ "component" : "scala", "hostslist" : "localhost" }""")
            addedComp += 1
          }
          if (!skipComponents.contains("java")) {
            if (addedComp > 0)
              sb.append(",")
            sb.append("""{ "component" : "java", "hostslist" : "localhost" }""")
            addedComp += 1
          }
          if (addedComp > 0) {
            sb.append("]")
            sb.toString()
          }
          else {
            skipedAll = true
            "[]"
          }
        } else {
          skipedAll = true
          "[]"
        }

      /**
        * Obtain the component information found on the cluster nodes.  There is a ComponentInfo created for each
        * type of information sought... namely one for scala version, java version, zookeeper, kafka and hbase. For the
        * entire cluster, this is an array of this.
        */
      val componentResults: Array[(Array[ComponentInfo], String)] = ips.map(ip => {
        val resultsFileAbsolutePath: String = s"/tmp/componentResultsFile$ip.txt"
        val (componentInfos, physicalRootDir): (Array[ComponentInfo], String) =
          getComponentVersions(log
            , rootDirPath
            , componentVersionScriptAbsolutePath
            , componentVersionJarAbsolutePath
            , ip
            , resultsFileAbsolutePath
            , jsonParm, skipedAll)

        if (physicalRootDir != null && physicalRootDir.length > 0) phyDirLast = physicalRootDir
        (componentInfos, physicalRootDir)
      })

      if (skipedAll) {
        val uniquePhysDirs: Set[String] = componentResults.map(pair => pair._2).filter(p => p != null).toSet
        if (uniquePhysDirs.size != 1)
          printAndLogError(s"Found different targetPaths ($uniquePhysDirs) on nodes. So it is not same path on all nodes", log)
        (uniquePhysDirs.size == 1)
      } else {
        /** Let's look at the component results. comparing with expectations
          * case class ComponentInfo(version: String, status: String, errorMessage: String, componentName: String, invocationNode: String)
          */
        val scalaJavaScores: Array[(Boolean, Boolean)] = componentResults.map(pair => {
          var scalaIsValid = false
          var javaIsValid = false
          val (components, physDir): (Array[ComponentInfo], String) = pair
          if (!skipComponents.contains("scala")) {
            val optInfo: Option[ComponentInfo] = components.filter(component => component.componentName.toLowerCase == "scala").headOption
            val info = optInfo.orNull
            scalaIsValid = (info != null && info.version != null && info.version.startsWith(toScala))
            if (!scalaIsValid) {
              if (info != null) {
                printAndLogError(s"Scala for ip ${info.invocationNode} is not same as $toScala or invalid... msg=${info.errorMessage}", log)
              } else {
                printAndLogError("Incredible... no scala info", log)
              }
            }
          } else {
            scalaIsValid = true
          }

          if (!skipComponents.contains("java")) {
            val joptInfo: Option[ComponentInfo] = components.filter(component => component.componentName.toLowerCase == "java").headOption
            val jinfo = joptInfo.orNull
            javaIsValid = (jinfo != null && jinfo.version != null && (jinfo.version.startsWith("1.7") || jinfo.version.startsWith("1.8")))
            if (!javaIsValid) {
              if (jinfo != null) {
                printAndLogError(s"Java for ip ${jinfo.invocationNode} is invalid...must be java 1.7 or java 1.8 msg=${jinfo.errorMessage}", log)
              } else {
                printAndLogError("Incredible... no java info", log)
              }
            }
          } else {
            javaIsValid = true
          }

          (scalaIsValid, javaIsValid)
        })
        val scalaAndJavaAreValidAllNodes: Boolean = scalaJavaScores.filter(langScores => {
          val (scala, java): (Boolean, Boolean) = langScores
          (scala && java)
        }).size == componentResults.size

        /** Test if the zookeeper, kafka and hbase are healthy and suitable for use
          * FIXME: For this test, we assume the status is "ok" when the kafka can be used from the corresponding ip node */
        val zkKafkaHbaseHealthCheck: Array[(Boolean, Boolean, Boolean)] = componentResults.map(pair => {
          val (components, physDir): (Array[ComponentInfo], String) = pair
          var zkIsValid = false
          var kafkaIsValid = false
          var hbaseIsValid = false

          if (!skipComponents.contains("zookeeper") && zkConnections.nonEmpty) {
            val zkOptInfo: Option[ComponentInfo] = components.filter(component => component.componentName.toLowerCase == "zookeeper").headOption
            val info = zkOptInfo.orNull
            zkIsValid = (info != null && info.status != null && info.status.toLowerCase == "success")
            if (!zkIsValid) {
              if (info != null) {
                printAndLogError(s"Zookeeper for ip ${info.invocationNode} is not healthy... msg=${info.errorMessage}", log)
              } else {
                printAndLogError("Incredible... no zookeeper info", log)
              }
            }
          } else {
            zkIsValid = true
          }

          if (!skipComponents.contains("kafka") && kafkaConnections.nonEmpty) {
            val kafkaOptInfo: Option[ComponentInfo] = components.filter(component => component.componentName.toLowerCase == "kafka").headOption
            val kinfo = kafkaOptInfo.orNull
            kafkaIsValid = (kinfo != null && kinfo.status != null && kinfo.status.toLowerCase == "success")
            if (!kafkaIsValid) {
              if (kinfo != null) {
                printAndLogError(s"Kafka for ip ${kinfo.invocationNode} is not healthy... msg=${kinfo.errorMessage}", log)
              } else {
                printAndLogError("Incredible... no kafka info", log)
              }
            }
          } else {
            kafkaIsValid = true
          }

          if (!skipComponents.contains("hbase") && hbaseConnections.nonEmpty) {
            val hbaseOptInfo: Option[ComponentInfo] = components.filter(component => component.componentName.toLowerCase == "hbase").headOption
            val hinfo = hbaseOptInfo.orNull
            hbaseIsValid = (hinfo != null && hinfo.status != null && hinfo.status.toLowerCase == "success")
            if (!hbaseIsValid) {
              if (hinfo != null) {
                printAndLogError(s"HBase for ip ${hinfo.invocationNode} is not healthy... msg=${hinfo.errorMessage}", log)
              } else {
                printAndLogError("Incredible... no hbase info", log)
              }
            }
          } else {
            hbaseIsValid = true
          }

          (zkIsValid, kafkaIsValid, hbaseIsValid)
        })

        val zkKafkaHbaseHealthyForAllNodes: Boolean = zkKafkaHbaseHealthCheck.filter(healthTriple => {
          val (zk, kafka, hbase): (Boolean, Boolean, Boolean) = healthTriple
          (zk && kafka && hbase)
        }).size == componentResults.size

        /** Examine the physical directories on each cluster node. The physical directory returned for all of them
          * should be the same path, right?
          */
        val uniquePhysDirs: Set[String] = componentResults.map(pair => pair._2).filter(p => p != null).toSet
        if (uniquePhysDirs.size != 1)
          printAndLogError(s"Found different targetPaths ($uniquePhysDirs) on nodes. So it is not same path on all nodes", log)
        val physDirsAllHaveSamePath: Boolean = (uniquePhysDirs.size == 1)

        (scalaAndJavaAreValidAllNodes && zkKafkaHbaseHealthyForAllNodes && physDirsAllHaveSamePath)
      }
    } catch {
      case e: Exception => {
        logger.error("Failed to validateClusterEnvironment", e)
        phyDirLast = rootDirPath // substitute the symbol link to see if we can test more ... this is temporary hack
        false
      }
      case t: Throwable => {
        logger.error("Failed to validateClusterEnvironment", t)
        phyDirLast = rootDirPath
        false
      }
    }

    (proposedClusterEnvironmentIsSuitable, phyDirLast)
  }

  /** **************************************************************************************************************/


  /**
    * If the supplied file path exists, answer the file name
    *
    * @param log               the installation log... if the file doesn't exist, a message about that is logged here
    * @param flPath
    * @param checkForFile      when true, the filename (last node in path) must exist in file system
    * @param checkForParentDir when true, the parent directory that prefixes the file name must exist in file system
    * @return base Filename
    */
  private def getFileName(log: InstallDriverLog, flPath: String, checkForFile: Boolean, checkForParentDir: Boolean): String = {
    val fl = new File(flPath)

    if (checkForFile && !fl.exists) {
      val msg = s"$flPath does not exists"
      printAndLogError(msg, log)
      throw new Exception(msg)
    }

    if (checkForParentDir) {
      val parentPath = fl.getParent()
      val parent = new File(parentPath)
      if (!parent.exists) {
        val msg = s"$parentPath does not exists"
        printAndLogError(msg, log)
        throw new Exception(msg)
      }
    }

    fl.getName
  }

  private def logLogFile(logFile: String): Unit = {
    if (isFileExists(logFile, true)) {
      val logStmts = Source.fromFile(logFile).mkString
      if (logStmts != null)
        printAndLogDebug(logStmts, null, false)
    }
  }

  /** ComponentInfo packages essential information about some GetComponent request's results */
  case class ComponentInfo(version: String, status: String, errorMessage: String, componentName: String, invocationNode: String)

  var _cntr = 0

  /**
    *
    * @param log                             the install driver log containing key events and messages about the installation
    * @param rootDirPath                     To get the absolute DIR name, if it is soft link otherwise it returns as it is if it exists, otherwise null
    * @param scriptAbsolutePath              Path of the script to get all the Components information
    * @param componentVersionJarAbsolutePath FAT JAR to copy to client node and execute that script
    * @param remoteNodeIp                    Remote Node IP to do scp & ssh
    * @param resultsFileAbsolutePath         Results file will be writen here and parsed to Array[ComponentInfo]
    * @param jsonArgInput                    Requested components information. Ex: [{ "component" : "zookeeper", "hostslist" : "192.168.10.20:2181,192.168.10.21:2181" } , { "component" : "kafka", "hostslist" : "localhost:9092" }, { "component" : "hbase", "hostslist" : "localhost" }, { "component" : "scala", "hostslist" : "localhost" }, { "component" : "java", "hostslist" : "localhost" }]
    * @return (Array[ComponentInfo], String) => All Components Versions and physicalRootDir
    */

  def getComponentVersions(log: InstallDriverLog
                           , rootDirPath: String
                           , scriptAbsolutePath: String
                           , componentVersionJarAbsolutePath: String
                           , remoteNodeIp: String
                           , resultsFileAbsolutePath: String
                           , jsonArgInput: String
                           , ignoreGetComponentsInfo: Boolean): (Array[ComponentInfo], String) = {
    val checkForJar: Boolean = true
    val checkForJarParentDir: Boolean = false
    val componentVersionJarFileName = getFileName(log, componentVersionJarAbsolutePath, checkForJar, checkForJarParentDir)
    val checkForFile: Boolean = false
    val checkForParentDir: Boolean = true
    val resultFileName = getFileName(log, resultsFileAbsolutePath, checkForFile, checkForParentDir)
    val jsonArg = jsonArgInput.replace("\r", " ").replace("\n", " ")
    val igStr = if (ignoreGetComponentsInfo) "true" else "false"

    _cntr += 1
    val pathOutputFlName = "__path_output_" + _cntr + "_" + math.abs(this.hashCode) + "_" + math.abs(resultFileName.hashCode) + "_" + math.abs(scriptAbsolutePath.hashCode) + "_" + math.abs(componentVersionJarFileName.hashCode)
    val getComponentInvokeCmd: String = s"$scriptAbsolutePath  --componentVersionJarAbsolutePath $componentVersionJarAbsolutePath --componentVersionJarFileName $componentVersionJarFileName --remoteNodeIp $remoteNodeIp --resultsFileAbsolutePath $resultsFileAbsolutePath --resultFileName $resultFileName --rootDirPath $rootDirPath --pathOutputFileName $pathOutputFlName --ignoreGetComponentsInfo $igStr --jsonArg \'$jsonArg\'"
    val getComponentsVersionCmd: Seq[String] = Seq("bash", "-c", getComponentInvokeCmd)
    _cntr += 1
    val logFile = "/tmp/__get_comp_ver_results_" + _cntr + "_" + math.abs(pathOutputFlName.hashCode) + "_" + math.abs(getComponentsVersionCmd.hashCode) + "_" + math.abs(scriptAbsolutePath.hashCode)

    printAndLogDebug(s"getComponentsVersion cmd used: $getComponentsVersionCmd", log)
    val getVerCmdRc: Int = (getComponentsVersionCmd #> new File(logFile)).!
    val getVerCmdResults = Source.fromFile(logFile).mkString
    logLogFile("/tmp/Get-Component.log")
    if (getVerCmdRc != 0) {
      printAndLogError(s"getComponentsVersion has failed...rc = $getVerCmdRc", log)
      printAndLogError(s"Command used: $getComponentsVersionCmd", log)
      printAndLogError(s"Command report:\n$getVerCmdResults", null, false)
      throw new Exception("Failed to get Components Versions. Return code:" + getVerCmdRc)
    }

    printAndLogDebug(s"Command report:\n$getVerCmdResults", null, false)

    val pathAbsPath = "/tmp/" + pathOutputFlName
    val physicalRootDir = (if (isFileExists(pathAbsPath)) Source.fromFile(pathAbsPath).mkString else "").trim
    printAndLogDebug("Found PhysicalRootDir:" + physicalRootDir, log)

    var results = ArrayBuffer[ComponentInfo]()

    if (!ignoreGetComponentsInfo) {
      try {
        val jsonStr = Source.fromFile(resultsFileAbsolutePath).mkString
        printAndLogDebug("Components Results:" + jsonStr, log)
        implicit val jsonFormats = org.json4s.DefaultFormats
        val json = org.json4s.jackson.JsonMethods.parse(jsonStr)
        if (json == null) {
          throw new Exception("Failed to parse Components Versions json : " + jsonStr)
        }

        var components = List[Any]()

        if (json.values.isInstanceOf[List[_]])
          components = json.values.asInstanceOf[List[_]]
        else
          components = List(json.values)

        components.foreach(c => {
          val tmpmap = c.asInstanceOf[Map[String, Any]]
          val version = tmpmap.getOrElse("version", null)
          val status = tmpmap.getOrElse("status", null)
          val errorMessage = tmpmap.getOrElse("errorMessage", null)
          val componentName = tmpmap.getOrElse("componentName", null)
          val invocationNode = tmpmap.getOrElse("invocationNode", remoteNodeIp)

          results += ComponentInfo(if (version == null) "" else version.toString
            , if (status == null) "" else status.toString
            , if (errorMessage == null) "" else errorMessage.toString
            , if (componentName == null) "" else componentName.toString
            , if (invocationNode == null) "" else invocationNode.toString)
        })
      } catch {
        case e: Exception => throw new Exception("Failed to parse Components Versions json", e)
        case e: Throwable => throw new Exception("Failed to parse Components Versions json", e)
      }
    }

    (results.toArray, physicalRootDir)
  }

  /** **************************************************************************************************************/

  def collectNodeInfo(log: InstallDriverLog, clusterId: String, clusterNodes: List[Map[String, Any]], installDir: String, rootDirPath: String)
  : (Array[String], Array[(String, String, String, String)], Array[(String, String)]) = {
    val configDir: String = s"$installDir/config"
    val ipsSet: Set[String] = clusterNodes.map(info => {
      val nodeInfo: Map[String, _] = info.asInstanceOf[Map[String, _]]
      val ipAddr: String = nodeInfo.getOrElse("NodeIpAddr", "_bo_gu_us_node_ip_ad_dr").asInstanceOf[String]
      ipAddr
    }).toSet
    if (ipsSet.contains("_bo_gu_us_node_ip_ad_dr")) {
      printAndLogError(s"the node ip information for cluster id $clusterId is invalid... aborting", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }
    val ips: Array[String] = ipsSet.toSeq.sorted.toArray

    val ipIdTargPathsSet: Set[(String, String, String, String)] = clusterNodes.map(info => {
      val nodeInfo: Map[String, _] = info.asInstanceOf[Map[String, _]]
      val ipAddr: String = nodeInfo.getOrElse("NodeIpAddr", "_bo_gu_us_node_ip_ad_dr").asInstanceOf[String]
      val nodeId: String = nodeInfo.getOrElse("NodeId", "unkn_own_node_id").asInstanceOf[String]
      val roles: List[String] = nodeInfo.getOrElse("Roles", List[String]()).asInstanceOf[List[String]]
      val rolesStr: String = if (roles.size > 0) roles.mkString(",") else "UNKNOWN_ROLES"
      (ipAddr, nodeId, configDir, rolesStr)
    }).toSet
    val ipIdTargPathsReasonable: Boolean = ipIdTargPathsSet.filter(quartet => {
      val (ipAddr, nodeId, _, rolesStr): (String, String, String, String) = quartet
      (ipAddr == "_bo_gu_us_node_ip_ad_dr" || nodeId == "unkn_own_node_id" || rolesStr == "UNKNOWN_ROLES")
    }).size == 0
    if (!ipIdTargPathsReasonable) {
      printAndLogError(s"the node ip addr, node identifier, and/or node roles are bad for cluster id $clusterId ... aborting", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }
    val ipIdTargPaths: Array[(String, String, String, String)] = ipIdTargPathsSet.toSeq.sorted.toArray

    val uniqueNodePaths: Set[String] = clusterNodes.map(info => {
      val nodeInfo: Map[String, _] = info.asInstanceOf[Map[String, _]]
      val ipAddr: String = nodeInfo.getOrElse("NodeIpAddr", "_bo_gu_us_node_ip_ad_dr").asInstanceOf[String]
      s"$ipAddr~$rootDirPath"
    }).toSet
    val ipPathPairs: Array[(String, String)] = uniqueNodePaths.map(itm => (itm.split('~').head, itm.split('~').last)).toSeq.sorted.toArray

    (ips, ipIdTargPaths, ipPathPairs)
  }

  /**
    * Produce a properties map by ingesting the contents of the supplied file. If the file is invalid or an error occurs,
    * the log is updated with the error message and processing stops.
    *
    * @param log
    * @param apiConfigPath
    * @return a Properties instance
    */
  def mapProperties(log: InstallDriverLog, apiConfigPath: String): Properties = {

    val (properties, errorDescr): (Properties, String) = Utils.loadConfiguration(apiConfigPath, true)

    if (errorDescr != null) {
      printAndLogError(s"The apiConfigPath properties path ($apiConfigPath) could not produce a valid set of properties...aborting", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }

    properties
  }

  /**

    * @param log
    * @param rootDirPath
    * @param fromKamanjaVer
    * @param toKamanjaVer
    * @return
    */

  def CreateInstallationNames(log: InstallDriverLog, rootDirPath: String, fromKamanjaVer: String, toKamanjaVer: String)
  : (String, String, String) = {


    val parentPath: String = rootDirPath.split('/').dropRight(1).mkString("/")
    val rootDirName: String = rootDirPath.split('/').last
    val dateTime: DateTime = new DateTime
    val fmt: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMdd_HHmmss")
    val datestr: String = fmt.print(dateTime);
    /*
    // ROOT_DIR/parPath as /apps/KamanjaInstall can be link or dir. If it has link that is prior installation. Otherwise create as below

    /apps/KamanjaInstall =>

    /apps/KamanjaInstall_pre_<datetime> if  ROOT_DIR/parPath is DIR

    /apps/KamanjaInstall_<version>_<datetime>
    */
    val priorInstallDirName: String = s"${rootDirName}_${fromKamanjaVer}_${datestr}"
    val newInstallDirName: String = s"${rootDirName}_${toKamanjaVer}_${datestr}"

    (parentPath, priorInstallDirName, newInstallDirName)
  }

  private def CheckInstallVerificationFile(log: InstallDriverLog, fl: String, ipPathPairs: Array[(String, String)], newInstallDirPath: String, rootDirPath: String): Boolean = {
    val allValues = ArrayBuffer[Array[String]]()
    val allLines = ArrayBuffer[String]()
    printAndLogDebug(fl + " contents")
    for (line <- Source.fromFile(fl).getLines()) {
      printAndLogDebug(line)
      val vals = line.split(",")
      if (vals.size != 7) {
        val errMsg = "Expecting HostName,LinkDir,LinkExists(Yes|No),LinkPointingToDir,LinkPointingDirExists(Yes|No),NewInstallDir, NewInstallDirExists(Yes|No). But found:" + line
        printAndLogError(errMsg, log)
      } else {
        allValues += vals;
        allLines += line
      }
    }

    // Checke whether we have all nodes or not
    if (ipPathPairs.size != allValues.size) {
      val errMsg = "Suppose to get verification information for %d nodes. But we got only for %d".format(ipPathPairs.size, allValues.size)
      printAndLogError(errMsg, log)
      return false
    }

    var isInvalid = false
    // Starting with index 0
    // #3 & #5 should match
    // #2, #4, #6 should have only Yes
    // #5 & newInstallDirPath should match
    allValues.foreach(av => {
      if (av(3).compare(av(5)) != 0) {
        val errMsg = ("LinkPointingToDir:%s != NewInstallDir:%s from %s".format(av(3), av(5), av.mkString(",")))
        printAndLogError(errMsg, log)
        isInvalid = true;
      }
      if (av(2).equalsIgnoreCase("No") || av(4).equalsIgnoreCase("No") || av(6).equalsIgnoreCase("No")) {
        val errMsg = ("LinkExists/LinkPointingDirExists/NewInstallDirExists is NO from %s".format(av.mkString(",")))
        printAndLogError(errMsg, log)
        isInvalid = true;
      }
      if (av(5).compare(newInstallDirPath) != 0) {
        val errMsg = ("NewInstallDir:%s != newInstallDirPath:%s from %s".format(av(3), newInstallDirPath, av.mkString(",")))
        printAndLogError(errMsg, log)
        isInvalid = true;
      }
    })

    if (isInvalid)
      return false;

    val map = allValues.map(av => ((av(0).toLowerCase() + "," + av(1)), av)).toMap

    val notFoundNodes = ArrayBuffer[String]()

    ipPathPairs.foreach(ipAndPath => {
      val foundNode = map.getOrElse((ipAndPath._1.toLowerCase() + "," + ipAndPath._2), null)

      if (foundNode == null) {
        isInvalid = true
        notFoundNodes += "(Host:%s & TargetPath:%s)".format(ipAndPath._1.toLowerCase(), ipAndPath._2)
      }
    })

    if (isInvalid) {
      val errMsg = ("%s not found in %s ".format(notFoundNodes.mkString(","), allLines.mkString(",")))
      printAndLogError(errMsg, log)
      return false;
    }

    return true
  }

  private def isFileExists(flPath: String, checkForFile: Boolean = false, checkForDir: Boolean = false): Boolean = {
    val fl = new File(flPath)
    if (!fl.exists)
      return false;
    if (checkForFile)
      return fl.isFile
    if (checkForDir)
      return fl.isDirectory
    true
  }


  /**
    * Call the KamanjaClusterInstall with the following parameters that it needs to install the software on the cluster.
    * Important events encountered during the installation are added to the InstallDriverLog instance.
    * Semantics:
    *
    * The installation is performed and as part of the result a file is produced that has information in it like this that
    * is used to verify the installation. There will be a row for each cluster node.  The fields are compared/contrasted
    * to see if the installation was successful.  Here is sample file content:
    *
    * # {HostName,LinkDir,LinkExists(Yes|No),LinkPointingToDir,LinkPointingDirExists(Yes|No),NewInstallDir, NewInstallDirExists(Yes|No)}

    * 10.100.0.53,/opt/KamanjaCluster-2.11,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes
    * 10.100.0.54,/opt/KamanjaCluster-2.11,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes
    * 10.100.0.55,/opt/KamanjaCluster-2.11,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes,/opt/KamanjaCluster-2.11_20160218231108,Yes

    * Checks: 1) All should be Yes/No should be Yes
    * 2) LinkPointingToDir and NewInstallDir should be equivalent
    * 3) The newInstallDirName passed should match  NewInstallDir
    * 4) LinkDir and API config property ROOT_DIR value should match
    *
    * CheckInstallVerificationFile check for this information
    *

    * @param log                       the InstallDriverLog that tracks progress and important events of this installation
    * @param kamanjaClusterInstallPath the kamanjaClusterInstallPath script, which is invoked to install Kamanja
    * @param rootDirPath               the actual root dir for the installation
    * @param apiConfigPath             the api config that contains seminal information about the cluster installation
    * @param nodeConfigPath            the node config that contains the cluster description used for the installation
    * @param priorInstallDirName       the name of the directory to be used for a prior installation that is being upgraded (if appropriate)
    * @param newInstallDirName         the new installation directory name that will live in parentPath
    * @param tarballPath               the local tarball path that contains the kamanja installation
    * @param ips                       a file path that contains the unique ip addresses for each node in the cluster
    * @param ipIdTargPaths             a file path that contains the ip address, node id, target dir path and roles
    * @param ipPathPairs               a file containing the unique ip addresses and path pairs
    * @return true if the installation succeeded.
    */
  def installCluster(log: InstallDriverLog
                     , kamanjaClusterInstallPath: String
                     , rootDirPath: String
                     , apiConfigPath: String
                     , nodeConfigPath: String
                     , priorInstallDirName: String
                     , newInstallDirName: String
                     , tarballPath: String
                     , ips: Array[String]
                     , ipIdTargPaths: Array[(String, String, String, String)]
                     , ipPathPairs: Array[(String, String)]
                     , workDir: String
                     , clusterId: String
                     , metadataDataStore: String
                     , externalJarsDir: String
                     , tenantId: String): Boolean = {

    val parentPath: String = rootDirPath.split('/').dropRight(1).mkString("/")

    val ipDataFile = workDir + "/ipData.txt" // We may need to use workingDir
    val ipPathDataFile = workDir + "/ipPathData.txt" // We may need to use workingDir
    val ipIdCfgTargDataFile = workDir + "/ipIdCfgTargData.txt" // We may need to use workingDir

    writeCfgFile(ips.mkString("\n") + "\n", ipDataFile)
    writeCfgFile(ipPathPairs.map(pair => pair._1 + "\n" + pair._2).mkString("\n") + "\n", ipPathDataFile)
    writeCfgFile(ipIdTargPaths.map(quad => quad._1 + "\n" + quad._2 + "\n" + s"$workDir/node${quad._2}.cfg\n" + quad._3 + "\n" + quad._4).mkString("\n") + "\n", ipIdCfgTargDataFile)

    ipIdTargPaths.foreach(quad => {
      writeCfgFile(s"# Node Information\nnodeId=${quad._2}\n\n#Storing metadata using MetadataStoreType, MetadataSchemaName & MetadataLocation\nMetadataDataStore=$metadataDataStore\n\n", s"$workDir/node${quad._2}.cfg")
    })

    val dateTime: DateTime = new DateTime
    val fmt: DateTimeFormatter = DateTimeFormat.forPattern("yyyyMMdd_HHmmss")
    val datestr: String = fmt.print(dateTime);
    val verifyFilePath: String = s"/tmp/__KamanjaClusterResults_$datestr"

    val priorInstallDirPath: String = s"$parentPath/$priorInstallDirName"
    val newInstallDirPath: String = s"$parentPath/$newInstallDirName"
    val externalJarsDirOptStr = if (externalJarsDir != null && externalJarsDir.nonEmpty) {
      s" --externalJarsDir '$externalJarsDir' "
    } else {
      ""
    }
    val tenantIdOptStr = if (tenantId != null && tenantId.nonEmpty) {
      s" --tenantId '$tenantId' "
    } else {
      ""
    }
    val installCmd: Seq[String] = Seq("bash", "-c", s"$kamanjaClusterInstallPath --ClusterId '$clusterId' --WorkingDir '$workDir' --MetadataAPIConfig '$apiConfigPath' --NodeConfigPath '$nodeConfigPath' --TarballPath '$tarballPath' --ipAddrs '$ipDataFile' --ipIdTargPaths '$ipIdCfgTargDataFile' --ipPathPairs '$ipPathDataFile' --priorInstallDirPath '$priorInstallDirPath' --newInstallDirPath '$newInstallDirPath' --installVerificationFile '$verifyFilePath' $externalJarsDirOptStr $tenantIdOptStr")
    val installCmdRep: String = installCmd.mkString(" ")
    printAndLogDebug(s"KamanjaClusterInstall cmd used: \n\n$installCmdRep", log)

    //printAndLogDebug(s"KamanjaClusterInstall cmd used: \n\n$installCmdRep\n")

    val installCmdRc: Int = (installCmd #> new File("/tmp/__install_results_")).!
    val installCmdResults: String = Source.fromFile("/tmp/__install_results_").mkString
    if (installCmdRc != 0) {
      printAndLogError(s"KamanjaClusterInstall has failed...rc = $installCmdRc", log)
      printAndLogError(s"Command used: $installCmd", log)
      printAndLogError(s"Command report:\n$installCmdResults", null, false)
      printAndLogError(s"Installation is aborted. Consult the log file (${log.logPath}) for details.", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    } else {
      printAndLogDebug(s"New installation command result report:\n$installCmdResults", null, false)
      if (!CheckInstallVerificationFile(log, verifyFilePath, ipPathPairs, newInstallDirPath, rootDirPath)) {
        printAndLogError("Failed to verify information collected from installation.", log)
        printAndLogDebug(usage, log)
        closeLog
        sys.exit(1)
      }
    }

    (installCmdRc == 0)
  }

  /**
    * runFromJsonConfigString(jsonConfigString : String) : Int
    *
    * @param log                      InstallDriverLog instance that records installation process events
    * @param apiConfigFile            the local path location of the metadata api configuration file
    * @param apiConfigMap             a Properties file version instance of that fil's content
    * @param nodeConfigPath           the cluster nodes configuration file
    * @param migrateConfigFilePath    the path to the migrateTemplate
    * @param unhandledMetadataDumpDir the location where logs and unhandledMetadata is dumped
    * @param fromKamanja              which Kamanja is being upgraded (e.g., 1.1 or 1.2)
    * @param fromScala                the Scala for the existing implementation (2.10)
    * @param toScala                  the Scala for the new installation (either 2.10 or 2.11)
    * @param parentPath               the directory that will contain the new and prior installation
    * @param priorInstallDirName      the physical name of the prior installation (after installation completes)
    * @param newInstallDirName        the physical name of the new installation
    * @return
    */
  def prepareForMigration(log: InstallDriverLog
                          , apiConfigFile: String
                          , apiConfigMap: Properties
                          , nodeConfigPath: String
                          , migrateConfigFilePath: String
                          , unhandledMetadataDumpDir: String
                          , fromKamanja: String
                          , fromScala: String
                          , toScala: String
                          , parentPath: String
                          , priorInstallDirName: String
                          , newInstallDirName: String
                          , physicalRootDir: String
                          , rootDirPath: String
			  , tenantId: String): Boolean = {

    val migrationToBeDone: String = if (fromKamanja == "1.1") "1.1=>1.4" else if (fromKamanja == "1.2") "1.2=>1.4" else if (fromKamanja == "1.3") "1.3=>1.4" else "hmmm"

    // We should use these insted of below ones
    // val kamanjaFromVersion: String = fromKamanja
    // val kamanjaFromVersionWithUnderscore: String = fromKamanja.replace('.', '_')

    val migratePreparationOk: Boolean = migrationToBeDone match {
      case "1.1=>1.4" => {
        val kamanjaFromVersion: String = "1.1"
        val kamanjaFromVersionWithUnderscore: String = "1_1"
        val migrateConfigJSON: String = createMigrationConfig(log
          , migrateConfigFilePath
          , nodeConfigPath
          , apiConfigFile
          , kamanjaFromVersion
          , kamanjaFromVersionWithUnderscore
          , newInstallDirName
          , priorInstallDirName
          , fromScala
          , toScala
          , unhandledMetadataDumpDir
          , parentPath
          , physicalRootDir
          , rootDirPath
          , tenantId
        )
        migratePending = true
        migrateConfig = migrateConfigJSON
        printAndLogDebug("Pending migrate %s with config %s".format(migrationToBeDone, migrateConfigJSON))


        /*
                val migrateObj: Migrate = new Migrate()
                migrateObj.registerStatusCallback(log)
                val rc: Int = migrateObj.runFromJsonConfigString(migrateConfigJSON)
                (rc == 0)
        */
        true
      }
      case "1.2=>1.4" => {
        val kamanjaFromVersion: String = "1.2"
        val kamanjaFromVersionWithUnderscore: String = "1_2"
        val migrateConfigJSON: String = createMigrationConfig(log
          , migrateConfigFilePath
          , nodeConfigPath
          , apiConfigFile
          , kamanjaFromVersion
          , kamanjaFromVersionWithUnderscore
          , newInstallDirName
          , priorInstallDirName
          , fromScala
          , toScala
          , unhandledMetadataDumpDir
          , parentPath
          , physicalRootDir
          , rootDirPath
          , tenantId
        )
        migratePending = true
        migrateConfig = migrateConfigJSON
        printAndLogDebug("Pending migrate %s with config %s".format(migrationToBeDone, migrateConfigJSON))
        true
      }
      case "1.3=>1.4" => {
        val kamanjaFromVersion: String = "1.3"
        val kamanjaFromVersionWithUnderscore: String = "1_3"
        val migrateConfigJSON: String = createMigrationConfig(log
          , migrateConfigFilePath
          , nodeConfigPath
          , apiConfigFile
          , kamanjaFromVersion
          , kamanjaFromVersionWithUnderscore
          , newInstallDirName
          , priorInstallDirName
          , fromScala
          , toScala
          , unhandledMetadataDumpDir
          , parentPath
          , physicalRootDir
          , rootDirPath
          , tenantId
        )
        migratePending = true
        migrateConfig = migrateConfigJSON
        printAndLogDebug("Pending migrate %s with config %s".format(migrationToBeDone, migrateConfigJSON))
        true
      }
      case _ => {
        printAndLogError("The 'fromKamanja' parameter is incorrect... this needs to be fixed.  The value can only be '1.1' or '1.2' or '1.3' for the '1.4' upgrade", log)
        false
      }
    }
    if (!migratePreparationOk) {
      printAndLogError(s"The upgrade has failed.  Please consult the log (${log.logPath}) for guidance as to how to recover from this.", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }
    migratePreparationOk
  }

  /**
    *
    * Write the supplied configuration string to the supplied path.
    *
    * @param cfgString some configuration
  @param filePath      the path of the file that will be created.  It should be valid... no checks here.

    */
  private def writeCfgFile(cfgString: String, filePath: String) {
    val file = new File(filePath);
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(cfgString)
    bufferedWriter.close
  }


  /**
    * Answer a migration configuration json structure that contains the requisite migration info required for one of the migration/upgrade paths.
    *
    * @param log
    * @param migrateConfigFilePath            the local file system path where the template file is found
    * @param clusterConfigFile                substitution value
    * @param apiConfigFile                    substitution value
    * @param kamanjaFromVersion               substitution value
    * @param kamanjaFromVersionWithUnderscore substitution value
    * @param newInstallDirName                substitution value
    * @param priorInstallDirName              substitution value
    * @param scalaFromVersion                 substitution value
    * @param scalaToVersion                   substitution value
    * @param unhandledMetadataDumpDir         substitution value
    * @return filled in template with supplied values.
    */
  def createMigrationConfig(log: InstallDriverLog
                            , migrateConfigFilePath: String
                            , clusterConfigFile: String
                            , apiConfigFile: String
                            , kamanjaFromVersion: String
                            , kamanjaFromVersionWithUnderscore: String
                            , newInstallDirName: String
                            , priorInstallDirName: String
                            , scalaFromVersion: String
                            , scalaToVersion: String
                            , unhandledMetadataDumpDir: String
                            , parentPath: String
                            , physicalRootDir: String
                            , rootDirPath: String
			    , tenantId: String
                           ): String = {

    val template: String = Source.fromFile(migrateConfigFilePath).mkString

    /**

      * Current Macro names

      * ClusterConfigFile
      * ApiConfigFile
      * KamanjaFromVersion
      * KamanjaFromVersionWithUnderscore
      * NewPackageInstallPath
      * OldPackageInstallPath
      * ScalaFromVersion
      * ScalaToVersion
      * UnhandledMetadataDumpDir

      */

    val lastChar1 = rootDirPath.charAt(rootDirPath.size - 1)
    val lastChar2 = physicalRootDir.charAt(physicalRootDir.size - 1)

    val tRootDir = if (lastChar1 == '/' || lastChar1 == '\\') rootDirPath.substring(0, rootDirPath.size - 2) else rootDirPath

    val tPhyRootDir = if (lastChar2 == '/' || lastChar2 == '\\') physicalRootDir.substring(0, physicalRootDir.size - 2) else physicalRootDir

    // If ROOT_DIR & PHYDIR matches, that means it is dir. We moved it to s"$parentPath/$priorInstallDirName"
    val oldPackageInstallPath: String = if (tRootDir.compareTo(tPhyRootDir) == 0) s"$parentPath/$priorInstallDirName" else physicalRootDir
    val newPackageInstallPath: String = s"$parentPath/$newInstallDirName"

    val subPairs = Map[String, String]("{ClusterConfigFile}" -> clusterConfigFile
      , "{ApiConfigFile}" -> apiConfigFile
      , "{KamanjaFromVersion}" -> kamanjaFromVersion
      , "{KamanjaFromVersionWithUnderscore}" -> kamanjaFromVersionWithUnderscore
      , "{NewPackageInstallPath}" -> newPackageInstallPath
      , "{OldPackageInstallPath}" -> oldPackageInstallPath
      , "{ScalaFromVersion}" -> scalaFromVersion
      , "{ScalaToVersion}" -> scalaToVersion
      , "{UnhandledMetadataDumpDir}" -> unhandledMetadataDumpDir
      , "{TenantId}" -> tenantId)

    val substitutionMap: Map[String, String] = subPairs.toMap
    val varSub = new MapSubstitution(template, substitutionMap, logger, log)
    val substitutedTemplate: String = varSub.makeSubstitutions
    if (substitutedTemplate == null || substitutedTemplate.isEmpty) {
      printAndLogError("Failed to substitue valuesin Migration Template", log)
      printAndLogDebug(usage, log)
      closeLog
      sys.exit(1)
    }
    substitutedTemplate
  }

  override def statusUpdate(statusText: String, typStr: String): Unit = {
    print(statusText, typStr, log)
  }
}

class ClusterConfigMap(cfgStr: String, var clusterIdOfInterest: String) {

  val clusterMap: Map[String, Any] = getClusterConfigMapOfInterest(cfgStr)
  if (clusterMap.size == 0) {
    if (clusterIdOfInterest != null)
      throw new RuntimeException(s"There is no cluster information for cluster $clusterIdOfInterest")
    else
      throw new RuntimeException("Did not find any clusters information")
  }
  val clusterId: String = getClusterId
  val dataStore: Map[String, Any] = getDataStore
  val zooKeeperInfo: Map[String, Any] = getZooKeeperInfo
  val environmentContext: Map[String, Any] = getEnvironmentContext
  val clusterNodes: List[Map[String, Any]] = getClusterNodes
  val adapters: List[Map[String, Any]] = getAdapters

  def ClusterMap: Map[String, Any] = clusterMap

  def ClusterId: String = clusterId

  def DataStore: Map[String, Any] = dataStore

  def ZooKeeperInfo: Map[String, Any] = zooKeeperInfo

  def EnvironmentContext: Map[String, Any] = environmentContext

  def ClusterNodes: List[Map[String, Any]] = clusterNodes

  def Adapters: List[Map[String, Any]] = adapters

  def ZooKeeperConnectionString: String = {

    zooKeeperInfo.getOrElse("ZooKeeperConnectString", "").asInstanceOf[String]
  }

  def KafkaConnections: String = {
    val kafkaAdapters: List[Map[String, Any]] = adapters.filter(adapterMap => {
      val adapterJars: List[String] = adapterMap.getOrElse("DependencyJars", List[String]()).asInstanceOf[List[String]]
      val hasKafka: Boolean = adapterJars.filter(jarName => jarName.contains("kafka_2")).nonEmpty
      hasKafka
    })
    val hostConnections: List[String] = kafkaAdapters.map(adapter => {
      val adapterSpecificCfg: Map[String, Any] = adapter.getOrElse("AdapterSpecificCfg", Map[String, Any]()).asInstanceOf[Map[String, Any]]
      val conn: String = adapterSpecificCfg.getOrElse("HostList", "").asInstanceOf[String]
      conn
    })

    if (hostConnections.isEmpty) return ""
    hostConnections.toSet.mkString(",")
  }

  /**
    * getStringFromJsonNode
    *
    * @param v just any old thing
    * @return a string representation
    */
  private def getStringFromJsonNode(v: Map[String, Any]): String = {
    if (v == null) return ""
    implicit val jsonFormats: Formats = DefaultFormats
    return Serialization.write(v)
  }

  def DataStoreConnections: String = {

    if (DataStore != null) {
      var storeTyp = DataStore.getOrElse("StoreType", DataStore.getOrElse("component", "")).toString
      if (!storeTyp.equalsIgnoreCase("hbase")) return "" // if it is not hbase we are returning empty string for now.
      return getStringFromJsonNode(DataStore)
    }
    ""
  }

  private def getClusterConfigMapOfInterest(cfgStr: String): Map[String, Any] = {
    val clusterMap: Map[String, Any] = try {
      // extract config objects
      val map = JsonSerializer.parseEngineConfig(cfgStr)
      // process clusterInfo object if it exists
      val mapOfInterest: Map[String, Any] = if (map.contains("Clusters")) {
        val clusterList: List[_] = map.get("Clusters").get.asInstanceOf[List[_]]
        val clusters = clusterList.length

        val clusterIdList: List[Any] =
          if (clusterIdOfInterest == null) {
            if (clusterList.size > 1)
              throw new RuntimeException("Cluster config has more than one cluster defined. Either specify --clusterId to take that cluster or provide config which has only one cluster")
            clusterList
          } else {
            val clusterSought: String = clusterIdOfInterest.toLowerCase
            val clusterIdList: List[Any] = clusterList.filter(aCluster => {
              val cluster: Map[String, Any] = aCluster.asInstanceOf[Map[String, Any]]
              val clusterId = cluster.getOrElse("ClusterId", "").toString.trim.toLowerCase
              (clusterId == clusterSought)
            })
            clusterIdList
          }

        val clusterOfInterestMap: Map[String, Any] = if (clusterIdList.size > 0) {
          val retVal = clusterIdList.head.asInstanceOf[Map[String, Any]]
          if (clusterIdOfInterest == null)
            clusterIdOfInterest = retVal.getOrElse("ClusterId", "").toString.trim.toLowerCase
          retVal
        } else {
          Map[String, Any]()
        }
        clusterOfInterestMap

      } else {
        Map[String, Any]()
      }
      mapOfInterest
    } catch {
      case e: Exception => {
        throw new Exception("Failed to parse clusterconfig", e)
      }
      case e: Throwable => {
        throw new Exception("Failed to parse clusterconfig", e)
      }
    }
    clusterMap
  }

  private def getClusterId: String = {
    val clustId: String = if (clusterMap.size > 0 && clusterMap.contains("ClusterId")) {
      clusterMap("ClusterId").asInstanceOf[String]
    } else {
      ""
    }
    clustId
  }

  private def getDataStore: Map[String, Any] = {
    val dataStoreMap: Map[String, Any] = if (clusterMap.size > 0 && clusterMap.contains("SystemCatalog")) {
      clusterMap("SystemCatalog").asInstanceOf[Map[String, Any]]
    } else {
      Map[String, Any]()
    }
    dataStoreMap
  }

  private def getZooKeeperInfo: Map[String, Any] = {
    val dataStoreMap: Map[String, Any] = if (clusterMap.size > 0 && clusterMap.contains("ZooKeeperInfo")) {
      clusterMap("ZooKeeperInfo").asInstanceOf[Map[String, Any]]
    } else {
      Map[String, Any]()
    }
    dataStoreMap
  }

  private def getEnvironmentContext: Map[String, Any] = {
    val envCtxMap: Map[String, Any] = if (clusterMap.size > 0 && clusterMap.contains("EnvironmentContext")) {
      clusterMap("EnvironmentContext").asInstanceOf[Map[String, Any]]
    } else {
      Map[String, Any]()
    }
    envCtxMap
  }

  private def getClusterNodes: List[Map[String, Any]] = {
    val nodeMapList: List[Map[String, Any]] = if (clusterMap.size > 0 && clusterMap.contains("Nodes")) {
      clusterMap("Nodes").asInstanceOf[List[Map[String, Any]]]
    } else {
      List[Map[String, Any]]()
    }
    nodeMapList
  }

  private def getAdapters: List[Map[String, Any]] = {
    val adapterMapList: List[Map[String, Any]] = if (clusterMap.size > 0 && clusterMap.contains("Adapters")) {
      clusterMap("Adapters").asInstanceOf[List[Map[String, Any]]]
    } else {
      List[Map[String, Any]]()
    }
    adapterMapList
  }
}

class MapSubstitution(template: String, vars: scala.collection.immutable.Map[String, String], logger: Logger, log: InstallDriverLog) {
  private def printAndLogDebug(msg: String, log: InstallDriverLog = null): Unit = {
    logger.debug(msg);
    if (!logger.isDebugEnabled())
      println(msg)
    if (log != null)
      log.emit(msg, "DEBUG")
  }

  private def printAndLogError(msg: String, log: InstallDriverLog = null, e: Throwable = null): Unit = {
    if (e != null)
      logger.error(msg, e);
    else
      logger.error(msg, e);
    println(msg)
    if (log != null)
      log.emit(msg, "ERROR")
  }

  def findAndReplace(m: Matcher)(callback: String => String): String = {
    val sb = new StringBuffer
    while (m.find) {
      printAndLogDebug("Found Template Varibale:" + m.group(1))
      val replStr = vars(m.group(1))
      printAndLogDebug("Replacing Template Varibale from %s to %s".format(m.group(1), replStr))
      m.appendReplacement(sb, callback(replStr))
    }
    m.appendTail(sb)
    sb.toString
  }

  def makeSubstitutions: String = {
    var retrStr = ""
    try {
      val patStr = """(\{[A-Za-z0-9_.-]+\})"""
      printAndLogDebug("Using RegEx:" + patStr)
      printAndLogDebug("Key & Values to replace:" + vars.mkString(","))
      val m = Pattern.compile(patStr).matcher(template)
      retrStr = findAndReplace(m) { x => x }
    } catch {
      case e: Exception => {
        printAndLogError("Failed to substitute Migration template", log, e)
        retrStr = ""
      }
      case e: Throwable => {
        printAndLogError("Failed to substitute Migration template", log, e)
        retrStr = ""
      }
    }
    retrStr
  }

}

object InstallDriver {
  def main(args: Array[String]): Unit = {
    val inst = new InstallDriver
    inst.run(args)
    if (inst.migrationPending) {
      println("Migration still pending. Migration config:" + inst.migrationConfig)
    }
  }
}

