package com.ligadata.tools.containersutility

/**
  * Created by Yousef on 3/9/2016.
  */
import scala.collection.mutable._
import org.apache.logging.log4j. LogManager
import com.ligadata.Utils.Utils
import com.ligadata.MetadataAPI.MetadataAPIImpl
import com.ligadata.Exceptions._

trait LogTrait {
  val loggerName = this.getClass.getName()
  val logger = LogManager.getLogger(loggerName)
}

object ContainersUtility extends App with LogTrait {

  def usage: String = {
    """
Usage: scala com.ligadata.containersutility.ContainersUtility
    --config <config file while has jarpaths, metadata store information & data store information>
    --typename <full package qualified name of a Container>
    --keyfieldname  <name of key for container>
    --operation <truncate, select, delete>
    --keyid <key ids for select or delete>

Sample uses:
      java -jar /tmp/KamanjaInstall/ContainersUtility-1.0 --typename System.TestContainer --config /tmp/KamanjaInstall/EngineConfig.cfg --keyfieldname Id --oepration truncate

    """
  }

  override def main(args: Array[String]) {

    logger.debug("ContainersUtility.main begins")

    if (args.length == 0) logger.error(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, String]
    logger.debug(arglist)
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      list match {
        case Nil => map
        case "--config" :: value :: tail =>
          nextOption(map ++ Map('config -> value), tail)
        case "--kvname" :: value :: tail => // Deprecated, use typename instead
          nextOption(map ++ Map('kvname -> value), tail) // Deprecated, use typename instead
        case "--typename" :: value :: tail =>
          nextOption(map ++ Map('typename -> value), tail)
        case "--operation" :: value :: tail =>
          nextOption(map ++ Map('operation -> value), tail)
        case "--keyfieldname" :: value :: tail => // Deprecated, use keyfields instead
          nextOption(map ++ Map('keyfieldname -> value), tail) // Deprecated, use keyfields instead
        case "--keyfields" :: value :: tail =>
          nextOption(map ++ Map('keyfields -> value), tail)
        case "--keyid" :: value :: tail =>
          nextOption(map ++ Map('keyid -> value), tail)
        case option :: tail =>
          logger.error("Unknown option " + option)
          sys.exit(1)
      }
    }

    val options = nextOption(Map(), arglist)

    var cfgfile = if (options.contains('config)) options.apply('config) else null
    var typename = if (options.contains('typename)) options.apply('typename) else if (options.contains('kvname)) options.apply('kvname) else null
    var operation = if (options.contains('operation)) options.apply('operation) else null
    val tmpkeyfieldnames = if (options.contains('keyfields)) options.apply('keyfields) else if (options.contains('keyfieldname)) options.apply('keyfieldname) else null
    val keyid = if (options.contains('keyid)) options.apply('keyid) else null

  var valid: Boolean = (cfgfile != null && typename != null)

  if (valid) {
    cfgfile = cfgfile.trim
    typename = typename.trim
    valid = (cfgfile.size != 0 && typename.size != 0)
  }

  if (valid) {
    val (loadConfigs, failStr) = Utils.loadConfiguration(cfgfile.toString, true)
    if (failStr != null && failStr.size > 0) {
      logger.error(failStr)
      return
    }
    if (loadConfigs == null) {
      logger.error("Failed to load configurations from configuration file")
      return
    }

    containersUtilityConfiguration.configFile = cfgfile.toString

    val utilmaker: UtilityForContainers = new UtilityForContainers(loadConfigs, typename.toLowerCase, tmpkeyfieldnames,operation,keyid)
if (utilmaker.isOk) {
      try {
        val dstore = utilmaker.GetDataStoreHandle(containersUtilityConfiguration.jarPaths, utilmaker.dataDataStoreInfo)
        if (dstore != null) {
          try {
            //do truncate/select/delete
          } catch {
            case e: Exception => {
              logger.error("Failed to build Container or Message.", e)
            }
          } finally {
            if (dstore != null)
              dstore.Shutdown()
            com.ligadata.transactions.NodeLevelTransService.Shutdown
            if (utilmaker.zkcForSetData != null)
              utilmaker.zkcForSetData.close()
          }
        }
      } catch {
        case e: FatalAdapterException => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: StorageConnectionException => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: StorageFetchException => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: StorageDMLException => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: StorageDDLException => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: Exception => {
          logger.error("Failed to connect to Datastore.", e)
        }
        case e: Throwable => {
          logger.error("Failed to connect to Datastore.", e)
        }
      }
    }
    MetadataAPIImpl.CloseDbStore

  } else {
    logger.error("Illegal and/or missing arguments")
    logger.error(usage)
  }
}
}

object containersUtilityConfiguration {
  var nodeId: Int = _
  var configFile: String = _
  var jarPaths: collection.immutable.Set[String] = _
}