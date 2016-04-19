package com.ligadata.filedataprocessor

import java.io.{IOException, File, PrintWriter}
import java.nio.file.{Path, FileSystems}

import com.ligadata.Exceptions.{InternalErrorException, MissingArgumentException}
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.KamanjaVersion.KamanjaVersion

/**
 * Created by danielkozin on 9/24/15.
 */
class DirectoryListener {

}

object LocationWatcher {
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  def main (args: Array[String]) : Unit = {

      if (args.size == 0 || args.size > 1) {
        logger.error("Smart File Consumer requires a configuration file as its argument")
        return
      }

    if (args(0).equalsIgnoreCase("--version")) {
      KamanjaVersion.print
      return
    }

      // Read the config and figure out how many consumers to start
      var config = args(0)
      var properties = scala.collection.mutable.Map[String,String]()

      val lines = scala.io.Source.fromFile(config).getLines.toList
      lines.foreach(line => {
        if (!line.startsWith("#")) {
          val lProp = line.split("=")
          try {
            logger.info("SMART FILE CONSUMER "+lProp(0) + " = "+lProp(1))
            properties(lProp(0)) = lProp(1)
          } catch {
            case iobe: IndexOutOfBoundsException => {
              logger.error("SMART FILE CONSUMER: Invalid format in the configuration file " + config, iobe)
              logger.error("SMART FILE CONSUMER: unable to determine the value for property " + lProp(0))
              return
            }
          }
        }
      })

      // FileConsumer is a special case we need to default to 1, but also have it present in the properties since
      // it is used later for memory managemnt
      var numberOfProcessorsRaw = properties.getOrElse(SmartFileAdapterConstants.NUMBER_OF_FILE_CONSUMERS,null)
      var numberOfProcessors: Int = 1
      if (numberOfProcessorsRaw == null) {
        properties(SmartFileAdapterConstants.NUMBER_OF_FILE_CONSUMERS) = "1"
        logger.info("SMART FILE CONSUMER: Defaulting the number of file consumers to 1")
      } else  {
        numberOfProcessors = numberOfProcessorsRaw.toInt
      }

      var processors: Array[FileProcessor] = new Array[FileProcessor](numberOfProcessors)
      var threads: Array[Thread] = new Array[Thread](numberOfProcessors)
      var path: Path= null
      try {
         val dirName = properties.getOrElse(SmartFileAdapterConstants.DIRECTORY_TO_WATCH, null)
         if (dirName == null) {
           logger.error("SMART FILE CONSUMER: Directory to watch is missing, must be specified")
           return
         }
         path = FileSystems.getDefault().getPath(dirName)
      } catch {
        case e: IOException => {
          logger.error ("Unable to find the directory to watch", e)
          return
        }
      }

      logger.info("SMART FILE CONSUMER: Starting "+ numberOfProcessors+" file consumers, reading from "+ path)

      try {
        for (i <- 1 to numberOfProcessors) {
          var processor = new FileProcessor(path,i)
          processor.init(properties)
          val watch_thread = new Thread(processor)
          watch_thread.start
        }
      } catch {
        case e: Exception => {
          logger.error("SMART FILE CONSUMER:  ERROR in starting SMART FILE CONSUMER ", e)
          return
        }
      }
  }
}
