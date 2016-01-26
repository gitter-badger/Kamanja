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

package com.ligadata.OutputAdapters

import org.apache.logging.log4j.{ Logger, LogManager }
import java.io._
import java.util.zip.{ZipException, GZIPOutputStream}
import java.nio.file.{ Paths, Files }
import com.ligadata.InputOutputAdapterInfo.{ AdapterConfiguration, OutputAdapter, OutputAdapterObj, CountersAdapter }
import com.ligadata.AdaptersConfiguration.FileAdapterConfiguration
import com.ligadata.Exceptions.{FatalAdapterException, StackTrace}
import com.ligadata.HeartBeat.{Monitorable, MonitorComponentInfo}
import org.json4s.jackson.Serialization


object FileProducer extends OutputAdapterObj {
  val ADAPTER_DESCRIPTION = "File Producer"
  def CreateOutputAdapter(inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter): OutputAdapter = new FileProducer(inputConfig, cntrAdapter)
}

class FileProducer(val inputConfig: AdapterConfiguration, cntrAdapter: CountersAdapter) extends OutputAdapter {
  private[this] val _lock = new Object()
  private[this] val LOG = LogManager.getLogger(getClass);

  private[this] val fc = FileAdapterConfiguration.GetAdapterConfig(inputConfig)
  private var os: OutputStream = null
  private val NEW_LINE = "\n".getBytes("UTF8")
  private val FAIL_WAIT = 2000
  private var numOfRetries = 0
  private var MAX_RETRIES = 3
  private val GZ = "gz"
  private var startTime = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var lastSeen = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(System.currentTimeMillis))
  private var metrics: scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map[String,Any]()

  //BUGBUG:: Not validating the values in FileAdapterConfiguration 

  //BUGBUG:: Open file to write the data
  // Taking only first file, if exists
  val sFileName = if (fc.Files != null && fc.Files.size > 0) fc.Files(0).trim else null
  if (sFileName == null || sFileName.size == 0)
    throw FatalAdapterException("First File Name should not be NULL or empty", new Exception("Invalid Parameters"))

  // Initialize the type of a file to Write to... Currently handles only TXT and GZ formats.
  val compString = if (fc.CompressionString == null) null else fc.CompressionString.trim

  while (os == null) {
    try {
      if (compString == null || compString.size == 0) {
        os = new FileOutputStream(sFileName, fc.append);
      } else if (compString.compareToIgnoreCase(GZ) == 0) {
        os = new GZIPOutputStream(new FileOutputStream(sFileName, fc.append)) // fc.append make sense here??
      } else {
        throw new Exception("Invalid Parameters")
      }
    } catch {
      case zio: ZipException => {throw FatalAdapterException("File Corruption (bad compression)", zio)}
      case fio: IOException => {
        LOG.warn("File input adapter "+fc.Name + ": Unable to create a file destination " + sFileName + " due to an IOException", fio)
        if (numOfRetries > MAX_RETRIES) {
          LOG.error("File input adapter " + fc.Name + ":Unable to create a file destination after " + MAX_RETRIES +" tries.  Aborting.")
          throw FatalAdapterException("Unable to open connection to specified file after " + MAX_RETRIES +" retries", fio)
        }
        numOfRetries += 1
        LOG.warn("File input adapter " + fc.Name + ": Retyring "+ numOfRetries + "/" + MAX_RETRIES)
        Thread.sleep(FAIL_WAIT)
      }
      case e: Exception => {throw FatalAdapterException("Unable to open connection to specified file ", e)}
    }
    LOG.info("File input adapter " + fc.Name + ": Output adapter file destination is " + sFileName)
    numOfRetries = 0
  }

  override  def getComponentStatusAndMetrics: MonitorComponentInfo = {
    implicit val formats = org.json4s.DefaultFormats
    return new MonitorComponentInfo(AdapterConfiguration.TYPE_OUTPUT, fc.Name, FileProducer.ADAPTER_DESCRIPTION, startTime, lastSeen,  Serialization.write(metrics).toString)
  }

  // Locking before we write into file
  // To send an array of messages. messages.size should be same as partKeys.size
  override def send(messages: Array[Array[Byte]], partKeys: Array[Array[Byte]]): Unit = _lock.synchronized {
    if (messages.size != partKeys.size) {
      LOG.error("File input adapter " + fc.Name + ": Message and Partition Keys hould has same number of elements. Message has %d and Partition Keys has %d".format(messages.size, partKeys.size))
      //TODO Need to record an error here... is this a job for the ERROR Q?
      return
    }
    if (messages.size == 0) return

    try {
      // Op is not atomic
      messages.foreach(message => {
        var isSuccess = false
        numOfRetries = 0
        while (!isSuccess) {
          try {
            os.write(message ++ NEW_LINE);
            isSuccess = true
          }
          catch {
            case zio: ZipException => {
              LOG.error("File input adapter " + fc.Name + ": File Corruption (bad compression)")
              throw zio
            }
            case fio: IOException => {
              LOG.warn("File input adapter " + fc.Name + ": Unable to write to file " + sFileName)
              if (numOfRetries >= MAX_RETRIES) {
                LOG.error("File input adapter " + fc.Name + ": Unable to create a file destination after " + MAX_RETRIES +" tries.  Aborting.")
                throw FatalAdapterException("Unable to open connection to specified file after " + MAX_RETRIES +" retries", fio)
              }
              numOfRetries += 1
              LOG.warn("File input adapter " + fc.Name + ": Retyring "+ numOfRetries + "/" + MAX_RETRIES)
              Thread.sleep(FAIL_WAIT)
            }
            case e: Exception => {
              LOG.error("File input adapter " + fc.Name + ": Unable to write output message: " + new String(message))
              throw e
            }
          }
        }
      })
      val key = Category + "/" + fc.Name + "/evtCnt"
      cntrAdapter.addCntr(key, messages.size) // for now adding rows
    } catch {
      case e: Exception => {
        LOG.error("File input adapter " + fc.Name + ": Failed to send :" + e.getMessage)
        throw FatalAdapterException("Unable to send message",e)
      }
    }
  }

  override def Shutdown(): Unit = _lock.synchronized {
    if (os != null)
      os.close
  }

}

