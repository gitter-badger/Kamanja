package com.ligadata.InputAdapters.hdfs

/**
 * Created by Yasser on 12/6/2015.
 */
import com.ligadata.AdaptersConfiguration.{SmartFileAdapterConfiguration, FileAdapterMonitoringConfig, FileAdapterConnectionConfig}
import com.ligadata.Exceptions.KamanjaException
import com.ligadata.InputAdapters.FileChangeType.FileChangeType
import com.ligadata.InputAdapters.FileChangeType._
import org.apache.hadoop.fs.FileStatus
import org.apache.hadoop.fs.FileSystem
import org.apache.hadoop.fs.Path
import org.apache.hadoop.fs.FSDataInputStream
import org.apache.hadoop.conf.Configuration
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.actors.threadpool.{ Executors, ExecutorService }
import java.io.{InputStream}
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.InputAdapters.{CompressionUtil, SmartFileHandler, SmartFileMonitor}
import scala.actors.threadpool.{Executors, ExecutorService}

class HdfsFileEntry {
  var name : String = ""
  var lastReportedSize : Long = 0
  var lastModificationTime : Long = 0
  //boolean processed
}

class MofifiedFileCallbackHandler(fileHandler : SmartFileHandler, modifiedFileCallback:(SmartFileHandler) => Unit) extends Runnable{
  def run() {
    modifiedFileCallback(fileHandler)
  }
}

class HdfsFileHandler extends SmartFileHandler{

  private var fileFullPath = ""
  
  private var in : InputStream = null
  private var hdFileSystem : FileSystem = null
  private var hdfsConfig : Configuration = null

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)
  
  def this(fullPath : String, connectionConf : FileAdapterConnectionConfig){
    this()

    fileFullPath = fullPath
    hdfsConfig = HdfsUtility.createConfig(connectionConf)
    hdFileSystem = FileSystem.newInstance(hdfsConfig)
  }

  /*def this(fullPath : String, fs : FileSystem){
    this(fullPath)

	hdFileSystem = fs
	closeFileSystem = false
  }*/

  def getFullPath = fileFullPath

  //gets the input stream according to file system type - HDFS here
  def getDefaultInputStream() : InputStream = {

    hdFileSystem = FileSystem.newInstance(hdfsConfig)
    val inputStream : FSDataInputStream =
      try {
        val inFile : Path = new Path(getFullPath)
        hdFileSystem.open(inFile)
      }
      catch {
        case e: Exception =>
          logger.error(e)
          null
      }

    inputStream
  }

  @throws(classOf[KamanjaException])
  def openForRead(): InputStream = {
    try {
      val tempInputStream = getDefaultInputStream()
      val compressionType = CompressionUtil.getFileType(this, null)
      tempInputStream.close() //close this one, only first bytes were read to decide compression type, reopen to read from the beginning
      in = CompressionUtil.getProperInputStream(getDefaultInputStream, compressionType)
      in
    }
    catch{
      case e : Exception => throw new KamanjaException (e.getMessage, e)
    }
  }

  @throws(classOf[KamanjaException])
  def read(buf : Array[Byte], length : Int) : Int = {

    try {
      logger.debug("Reading from hdfs file " + fileFullPath)

      if (in == null)
        return -1
      val readLength = in.read(buf, 0, length)
      logger.debug("readLength= " + readLength)
      readLength
    }
    catch{
      case e : Exception => {
        logger.warn("Error while reading from hdfs file [" + fileFullPath + "]",e)
        throw e
      }
    }
  }

  @throws(classOf[KamanjaException])
  def moveTo(newFilePath : String) : Boolean = {
     if(getFullPath.equals(newFilePath)){
      logger.warn(s"Trying to move file ($getFullPath) but source and destination are the same")
      return false
     }

     try {
       hdFileSystem = FileSystem.get(hdfsConfig)
       val srcPath = new Path(getFullPath)
       val destPath = new Path(newFilePath)
       
        if (hdFileSystem.exists(srcPath)) {
          hdFileSystem.rename(srcPath, destPath)
            logger.debug("Moved remote file success")
            fileFullPath = newFilePath
            return true
        }
        else{
            logger.warn("Source file was not found")
            return false
        }
     } 
     catch {
       case ex : Exception => ex.printStackTrace()
        return false
     } finally {

     }
  }
  
  @throws(classOf[KamanjaException])
  def delete() : Boolean = {
    logger.info(s"Deleting file ($getFullPath)")
     try {
       hdFileSystem = FileSystem.get(hdfsConfig)
       hdFileSystem.delete(new Path(getFullPath), true)
        logger.debug("Successfully deleted")
        return true
     } 
     catch {
       case ex : Exception => {
        ex.printStackTrace()
        logger.error(ex.getMessage)
        return false 
       }
        
     } finally {

     }
  }

  @throws(classOf[KamanjaException])
  def close(): Unit = {
    if(in != null)
      in.close()
    if(hdFileSystem != null) {
      logger.debug("Closing Hd File System object hdFileSystem")
      hdFileSystem.close()
    }
  }

  @throws(classOf[KamanjaException])
  def length : Long = getHdFileSystem.getFileStatus(new Path(getFullPath)).getLen

  @throws(classOf[KamanjaException])
  def lastModified : Long = getHdFileSystem.getFileStatus(new Path(getFullPath)).getModificationTime

  @throws(classOf[KamanjaException])
  override def exists(): Boolean = getHdFileSystem.exists(new Path(getFullPath))

  @throws(classOf[KamanjaException])
  override def isFile: Boolean = getHdFileSystem.isFile(new Path(getFullPath))

  @throws(classOf[KamanjaException])
  override def isDirectory: Boolean = getHdFileSystem.isDirectory(new Path(getFullPath))

  private def getHdFileSystem() : FileSystem = {
    try {
      hdFileSystem = FileSystem.get(hdfsConfig)
      hdFileSystem
    }
    catch {
      case ex : Exception => {
        throw new KamanjaException("", ex)
      }

    } finally {
    }
  }
}

/**
 * callback is the function to call when finding a modified file, currently has one parameter which is the file path
 */
class HdfsChangesMonitor (adapterName : String, modifiedFileCallback:(SmartFileHandler) => Unit) extends SmartFileMonitor{

  private var isMonitoring = false
  
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  val poolSize = 5
  private val globalFileMonitorCallbackService: ExecutorService = Executors.newFixedThreadPool(poolSize)

  private var connectionConf : FileAdapterConnectionConfig = null
  private var monitoringConf :  FileAdapterMonitoringConfig = null
  private var monitorsExecutorService: ExecutorService = null
  private var hdfsConfig : Configuration = null

  def init(adapterSpecificCfgJson: String): Unit ={
    val(_type, c, m) =  SmartFileAdapterConfiguration.parseSmartFileAdapterSpecificConfig(adapterName, adapterSpecificCfgJson)
    connectionConf = c
    monitoringConf = m

    if(connectionConf.hostsList == null || connectionConf.hostsList.length == 0){
      val err = "HostsList is missing or invalid for Smart HDFS File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }
    if(connectionConf.authentication.equalsIgnoreCase("kerberos")){
      if(connectionConf.principal == null || connectionConf.principal.length == 0 ||
        connectionConf.keytab == null || connectionConf.keytab.length == 0){
        val err = "Principal and Keytab cannot be empty for Kerberos authentication for Smart HDFS File Adapter Config:" + adapterName
        throw new KamanjaException(err, null)
      }
    }

    hdfsConfig = HdfsUtility.createConfig(connectionConf)
  }

  def shutdown: Unit ={

    isMonitoring = false
    monitorsExecutorService.shutdown()
  }

  def getFolderContents(parentfolder : String, hdFileSystem : FileSystem) : Array[FileStatus] = {
    try {
      val files = hdFileSystem.listStatus(new Path(parentfolder))
      files
    }
    catch{
      case ex : Exception => {
        logger.error(ex)
        new Array[FileStatus](0)
      }
    }
  }

  def monitor(){

    isMonitoring = true
    monitorsExecutorService = Executors.newFixedThreadPool(monitoringConf.locations.length)

    monitoringConf.locations.foreach(folderToWatch => {
      val dirMonitorthread = new Runnable() {
        private var targetFolder: String = _
        def init(dir: String) = targetFolder = dir

        override def run() = {

          val filesStatusMap = Map[String, HdfsFileEntry]()
          var firstCheck = true

          while (isMonitoring) {

            try {
              logger.info(s"Checking configured HDFS directory (targetFolder)...")


              val modifiedDirs = new ArrayBuffer[String]()
              modifiedDirs += targetFolder
              while (modifiedDirs.nonEmpty) {
                //each time checking only updated folders: first find direct children of target folder that were modified
                // then for each folder of these search for modified files and folders, repeat for the modified folders

                val aFolder = modifiedDirs.head
                val modifiedFiles = Map[SmartFileHandler, FileChangeType]() // these are the modified files found in folder $aFolder

                modifiedDirs.remove(0)
                val fs = FileSystem.get(hdfsConfig)
                findDirModifiedDirectChilds(aFolder, fs, filesStatusMap, modifiedDirs, modifiedFiles, firstCheck)

                //logger.debug("Closing Hd File System object fs in monitorDirChanges()")
                //fs.close()

                if (modifiedFiles.nonEmpty)
                  modifiedFiles.foreach(tuple => {
                    val handler = new MofifiedFileCallbackHandler(tuple._1, modifiedFileCallback)
                    // run the callback in a different thread
                    new Thread(handler).start()
                    //globalFileMonitorCallbackService.execute(handler)
                    //modifiedFileCallback(tuple._1,tuple._2)

                  }
                  )

              }

            }
            catch {
              case ex: Exception => {
                logger.error(ex.getMessage)
                ex.printStackTrace()
              }
            }

            firstCheck = false

            logger.info(s"Sleepng for ${monitoringConf.waitingTimeMS} milliseconds...............................")
            Thread.sleep(monitoringConf.waitingTimeMS)
          }
        }
      }
      dirMonitorthread.init(folderToWatch)
      monitorsExecutorService.execute(dirMonitorthread)
    })


  }

  private def findDirModifiedDirectChilds(parentfolder : String, hdFileSystem : FileSystem, filesStatusMap : Map[String, HdfsFileEntry],
                                          modifiedDirs : ArrayBuffer[String], modifiedFiles : Map[SmartFileHandler, FileChangeType], isFirstCheck : Boolean){
    logger.info("checking folder with full path: " + parentfolder)

    val directChildren = getFolderContents(parentfolder, hdFileSystem).sortWith(_.getModificationTime < _.getModificationTime)
    var changeType : FileChangeType = null //new, modified

    //process each file reported by FS cache.
    directChildren.foreach(fileStatus => {
      var isChanged = false
      val uniquePath = fileStatus.getPath.toString
      if(!filesStatusMap.contains(uniquePath)){
        //path is new
        isChanged = true
        changeType = if(isFirstCheck) AlreadyExisting else New

        val fileEntry = makeFileEntry(fileStatus)
        filesStatusMap.put(uniquePath, fileEntry)
        if(fileStatus.isDirectory)
          modifiedDirs += uniquePath
      }
      else{
        val storedEntry = filesStatusMap.get(uniquePath).get
        if(fileStatus.getModificationTime >  storedEntry.lastModificationTime){//file has been modified
          storedEntry.lastModificationTime = fileStatus.getModificationTime
          isChanged = true

          changeType = Modified
        }
      }
      
      //TODO : this method to find changed folders is not working as expected. so for now check all dirs
      if(fileStatus.isDirectory)
        modifiedDirs += uniquePath

      if(isChanged){
        if(fileStatus.isDirectory){
          
        }
        else{
          if(changeType == New || changeType == AlreadyExisting) {
            val fileHandler = new HdfsFileHandler(uniquePath, connectionConf)
            modifiedFiles.put(fileHandler, changeType)
          }
        }
      }
    }
    )


    val deletedFiles = new ArrayBuffer[String]()
    filesStatusMap.keys.filter(filePath => isDirectParentDir(filePath, parentfolder)).foreach(pathKey =>
      if(!directChildren.exists(fileStatus => fileStatus.getPath.toString.equals(pathKey))){ //key that is no more in the folder => file/folder deleted
        deletedFiles += pathKey
      }
    )
    deletedFiles.foreach(f => filesStatusMap.remove(f))
  }


  private def isDirectParentDir(file : String, dir : String) : Boolean = {
    try{
      val filePath = new org.apache.hadoop.fs.Path(file)
      val directParentDir = filePath.getParent.toString
      val dirPath = new org.apache.hadoop.fs.Path(dir).toString
      dirPath.toString.equals(directParentDir)
    }
    catch{
      case ex : Exception => false
    }
  }

  private def makeFileEntry(fileStatus : FileStatus) : HdfsFileEntry = {

    val newFile = new HdfsFileEntry()
    newFile.lastReportedSize = fileStatus.getLen
    newFile.name = fileStatus.getPath.toString
    newFile.lastModificationTime = fileStatus.getModificationTime
    newFile
  }

  def stopMonitoring(){
    isMonitoring = false
  }
}