package com.ligadata.InputAdapters.sftp

/**
  * Created by Yasser on 3/10/2016.
  */
import java.io._
import java.util.zip.GZIPInputStream
import com.ligadata.AdaptersConfiguration.{SmartFileAdapterConfiguration, FileAdapterMonitoringConfig, FileAdapterConnectionConfig}
import com.ligadata.Exceptions.{KamanjaException}
import com.ligadata.InputAdapters.FileChangeType.FileChangeType
import com.ligadata.InputAdapters.FileChangeType._

import scala.collection.mutable.{ArrayBuffer, Map}
import org.apache.commons.vfs2.{FileType, FileObject, FileSystemOptions, Selectors}
import org.apache.commons.vfs2.impl.StandardFileSystemManager
import com.ligadata.InputAdapters._
import org.apache.logging.log4j.{ Logger, LogManager }
import SftpUtility._
import scala.actors.threadpool.{Executors, ExecutorService}



class SftpFileEntry {
  var name : String = ""
  var lastReportedSize : Long = 0
  var lastModificationTime : Long = 0
  var parent : String = ""
  var isDirectory : Boolean = false
}

class SftpFileHandler extends SmartFileHandler{
  private var remoteFullPath = ""

  private var connectionConfig : FileAdapterConnectionConfig = null
  private var manager : StandardFileSystemManager = null
  private var in : InputStream = null
  //private var bufferedReader : BufferedReader = null

  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  def opts = createDefaultOptions(connectionConfig)
  def sftpEncodedUri = SftpUtility.createConnectionString(connectionConfig, getFullPath)

  def this(path : String, config : FileAdapterConnectionConfig){
    this()
    this.remoteFullPath = path
    connectionConfig = config
  }

  def getFullPath = remoteFullPath

  /*private def isCompressed : Boolean = {
    val tempInputStream : InputStream =
      try {
        val remoteFileObj = manager.resolveFile(sftpEncodedUri, opts)
        remoteFileObj.getContent().getInputStream()
      }
      catch {
        case e: Exception =>
          logger.error(e)
          null
      }
    val compressed = if(tempInputStream == null) false else isStreamCompressed(tempInputStream)
    if(tempInputStream != null){
      try{
        tempInputStream.close()
      }
      catch{case e : Exception => }
    }
    compressed
  }*/

  //gets the input stream according to file system type - SFTP here
  def getDefaultInputStream() : InputStream = {
    val inputStream : InputStream =
      try {
        manager = new StandardFileSystemManager()
        manager.init()

        val remoteFileObj = manager.resolveFile(sftpEncodedUri, opts)
        remoteFileObj.getContent().getInputStream()
      }
      catch {
        case e: Exception =>
          logger.error(e)
          null
      }
      finally {
        if(manager!=null)
          manager.close()
      }

    inputStream
  }

  @throws(classOf[KamanjaException])
  def openForRead(): InputStream = {
    logger.debug(s"Opening SFTP file ($getFullPath) to read")

    try {
      manager = new StandardFileSystemManager()
      manager.init()

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
      if (in == null) {
        logger.warn(s"Trying to read from SFTP file ($getFullPath) but input stream is null")
        return -1
      }
      logger.debug(s"Reading from SFTP file ($getFullPath)")
      in.read(buf, 0, length)
    }
    catch{
      case e : Exception => throw new KamanjaException (e.getMessage, e)
    }
  }

  @throws(classOf[KamanjaException])
  def moveTo(remoteNewFilePath : String) : Boolean = {
    if(getFullPath.equals(remoteNewFilePath)){
      logger.warn(s"Trying to move file ($getFullPath) but source and destination are the same")
      return false
    }
    try {
      manager  = new StandardFileSystemManager()
      manager.init()

      val remoteSrcFile = manager.resolveFile(createConnectionString(connectionConfig, getFullPath), createDefaultOptions(connectionConfig))
      val remoteDestFile = manager.resolveFile(createConnectionString(connectionConfig, remoteNewFilePath), createDefaultOptions(connectionConfig))

      if (remoteSrcFile.exists()) {
        remoteSrcFile.moveTo(remoteDestFile)
        logger.debug("Moved remote file success")
        remoteFullPath = remoteNewFilePath
        return true
      }
      else{
        logger.warn("Source file was not found")
        return false
      }
    }
    catch {
      case ex : Exception => {
        logger.error("", ex)
        return false
      }
    } finally {
      if(manager!=null)
        manager.close()
    }
  }

  @throws(classOf[KamanjaException])
  def delete() : Boolean = {
    logger.debug(s"Deleting file ($getFullPath)")
    try {
      manager  = new StandardFileSystemManager()
      manager.init()

      val remoteFile = manager.resolveFile(createConnectionString(connectionConfig, getFullPath), createDefaultOptions(connectionConfig))
      remoteFile.delete()
      logger.debug("Successfully deleted")
      return true
    }
    catch {
      case ex : Exception => {
        logger.error(ex.getMessage)
        return false
      }

    } finally {
      if(manager!=null)
        manager.close()
    }
  }

  @throws(classOf[KamanjaException])
  def close(): Unit = {
    logger.debug(s"closing SFTP file ($getFullPath)")
    /*if(bufferedReader != null)
      bufferedReader.close()*/
    if(in != null)
      in.close()
    if(manager != null)
      manager.close()
  }

  @throws(classOf[KamanjaException])
  def length : Long = getRemoteFileObject.getContent.getSize

  @throws(classOf[KamanjaException])
  def lastModified : Long = getRemoteFileObject.getContent.getLastModifiedTime

  @throws(classOf[KamanjaException])
  override def exists(): Boolean = getRemoteFileObject.exists()

  @throws(classOf[KamanjaException])
  override def isFile: Boolean = getRemoteFileObject.getType == FileType.FILE

  @throws(classOf[KamanjaException])
  override def isDirectory: Boolean = getRemoteFileObject.getType == FileType.FOLDER

  private def getRemoteFileObject : FileObject = {
    try {
      manager = new StandardFileSystemManager()
      manager.init()
      val remoteFile = manager.resolveFile(createConnectionString(connectionConfig, getFullPath), createDefaultOptions(connectionConfig))
      remoteFile
    }
    catch {
      case ex : Exception => {
        throw new KamanjaException("", ex)
      }

    } finally {
      if(manager!=null)
        manager.close()
    }
  }
}

class SftpChangesMonitor (adapterName : String, modifiedFileCallback:(SmartFileHandler) => Unit) extends SmartFileMonitor{

  private var isMonitoring = false
  lazy val loggerName = this.getClass.getName
  lazy val logger = LogManager.getLogger(loggerName)

  private var connectionConf : FileAdapterConnectionConfig = null
  private var monitoringConf :  FileAdapterMonitoringConfig = null

  private var monitorsExecutorService: ExecutorService = null

  def init(adapterSpecificCfgJson: String): Unit ={
    val(_type, c, m) =  SmartFileAdapterConfiguration.parseSmartFileAdapterSpecificConfig(adapterName, adapterSpecificCfgJson)
    connectionConf = c
    monitoringConf = m

    if(connectionConf.hostsList == null || connectionConf.hostsList.length == 0){
      val err = "Invalid host for Smart SFTP File Adapter Config:" + adapterName
      throw new KamanjaException(err, null)
    }

  }

  def monitor: Unit ={

    val manager : StandardFileSystemManager  = new StandardFileSystemManager()

    isMonitoring = true
    //Initializes the file manager
    manager.init();

    //Setup our SFTP configuration
    val opts = createDefaultOptions(connectionConf)

    monitorsExecutorService = Executors.newFixedThreadPool(monitoringConf.locations.length)

    monitoringConf.locations.foreach(folderToWatch => {
      val dirMonitorthread = new Runnable() {
        private var targetRemoteFolder: String = _
        def init(dir: String) = targetRemoteFolder = dir

        override def run() = {
          try {

            val sftpEncodedUri = createConnectionString(connectionConf, targetRemoteFolder)

            val filesStatusMap = Map[String, SftpFileEntry]()
            var firstCheck = true

            while (isMonitoring) {

              try {
                logger.info(s"Checking configured SFTP directory ($targetRemoteFolder)...")

                val modifiedDirs = new ArrayBuffer[String]()
                modifiedDirs += sftpEncodedUri
                while (modifiedDirs.nonEmpty) {
                  //each time checking only updated folders: first find direct children of target folder that were modified
                  // then for each folder of these search for modified files and folders, repeat for the modified folders

                  val aFolder = modifiedDirs.head
                  val modifiedFiles = Map[SmartFileHandler, FileChangeType]() // these are the modified files found in folder $aFolder

                  modifiedDirs.remove(0)
                  findDirModifiedDirectChilds(aFolder, manager, filesStatusMap, modifiedDirs, modifiedFiles, firstCheck)

                  if (modifiedFiles.nonEmpty)
                    modifiedFiles.foreach(tuple => {

                      /*val handler = new MofifiedFileCallbackHandler(tuple._1, tuple._2, modifiedFileCallback)
                   // run the callback in a different thread
                  //new Thread(handler).start()
                  globalFileMonitorCallbackService.execute(handler)*/
                      modifiedFileCallback(tuple._1)

                    }
                    )
                }

              }
              catch {
                case ex: Exception => ex.printStackTrace()
              }

              firstCheck = false

              logger.info(s"Sleepng for ${monitoringConf.waitingTimeMS} milliseconds...............................")
              Thread.sleep(monitoringConf.waitingTimeMS)
            }

            //if(!isMonitoring)
            //globalFileMonitorCallbackService.shutdown()
          }
          catch {
            case ex: Exception => {
              ex.printStackTrace()
            }
          }
          finally {
            manager.close()
          }
        }
      }
      dirMonitorthread.init(folderToWatch)
      monitorsExecutorService.execute(dirMonitorthread)
    })

  }

  def shutdown: Unit ={
    isMonitoring = false
    monitorsExecutorService.shutdown()
  }


  private def findDirModifiedDirectChilds(parentfolder : String, manager : StandardFileSystemManager, filesStatusMap : Map[String, SftpFileEntry],
                                          modifiedDirs : ArrayBuffer[String], modifiedFiles : Map[SmartFileHandler, FileChangeType], isFirstCheck : Boolean){
    val parentfolderHashed = hashPath(parentfolder)//used for logging since path contains user and password
    logger.info("checking folder with full path: " + parentfolderHashed)

    val directChildren = getRemoteFolderContents(parentfolder, manager).sortWith(_.getContent.getLastModifiedTime < _.getContent.getLastModifiedTime)
    var changeType : FileChangeType = null //new, modified

    //process each file reported by FS cache.
    directChildren.foreach(child => {
      val currentChildEntry = makeFileEntry(child)
      var isChanged = false
      val uniquePath = child.getURL().toString()

      if(!filesStatusMap.contains(uniquePath)){
        //path is new
        isChanged = true
        changeType = if(isFirstCheck) AlreadyExisting else New

        filesStatusMap.put(uniquePath, currentChildEntry)
        if(currentChildEntry.isDirectory)
          modifiedDirs += uniquePath
      }
      else{
        val storedEntry = filesStatusMap.get(uniquePath).get
        if(currentChildEntry.lastModificationTime >  storedEntry.lastModificationTime){//file has been modified
          storedEntry.lastModificationTime = currentChildEntry.lastModificationTime
          isChanged = true

          changeType = Modified
        }
      }

      //TODO : this method to find changed folders is not working as expected. so for now check all dirs
      if(currentChildEntry.isDirectory)
        modifiedDirs += uniquePath

      if(isChanged){
        if(currentChildEntry.isDirectory){

        }
        else{
          if(changeType == New || changeType == AlreadyExisting) {
            val fileHandler = new SftpFileHandler(getPathOnly(uniquePath), connectionConf)
            modifiedFiles.put(fileHandler, changeType)
          }
        }
      }
    }
    )


    val deletedFiles = new ArrayBuffer[String]()
    /*filesStatusMap.keys.filter(filePath => isDirectParentDir(filePath, parentfolder)).foreach(pathKey =>
      if(!directChildren.exists(fileStatus => fileStatus.getURL().toString.equals(pathKey))){ //key that is no more in the folder => file/folder deleted
        deletedFiles += pathKey
      }
    )*/

    filesStatusMap.values.foreach(fileEntry =>{
      if(isDirectParentDir(fileEntry, parentfolder)){
        if(!directChildren.exists(fileStatus => fileStatus.getURL().toString.equals(fileEntry.name))) //key that is no more in the folder => file/folder deleted
          deletedFiles += fileEntry.name
      }
    })

    deletedFiles.foreach(f => filesStatusMap.remove(f))
  }

  private def getRemoteFolderContents(parentRemoteFolderUri : String, manager : StandardFileSystemManager) : Array[FileObject] = {
    val remoteDir : FileObject = manager.resolveFile(parentRemoteFolderUri )
    val children = remoteDir.getChildren()
    children
  }

  private def makeFileEntry(fileObject : FileObject) : SftpFileEntry = {

    val newFile = new SftpFileEntry()
    newFile.name = fileObject.getURL.toString
    newFile.parent = fileObject.getParent.getURL.toString
    newFile.lastModificationTime = fileObject.getContent.getLastModifiedTime
    newFile.isDirectory = fileObject.getType.getName.equalsIgnoreCase("folder")
    newFile.lastReportedSize = if(newFile.isDirectory) -1 else fileObject.getContent.getSize //size is not defined for folders
    newFile
  }

  /*private def isDirectParentDir(fileObj : FileObject, dir : String) : Boolean = {
    fileObj.getParent.getURL.toString().equals(dir)
  }*/

  private def isDirectParentDir(fileObj : SftpFileEntry, dirUrl : String) : Boolean = {
    fileObj.parent.equals(dirUrl)
  }

}