package com.ligadata.InputAdapters.sftp

import java.io.File
import java.net.URLEncoder

import com.ligadata.AdaptersConfiguration.FileAdapterConnectionConfig
import com.ligadata.Exceptions.KamanjaException
import org.apache.commons.vfs2.FileSystemOptions
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder

/**
  * Created by Yasser on 3/24/2016.
  */
object SftpUtility{
  def createConnectionString(connectionConf : FileAdapterConnectionConfig, remoteFilePath : String) : String = {
    if(connectionConf.hostsList == null || connectionConf.hostsList.length == 0)
      throw new KamanjaException("Smart File Adapter Hostslist cannot be empty for SFTP", null)
    //"sftp://" + username + ":" + password + "@" + hostName + "/" + remoteFilePath
    if(connectionConf.keyFile != null && connectionConf.keyFile.length != 0)
      "sftp://" + connectionConf.userId + "@" + connectionConf.hostsList(0) + "/" +  remoteFilePath
    else
      "sftp://" + connectionConf.userId + ":" + URLEncoder.encode(connectionConf.password) +
        "@" + connectionConf.hostsList(0) + "/" +  remoteFilePath
  }

  /**
    * To setup default SFTP config
    */
  def createDefaultOptions(connectionConf : FileAdapterConnectionConfig) : FileSystemOptions = {
    // Create SFTP options
    val opts = new FileSystemOptions();
    // SSH Key checking
    SftpFileSystemConfigBuilder.getInstance().setStrictHostKeyChecking(opts, "no")
    /*
     * Using the following line will cause VFS to choose File System's Root
     * as VFS's root. If I wanted to use User's home as VFS's root then set
     * 2nd method parameter to "true"
     */
    // Root directory set to user home
    SftpFileSystemConfigBuilder.getInstance().setUserDirIsRoot(opts, false)

    // Timeout is count by Milliseconds
    SftpFileSystemConfigBuilder.getInstance().setTimeout(opts, 10000)

    if (connectionConf.keyFile != null && connectionConf.keyFile.length > 0) {
      SftpFileSystemConfigBuilder.getInstance().setUserInfo(opts, new SftpPassphraseUserInfo(connectionConf.passphrase))
      SftpFileSystemConfigBuilder.getInstance().setIdentities(opts, Array (new File(connectionConf.keyFile) ))
    }

    return opts
  }


  def hashPath(origianlPathWithConnectionString : String) : String = {
    val colonIndex = origianlPathWithConnectionString.indexOf("://")
    val atIndex = origianlPathWithConnectionString.indexOf("@")

    if(colonIndex < 0 || atIndex < 0  )
      return origianlPathWithConnectionString

    val partToReplace = origianlPathWithConnectionString.substring(colonIndex, atIndex)
    //val hashedPath = origianlPath.replace(partToReplace, "://"+sftpConnectionConfig.userId +":"+"*****")
    val hashedPath = origianlPathWithConnectionString.replace(partToReplace, "://"+"user:pass")
    hashedPath
  }

  def getPathOnly(origianlPathWithConnectionString : String) : String = {
    val atIndex = origianlPathWithConnectionString.indexOf("@")

    if(atIndex < 0  )
      return origianlPathWithConnectionString

    val partToReplace = origianlPathWithConnectionString.substring(0, atIndex + 1)
    val pathWithHost = origianlPathWithConnectionString.replace(partToReplace, "")
    val hostPos = pathWithHost.indexOf("/")
    val pathOnly = pathWithHost.substring(hostPos)
    pathOnly
  }
}
