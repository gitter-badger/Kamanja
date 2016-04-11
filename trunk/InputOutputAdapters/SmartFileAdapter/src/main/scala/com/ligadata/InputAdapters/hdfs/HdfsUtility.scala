package com.ligadata.InputAdapters.hdfs

import com.ligadata.AdaptersConfiguration.FileAdapterConnectionConfig
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.security.UserGroupInformation

object HdfsUtility{
  def createConfig(connectionConf : FileAdapterConnectionConfig) : Configuration = {
    val hdfsConfig = new Configuration()
    hdfsConfig.set("fs.default.name", connectionConf.hostsList.mkString(","))
    hdfsConfig.set("fs.hdfs.impl", classOf[org.apache.hadoop.hdfs.DistributedFileSystem].getName)
    hdfsConfig.set("fs.file.impl", classOf[org.apache.hadoop.fs.LocalFileSystem].getName)
    //hdfsConfig.set("hadoop.job.ugi", "hadoop");//user ???
    if(connectionConf.authentication.equalsIgnoreCase("kerberos")){
      hdfsConfig.set("hadoop.security.authentication", "Kerberos")
      UserGroupInformation.setConfiguration(hdfsConfig)
      UserGroupInformation.loginUserFromKeytab(connectionConf.principal, connectionConf.keytab)
    }
    hdfsConfig
  }
}
