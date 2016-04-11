package com.ligadata.jtm.eval

import java.text.{SimpleDateFormat, DateFormat}
import java.util.Date

/**
  * Created by joerg on 3/15/16.
  */
object Stamp {

  private val df: DateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mmZ")

  def Generate(): Array[String] = {
    var result = Array.empty[String]
    val path: String  = classOf[com.ligadata.jtm.Compiler].getProtectionDomain.getCodeSource.getLocation.getPath
    val localhostname = java.net.InetAddress.getLocalHost.getHostName
    result :+= "// Path: " + path
    result :+= "// Timestmap: " + df.format(new Date())
    result :+= "// Host: " + localhostname
    result :+= "// User: " + System.getProperty("user.name")
    result :+= "// Scala: " + scala.tools.nsc.Properties.versionMsg
    result :+= "// Os: " + scala.tools.nsc.Properties.osName
    result
  }
}
