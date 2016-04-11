package com.ligadata.msgcompiler


import com.ligadata.kamanja.metadata.MdMgr;

object MsgUtils {
  def LowerCase(str: String): String = {
    if (str != null && str.trim != "")
      return str.toLowerCase()
    else return ""
  }

  def isTrue(boolStr: String): Boolean = {
    if (boolStr.equals("true"))
      return true
    else return false
  }

  // Make sure the version is in the format of nn.nn.nn
  def extractVersion(message: scala.collection.mutable.Map[String, Any]): String = {
    MdMgr.FormatVersion(message.getOrElse("version", "0").toString)
  }

}