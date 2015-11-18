package com.ligadata.msgcompiler

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
}