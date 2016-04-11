package com.ligadata.InputAdapters.sftp

import com.jcraft.jsch.UserInfo

/**
  * this class is used to represent SFTP User Info. since the API asks for a class implementing interface UserInfo
  */
class SftpPassphraseUserInfo (val passphrase : String) extends UserInfo {

  /*private var passphrase : String = null
  def this (pp : String)  {
    this()
    passphrase = pp
  }*/

  def  getPassphrase : String = {
    passphrase
  }

  def getPassword : String = {
    null
  }

  def promptPassphrase (arg0 : String ) : Boolean = {
    true
  }

  def promptPassword(arg0 : String) : Boolean = {
    false
  }

  def showMessage( message : String) : Unit = {

  }

  def promptYesNo(str : String): Boolean = {
    true
  }

}