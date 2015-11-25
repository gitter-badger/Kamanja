package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.log4j.Logger;

class MessageObjectGenerator {

  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)
  var msgConstants = new MessageConstants

  //object CustAlertHistory extends RDDObject[CustAlertHistory] with BaseContainerObj {

  /*
   *  Generate the message/container object
   *  Message object line with RDDObject and BaseMsg
   *  Message object variable declaration - NAME, FULLNAME, NAMESPACE, VERSION, CreateNewMessage, IsFixed, CanPersist
   *  2 build methods - not sure whether we need it or not
   *  Paritiion keys stuff
   *  Primary keys stuff
   *  TimePartitionData
   *  getTimeParitionInfo method
   *  hasPrimaryKeys
   *  hasParitionKeys
   *  hasTimePartitionInfo
   *  getFullName
   *  toJavaRDDObject
   */
  def generateMessageObject(message: Message): String = {
    var msgObjeGenerator = new StringBuilder(8 * 1024)
    try {

      log.info("========== Message object Start==============")
      msgObjeGenerator = msgObjeGenerator.append(msgObject(message))
      msgObjeGenerator = msgObjeGenerator.append(msgObjVarsGeneration(message))
      msgObjeGenerator = msgObjeGenerator.append(msgConstants.msgObjectBuildStmts)
      msgObjeGenerator = msgObjeGenerator.append(keysCodeGeneration(message))
      msgObjeGenerator = msgObjeGenerator.append(msgConstants.newline + msgConstants.closeBrace)
      log.info("========== Message object End==============")

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return msgObjeGenerator.toString
  }

  /*
   * MessageObject with RDDObject and BaseMsg
   */

  private def msgObject(message: Message): String = {

    var baseMsgType: String = ""
    val msgType = message.MsgType
    if (msgType == null || msgType.trim() == "")
      throw new Exception("Message Definition root element should be either Message or Container")

    if (msgType.equalsIgnoreCase(msgConstants.messageStr))
      baseMsgType = msgConstants.baseMsgObj
    else if (msgType.equalsIgnoreCase(msgConstants.containerStr))
      baseMsgType = msgConstants.baseContainerObj

    log.info("1 ==============" + baseMsgType)
    log.info("2 ==============" + msgConstants.msgObjectStr.format(message.Name, message.Name, baseMsgType, msgConstants.newline))

    return msgConstants.msgObjectStr.format(message.Name, message.Name, baseMsgType, msgConstants.newline)
  }

  private def msgObjVarsGeneration(message: Message): String = {
    var msgObjeGenerator = new StringBuilder(8 * 1024)
    try {
      var createMsgType: String = ""
      val msgType = message.MsgType
      if (msgType == null || msgType.trim() == "")
        throw new Exception("Message Definition root element should be either Message or Container")

      if (msgType.equalsIgnoreCase(msgConstants.messageStr))
        createMsgType = msgConstants.createNewMessage
      else if (msgType.equalsIgnoreCase(msgConstants.containerStr))
        createMsgType = msgConstants.createNewContainer

      var isFixed: String = ""
      var isKV: String = ""
      if (message.Fixed.equalsIgnoreCase(msgConstants.True)) {
        isFixed = msgConstants.True
        isKV = msgConstants.False
      } else if (message.Fixed.equalsIgnoreCase(msgConstants.False)) {
        isFixed = msgConstants.False
        isKV = msgConstants.True
      }

      msgObjeGenerator.append(msgConstants.template.format(msgConstants.pad1, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.fullName.format(msgConstants.pad1, message.Pkg, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.namespace.format(msgConstants.pad1, message.NameSpace, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.name.format(msgConstants.pad1, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.version.format(msgConstants.pad1, message.Version, msgConstants.newline))
      msgObjeGenerator.append(createMsgType.format(msgConstants.pad1, message.Name, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.isFixed.format(msgConstants.pad1, isFixed, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.isKV.format(msgConstants.pad1, isKV, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.canPersist.format(msgConstants.pad1, message.Persist, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.getFullName.format(msgConstants.pad1, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.toJavaRDD.format(msgConstants.pad1, msgConstants.newline))

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return msgObjeGenerator.toString
  }

  /*
   * message object other methods
   */

  private def keysCodeGeneration(message: Message) = {
    """
    """ + getPartitionKeys(message) + """
      """ + getPrimaryKeys(message) + """
   

  override def PartitionKeyData(inputdata: InputData): Array[String] = Array[String]()
  override def PrimaryKeyData(inputdata: InputData): Array[String] = Array[String]()
  override def getTimePartitionInfo: (String, String, String) = (null, null, null) // FieldName, Format & Time Partition Types(Daily/Monthly/Yearly)
  override def TimePartitionData(inputdata: InputData): Long = 0
  
  override def hasPrimaryKey(): Boolean = {
	if(primaryKeys == null) return false;
	(primaryKeys.size > 0);
  }

  override def hasPartitionKey(): Boolean = {
	 if(partitionKeys == null) return false;
    (partitionKeys.size > 0);
  }

  override def hasTimeParitionInfo(): Boolean = {
    val tmPartInfo = getTimePartitionInfo
    (tmPartInfo != null && tmPartInfo._1 != null && tmPartInfo._2 != null && tmPartInfo._3 != null);
  }
  """
  }

  /*
   * gete the primary keys and parition keys
   */

  private def getPartitionKeys(message: Message): String = {

    var partitionInfo: String = ""
    var paritionKeys = new StringBuilder(8 * 1024)
    if (message.PartitionKey != null && message.PartitionKey.size > 0) {
      message.PartitionKey.foreach(key => {
        paritionKeys.append("\"" + key + "\", ")
      })
      partitionInfo = msgConstants.partitionKeys.format(msgConstants.pad1, "("+paritionKeys.toString.substring(0, paritionKeys.toString.length() - 2)+")", msgConstants.newline)

    } else partitionInfo = msgConstants.partitionKeys.format(msgConstants.pad1, "[String]()", msgConstants.newline)

    return partitionInfo
  }

  /*
   * gete the primary keys and parition keys
   */

  private def getPrimaryKeys(message: Message): String = {

    var primaryInfo: String = ""
    var primaryKeys = new StringBuilder(8 * 1024)
    if (message.PrimaryKeys != null && message.PrimaryKeys.size > 0) {
      message.PrimaryKeys.foreach(key => {
        primaryKeys.append("\"" + key + "\", ")
      })
      primaryInfo = msgConstants.primaryKeys.format(msgConstants.pad1, "("+primaryKeys.toString.substring(0, primaryKeys.toString.length() - 2)+")", msgConstants.newline)

    } else primaryInfo = msgConstants.primaryKeys.format(msgConstants.pad1, "[String]()", msgConstants.newline)

    return primaryInfo
  }

}