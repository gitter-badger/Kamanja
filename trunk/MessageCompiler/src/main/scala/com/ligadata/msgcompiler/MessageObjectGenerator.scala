package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }

class MessageObjectGenerator {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var msgConstants = new MessageConstants

  //object CustAlertHistory extends RDDObject[CustAlertHistory] with ContainerFactoryInterface {

  /*
   *  Generate the message/container object
   *  Message object line with RDDObject and MessageInterface
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

      // log.info("========== Message object Start==============")
      msgObjeGenerator = msgObjeGenerator.append(msgObject(message))
      msgObjeGenerator = msgObjeGenerator.append(msgObjVarsGeneration(message))
      msgObjeGenerator = msgObjeGenerator.append(msgConstants.msgObjectBuildStmts)
      msgObjeGenerator = msgObjeGenerator.append(keysCodeGeneration(message))
      msgObjeGenerator = msgObjeGenerator.append(ObjDeprecatedMethods(message))
      msgObjeGenerator = msgObjeGenerator.append(msgConstants.closeBrace)
      // log.info("========== Message object End==============")

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
   * MessageObject with RDDObject and MessageInterface
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

    log.info("MsgType: " + baseMsgType)
    log.info("ObjectStr: " + msgConstants.msgObjectStr.format(message.Name, message.Name, baseMsgType, msgConstants.newline))

    return msgConstants.msgObjectStr.format(message.Name, message.Name, baseMsgType, msgConstants.newline)
  }

  private def msgObjVarsGeneration(message: Message): String = {
    var msgObjeGenerator = new StringBuilder(8 * 1024)
    try {
      var interfaceType: String = "";
      var getContainerType: String = "";
      val msgType = message.MsgType
      if (msgType == null || msgType.trim() == "")
        throw new Exception("Message Definition root element should be either Message or Container")

      if (msgType.equalsIgnoreCase(msgConstants.messageStr)) {
        interfaceType = msgConstants.messageInstanceType
        getContainerType = msgConstants.getContainerTypeMsg
      } else if (msgType.equalsIgnoreCase(msgConstants.containerStr)) {
        interfaceType = msgConstants.containerInstanceType
        getContainerType = msgConstants.getContainerTypeContainer
      }

      var isFixed: String = ""
      var isKV: String = ""
      if (message.Fixed.equalsIgnoreCase(msgConstants.True)) {
        isFixed = msgConstants.True
      } else if (message.Fixed.equalsIgnoreCase(msgConstants.False)) {
        isFixed = msgConstants.False
      }

      msgObjeGenerator.append(msgConstants.template.format(msgConstants.pad1, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.fullName.format(msgConstants.pad1, message.NameSpace, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.namespace.format(msgConstants.pad1, message.NameSpace, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.name.format(msgConstants.pad1, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.version.format(msgConstants.pad1, message.Version, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.schemaId.format(msgConstants.pad1, message.schemaId, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.createInstance.format(msgConstants.pad1, message.Name, message.Name, message.Name, msgConstants.newline))
      msgObjeGenerator.append(msgConstants.isFixed.format(msgConstants.pad1, isFixed, msgConstants.newline))
      msgObjeGenerator.append(getContainerType.format(msgConstants.pad1) + msgConstants.newline)
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

    """   """ + getPartitionKeyNames(message) + """
  """ + getPrimaryKeyNames(message) + """   
  """ + getTimeParitionInfo(message) + """   
    override def hasPrimaryKey(): Boolean = {
      val pKeys = getPrimaryKeyNames();
      return (pKeys != null && pKeys.length > 0);
    }

    override def hasPartitionKey(): Boolean = {
      val pKeys = getPartitionKeyNames();
      return (pKeys != null && pKeys.length > 0);
    }

    override def hasTimePartitionInfo(): Boolean = {
      val tmInfo = getTimePartitionInfo();
      return (tmInfo != null && tmInfo.getTimePartitionType != TimePartitionInfo.TimePartitionType.NONE);
    }
  
    override def getAvroSchema: String = """ + "\"\"\"" + message.Schema + "\"\"\"" + """;  
"""
  }

  /*
   * Get TimePartitionInfo -- set the field name, format and type in TimePartitionInfo
   */

  private def getTimeParitionInfo(message: Message): String = {
    var timePartType: String = "";

    if (message.timePartition != null) {
      if (message.timePartition.DType == null || message.timePartition.DType.trim() == "")
        timePartType = "TimePartitionInfo.TimePartitionType.NONE";
      if (message.timePartition.DType.equalsIgnoreCase("yearly"))
        timePartType = "TimePartitionInfo.TimePartitionType.YEARLY";
      if (message.timePartition.DType.equalsIgnoreCase("monthly"))
        timePartType = "TimePartitionInfo.TimePartitionType.MONTHLY";
      if (message.timePartition.DType.equalsIgnoreCase("daily"))
        timePartType = "TimePartitionInfo.TimePartitionType.DAILY";

      return """
  def getTimePartitionInfo: TimePartitionInfo = {
    var timePartitionInfo: TimePartitionInfo = new TimePartitionInfo();
    timePartitionInfo.setFieldName("""" + message.timePartition.Key.toLowerCase() + """");
    timePartitionInfo.setFormat("""" + message.timePartition.Format.toLowerCase() + """");
    timePartitionInfo.setTimePartitionType("""" + timePartType + """");
    return timePartitionInfo
  }

    """
    } else {
      return """
  override def getTimePartitionInfo: TimePartitionInfo = { return null;}  // FieldName, Format & Time Partition Types(Daily/Monthly/Yearly)
  
    """
    }
  }

  /*
   * get the primary keys and parition keys
   */

  private def getPartitionKeyNames(message: Message): String = {

    var partitionInfo: String = ""
    var paritionKeys = new StringBuilder(8 * 1024)
    if (message.PartitionKeys != null && message.PartitionKeys.size > 0) {
      message.PartitionKeys.foreach(key => {
        paritionKeys.append("\"" + key + "\", ")
      })
      val partitionKys = "(" + paritionKeys.toString.substring(0, paritionKeys.toString.length() - 2) + ")";
      partitionInfo = msgConstants.getPartitionKeyNames.format(partitionKys, msgConstants.newline)

    } else partitionInfo = msgConstants.getPartitionKeyNames.format("[String]()", msgConstants.newline)

    """override def getPartitionKeyNames: Array[String] = """ + partitionInfo
  }

  /*
   * gete the primary keys and parition keys
   */

  private def getPrimaryKeyNames(message: Message): String = {

    var primaryInfo: String = ""
    var primaryKeys = new StringBuilder(8 * 1024)
    if (message.PrimaryKeys != null && message.PrimaryKeys.size > 0) {
      message.PrimaryKeys.foreach(key => {
        primaryKeys.append("\"" + key + "\", ")
      })
      val primaryKys = "(" + primaryKeys.toString.substring(0, primaryKeys.toString.length() - 2) + ")";
      primaryInfo = msgConstants.getPrimaryKeyNames.format(primaryKys, msgConstants.newline)

    } else primaryInfo = msgConstants.getPrimaryKeyNames.format("[String]()", msgConstants.newline)

    """override def getPrimaryKeyNames: Array[String] = """ + primaryInfo
  }

  private def ObjDeprecatedMethods(message: Message) = {
    var isMsg: String = "";
    var isCntr: String = "";
    var isFixed: String = ""
    var containerType: String = "BaseContainer"
    var msgType: String = "BaseMsg"
    var isKv: String = ""
    var createContrStr: String = ""
    var createMsgStr: String = ""
    var tranformData: String = ""

    if (msgConstants.isMessageFunc(message)) {
      isMsg = "true";
      isCntr = "false"

      createMsgStr = "def CreateNewMessage: " + msgType + "= createInstance.asInstanceOf[" + msgType + "];"
      createContrStr = "override def CreateNewContainer: " + containerType + "= null;"

      tranformData = "override def NeedToTransformData: Boolean = false";
    } else {
      isMsg = "false";
      isCntr = "true"
      containerType = "BaseContainer"
      createMsgStr = "def CreateNewMessage: " + msgType + "= null;"
      createContrStr = "override def CreateNewContainer: " + containerType + "= createInstance.asInstanceOf[" + containerType + "];"
      tranformData = "override def NeedToTransformData: Boolean = false"
    }

    if (msgConstants.isFixedFunc(message)) {
      isFixed = "true";
      isKv = "false"
    } else {
      isFixed = "false";
      isKv = "true"
    }

    """  
  override def FullName: String = getFullTypeName
  override def NameSpace: String = getTypeNameSpace
  override def Name: String = getTypeName
  override def Version: String = getTypeVersion
  """ + createMsgStr + """
  """ + createContrStr + """
  override def IsFixed: Boolean = """ + isFixed + """
  override def IsKv: Boolean = """ + isKv + """
  override def CanPersist: Boolean = """ + message.Persist + """
  override def isMessage: Boolean = """ + isMsg + """
  override def isContainer: Boolean = """ + isCntr + """
  override def PartitionKeyData(inputdata: InputData): Array[String] = createInstance.getPartitionKey();
  override def PrimaryKeyData(inputdata: InputData): Array[String] = createInstance.getPrimaryKey();
  override def TimePartitionData(inputdata: InputData): Long = createInstance.getTimePartitionData;
  """ + tranformData + """
    """
  }
}