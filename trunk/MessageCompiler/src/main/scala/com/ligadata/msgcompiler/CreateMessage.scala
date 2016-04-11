package com.ligadata.msgcompiler

import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;

class CreateMessage {

  val defaultOwner = "kamanja"
  val defaultTenant = ""
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)

  /*
   * create the message to add message in metadata (call MakeFixedMsg )
   */
  def createMessage(msg: Message, mdMgr: MdMgr, recompile: Boolean = false): ContainerDef = {
    var containerDef: ContainerDef = null
    if (msg.MsgType.equalsIgnoreCase("message")) {
      if (msg.Fixed.equalsIgnoreCase("true")) {
        containerDef = createFixedMsgDef(msg, mdMgr, recompile)
      } else if (msg.Fixed.equalsIgnoreCase("false")) {
        containerDef = createMappedMsgDef(msg, mdMgr, recompile)
      }

    } else if (msg.MsgType.equalsIgnoreCase("container")) {
      if (msg.Fixed.equalsIgnoreCase("true")) {
        containerDef = createFixedContainerDef(msg, mdMgr, recompile)
      } else if (msg.Fixed.equalsIgnoreCase("false")) {
        containerDef = createMappedContainerDef(msg, mdMgr, recompile)
      }
    }

    containerDef
  }

  /*
   * create the fixed message in metadata
   */
  private def createFixedMsgDef(msg: Message, mdMgr: MdMgr, recompile: Boolean = false): ContainerDef = {
    var msgDef: MessageDef = new MessageDef()

    try {
      var version = MdMgr.ConvertVersionToLong(msg.Version)

      if (msg.Persist) {
        if (msg.PartitionKeys == null || msg.PartitionKeys.size == 0) {
          throw new Exception("Please provide parition keys in the MessageDefinition since the Message will be Persisted based on Partition Keys")
        }
      }
      if (msg.PartitionKeys != null)
        msgDef = mdMgr.MakeFixedMsg(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, version, null, msg.Jarset.toArray, null, null, msg.PartitionKeys.toArray, recompile, msg.Persist)
      else
        msgDef = mdMgr.MakeFixedMsg(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, version, null, msg.Jarset.toArray, null, null, null, recompile, msg.Persist)

      log.info(" msg.NameSpace   " + msg.NameSpace)
      log.info("msg.Name    " + msg.Name)
      log.info("msg.PhysicalName   " + msg.PhysicalName)
      log.info("version    " + version)
      log.info("msg.ArgsList   " + msg.ArgsList.toList)
      log.info("msg.jarset    " + msg.Jarset.toList)
      log.info("recompile   " + recompile)
      log.info("msg.Persist    " + msg.Persist)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    msgDef
  }

  /*
   * create the mapped message in metadata
   */
  private def createMappedMsgDef(msg: Message, mdMgr: MdMgr, recompile: Boolean = false): ContainerDef = {
    var msgDef: MessageDef = new MessageDef()

    try {
      var version = MdMgr.ConvertVersionToLong(msg.Version)
      if (msg.Persist) {
        if (msg.PartitionKeys == null || msg.PartitionKeys.size == 0) {
          throw new Exception("Please provide parition keys in the MessageDefinition since the Message will be Persisted based on Partition Keys")
        }
      }

      log.info(" msg.NameSpace   " + msg.NameSpace)
      log.info("msg.Name    " + msg.Name)
      log.info("msg.PhysicalName   " + msg.PhysicalName)
      log.info("version    " + version)
      log.info("msg.ArgsList   " + msg.ArgsList.toList)
      log.info("msg.jarset    " + msg.Jarset.toList)
      log.info("recompile   " + recompile)
      log.info("msg.Persist    " + msg.Persist)

      if (msg.PartitionKeys != null)
        msgDef = mdMgr.MakeMappedMsg(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, msg.PartitionKeys.toArray, recompile, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, msg.Persist)
      else
        msgDef = mdMgr.MakeMappedMsg(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, null, recompile, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, msg.Persist)
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    msgDef
  }

  /*
   * create the Fixed Container in metadata
   */
  private def createFixedContainerDef(msg: Message, mdMgr: MdMgr, recompile: Boolean = false): ContainerDef = {
    var containerDef: ContainerDef = null;

    try {
      var version = MdMgr.ConvertVersionToLong(msg.Version)

      if (msg.Persist) {
        if (msg.PartitionKeys == null || msg.PartitionKeys.size == 0) {
          throw new Exception("Please provide parition keys in the MessageDefinition since the Message will be Persisted based on Partition Keys")
        }
      }

      if (msg.PartitionKeys != null)
        containerDef = mdMgr.MakeFixedContainer(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, msg.PartitionKeys.toArray, recompile, msg.Persist)
      else
        containerDef = mdMgr.MakeFixedContainer(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, null, recompile, msg.Persist)

      log.info(" msg.NameSpace   " + msg.NameSpace)
      log.info("msg.Name    " + msg.Name)
      log.info("msg.PhysicalName   " + msg.PhysicalName)
      log.info("version    " + version)
      log.info("msg.ArgsList   " + msg.ArgsList.toList)
      log.info("msg.jarset    " + msg.Jarset.toList)
      log.info("recompile   " + recompile)
      log.info("msg.Persist    " + msg.Persist)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    containerDef
  }

  /*
   * create the mapped Container in metadata
   */
  private def createMappedContainerDef(msg: Message, mdMgr: MdMgr, recompile: Boolean = false): ContainerDef = {
    var containerDef: ContainerDef = null;

    try {
      var version = MdMgr.ConvertVersionToLong(msg.Version)

      if (msg.Persist) {
        if (msg.PartitionKeys == null || msg.PartitionKeys.size == 0) {
          throw new Exception("Please provide parition keys in the MessageDefinition since the Message will be Persisted based on Partition Keys")
        }
      }
      if (msg.PartitionKeys != null)
        containerDef = mdMgr.MakeMappedContainer(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, msg.PartitionKeys.toArray, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, recompile, msg.Persist)
      else
        containerDef = mdMgr.MakeMappedContainer(msg.NameSpace, msg.Name, msg.PhysicalName, msg.ArgsList, MdMgr.ConvertVersionToLong(msg.Version), null, msg.Jarset.toArray, null, null, null, defaultOwner, defaultTenant, 0, 0, msg.schemaId, msg.Schema, recompile, msg.Persist)

      log.info(" msg.NameSpace   " + msg.NameSpace)
      log.info("msg.Name    " + msg.Name)
      log.info("msg.PhysicalName   " + msg.PhysicalName)
      log.info("version    " + version)
      log.info("msg.ArgsList   " + msg.ArgsList.toList)
      log.info("msg.jarset    " + msg.Jarset.toList)
      log.info("recompile   " + recompile)
      log.info("msg.Persist    " + msg.Persist)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    containerDef
  }

}