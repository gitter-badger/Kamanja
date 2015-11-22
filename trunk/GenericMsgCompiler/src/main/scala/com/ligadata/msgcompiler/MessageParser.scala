package com.ligadata.msgcompiler

import scala.util.parsing.json.JSON;
import scala.reflect.runtime.universe;
import scala.io.Source;
import java.io.File;
import java.io.PrintWriter;
import java.util.Date;
import scala.collection.mutable.ListBuffer;
import scala.collection.mutable.ArrayBuffer;
import org.json4s.jackson.JsonMethods._;
import org.json4s.DefaultFormats;
import org.json4s.Formats;
import org.apache.log4j.Logger;
import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import com.ligadata.kamanja.metadata.MdMgr;
import com.ligadata.kamanja.metadata.EntityType;
import com.ligadata.kamanja.metadata.MessageDef;
import com.ligadata.kamanja.metadata.BaseAttributeDef;
import com.ligadata.kamanja.metadata.ContainerDef;
import com.ligadata.kamanja.metadata.ArrayTypeDef;
import com.ligadata.kamanja.metadata.ArrayBufTypeDef;

class MessageParser {

  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)

  /**
   * process the json map and return the message object
   */
  def processJson(json: String, mdMgr: MdMgr, recompile: Boolean = false): Message = {
    var message: Message = null
    var messages: Messages = null
    var msgList: List[Message] = List[Message]()
    var jtype: String = null

    val mapOriginal = parse(json).values.asInstanceOf[scala.collection.immutable.Map[String, Any]]

    if (mapOriginal == null)
      throw new Exception("Invalid json data")

    val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
    mapOriginal.foreach(kv => { map(kv._1.toLowerCase()) = kv._2 })

    var key: String = ""
    try {
      jtype = geJsonType(map)
      log.info("map : " + map)
      message = processJsonMap(jtype, map, mdMgr, recompile).asInstanceOf[Message]
      msgList :+ message
      messages = new Messages(msgList)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    message
  }

  /**
   * Get the type of message
   */
  private def geJsonType(map: scala.collection.mutable.Map[String, Any]): String = {
    var jtype: String = null
    try {
      if (map.contains("message"))
        jtype = "message"
      else if (map.contains("container"))
        jtype = "container"
    } catch {
      case e: Exception => {
        e.printStackTrace()
        throw e
      }
    }
    jtype
  }

  /**
   * Parse the json and create the Message Object
   */
  private def processJsonMap(key: String, map: scala.collection.mutable.Map[String, Any], mdMgr: MdMgr, recompile: Boolean = false): Message = {
    var msg: Message = null
    type messageMap = scala.collection.immutable.Map[String, Any]
    var msgLevel: Int = 0
    try {
      if (map.contains(key)) {
        if (map.get(key).get.isInstanceOf[messageMap]) {
          val message = map.get(key).get.asInstanceOf[messageMap]
          log.info("message map" + message)
          val messageMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          message.foreach(kv => { messageMap(kv._1.toLowerCase()) = kv._2 })

          msg = getMsgorCntrObj(messageMap, key, mdMgr, recompile, msgLevel).asInstanceOf[Message]
        }
      } else throw new Exception("Incorrect json")
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }

    msg
  }

  /**
   * Generate the Message object from message definition Map
   */
  private def getMsgorCntrObj(message: scala.collection.mutable.Map[String, Any], mtype: String, mdMgr: MdMgr, recompile: Boolean = false, msgLevel: Int): Message = {
    var ele: List[Element] = null
    var elements: List[Element] = null
    var messages: List[Message] = null
    var tdata: TransformData = null
    var tdataexists: Boolean = false
    var container: Message = null
    val tkey: String = "TransformData"
    var pKey: String = null
    var prmryKey: String = null
    var partitionKeysList: List[String] = null
    var primaryKeysList: List[String] = null
    var conceptsList: List[String] = null
    var msgVersion: String = ""
    var persistMsg: Boolean = false
    var NameSpace: String = ""
    var Name: String = ""
    var Description: String = ""
    var Fixed: String = ""

    try {
      if (message != null) {
        //  val message: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
        //  messageOriginal.foreach(kv => { message(kv._1.toLowerCase()) = kv._2 })

        if (message.getOrElse("namespace", null) == null)

          Name = "name"

        // throw new Exception("Please provide the Name space in the message definition ")

        if (message.getOrElse("name", null) == null)
          throw new Exception("Please provide the Name of the message definition ")

        if (message.getOrElse("version", null) == null)
          throw new Exception("Please provide the Version of the message definition ")

        // if (message.getOrElse("fixed", null) == null)
        //  throw new Exception("Please provide the type of the message definition (either Fixed or Mapped) ")
        if (message.getOrElse("fixed", null) == null)
          Fixed = ""
        else
          Fixed = message.get("fixed").get.toString()

        val persist = message.getOrElse("persist", "false").toString.toLowerCase

        if (MsgUtils.isTrue(MsgUtils.LowerCase(persist)))
          persistMsg = true

        for (key: String <- message.keys) {

          if (key.equals("elements") || key.equals("fields")) {
            val (elmnts, msgs) = getElementsObj(message, key)
            elements = elmnts
            messages = msgs
          }

          if (mtype.equals("message") && message.contains(tkey)) {
            if (key.equals(tkey)) {
              tdataexists = true
              tdata = getTransformData(message, key)
            }
          }

          if (key.equals("partitionkey")) {
            var partitionKeys = message.getOrElse("partitionkey", null)
            if (partitionKeys != null) {
              partitionKeysList = partitionKeys.asInstanceOf[List[String]]
              partitionKeysList = partitionKeysList.map(p => MsgUtils.LowerCase(p))
            }
          }

          if (key.equals("primarykey")) {
            var primaryKeys = message.getOrElse("primarykey", null)

            if (primaryKeys != null) {
              primaryKeysList = primaryKeys.asInstanceOf[List[String]]
              primaryKeysList = primaryKeysList.map(p => MsgUtils.LowerCase(p))
            }
          }

          var fldList: Set[String] = Set[String]()
          if (elements != null && elements.size > 0) {
            elements.foreach(Fld => { fldList += Fld.Name })

            if (fldList != null && fldList.size > 0) {
              if (partitionKeysList != null && partitionKeysList.size > 0) {
                if (!(partitionKeysList.toSet subsetOf fldList))
                  throw new Exception("Partition Keys should be included in fields/elements of message/container definition " + message.get("Name").get.toString())
              }

              if (primaryKeysList != null && primaryKeysList.size > 0) {
                if (!(primaryKeysList.toSet subsetOf fldList))
                  throw new Exception("Primary Keys should be included in fields/elements of message/container definition " + message.get("Name").get.toString())
              }
            }
          }
        }

        NameSpace = message.getOrElse("namespace", " ").toString
        Name = message.getOrElse("name", " ").toString
        if (message.getOrElse("description", null) == null)
          Description = ""
        else
          Description = message.get("description").get.toString()

        log.info("Name========================" + Name)

        if (MsgUtils.isTrue(MsgUtils.LowerCase(Fixed)) && elements == null)
          throw new Exception("Either Fields or Elements or Concepts  do not exist in " + message.get("name").get.toString())

        if (elements != null)
          elements = elements :+ new Element("", "transactionId", "system.long", "", "Fields", null, -1, null, null)
        else
          elements = List(new Element("", "transactionId", "system.long", "", "Fields", null, -1, null, null))

        // ele.foreach(f => log.debug("====" + f.Name))

        /*
          if (recompile) {
          msgVersion = messageGenUtil.getRecompiledMsgContainerVersion(mtype, NameSpace, Name, mdMgr)
        } else {
          msgVersion = extractVersion(message)
        }
        * 
        */
        msgVersion = extractVersion(message)

      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }

    val cur_time = System.currentTimeMillis
    //  val physicalName: String = pkg + "." + message.get("NameSpace").get.toString + "." + message.get("Name").get.toString() + "." + MdMgr.ConvertVersionToLong(msgVersion).toString + "_" + cur_time
    val pkg = NameSpace
    val physicalName: String = pkg + ".V" + MdMgr.ConvertVersionToLong(msgVersion).toString + "." + Name

    new Message(mtype, NameSpace, Name, physicalName, msgVersion, "Description", Fixed, persistMsg, elements, tdataexists, tdata, null, pkg.trim(), null, null, null, partitionKeysList, primaryKeysList, cur_time, msgLevel)
  }

  /**
   * Extract the message defintiion map to the List of Element objects
   */
  private def getElementsObj(message: scala.collection.mutable.Map[String, Any], key: String): (List[Element], List[Message]) = {
    // type list = List[Element]
    var elist: List[Element] = null
    var msglist: List[Message] = null
    var containerList: List[String] = null
    var container: Message = null
    var lbuffer = new ListBuffer[Element]
    var conceptStr: String = "Concepts"
    val msgLevel = 0

    try {
      if (key.equals("elements") || key.equals("fields")) {
        log.info("Element Map " + message)
        val (elementlist, messagelist) = getElements(message, key, msgLevel)
        elist = elementlist
        msglist = messagelist

      } else throw new Exception("Either Fields or Elements or Concepts  do not exist in " + key + " json")
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    (elist, msglist)
  }

  /**
   * Extract the message defintiion map to the List of Element objects
   */
  private def getElements(message: scala.collection.mutable.Map[String, Any], key: String, msgLevel: Int): (List[Element], List[Message]) = {
    var lbuffer = new ListBuffer[Element]
    var msgLstbuffer = new ListBuffer[Message]
    var container: Message = null
    type messageList = List[Map[String, Any]]
    type keyMap = Map[String, Any]
    type typList = List[String]
    var cntrList: List[String] = null
    try {
      var Fixed: String = ""
      if (message.getOrElse("fixed", null) != null)
        Fixed = message.get("fixed").get.toString()
      if (message.get(key).get.isInstanceOf[messageList]) {

        val eList = message.get(key).get.asInstanceOf[List[Map[String, Any]]]
        var count: Int = 0
        for (l <- eList) {
          if (l.isInstanceOf[keyMap]) {

            val eMap1: scala.collection.immutable.Map[String, Any] = l.asInstanceOf[scala.collection.immutable.Map[String, Any]]

            val eMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
            eMap1.foreach(kv => { eMap(kv._1.toLowerCase()) = kv._2 })

            // val eMap: Map[String, Any] = l.asInstanceOf[Map[String, Any]]

            if (eMap.contains("field")) {
              val (elmnt, msg) = getElement(eMap, count, msgLevel)
              lbuffer += elmnt
              msgLstbuffer += msg

            } else if (key.equals("fields")) {
              val (elmnt, msg) = getElementData(eMap.asInstanceOf[scala.collection.mutable.Map[String, Any]], key, count, msgLevel)
              lbuffer += elmnt
              msgLstbuffer += msg

            } else if (eMap.contains("container") || eMap.contains("message") || eMap.contains("containers") || eMap.contains("messages")) {

              var key: String = ""
              if (eMap.contains("container"))
                key = "container"
              else if (eMap.contains("containers"))
                key = "containers"
              else if (eMap.contains("message"))
                key = "message"
              else if (eMap.contains("messages"))
                key = "messages"

              if (eMap.get(key).get.isInstanceOf[keyMap]) {
                val containerMap: Map[String, Any] = eMap.get(key).get.asInstanceOf[Map[String, Any]]
                val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
                containerMap.foreach(kv => { map(kv._1) = kv._2 })

                val (elmnt, msg) = getElementData(map, key, count, msgLevel)

                lbuffer += elmnt
                msgLstbuffer += msg
              }

            } else if (MsgUtils.isTrue(MsgUtils.LowerCase(Fixed))) throw new Exception("Either Fields or Container or Message do not exist in " + key + " json")
          }
          count = count + 1
        }
      } else throw new Exception("Elements list do not exist in message/container definition json")
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    (lbuffer.toList, msgLstbuffer.toList)
  }

  /**
   * Extract the Element object from elements Map of message definition
   */
  private def getElement(eMap: scala.collection.mutable.Map[String, Any], ordinal: Int, msgLevel: Int): (Element, Message) = {
    var fld: Element = null
    var message: Message = null
    type keyMap = Map[String, String]
    if (eMap == null) throw new Exception("element Map is null")
    try {
      for (eKey: String <- eMap.keys) {
        val fldMap = eMap.get(eKey).get
        if (fldMap != null && fldMap != "None" && fldMap.isInstanceOf[keyMap]) {
          val fldMap1 = fldMap.asInstanceOf[scala.collection.immutable.Map[String, Any]]
          val mapElement: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          fldMap1.foreach(kv => { mapElement(kv._1.toLowerCase()) = kv._2 })
          val (field, msg) = getElementData(mapElement, eKey, ordinal, msgLevel)
          fld = field
          message = msg
        }
      }
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    (fld, message)
  }

  /**
   * Extract Element object from each Field in message definition
   */
  private def getElementData(fieldMap: scala.collection.mutable.Map[String, Any], key: String, ordinal: Int, msgLevel: Int): (Element, Message) = {
    var fld: Element = null
    var name: String = ""
    var fldTypeVer: String = null
    var childMessage: Message = null

    var namespace: String = ""
    var ttype: String = ""
    var collectionType: String = ""
    type string = String;
    if (fieldMap == null) throw new Exception("element Map is null")

    log.info("Field " + fieldMap.toList)
    log.info("key ===================== " + key)
    try {
      val field: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
      fieldMap.foreach(kv => { field(kv._1.toLowerCase()) = kv._2 })

      if (field.contains("namespace") && (field.get("namespace").get.isInstanceOf[string]))
        namespace = field.get("namespace").get.asInstanceOf[String]

      if (field.contains("name") && (field.get("name").get.isInstanceOf[String]))
        name = field.get("name").get.asInstanceOf[String].toLowerCase()
      else throw new Exception("Field Name do not exist in " + key)

      if (field.contains("type")) {
        val fieldtype: Any = field.get("type").get
        if (fieldtype.isInstanceOf[string]) {
          val fieldstr = fieldtype.toString.split("\\.")
          if ((fieldstr != null) && (fieldstr.size == 2)) {
            namespace = fieldstr(0).toLowerCase()
            ttype = fieldtype.asInstanceOf[String].toLowerCase()
          } else
            ttype = fieldtype.asInstanceOf[String].toLowerCase()
          if (field.contains("collectionType") && (field.get("collectionType").get.isInstanceOf[string])) {
            collectionType = field.get("collectionType").get.asInstanceOf[String].toLowerCase()
          }
          if (field.contains("version") && (field.get("version").get.isInstanceOf[string])) {
            fldTypeVer = field.get("version").get.asInstanceOf[String].toLowerCase()
          }
          fld = new Element(namespace, name, ttype, collectionType, key, fldTypeVer, ordinal, null, null)

        } else if (fieldtype.isInstanceOf[Map[String, Any]]) {
          log.info("Child Container ========== Start ==============  ")

          val childFld = fieldtype.asInstanceOf[Map[String, Any]]
          log.info("Child Map ************ " + childFld)

          val field: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          childFld.foreach(kv => { field(kv._1.toLowerCase()) = kv._2 })
          childMessage = getChildRecord(field, name, namespace, msgLevel + 1)
          // msgBuffer += message
          fld = new Element(namespace, name, ttype, collectionType, key, fldTypeVer, ordinal, null, null)

          log.info("Child Container ==========  End  ============== ")
        }
      } else {
        throw new Exception("Field Type do not exist in " + key)
      }

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    (fld, childMessage)
  }

  /**
   * Parse the child container in the message
   */
  private def getChildRecord(childrec: scala.collection.mutable.Map[String, Any], name: String, namespace: String, msgLevel: Int): Message = {
    var message: Message = null
    var lbuffer = new ListBuffer[Element]
    var count: Int = 0
    val eList = childrec.get("fields").get.asInstanceOf[List[Map[String, Any]]]

    for (l <- eList) {
      if (l.isInstanceOf[Map[String, Any]]) {
        val eMap1: scala.collection.immutable.Map[String, Any] = l.asInstanceOf[scala.collection.immutable.Map[String, Any]]

        val eMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
        eMap1.foreach(kv => { eMap(kv._1.toLowerCase()) = kv._2 })
        val (elmnt, msg) = getElementData(eMap, "fields", count, msgLevel)
        lbuffer += elmnt
        count = count+1
      }
    }
    val msgVersion: String = "0"
    val persistMsg = false
    val mtype = "Container"
    val Fixed = "true"
    val tdataexists = false
    val tdata = null
    val pkg = namespace
    val partitionKeysList = null
    val primaryKeysList = null
    val physicalName: String = namespace + ".V" + MdMgr.ConvertVersionToLong(msgVersion).toString + "." + name
    val cur_time = System.currentTimeMillis
    val msg = new Message(mtype, namespace, name, physicalName, msgVersion, "Description", Fixed, persistMsg, lbuffer.toList, tdataexists, tdata, null, pkg.trim(), null, null, null, partitionKeysList, primaryKeysList, cur_time, msgLevel)

    log.info("child message name " +msg.Name)
    log.info("child message name space" +msg.NameSpace)
    log.info("child message level " +msg.MsgLvel)
    log.info("child msg phisical name " +msg.PhysicalName)
   
    lbuffer.foreach(ele => {
      log.info("Child Field Name " + ele.Name)
      log.info("Child Field Type " + ele.Ttype)
      log.info("Chils Fields Ordinal " +ele.FieldOrdinal)

    })
    return msg

  }

  /**
   * check if the transformation Data from the input message definition
   */
  private def getTransformData(message: scala.collection.mutable.Map[String, Any], tkey: String): TransformData = {
    var iarr: Array[String] = null
    var oarr: Array[String] = null
    var karr: Array[String] = null
    type tMap = Map[String, Any]

    if (message.get(tkey).get.isInstanceOf[tMap]) {
      val tmap: Map[String, Any] = message.get(tkey).get.asInstanceOf[Map[String, Any]]
      for (key <- tmap.keys) {
        if (key.equals("input"))
          iarr = gettData(tmap, key)
        if (key.equals("output"))
          oarr = gettData(tmap, key)
        if (key.equals("keys"))
          karr = gettData(tmap, key)
      }
    }
    new TransformData(iarr, oarr, karr)
  }

  /**
   * Extract the transformation data from message definition
   */
  private def gettData(tmap: Map[String, Any], key: String): Array[String] = {
    type tList = List[String]
    var tlist: List[String] = null
    if (tmap.contains(key) && tmap.get(key).get.isInstanceOf[tList])
      tlist = tmap.get(key).get.asInstanceOf[List[String]]
    tlist.toArray
  }

  // Make sure the version is in the format of nn.nn.nn
  private def extractVersion(message: scala.collection.mutable.Map[String, Any]): String = {
    MdMgr.FormatVersion(message.getOrElse("version", "0").toString)
  }

}