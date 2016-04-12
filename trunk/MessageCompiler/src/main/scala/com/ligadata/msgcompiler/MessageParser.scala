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
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import com.ligadata.kamanja.metadata.MdMgr;
import com.ligadata.kamanja.metadata.EntityType;
import com.ligadata.kamanja.metadata.MessageDef;
import com.ligadata.kamanja.metadata.BaseAttributeDef;
import com.ligadata.kamanja.metadata.ContainerDef;
import com.ligadata.kamanja.metadata.ArrayTypeDef;
import java.text.SimpleDateFormat;

class MessageParser {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var ParentMsgNameSpace: String = ""
  val timePartitionTypeList: List[String] = List("yearly", "monthly", "daily") //"weekly", "30minutes", "60minutes", "15minutes", "5minutes", "1minute")
  val timePartitionFormatList: List[String] = List("epochtimeinmillis", "epochtimeinseconds", "epochtime")
  val timePartInfo: String = "TimePartitionInfo"

  /**
   * process the json map and return the message object
   */
  def processJson(json: String, mdMgr: MdMgr, recompile: Boolean = false): Message = {
    var message: Message = null
    var messages: Messages = null
    var msgList: List[Message] = List[Message]()
    var jtype: String = null

    val definition: String = json.replaceAllLiterally("\t", "").trim().replaceAllLiterally(" ", "").trim().replaceAllLiterally("\"", "\\\"").trim();
    log.info("Definition : " + definition)
    val mapOriginal = parse(json).values.asInstanceOf[scala.collection.immutable.Map[String, Any]]

    if (mapOriginal == null)
      throw new Exception("Invalid json data")

    val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
    mapOriginal.foreach(kv => { map(kv._1.trim().toLowerCase()) = kv._2 })

    var key: String = ""
    try {
      jtype = geJsonType(map)
      if (jtype == null || jtype.trim() == "") throw new Exception("The root element of message definition should be wither Message or Container");
      message = processJsonMap(jtype, map, mdMgr, recompile, definition)
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
  private def processJsonMap(key: String, map: scala.collection.mutable.Map[String, Any], mdMgr: MdMgr, recompile: Boolean = false, definition: String): Message = {
    var msgs: Messages = null
    var msg: Message = null
    type messageMap = scala.collection.immutable.Map[String, Any]
    var msgLevel: Int = 0
    try {
      if (map.contains(key)) {
        if (map.get(key).get.isInstanceOf[scala.collection.immutable.Map[_, _]]) {
          val message = map.get(key).get.asInstanceOf[messageMap]
          //log.info("message map" + message)
          val messageMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          message.foreach(kv => { messageMap(kv._1.trim().toLowerCase()) = kv._2 })

          msg = getMsgorCntrObj(messageMap, key, mdMgr, recompile, msgLevel, definition)
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
  private def getMsgorCntrObj(message: scala.collection.mutable.Map[String, Any], mtype: String, mdMgr: MdMgr, recompile: Boolean = false, msgLevel: Int, definition: String): Message = {
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
    var msgVersionLong: Long = 0
    var persistMsg: Boolean = false
    var NameSpace: String = ""
    var Name: String = ""
    var Description: String = ""
    var Fixed: String = ""
    var fldList: Set[String] = Set[String]();
    var timePartition: TimePartition = null;

    try {
      if (message != null) {
        //  val message: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
        //  messageOriginal.foreach(kv => { message(kv._1.toLowerCase()) = kv._2 })

        if (message.getOrElse("namespace", null) == null)
          throw new Exception("Please provide the Name space in the message definition ")

        if (message.getOrElse("name", null) == null)
          throw new Exception("Please provide the Name of the message definition ")

        if (message.getOrElse("version", null) == null)
          throw new Exception("Please provide the Version of the message definition ")

        if (message.getOrElse("fixed", null) == null)
          throw new Exception("Please provide the type of the message definition (either Fixed or Mapped) ")

        Fixed = message.get("fixed").get.toString().trim();

        if (!(Fixed.equalsIgnoreCase("true") || Fixed.equalsIgnoreCase("false")))
          throw new Exception("The message type key \"Fixed\" should be either true or false but not " + Fixed)

        NameSpace = message.get("namespace").get.toString()
        Name = message.get("name").get.toString()

        val persist = message.getOrElse("persist", "false").toString.toLowerCase
        if (MsgUtils.isTrue(MsgUtils.LowerCase(persist)))
          persistMsg = true

        if (message.getOrElse("description", null) == null)
          Description = ""
        else
          Description = message.get("description").get.toString()

        msgVersion = MsgUtils.extractVersion(message)
        msgVersionLong = MdMgr.ConvertVersionToLong(msgVersion)

        for (key: String <- message.keys) {
          if (key.equals("elements") || key.equals("fields")) {
            val (elmnts, msgs) = getElementsObj(message, key)
            elements = elmnts
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
                  throw new Exception("Partition Key Names should be included in fields/elements of message/container definition " + message.get("Name").get.toString())
              }

              if (primaryKeysList != null && primaryKeysList.size > 0) {
                if (!(primaryKeysList.toSet subsetOf fldList))
                  throw new Exception("Primary Key Names should be included in fields/elements of message/container definition " + message.get("Name").get.toString())
              }
            }
          }
        }

        if (MsgUtils.isTrue(MsgUtils.LowerCase(Fixed)) && elements == null)
          throw new Exception("Either Fields or Elements or Concepts  do not exist in " + message.get("name").get.toString())

        if (ele != null && ele.size > 0) {
          ele.foreach(Fld => { fldList += Fld.Name })
        }
        if (message.getOrElse(timePartInfo, null) != null) {
          timePartition = parseTimePartitionInfo(timePartInfo, message, fldList)
        }

        /*
        if (elements != null)
          elements = elements :+ new Element("", "transactionId", "system.long", "", "Fields", null, -1, null, null)
        else
          elements = List(new Element("", "transactionId", "system.long", "", "Fields", null, -1, null, null))
				*/

        // ele.foreach(f => log.debug("====" + f.Name))

        /*
          if (recompile) {
          msgVersion = messageGenUtil.getRecompiledMsgContainerVersion(mtype, NameSpace, Name, mdMgr)
        } else {
          msgVersion = extractVersion(message)
        }
        * 
        */

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
    val pkg = NameSpace + ".V" + MdMgr.ConvertVersionToLong(msgVersion).toString
    val physicalName: String = pkg + "." + Name

    val msg: Message = new Message(mtype, NameSpace, Name, physicalName, msgVersion, msgVersionLong, "Description", Fixed, persistMsg, elements, tdataexists, tdata, null, pkg.trim(), null, null, null, partitionKeysList, primaryKeysList, cur_time, msgLevel, null, null, definition, timePartition, 0)

    var msglist: List[Message] = List[Message]()
    if (messages != null && messages.size > 0)
      messages.foreach(m => {
        if (m.NameSpace == null || m.NameSpace.trim() == "")
          m.NameSpace = NameSpace

        if (m.Version == null || m.Version.trim() == "")
          m.Version = msgVersion

        if (m.Fixed == null || m.Fixed.trim() == "")
          m.Fixed = Fixed

      })

    if (messages != null && messages.size > 0)
      msglist = messages :+ msg
    else
      msglist = msglist :+ msg

    msg
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
        // log.info("Element Map " + message)
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
      if (message.get(key).get == null || message.get(key).get == "None")
        throw new Exception("Elements list do not exist in message/container definition json")

      if (message.get(key).get.isInstanceOf[List[_]]) { //List[Map[String, Any]]]

        val eList = message.get(key).get.asInstanceOf[List[Map[String, Any]]]
        var count: Int = 0
        for (l <- eList.seq) {
          if (l.isInstanceOf[keyMap]) {

            val eMap1: scala.collection.immutable.Map[String, Any] = l.asInstanceOf[scala.collection.immutable.Map[String, Any]]

            val eMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
            eMap1.foreach(kv => { eMap(kv._1.trim().toLowerCase()) = kv._2 })
            if (eMap.contains("field")) {
              val (elmnt, msg) = getElement(eMap, count, msgLevel)
              lbuffer += elmnt
              msgLstbuffer += msg

            } else if (key.equals("fields")) {
              val (elmnt, msg) = getElementData(eMap.asInstanceOf[scala.collection.mutable.Map[String, Any]], key, count, msgLevel)
              lbuffer += elmnt
              if (msg != null)
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

              if (eMap.get(key).get.isInstanceOf[Map[_, _]]) {
                val containerMap: Map[String, Any] = eMap.get(key).get.asInstanceOf[Map[String, Any]]
                val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
                containerMap.foreach(kv => { map(kv._1) = kv._2 })

                val (elmnt, msg) = getElementData(map, key, count, msgLevel)
                lbuffer += elmnt
                // if (msg != null)
                //    msgLstbuffer += msg
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

    /*
    if( msgLstbuffer.toList != null &&  msgLstbuffer.toList.size > 1){
      log.info("#######################  "+ msgLstbuffer.size)
      msgLstbuffer.toList .foreach(m => {  log.info("#######################  "+ m.Name)  })
      
    }*/
    //log.info("#######################  " + msgLstbuffer.size)
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
        if (fldMap != null && fldMap != "None" && fldMap.isInstanceOf[Map[_, _]]) {
          val fldMap1 = fldMap.asInstanceOf[scala.collection.immutable.Map[String, Any]]
          val mapElement: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          fldMap1.foreach(kv => { mapElement(kv._1.trim().toLowerCase()) = kv._2 })
          val (field, msg) = getElementData(mapElement, eKey, ordinal, msgLevel)
          fld = field
          //  message = msg
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
    type FieldMap = Map[String, Any]
    if (fieldMap == null) throw new Exception("element Map is null")

    //  log.info("Field " + fieldMap.toList)
    //  log.info("key ===================== " + key)
    try {
      val field: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
      fieldMap.foreach(kv => { field(kv._1.trim().toLowerCase()) = kv._2 })

      if (field.contains("namespace") && (field.get("namespace").get.isInstanceOf[string]))
        namespace = field.get("namespace").get.asInstanceOf[String]

      if (field.contains("name") && (field.get("name").get.isInstanceOf[String]))
        name = field.get("name").get.asInstanceOf[String].toLowerCase()
      else throw new Exception("Field Name do not exist in " + key)

      if (field.contains("type")) {
        val fieldtype: Any = field.get("type").get
        if (fieldtype.isInstanceOf[string]) {
          val fieldstr = fieldtype.toString.split("\\.")
          if (fieldstr != null) {

            //FIXE ME: Add Field Type Validation....

            if (fieldstr.size == 1) {
              namespace = "system"
              ttype = namespace + "." + fieldtype.asInstanceOf[String].toLowerCase()
            } else if (fieldstr.size == 2) {
              namespace = fieldstr(0).toLowerCase()
              ttype = fieldtype.asInstanceOf[String].toLowerCase()

            } else
              ttype = fieldtype.asInstanceOf[String].toLowerCase()
          }
          if (field.contains("collectionType") && (field.get("collectionType").get.isInstanceOf[string])) {
            collectionType = field.get("collectionType").get.asInstanceOf[String].toLowerCase()
          }

          if (field.contains("version") && (field.get("version").get.isInstanceOf[string])) {
            fldTypeVer = field.get("version").get.asInstanceOf[String].toLowerCase()
          }
          fld = new Element(namespace, name, ttype, collectionType, key, fldTypeVer, ordinal, null, null, null, null, null)

        } else if (fieldtype.isInstanceOf[Map[_, _]]) {
          //  log.info("Child Container ========== Start ==============  ")

          val childFld = fieldtype.asInstanceOf[Map[String, Any]]
          //  log.info("Child Map ************ " + childFld)

          val field: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
          childFld.foreach(kv => { field(kv._1.toLowerCase()) = kv._2 })
          // log.info("child Map" + childFld)
          val (childMsg, childMsgType) = getChildRecord(field, name, namespace, msgLevel + 1)
          childMessage = childMsg
          // msgBuffer += message

          fld = new Element(namespace, name, childMsgType, collectionType, key, fldTypeVer, ordinal, null, null, null, null, null)

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
  private def getChildRecord(childrec: scala.collection.mutable.Map[String, Any], name: String, namespace: String, msgLevel: Int): (Message, String) = {
    var message: Message = null
    var lbuffer = new ListBuffer[Element]
    var count: Int = 0
    var Name: String = ""
    var NameSpace: String = ""
    var childMsgType: String = ""
    val eList = childrec.get("fields").get.asInstanceOf[List[Map[String, Any]]]

    if (childrec.contains("namespace") && (childrec.get("namespace").get.isInstanceOf[String]))
      NameSpace = childrec.get("namespace").get.asInstanceOf[String]

    if (childrec.contains("name") && (childrec.get("name").get.isInstanceOf[String]))
      Name = childrec.get("name").get.asInstanceOf[String].toLowerCase()
    else throw new Exception("Name do not exist in for the child container " + name)

    for (l <- eList) {
      if (l.isInstanceOf[Map[String, Any]]) {
        val eMap1: scala.collection.immutable.Map[String, Any] = l.asInstanceOf[scala.collection.immutable.Map[String, Any]]

        val eMap: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()
        eMap1.foreach(kv => { eMap(kv._1.toLowerCase()) = kv._2 })

        val (elmnt, msg) = getElementData(eMap, "fields", count, msgLevel)
        lbuffer += elmnt
        count = count + 1
      }
    }
    if (NameSpace == null || NameSpace.trim() == "")
      NameSpace = ParentMsgNameSpace
    val msgVersion: String = "0"
    val persistMsg = false
    val mtype = "Container"
    val Fixed = "true"
    val tdataexists = false
    val tdata = null
    val pkg = NameSpace + ".V" + MdMgr.ConvertVersionToLong(msgVersion).toString
    val partitionKeysList = null
    val primaryKeysList = null
    val physicalName: String = pkg + "." + Name
    val cur_time = System.currentTimeMillis
    val msg = new Message(mtype, NameSpace, Name, physicalName, msgVersion, 0, "Description", Fixed, persistMsg, lbuffer.toList, tdataexists, tdata, null, pkg.trim(), null, null, null, partitionKeysList, primaryKeysList, cur_time, msgLevel, null, null, null, null, 0)

    /*log.info("child message level " + msg.MsgLvel)
    log.info("child message name " + msg.Name)
    log.info("child message name space" + msg.NameSpace)
    log.info("child message level " + msg.MsgLvel)
    log.info("child msg phisical name " + msg.PhysicalName)

    lbuffer.foreach(ele => {
      log.info("Child Field Name " + ele.Name)
      log.info("Child Field Type " + ele.Ttype)
      log.info("Chils Fields Ordinal " + ele.FieldOrdinal)

    })
    log.info("child message name " + Name)
    * 
    */
    childMsgType = NameSpace + "." + Name
    return (msg, childMsgType)

  }

  /**
   * check if the transformation Data from the input message definition
   */
  private def getTransformData(message: scala.collection.mutable.Map[String, Any], tkey: String): TransformData = {
    var iarr: Array[String] = null
    var oarr: Array[String] = null
    var karr: Array[String] = null
    type tMap = Map[String, Any]

    if (message.get(tkey).get.isInstanceOf[Map[_, _]]) {
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
    if (tmap.contains(key) && tmap.get(key).get.isInstanceOf[List[_]])
      tlist = tmap.get(key).get.asInstanceOf[List[String]]
    tlist.toArray
  }

  /*
   * parse timepartitionInfo
   */
  private def parseTimePartitionInfo(key: String, message: scala.collection.mutable.Map[String, Any], fldList: Set[String]): TimePartition = {
    var timePartitionKey: String = null
    var timePartitionKeyFormat: String = null
    var timePartitionType: String = null
    type sMap = Map[String, String]
    try {

      if (message.getOrElse(key, null) != null && message.get(key).get.isInstanceOf[Map[_, _]]) {
        val timePartitionMap: sMap = message.get(key).get.asInstanceOf[sMap]

        if (timePartitionMap.contains("Key") && (timePartitionMap.get("Key").get.isInstanceOf[String])) {
          timePartitionKey = timePartitionMap.get("Key").get.asInstanceOf[String].toLowerCase()

          if (!fldList.contains(timePartitionKey))
            throw new Exception("Time Partition Key " + timePartitionKey + " should be defined as one of the fields in the message definition");

        } else throw new Exception("Time Partition Key should be defined in the message definition");

        if (timePartitionMap.contains("Format") && (timePartitionMap.get("Format").get.isInstanceOf[String])) {
          timePartitionKeyFormat = timePartitionMap.get("Format").get.asInstanceOf[String] //.toLowerCase()

          if (!validateTimePartitionFormat(timePartitionKeyFormat))
            throw new Exception("Time Parition format given in message definition " + timePartitionKeyFormat + " is not a valid format");
        } else throw new Exception("Time Partition Format should be defined in the message definition");

        if (timePartitionMap.contains("Type") && (timePartitionMap.get("Type").get.isInstanceOf[String])) {
          timePartitionType = timePartitionMap.get("Type").get.asInstanceOf[String].toLowerCase()

          if (!containsIgnoreCase(timePartitionTypeList, timePartitionType))
            throw new Exception("Time Parition Type " + timePartitionType + " defined in the message definition is not a valid Type");
        } else throw new Exception("Time Partition Type should be defined in the message definition");

      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }

    new TimePartition(timePartitionKey, timePartitionKeyFormat, timePartitionType);

  }
  //Validate the Time Partition format 
  private def validateTimePartitionFormat(format: String): Boolean = {

    if (containsIgnoreCase(timePartitionFormatList, format))
      return true;
    else return validateDateTimeFormat(format)

    return false
  }

  //Validate if the date time format is valid SimpleDateFormat

  private def validateDateTimeFormat(format: String): Boolean = {
    try {
      new SimpleDateFormat(format);
      return true
    } catch {
      case e: Exception => {
        log.debug("", e)
        return false
      }
    }
    return false
  }
  def containsIgnoreCase(list: List[String], s: String): Boolean = {

    list.foreach(l => {
      if (l.equalsIgnoreCase(s))
        return true;
    })
    return false;
  }

}