package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;
import scala.collection.mutable.ArrayBuffer

class ConversionFuncGenerator {
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  val messageStr = "message";
  val containerStr = "container";
  var msgConstants = new MessageConstants
  val prevVerTypMatchKeys = "prevVerTypMatchKeys";
  val prevVerTypesNotMatch = "prevVerTypsNotMatchKeys";

  /*
   * Get Previous version msg
   */
  def getPrevVersionMsg(message: Message, mdMgr: MdMgr): String = {
    var prevVerMsgObjstr: String = ""
    var prevMsgConvCase: String = ""
    var prevVerMsgBaseTypesIdxArry = new ArrayBuffer[String]
    val Fixed = msgConstants.isFixedFunc(message);
    val isMsg = msgConstants.isMessageFunc(message);
    val msgdefArray = getPrevVersionMsgContainers(message, mdMgr);
    var prevVerCaseStmts = new StringBuilder(8 * 1024)
    var prevVerConvFuncs = new StringBuilder(8 * 1024)

    if (msgdefArray == null) return null;
    prevVerCaseStmts.append(generatePrevVerCaseStmts(message.PhysicalName, message.VersionLong.toString()))
    msgdefArray.foreach(msgdef => {
      //call the function which generates the complete conversion function and also another string with case stmt and append to string buffer 
      if (msgdef != null) {
        val (caseStmt, convertFunc) = getconversionFunc(msgdef, isMsg, Fixed, message, mdMgr)
        // get the case stmsts and put it in array of case stmsnts        
        prevVerCaseStmts.append(caseStmt)
        prevVerConvFuncs.append(convertFunc)
      }
    })

    //put array of case stmts in fucnction  and generate main conversion func
    val ConversionStr = ConversionFunc(message, prevVerCaseStmts.toString())
    var conversion: String = ""
    if (Fixed)
      conversion = ConversionStr + generateConvToCurrentVer(message) + prevVerConvFuncs.toString
    else conversion = ConversionStr + generateConvToCurrentVer(message) + prevVerConvFuncs.toString + msgConstants.typeConversion;

    //append the prev conversion funcs to this string buffer and return string 
    return conversion
  }

  /*
   * Get Conversion Func for each prev version 
   */
  private def getconversionFunc(msgdef: ContainerDef, isMsg: Boolean, fixedMsg: Boolean, message: Message, mdMgr: MdMgr): (String, String) = {
    var attributes: Map[String, Any] = Map[String, Any]()
    var caseStmt: String = "";
    var conversionFunc: String = "";
    try {
      if (msgdef != null) {
        val childAttrs = getPrevVerMsgAttributes(msgdef, isMsg, fixedMsg)
        attributes = childAttrs
        // attributes.foreach(a => println(a._1 + "========" + a._2.asInstanceOf[AttributeDef].aType.implementationName))
        // generate the previous version match keys and prevVer keys do not match 
        conversionFunc = generateConvToPrevObjsFunc(message, mdMgr, attributes, fixedMsg, msgdef)
        caseStmt = generatePrevVerCaseStmts(msgdef.PhysicalName, msgdef.Version.toString())

        //generate the whole functtion

        /* if ((msgdef.dependencyJarNames != null) && (msgdef.JarName != null))
          message.Jarset = message.Jarset + pMsgdef.JarName ++ pMsgdef.dependencyJarNames
        else if (pMsgdef.JarName != null)
          message.Jarset = message.Jarset + pMsgdef.JarName
        else if (pMsgdef.dependencyJarNames != null)
          message.Jarset = message.Jarset ++ pMsgdef.dependencyJarNames*/
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
      }
    }
    (caseStmt, conversionFunc)
  }

  /*
   * Get the previous version messages from metadata if exists 
   */
  private def getPrevVersionMsgContainers(message: Message, mdMgr: MdMgr): Array[ContainerDef] = {
    var msgdefArray = new scala.collection.mutable.ArrayBuffer[ContainerDef]
    var cntrdefobjs: Option[scala.collection.immutable.Set[ContainerDef]] = null
    var msgdefobjs: Option[scala.collection.immutable.Set[_]] = null

    var prevVerMsgObjstr: String = ""
    var childs: ArrayBuffer[(String, String)] = ArrayBuffer[(String, String)]()
    var isMsg: Boolean = false
    try {
      val messagetype = message.MsgType
      val namespace = message.NameSpace
      val name = message.Name
      val ver = message.Version
      var ctr: Option[ContainerDef] = null;

      if (namespace == null || namespace.trim() == "")
        throw new Exception("Proper Namespace do not exists in message/container definition")
      if (name == null || name.trim() == "")
        throw new Exception("Proper Name do not exists in message")
      if (ver == null || ver.trim() == "")
        throw new Exception("Proper Version do not exists in message/container definition")

      if (messagetype != null && messagetype.trim() != "") {
        if (messagetype.equalsIgnoreCase(messageStr)) {
          msgdefobjs = mdMgr.Messages(namespace, name, false, false)
          val isMsg = true
        } else if (messagetype.equalsIgnoreCase(containerStr)) {
          msgdefobjs = mdMgr.Containers(namespace, name, false, false)
        }

        if (msgdefobjs != null) {
          msgdefobjs match {
            case None => {
              return null
            }
            case Some(m) =>
              {
                if (isMsg)
                  m.foreach(msgdef => msgdefArray += msgdef.asInstanceOf[MessageDef])
                else
                  m.foreach(msgdef => msgdefArray += msgdef.asInstanceOf[ContainerDef]) // val fullname = msgdef.FullNameWithVer.replaceAll("[.]", "_")           // prevVerMsgObjstr = msgdef.PhysicalName
              }
              return msgdefArray.toArray
          }
        }
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
      }
    }
    return null;

  }

  /*
   * Get attributes from previous message
   */
  private def getPrevVerMsgAttributes(pMsgdef: ContainerDef, isMsg: Boolean, fixed: Boolean): (Map[String, Any]) = {
    var prevVerCtrdef: ContainerDef = new ContainerDef()
    var prevVerMsgdef: MessageDef = new MessageDef()
    var attributes: Map[String, Any] = Map[String, Any]()

    if (pMsgdef != null) {
      if (isMsg) {
        prevVerCtrdef = pMsgdef.asInstanceOf[MessageDef]
      } else {
        prevVerCtrdef = pMsgdef.asInstanceOf[ContainerDef]
      }
      if (fixed) {
        val memberDefs = prevVerCtrdef.containerType.asInstanceOf[StructTypeDef].memberDefs
        if (memberDefs != null) {
          attributes ++= memberDefs.filter(a => (a.isInstanceOf[AttributeDef])).map(a => (a.Name, a))
        }
      } else {
        val attrMap = prevVerCtrdef.containerType.asInstanceOf[MappedMsgTypeDef].attrMap
        if (attrMap != null) {
          attributes ++= attrMap.filter(a => (a._2.isInstanceOf[AttributeDef])).map(a => (a._2.Name, a._2))
        }
      }
    }
    (attributes)
  }

  /*
   * Generating Conversion Function for all fields
   */
  private def ConversionFunc(message: Message, prevMsgConvCase: String) = {
    val msgName = message.PhysicalName

    """
    def convertPrevVersionToCurVersion(oldVerobj: Any): """ + msgName + """ = {
      try {
        oldVerobj match {
          """ + prevMsgConvCase + """
          case _ => {
            throw new Exception("Unhandled Version Found");
          }
        }
      } catch {
        case e: Exception => {
          log.debug("", e)
          throw e
        }
      }
      return null;
    }
  """
  }

  /*
   *generate the case stmt for current vrsion 
   */
  private def generateConvToCurrentVer(message: Message) = {
    val version = message.VersionLong.toString()
    """
    private def convertToVer""" + version + """(oldVerobj: """ + message.PhysicalName + """): """ + message.PhysicalName + """= {
      return oldVerobj
    }
  
    """
  }

  /*
   * Generate the case Stmts for prevobjects conversion
   */
  private def generatePrevVerCaseStmts(msgPhyicalName: String, version: String): String = {
    """
      case oldVerobj: """ + msgPhyicalName + """ => { return  convertToVer""" + version + """(oldVerobj); } """
  }

  /*
   * generate conversion Func
   */
  private def generateConvToPrevObjsFunc(message: Message, mdMgr: MdMgr, attributes: Map[String, Any], fixedMsg: Boolean, prevMsgdef: ContainerDef): String = {
    var convStmtArray = Array[String]();
    var genPrevVerTypMatchKeys: String = ""
    var genPrevVerTypNotMatchKeys: String = ""
    var conversionFunc: String = ""
    try {
      convStmtArray = CheckFieldsWithPrevObjs(message, mdMgr, attributes, fixedMsg)
      //generate prevtyper match keys variable and generate prevTypeNotmatchKeys Variable
      if (fixedMsg) {
        conversionFunc = ConvertToPreVersionFixedFunc(convStmtArray(2), message, prevMsgdef)
      } else {
        genPrevVerTypMatchKeys = getMappedMsgPrevVerKeys(convStmtArray(0), prevVerTypMatchKeys)
        genPrevVerTypNotMatchKeys = getMappedMsgPrevVerKeys(convStmtArray(1), prevVerTypesNotMatch)
        conversionFunc = convStmtArray(2)
       // log.info("genPrevVerTypMatchKeys " + genPrevVerTypMatchKeys)
       // log.info("genPrevVerTypNotMatchKeys " + genPrevVerTypNotMatchKeys)

        conversionFunc = ConvertToPreVersionMappedFunc(genPrevVerTypMatchKeys, genPrevVerTypNotMatchKeys, message, prevMsgdef, conversionFunc)
      }
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return conversionFunc;

  }

  private def ConvertToPreVersionFixedFunc(convFuncStr: String, message: Message, prevMsgdef: ContainerDef): String = {
    val currentMsgPhysicalName: String = message.PhysicalName
    val prevVerMsgPhysicalName: String = prevMsgdef.PhysicalName
    val prevVersion = prevMsgdef.Version.toString()

    """
      private def convertToVer""" + prevVersion + """(oldVerobj: """ + prevVerMsgPhysicalName + """): """ + currentMsgPhysicalName + """= {
        var newVerObj = new """ + currentMsgPhysicalName + """(factory)
    """ + convFuncStr + """  
      return newVerObj
      }
      
   """
  }

  /*
   * Conversion function in mapped mag
   */
  private def ConvertToPreVersionMappedFunc(genPrevVerTypMatchKeys: String, genPrevVerTypNotMatchKeys: String, message: Message, prevMsgdef: ContainerDef, genConvForMsgsAndCntrs: String): String = {
    val currentMsgPhysicalName: String = message.PhysicalName
    val prevVerMsgPhysicalName: String = prevMsgdef.PhysicalName
    val prevVersion = prevMsgdef.Version.toString()
    //   val genConvForMsgsAndCntrs = ""
    """
    private def convertToVer""" + prevVersion + """(oldVerobj: """ + prevVerMsgPhysicalName + """): """ + currentMsgPhysicalName + """= {
      var newVerObj = new """ + currentMsgPhysicalName + """(factory)
       """ + genPrevVerTypMatchKeys + genPrevVerTypNotMatchKeys + """
      val iter = oldVerobj.valuesMap.entrySet().iterator();
      val primitives = Array("string", "int", "float", "double", "long", "char", "any");

      while (iter.hasNext()) {
        //Name and Base Types Match
        val key = iter.next().getKey.toLowerCase
        val valueType = iter.next().getValue.getValueType.toLowerCase
        val value = iter.next().getValue.getValue
        val attributeVal = iter.next().getValue

        if (prevVerTypMatchKeys.contains(key)) {
          if (primitives.contains(valueType)) {
            newVerObj.valuesMap.put(key, attributeVal);
          } else {
            //   handle the other types here
            key match {
              """ + genConvForMsgsAndCntrs + """

              case _ => { newVerObj.valuesMap.put(key, attributeVal); }
            }
          }
        } else if (prevVerTypsNotMatchKeys.contains(key)) { //Name Match and Base Types Not Match

          val attributeVal = typeConv(keyTypes(key), iter.next().getValue.getValue)
          newVerObj.valuesMap.put(key, attributeVal);
          // handle for complex types

        } else if (!(prevVerTypMatchKeys.contains(key) && prevVerTypsNotMatchKeys.contains(key))) { //////Extra Fields in Prev Ver Obj

          if (primitives.contains(valueType)) {
            val attributeVal = typeConv(valueType, value)
            newVerObj.valuesMap.put(key, attributeVal);

          } else { // if extra fields are not primitives
            var attributeValue: AttributeValue = new AttributeValue();
            attributeValue.setValueType("string")
            attributeValue.setValue(com.ligadata.BaseTypes.StringImpl.Input(value.asInstanceOf[String]))
            newVerObj.valuesMap.put(key, attributeValue);
          }
        }
      }
      return newVerObj;
    }
      
  """
  }

  /*
   * Generate Prevversion match or not match keys variable
   */
  def getMappedMsgPrevVerKeys(matchKeys: String, prevMatchStr: String): String = {

    if (matchKeys != null && matchKeys.trim != "")
      "val " + prevMatchStr + "= Array(" + matchKeys.toString.substring(0, matchKeys.toString.length - 1) + ") \n "
    else
      "val " + prevMatchStr + " = Array(\"\") \n "
  }

  /*
   * generate FromFunc code for message fields 
   */
  private def CheckFieldsWithPrevObjs(message: Message, mdMgr: MdMgr, attributes: Map[String, Any], fixedMsg: Boolean): Array[String] = {
    var conversionFuncBuf = new StringBuilder(8 * 1024)
    var returnStmts = new ArrayBuffer[String]
    var mappedPrevVerMatchkeys = new StringBuilder(8 * 1024)
    var mappedPrevTypNotMatchkeys = new StringBuilder(8 * 1024)
    var convFuncStr = new StringBuilder(8 * 1024)
    try {
      if (message.Elements != null) {
        message.Elements.foreach(field => {
          if (field != null) {

            val fieldBaseType: BaseTypeDef = field.FldMetaataType
            if (fieldBaseType == null)
              throw new Exception("Type not found in metadata for Name: " + field.Name + " , NameSpace: " + field.NameSpace + " , Type : " + field.Ttype)
            if (field.Name == null || field.Name.trim() == "")
              throw new Exception("Field name do not exists")
            if (fieldBaseType.FullName == null || fieldBaseType.FullName.trim() == "")
              throw new Exception("Full name of Type " + field.Ttype + " do not exists in metadata ")

            val fieldType = fieldBaseType.tType.toString().toLowerCase()
            val fieldTypeType = fieldBaseType.tTypeType.toString().toLowerCase()
            fieldTypeType match {
              case "tscalar" => {
                val conversionFunc = ConversionFuncForScalar(field, attributes, fixedMsg, fieldBaseType) // do nothing already added 
                mappedPrevVerMatchkeys = mappedPrevVerMatchkeys.append(conversionFunc(0))
                mappedPrevTypNotMatchkeys = mappedPrevTypNotMatchkeys.append(conversionFunc(1))
                convFuncStr = convFuncStr.append(conversionFunc(2))
              }
              case "tcontainer" => {
                fieldType match {
                  case "tarray" => {
                    var arrayType: ArrayTypeDef = null
                    arrayType = fieldBaseType.asInstanceOf[ArrayTypeDef]
                    val conversionFunc = ConversionFuncForArray(field, attributes, fixedMsg, arrayType)
                    conversionFuncBuf = conversionFuncBuf.append()
                    mappedPrevVerMatchkeys = mappedPrevVerMatchkeys.append(conversionFunc(0))
                    mappedPrevTypNotMatchkeys = mappedPrevTypNotMatchkeys.append(conversionFunc(1))
                    convFuncStr = convFuncStr.append(conversionFunc(2))
                  }
                  case "tstruct" => {
                    var ctrDef: ContainerDef = mdMgr.Container(field.Ttype, -1, true).getOrElse(null) //field.FieldtypeVer is -1 for now, need to put proper version
                    if (ctrDef != null) {
                      val conversionFunc = ConversionFuncForStruct(field, ctrDef, fixedMsg, attributes)
                      mappedPrevVerMatchkeys = mappedPrevVerMatchkeys.append(conversionFunc(0))
                      mappedPrevTypNotMatchkeys = mappedPrevTypNotMatchkeys.append(conversionFunc(1))
                      convFuncStr = convFuncStr.append(conversionFunc(2))
                    }
                  }
                  case "tmap" => {
                    conversionFuncBuf = conversionFuncBuf.append(ConversionFuncForMap(field))

                  }
                  case "tmsgmap" => {
                    var ctrDef: ContainerDef = mdMgr.Container(field.Ttype, -1, true).getOrElse(null) //field.FieldtypeVer is -1 for now, need to put proper version
                    if (ctrDef != null) {
                      val conversionFunc = ConversionFuncForStruct(field, ctrDef, fixedMsg, attributes)
                      mappedPrevVerMatchkeys = mappedPrevVerMatchkeys.append(conversionFunc(0))
                      mappedPrevTypNotMatchkeys = mappedPrevTypNotMatchkeys.append(conversionFunc(1))
                      convFuncStr = convFuncStr.append(conversionFunc(2))
                    }
                  }
                  case _ => {
                    throw new Exception("This types is not handled at this time ") // BUGBUG - Need to handled other cases
                  }
                }
              }
              case _ => {
                throw new Exception("This types is not handled at this time ") // BUGBUG - Need to handled other cases
              }
            }
          }
        })
      }
      //log.info("convFuncStr " + convFuncStr.toString())
      returnStmts += mappedPrevVerMatchkeys.toString()
      returnStmts += mappedPrevTypNotMatchkeys.toString()
      returnStmts += convFuncStr.toString()
    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return returnStmts.toArray
  }

  /*
   * Handlie Array
   */
  private def ConversionFuncForArray(field: Element, attributes: Map[String, Any], fixedMsg: Boolean, arrayTypDef: ArrayTypeDef): String = {
    return null
  }

  private def ConversionFuncForStruct(field: Element, ctrDef: ContainerDef, fixedMsg: Boolean, attributes: Map[String, Any]): Array[String] = {
    var convPrevVerStr = new StringBuilder(8 * 1024)
    var mappedConvPrevVerStr = new StringBuilder(8 * 1024)
    var mappedPrevVerMatchkeys = new StringBuilder(8 * 1024)
    var mappedPrevTypNotrMatchkeys = new StringBuilder(8 * 1024)
    var returnStmts = new ArrayBuffer[String]

    try {
      val childCtrFullame = ctrDef.FullName
      val childCtrPhysicalName = ctrDef.PhysicalName
      val (mbrExists, sameTyp, mbrMatchTypNotMatch) = AtrributesTypeMatchCheck(attributes, field.Name, childCtrFullame, true, childCtrPhysicalName)
      var memberExists = mbrExists
      var sameType = sameTyp
      var membrMatchTypeNotMatch = mbrMatchTypNotMatch
      if (memberExists) {
        if (fixedMsg) {
          convPrevVerStr = convPrevVerStr.append("%s{%s%sval curVerObj = new %s()%s".format(msgConstants.pad3, msgConstants.newline, msgConstants.pad3, ctrDef.typeString, msgConstants.newline))
          convPrevVerStr = convPrevVerStr.append("%scurVerObj.convertPrevVersionToCurVersion(oldVerobj.%s)%s".format(msgConstants.pad3, field.Name, msgConstants.newline))
          convPrevVerStr = convPrevVerStr.append("%sthis.%s = curVerObj}%s".format(msgConstants.pad3, field.Name, msgConstants.newline))
        } else {
          mappedPrevVerMatchkeys.append("\"" + field.Name + "\",")
          convPrevVerStr = convPrevVerStr.append("%s case \"%s\" => { %s".format(msgConstants.pad2, field.Name, msgConstants.newline))
          convPrevVerStr = convPrevVerStr.append("%s  newVerObj.valuesMap(\"%s\") =  oldVerobj.valuesMap(\"%s\")  }}%s".format(msgConstants.pad2, field.Name, msgConstants.newline))
          if (sameType) {

          } else {
            convPrevVerStr = convPrevVerStr.append("%s{%s%sval curVerObj = new %s()%s".format(msgConstants.pad2, msgConstants.newline, msgConstants.pad2, ctrDef.typeString, msgConstants.newline))
            convPrevVerStr = convPrevVerStr.append("%scurVerObj.convertPrevVersionToCurVersion(prevObjfield._2._2)%s".format(msgConstants.pad2, field.Name, msgConstants.newline))
            convPrevVerStr = convPrevVerStr.append("%s fields(\"%s\") = (prevObjfield._2._1,curVerObj) }%s".format(msgConstants.pad2, field.Name, msgConstants.newline))
          }
        }
      }
      if (membrMatchTypeNotMatch) {
        mappedPrevTypNotrMatchkeys.append("\"" + field.Name + "\",")
      }

      returnStmts += mappedPrevVerMatchkeys.toString()
      returnStmts += mappedPrevTypNotrMatchkeys.toString()
      returnStmts += convPrevVerStr.toString()

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    return returnStmts.toArray
  }

  private def ConversionFuncForMap(field: Element): String = {
    return null
  }

  private def ConversionFuncForScalar(field: Element, attributes: Map[String, Any], fixedMsg: Boolean, fieldBaseType: BaseTypeDef): Array[String] = {
    var mappedPrevVerMatchkeys = new StringBuilder(8 * 1024)
    var mappedPrevTypNotrMatchkeys = new StringBuilder(8 * 1024)
    var filedStrBuf = new StringBuilder(8 * 1024)
    var convPrevVerStr = new StringBuilder(8 * 1024)
    var returnStmts = new ArrayBuffer[String]

    try {

      val (mbrExists, sameTyp, mbrMatchTypNotMatch) = AtrributesTypeMatchCheck(attributes, field.Name, fieldBaseType.FullName, false, null)
      var memberExists = mbrExists
      var sameType = sameTyp
      var membrMatchTypeNotMatch = mbrMatchTypNotMatch
      if (memberExists) {
        if (fixedMsg) {
          //add fixed stuff
          convPrevVerStr = convPrevVerStr.append("%snewVerObj.%s = oldVerobj.%s; %s".format(msgConstants.pad3, field.Name, field.Name, msgConstants.newline))

        } else {
          mappedPrevVerMatchkeys.append("\"" + field.Name + "\",")
        }
      }
      if (membrMatchTypeNotMatch) {
        if (!fixedMsg) {
          mappedPrevTypNotrMatchkeys = mappedPrevTypNotrMatchkeys.append("\"" + field.Name + "\",")
        }
      }

    } catch {
      case e: Exception => {
        log.debug("", e)
        throw e
      }
    }
    returnStmts += mappedPrevVerMatchkeys.toString()
    returnStmts += mappedPrevTypNotrMatchkeys.toString()
    returnStmts += convPrevVerStr.toString()
    return returnStmts.toArray
  }

  /*
   * Check for attributes type match
   */
  private def AtrributesTypeMatchCheck(attributes: Map[String, Any], fieldName: String, fieldTypeFullName: String, isMsgorCtr: Boolean, curObjtype: String): (Boolean, Boolean, Boolean) = {
    var memberExists: Boolean = false
    var membrMatchTypeNotMatch = false // for mapped messages to handle if prev ver obj and current version obj member types do not match...
    var childTypeImplName: String = ""
    var childtypeName: String = ""
    var childtypePhysicalName: String = ""
    var sameType: Boolean = false // same type check valid only container as child msg

    if (attributes != null) {
      if (attributes.contains(fieldName)) {
        var child = attributes.getOrElse(fieldName, null)
        if (child != null) {
          val typefullname = child.asInstanceOf[AttributeDef].aType.FullName
          childtypeName = child.asInstanceOf[AttributeDef].aType.tTypeType.toString
          childtypePhysicalName = child.asInstanceOf[AttributeDef].aType.physicalName
          if (typefullname != null && typefullname.trim() != "" && typefullname.equals(fieldTypeFullName)) {
            memberExists = true
            if (isMsgorCtr) {
              val childPhysicalName = child.asInstanceOf[AttributeDef].aType.typeString
              if (childPhysicalName != null && childPhysicalName.trim() != "") {
                if (curObjtype != null && curObjtype.trim() != "" && childPhysicalName.equals(curObjtype))
                  sameType = true
              }
            }
          } else {
            membrMatchTypeNotMatch = true
          }
        }
      }
    }
    return (memberExists, sameType, membrMatchTypeNotMatch)
  }

}