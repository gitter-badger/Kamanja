package com.ligadata.msgcompiler

import org.apache.log4j.Logger;
import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;

class MessageFieldTypesHandler {
  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)
  val tInt: String = "tInt"
  val tChar: String = "tChar"
  val tString: String = "tString"
  val tLong: String = "tLong"
  val tFloat: String = "tFloat"
  val tDouble: String = "tDouble"
  val tBoolean: String = "tBoolean"
  val tArray: String = "tArray"
  val tContainer: String = "tContainer"
  val tScalar: String = "tScalar"

  def handleFieldTypes(message: Message, mdMgr: MdMgr): Message = {

    message.Elements.foreach(field => {
      log.info("fields type " + field.Ttype)
      val typ = MdMgr.GetMdMgr.Type(field.Ttype, -1, true) // message.Version.toLong
      field.FldMetaataType = typ.get
      log.info("******************TYPES FROM METADATA START******************************")

      log.info("type " + typ.get.tType.toString())

      log.info("******************TYPES FROM METADATA START******************************")

    })

    message.Elements.foreach(field => {
      log.info("****************** TYPES FROM METADATA START  --- In Message******************************")

      log.info("=========mesage fld type " + field.Ttype)
      log.info("=========mesage fld metadata type " + field.FldMetaataType.tType.toString())
      log.info("=========mesage fld metadata tTypeType " + field.FldMetaataType.tTypeType.toString())
      log.info("=========mesage fld metadata implementationName " + field.FldMetaataType.implementationName)

      val types = getMetadataTypesForMsgFields(field, mdMgr)
      log.info("=========mesage fld size " + types.size)

      log.info("=========mesage fld 2 :  " + types(1))
      field.FieldTypePhysicalName = types(0)
      log.info("=========mesage fld  " + field.FieldTypePhysicalName)

      log.info("******************TYPES FROM METADATA START --- In Message ******************************")

    })

    return message

  }

  private def getMetadataTypesForMsgFields(field: Element, mdMgr: MdMgr): Array[String] = {

    val fieldBaseType: BaseTypeDef = field.FldMetaataType
    var arrayType: ArrayTypeDef = null
    var types: Array[String] = new Array[String](2);
    val fieldType = fieldBaseType.tType.toString()
    val fieldTypeType = fieldBaseType.tTypeType.toString()
    if (fieldBaseType.isInstanceOf[ArrayTypeDef])
      arrayType = fieldBaseType.asInstanceOf[ArrayTypeDef]

    log.info("fieldTypeType " + fieldTypeType)
    fieldTypeType match {
      case "tScalar" => {
        types(0) = fieldBaseType.PhysicalName
        types(1) = ""
      }
      case "tArray" => {
        types(0) = arrayType.typeString
        types(0) = ""
      }
      case "tContainer" => {
        var ctrDef: ContainerDef = mdMgr.Container(field.Ttype, -1, true).getOrElse(null) //field.FieldtypeVer is -1 for now, need to put proper version
        types(0) = ctrDef.PhysicalName
        types(0) = ""
      }
      case _ => {
        throw new Exception("This types is not handled at this time ") // BUGBUG - Need to handled other cases
      }

    }
    return types
  }
}