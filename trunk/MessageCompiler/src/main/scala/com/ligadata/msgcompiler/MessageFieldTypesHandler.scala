package com.ligadata.msgcompiler

import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;
import com.ligadata.Exceptions._;

class MessageFieldTypesHandler {
  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
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

  //Handling the field types - get the metadata type for each field 
  def handleFieldTypes(message: Message, mdMgr: MdMgr): Message = {
    var argsList: List[(String, String, String, String, Boolean, String)] = List[(String, String, String, String, Boolean, String)]()
    var jarset: Set[String] = Set[String]();
    if (message.Elements == null && message.Fixed.equalsIgnoreCase("true"))
      throw new Exception("Please provide the fields in the message definition since fields are mandatory for Fixed Message");
    if (message.Elements != null) {
      message.Elements.foreach(field => {
        if (field != null) {
          log.info("field.Ttype =================" + field.Ttype);

          val typ = MdMgr.GetMdMgr.Type(field.Ttype, -1, true) // message.Version.toLong

          if (typ.getOrElse("None").equals("None"))
            throw new Exception("Type not found in metadata for " + field.Name + " given type is " + field.Ttype)

          // to do - check null pointer if type is not avaialble.....
          field.FldMetaataType = typ.get

          val types = getMetadataTypesForMsgFields(field, mdMgr)
          field.FieldTypePhysicalName = types(0)
          field.FieldTypeImplementationName = types(1)

          //get the fields jarset for adding msg in the metadata
          jarset = jarset ++ getDependencyJarSet(field.FldMetaataType)

          //get the fields args list for addding message in the metadata 
          argsList = (field.NameSpace, field.Name, field.FldMetaataType.NameSpace, field.FldMetaataType.Name, false, null) :: argsList

          log.info("******************TYPES FROM METADATA START******************************")
          log.info("type " + typ.get.tType.toString())
          log.info("fields name " + field.Name)
          log.info("fields type " + field.Ttype)
          log.info("******************TYPES FROM METADATA START******************************")
        }
      })
    }

    /*message.Elements.foreach(field => {
      
      log.info("*************==================================== " + field.FieldTypeImplementationName);      
      log.info("****************** TYPES FROM METADATA START  --- In Message******************************")       
      log.info("=========mesage fld type " + field.Ttype)
      log.info("=========mesage fld metadata type " + field.FldMetaataType.tType.toString())
      log.info("=========mesage fld metadata tTypeType " + field.FldMetaataType.tTypeType.toString())
      log.info("=========mesage fld metadata implementationName " + field.FldMetaataType.implementationName)
      log.info("=========mesage fld size " + types.size)
      log.info("=========mesage fld 2 :  " + types(1))
      log.info("=========mesage fld  " + field.FieldTypePhysicalName)
      log.info("******************TYPES FROM METADATA End --- In Message ******************************")

    })
*/
    // set the field args list and jarset in message object to retrieve while adding mesage to metadata
    message.ArgsList = argsList
    message.Jarset = jarset

    return message
  }

  /*
   * Get the Dependecy Jar Set for each field
   */

  private def getDependencyJarSet(fieldBaseType: BaseTypeDef): Set[String] = {

    var jarset: Set[String] = Set[String]();

    if ((fieldBaseType.dependencyJarNames != null) && (fieldBaseType.JarName != null))
      jarset = jarset + fieldBaseType.JarName ++ fieldBaseType.dependencyJarNames
    else if (fieldBaseType.JarName != null)
      jarset = jarset + fieldBaseType.JarName
    else if (fieldBaseType.dependencyJarNames != null)
      jarset = jarset ++ fieldBaseType.dependencyJarNames

    return jarset

  }

  /*
   * Get MetadataType based on the type of field
   */

  private def getMetadataTypesForMsgFields(field: Element, mdMgr: MdMgr): Array[String] = {

    val fieldBaseType: BaseTypeDef = field.FldMetaataType
    var types: Array[String] = new Array[String](2);
    val fieldType = fieldBaseType.tType.toString().toLowerCase()

    val fieldTypeType = fieldBaseType.tTypeType.toString().toLowerCase()
    var arrayType: ArrayTypeDef = null
    if (fieldBaseType.isInstanceOf[ArrayTypeDef])
      arrayType = fieldBaseType.asInstanceOf[ArrayTypeDef]

    log.info("fieldTypeType " + fieldTypeType)
    log.info("fieldBaseType 1 " + fieldBaseType.tType)
    log.info("fieldBaseType 2 " + fieldBaseType.typeString)
    log.info("fieldBaseType 3" + fieldBaseType.tTypeType)

    // log.info("fieldType " + fieldType)

    fieldTypeType match {
      case "tscalar" => {
        types(0) = fieldBaseType.PhysicalName
        types(1) = fieldBaseType.implementationName
        //  log.info("fieldBaseType.implementationName    " + fieldBaseType.implementationName)

      }
      case "tcontainer" => {
        fieldType match {
          case "tarray" => {
            var arrayType: ArrayTypeDef = null
            arrayType = fieldBaseType.asInstanceOf[ArrayTypeDef]
            types(0) = arrayType.typeString
            types(1) = arrayType.elemDef.implementationName
            log.info("!!!!!!!!!!!!!!!!!!!!!!!!" + types(1) + "........ " + types(0) + "...... " + arrayType.elemDef.PhysicalName)

          }
          case "tstruct" => {
            var ctrDef: ContainerDef = mdMgr.Container(field.Ttype, -1, true).getOrElse(null) //field.FieldtypeVer is -1 for now, need to put proper version
            types(0) = ctrDef.PhysicalName
            types(1) = ctrDef.FullName
            log.info(ctrDef.objectDefinition);
          }
          case "tmsgmap" => {
            var ctrDef: ContainerDef = mdMgr.Container(field.Ttype, -1, true).getOrElse(null) //field.FieldtypeVer is -1 for now, need to put proper version
            types(0) = ctrDef.PhysicalName
            types(1) = ctrDef.FullName
            log.info(ctrDef.objectDefinition);
          }
          case "tmap" => {
            var maptypeDef: MapTypeDef = null;
            maptypeDef = fieldBaseType.asInstanceOf[MapTypeDef]
            types(0) = maptypeDef.typeString
            types(1) = maptypeDef.valDef.implementationName
            log.info(types(1) + ".......... " + types(0))
            log.info(maptypeDef.valDef.tType)
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
    return types
  }

  /*
   * Get the arguments list of all fields for the message
   */

}