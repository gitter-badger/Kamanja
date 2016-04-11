package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.logging.log4j.{ Logger, LogManager }
import com.ligadata.kamanja.metadata._;

class MessageBuilderGenerator {

  val logger = this.getClass.getName
  lazy val log = LogManager.getLogger(logger)
  var msgConstants = new MessageConstants
  var mappedMsgGenerator = new MappedMsgGenerator()
  val newline: String = "\n"
  val pad1: String = "\t"
  val pad2: String = "\t\t"
  val closeBrace = "}"

  def generatorBuilder(message: Message, mdMgr: MdMgr): String = {
    var builderGenerator = new StringBuilder(8 * 1024)
    try {
      builderGenerator = builderGenerator.append(builderClassGen(message) + newline)
      if (message.Fixed.equalsIgnoreCase("true")) {
        builderGenerator = builderGenerator.append(newline + generatedBuilderVariables(message))
        builderGenerator = builderGenerator.append(getFuncGeneration(message.Elements))
        builderGenerator = builderGenerator.append(setFuncGeneration(message.Elements))
      } else if (message.Fixed.equalsIgnoreCase("false")) {
        var fieldIndexMap: Map[String, Int] = msgConstants.getScalarFieldindex(message.Elements)
        builderGenerator = builderGenerator.append(msgConstants.newline + msgConstants.pad1 + msgConstants.fieldsForMappedVar)
        builderGenerator = builderGenerator.append(mappedMsgGenerator.getFuncGenerationForMapped(message.Elements, mdMgr))
        builderGenerator = builderGenerator.append(setFuncGenerationForMapped(message.Elements, fieldIndexMap))
      }

      builderGenerator = builderGenerator.append(build(message))
      builderGenerator = builderGenerator.append(newline + closeBrace)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return builderGenerator.toString
  }
  /*
   * Builder Class
   */
  private def builderClassGen(message: Message): String = {

    return "class Builder { ";
  }

  /*
   * Generate the variables for the Builder class 
   */
  private def generatedBuilderVariables(message: Message): String = {
    var msgVariables = new StringBuilder(8 * 1024)
    try {
      message.Elements.foreach(field => {
        if (field != null) {
          msgVariables.append(" %s private var %s: %s = _; %s".format(pad1, field.Name, field.FieldTypePhysicalName, newline))
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    msgVariables.toString()
  }

  /*
   * Get Method generation function
   */
  private def getFuncGeneration(fields: List[Element]): String = {
    var getMethod = new StringBuilder(8 * 1024)
    var getmethodStr: String = ""
    try {
      fields.foreach(field => {
        if (field != null) {
          getmethodStr = """
        def get""" + field.Name.capitalize + """: """ + field.FieldTypePhysicalName + """= {
        	return this.""" + field.Name + """;
        }          
        """
          getMethod = getMethod.append(getmethodStr.toString())
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return getMethod.toString
  }

  /*
   * Set Method Generation Function
   */
  private def setFuncGeneration(fields: List[Element]): String = {
    var setMethod = new StringBuilder(8 * 1024)
    var setmethodStr: String = ""
    try {
      fields.foreach(field => {
        if (field != null) {
          setmethodStr = """
        def set""" + field.Name.capitalize + """(value: """ + field.FieldTypePhysicalName + """): Builder = {
        	this.""" + field.Name + """ = value;
        	return this;
        }
        """
          setMethod = setMethod.append(setmethodStr.toString())
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return setMethod.toString
  }

  /*
   * build method for  Messages
   */
  private def build(message: Message): String = {
    var buildMethod: String = ""
    val msgFullName = message.Pkg + "." + message.Name + " = { " + newline
    val vardeclrtion: String = "%s var message = new %s %s".format(pad2, message.Name, newline)
    val build: String = "def build() : "
    val returnVal = "return message;"
    if (message.Fixed.equalsIgnoreCase("true"))
      buildMethod = build + msgFullName + vardeclrtion + buildForFixedMessage(message.Elements) + pad2 + returnVal + newline + pad1 + closeBrace
    else if (message.Fixed.equalsIgnoreCase("false"))
      buildMethod = build + msgFullName + vardeclrtion + buildForMappedMessage(message.Elements) + pad2 + returnVal + newline + pad1 + closeBrace

    //log.info("build method Start")
    //log.info(buildMethod)
    //log.info("build method end")
    return buildMethod
  }

  /*
   * build method to build the mesage object 
   */
  private def buildForFixedMessage(fields: List[Element]): String = {
    var buildMethodSB = new StringBuilder(8 * 1024)
    val buldstr = "%s message.set%s(this.%s)%s"
    try {

      fields.foreach(field => {
        if (field != null) {
          buildMethodSB = buildMethodSB.append(buldstr.format(pad2, field.Name.capitalize, field.Name, newline))
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return buildMethodSB.toString
  }

  /*
   * build method for Mapped Messages
   */
  private def buildForMappedMessage(fields: List[Element]): String = {
    """
       this.fields.foreach(field => {
          message.fields.put(field._1, (field._2._1, field._2._2 ) )
       })    
   """
  }

  /*
   * Get Method generation function for Mapped Messages
   */
  private def getFuncGenerationForMapped(fields: List[Element]): String = {
    var getMethod = new StringBuilder(8 * 1024)
    var getmethodStr: String = ""
    try {
      fields.foreach(field => {
        if (field != null) {
          getmethodStr = """
        def get""" + field.Name.capitalize + """: """ + field.FieldTypePhysicalName + """= {
        	return """ + field.FieldTypeImplementationName + """.Input(fields("""" + field.Name + """")._2.toString);
        }          
        """
          getMethod = getMethod.append(getmethodStr.toString())
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return getMethod.toString
  }

  /*
   * Set Method Generation Function for mapped messages
   */
  private def setFuncGenerationForMapped(fields: List[Element], fldsMap: Map[String, Int]): String = {
    var setMethod = new StringBuilder(8 * 1024)
    var setmethodStr: String = ""
    try {
      fields.foreach(field => {
        if (field != null) {
          setmethodStr = """
        def set""" + field.Name.capitalize + """(value: """ + field.FieldTypePhysicalName + """): Builder = {
        	this.fields("""" + field.Name + """") = (""" + fldsMap(field.FieldTypePhysicalName) + """, value);
        	return this;
        }
        """
          setMethod = setMethod.append(setmethodStr.toString())
        }
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return setMethod.toString
  }

}