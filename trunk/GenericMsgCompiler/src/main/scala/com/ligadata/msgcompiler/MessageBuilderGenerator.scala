package com.ligadata.msgcompiler

import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.log4j.Logger;

class MessageBuilderGenerator {

  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)
  val newline: String = "\n"
  val pad1: String = "\t"
  val pad2: String = "\t\t"
  val closeBrace = "}"

  def generatorBuilder(message: Message): String = {
    var builderGenerator = new StringBuilder(8 * 1024)
    try {
      builderGenerator = builderGenerator.append(builderClassGen(message) + newline)
      builderGenerator = builderGenerator.append(newline + generatedBuilderVariables(message))
      builderGenerator = builderGenerator.append(getFuncGeneration(message.Elements))
      builderGenerator = builderGenerator.append(setFuncGeneration(message.Elements))
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
        msgVariables.append(" %s private var %s: %s = _; %s".format(pad1, field.Name, field.FieldTypePhysicalName, newline))
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
        getmethodStr = """
        def get""" + field.Name.capitalize + """: """ + field.FieldTypePhysicalName + """= {
        	return this.""" + field.Name + """;
        }          
        """
        getMethod = getMethod.append(getmethodStr.toString())
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
        setmethodStr = """
        def set""" + field.Name.capitalize + """(value: """ + field.FieldTypePhysicalName + """): Builder = {
        	this.""" + field.Name + """ = value;
        	return this;
        }
        """
        setMethod = setMethod.append(setmethodStr.toString())
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
   * build method to build the mesage object 
   */
  private def buildMessage(fields: List[Element]): String = {
    var buildMethodSB = new StringBuilder(8 * 1024)
    val buldstr = "%s message.set%s(this.%s)%s"
    try {

      fields.foreach(field => {
        buildMethodSB = buildMethodSB.append(buldstr.format(pad2, field.Name.capitalize, field.Name, newline))
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
   * build method
   */
  private def build(message: Message): String = {
    var buildMethod: String = ""
    val msgFullName = message.Pkg + "." + message.Name + " = { " + newline
    val vardeclrtion: String = "%s var message = new %s %s".format(pad2, message.Name, newline)
    val build: String = "def build() : "
    val returnVal = "return message;"
    buildMethod = build + msgFullName + vardeclrtion + buildMessage(message.Elements) + pad2 + returnVal + newline + pad1 + closeBrace
    log.info("build method Start")
    log.info(buildMethod)
    log.info("build method end")
    return buildMethod
  }
}