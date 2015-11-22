package com.ligadata.msgcompiler
import com.ligadata.Exceptions._;
import com.ligadata.Exceptions.StackTrace;
import org.apache.log4j.Logger;

class MessageGenerator {
  val logger = this.getClass.getName
  lazy val log = Logger.getLogger(logger)
  val newline: String = "\n"
  val pad1: String = "\t"
  val pad2: String = "\t\t"
  val closeBrace = "}"

  /*
   * Generate Message Class
   * Message Class lines generation
   * Generate all the getter methods in the class generation
   * Generation all the setter methods in class generation
   */
  def generateMessage(message: Message): String = {

    var messageGenerator = new StringBuilder(8 * 1024)
    try {
      messageGenerator = messageGenerator.append(classGen(message) + newline)
      messageGenerator = messageGenerator.append(msgClassConstructorGen(message))
      messageGenerator = messageGenerator.append(newline + generatedMsgVariables(message))
      messageGenerator = messageGenerator.append(getFuncGeneration(message.Elements))
      messageGenerator = messageGenerator.append(setFuncGeneration(message.Elements))
      messageGenerator = messageGenerator.append(getFuncByOffset(message.Elements))
      messageGenerator = messageGenerator.append(setFuncByOffset(message.Elements))
      messageGenerator = messageGenerator.append(newline + closeBrace)

    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return messageGenerator.toString
  }
  /*
   * Message Class
   */
  private def classGen(message: Message): String = {
    return "class " + message.Name + " { ";

  }

  /*
   * Generate the variables for the message 
   */
  private def generatedMsgVariables(message: Message): String = {
    var msgVariables = new StringBuilder(8 * 1024)
    try {
      message.Elements.foreach(field => {
        msgVariables.append(" %s private var %s: %s = _; %s".format(pad1, field.Name, field.Ttype, newline))
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
   * Message constructor generation
   */

  private def msgConstructor(message: Message, msgStr: String, constructorStmts: String): String = {
    var msgClassConsGen: String = ""

    msgClassConsGen = """
        def """ + message.Name + """(""" + msgStr.substring(0, msgStr.length() - 2) + """) {
    """ + constructorStmts + """
        }"""

    return msgClassConsGen
  }

  /*
   * Message Class Constructor Generation
   */

  private def msgClassConstructorGen(message: Message): String = {
    var msgClassConsGen: String = ""
    var msgConsStr = new StringBuilder(8 * 1024)
    var constructorStmts = new StringBuilder(8 * 1024)

    try {
      message.Elements.foreach(element => {
        msgConsStr.append("%s: %s, ".format(element.Name, element.Ttype))
        constructorStmts.append("%s this.%s = %s; %s ".format(pad2, element.Name, element.Ttype, newline))
      })
      val msgStr = msgConsStr.toString
      log.info("constructor Generation ===================" + msgStr.substring(0, msgStr.length() - 1))
      msgClassConsGen = msgConstructor(message, msgStr, constructorStmts.toString)
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return msgClassConsGen.toString
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
        def get""" + field.Name.capitalize + """: """ + field.Ttype + """= {
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
        def set""" + field.Name.capitalize + """(value: """ + field.Ttype + """): Unit = {
        	this.""" + field.Name + """ = value;
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
   * Get By Ordinal Function generation
   */
  private def getFuncByOffset(fields: List[Element]): String = {
    var getFuncByOffset: String = ""
    getFuncByOffset = """
      def get(field$ : Int) : Any = {
      	field$.match {
  """ + getByOffset(fields) + """
      	 case _ => throw new Exception("Bad index");
    	}
      }      
    """
    return getFuncByOffset
  }

  /*
   * Get By Ordinal Function generation
   */
  private def getByOffset(fields: List[Element]): String = {
    var getByOffset = new StringBuilder(8 * 1024)
    try {
      fields.foreach(field => {
        getByOffset.append("%s case %s => return %s; %s".format(pad1, field.FieldOrdinal, field.Name, newline))
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return getByOffset.toString
  }

  /*
   * Set By Ordinal Function generation
   */
  private def setFuncByOffset(fields: List[Element]): String = {
    var getFuncByOffset: String = ""
    getFuncByOffset = """
      def set(field$ : Int, value :Any): Unit = {
      	field$.match {
  """ + setByOffset(fields) + """
      	 case _ => throw new Exception("Bad index");
    	}
      }      
    """
    return getFuncByOffset
  }

  /*
   * Set By Ordinal Function generation
   */
  private def setByOffset(fields: List[Element]): String = {
    var setByOffset = new StringBuilder(8 * 1024)
    try {
      fields.foreach(field => {
        setByOffset.append("%s case %s => {this.%s = value.asInstanceOf[%s]}; %s".format(pad1, field.FieldOrdinal, field.Name, field.Ttype, newline))
      })
    } catch {
      case e: Exception => {
        val stackTrace = StackTrace.ThrowableTraceString(e)
        log.debug("StackTrace:" + stackTrace)
        throw e
      }
    }
    return setByOffset.toString
  }
}