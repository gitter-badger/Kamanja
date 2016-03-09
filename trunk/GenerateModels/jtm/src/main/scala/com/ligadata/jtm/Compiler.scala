/*
 * Copyright 2016 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.ligadata.jtm

import com.ligadata.jtm.eval.{Types => EvalTypes}
import com.ligadata.kamanja.metadata.{StructTypeDef, MdMgr}
import com.ligadata.kamanja.metadataload.MetadataLoad
import com.ligadata.messagedef.MessageDefImpl
import org.apache.logging.log4j.{ Logger, LogManager }
import org.json4s.jackson.JsonMethods._
import org.rogach.scallop._
import org.apache.commons.io.FileUtils
import java.io.File

import com.ligadata.jtm.nodes._

// Laundry list
/*
3) Support java
4) Plug into kamanja metadata tool

a) End-End-Test
b) Generate code validation for unit test => compile
c) Model run and validation for unit test => compile, run
*/

object jtmGlobalLogger {
  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)
}

trait LogTrait {
  val logger = jtmGlobalLogger.logger
}

class Conf (arguments: Seq[String] ) extends ScallopConf (arguments)  with LogTrait {

  val jtm = opt[String] (required = true, descr = "Sources to compile", default = None )
/*
  val scalahome = opt[String] (required = false, descr = "", default = Some ("") )
  val javahome = opt[String] (required = false, descr = "", default = Some ("") )
  val cp = opt[String] (required = false, descr = "", default = Some ("") )
  val jarpath = opt[String] (required = false, descr = "", default = Some ("") )
  val scriptout = opt[String] (required = false, descr = "Sources to compile", default = Some ("") )
  val manifest = opt[String] (required = false, descr = "Sources to compile", default = Some ("") )
  val client = opt[String] (required = false, descr = "Sources to compile", default = Some ("") )
  val sourceout = opt[String] (required = false, descr = "Path to the location to store generated sources", default = Some ("") )
  val addLogging = opt[Boolean] (required = false, descr = "Add logging code to the model", default = Some (true) )
  val createjar = opt[Boolean] (required = false, descr = "Create the final jar output ", default = Some (true) )
*/
}

/* Commandline interface to compiler
 *
 */
object Compiler extends App with LogTrait {
  override def main (args: Array[String] ) {

      try {
        val cmdconf = new Conf(args)
      }
      catch {
        case e: Exception => System.exit(1)
      }
      // Do all validations

      // Create compiler instance and generate scala code
  }
}

object CompilerBuilder {
  def create() = { new CompilerBuilder }
}

/** Class to collect all the parameter to build a compiler instance
  *
  */
class CompilerBuilder {

  def setSuppressTimestamps(switch: Boolean = true) = { suppressTimestamps = switch; this }
  def setInputFile(filename: String) = { inputFile = filename; this }
  def setOutputFile(filename: String) = { outputFile = filename; this }
  def setMetadataLocation(filename: String) = { metadataLocation = filename; this }

  var inputFile : String = null
  var outputFile : String = null
  var metadataLocation : String = null
  var suppressTimestamps : Boolean = false

  def build() : Compiler = {
    new Compiler(this)
  }
}

/* Translates a jtm (json) file(s) into scala classes
 *
 */
class Compiler(params: CompilerBuilder) extends LogTrait {

  /** Split a fully qualified object name into namspace and class
    *
    * @param name is a fully qualified class name
    * @return tuple with namespace and class name
    */
  def splitNamespaceClass(name: String): (String, String) = {
    val elements = name.split('.')
    (elements.dropRight(1).mkString("."), elements.last)
  }

  /** Split a name into alias and field name
    *
    * @param name Name
    * @return
    */
  def splitAlias(name: String): (String, String) = {
    val elements = name.split('.')
    if(elements.length==1)
      ("", name)
    else
      ( elements.head, elements.slice(1, elements.length).mkString(".") )
  }

  /** Creates a metadata instance with defaults and json objects located on the file system
    *
    * @return Metadata manager
    */
  def loadMetadata(): MdMgr= {

    val typesPath : String = ""
    val fcnPath : String = ""
    val attrPath : String = ""
    val msgCtnPath : String = ""
    val mgr : MdMgr = MdMgr.GetMdMgr

    // If the compiler is called again this will throw
    // To Do: move the metadata and improve handling
    try {
      val mdLoader = new MetadataLoad(mgr, typesPath, fcnPath, attrPath, msgCtnPath)
      mdLoader.initialize

      def getRecursiveListOfFiles(dir: File): Array[File] = {
        val these = dir.listFiles.filter(_.isFile)
        val those = dir.listFiles.filter(_.isDirectory)
        these ++ those.flatMap(getRecursiveListOfFiles)
      }

      val files = getRecursiveListOfFiles(new File(params.metadataLocation))

      // Load all json files for the metadata directory
      files.map ( jsonFile => {
        val json = FileUtils.readFileToString(jsonFile, null)
        val map = parse(json).values.asInstanceOf[Map[String, Any]]
        val msg = new MessageDefImpl()
        val ((classStrVer, classStrVerJava), msgDef, (classStrNoVer, classStrNoVerJava)) = msg.processMsgDef(json, "JSON", mgr, false)
        val msg1 = msgDef.asInstanceOf[com.ligadata.kamanja.metadata.MessageDef]
        mgr.AddMsg(msg1)
      })
    } catch {
      case _ : Throwable => ;
    }

    mgr
  }

  /** Find all logical column names that are encode in this expression $name
    *
    * @param expression
    * @return
    */
  def ExtractColumnNames(expression: String): Set[String] = {

    // ToDo: Extract only the first two components
    // ToDo: allow to throw away the 2nd component if the first matches
    // and attribute to the expression
    val regex = """(\$[a-zA-Z0-9_.]+)""".r
    regex.findAllMatchIn(expression).toArray.map( m => m.matched.drop(1)).toSet
  }

  /** Replace all logical column namess with the variables
    *
    * @param expression expression to update
    * @param mapNameSource name to variable mapping
    * @return string with the result
    */
  def FixupColumnNames(expression: String, mapNameSource: Map[String, String], aliaseMessages: Map[String, String]): String = {
    val regex = """(\$[a-zA-Z0-9_.]+)""".r
    val m = regex.pattern.matcher(expression)
    val sb = new StringBuffer
    var i = 0
    while (m.find) {
      val name = m.group(0).drop(1)
      val resolvedName = ResolveName(name, aliaseMessages)
      m.appendReplacement(sb, mapNameSource.get(resolvedName).get)
      i = i + 1
    }
    m.appendTail(sb)
    sb.toString
  }

  def Validate(root: Root) = {

    // Check requested language
    //
    if(root.header==null)
      throw new Exception("No header provided")

    if(root.aliases!=null) {
      if(root.aliases.concepts.size>0) {
        throw new Exception("Currently concept aren't supported")
      }
      if(root.aliases.variables.size>0) {
        throw new Exception("Currently variables aren't supported")
      }
    }

    val header = root.header

    if(header.language.trim.toLowerCase() !="scala")
        throw new Exception("Currently only Scala is supported")

    // Check the min version
    //
    if(header.language.trim.toLowerCase=="scala") {
      // ToDo: Add version parser here
      if(header.minVersion.toDouble < 2.11) {
        throw new Exception("The minimum language requirement must be 2.11")
      }
    }

    if(root.imports.packages.toSet.size < root.imports.packages.length) {
      val dups = root.imports.packages.groupBy(identity).collect { case (x,ys) if ys.length > 1 => x }
      logger.warn("Dropped duplicate imports: {}", dups.mkString(", "))
    }

    // Validate any computes nodes if val and vals are set
    val computeConstraint = root.transformations.foldLeft("root/", Map.empty[String, String])( (r, t) => {

      val r1 = t._2.computes.foldLeft(r._1 + t._1 + "/", Map.empty[String, String])( (r, c) => {
        if(c._2.expression.length > 0 && c._2.expressions.length > 0) {
          ("", r._2 ++ Map(r._1 + c._1 -> "Vals and val attribute are set, please choose one.") )
        } else {
          r
        }
      })._2

      val r2 = t._2.outputs.foldLeft(r._1 + t._1 + "/", Map.empty[String, String])( (r, o) => {
        o._2.computes.foldLeft( r._1 + o._1 + "/", r._2 )((r, c) => {
          if(c._2.expression.length > 0 && c._2.expressions.length > 0) {
            ("", r._2 ++ Map(r._1 + c._1 -> "Vals and val attribute are set, please choose one.") )
          } else {
            r
          }
        })
      })._2

      ("", r1 ++ r2)
    })._2

    if(computeConstraint.nonEmpty) {
      computeConstraint.foreach( m => logger.warn(m.toString()))
      throw new Exception("Conflicting %d compute nodes".format(computeConstraint.size))
    }

    // Check that we only have a single grok instance
    if(root.grok.size>1) {
      throw new Exception("Found %d grok configurations, only a single configuration allowed.".format(root.grok.size))
    }
  }

  // Casing of system columns is inconsistsent
  // provide a atch up map
  val columnNamePatchUp = Map("transactionid" -> "transactionId")

  def ColumnNames(mgr: MdMgr, classname: String): Set[String] = {
    val classinstance = md.Message(classname, 0, true)
    if(classinstance.isEmpty) {
      throw new Exception("Metadata: unable to find class %s".format(classname))
    }
    val members = classinstance.get.containerType.asInstanceOf[StructTypeDef].memberDefs
    members.map( e => columnNamePatchUp.getOrElse(e.Name, e.Name)).toSet
  }

  def ResolveToVersionedClassname(mgr: MdMgr, classname: String): String = {
    val classinstance = md.Message(classname, 0, true)
    if(classinstance.isEmpty) {
      throw new Exception("Metadata: unable to find class %s".format(classname))
    }
    // We convert to lower case
    classinstance.get.physicalName.toLowerCase
  }

  /**
    *
    * @param argName
    * @param className
    * @param fieldName
    */
  case class Element(argName: String, className: String, fieldName: String)
  def ColumnNames(mgr: MdMgr, classList: Set[String]): Array[Element] = {
    classList.foldLeft(1, Array.empty[Element])( (r, classname) => {
      val classMd = md.Message(classname, 0, true)
      val members = classMd.get.containerType.asInstanceOf[StructTypeDef].memberDefs
      (r._1 + 1, r._2 ++ members.map( e => Element("msg%d".format( r._1), classname, columnNamePatchUp.getOrElse(e.Name, e.Name))))
    })._2
  }

  def ResolveNames(names: Set[String], aliaseMessages: Map[String, String] ) : Map[String, String] =  {

    names.map ( n => {
      val (alias, name) = splitAlias(n)
      if(alias.length>0) {
        val a = aliaseMessages.get(alias)
        if(a.isEmpty) {
          throw new Exception("Missing alias %s for %s".format(alias, n))
        } else {
          n -> "%s.%s".format(a.get, name)
        }
      } else {
        n -> n
      }
    }).toMap
  }

  def ResolveName(n: String, aliaseMessages: Map[String, String] ) : String =  {

    val (alias, name) = splitAlias(n)
    if(alias.length>0) {
      val a = aliaseMessages.get(alias)
      if(a.isEmpty) {
        throw new Exception("Missing alias %s for %s".format(alias, n))
      } else {
        "%s.%s".format(a.get, name)
      }
    } else {
      n
    }
  }

  def ResolveAlias(n: String, aliaseMessages: Map[String, String] ) : String =  {

    val a = aliaseMessages.get(n)
    if(a.isEmpty) {
      throw new Exception("Missing alias %s".format(n))
    } else {
      a.get
    }
  }

  // Load metadata
  val md = loadMetadata

  val suppressTimestamps: Boolean = params.suppressTimestamps // Suppress timestamps
  val inputFile: String = params.inputFile // Input file to compile
  val outputFile: String = params.outputFile // Output file to write

  // Controls the code generation
  def Execute(): String = {

    // Load Json
    val root = Root.fromJson(inputFile)

    // Validate model
    Validate(root)

    val aliaseMessages: Map[String, String] = root.aliases.messages.toMap
    var result = Array.empty[String]
    var exechandler = Array.empty[String]
    var methods = Array.empty[String]
    var messages = Array.empty[String]

    // Process header
    // ToDo: do we need a different license here
    result :+= Parts.header

    // Namespace
    //
    result :+= "package %s\n".format(root.header.namespace)

    // Process the imports
    //
    var subtitutions = new Substitution
    subtitutions.Add("model.name", root.header.namespace)
    subtitutions.Add("model.version", root.header.version)
    result :+= subtitutions.Run(Parts.imports)

    // Process additional imports
    //
    result ++= root.imports.packages.distinct.map( i => "import %s".format(i) )

    // Add message so we can actual compile
    // Check how to reconcile during add/compilation
    //result ++= aliaseMessages.map(p => p._2).toSet.toArray.map( i => "import %s".format(i))

    // Collect all classes
    //
    val messagesSet = EvalTypes.CollectMessages(root)
    messagesSet.map( e => "%s aliases %s".format(e._1, e._2.mkString(", ")) ).foreach( m => {
      logger.trace(m)
    })

    // Collect all specified types
      // Should check we can resolve them
    val types = EvalTypes.CollectTypes(root)
    types.map( e => "%s usedby %s".format(e._1, e._2.mkString(", ")) ).foreach( m => {
      logger.trace(m)
    })

    // Check all found types against metadata
    //

    // Resolve dependencies fro transformations
    //
    val dependencyToTransformations = EvalTypes.ResolveDependencies(root)

    // Upshot of the dependencies
    //
    dependencyToTransformations.map( e => {
      "Dependency [%s] => (%s)".format(e._1.mkString(", "), e._2._2.mkString(", "))
    }).foreach( m =>logger.trace(m) )

    // Create a map of dependency to id
    //
    val incomingToMsgId = dependencyToTransformations.foldLeft(Set.empty[String]) ( (r, e) => {
      r ++ e._1
    }).zipWithIndex.map( e => (e._1, e._2 + 1)).toMap

    // Return tru if we accept the message, flatten the messages into a list
    //
    val msgs = dependencyToTransformations.foldLeft(Set.empty[String]) ( (r, d) => {
      d._1.foldLeft(r) ((r, n) => {
        r ++ Set(n)
      })
    })

    subtitutions.Add("factory.isvalidmessage", msgs.map( m => "msg.isInstanceOf[%s]".format(ResolveToVersionedClassname(md, m))).mkString("||") )
    val factory = subtitutions.Run(Parts.factory)
    result :+= factory

    // Generate variables
    //
    incomingToMsgId.foreach( e => {
      messages :+= "val msg%d = msgs.get(\"%s\").getOrElse(null).asInstanceOf[%s]".format(e._2, e._1, ResolveToVersionedClassname(md, e._1))
    })

    // Compute the highlevel handler that match dpendencies
    //
    val handler = dependencyToTransformations.map( e => {
        val check = e._1.map( m => { "msg%d!=null".format(incomingToMsgId.get(m).get)}).mkString(" && ")
        val names = e._1.map( m => { "msg%d".format(incomingToMsgId.get(m).get)}).mkString(", ")
        val depId = e._2._1
        val calls = e._2._2.map( f => "exeGenerated_%s_%d(%s)".format(f, depId, names) ).mkString(" ++ \n")
      """|if(%s) {
         |%s
         |} else {
         |  Array.empty[Result]
         |}
         |""".stripMargin('|').format(check, calls)
    })
    exechandler :+=  handler.mkString( "++\n")

    // Actual function to be called
    //
    dependencyToTransformations.foreach( e => {
      val deps = e._1
      val depId = e._2._1
      val transformationNames = e._2._2

      transformationNames.foreach( t => {

        val transformation = root.transformations.get(t).get
        val names = deps.map( m => { "msg%d: %s".format(incomingToMsgId.get(m).get, ResolveToVersionedClassname(md, m))}).mkString(", ")

        methods :+= transformation.Comment
        methods :+= "def exeGenerated_%s_%d(%s): Array[Result] = {".format(t, depId, names)

        // Collect form metadata
        val inputs: Array[Element] = ColumnNames(md, deps).map( e => {
          Element("msg%d".format(incomingToMsgId.get(e.className).get), e.className, e.fieldName)
        })

        // Resolve inputs, either we have unique or qualified names
        //
        val uniqueInputs: Map[String, String] = {
          val u = inputs.map( e => e.fieldName ).groupBy(identity).mapValues(_.length).filter( f => f._2==1).keys
          val u1 = u.map( e => inputs.find( c => c.fieldName == e).get)
          u1.map( p => p.fieldName -> "%s.%s".format(p.argName, p.fieldName))
        }.toMap

        val qualifiedInputs: Map[String, String]  = inputs.map( p => {
          p.className + "." + p.fieldName -> "%s.%s".format(p.argName, p.fieldName)
        }).toMap

        var fixedMappingSources = uniqueInputs ++ qualifiedInputs

        // Common computes section
        //
        var computes = transformation.computes
        var cnt1 = computes.size
        var cnt2 = 0

        while(cnt1!=cnt2 && computes.nonEmpty) {
          cnt2 = cnt1

          val computes1 = computes.filter(c => {

            // Check if the compute if determined
            val (open, expression) =  if(c._2.expression.nonEmpty) {
              val list = ExtractColumnNames(c._2.expression)
              val rList = ResolveNames(list, aliaseMessages)
              val open = rList.filter(f => !fixedMappingSources.contains(f._2))
              (open, c._2.expression)
            } else {
              val evaluate = c._2.expressions.map( expression => {
                val list = ExtractColumnNames(expression)
                val rList = ResolveNames(list, aliaseMessages)
                val open = rList.filter(f => !fixedMappingSources.contains(f._2))
                (open, expression)
              })
              evaluate.foldLeft(evaluate.head) ( (r, e) => {
                if(e._1.size < r._1.size)
                  e
                else
                  r
              })
            }

            if(open.isEmpty) {
              val newExpression = FixupColumnNames(expression, fixedMappingSources, aliaseMessages)
              // Output the actual compute
              methods :+= c._2.Comment
              if(c._2.typename.length>0)
                methods ++= Array("val %s: %s = %s\n".format(c._1, c._2.typename, newExpression))
              else
                methods ++= Array("val %s = %s\n".format(c._1, newExpression))

              fixedMappingSources ++= Map(c._1 -> c._1)
              false
            } else {
              true
            }
          })

          cnt1 = computes1.size
          computes = computes1
        }

        if(computes.nonEmpty){
          throw new Exception("Not all elements used")
          logger.trace("Not all elements used")
        }

        // Individual outputs
        //
        val inner = transformation.outputs.foldLeft(Array.empty[String]) ( (r, o) => {

          var collect = Array.empty[String]
          collect ++= Array("\ndef process_%s(): Array[Result] = {\n".format(o._1))

          val outputSet: Set[String] = ColumnNames(md, ResolveAlias(o._1, aliaseMessages))

          // State variables to track the progress
          // a little bit simpler than having val's
          var mappingSources: Map[String, String] = fixedMappingSources

          var outputSet1: Set[String] = outputSet
          // To Do: Clarify how to resolve transactionId (and other auto columns)
          // Transaction id is in the input
          // so will just push it back if needed
          if(outputSet1.contains("transactionId")) {
            outputSet1 --= Set("transactionId")
          }

          var mapping = o._2.mapping
          var wheres =  Array(o._2.where)
          var computes = o._2.computes
          var cnt1 = wheres.length + computes.size
          var cnt2 = 0

          // Remove provided computes -  outer computes
          outputSet1 = outputSet1.filter( f => !mappingSources.contains(f))

          // Removed if mappings are provided
          val found = mapping.filter( f => mappingSources.contains(f._2) )
          found.foreach( f => { outputSet1 --= Set(f._1); mappingSources ++= Map(f._1 -> mappingSources.get(f._2).get) } )
          mapping = mapping.filterKeys( f => !found.contains(f) )

          // Abort this loop if nothing changes or we can satisfy all outputs
          while(cnt1!=cnt2 && outputSet1.nonEmpty) {

            cnt2 = cnt1

            // filters
            val wheres1 = wheres.filter(f => {
              val list = ExtractColumnNames(f)
              val open = list.filter(f => !mappingSources.contains(f) )
              if(open.isEmpty) {
                // Sub names to
                val newExpression = FixupColumnNames(f, mappingSources, aliaseMessages)
                // Output the actual filter
                collect ++= Array("if (%s) return Array.empty[Result]\n".format(newExpression))
                false
              } else {
                true
              }
            })

            // computes
            val computes1 = computes.filter( c => {

              // Check if the compute if determind
              val (open, expression) =  if(c._2.expression.length > 0) {
                val list = ExtractColumnNames(c._2.expression)
                val rList = ResolveNames(list, root.aliases.messages.toMap)
                val open = rList.filter(f => !fixedMappingSources.contains(f._2))
                (open, c._2.expression)
              } else {
                val evaluate = c._2.expressions.map( expression => {
                  val list = ExtractColumnNames(expression)
                  val rList = ResolveNames(list, root.aliases.messages.toMap)
                  val open = rList.filter(f => !fixedMappingSources.contains(f._2))
                  (open, expression)
                })
                evaluate.foldLeft(evaluate.head) ( (r, e) => {
                  if(e._1.size < r._1.size)
                    e
                  else
                    r
                })
              }

              if(open.isEmpty) {
                // Sub names to
                val newExpression = FixupColumnNames(expression, mappingSources, aliaseMessages)

                // Output the actual compute
                // To Do: multiple vals and type provided
                collect :+= c._2.Comment
                if(c._2.typename.length>0)
                  collect ++= Array("val %s: %s = %s\n".format(c._1, c._2.typename, newExpression))
                else
                  collect ++= Array("val %s = %s\n".format(c._1, newExpression))

                mappingSources ++= Map(c._1 -> c._1)
                outputSet1 --= Set(c._1)
                false
              } else {
                true
              }
            })

            // Check Mapping
            if(mapping.nonEmpty)
            {
              val found = mapping.filter( f => mappingSources.contains(f._2) )
              found.foreach(f => {outputSet1 --= Set(f._1); mappingSources ++= Map(f._1 -> mappingSources.get(f._2).get)})
              mapping = mapping.filterKeys( f => !found.contains(f)  )
            }

            // Update state
            cnt1 = wheres1.length + computes1.size
            wheres = wheres1
            computes = computes1
          }

          if(outputSet1.nonEmpty){
            logger.trace("Not all outputs satisfied. missing={}" , outputSet1.mkString(", "))
            throw new Exception("Not all outputs satisfied. missing=" + outputSet1.mkString(", "))
          }

          if(cnt2!=0){
            logger.trace("Not all elements used")
            //throw new Exception("Not all elements used")
          }

          // Generate the output for this iteration
          // Translate outputs to the values
          val outputElements = outputSet.map( e => {
            // e.name -> from input, from mapping, from variable
            "new Result(\"%s\", %s)".format(e, mappingSources.get(e).get)
          }).mkString(", ")

          // To Do: this is not correct
          val outputResult = "Array[Result](%s)".format(outputElements)

          collect ++= Array(outputResult)
          collect ++= Array("}\n")

          // outputs
          r ++ collect
        })

        methods ++= inner

        // Output the function calls
        methods :+= transformation.outputs.map( o => {
          "process_%s()".format(o._1)
        }).mkString("++\n")

        methods :+= "}"

      })
    })

    val resultVar = "val results: Array[Result] = \n"
    val returnValue = "factory.createResultObject().asInstanceOf[MappedModelResults].withResults(results)"
    subtitutions.Add("model.message", messages.mkString("\n"))
    subtitutions.Add("model.methods", methods.mkString("\n"))
    subtitutions.Add("model.code", resultVar + "\n" + exechandler.mkString("\n") + "\n" + returnValue + "\n")
    val model = subtitutions.Run(Parts.model)
    result :+= model

    // Write to output file
    val code = CodeHelper.Indent(result)
    logger.trace("Output to file {}", outputFile)
    FileUtils.writeStringToFile(new File(outputFile), code)

    outputFile
  }
}
