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

import com.ligadata.jtm.eval.{Types => EvalTypes, Stamp, Expressions, GrokHelper}
import com.ligadata.kamanja.metadata._
import com.ligadata.kamanja.metadataload.MetadataLoad
import com.ligadata.msgcompiler._
import org.aicer.grok.dictionary.GrokDictionary
import org.apache.logging.log4j.{ Logger, LogManager }
import org.json4s.jackson.JsonMethods._
import org.rogach.scallop._
import org.apache.commons.io.FileUtils
import java.io.{StringReader, File}
import com.ligadata.runtime.Conversion
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
  def setMetadata(md: MdMgr) = { metadataMgr = md; this }
  def setJtm(jtm: String) = { jtmData = jtm; this }

  var jtmData: String = null
  var inputFile: String = null
  var outputFile: String = null
  var metadataLocation: String = null
  var suppressTimestamps: Boolean = false
  var metadataMgr: MdMgr = null

  def build() : Compiler = {
    new Compiler(this)
  }
}

/* Translates a jtm (json) file(s) into scala classes
 *
 */
class Compiler(params: CompilerBuilder) extends LogTrait {

  /** Initialize from parameter block
    *
    */
  val md = if(params.metadataMgr==null) {
    loadMetadata(params.metadataLocation) // Load metadata if not passed in
  } else {
    params.metadataMgr
  }

  val suppressTimestamps: Boolean = params.suppressTimestamps // Suppress timestamps

  val inputFile: String = params.inputFile // Input file to compile
  val outputFile: String = params.outputFile // Output file to write
  val root = Root.fromJson(inputFile) // Load Json

  private var code: String = null // The generated code

  /**
    * Collect information needed for modeldef
    */
  private var inmessages: Array[Map[String, Set[String]]] = null // Records all sets of incoming classes and attributes accessed
  private var outmessages: Set[String] = null //Records all outgoing classes

  def Imports(): Array[String] = {
    val imports = if (root.grok.nonEmpty) {
      root.imports.packages :+ "org.aicer.grok.dictionary.GrokDictionary"
    } else {
      root.imports.packages
    }

    val imports1 = imports :+ "com.ligadata.runtime.Conversion"

    imports1.distinct
  }

  def ModelName(): String = {
    if(root.header.name.isEmpty)
      "Model"
    else
      root.header.name
  }

  def FactoryName(): String = {
    if(root.header.name.isEmpty)
      "ModelFactory"
    else
      "%sFactory".format(root.header.name)
  }

  /** Returns the modeldef after compiler completed
    *
    */
  def MakeModelDef() : ModelDef = {

    if(code==null)
      throw new Exception("No code was successful created")

    val supportsInstanceSerialization : Boolean = false
    val isReusable: Boolean = true

    val out: Array[String] = outmessages.toArray

    val in: Array[Array[MessageAndAttributes]] = inmessages.map( s =>
          s.map( m => {
            val ma = new MessageAndAttributes
            ma.origin = "" //FIXME:- Fill this if looking for specific input
            ma.message = m._1
            ma.attributes = m._2.toArray
            ma
          }).toArray
    )

    var model = new ModelDef(ModelRepresentation.JAR, MiningModelType.JTM, in, out, isReusable, supportsInstanceSerialization)

    // Append addtional attributes
    model.nameSpace = root.header.namespace
    model.name = if(root.header.name.isEmpty) "Model" else root.header.name
    model.description = root.header.description
    model
  }

  def Code() : String = {
    if(code==null)
      throw new Exception("No code was successful created")

    code
  }

  /** Split a fully qualified object name into namspace and class
    *
    * @param name is a fully qualified class name
    * @return tuple with namespace and class name
    */
  def splitNamespaceClass(name: String): (String, String) = {
    val elements = name.split('.')
    (elements.dropRight(1).mkString("."), elements.last)
  }

  def Validate(root: Root) = {

    // Check requested language
    //
    if(root.header==null)
      throw new Exception("No header provided")

    if(root.aliases!=null) {
      if(root.aliases.concepts.size>0) {
        throw new Exception("Currently concepts aren't supported")
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
      if(header.minVersion.toDouble < 2.10) {
        throw new Exception("The minimum language requirement must be 2.10")
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

    // Validate any grok configuration
    GrokHelper.Validate(root)
  }

  /** Escape string as literal
    *
    * @param raw
    * @return
    */
  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }
  /** Product the configuration
    *
    * @param grok
    * @return
    */
  def BuildGrokInstance(grok : Grok): Array[String] = {

    var result = Array.empty[String]

    result :+= "lazy val grok_instance_1: GrokDictionary = {"
    result :+= "val dict = new GrokDictionary"

    if(grok.builtInDictionary)
      result :+= "dict.addBuiltInDictionaries"

    result ++= grok.file.distinct.map(
      f => {
        val file = new File(f)
        val name = "\"grok/%08X/%s\"".format(f.hashCode, file.getName)
        s"dict.addDictionary(new File(getClass.getResource($name).getPath))"
    })

    result ++= grok.patterns.map( p => {
      "dict.addDictionary(new StringReader(%s)".format(escape(p._1 + " " + p._2))
    })

    result :+= "dict.bind()"
    result :+= "dict"
    result :+= "}"
    result
  }

  /** Emit code to compile the unique expressions
    *
    * @param root
    * @return
    */
  def BuildGrokCompiledExpression(root: Root) : (Array[String], Map[String, (String, String, Set[String])]) = {

    var result = Array.empty[String]

    // Get the root instance - we only support 1 right now
    val grok : Grok = root.grok.head._2
    //val grokInstance: GrokDictionary = GrokHelper.Validate(grok)

    // Collect all unique expressions
    val expressions = root.transformations.foldLeft(Set.empty[String])((r, t) => {
      t._2.grokMatch.foldLeft(r) ((r, m) => {
        r + m._2
      })
    }).zipWithIndex.toMap

    val mapping = expressions.map( p => {

      val index = p._2
      val expressionString = p._1
      val outputs = GrokHelper.ExtractDictionaryKeys(expressionString)
      val pattern =  GrokHelper.ConvertToGrokPattern(expressionString)

      expressionString -> ("grok_instance_1_%d".format(p._2), pattern, outputs)
    }).toMap

    val defs = mapping.map( p=>
      "lazy val %s = grok_instance_1.compileExpression(%s)".format(p._2._1, escape(p._2._2))
    ).toArray

    (defs, mapping)
  }
  // Casing of system columns is inconsistent
  // provide a patch up map
  val columnNamePatchUp = Map.empty[String, String]
  val columnSystem = Set("transactionId", "rowNumber", "timePartitionData")

  def IsMappedMessage(mgr: MdMgr, classname: String): Boolean = {
    val classMd = md.Message(classname, 0, true)
    if(classMd.isEmpty) {
      throw new Exception("Metadata: unable to find class %s".format(classname))
    }
    if(classMd.get.containerType.isInstanceOf[MappedMsgTypeDef])
      true
    else
      false
  }

  def ColumnNames(mgr: MdMgr, classname: String): Map[String, String] = {
    val classMd = md.Message(classname, 0, true)
    if(classMd.isEmpty) {
      throw new Exception("Metadata: unable to find class %s".format(classname))
    }
    if(classMd.get.containerType.isInstanceOf[StructTypeDef]) {
      val members = classMd.get.containerType.asInstanceOf[StructTypeDef].memberDefs
      members.map(e => columnNamePatchUp.getOrElse(e.Name, e.Name) -> e.typeString).toMap
    } else if(classMd.get.containerType.isInstanceOf[MappedMsgTypeDef]) {
      val members = classMd.get.containerType.asInstanceOf[MappedMsgTypeDef].attrMap
      members.map(e => columnNamePatchUp.getOrElse(e._2.Name, e._2.Name)-> e._2.typeString).toMap
    } else {
      throw new Exception("Unhandled type %s".format(classMd.get.containerType.getClass.getName))
    }
  }

  def ResolveToVersionedClassname(mgr: MdMgr, classname: String): String = {
    val classMd = md.Message(classname, 0, true)
    if(classMd.isEmpty) {
      throw new Exception("Metadata: unable to find class %s".format(classname))
    }
    classMd.get.physicalName
  }

  /**
    *
    * @param argName arg name as string
    * @param className class name as string
    * @param fieldName field name as string
    * @param fieldType type name as string
    */
  case class Element(argName: String, className: String, fieldName: String, fieldType: String)

  def ColumnNames(mgr: MdMgr, classList: Set[String]): Array[Element] = {
    classList.foldLeft(1, Array.empty[Element])( (r, classname) => {
      val classMd = md.Message(classname, 0, true)

      if(classMd.get.containerType.isInstanceOf[StructTypeDef]) {
        val members = classMd.get.containerType.asInstanceOf[StructTypeDef].memberDefs
        (r._1 + 1, r._2 ++ members.map(e => Element("msg%d".format(r._1), classname, columnNamePatchUp.getOrElse(e.Name, e.Name), e.typeString)))
      } else if(classMd.get.containerType.isInstanceOf[MappedMsgTypeDef]) {
        val members = classMd.get.containerType.asInstanceOf[MappedMsgTypeDef].attrMap
        (r._1 + 1, r._2 ++ members.map(e => Element("msg%d".format(r._1), classname, columnNamePatchUp.getOrElse(e._2.Name, e._2.Name), e._2.typeString)))
      } else {
        throw new Exception("Unhandled type %s".format(classMd.get.containerType.getClass.getName))
      }
    })._2
  }

  def ResolveNames(names: Set[String], aliaseMessages: Map[String, String] ) : Map[String, String] =  {
    Expressions.ResolveNames(names, aliaseMessages)
  }

  def ResolveName(n: String, aliaseMessages: Map[String, String] ) : String =  {
    Expressions.ResolveName(n, aliaseMessages)
  }

  def ResolveAlias(n: String, aliaseMessages: Map[String, String] ) : String =  {
    Expressions.ResolveAlias(n, aliaseMessages)
  }

  def Generate(groks_in: scala.collection.Map[String, String],
               mapping_in: scala.collection.Map[String, String],
               wheres_in: Array[String],
               computes_in: scala.collection.Map[String, Compute],
               output_in: String,
               mappingset_in: Map[String, eval.Tracker],
               trackingset_in: Set[String],
               aliaseMessages: Map[String, String],
               grokExpressions: Map[String, (String, String, Set[String])],
               notUniqueInputs: Set[String],
               currentPath: String,
               inputs: Array[Element],
               dictMessages: Map[String, String]): (Array[String], Array[String], Map[String, eval.Tracker], Set[String]) = {

    var collect: Array[String] = Array.empty[String]
    var methods: Array[String] = Array.empty[String]

    var groks = groks_in
    var mapping = mapping_in
    var wheres = wheres_in
    var computes = computes_in

    var cnt1 = wheres.length + computes.size + groks.size + mapping.size
    var cnt2 = -1

    var (outputSet, outputtype) = if(output_in.nonEmpty) {
      val outputType1 = ResolveAlias(output_in, aliaseMessages)
      val outputType = ResolveToVersionedClassname(md, outputType1)
      (ColumnNames(md, outputType1).map(m=> m._1).toSet, outputType)
    } else {
      (Set.empty[String], "")
    }

    // State variables to track the progress
    // a little bit simpler than having val's
    var innerMapping: Map[String, eval.Tracker] = mappingset_in
    var innerTracking = trackingset_in

    logger.trace("\n\nProcessing: {} outputs: {}\n", currentPath, outputSet.mkString(","))
    logger.trace("Mapping: \n{}\n", innerMapping.mkString("\n"))
    logger.trace("Mapped Msg: \n{}\n", dictMessages.mkString("\n"))
    logger.trace("Tracked: \n{}\n\n", innerTracking.mkString("\n"))
    logger.trace("Grocks: \n{}", groks.mkString("\n"))
    logger.trace("Mapping: \n{}", mapping.mkString("\n"))
    logger.trace("Wheres: \n{}", wheres.mkString("\n"))
    logger.trace("Computes: \n{}", computes.map(m => s"${m._1} -> ${m._2.expression}").mkString("\n"))

    if(output_in.nonEmpty) {
      // Go through the inputs and find the system column so we can just funnel it through
      columnSystem.foreach(c => {
        if (outputSet.contains(c)) {
          val i = inputs.find(f => f.fieldName == c)
          outputSet --= Set(c)
          val f = "%s.%s".format(i.get.argName, i.get.fieldName)
          innerMapping ++= Map(c -> eval.Tracker(f, i.get.className, i.get.fieldType, false, i.get.fieldName, ""))
          innerTracking += f
        }
      })
    }

    def AmbiguousCheck(rList : Map[String, String], f : String) = {
      val ambiguous = rList.filter(f => notUniqueInputs.contains(f._2)).map(m => m._2)
      if (ambiguous.nonEmpty) {
        val a = ambiguous.mkString(", ")
        logger.error("Found ambiguous variables %s in expression %s".format(a, f))
        throw new Exception("Found ambiguous variables %s in expression %s".format(a, f))
      }
    }

    // Abort this loop if nothing changes or we can satisfy all outputs
    //
    // Mappings
    // Grocks
    // Filters
    // Where
    logger.trace("while: {}!={}", cnt1.toString, cnt2.toString)

    while (cnt1 != cnt2) {

      cnt2 = cnt1

      logger.trace("Mappings left {}", mapping.mkString(", "))

      // Process mappings that are variables, those are stuffed in the tracker
      // so they can be assigned to outputs directly
      val mapping2 = if (mapping.nonEmpty) {
        mapping.filter(f => {
          val tracker = Expressions.isVariable(f._2, innerMapping, dictMessages, aliaseMessages)
          if (tracker != null) {
            logger.trace("Matched mapping variable {} -> {}", f._1, tracker.toString)
            outputSet --= Set(f._1)
            innerMapping ++= Map(f._1 -> tracker)
            false
          } else {
            true
          }
        })
      } else {
        mapping
      }

      logger.trace("Mappings2 left {}", mapping2.mkString(", "))

      // Check Mapping with expressions
      val mapping1 = if (mapping2.nonEmpty) {

        val found = mapping2.filter(f => {

          // Try to extract variables, than it is an expression
          val list = Expressions.ExtractColumnNames(f._2)
          if (list.nonEmpty) {
            val rList = ResolveNames(list, aliaseMessages)
            val open = rList.filter(f => !innerMapping.contains(f._2)).filter(f => {
              val (c, v) = splitNamespaceClass(f._2)
              if (dictMessages.contains(c)) {
                val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
                val variableName = "%s.%s".format(dictMessages.get(c).get, v)
                innerMapping ++= Map(f._2 -> eval.Tracker(variableName, c, "Any", true, v, expression))
                false
              } else {
                true
              }
            })

            if (open.isEmpty)
              AmbiguousCheck(rList, f._2)
            else
              logger.trace("{} not found {}", currentPath, open.mkString(", "))

            open.isEmpty
          } else {

            if (innerMapping.contains(f._2))
              true
            else {
              val (c, v) = splitNamespaceClass(f._2)
              if (dictMessages.contains(c)) {
                val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
                val variableName = "%s.%s".format(dictMessages.get(c).get, v)
                innerMapping ++= Map(f._2 -> eval.Tracker(variableName, c, "Any", true, v, expression))
                true
              } else {
                false
              }
            }
          }
        })

        logger.trace("Mappings found {}", found.mkString(", "))

        found.foreach(f => {
          // Try to extract variables, than it is an expression
          val expression = f._2
          val list = Expressions.ExtractColumnNames(expression)

          val newExpression = if (list.nonEmpty) {
            val newExpression = Expressions.FixupColumnNames(expression, innerMapping, aliaseMessages)
            val rList = ResolveNames(list, aliaseMessages)
            val open = rList.filter(f => !innerMapping.contains(f._2))
            logger.trace("Matched mapping expression {} ({})-> {}", f._1, f._2, newExpression)
            innerTracking ++= rList.map(m => innerMapping.get(m._2).get.variableName).toSet
            newExpression
          } else {
            val mapped = innerMapping.get(expression).get.variableName
            innerTracking += mapped
            logger.trace("Column mapping {} -> {}", expression, mapped)
            mapped
          }
          outputSet --= Set(f._1)
          innerMapping ++= Map(f._1 -> eval.Tracker("", "", "", false, "", newExpression))
        })
        mapping2.filterKeys(f => !found.contains(f))
      } else {
        mapping2
      }

      logger.trace("Mappings1 left {}", mapping1.mkString(", "))

      // Check grok matches
      //
      val groks1 = groks.filter( g => {

        // Check if we have the input
        val nameColumn = ResolveName(g._1, aliaseMessages)
        val matched = if(innerMapping.contains(nameColumn)) {
          true
        } else {
          val (c, v) = splitNamespaceClass(nameColumn)
          if(dictMessages.contains(c)) {
            val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
            val variableName = "%s.%s".format(dictMessages.get(c).get, v)
            innerMapping ++= Map(g._1 -> eval.Tracker(variableName, c, "Any", true, v, expression))
            false
          } else  {
            true
          }
        }

        // Input determined, emit as output expressions
        if(matched) {

          AmbiguousCheck(Map(nameColumn->nameColumn), g._2)

          innerTracking += innerMapping.get(g._1).get.variableName

          // Get the expression
          //
          val d = grokExpressions.get(g._2).get

          logger.trace("Grok common matched {} -> {}", nameColumn, d._3.mkString(", "))

          // The var name might generate conflicts
          // Let's be optimistic for now
          val varName = "%s_%s".format(d._1, g._1)
          methods :+= "lazy val %s = %s.extractNamedGroups(%s)".format(varName, d._1, nameColumn)

          // Emit variables w/ null value is needed
          // we are adding a complete expression here
          // potentially we have to decorate it to avoid naming conflicts
          d._3.foreach( e => {
            val expr = "if(%s.containsKey(\"%s\")) %s.get(\"%s\") else \"\")".format(varName, e, varName, e)
            innerMapping ++= Map(e -> eval.Tracker(varName, "", "", false, "", expr))
          })
          false
        } else {
          true
        }

      })

      // filters
      val wheres1 = wheres.filter(f => {
        val list = Expressions.ExtractColumnNames(f)
        val rList = ResolveNames(list, aliaseMessages)
        val open = rList.filter(f => !innerMapping.contains(f._2)).filter(f => {
          val (c, v) = splitNamespaceClass(f._2)
          if(dictMessages.contains(c)) {
            val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
            val variableName = "%s.%s".format(dictMessages.get(c).get, v)
            innerMapping ++= Map(f._2 -> eval.Tracker(variableName, c, "Any", true, v,expression))
            false
          } else  {
            true
          }
        })

        AmbiguousCheck(rList, f)
        if (open.isEmpty) {

          innerTracking ++= rList.map(m => innerMapping.get(m._2).get.variableName).toSet

          // Sub names to
          val newExpression = Expressions.FixupColumnNames(f, innerMapping, aliaseMessages)
          logger.trace("Matched where expression {}", newExpression)
          // Output the actual filter
          collect ++= Array("if (%s) return Array.empty[MessageInterface]\n".format(newExpression))
          false
        } else {
          true
        }
      })

      // computes
      val computes1 = computes.filter(c => {

        // Check if the compute if determind
        val (open, expression, list) = if (c._2.expression.length > 0) {

          val list = Expressions.ExtractColumnNames(c._2.expression)
          val rList = ResolveNames(list, aliaseMessages)
          val open = rList.filter(f => !innerMapping.contains(f._2)).filter(f => {
            val (c, v) = splitNamespaceClass(f._2)
            if(dictMessages.contains(c)) {
              val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
              val variableName = "%s.%s".format(dictMessages.get(c).get, v)
              innerMapping ++= Map(f._2 -> eval.Tracker(variableName, c, "Any", true, v, expression))
              false
            } else  {
              true
            }
          })
          AmbiguousCheck(rList, c._2.expression)
          (open, c._2.expression, rList)
        } else {
          val evaluate = c._2.expressions.map(expression => {
            val list = Expressions.ExtractColumnNames(expression)
            val rList = ResolveNames(list, aliaseMessages)
            val open = rList.filter(f => !innerMapping.contains(f._2)).filter(f => {
              val (c, v) = splitNamespaceClass(f._2)
              if(dictMessages.contains(c)) {
                val expression = "%s.get(\"%s\")".format(dictMessages.get(c).get, v)
                val variableName = "%s.%s".format(dictMessages.get(c).get, v)
                innerMapping ++= Map(f._2 -> eval.Tracker(variableName, c, "Any", true, v, expression))
                false
              } else  {
                true
              }
            })

            if (open.isEmpty)
              AmbiguousCheck(rList, expression)

            (open, expression, rList)
          })

          evaluate.foldLeft(evaluate.head)((r, e) => {
            if (e._1.size < r._1.size)
              e
            else
              r
          })
        }

        if (open.isEmpty) {

          innerTracking ++= list.map(m => innerMapping.get(m._2).get.variableName).toSet

          // Sub names to
          val newExpression = Expressions.FixupColumnNames(expression, innerMapping, aliaseMessages)
          logger.trace("Matched compute expression {} -> {}", newExpression, c._1)


          collect :+= c._2.Comment
          if (c._2.typename.length > 0) {

            // Check if we track the type or need a type coersion
            val isVariable = Expressions.IsExpressionVariable(expression, innerMapping)
            if(isVariable) {
              val cols = Expressions.ExtractColumnNames(expression)
              val rt = innerMapping.get(cols.head).get
              if(rt.typeName!=c._2.typename && rt.typeName.nonEmpty) {
                // Find the conversion and wrap the call
                if(Conversion.builtin.contains(rt.typeName) && Conversion.builtin.get(rt.typeName).get.contains(c._2.typename))
                {
                  val conversionExpr = Conversion.builtin.get(rt.typeName).get.get(c._2.typename).get
                  collect ++= Array("val %s: %s = conversion.%s(%s)\n".format(c._1, c._2.typename, conversionExpr, newExpression))
                }
                else
                {
                  collect ++= Array("val %s: %s = %s\n".format(c._1, c._2.typename, newExpression))
                }
              } else {
                collect ++= Array("val %s: %s = %s\n".format(c._1, c._2.typename, newExpression))
              }

            }
            else {
              collect ++= Array("val %s: %s = %s\n".format(c._1, c._2.typename, newExpression))
            }
          } else {
            collect ++= Array("val %s = %s\n".format(c._1, newExpression))
          }
          outputSet --= Set(c._1)
          innerMapping ++= Map(c._1 -> eval.Tracker(c._1, "", c._2.typename, false, "", c._1))
          false
        } else {
          true
        }
      })

      // Update state
      cnt1 = wheres1.length + computes1.size + groks1.size + mapping1.size
      wheres = wheres1
      computes = computes1
      mapping = mapping1
      groks = groks1

      logger.trace("while: {}!={}", cnt1.toString, cnt2.toString)
    }

    if (outputSet.nonEmpty) {
      val m = "Not all outputs satisfied. transformation: %s missing: %s".format(currentPath, outputSet.mkString(", "))
      logger.trace(m)
      throw new Exception(m)
    }

    if (cnt2 != 0) {
      logger.trace("Not all elements used")
    }

    (collect, methods, innerMapping, innerTracking)
  }

  // Controls the code generation
  def Execute(): String = {

    // Reset any state
    inmessages = Array.empty[Map[String, Set[String]]]
    outmessages = Set.empty[String]

    // Validate model
    Validate(root)

    val aliaseMessages: Map[String, String] = root.aliases.messages.toMap
    var groks = Array.empty[String]
    var result = Array.empty[String]
    var exechandler = Array.empty[String]
    var methods = Array.empty[String]
    var messages = Array.empty[String]

    // Process header
    // ToDo: do we need a different license here
    result :+= Parts.header

    // Only output generation stamp for production environments
    if(!suppressTimestamps) {
      result ++= Stamp.Generate()
    }

    // Namespace
    //
    result :+= "package %s\n".format(root.header.namespace)

    // Process the imports
    //
    var subtitutions = new Substitution
    subtitutions.Add("model.name", root.header.namespace)
    subtitutions.Add("model.version", root.header.version)
    subtitutions.Add("factoryclass.name", FactoryName)
    subtitutions.Add("modelclass.name", ModelName)

    result :+= subtitutions.Run(Parts.imports)

    // Process additional imports like grok
    //
    val imports = Imports()

    // Emit grok initialization
    val grokExpressions = if(root.grok.nonEmpty) {
      groks ++= BuildGrokInstance(root.grok.head._2)
      val (e, m) = BuildGrokCompiledExpression(root)
      groks ++= e
      m
    } else {
      Map.empty[String, (String, String, Set[String])]
    }

    // Append the packages needed
    //
    result ++= imports.map( i => "import %s".format(i) )

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

    // Resolve dependencies from transformations
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

    subtitutions.Add("factory.isvalidmessage", msgs.map( m => {
      outmessages += m
      val verMsg = ResolveToVersionedClassname(md, m)
      "msg.isInstanceOf[%s]".format(verMsg)
    }).mkString("||") )

    val factory = subtitutions.Run(Parts.factory)
    result :+= factory

    // Generate variables
    //
    messages :+= "val msgs = execMsgsSet.map(m => m.getFullTypeName -> m).toMap"
    incomingToMsgId.foreach( e => {
      messages :+= "val msg%d = msgs.get(\"%s\").getOrElse(null).asInstanceOf[%s]".format(e._2, e._1, ResolveToVersionedClassname(md, e._1))
    })

    // Compute the highlevel handler that match dependencies
    //
    val handler = dependencyToTransformations.map( e => {
        // Trigger of incoming messages
        val check = e._1.map( m => { "msg%d!=null".format(incomingToMsgId.get(m).get)}).mkString(" && ")
        val names = e._1.map( m => { "msg%d".format(incomingToMsgId.get(m).get)}).mkString(", ")
        val depId = e._2._1
        val calls = e._2._2.map( f => "exeGenerated_%s_%d(%s)".format(f, depId, names) ).mkString(" ++ \n")
      """|if(%s) {
         |%s
         |} else {
         |  Array.empty[MessageInterface]
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
        methods :+= "def exeGenerated_%s_%d(%s): Array[MessageInterface] = {".format(t, depId, names)

        // Collect form metadata
        val inputs: Array[Element] = ColumnNames(md, deps).map( e => {
          Element("msg%d".format(incomingToMsgId.get(e.className).get), e.className, e.fieldName, e.fieldType)
        })

        // Resolve inputs, either we have unique or qualified names
        //
        val uniqueInputs: Map[String, eval.Tracker] = {
          val u = inputs.map( e => e.fieldName ).groupBy(identity).mapValues(_.length).filter( f => f._2==1).keys
          val u1 = u.map( e => inputs.find( c => c.fieldName == e).get)
          u1.map( p => {
            val variableName = "%s.%s".format(p.argName, p.fieldName)
            p.fieldName -> eval.Tracker(variableName, p.className, p.fieldType, true, p.fieldName, "")
          })
        }.toMap

        val notUniqueInputs: Set[String] = {
          inputs.map( e => e.fieldName ).groupBy(identity).mapValues(_.length).filter( f => f._2>1).keys.toSet
        }

        val qualifiedInputs: Map[String, eval.Tracker]  = inputs.map( p => {
          val variableName = "%s.%s".format(p.argName, p.fieldName)
          val classString = "%s.%s".format(p.className, p.fieldName)
          classString -> eval.Tracker(variableName, p.className, p.fieldType, true, p.fieldName, "")
        }).toMap

        // Find all dictionary messages
        val dictMessages =  deps.filter( f => {
          val classMd = md.Message(f, 0, true)
          !classMd.get.containerType.asInstanceOf[ContainerTypeDef].IsFixed
        }).map(m => m -> "msg%d".format(incomingToMsgId.get(m).get)).toMap

        // Common computes section
        //
        val (collectOuter, methodsOuter, outerMapping, outerTracking) = {

          Generate(transformation.grokMatch,
            Map.empty[String, String],
            Array.empty[String],
            transformation.computes,
            "",
            uniqueInputs ++ qualifiedInputs, // Mapping
            Set.empty[String], // Tracking
            aliaseMessages,
            grokExpressions,
            notUniqueInputs,
            t.toString(),
            Array.empty[Element],
            dictMessages)
        }

        methods ++= methodsOuter
        methods ++= collectOuter

        // Individual outputs
        //
        val inner = transformation.outputs.foldLeft(Array.empty[String]) ( (r, o) => {

          val (collectInner, methodsInner, innerMapping, innerTracking) = {

            val mapping = uniqueInputs ++ qualifiedInputs

            Generate(transformation.grokMatch,
              o._2.mapping,
              if (o._2.where.nonEmpty) Array(o._2.where) else Array.empty[String],
              o._2.computes,
              o._1,
              outerMapping, // Mapping
              outerTracking, // Tracking
              aliaseMessages,
              grokExpressions,
              notUniqueInputs,
              t.toString() + "@" + o._1,
              inputs,
              dictMessages)
          }

          var collect = Array.empty[String]
          collect ++= Array("\ndef process_%s(): Array[MessageInterface] = {\n".format(o._1))
          collect ++= collectInner

          {
            // Generate the output for this iteration
            // Translate outputs to the values
            val outputType1 = ResolveAlias(o._1, aliaseMessages)
            val outputType = ResolveToVersionedClassname(md, outputType1)
            val outputSet: Map[String, String] = ColumnNames(md, outputType1)

            val outputElements = outputSet.toArray.map(e => {
              // e.name -> from input, from mapping, from variable
              val m = innerMapping.get(e._1)
              if (m.isEmpty) {
                throw new Exception("Output %s not found".format(e))
              }

              // Coerce type
              val newExpression = Expressions.Coerce(e._2, m.get.typeName, m.get.getExpression())

              "result.%s = %s".format(e._1, newExpression)
            })

            // If output is a dictionary, collect all mappings
            //
            val outputElements1 = if(IsMappedMessage(md, outputType1)) {
              o._2.mapping.filter(f => !outputSet.contains(f._1)).toArray.map(e => {
                val m = innerMapping.get(e._1)
                if (m.isEmpty) {
                  throw new Exception("Output %s not found".format(e))
                }
                "result.set(\"%s\", %s)".format(e._1, m.get.getExpression())
              })
            } else {
              Array.empty[String]
            }

            // To Construct the final output
            val outputResult = "val result = new %s(messagefactoryinterface)\n%s\n%s\nArray(result)".format(outputType,
                                    outputElements.mkString("\n"), outputElements1.mkString("\n"))
            collect ++= Array(outputResult)
            collect ++= Array("}\n")
          }

          // Collect all input messages attribute used
          logger.trace("Final map: transformation %s ouput %s used %s".format(t, o._1, innerTracking.mkString(", ")))

          // Create the inmessage entry
          //
          {
            val a = innerMapping.filter(f => f._2.isInput).map(e => e._2.variableName -> (e._2.className, e._2.accessor))
            val t = innerTracking.foldLeft(Array.empty[(String, String)])((r, e) => {
              if (a.contains(e)) {
                val i = a.get(e).get
                r :+ (i._1, i._2)
              } else {
                r
              }
            })

            val classes = t.map( e => e._1 )
            val map1 = classes.map( e => (e -> t.filter( f => f._1 == e).map(e1 => e1._2).toSet)).toMap
            logger.trace("Incomming: \n%s".format(map1.mkString(",\n")))
            inmessages :+= map1
          }

          // Outputs result
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

    val resultVar = "val results: Array[MessageInterface] = \n"
    val returnValue = "results.asInstanceOf[Array[ContainerOrConcept]]"
    subtitutions.Add("model.grok", groks.mkString("\n"))
    subtitutions.Add("model.message", messages.mkString("\n"))
    subtitutions.Add("model.methods", methods.mkString("\n"))
    subtitutions.Add("model.code", resultVar + "\n" + exechandler.mkString("\n") + "\n" + returnValue + "\n")
    val model = subtitutions.Run(Parts.model)
    result :+= model

    // Write to output file
    // Store a copy in the object
    code = CodeHelper.Indent(result)

    if(outputFile!=null && outputFile.nonEmpty) {
      logger.trace("Output to file {}", outputFile)
      FileUtils.writeStringToFile(new File(outputFile), code)
    }

    code
  }
}
