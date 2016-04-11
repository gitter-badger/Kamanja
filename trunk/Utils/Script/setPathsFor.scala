#!/bin/bash
exec scala "$0" "$@"
!#

/***************************************************************************************
 * 	setPathsFor.scala --templateFile <templateFile path> 
 *	       		  --installDir <kamanja install dir> 
 *			  --scalaHome <Scala Home> 
 *	       		  --javaHome <Java Home> 
 *
 * Substitute the Kamanja template file that contains the standard config substitution
 * keys ({InstallDirectory}, {ScalaInstallDirectory}, {JavaInstallDirectory})
 * 
 * The corresponding values in the named parameters are substituted in the template.
 * The output is sent to standard out:
 *
 * Example:
 *
 *  setPathsFor.scala --installDir /tmp/drdigital/KamanjaInstall-1.4.0_2.11 --templateFile ClusterConfig.1.4.0.json --scalaHome `which scala` --javaHome `which java` >/tmp/clusterconfig.json 
 *
 * The captured file then can be used for metadata ingestion (e.g., kamanja command ).
 *
 **************************************************************************************/

import scala.io.Source
import java.util.regex.Pattern
import java.util.regex.Matcher


class MapSubstitution(template: String, subMap: scala.collection.immutable.Map[String, String]) {

	def findAndReplace(m: Matcher)(callback: String => String): String = {
		val sb = new StringBuffer
		while (m.find) {
			val replStr = subMap(m.group(1))
			m.appendReplacement(sb, callback(replStr))
		}
		m.appendTail(sb)
		sb.toString
	}

	def makeSubstitutions: String = {
		var retrStr = ""
		try {
			val patStr = """(\{[A-Za-z0-9_.-]+\})"""
			val m = Pattern.compile(patStr).matcher(template)
			retrStr = findAndReplace(m) { x => x }
		} catch {
			case e: Exception => retrStr = ""
			case e: Throwable => retrStr = ""
		}
		retrStr
	}

}

object MapSubstitution extends App {

	def usage: String = {
    """
Substitute the supplied Kamanja template file that contains the standard config substitution
keys ({InstallDirectory}, {ScalaInstallDirectory}, {JavaInstallDirectory}) with the supplied 
associated values.

Usage: setPathsFor --installDir <kamanja install dir> --templateFile <templateFile path> --scalaHome <Scala Home> --javaHome <Java Home> 
    """
	}


    override def main (args : Array[String]) {
        val arglist = args.toList
        if (args.isEmpty) {
            println(usage)
            sys.exit(1)
        }

        type OptionMap = Map[Symbol, String]
	    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
	      list match {
	        case Nil => map
	        case "--templateFile" :: value :: tail =>
	          nextOption(map ++ Map('templateFile -> value), tail)
	        case "--scalaHome" :: value :: tail =>
	          nextOption(map ++ Map('scalaHome -> value), tail)
	        case "--javaHome" :: value :: tail =>
	          nextOption(map ++ Map('javaHome -> value), tail)
	        case "--installDir" :: value :: tail =>
	          nextOption(map ++ Map('installDir -> value), tail)
	        case option :: tail => println("Unknown option " + option)
	          sys.exit(1)
	      }
	    }

	    val options = nextOption(Map(), arglist)

	    val templateFile : String = if (options.contains('templateFile)) options.apply('templateFile) else null
	    val scalaHome : String = if (options.contains('scalaHome)) options.apply('scalaHome) else null
        val javaHome : String = if (options.contains('javaHome)) options.apply('javaHome) else null
        val installDir : String = if (options.contains('installDir)) options.apply('installDir) else null

	    val ok : Boolean = templateFile != null && scalaHome != null && javaHome != null && installDir != null
	    if (! ok) {
	    	println("\ninvalid arguments\n")
	    	println(usage)
            sys.exit(1)
	    }

	    val template : String = Source.fromFile(templateFile, "ASCII").mkString
	    val subMap : Map[String,String] = Map[String,String]("{ScalaInstallDirectory}" -> scalaHome.trim, "{JavaInstallDirectory}" -> javaHome.trim, "{InstallDirectory}" -> installDir.trim)


	    /** do the substitutions */
	    val varSub = new MapSubstitution(template, subMap)
	    println(varSub.makeSubstitutions)  
    }
}

MapSubstitution.main(args)
