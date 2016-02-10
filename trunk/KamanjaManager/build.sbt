import AssemblyKeys._ // put this at the top of the file
import sbt._
import Keys._

shellPrompt := { state =>  "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

assemblySettings

assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

jarName in assembly := { s"${name.value}-${version.value}" }

//crossScalaVersions := Seq("2.10.4", "2.11.7")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    // case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
    // case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
    case PathList("META-INF", "maven","jline","jline", ps) if ps.startsWith("pom") => MergeStrategy.discard
    case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
    case x if x endsWith "google/common/annotations/GwtCompatible.class" => MergeStrategy.first
    case x if x endsWith "google/common/annotations/GwtIncompatible.class" => MergeStrategy.first
    case x if x endsWith "/apache/commons/beanutils/BasicDynaBean.class" => MergeStrategy.first
    case x if x endsWith "com\\ligadata\\kamanja\\metadataload\\MetadataLoad.class" => MergeStrategy.first
    case x if x endsWith "com/ligadata/kamanja/metadataload/MetadataLoad.class" => MergeStrategy.first
    case x if x endsWith "org/apache/commons/beanutils/BasicDynaBean.class" => MergeStrategy.last
    case x if x endsWith "com\\esotericsoftware\\minlog\\Log.class" => MergeStrategy.first
    case x if x endsWith "com\\esotericsoftware\\minlog\\Log$Logger.class" => MergeStrategy.first
    case x if x endsWith "com/esotericsoftware/minlog/Log.class" => MergeStrategy.first
    case x if x endsWith "com/esotericsoftware/minlog/Log$Logger.class" => MergeStrategy.first
    case x if x endsWith "com\\esotericsoftware\\minlog\\pom.properties" => MergeStrategy.first
    case x if x endsWith "com/esotericsoftware/minlog/pom.properties" => MergeStrategy.first
    case x if x contains "com.esotericsoftware.minlog\\minlog\\pom.properties" => MergeStrategy.first
    case x if x contains "com.esotericsoftware.minlog/minlog/pom.properties" => MergeStrategy.first
    case x if x contains "org\\objectweb\\asm\\" => MergeStrategy.last
    case x if x contains "org/objectweb/asm/" => MergeStrategy.last
    case x if x contains "org/apache/commons/collections" =>  MergeStrategy.last
    case x if x contains "org\\apache\\commons\\collections" =>  MergeStrategy.last
    case x if x contains "com.fasterxml.jackson.core" => MergeStrategy.first
    case x if x contains "com/fasterxml/jackson/core" => MergeStrategy.first
    case x if x contains "com\\fasterxml\\jackson\\core" => MergeStrategy.first
    case x if x contains "commons-logging" => MergeStrategy.first
//second try
   case x if x contains "com/ligadata/KamanjaManager/ActionOnAdaptersMap.class" =>
    CrossVersion.partialVersion(scalaVersion.value) match {
	case Some((2, scalaMajor)) if scalaMajor >= 11 => MergeStrategy.first
	case Some((2, scalaMinor)) if scalaMinor < 11 => MergeStrategy.last
    }
    case "log4j.properties" => MergeStrategy.first
    case "unwanted.txt"     => MergeStrategy.discard
    case x => old(x)
  }
}
// { s"${scalaVersion}" }

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  val excludes = Set("commons-beanutils-1.7.0.jar", "google-collections-1.0.jar", "commons-collections4-4.0.jar" )
  cp filter { jar => excludes(jar.data.getName) }
}

unmanagedBase <<= baseDirectory { base => base / "custom_lib" }

unmanagedJars in Compile <<= baseDirectory map { base => (base ** "*.jar").classpath }

name := "KamanjaManager"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"
libraryDependencies += "org.ow2.asm" % "asm-tree" % "4.0"
libraryDependencies += "org.ow2.asm" % "asm-commons" % "4.0"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-actors" % _)
libraryDependencies += "org.scala-lang" % "scala-actors" % scalaVersion.value

