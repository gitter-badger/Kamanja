import sbt.Keys._
import sbt._

name := "CleanUtil"

version := "1.4.0"

shellPrompt := { state => "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

//libraryDependencies ++= Seq (
//  "com.101tec" % "zkclient" % "0.6",
//  "org.apache.kafka" %% "kafka" % "0.8.2.2",
//  "org.apache.logging.log4j" % "log4j-core" % "2.4.1",
//  "org.json4s" %% "json4s-native" % "{latestVersion}",
//  "org.json4s" %% "json4s-jackson" % "{latestVersion}",
//  "org.apache.curator" % "curator-client" % "2.6.0",
//  "org.apache.curator" % "curator-framework" % "2.6.0",
//  "org.apache.curator" % "curator-recipes" % "2.6.0",
//  "org.apache.curator" % "curator-test" % "2.8.0"
//    exclude("javax.jms", "jms")
//    exclude("com.sun.jdmk", "jmxtools")
//    exclude("com.sun.jmx", "jmxri")
//)

fork := true



test in assembly := {}

val kamanjaVersion = "1.4.0"

assemblyJarName in assembly := {
  s"${name.value}_${scalaBinaryVersion.value}-${kamanjaVersion}.jar"
}

assemblyMergeStrategy in assembly := {
  // case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  // case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case PathList("META-INF", "maven", "jline", "jline", ps) if ps.startsWith("pom") => MergeStrategy.discard
  case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
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
  case x if x contains "org/apache/commons/collections" => MergeStrategy.last
  case x if x contains "org\\apache\\commons\\collections" => MergeStrategy.last
  case x if x contains "com.fasterxml.jackson.core" => MergeStrategy.first
  case x if x contains "com/fasterxml/jackson/core" => MergeStrategy.first
  case x if x contains "com\\fasterxml\\jackson\\core" => MergeStrategy.first
  case x if x contains "commons-logging" => MergeStrategy.first
  case "log4j.properties" => MergeStrategy.first
  case "unwanted.txt" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)

}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  val excludes = Set("commons-beanutils-1.7.0.jar", "google-collections-1.0.jar", "commons-collections4-4.0.jar", "log4j-1.2.17.jar", "log4j-1.2.16.jar")
  cp filter { jar => excludes(jar.data.getName) }
}

coverageMinimum := 80

coverageFailOnMinimum := false