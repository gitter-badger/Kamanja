import sbtassembly.AssemblyPlugin.defaultShellScript
import sbt._
import Keys._

shellPrompt := { state => "sbt (%s)> ".format(Project.extract(state).currentProject.id) }



assemblyOption in assembly ~= {
  _.copy(prependShellScript = Some(defaultShellScript))
}

val kamanjaVersion = "1.4.0"

assemblyJarName in assembly := {
  s"${name.value}_${scalaBinaryVersion.value}-${kamanjaVersion}.jar"
}

// for some reason the merge strategy for non ligadata classes are not working and thus added those conflicting jars in exclusions
// this may result some run time errors

assemblyMergeStrategy in assembly := {
  // case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  // case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case PathList("META-INF", "maven", "jline", "jline", ps) if ps.startsWith("pom") => MergeStrategy.discard
  case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
  case x if x endsWith "google/common/annotations/GwtCompatible.class" => MergeStrategy.first
  case x if x endsWith "google/common/annotations/GwtIncompatible.class" => MergeStrategy.first
  case x if x endsWith "/apache/commons/beanutils/BasicDynaBean.class" => MergeStrategy.first
  case x if x endsWith "org/apache/commons/beanutils/BasicDynaBean.class" => MergeStrategy.last
  case x if x contains "org/apache/commons/collections" => MergeStrategy.last
  case x if x contains "org\\apache\\commons\\collections" => MergeStrategy.last
  case "log4j.properties" => MergeStrategy.first
  case "unwanted.txt" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)

}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  val excludes = Set("commons-beanutils-1.7.0.jar", "google-collections-1.0.jar", "commons-collections4-4.0.jar", "log4j-1.2.17.jar", "log4j-1.2.16.jar", "minlog-1.2.jar")
  cp filter { jar => excludes(jar.data.getName) }
}

name := "SimpleKafkaProducer"


version := "1.4.0"

//resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
//
//libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % "2.4.1"
//
//libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"
//
//libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"
//
//resolvers += "Apache repo" at "https://repository.apache.org/content/repositories/releases"
//
//libraryDependencies ++= Seq("org.apache.kafka" %% "kafka" % "0.8.2.2"
//
//                              exclude("javax.jms", "jms")
//                              exclude("com.sun.jdmk", "jmxtools")
//                              exclude("com.sun.jmx", "jmxri")
//)
//libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-actors" % _)
//
//libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9"
//
//libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9"

coverageMinimum := 80

coverageFailOnMinimum := false
