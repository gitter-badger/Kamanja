name := "ExtDependencyLibs"

version := "1.0"


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % "2.4.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"
libraryDependencies += "org.ow2.asm" % "asm-tree" % "4.0"
libraryDependencies += "org.ow2.asm" % "asm-commons" % "4.0"
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-actors" % _)
libraryDependencies += "org.scala-lang" % "scala-actors" % scalaVersion.value

/////////////////////// MetadataAPI
libraryDependencies += "org.joda" % "joda-convert" % "1.6"
libraryDependencies += "joda-time" % "joda-time" % "2.8.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9"
libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9"
libraryDependencies += "org.apache.zookeeper" % "zookeeper" % "3.4.6"
libraryDependencies += "org.apache.curator" % "apache-curator" % "2.0.0-incubating"
libraryDependencies += "com.google.guava" % "guava" % "14.0.1"
libraryDependencies += "org.jpmml" % "pmml-evaluator" % "1.2.4"
libraryDependencies += "org.jpmml" % "pmml-model" % "1.2.5"
libraryDependencies += "org.jpmml" % "pmml-schema" % "1.2.5"
dependencyOverrides += "com.google.guava" % "guava" % "14.0.1"
libraryDependencies += "commons-codec" % "commons-codec" % "1.10"
libraryDependencies += "commons-io" % "commons-io" % "2.4"
libraryDependencies ++= Seq(
  "com." %% "chill" % "0.5.0",
  "org.apache.shiro" % "shiro-core" % "1.2.3",
  "org.apache.shiro" % "shiro-root" % "1.2.3"
)
//scalacOptions += "-deprecation"
//retrieveManaged := true
//parallelExecution := false



/////////////////////// SimpleKafkaProducer
resolvers += "Apache repo" at "https://repository.apache.org/content/repositories/releases"
libraryDependencies ++= Seq("org.apache.kafka" %% "kafka" % "0.8.2.2"

  exclude("javax.jms", "jms")
  exclude("com.sun.jdmk", "jmxtools")
  exclude("com.sun.jmx", "jmxri")
)


/////////////////////// GetComponent
libraryDependencies += "org.apache.hbase" % "hbase-client" % "1.0.2"
libraryDependencies += "org.apache.hbase" % "hbase-common" % "1.0.2"
libraryDependencies += "org.apache.hadoop" % "hadoop-common" % "2.7.1"
//libraryDependencies += "org.apache.kafka" %% "kafka" % "0.8.2.2"
libraryDependencies += "com.googlecode.json-simple" % "json-simple" % "1.1"
libraryDependencies += "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.3"
libraryDependencies += "log4j" % "log4j" % "1.2.17"
//scalacOptions += "-deprecation"




////////////////////// NodeInfoExtract
//net.virtualvoid.sbt.graph.Plugin.graphSettings




////////////////////// MetadataAPIService
//scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
resolvers ++= Seq(
  "spray repo" at "http://repo.spray.io/"
)
libraryDependencies ++= {
  val sprayVersion = "1.3.3"
  val akkaVersion = "2.3.9"
  Seq(
    "io.spray" %% "spray-can" % sprayVersion,
    "io.spray" %% "spray-routing" % sprayVersion,
    "io.spray" %% "spray-testkit" % sprayVersion,
    "io.spray" %% "spray-client" % sprayVersion,
    "io.spray" %%  "spray-json" % "1.3.2",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    //  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "ch.qos.logback" % "logback-classic" % "1.0.12",
    "org.apache.camel" % "camel-core" % "2.9.2"
  )
}





/////////////////////// PmmlTestTool
// 1.2.9 is currently used in other engine... use same here
libraryDependencies += "org.jpmml" % "pmml-evaluator" % "1.2.9"
libraryDependencies += "org.jpmml" % "pmml-model" % "1.2.9"
libraryDependencies += "org.jpmml" % "pmml-schema" % "1.2.9"
libraryDependencies += "com.beust" % "jcommander" % "1.48"
libraryDependencies += "com.codahale.metrics" % "metrics-core" % "3.0.2"
libraryDependencies += "org.glassfish.jaxb" % "jaxb-runtime" % "2.2.11"
//// Do not append Scala versions to the generated artifacts
//crossPaths := false
//// This forbids including Scala related libraries into the dependency
//autoScalaLibrary := false





/////////////////////// ClusterInstallerDriver
//// Do not append Scala versions to the generated artifacts
//crossPaths := false
//// This forbids including Scala related libraries into the dependency
//autoScalaLibrary := false





/////////////////////// MigrateManager
// should this be excluded from here ?
//// Do not append Scala versions to the generated artifacts
//crossPaths := false
//// This forbids including Scala related libraries into the dependency
//autoScalaLibrary := false
libraryDependencies += "com.google.code.gson" % "gson" % "2.3.1"




/////////////////////// InstallerDriver
//already available
//scalacOptions += "-deprecation"




//////////////////////// KVInit
//already available




////////////////////// JdbcDataCollector
//already available





////////////////////// CleanUtil
//already available



////////////////////// FileDataConsumer
//already available
//libraryDependencies ++= {
//  val sprayVersion = "1.3.3"
//  val akkaVersion = "2.3.9"
//  Seq(
//    "org.apache.kafka" %% "kafka" % "0.8.2.2",
//    "org.scala-lang" % "scala-actors" % scalaVersion.value
//  )
//}


////////////////////// Metadata
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"
libraryDependencies += "com.novocode" % "junit-interface" % "0.11-RC1" % "test"
//testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")



////////////////////// KamanjaBase
libraryDependencies += "com.google.code.findbugs" % "jsr305" % "1.3.9"


////////////////////// Bootstrap
//scalacOptions += "-deprecation"
unmanagedSourceDirectories in Compile <+= (scalaVersion, sourceDirectory in Compile) {
  case (v, dir) if v startsWith "2.10" => dir / "scala_2.10"
  case (v, dir) if v startsWith "2.11" => dir / "scala_2.11"
}




////////////////////// Serialize
libraryDependencies ++= Seq(
  "com.twitter" %% "chill" % "0.5.0"
)
libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.6.0"
//scalacOptions += "-deprecation"




////////////////////// ZooKeeperListener
libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-collections4" % "4.0",
  "commons-configuration" % "commons-configuration" % "1.7",
  "commons-logging" % "commons-logging" % "1.1.1",
  "org.apache.curator" % "curator-client" % "2.6.0",
  "org.apache.curator" % "curator-framework" % "2.6.0",
  "org.apache.curator" % "curator-recipes" % "2.6.0"
//  "com.googlecode.json-simple" % "json-simple" % "1.1"
)


////////////////////// ZooKeeperLeaderLatch
//already available


////////////////////// Exceptions
//already available


////////////////////// KamanjaUtils
//already available


////////////////////// TransactionService
//already available



////////////////////// DataDelimiters
//already available


////////////////////// InputOutputAdapterBase
//already available





////////////////////// StorageManager
//already available



////////////////////// MessageDef
//not sure if needed
libraryDependencies += "metadata" %% "metadata" % "1.0"




////////////////////// PmmlCompiler
//net.virtualvoid.sbt.graph.Plugin.graphSettings




////////////////////// ZooKeeperClient
//already available