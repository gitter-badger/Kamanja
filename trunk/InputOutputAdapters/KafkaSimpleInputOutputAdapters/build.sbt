name := "KafkaSimpleInputOutputAdapters"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

resolvers += "Apache repo" at "https://repository.apache.org/content/repositories/releases"

libraryDependencies += "org.apache.kafka" % "kafka_2.10" % "0.8.1.1"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-actors" % _)

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9"

libraryDependencies += "com.typesafe" % "config" % "1.3.0"