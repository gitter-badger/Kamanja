name := "HBase"

version := "0.1.0"

scalaVersion := "2.10.4"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.apache.hbase" % "hbase-client" % "0.98.4-hadoop2"

libraryDependencies += "org.apache.hbase" % "hbase-common" % "0.98.4-hadoop2"

libraryDependencies += "org.apache.hadoop" % "hadoop-common" % "2.4.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies += "log4j" % "log4j" % "1.2.17"

parallelExecution := false
