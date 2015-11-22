name := "SqlServer"

version := "0.1.0"

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "org.apache.commons" % "commons-dbcp2" % "2.1"

parallelExecution := false
