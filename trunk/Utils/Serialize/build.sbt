name := "Serialize"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies ++= Seq(
"com.twitter" %% "chill" % "0.5.0",
"org.scalameta" %% "quasiquotes" % "0.0.3"
)

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.6.0" 

scalacOptions += "-deprecation"
