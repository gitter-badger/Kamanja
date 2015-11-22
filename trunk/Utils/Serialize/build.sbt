name := "Serialize"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies ++= Seq(
"com.twitter" %% "chill" % "0.5.0",
"org.scalameta" %% "quasiquotes" % "0.0.3"
)

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.6.0" 

scalacOptions += "-deprecation"
