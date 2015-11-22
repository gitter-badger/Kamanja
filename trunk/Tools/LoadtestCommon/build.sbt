import AssemblyKeys._ // put this at the top of the file

import sbt._

import Keys._

shellPrompt := { state =>  "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

assemblySettings

jarName in assembly := { s"${name.value}-${version.value}.jar" }

name := "LoadtestCommon"

version := "0.0.0.1"

scalaVersion := "2.11.7"

libraryDependencies += "org.joda" % "joda-convert" % "1.6"

libraryDependencies += "joda-time" % "joda-time" % "2.8.2"

resolvers += "spring-milestones" at "http://repo.springsource.org/libs-milestone"

resolvers += "mvnrepository" at "http://mvnrepository.com/artifact"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.3"

libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.3"

libraryDependencies += "net.debasishg" %% "redisclient" % "2.13"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.7"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.7"

libraryDependencies += "org.rogach" %% "scallop" % "0.9.5"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11-RC1" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

scalacOptions += "-deprecation"

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  cp filter { _.data.getName == "google-collections-1.0.jar" }
}

