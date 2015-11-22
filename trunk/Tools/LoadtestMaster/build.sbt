import AssemblyKeys._ // put this at the top of the file

import sbt._

import Keys._

shellPrompt := { state =>  "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

assemblySettings

assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

jarName in assembly := { s"${name.value}-${version.value}" }

name := "LoadtestMaster"

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

libraryDependencies += "org.mapdb" % "mapdb" % "1.0.6"

scalacOptions += "-deprecation"

excludedJars in assembly <<= (fullClasspath in assembly) map
{
	cp => val excludes = Set("google-collections-1.0.jar"
						, "commons-beanutils-1.7.0.jar"
						, "commons-collections4-4.0.jar"
						, "slf4j-log4j12-1.7.5.jar")
		   cp filter { jar => excludes(jar.data.getName) }
}

