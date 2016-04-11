name := "MigrateTo_V_1_4"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9"

libraryDependencies += "com.google.code.gson" % "gson" % "2.3.1"

EclipseKeys.relativizeLibs := false


// newly added
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % "2.4.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"
libraryDependencies += "org.ow2.asm" % "asm-tree" % "4.0"
libraryDependencies += "org.ow2.asm" % "asm-commons" % "4.0"
libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-actors" % _) // ???
libraryDependencies += "org.scala-lang" % "scala-actors" % scalaVersion.value

libraryDependencies ++= Seq(
  "commons-logging" % "commons-logging" % "1.1.1",
  "org.apache.curator" % "curator-client" % "2.6.0",
  "org.apache.curator" % "curator-framework" % "2.6.0",
  "org.apache.curator" % "curator-recipes" % "2.6.0"
)

coverageExcludedPackages := "com.ligadata.Migrate.MigrateTo_V_1_3"

