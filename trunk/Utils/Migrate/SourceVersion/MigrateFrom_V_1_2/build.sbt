name := "MigrateFrom_V_1_2"

version := "1.0"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.apache.logging.log4j" % "log4j-1.2-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.4.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.4.1"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.2.9" 

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.2.9" 

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

EclipseKeys.relativizeLibs := false

coverageMinimum := 80

coverageFailOnMinimum := false

coverageExcludedPackages := "com.ligadata.Migrate.MigrateFrom_V_1_2"