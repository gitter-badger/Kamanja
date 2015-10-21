name := "JpmmlAdapter"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.jpmml" % "pmml-evaluator" % "1.2.4"

libraryDependencies += "org.jpmml" % "pmml-model" % "1.2.5"

libraryDependencies += "org.jpmml" % "pmml-schema" % "1.2.5"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

scalacOptions += "-deprecation"
