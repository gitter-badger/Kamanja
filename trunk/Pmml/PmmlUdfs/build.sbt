name := "PmmlUdfs"

version := "1.0"

scalacOptions += "-deprecation"

net.virtualvoid.sbt.graph.Plugin.graphSettings

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4"

coverageEnabled := false