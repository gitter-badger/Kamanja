name := "jtm"

version := "1.0"

scalaVersion := "2.11.7"

shellPrompt := { state =>  "sbt (%s)> ".format(Project.extract(state).currentProject.id) }

fork in (run) := true

net.virtualvoid.sbt.graph.Plugin.graphSettings

parallelExecution in Test := false

libraryDependencies += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) =>
       "org.rogach" % s"scallop_2.$scalaMajor"  % "0.9.5"
    case _ => throw new Exception("Unsupported version")
  }
}

libraryDependencies += "com.google.code.gson" % "gson" % "2.5"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test->default"

libraryDependencies += "com.novocode" % "junit-interface" % "0.9" % "test->default"

libraryDependencies += "junit" % "junit" % "4.11" % "test->default"

libraryDependencies += "org.skyscreamer" % "jsonassert" % "1.3.0"  % "test->default"

libraryDependencies += "org.aicer.grok" % "grok" % "0.9.0"


