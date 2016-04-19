name := "Bootstrap"

version := "1.0"

scalacOptions += "-deprecation"
//
unmanagedSourceDirectories in Compile <+= (scalaVersion, sourceDirectory in Compile) {
  case (v, dir) if v startsWith "2.10" => dir / "scala_2.10"
  case (v, dir) if v startsWith "2.11" => dir / "scala_2.11"
}

coverageMinimum := 80

coverageFailOnMinimum := false
