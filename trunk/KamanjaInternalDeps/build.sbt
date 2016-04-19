import sbt.Keys._
import sbt._


name := "KamanjaInternalDeps"

//version := "1.0"
val kamanjaVersion = "1.4.0"

assemblyJarName in assembly := {
  s"${name.value}_${scalaBinaryVersion.value}-${kamanjaVersion}.jar"
}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
  val excludes = Set("commons-beanutils-1.7.0.jar", "google-collections-1.0.jar", "commons-collections4-4.0.jar", "log4j-1.2.17.jar", "log4j-1.2.16.jar", "commons-collections-4-4.0.jar", "scalatest_2.11-2.2.0.jar"
    , "scala-reflect-2.11.0.jar", "akka-actor_2.11-2.3.2.jar", "scala-reflect-2.11.2.jar", "scalatest_2.11-2.2.4.jar", "joda-time-2.9.1-javadoc.jar", "voldemort-0.96.jar", "scala-compiler-2.11.0.jar", "guava-14.0.1.jar"
    , "minlog-1.2.jar", s"KamanjaInternalDeps_2.10-${kamanjaVersion}.jar", s"KamanjaInternalDeps_2.11-${kamanjaVersion}.jar")
  cp filter { jar => excludes(jar.data.getName) }
}

unmanagedBase <<= baseDirectory { base => base / "custom_lib" }

unmanagedJars in Compile <<= baseDirectory map { base => (base ** "*.jar").classpath }

excludeFilter in unmanagedJars := s"${name.value}_${scalaBinaryVersion.value}-${kamanjaVersion}.jar"
