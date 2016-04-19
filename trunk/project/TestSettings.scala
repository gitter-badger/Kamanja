import sbt._
import Keys._
import TestConfigs._

object TestSettings
{
  lazy val testAll = TaskKey[Unit]("test-all")

  private lazy val itSettings =
    inConfig(TestConfigs.IntegrationTest)(Defaults.testSettings) ++
    Seq(
      fork in TestConfigs.IntegrationTest := false,
      parallelExecution in TestConfigs.IntegrationTest := false,
      scalaSource in TestConfigs.IntegrationTest := baseDirectory.value / "src/it/scala")

  private lazy val e2eSettings =
    inConfig(EndToEndTest)(Defaults.testSettings) ++
    Seq(
      fork in EndToEndTest := false,
      parallelExecution in EndToEndTest := false,
      scalaSource in EndToEndTest := baseDirectory.value / "src/e2e/scala")


  lazy val settings = itSettings ++ e2eSettings ++ Seq(
    testAll <<= (test in EndToEndTest).dependsOn((test in TestConfigs.IntegrationTest).dependsOn(test in Test)))

}