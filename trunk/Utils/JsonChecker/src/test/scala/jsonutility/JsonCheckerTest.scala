package com.ligadata.jsonutility

import org.scalatest._
import Matchers._
/**
  * Created by Yousef on 3/23/2016.
  */
class JsonCheckerTest extends FeatureSpec with GivenWhenThen {

  private def getResourceFullPath(resourcePath : String): String ={
    val os = System.getProperty("os.name")
    val isWidnows = os.toLowerCase.contains("windows")
    val path = getClass.getResource(resourcePath).getPath
    val finalPath = if(isWidnows) path.substring(1) else path
    finalPath
  }

  info("This an example to test JsonChecker class")
  info("I want to be able to create an JsonChecker object")
  info("So i can access the variables")
  info("And get all variables when I need it using functions")
  feature("JsonChecker object") {
    val JsonChecker = new JsonChecker()
    scenario("Unit Tests for all JsonChecker function") {

      Given("Initiate variables")
      val filePath = getResourceFullPath("/ConfigParameters.json")
      val filePathWithNoExtension = getResourceFullPath("/ConfigParameters")
      val filePathEmpty = getResourceFullPath("/ConfigParametersTest.json")
      val filePathIncorrectStructure = getResourceFullPath("/ConfigParametersIncorrect.json")

      Given("Test readFile function")

      When("The file includes data")
      val fileContent = JsonChecker.ReadFile(filePath)
      Then("The fileContent value should be set and FileContent length should not equal 0")
      fileContent should not be (null)
      fileContent.length should not equal (0)

      When("file does not include data")
      val fileContentEmpty = JsonChecker.ReadFile(filePathEmpty)
      Then("The fileContent length should be 0")
      fileContentEmpty.length should be(0)

      Given("Test FileExist function")

      When("The file in path")
      val fileExitFlag = JsonChecker.FileExist(filePath)
      Then("The fileExist flag should be true")
      fileExitFlag should be(true)

      When("The file not in path")
      Then("a NullPointException should be raise")
      intercept[NullPointerException] {
        val fileExitFlag = JsonChecker.FileExist(getResourceFullPath("/Config.json"))
      }

      Given("Test FindFileExtension function")

      When("The extension of file is JSON")
      var fileExtinsionFlag = JsonChecker.FindFileExtension(filePath)
      Then("The file extension Flag should be true")
      fileExtinsionFlag should be(true)

      When("The extension of file is not JSON")
      fileExtinsionFlag = JsonChecker.FindFileExtension(filePathWithNoExtension)
      Then("The fileExist flag should be false")
      fileExtinsionFlag should be(false)

      Given("Test ParseFile function")

      When("The structure of file is correct")
      Then("no exception should be raise")
      noException should be thrownBy {
        JsonChecker.ParseFile(fileContent)
      }
    }
  }
}
