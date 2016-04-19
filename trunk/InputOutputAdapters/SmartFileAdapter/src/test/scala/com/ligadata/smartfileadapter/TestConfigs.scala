package com.ligadata.smartfileadapter

import org.scalatest._
import com.ligadata.AdaptersConfiguration.SmartFileAdapterConfiguration
import com.ligadata.InputOutputAdapterInfo.AdapterConfiguration
/**
  * Created by Yasser on 3/10/2016.
  */
class TestConfigs extends FunSpec with BeforeAndAfter with ShouldMatchers with BeforeAndAfterAll with GivenWhenThen {

  val adapterSpecificCfgJson =
    """
      |{
      |  "Type": "Hdfs",
      |  "ConnectionConfig": {
      |    "HostLists": "10.20.30.40:2000,10.20.30.40:2001",
      |    "UserId": "uid",
      |    "Password": "pwd"
      |  },
      |  "MonitoringConfig": {
      |  "Locations": "/data/input,/tmp/input",
      |  "TargetMoveDir": "/data/processed",
      |    "MaxTimeWait": "5000"
      |  }
      |}
    """.stripMargin

  val adapterSpecificCfgJsonKerberos =
  """{
    |    "Type": "Hdfs",
    |    "ConnectionConfig": {
    |      "HostLists": "10.20.30.40:2000,10.20.30.40:2001",
    |      "Authentication": "kerberos",
    |	     "Principal": "user@domain.com",
    |	     "Keytab": "/tmp/user.keytab"
    |    },
    |    "MonitoringConfig": {
    |	  "Locations": "/data/input,/tmp/input",
    |   "TargetMoveDir": "/data/processed",
    |      "MaxTimeWait": "5000"
    |    }
    |}""".stripMargin

  describe("Test smart file adapter adapterSpecificCfg json parsing") {

    it("should get right values for attributes (type, userid, password, hostslist, location and waitingTimeMS)") {

      val inputConfig = new AdapterConfiguration()
      inputConfig.Name = "TestInput_2"
      inputConfig.className = "com.ligadata.InputAdapters.SamrtFileInputAdapter$"
      inputConfig.jarName = "smartfileinputoutputadapters_2.10-1.0.jar"
      //inputConfig.dependencyJars = new Set()
      inputConfig.adapterSpecificCfg = adapterSpecificCfgJson

      val conf = SmartFileAdapterConfiguration.getAdapterConfig(inputConfig)

      conf._type shouldEqual  "Hdfs"
      conf.connectionConfig.userId shouldEqual "uid"
      conf.connectionConfig.password shouldEqual "pwd"
      conf.connectionConfig.hostsList.mkString(",") shouldEqual "10.20.30.40:2000,10.20.30.40:2001"
      conf.monitoringConfig.locations.mkString(",") shouldEqual "/data/input,/tmp/input"
      conf.monitoringConfig.waitingTimeMS shouldEqual 5000
    }
  }


  describe("Test smart file adapter adapterSpecificCfg json parsing with kerberos auth") {

    it("should get right values for attributes (type, principal, password, keytab, location and waitingTimeMS") {

      val inputConfig = new AdapterConfiguration()
      inputConfig.Name = "TestInput_2"
      inputConfig.className = "com.ligadata.InputAdapters.SamrtFileInputAdapter$"
      inputConfig.jarName = "smartfileinputoutputadapters_2.10-1.0.jar"
      //inputConfig.dependencyJars = new Set()
      inputConfig.adapterSpecificCfg = adapterSpecificCfgJsonKerberos

      val conf = SmartFileAdapterConfiguration.getAdapterConfig(inputConfig)

      conf._type shouldEqual  "Hdfs"
      conf.connectionConfig.authentication shouldEqual "kerberos"
      conf.connectionConfig.principal shouldEqual "user@domain.com"
      conf.connectionConfig.keytab shouldEqual "/tmp/user.keytab"
      conf.connectionConfig.hostsList.mkString(",") shouldEqual "10.20.30.40:2000,10.20.30.40:2001"
      conf.monitoringConfig.locations.mkString(",") shouldEqual "/data/input,/tmp/input"
      conf.monitoringConfig.waitingTimeMS shouldEqual 5000
    }
  }
}
