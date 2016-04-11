import com.ligadata.MetadataAPI.MonitorAPIImpl
import org.scalatest._

class TestMonitoring extends FunSpec with BeforeAndAfter with ShouldMatchers with BeforeAndAfterAll {

  override def beforeAll() {
    fillHealthInfo()
  }

  describe("heart beat info retrieval test") {

    it("should get all info") {
      val result = MonitorAPIImpl.getHeartbeatInfo(List())
      val expected = """[{"Name":"Node2","Components":[{"Name":"testout_1","LastSeen":"2015-08-0418: 26: 12","Description":"kafka output","Metrics":[],"StartTime":"2015-08-0418: 24: 42","Type":"Output"}],"LastSeen":"2016-03-0418: 26: 12","UniqueId":20,"Version":"1.3.0.0","StartTime":"2016-03-0418: 24: 42"},{"Name":"Node1","Components":[{"Name":"testin_1","LastSeen":"2015-08-0418: 26: 12","Description":"kafka input","Metrics":[],"StartTime":"2015-08-0418: 24: 42","Type":"Input"},{"Name":"hbase_storage_1","LastSeen":"2015-08-0418: 26: 12","Description":"hbase storage adapter","Metrics":[],"StartTime":"2015-08-0418: 24: 42","Type":"Storage"}],"LastSeen":"2015-08-0418: 26: 12","UniqueId":19,"Version":"1.3.0.0","StartTime":"2015-08-0418: 24: 42"}]"""
      result shouldEqual expected
      //println("all hb :\n"+result)
    }

    it("should get only nodes info") {
      val result = MonitorAPIImpl.getHBNodesOnly(List())
      val expected = """[{"Name":"Node2","LastSeen":"2016-03-0418: 26: 12","UniqueId":20,"Version":"1.3.0.0","StartTime":"2016-03-0418: 24: 42"},{"Name":"Node1","LastSeen":"2015-08-0418: 26: 12","UniqueId":19,"Version":"1.3.0.0","StartTime":"2015-08-0418: 24: 42"}]"""
      result shouldEqual expected
      //println("nodes only hb :\n"+result)
    }

    it("should get only component name+type info") {
      val result = MonitorAPIImpl.getHBComponentNames(List())
      //println("only component name+type hb :\n"+result)
      val expected = """[{"Name":"Node2","Components":[{"Type":"Output","Name":"testout_1"}],"LastSeen":"2016-03-0418: 26: 12","UniqueId":20,"Version":"1.3.0.0","StartTime":"2016-03-0418: 24: 42"},{"Name":"Node1","Components":[{"Type":"Input","Name":"testin_1"},{"Type":"Storage","Name":"hbase_storage_1"}],"LastSeen":"2015-08-0418: 26: 12","UniqueId":19,"Version":"1.3.0.0","StartTime":"2015-08-0418: 24: 42"}]"""
      result shouldEqual expected
    }

    it("should get only these components info (testout_1, testin_1)") {
      val result = MonitorAPIImpl.getHBComponentDetailsByNames(List("testout_1", "testin_1"))
      //println("specific components only hb :\n"+result)
      val expected = """[{"Name":"Node2","Components":[{"Name":"testout_1","LastSeen":"2015-08-0418: 26: 12","Description":"kafka output","Metrics":[],"StartTime":"2015-08-0418: 24: 42","Type":"Output"}],"LastSeen":"2016-03-0418: 26: 12","UniqueId":20,"Version":"1.3.0.0","StartTime":"2016-03-0418: 24: 42"},{"Name":"Node1","Components":[{"Name":"testin_1","LastSeen":"2015-08-0418: 26: 12","Description":"kafka input","Metrics":[],"StartTime":"2015-08-0418: 24: 42","Type":"Input"}],"LastSeen":"2015-08-0418: 26: 12","UniqueId":19,"Version":"1.3.0.0","StartTime":"2015-08-0418: 24: 42"}]"""
      result shouldEqual expected
    }
  }

  val inputHealthInfoNode1=
    """
      {
      |    "Name": "Node1",
      |    "Components": [
      |      {
      |        "Name": "testin_1",
      |        "LastSeen": "2015-08-0418: 26: 12",
      |        "Description": "kafka input",
      |        "StartTime": "2015-08-0418: 24: 42",
      |        "Type": "Input",
      |        "Metrics": []
      |      },
      |      {
      |        "Name": "hbase_storage_1",
      |        "LastSeen": "2015-08-0418: 26: 12",
      |        "Description": "hbase storage adapter",
      |        "StartTime": "2015-08-0418: 24: 42",
      |        "Type": "Storage",
      |        "Metrics": []
      |      }
      |    ],
      |    "LastSeen": "2015-08-0418: 26: 12",
      |    "UniqueId": 19,
      |    "Version": "1.3.0.0",
      |    "StartTime": "2015-08-0418: 24: 42"
      |  }
    """.stripMargin

  val inputHealthInfoNode2=
    """
      |  {
      |    "Name": "Node2",
      |    "Components": [
      |      {
      |        "Name": "testout_1",
      |        "LastSeen": "2015-08-0418: 26: 12",
      |        "Description": "kafka output",
      |        "StartTime": "2015-08-0418: 24: 42",
      |        "Type": "Output",
      |        "Metrics": []
      |      }
      |    ],
      |    "LastSeen": "2016-03-0418: 26: 12",
      |    "UniqueId": 20,
      |    "Version": "1.3.0.0",
      |    "StartTime": "2016-03-0418: 24: 42"
      |  }
    """.stripMargin


  //puts some heart beat info using MonitorAPIImpl.updateHeartbeatInfo
  def fillHealthInfo(): Unit ={

    println("putting some heart beat info")
    MonitorAPIImpl.updateHeartbeatInfo("CHILD_ADDED", "testin_1/Node1",inputHealthInfoNode1.getBytes(), null)
    MonitorAPIImpl.updateHeartbeatInfo("CHILD_ADDED", "testout_1/Node2",inputHealthInfoNode2.getBytes(), null)
    val hb = MonitorAPIImpl.getHeartbeatInfo(List())
  }
}