/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.metadataapiservice

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import akka.io.IO
import com.ligadata.kamanja.metadata._
import spray.routing.RequestContext
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._

import scala.util.{ Success, Failure }

import com.ligadata.MetadataAPI._

object GetHeartbeatService {
  case class Process(nodeIds:String, detailsLevel : DetailsLevel.DetailsLevel)
}

/**
 * @author danielkozin
 */
class GetHeartbeatService(requestContext: RequestContext, userid:Option[String], password:Option[String], cert:Option[String]) extends Actor  {
  import GetHeartbeatService._ 
  import system.dispatcher
  
  implicit val system = context.system
  val log = Logging(system, getClass)
  val APIName = "GetHeartbeatService"
  
  def receive = {
    case Process(ids, detailsLevel) =>
      process(ids, detailsLevel)
      context.stop(self)
  }
  
  def process(ids:String, detailsLevel : DetailsLevel.DetailsLevel): Unit = {
    var apiResult : String = ""

    // NodeIds is a JSON array of nodeIds.
    if (ids == null || (ids != null && ids.length == 0))
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Invalid BODY in a POST request.  Expecting either an array of nodeIds or an empty array for all").toString)  
  
    if (!MetadataAPIImpl.checkAuth(userid,password,cert, MetadataAPIImpl.getPrivilegeName("get","heartbeat"))) {
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error:Checking Heartbeat is not allowed for this user").toString )
    } else {

      detailsLevel match {
        case DetailsLevel.ALL => {
          apiResult = MetadataAPIImpl.getHealthCheck(ids, userid)
        }
        case DetailsLevel.NODESONLY => {
          apiResult = MetadataAPIImpl.getHealthCheckNodesOnly(ids, userid)
        }
        case DetailsLevel.COMPONENTSNAMES => {
          apiResult = MetadataAPIImpl.getHealthCheckComponentNames(ids, userid)
        }
        case DetailsLevel.SPECIFICCOMPONENTS => {
          apiResult = MetadataAPIImpl.getHealthCheckComponentDetailsByNames(ids, userid)
        }
        case _ => {
          apiResult = ("Value (" + detailsLevel + ") is not supported ")
          apiResult = new ApiResult(ErrorCodeConstants.Failure, APIName, null,  "Invalid URL:" + apiResult).toString
        }
      }
      requestContext.complete(apiResult)
    }
  }  
  
}

object DetailsLevel extends Enumeration {
  type DetailsLevel = Value
  val ALL = Value("all")
  val NODESONLY = Value("nodesonly")
  val COMPONENTSNAMES = Value("componentnames")
  val SPECIFICCOMPONENTS = Value("specificcomponents")
  val UNKNOWN = Value("unknown")

  def fromString(typstr : String) : DetailsLevel = {
    val typ : DetailsLevel.Value = typstr.toLowerCase match {
      case "all" => ALL
      case "nodesonly" => NODESONLY
      case "componentnames" => COMPONENTSNAMES
      case "specificcomponents" => SPECIFICCOMPONENTS
      case _ => UNKNOWN
    }
    typ
  }
}