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

import spray.routing.RequestContext
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._
import com.ligadata.kamanja.metadata._

import scala.util.{ Success, Failure }

import com.ligadata.MetadataAPI._
import com.ligadata.AuditAdapterInfo.AuditConstants

object AddContainerService {
  case class Process(containerJson:String)
}

class AddContainerService(requestContext: RequestContext, userid:Option[String], password:Option[String], cert:Option[String], tenantId: Option[String]) extends Actor {

  import AddContainerService._
  
  implicit val system = context.system
  import system.dispatcher
  val log = Logging(system, getClass)
  val APIName = "AddContainerService"
  
  def receive = {
    case Process(containerJson) =>
      process(containerJson)
      context.stop(self)
  }
  
  def process(containerJson:String) = {
    log.debug("Requesting AddContainer {}",containerJson)

    var nameVal = APIService.extractNameFromJson(containerJson,AuditConstants.CONTAINER) 
    
    if (!MetadataAPIImpl.checkAuth(userid,password,cert, MetadataAPIImpl.getPrivilegeName("insert","container"))) {
      MetadataAPIImpl.logAuditRec(userid,Some(AuditConstants.WRITE),AuditConstants.INSERTOBJECT,containerJson,AuditConstants.FAIL,"",nameVal)
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error:UPDATE not allowed for this user").toString )
    } else { 
      val apiResult = MetadataAPIImpl.AddContainer(containerJson,"JSON",userid, tenantId)
      requestContext.complete(apiResult)    
    }   
  }
}
