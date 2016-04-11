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

object AddMessageService {
  case class Process(messageJson:String)
}

class AddMessageService(requestContext: RequestContext, userid:Option[String], password:Option[String], cert:Option[String], tenantId: Option[String]) extends Actor {

  import AddMessageService._
  
  implicit val system = context.system
  import system.dispatcher
  val log = Logging(system, getClass)
  val APIName = "AddMessageService"
  
  def receive = {
    case Process(messageJson) =>
      process(messageJson)
      context.stop(self)
  }
  
  def process(messageJson:String) = {
    
    log.debug("Requesting AddMessage {}",messageJson)

    var nameVal = APIService.extractNameFromJson(messageJson,AuditConstants.MESSAGE)
    
    if (!MetadataAPIImpl.checkAuth(userid,password,cert, MetadataAPIImpl.getPrivilegeName("insert","message"))) {
      MetadataAPIImpl.logAuditRec(userid,Some(AuditConstants.WRITE),AuditConstants.INSERTOBJECT,messageJson,AuditConstants.FAIL,"",nameVal)   
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null,  "Error:UPDATE not allowed for this user").toString )      
    } else {
      val apiResult = MetadataAPIImpl.AddMessage(messageJson,"JSON", userid, tenantId)
      requestContext.complete(apiResult)
    }
  }
}
