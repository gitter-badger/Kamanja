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

import akka.actor.{ Actor, ActorRef }
import akka.event.Logging
import akka.io.IO

import spray.routing.RequestContext
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._
import com.ligadata.kamanja.metadata._

import scala.util.{ Success, Failure }

import com.ligadata.MetadataAPI._
import com.ligadata.AuditAdapterInfo.AuditConstants

object AddOutputMsgService {
  case class Process(outputMsgJson: String, formatType: String)
}

class AddOutputMsgService(requestContext: RequestContext, userid: Option[String], password: Option[String], cert: Option[String]) extends Actor {

  import AddOutputMsgService._

  implicit val system = context.system
  import system.dispatcher
  val log = Logging(system, getClass)
  val APIName = "AddOutputMsgService"

  def receive = {
    case Process(outputMsgJson, formatType) =>
      process(outputMsgJson, formatType)
      context.stop(self)
  }
  def process(outputMsgJson: String, formatType: String): Unit = {
    log.info("Requesting AddOutputMsg {},{}", outputMsgJson, formatType)

    var nameVal: String = null
    if (formatType.equalsIgnoreCase("json")) {
      nameVal = APIService.extractNameFromJson(outputMsgJson, AuditConstants.OUTPUTMSG)
    } else {
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error:Unsupported format: " + formatType).toString)
      return
    }
    if (!MetadataAPIImpl.checkAuth(userid, password, cert, MetadataAPIImpl.getPrivilegeName("insert", "outputmsg"))) {
      MetadataAPIImpl.logAuditRec(userid, Some(AuditConstants.WRITE), AuditConstants.INSERTOBJECT, outputMsgJson, AuditConstants.FAIL, "", nameVal)
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error:UPDATE not allowed for this user").toString)
    } else {
      val apiResult = MetadataAPIOutputMsg.AddOutputMessage(outputMsgJson, formatType, userid)
      requestContext.complete(apiResult)
    }

  }
}