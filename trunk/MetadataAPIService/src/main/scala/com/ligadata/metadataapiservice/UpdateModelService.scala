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
import com.ligadata.MetadataAPI.MetadataAPI.ModelType
import spray.routing.RequestContext
import spray.httpx.SprayJsonSupport
import spray.client.pipelining._
import scala.util.{ Success, Failure }
import com.ligadata.MetadataAPI._
import com.ligadata.kamanja.metadata._
import com.ligadata.AuditAdapterInfo.AuditConstants
import scala.util.control._
import org.apache.logging.log4j._

object UpdateModelService {
  case class Process(pmmlStr:String)
}

class UpdateModelService(requestContext: RequestContext, userid:Option[String], password:Option[String], cert:Option[String], modelCompileInfo: Option[String], tenantId: Option[String]) extends Actor {

  import UpdateModelService._
  
  implicit val system = context.system
  import system.dispatcher
  val log = Logging(system, getClass)
  val APIName = "UpdateModelService"

  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)
  
  def receive = {
    case Process(pmmlStr) =>
      process(pmmlStr)
      context.stop(self)
  }
  
  def process(pmmlStr:String) = {
    
    log.debug("Requesting UpdateModel {}",pmmlStr)

    var nameVal = APIService.extractNameFromPMML(pmmlStr) 

    if (!MetadataAPIImpl.checkAuth(userid,password,cert, MetadataAPIImpl.getPrivilegeName("update","model"))) {
       MetadataAPIImpl.logAuditRec(userid,Some(AuditConstants.WRITE),AuditConstants.UPDATEOBJECT,pmmlStr,AuditConstants.FAIL,"",nameVal)
      requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error:UPDATE not allowed for this user").toString )
    } else {

      // Ok, if this is a KPMML model, we dont need any additional info for compilation, its all enclosed in the model.  for normal PMML,
      // we need to know ModelName, Version, and associated Message.  modelCompileInfo will be set if this is PMML, and not set if KPMML
      if (modelCompileInfo == None) {
        log.info ("No configuration information provided, assuming Kamanja PMML implementation.")
        val apiResult = MetadataAPIImpl.UpdateModel(ModelType.KPMML, pmmlStr, userid)
        requestContext.complete(apiResult)
      } else {
        val cInfo = modelCompileInfo.getOrElse("")

        // Error if nothing specified in the modelCompileInfo
        if (cInfo.equalsIgnoreCase(""))
          requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error: modelconfig is not specified, PMML model is required to have Model Compilation Information.").toString)

        val compileConfigTokens = cInfo.split(",")
        if (compileConfigTokens.size < 2 ||
            compileConfigTokens.size > 3)
          requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error: Invalid compile config paramters specified for PMML, Needs  ModelName, ModelVersion, Optional[UpdateModelVersion].").toString)

        // if an optional parm is passed, pass it, else only pass in 2 parms
        if (compileConfigTokens.size == 2) {
          val apiResult = MetadataAPIImpl.UpdateModel(ModelType.PMML, pmmlStr, userid, tenantId, Some(compileConfigTokens(0)), Some(compileConfigTokens(1)))
          requestContext.complete(apiResult)
        } else {
          val apiResult = MetadataAPIImpl.UpdateModel(ModelType.PMML, pmmlStr, userid, tenantId, Some(compileConfigTokens(0)), Some(compileConfigTokens(1)), Some(compileConfigTokens(2)) )
          requestContext.complete(apiResult)
        }
      }
    }
  }
}
