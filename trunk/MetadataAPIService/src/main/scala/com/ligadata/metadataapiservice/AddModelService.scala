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
import com.ligadata.kamanja.metadata._
import scala.util.{ Success, Failure }

import com.ligadata.MetadataAPI._

import scala.util.control._
import org.apache.logging.log4j._
import com.ligadata.AuditAdapterInfo.AuditConstants

object AddModelService {
  case class Process(pmmlStr:String)
}

class AddModelService(requestContext: RequestContext, userid:Option[String], password:Option[String], cert:Option[String], modelCompileInfo: Option[String],tenantId: Option[String]) extends Actor {

  import AddModelService._
  
  implicit val system = context.system
  import system.dispatcher
  val log = Logging(system, getClass)
  val APIName = "AddModelService"
  
  val loggerName = this.getClass.getName
  val logger = LogManager.getLogger(loggerName)
  //logger.setLevel(Level.TRACE);

  def receive = {
    case Process(pmmlStr) =>
      process(pmmlStr)
      context.stop(self)
  }
  
  def process(pmmlStr:String) = {
    logger.debug("Requesting AddModel: " + pmmlStr.substring(0,500))

    var nameVal = APIService.extractNameFromPMML(pmmlStr) 
    
    if (!MetadataAPIImpl.checkAuth(userid,password,cert, MetadataAPIImpl.getPrivilegeName("insert","model"))) {
	    MetadataAPIImpl.logAuditRec(userid,Some(AuditConstants.WRITE),AuditConstants.INSERTOBJECT,pmmlStr,AuditConstants.FAIL,"",nameVal)    
	    requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null,  "Error:UPDATE not allowed for this user").toString )
    } else {
      // Ok, if this is a KPMML model, we dont need any additional info for compilation, its all enclosed in the model.  for normal PMML,
      // we need to know ModelName, Version, and associated Message.  modelCompileInfo will be set if this is PMML, and not set if KPMML
      if (modelCompileInfo == None) {
        logger.info ("No configuration information provided, assuming Kamanja PMML implementation.")
        val apiResult = MetadataAPIImpl.AddModel(ModelType.KPMML, pmmlStr, userid, tenantId)
        requestContext.complete(apiResult)
      } else {
        val cInfo = modelCompileInfo.getOrElse("")

        // Error if nothing specified in the modelCompileInfo
        if (cInfo.equalsIgnoreCase(""))
          requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error: modelconfig is not specified, PMML model is required to have Model Compilation Information.").toString)

        val compileConfigTokens = cInfo.split(",")
        if (compileConfigTokens.size < 3  ||
            compileConfigTokens.size > 4)
          requestContext.complete(new ApiResult(ErrorCodeConstants.Failure, APIName, null, "Error: Invalid compile config paramters specified for PMML, Needs at least ModelName, ModelVersion, MessageConsumed.").toString)

        if (compileConfigTokens.size == 3) {
          val apiResult = MetadataAPIImpl.AddModel(ModelType.PMML, pmmlStr, userid, tenantId, Some(compileConfigTokens(0)), Some(compileConfigTokens(1)), Some(compileConfigTokens(2)))
          requestContext.complete(apiResult)
        } else {
          val apiResult = MetadataAPIImpl.AddModel(ModelType.PMML, pmmlStr, userid, tenantId, Some(compileConfigTokens(0)), Some(compileConfigTokens(1)), Some(compileConfigTokens(2)), Some(compileConfigTokens(3)))
          requestContext.complete(apiResult)
        }

      }
    }
  }
}
