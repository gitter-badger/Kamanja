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

package com.ligadata.AuditAdapterInfo

import java.util.Properties
import java.util.Date
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

class AuditRecord {
  var actionTime: String = _
  var action: String = _
  var notes: String = _
  var objectAccessed: String = _
  var success: String = _
  var transactionId: String = _
  var userOrRole: String = _
  var userPrivilege: String = _

  override def toString: String =
    "(" + actionTime + "," + action + "," + "," + objectAccessed + "," + success + "," + transactionId + "," + userOrRole + "," + userPrivilege + ")"

  def toJson: JObject = {
    val ft = new java.text.SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
    val at = new java.util.Date(java.lang.Long.valueOf(actionTime))
    val jsonObj = ("ActionTime" -> ft.format(at)) ~
      ("Action" -> action) ~
      ("UserOrRole" -> userOrRole) ~
      ("Status" -> success) ~
      ("ObjectAccessed" -> objectAccessed) ~
      ("ActionResult" -> notes)
    jsonObj
  }

}

/**
 * This trait must be implemented by the actual Audit Implementation for Kamanja.  All Metadata access methods will
 * call the ADD method when
 */
trait AuditAdapter {
  def Shutdown() = {}

  // Implement these methods

  // Add an Audit Record to an appropriate system
  def addAuditRecord(rec: AuditRecord)

  // Get an audit record from an appropriate system.
  def getAuditRecord(startTime: Date, endTime: Date, userOrRole: String, action: String, objectAccessed: String): Array[AuditRecord]

  // Set the desired properties for this adapter
  def init(parmFile: String): Unit

  // truncate audit store
  def TruncateStore(): Unit
}

object AuditConstants {
  // Audit Actions
  val GETOBJECT = "getObject"
  val GETKEYS = "getKeys"
  val UPDATEOBJECT = "updateObject"
  val INSERTOBJECT = "insertObject"
  val DELETEOBJECT = "deleteObject"
  val ACTIVATEOBJECT = "activateObject"
  val DEACTIVATEOBJECT = "deactivateObject"
  val REMOVECONFIG = "removeConfig"
  val INSERTCONFIG = "insertConfig"
  val UPDATECONFIG = "updateConfig"
  val GETCONFIG = "getConfig"
  val INSERTJAR = "uploadJar"

  // Objects
  val MESSAGE = "Message"
  val OUTPUTMSG = "OutputMsg"
  val MODEL = "Model"
  val CONTAINER = "Container"
  val FUNCTION = "Function"
  val CONCEPT = "Concept"
  val TYPE = "Type"
  val OBJECT = "Object"
  val CLUSTERID = "ClusterId"
  val CONFIG = "ClusterConfiguration"
  val JAR = "JarFile"

  // Priviliges
  val READ = "read"
  val WRITE = "write"

  // Results
  val FAIL = "Access Denied"
  val SUCCESS = "Access Granted"
}

