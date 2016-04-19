
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

package com.ligadata.SecurityAdapterInfo

import java.util.Properties
import java.util.Date

/**
 * This trait must be implemented by the actual Security Implementation for Kamanja.  All Metadata access methods will 
 * call the preformAuth call prior to executing the code.
 */
trait SecurityAdapter {
  var adapterProperties: Map[String,Any] = null
  def setProperties(props: Map[String,Any]): Unit = {adapterProperties = props}
  
  // Implement these methods
  
  // call off to the appropriate engine to see if this user is allowed to proceed
  def performAuth(secParms: java.util.Properties): Boolean
  
  // get the name of the READ/WRITE privilege name
  def getPrivilegeName(operation: String, objectName: String): String
  
  // Set the desired properties for this adapter
  def init:Unit
}
