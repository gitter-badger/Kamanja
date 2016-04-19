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

package com.ligadata.kamanja.metadata

import com.ligadata.Exceptions.InvalidArgumentException

/**
 * @author danielkozin
 */
object Utils {
  
  /**
   * parseNameToken - Parses the fully qualified Kamanja Object name (with version).
   * @parm - name: String
   * @return - (ObjectNameSpace: String, ObjectName String, ObjectVerion String)
   */
  def parseNameToken(name: String): (String, String, String) = {
    var msgTokens = name.split("\\.")
    if (msgTokens.length < 3) throw InvalidArgumentException("Invalid Metadata Object Name, Unable to parse Namespace.Name.Version", null)
    return (msgTokens.dropRight(2).mkString("."), msgTokens(msgTokens.length-2), msgTokens(msgTokens.length-1))
  } 
  
  /**
   * parseNameToken - Parses the fully qualified Kamanja Object name (with version).
   * @parm - name: String
   * @return - (ObjectNameSpace: String, ObjectName String, ObjectVerion String)
   */
  def parseNameTokenNoVersion(name: String): (String, String) = {
    var msgTokens = name.split("\\.")
    if (msgTokens.length < 2) throw InvalidArgumentException("Invalid Metadata Object Name, Unable to parse Namespace.Name.Version", null)
    return (msgTokens.dropRight(1).mkString("."), msgTokens(msgTokens.length-1))
  }   
  
  def makeFullName (nameSpace: String, name: String): String = {nameSpace+"."+name}
  
}