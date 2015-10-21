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

package com.ligadata.MetadataAPI.Utility

import com.ligadata.MetadataAPI.Utility.Action.Value



/**
 * Created by dhaval on 8/7/15.
 */
object Action extends Enumeration {
  type Action = Value
  //message manangemen
  val ADDMESSAGE=Value("addmessage")
  val UPDATEMESSAGE=Value("updatemessage")
  val GETALLMESSAGES=Value("getallmessages")
  val REMOVEMESSAGE=Value("removemessage")
  val GETMESSAGE=Value("getmessage")
  //output message management
  val ADDOUTPUTMESSAGE=Value("addoutputmessage")
  val UPDATEOUTPUTMESSAGE=Value("updateoutputmessage")
  val REMOVEOUTPUTMESSAGE=Value("removeoutputmessage")
  val GETALLOUTPUTMESSAGES=Value("getalloutputmessages")
  //model management
  val ADDMODELPMML=Value("addmodelpmml")
  val ADDMODELSCALA=Value("addmodelscala")
  val ADDMODELJAVA=Value("addmodeljava")
  val REMOVEMODEL=Value("removemodel")
  val ACTIVATEMODEL=Value("activatemodel")
  val DEACTIVATEMODEL=Value("deactivatemodel")
  val UPDATEMODELPMML=Value("updatemodelpmml")
  val UPDATEMODELSCALA=Value("updatemodelscala")
  val UPDATEMODELJAVA=Value("updatemodeljava")
  val GETALLMODELS=Value("getallmodels")
  val GETMODEL=Value("getmodel")
  //container management
  val ADDCONTAINER = Value("addcontainer")
  val UPDATECONTAINER = Value("updatecontainer")
  val GETCONTAINER = Value("getcontainer")
  val GETALLCONTAINERS= Value("getallcontainers")
  val REMOVECONTAINER= Value("removecontainer")
  //type management
  val ADDTYPE  = Value("addtype")
  val GETTYPE = Value("gettype")
  val GETALLTYPES = Value("getalltypes")
  val REMOVETYPE = Value("removetype")
  val LOADTYPESFROMAFILE = Value("loadtypesfromafile")
  val DUMPALLTYPESBYOBJTYPEASJSON = Value("dumpalltypes")
  //function
  val ADDFUNCTION = Value("addfunction")
  val GETFUNCTION = Value("getfunction")
  val REMOVEFUNCTION = Value("removefunction")
  val UPDATEFUNCTION= Value("updatefunction")
  val LOADFUNCTIONSFROMAFILE= Value("loadfunctionsfromafile")
  val DUMPALLFUNCTIONSASJSON= Value("dumpallfunctions")
  //config
  val UPLOADCLUSTERCONFIG= Value("uploadclusterconfig")
  val UPLOADCOMPILECONFIG= Value("uploadcompileconfig")
  val DUMPALLCFGOBJECTS= Value("dumpallcfgobjects")
  val REMOVEENGINECONFIG= Value("removeengineconfig")
  //Concept
  val ADDCONCEPT= Value("addconcept")
  val REMOVECONCEPT= Value("removeconcept")
  val UPDATECONCEPT= Value("updateconcept")
  val LOADCONCEPTSFROMAFILE= Value("loadconceptsfromafile ")
  val DUMPALLCONCEPTSASJSON= Value("dumpallconcepts")
  val UPLOADJAR=Value("uploadjar")
  //dump
  val DUMPMETADATA=Value("dumpmetadata")
  val DUMPALLNODES=Value("dumpallnodes")
  val DUMPALLCLUSTERS=Value("dumpallclusters")
  val DUMPALLCLUSTERCFGS=Value("dumpallclustercfgs")
  val DUMPALLADAPTERS=Value("dumpalladapters")
  val GETOUTPUTMESSAGE=Value("getoutputmessage")
}
