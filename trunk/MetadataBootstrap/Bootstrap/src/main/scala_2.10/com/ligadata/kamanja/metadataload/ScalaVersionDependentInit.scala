/*
 * Copyright 2016 ligaDATA
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

package com.ligadata.kamanja.metadataload

import com.ligadata.kamanja.metadata.MdMgr._
import com.ligadata.kamanja.metadata.ObjType._
import com.ligadata.kamanja.metadata._
import com.ligadata.KamanjaBase._
import com.ligadata.BaseTypes._

object ScalaVersionDependentInit {
  /*
  def initFactoryOfModelInstanceFactories(mgr : MdMgr): Unit = {
    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.FactoryOfModelInstanceFactory", "JarFactoryOfModelInstanceFactory", ModelRepresentation.JAR, "com.ligadata.FactoryOfModelInstanceFactory.JarFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesVer, "jarfactoryofmodelinstancefactory_2.10-1.0.jar", Array("ExtDependencyLibs_2.10-1.0","KamanjaInternalDeps_2.10-1.0"))
    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.jpmml", "JpmmlFactoryOfModelInstanceFactory", ModelRepresentation.PMML, "com.ligadata.jpmml.JpmmlFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesVer, "jpmmlfactoryofmodelinstancefactory_2.10-1.0.jar", Array("ExtDependencyLibs_2.10-1.0","KamanjaInternalDeps_2.10-1.0"))
  }

  def InitTypeDefs(mgr : MdMgr): Unit = {
    mgr.AddScalar(MdMgr.sysNS, "Any", tAny, "Any", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.AnyImpl")
    mgr.AddScalar(MdMgr.sysNS, "String", tString, "String", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.StringImpl")
    mgr.AddScalar(MdMgr.sysNS, "Int", tInt, "Int", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.IntImpl")
    mgr.AddScalar(MdMgr.sysNS, "Integer", tInt, "Int", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.IntImpl")
    mgr.AddScalar(MdMgr.sysNS, "Long", tLong, "Long", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "Boolean", tBoolean, "Boolean", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.BoolImpl")
    mgr.AddScalar(MdMgr.sysNS, "Bool", tBoolean, "Boolean", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.BoolImpl")
    mgr.AddScalar(MdMgr.sysNS, "Double", tDouble, "Double", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.DoubleImpl")
    mgr.AddScalar(MdMgr.sysNS, "Float", tFloat, "Float", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.FloatImpl")
    mgr.AddScalar(MdMgr.sysNS, "Char", tChar, "Char", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.CharImpl")

    mgr.AddScalar(MdMgr.sysNS, "date", tLong, "Long", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "dateTime", tLong, "Long", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "time", tLong, "Long", MetadataLoad.baseTypesVer, "basetypes_2.10-0.1.0.jar", Array("metadata_2.10-1.0.jar"), "com.ligadata.BaseTypes.LongImpl")
  }
*/

  def initFactoryOfModelInstanceFactories(mgr: MdMgr): Unit = {
    //    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.FactoryOfModelInstanceFactory", "JarFactoryOfModelInstanceFactory", ModelRepresentation.JAR, "com.ligadata.FactoryOfModelInstanceFactory.JarFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesVer, null, Array[String]())
    //    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.jpmml", "JpmmlFactoryOfModelInstanceFactory", ModelRepresentation.PMML, "com.ligadata.jpmml.JpmmlFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesVer, null, Array[String]())
    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.FactoryOfModelInstanceFactory", "JarFactoryOfModelInstanceFactory", ModelRepresentation.JAR, "com.ligadata.FactoryOfModelInstanceFactory.JarFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String]())
    mgr.AddFactoryOfModelInstanceFactory("com.ligadata.jpmml", "JpmmlFactoryOfModelInstanceFactory", ModelRepresentation.PMML, "com.ligadata.jpmml.JpmmlFactoryOfModelInstanceFactory$", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String]())

  }

  def InitTypeDefs(mgr: MdMgr): Unit = {
    //    mgr.AddScalar(MdMgr.sysNS, "Any", tAny, "Any", MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.AnyImpl")
    //    mgr.AddScalar(MdMgr.sysNS, "String", tString, "String", MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.StringImpl")
    mgr.AddScalar(MdMgr.sysNS, "Any", tAny, "Any", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.AnyImpl")
    mgr.AddScalar(MdMgr.sysNS, "String", tString, "String", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.StringImpl")
    mgr.AddScalar(MdMgr.sysNS, "Int", tInt, "Int", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.IntImpl")
    mgr.AddScalar(MdMgr.sysNS, "Integer", tInt, "Int", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.IntImpl")
    mgr.AddScalar(MdMgr.sysNS, "Long", tLong, "Long", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "Boolean", tBoolean, "Boolean", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.BoolImpl")
    mgr.AddScalar(MdMgr.sysNS, "Bool", tBoolean, "Boolean", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.BoolImpl")
    mgr.AddScalar(MdMgr.sysNS, "Double", tDouble, "Double", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.DoubleImpl")
    mgr.AddScalar(MdMgr.sysNS, "Float", tFloat, "Float", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.FloatImpl")
    mgr.AddScalar(MdMgr.sysNS, "Char", tChar, "Char", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.CharImpl")

    mgr.AddScalar(MdMgr.sysNS, "date", tLong, "Long", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "dateTime", tLong, "Long", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.LongImpl")
    mgr.AddScalar(MdMgr.sysNS, "time", tLong, "Long", MetadataLoad.baseTypesOwnerId, MetadataLoad.baseTypesTenantId, MetadataLoad.baseTypesUniqId, MetadataLoad.baseTypesElementId, MetadataLoad.baseTypesVer, null, Array[String](), "com.ligadata.BaseTypes.LongImpl")
  }

}

