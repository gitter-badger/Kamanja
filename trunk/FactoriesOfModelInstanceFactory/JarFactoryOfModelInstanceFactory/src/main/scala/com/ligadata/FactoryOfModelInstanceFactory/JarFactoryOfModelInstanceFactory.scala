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

package com.ligadata.FactoryOfModelInstanceFactory

import com.ligadata.kamanja.metadata.{ ModelDef, BaseElem }
import com.ligadata.KamanjaBase._
import com.ligadata.Utils.{ Utils, KamanjaClassLoader, KamanjaLoaderInfo }
import org.apache.logging.log4j.{ Logger, LogManager }

object JarFactoryOfModelInstanceFactory extends FactoryOfModelInstanceFactory {
  private[this] val loggerName = this.getClass.getName()
  private[this] val LOG = LogManager.getLogger(loggerName)

  private[this] def LoadJarIfNeeded(metadataLoader: KamanjaLoaderInfo, jarPaths: collection.immutable.Set[String], elem: BaseElem): Boolean = {
    val allJars = GetAllJarsFromElem(jarPaths, elem)
    if (allJars.size > 0) {
      return Utils.LoadJars(allJars.toArray, metadataLoader.loadedJars, metadataLoader.loader)
    } else {
      return true
    }
  }

  private[this] def GetAllJarsFromElem(jarPaths: collection.immutable.Set[String], elem: BaseElem): Set[String] = {
    var allJars: Array[String] = null

    val jarname = if (elem.JarName == null) "" else elem.JarName.trim

    if (elem.DependencyJarNames != null && elem.DependencyJarNames.size > 0 && jarname.size > 0) {
      allJars = elem.DependencyJarNames :+ jarname
    } else if (elem.DependencyJarNames != null && elem.DependencyJarNames.size > 0) {
      allJars = elem.DependencyJarNames
    } else if (jarname.size > 0) {
      allJars = Array(jarname)
    } else {
      return Set[String]()
    }

    return allJars.map(j => Utils.GetValidJarFile(jarPaths, j)).toSet
  }

  private[this] def CheckAndPrepModelFactory(nodeContext: NodeContext, metadataLoader: KamanjaLoaderInfo, clsName: String, mdl: ModelDef): ModelInstanceFactory = {
    var isModelBaseObj = false
    var isModel = true
    var curClass: Class[_] = null

    try {
      // Convert class name into a class
      var curClz = Class.forName(clsName, true, metadataLoader.loader)
      curClass = curClz

      isModel = false

      while (curClz != null && isModel == false) {
        isModel = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.ModelInstanceFactory")
        if (isModel == false) {
          isModel = Utils.isDerivedFrom(curClz, "com.ligadata.KamanjaBase.ModelBaseObj")
          if (isModel == false)
            curClz = curClz.getSuperclass()
          else {
            isModelBaseObj = true
            LOG.debug("Found ModelBaseObj class:" + clsName)
          }
        }
      }
    } catch {
      case e: Exception => {
        LOG.error("Failed to get classname %s".format(clsName), e)
        return null
      }
    }

    if (isModel) {
      try {
        var objinst: Any = null
        try {
          if (isModelBaseObj) {
            var mdlBaseObjInst: Any = null
            try {
              // Trying Singleton Object
              LOG.debug("Creating mdlBaseObjInst")
              val module = metadataLoader.mirror.staticModule(clsName)
              val obj = metadataLoader.mirror.reflectModule(module)
              mdlBaseObjInst = obj.instance
              LOG.debug("Created mdlBaseObjInst:" + mdlBaseObjInst)
            } catch {
              case e: Exception => {
                LOG.debug("Creating mdlBaseObjInst", e)
                // Trying Regular Object instantiation
                mdlBaseObjInst = curClass.newInstance
                LOG.debug("Created mdlBaseObjInst:" + mdlBaseObjInst)
              }
            }

            if (mdlBaseObjInst.isInstanceOf[ModelBaseObj]) {
              LOG.debug("Getting modelBaseobj")
              val modelBaseobj = mdlBaseObjInst.asInstanceOf[ModelBaseObj]
              LOG.debug("Got modelBaseobj:" + modelBaseobj)
              objinst = new ModelBaseObjMdlInstanceFactory(mdl, nodeContext, modelBaseobj)
              LOG.debug("Got objinst:" + objinst)
            }
          } else {
            LOG.debug("Getting objinst")
            // Trying Regular class instantiation
            objinst = curClass.getConstructor(classOf[ModelDef], classOf[NodeContext]).newInstance(mdl, nodeContext)
            LOG.debug("Got objinst:" + objinst)
          }
        } catch {
          case e: Exception => {
            LOG.error("Failed to instantiate ModelInstanceFactory. clsName:" + clsName, e)
            return null
          }
        }

        if (objinst != null && objinst.isInstanceOf[ModelInstanceFactory]) {
          val modelobj = objinst.asInstanceOf[ModelInstanceFactory]
          val mdlName = (mdl.NameSpace.trim + "." + mdl.Name.trim).toLowerCase
          LOG.info("Created Model:" + mdlName)
          return modelobj
        }
        LOG.error("Failed to instantiate ModelInstanceFactory :" + clsName + ". ObjType0:" + objinst.getClass.getSimpleName + ". ObjType1:" + (if (objinst != null) objinst.getClass.getCanonicalName else ""))
        return null
      } catch {
        case e: Exception =>
          LOG.error("Failed to instantiate ModelInstanceFactory for classname:%s.".format(clsName), e)
          return null
      }
    }
    return null
  }

  override def getModelInstanceFactory(modelDef: ModelDef, nodeContext: NodeContext, loaderInfo: KamanjaLoaderInfo, jarPaths: collection.immutable.Set[String]): ModelInstanceFactory = {
    LoadJarIfNeeded(loaderInfo, jarPaths, modelDef)

    var clsName = modelDef.PhysicalName.trim
    var orgClsName = clsName

    var mdlInstanceFactory = CheckAndPrepModelFactory(nodeContext, loaderInfo, clsName, modelDef)

    if (mdlInstanceFactory == null) {
      LOG.error("Failed to instantiate ModelInstanceFactory :" + orgClsName)
    }

    mdlInstanceFactory
  }

  // Input: Model String, input & output Message Names. Output: ModelDef
  override def prepareModel(nodeContext: NodeContext, modelString: String, inputMessage: String, outputMessage: String, loaderInfo: KamanjaLoaderInfo, jarPaths: collection.immutable.Set[String]): ModelDef = {
    null
  }
}

