package com.ligadata.cache


import net.sf.ehcache.bootstrap.{BootstrapCacheLoader, BootstrapCacheLoaderFactory}
import net.sf.ehcache.distribution.jgroups.{JGroupsBootstrapCacheLoaderFactory, JGroupsCacheReplicatorFactory}
import net.sf.ehcache.event.CacheEventListener
import net.sf.ehcache.{Element, Cache, CacheManager}


import scala.collection.JavaConverters._

/**
  * Created by Saleh on 3/15/2016.
  */

class MemoryDataCacheImp extends DataCache{

  var cm:CacheManager = null
  var cache:Cache = null
  var cacheConfig:CacheCustomConfig = null

  override def init(jsonString:String): Unit = {

    cacheConfig = new CacheCustomConfig(new Config(jsonString))
    cm = CacheManager.create(cacheConfig.getConfiguration())
    val cache = new Cache(cacheConfig)
    cache.setBootstrapCacheLoader(cacheConfig.getBootStrap())
    cm.addCache(cache)

  }

  override def start(): Unit = {
    cache = cm.getCache(cacheConfig.getName)
    cacheConfig.addListeners(cache)
  }

  override def shutdown(): Unit = {
    cm.shutdown()
  }

  override def put(key: String, value: scala.Any): Unit = {
    cache.put(new Element(key,value))
  }

  override def get(key: String): AnyRef = {
    if(cache.isKeyInCache(key)){

      val ele:Element = cache.get(key)
      //val obj:Object = ele.getValue

      return ele.getObjectValue
    }else{
      System.out.println("get data from SSD");
      cache.load(key)

      return ""
    }

  }

  override def get(keys: Array[String]): java.util.Map[String, AnyRef] = {
    val map = new java.util.HashMap[String, AnyRef]
    keys.foreach(str => map.put(str,get(str)))

    map
  }

  override def put(map: java.util.Map[_, _]): Unit = {
    val scalaMap = map.asScala
    scalaMap.foreach {keyVal => cache.put(new Element(keyVal._1,keyVal._2))}
  }
}
