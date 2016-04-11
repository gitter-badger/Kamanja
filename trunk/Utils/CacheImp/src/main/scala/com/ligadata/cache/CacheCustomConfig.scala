package com.ligadata.cache

import java.util.Properties

import net.sf.ehcache.Cache
import net.sf.ehcache.bootstrap.BootstrapCacheLoader
import net.sf.ehcache.config.PersistenceConfiguration.Strategy
import net.sf.ehcache.config._
import net.sf.ehcache.distribution.jgroups.{JGroupsBootstrapCacheLoaderFactory, JGroupsCacheReplicatorFactory, JGroupsCacheManagerPeerProviderFactory}
import net.sf.ehcache.event.CacheEventListener


/**
  * Created by Saleh on 3/21/2016.
  */

object CacheCustomConfig{
  val ENABLELISTENER:String = "enableListener"
  val MAXBYTESLOCALHEAP:String="maxBytesLocalHeap"
  val ETERNAL:String="eternal"
  val DISKSPOOLBUFFERSIZEMB:String="diskSpoolBufferSizeMB"
  val TIMETOIDLESECONDS:String="timeToIdleSeconds"
  val TIMETOLIVESECONDS:String="timeToLiveSeconds"
  val MEMORYSTOREEVICTIONPOLICY:String="memoryStoreEvictionPolicy"
  val TRANSACTIONALMODE:String="transactionalMode"
  val CLASS:String="class"
  val SEPARATOR:String="separator"
  val PEERCONFIG:String="peerconfig"
  val BOOTSTRAPASYNCHRONOUSLY:String="bootstrapAsynchronously"
  val PREFERIPV4STACK:String="java.net.preferIPv4Stack"
  val SKIPUPDATECHECK:String="net.sf.ehcache.skipUpdateCheck"
}

class CacheCustomConfig(jsonconfig:Config) extends CacheConfiguration{
  private val config:Configuration = new Configuration
  private val factory:FactoryConfiguration[Nothing] = new FactoryConfiguration
  private val properties:Properties = new Properties
  private val propertiesBootStrap:Properties = new Properties
  private val some = jsonconfig.getvalue(Config.CACHECONFIG)
  private val values = some.get.asInstanceOf[Map[String, String]]


  this.name(jsonconfig.getvalue(Config.NAME).getOrElse("Ligadata").toString)
    .eternal(values.getOrElse(CacheCustomConfig.ETERNAL,"false").toBoolean)
    .persistence(new PersistenceConfiguration().strategy(Strategy.NONE))
    .maxBytesLocalHeap(values.getOrElse(CacheCustomConfig.MAXBYTESLOCALHEAP,"10000").toLong,MemoryUnit.BYTES)
    .diskSpoolBufferSizeMB(values.getOrElse(CacheCustomConfig.DISKSPOOLBUFFERSIZEMB,"20").toInt)
    .timeToLiveSeconds(values.getOrElse(CacheCustomConfig.TIMETOLIVESECONDS,"600").toInt)
    .timeToIdleSeconds(values.getOrElse(CacheCustomConfig.TIMETOIDLESECONDS,"300").toInt)
    .memoryStoreEvictionPolicy(values.getOrElse(CacheCustomConfig.MEMORYSTOREEVICTIONPOLICY,"LFU"))
    .transactionalMode(values.getOrElse(CacheCustomConfig.TRANSACTIONALMODE,"off"))

  //ADD A PROVIDER DEFUALT JGROUPS
  factory.setClass(values.getOrElse(CacheCustomConfig.CLASS,"net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory"))
  factory.setPropertySeparator(values.getOrElse(CacheCustomConfig.SEPARATOR,"::"))
  factory.setProperties(values.getOrElse(CacheCustomConfig.PEERCONFIG,"channelName=EH_CACHE::file=jgroups_udp.xml"));
  config.addCacheManagerPeerProviderFactory(factory)

  //LISTENER PROPERTIES
  properties.setProperty(Config.REPLICATE_PUTS,(jsonconfig.getvalue(Config.REPLICATE_PUTS).getOrElse("false")).toString)
  properties.setProperty(Config.REPLICATE_UPDATES,(jsonconfig.getvalue(Config.REPLICATE_UPDATES).getOrElse("false")).toString)
  properties.setProperty(Config.REPLICATE_UPDATES_VIA_COPY,(jsonconfig.getvalue(Config.REPLICATE_UPDATES_VIA_COPY).getOrElse("false")).toString)
  properties.setProperty(Config.REPLICATE_REMOVALS,(jsonconfig.getvalue(Config.REPLICATE_REMOVALS).getOrElse("false")).toString)
  properties.setProperty(Config.REPLICATE_ASYNCHRONOUSLY,(jsonconfig.getvalue(Config.REPLICATE_ASYNCHRONOUSLY).getOrElse("false")).toString)

  //ADD BOOTSTRAP PROPERTIES
  propertiesBootStrap.setProperty(CacheCustomConfig.BOOTSTRAPASYNCHRONOUSLY,(values.getOrElse(CacheCustomConfig.BOOTSTRAPASYNCHRONOUSLY,"false")).toString)

  private val enableListener = values.getOrElse(CacheCustomConfig.ENABLELISTENER,"false").toBoolean

  System.setProperty(CacheCustomConfig.PREFERIPV4STACK,"true")
  System.setProperty(CacheCustomConfig.SKIPUPDATECHECK, "true")
  System.setProperty(Config.INITIALHOSTS, jsonconfig.getvalue(Config.INITIALHOSTS).getOrElse("localhost[7800]").toString)
  System.setProperty(Config.UDPADD, jsonconfig.getvalue(Config.UDPADD).getOrElse("231.12.21.132").toString)
  System.setProperty(Config.PORT, jsonconfig.getvalue(Config.PORT).getOrElse("45566").toString)

  def  getConfiguration() : Configuration = {
    return config
  }

  def  addListeners(cache:Cache) : Unit = {
    cache.getCacheEventNotificationService.registerListener((new JGroupsCacheReplicatorFactory).createCacheEventListener(properties))
    if(enableListener){
      cache.getCacheEventNotificationService.registerListener(new EventCacheListener)
    }
    cache.getRegisteredCacheLoaders.add(new CacheLoaderFactory(cache))
  }

  def  getBootStrap() : BootstrapCacheLoader = {
    return (new JGroupsBootstrapCacheLoaderFactory).createBootstrapCacheLoader(propertiesBootStrap)
  }
}
