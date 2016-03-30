package com.ligadata.BasicCacheConcurrency

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
  val REPLICATE_PUTS:String = "replicatePuts"
  val REPLICATE_UPDATES:String = "replicateUpdates"
  val REPLICATE_UPDATES_VIA_COPY:String = "replicateUpdatesViaCopy"
  val REPLICATE_REMOVALS:String = "replicateRemovals"
  val REPLICATE_ASYNCHRONOUSLY:String = "replicateAsynchronously"
  val NAME:String="name"
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
  val INITIALHOSTS:String = "jgroups.tcpping.initial_hosts"
  val UDPADD:String = "jgroups.udp.add"
  val PORT:String = "jgroups.port"
}

class CacheCustomConfig(jsonString:String) extends CacheConfiguration{
  private val config:Configuration = new Configuration
  private val factory:FactoryConfiguration[Nothing] = new FactoryConfiguration
  private val properties:Properties = new Properties
  private val propertiesBootStrap:Properties = new Properties
  private val json = org.json4s.jackson.JsonMethods.parse(jsonString)
  private val values = json.values.asInstanceOf[Map[String, String]]


  /*
             enableListener="false"
             name="Node"
             maxBytesLocalHeap="10000"
             eternal="false"
             diskSpoolBufferSizeMB="20"
             timeToIdleSeconds="300"
             timeToLiveSeconds="600"
             memoryStoreEvictionPolicy="LFU"
             transactionalMode="off"
             class="net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory"
             separator="::"
             peerconfig="channelName=EH_CACHE::file=jgroups_udp.xml"
             replicatePuts=true
             replicateUpdates=true
             replicateUpdatesViaCopy=false
             replicateRemovals=true
             replicateAsynchronously=true
             bootstrapAsynchronously=false
             jgroups.tcpping.initial_hosts=localhost[7800]
             jgroups.port=45566

   */

  this.name(values.getOrElse(CacheCustomConfig.NAME,"Node"))
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
  properties.setProperty(CacheCustomConfig.REPLICATE_PUTS,(values.getOrElse(CacheCustomConfig.REPLICATE_PUTS,"true")).toString)
  properties.setProperty(CacheCustomConfig.REPLICATE_UPDATES,(values.getOrElse(CacheCustomConfig.REPLICATE_UPDATES,"true")).toString)
  properties.setProperty(CacheCustomConfig.REPLICATE_UPDATES_VIA_COPY,(values.getOrElse(CacheCustomConfig.REPLICATE_UPDATES_VIA_COPY,"false")).toString)
  properties.setProperty(CacheCustomConfig.REPLICATE_REMOVALS,(values.getOrElse(CacheCustomConfig.REPLICATE_REMOVALS,"true")).toString)
  properties.setProperty(CacheCustomConfig.REPLICATE_ASYNCHRONOUSLY,(values.getOrElse(CacheCustomConfig.REPLICATE_ASYNCHRONOUSLY,"true")).toString)

  //ADD BOOTSTRAP PROPERTIES
  propertiesBootStrap.setProperty(CacheCustomConfig.BOOTSTRAPASYNCHRONOUSLY,(values.getOrElse(CacheCustomConfig.BOOTSTRAPASYNCHRONOUSLY,"false")).toString)

  private val enableListener = values.getOrElse(CacheCustomConfig.ENABLELISTENER,"false").toBoolean

  System.setProperty(CacheCustomConfig.PREFERIPV4STACK,"true")
  System.setProperty(CacheCustomConfig.SKIPUPDATECHECK, "true")
  System.setProperty(CacheCustomConfig.INITIALHOSTS, values.getOrElse(CacheCustomConfig.INITIALHOSTS,"localhost[7800]"))
  System.setProperty(CacheCustomConfig.UDPADD, values.getOrElse(CacheCustomConfig.UDPADD,"231.12.21.132"))
  System.setProperty(CacheCustomConfig.PORT, values.getOrElse(CacheCustomConfig.PORT,"45566"))

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