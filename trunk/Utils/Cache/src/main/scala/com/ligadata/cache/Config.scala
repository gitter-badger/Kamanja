package com.ligadata.cache

/**
  * Created by Saleh on 3/31/2016.
  */
object Config {
  val REPLICATE_PUTS:String = "replicatePuts"
  val REPLICATE_UPDATES:String = "replicateUpdates"
  val REPLICATE_UPDATES_VIA_COPY:String = "replicateUpdatesViaCopy"
  val REPLICATE_REMOVALS:String = "replicateRemovals"
  val REPLICATE_ASYNCHRONOUSLY:String = "replicateAsynchronously"
  val NAME:String="name"
  val DISKSPOOLBUFFERSIZEMB:String="diskSpoolBufferSizeMB"
  val CACHECONFIG:String = "CacheConfig"
  val INITIALHOSTS:String = "jgroups.tcpping.initial_hosts"
  val UDPADD:String = "jgroups.udp.add"
  val PORT:String = "jgroups.port"
}


class Config(jsonString:String){

  private val json = org.json4s.jackson.JsonMethods.parse(jsonString)
  private val values = json.values.asInstanceOf[Map[String, String]]

  def getvalue(key:String): Option[String] ={
    return values.get(key)
  }
}
