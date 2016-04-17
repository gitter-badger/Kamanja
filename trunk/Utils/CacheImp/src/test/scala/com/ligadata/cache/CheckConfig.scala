package com.ligadata.cache

import org.scalatest._

/**
  * Created by Saleh on 3/30/2016.
  */
class CheckConfig extends FlatSpec with BeforeAndAfter with Matchers {
  var cacheConfig:CacheCustomConfig = null

  "check json" should "read config json correctly" in {

    cacheConfig = new CacheCustomConfig(new Config("""{"name":"CacheCluster","diskSpoolBufferSizeMB":"20","replicatePuts":"true","replicateUpdates":"true","replicateUpdatesViaCopy":"false","replicateRemovals":"true","replicateAsynchronously":"true","CacheConfig":{"maxBytesLocalHeap":"20971520","eternal":"false","bootstrapAsynchronously":"false","timeToIdleSeconds":"3000","timeToLiveSeconds":"3000","memoryStoreEvictionPolicy":"LFU","transactionalMode":"off","class":"net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory","separator":"::","peerconfig":"channelName=EH_CACHE::file=jgroups_udp.xml"}}"""), null)

    assert(cacheConfig.getName.equals("CacheCluster"))
    assert(cacheConfig.getMaxBytesLocalHeap == 20971520)
    assert(cacheConfig.isEternal==false)
    assert(cacheConfig.getDiskSpoolBufferSizeMB==20)
    assert(cacheConfig.getTimeToIdleSeconds==3000)
    assert(cacheConfig.getTimeToLiveSeconds==3000)
    assert(cacheConfig.getMemoryStoreEvictionPolicy.toString.equals("LFU"))
    assert(cacheConfig.getTransactionalMode.isTransactional==false)

  }

}
