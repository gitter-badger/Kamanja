package com.ligadata.cache

import org.scalatest._

/**
  * Created by Saleh on 3/29/2016.
  */
class PutGetData extends FlatSpec with BeforeAndAfter with Matchers {

  var node:DataCache = null

  before {
    val aclass = Class.forName("com.ligadata.cache.MemoryDataCacheImp").newInstance
    node = aclass.asInstanceOf[DataCache]
    node.init("""{"name":"CacheCluster","diskSpoolBufferSizeMB":"20","replicatePuts":"true","replicateUpdates":"true","replicateUpdatesViaCopy":"false","replicateRemovals":"true","replicateAsynchronously":"true","CacheConfig":{"maxBytesLocalHeap":"20971520","eternal":"false","bootstrapAsynchronously":"false","timeToIdleSeconds":"3000","timeToLiveSeconds":"3000","memoryStoreEvictionPolicy":"LFU","transactionalMode":"off","class":"net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory","separator":"::","peerconfig":"channelName=EH_CACHE::file=jgroups_udp.xml"}}""")
  }

  "put data in cache" should "get data from memory" in {
    node.start
    node.put("1","test")
    assert(node.get("1").toString.equals("test"))
  }

  after {
    node.shutdown
  }

}