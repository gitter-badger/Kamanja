import com.ligadata.cache.DataCache

/**
  * Created by Saleh on 3/23/2016.
  */
object Node2 {
  def main(args: Array[String]) {

    val aclass = Class.forName("com.ligadata.cache.MemoryDataCacheImp").newInstance
    val node = aclass.asInstanceOf[DataCache]

    node.init("""{"name":"CacheCluster","diskSpoolBufferSizeMB":"20","jgroups.tcpping.initial_hosts":"192.168.1.137[7800],192.168.1.137[7800]","jgroups.port":"7800","replicatePuts":"true","replicateUpdates":"true","replicateUpdatesViaCopy":"false","replicateRemovals":"true","replicateAsynchronously":"true","CacheConfig":{"maxBytesLocalHeap":"20971520","eternal":"false","bootstrapAsynchronously":"false","timeToIdleSeconds":"3000","timeToLiveSeconds":"3000","memoryStoreEvictionPolicy":"LFU","transactionalMode":"off","class":"net.sf.ehcache.distribution.jgroups.JGroupsCacheManagerPeerProviderFactory","separator":"::","peerconfig":"channelName=EH_CACHE::file=jgroups_tcp.xml","enableListener":"true"}}""", null)
    node.start()
//    println(node.get("1").toString)
    val test = node.get("1").asInstanceOf[Array[Byte]]
    test.foreach(k=>System.out.println(k.toChar))

//    System.out.println(node.get("70").toString)

//    var i = 0
//    while(true){
//      i += 1
//      node.put("" + i, "" + i+i)
//      System.out.println("push data " + i)
//      Thread.sleep(4000)
//    }

  }
}
