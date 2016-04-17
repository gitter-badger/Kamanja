package com.ligadata.cache


import net.sf.ehcache.{Element, Ehcache}
import net.sf.ehcache.event.CacheEventListener

/**
  * Created by Saleh on 3/27/2016.
  */

class EventCacheListener(val listenCallback: CacheCallback) extends CacheEventListener {
  override def notifyElementExpired(cache: Ehcache, element: Element): Unit = {
//    System.out.println("notifyElementExpired")
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("Expire", "", ""));
  }

  override def notifyElementEvicted(cache: Ehcache, element: Element): Unit = {
//    System.out.println("notifyElementEvicted")
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("Evicted", "", ""));
  }

  override def notifyRemoveAll(cache: Ehcache): Unit = {
//    System.out.println("notifyRemoveAll")
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("RemoveAll", "", ""));
  }

  override def notifyElementPut(cache: Ehcache, element: Element): Unit = {
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("Put", element.getObjectKey.toString, element.getObjectValue.toString));
//    System.out.println("notifyElementPut " + element.getObjectKey.toString + " " + new java.util.Date)
////    Thread.sleep(10000)
//    System.out.println("done " + new java.util.Date)
  }

  override def dispose(): Unit = {System.out.println("dispose")}

  override def notifyElementRemoved(cache: Ehcache, element: Element): Unit = {
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("Remove", element.getObjectKey.toString, element.getObjectValue.toString));
//    System.out.println("notifyElementRemoved"+ element.getObjectKey.toString + " " + new java.util.Date)
  }

  override def notifyElementUpdated(cache: Ehcache, element: Element): Unit = {
    if (listenCallback != null)
      listenCallback.call(new CacheCallbackData("Update", element.getObjectKey.toString, element.getObjectValue.toString));
//    System.out.println("notifyElementUpdated " + element.getObjectKey.toString + " " + new java.util.Date)
////    Thread.sleep(10000)
//    System.out.println("done update" + new java.util.Date)
  }
}