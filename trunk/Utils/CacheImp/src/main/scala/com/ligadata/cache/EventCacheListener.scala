package com.ligadata.cache


import net.sf.ehcache.{Element, Ehcache}
import net.sf.ehcache.event.CacheEventListener

/**
  * Created by Saleh on 3/27/2016.
  */

class EventCacheListener extends CacheEventListener {
  override def notifyElementExpired(cache: Ehcache, element: Element): Unit = {System.out.println("notifyElementExpired")}

  override def notifyElementEvicted(cache: Ehcache, element: Element): Unit = {System.out.println("notifyElementEvicted")}

  override def notifyRemoveAll(cache: Ehcache): Unit = {System.out.println("notifyRemoveAll")}

  override def notifyElementPut(cache: Ehcache, element: Element): Unit = {
    System.out.println("notifyElementPut " + element.getObjectKey.toString + " " + new java.util.Date)
//    Thread.sleep(10000)
    System.out.println("done " + new java.util.Date)

  }

  override def dispose(): Unit = {System.out.println("dispose")}

  override def notifyElementRemoved(cache: Ehcache, element: Element): Unit = {
    System.out.println("notifyElementRemoved"+ element.getObjectKey.toString + " " + new java.util.Date)
  }

  override def notifyElementUpdated(cache: Ehcache, element: Element): Unit = {
    System.out.println("notifyElementUpdated " + element.getObjectKey.toString + " " + new java.util.Date)
//    Thread.sleep(10000)
    System.out.println("done update" + new java.util.Date)
  }
}