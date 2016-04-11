package com.ligadata.cache

import java.util

import net.sf.ehcache.{Status, Ehcache}
import net.sf.ehcache.loader.CacheLoader

/**
  * Created by Saleh on 3/27/2016.
  */
class CacheLoaderFactory(cache: Ehcache) extends CacheLoader {
  override def init(): Unit = ???

  override def getName: String = ???

  override def loadAll(keys: util.Collection[_]): util.Map[_, _] = ???

  override def loadAll(keys: util.Collection[_], argument: scala.Any): util.Map[_, _] = ???

  override def getStatus: Status = ???

  override def dispose(): Unit = {
    System.out.println("///////////////////////////dispose")
  }

  override def clone(cache: Ehcache): CacheLoader = {
    new CacheLoaderFactory(cache)
  }

  override def load(key: scala.Any): AnyRef = {
    System.out.println("///////////////////////////loadkey")
    null
  }

  override def load(key: scala.Any, argument: scala.Any): AnyRef = ???
}
