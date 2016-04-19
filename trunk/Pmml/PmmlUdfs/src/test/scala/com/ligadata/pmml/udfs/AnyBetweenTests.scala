package com.ligadata.pmml.udfs

import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class AnyBetweenTests extends FlatSpec {

  /************** ARRAYBUFFER TESTS **************/

  //String Tests
  "AnyBetween" should "return true if any String within a given ArrayBuffer exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(ArrayBuffer("c","g"), "d", "g", true))
  }

  it should "return false if no Strings within a given ArrayBuffer exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(ArrayBuffer("c","g"), "d", "g", false))
  }

  //Int Tests
  it should "return true if any Int within a given ArrayBuffer exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(ArrayBuffer(10, 27), 13, 27, true))
  }

  it should "return false if no Ints within a given ArrayBuffer exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(ArrayBuffer(10, 27), 13, 27, false))
  }

  //Long Tests
  it should "return true if any Long within a given ArrayBuffer exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(ArrayBuffer(10L, 27L), 13L, 27L, true))
  }

  it should "return false if no Longs within a given ArrayBuffer exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(ArrayBuffer(10L, 27L), 13L, 27L, false))
  }

  //Float Tests
  it should "return true if any Float within a given ArrayBuffer exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(ArrayBuffer(10f, 27f), 13f, 27f, true))
  }

  it should "return false if no Floats within a given ArrayBuffer exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(ArrayBuffer(10f, 27f), 13f, 27f, false))
  }

  //Double Tests
  it should "return true if any Double within a given ArrayBuffer exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(ArrayBuffer(10d, 27d), 13d, 27d, true))
  }

  it should "return false if no Doubles within a given ArrayBuffer exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(ArrayBuffer(10d, 27d), 13d, 27d, false))
  }

  /************** ARRAY TESTS **************/

  //String Tests
  "AnyBetween" should "return true if any String within a given Array exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(Array("c","g"), "d", "g", true))
  }

  it should "return false if no Strings within a given Array exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(Array("c","g"), "d", "g", false))
  }

  //Int Tests
  it should "return true if any Int within a given Array exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(Array(10, 27), 13, 27, true))
  }

  it should "return false if no Ints within a given Array exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(Array(10, 27), 13, 27, false))
  }

  /*
  TODO The following tests are for an overload that is not implemented. See github issue ' ' for more info
  //Long Tests
  it should "return true if any Long within a given Array exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(Array(10L, 27L), 13L, 27L, true))
  }

  it should "return false if no Longs within a given Array exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(Array(10L, 27L), 13L, 27L, false))
  }
  */

  //Float Tests
  it should "return true if any Float within a given Array exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(Array(10f, 27f), 13f, 27f, true))
  }

  it should "return false if no Floats within a given Array exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(Array(10f, 27f), 13f, 27f, false))
  }

  //Double Tests
  it should "return true if any Double within a given Array exists within the given range inclusive" in {
    assert(Udfs.AnyBetween(Array(10d, 27d), 13d, 27d, true))
  }

  it should "return false if no Doubles within a given Array exists within the given range not inclusive" in {
    assert(!Udfs.AnyBetween(Array(10d, 27d), 13d, 27d, false))
  }
}
