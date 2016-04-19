package com.ligadata.pmml.udfs

import org.scalatest._

class FoundInAnyRangeTests extends FlatSpec {

  //String Tests
  "FoundInAnyRange" should "return true if a given string is within a given range inclusive" in {
    assert(Udfs.FoundInAnyRange("c", Array(("a","c"),("d","z")), true))
  }

  it should "return false if a given string is not within a given range not inclusive" in {
    assert(!Udfs.FoundInAnyRange("c", Array(("a","c"),("d","z")), false))
  }

  it should "return false if a given string is not within the given range" in {
    assert(!Udfs.FoundInAnyRange("c", Array(("d","f")), true))
  }

  //Int Tests
  it should "return true if a given integer is within a given range inclusive" in {
    assert(Udfs.FoundInAnyRange(3, Array((1,2),(3,12)), true))
  }

  it should "return false if a given integer is not within a given range not inclusive" in {
    assert(!Udfs.FoundInAnyRange(3, Array((1,2),(3,12)),false))
  }

  it should "return false if a given integer is not within the given range" in {
    assert(!Udfs.FoundInAnyRange(3, Array((1,2),(4,15)),true))
  }

  //Long Tests
  it should "return true if a given Long is within a given range inclusive" in {
    assert(Udfs.FoundInAnyRange(3L, Array((1L,2L),(3L,12L)), true))
  }

  it should "return false if a given Long is not within a given range not inclusive" in {
    assert(!Udfs.FoundInAnyRange(3L, Array((1L,2L),(3L,12L)),false))
  }

  it should "return false if a given Long is not within the given range" in {
    assert(!Udfs.FoundInAnyRange(3L, Array((1L,2L),(4L,15L)),true))
  }

  //Float Tests
  it should "return true if a given Float is within a given range inclusive" in {
    assert(Udfs.FoundInAnyRange(3f, Array((1f,2f),(3f,12f)), true))
  }

  it should "return false if a given Float is not within a given range not inclusive" in {
    assert(!Udfs.FoundInAnyRange(3f, Array((1f,2f),(3f,12f)),false))
  }

  it should "return false if a given Float is not within the given range" in {
    assert(!Udfs.FoundInAnyRange(3f, Array((1f,2f),(4f,15f)),true))
  }

  //Double Tests
  it should "return true if a given Double is within a given range inclusive" in {
    assert(Udfs.FoundInAnyRange(3d, Array((1d,2d),(3d,12d)), true))
  }

  it should "return false if a given Double is not within a given range not inclusive" in {
    assert(!Udfs.FoundInAnyRange(3d, Array((1d,2d),(3d,12d)),false))
  }

  it should "return false if a given Double is not within the given range" in {
    assert(!Udfs.FoundInAnyRange(3d, Array((1d,2d),(4d,15d)),true))
  }
}
