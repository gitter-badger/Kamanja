package com.ligadata.pmml.udfs

import scala.util.Random

import org.scalatest._

class BooleanUdfTests extends FlatSpec {

  "And" should "take a variable number of booleans and return true if all arguments are true" in {
    //Generating a sequence of 0 to 99 boolean equalling true
    val n = Random.nextInt(100)
    val boolSeq = Seq.fill(n)(true)
    assert(Udfs.And(boolSeq: _*))
  }

  it should "take a variable number of booleans and return false if at least one argument is false" in {
    val n = Random.nextInt(100)
    var boolSeq = Seq.fill(n)(Random.nextBoolean())
    // Adding a false to ensure a false exists
    boolSeq = boolSeq :+ false
    assert(!Udfs.And(boolSeq: _*))
  }

  "IntAnd" should "take a variable number of integers and return false if all arguments are 0" in {
    val n = Random.nextInt(100) + 1
    val intSeq = Seq.fill(n)(0)
    assert(!Udfs.IntAnd(intSeq: _*))
  }

  it should "take a variable number of integers and return true if all arguments are greater than 0" in {
    val intSeq = Seq.fill(Random.nextInt(100))(Random.nextInt(100) + 1)
    assert(Udfs.IntAnd(intSeq: _*))
  }

  "Or" should "take a variable number of booleans and return true if at least one argument is true" in {
    // Generate sequence of random size between 0 and 99 with random booleans at each index
    var boolSeq = Seq.fill(Random.nextInt(100))(Random.nextBoolean())
    boolSeq :+= true
    assert(Udfs.Or(boolSeq:_*))
  }

  it should "take a variable number of booleans and return false if no arguments are true" in {
    val boolSeq = Seq.fill(Random.nextInt(100))(false)
    assert(!Udfs.Or(boolSeq:_*))
  }

  "IntOr" should "take a variable number of integers and return true if at least one argument is greater than 0" in {
    val intSeq = Seq.fill(Random.nextInt(100))(Random.nextInt(100) + 1)
    assert(Udfs.IntOr(intSeq:_*))
  }

  it should "take a variable number of integers and return false if all arguments equal 0" in {
    val intSeq = Seq.fill(Random.nextInt(100))(0)
    assert(!Udfs.IntOr(intSeq:_*))
  }
}
