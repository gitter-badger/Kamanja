package com.ligadata.pmml.udfs

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.scalatest._

class IsInUdfTests extends FlatSpec {

  //ArrayBuffer tests
  "IsIn" should "return true if a String exists in an ArrayBuffer of Strings" in {
    val searchTerm = "Positive"
    val strArrBuffer = ArrayBuffer.fill(Random.nextInt(100) + 1)(Random.nextString(Random.nextInt(50) + 1)) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, strArrBuffer))
  }

  it should "return false if a String doesn't exist in an ArrayBuffer of Strings" in {
    val searchTerm = "Negative"
    val strArrBuffer = (Seq.fill(Random.nextInt(100))(Random.nextString(Random.nextInt(50) + 1))).to[ArrayBuffer]
    assert(!Udfs.IsIn(searchTerm, strArrBuffer))
  }

  it should "return true if an integer exists in an ArrayBuffer of integers" in {
    val searchTerm = Random.nextInt()
    val arrayBuffer = (Seq.fill(Random.nextInt(100) + 1)(Random.nextInt()) :+ searchTerm).to[ArrayBuffer]
    assert(Udfs.IsIn(searchTerm, arrayBuffer))
  }

  it should "return false if an integer does not exist in an ArrayBuffer of integers" in {
    val searchTerm = 100
    val arrayBuffer = ArrayBuffer.fill(Random.nextInt(100) + 1)(Random.nextInt(10) + 1)
    assert(!Udfs.IsIn(searchTerm, arrayBuffer))
  }

  it should "return true if a float exists in an ArrayBuffer of floats" in {
    val searchTerm = Random.nextFloat()
    val arrayBuffer = (Seq.fill(Random.nextInt(100) + 1)(Random.nextFloat()) :+ searchTerm).to[ArrayBuffer]
    assert(Udfs.IsIn(searchTerm, arrayBuffer))
  }

  it should "return false if a float does not exist in an ArrayBuffer of floats" in {
    val searchTerm: Float = 1e30f
    val arrayBuffer = ArrayBuffer.fill(Random.nextInt(100) + 1)(Random.nextFloat() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, arrayBuffer))
  }

  it should "return true if a double exists in an ArrayBuffer of double" in {
    val searchTerm = Random.nextDouble()
    val arrayBuffer = (Seq.fill(Random.nextInt(100) + 1)(Random.nextDouble()) :+ searchTerm).to[ArrayBuffer]
    assert(Udfs.IsIn(searchTerm, arrayBuffer))
  }

  it should "return false if a double does not exist in an ArrayBuffer of double" in {
    val searchTerm: Double = 1e10d
    val arrayBuffer = ArrayBuffer.fill(Random.nextInt(100) + 1)(Random.nextDouble() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, arrayBuffer))
  }

  //Array Tests
  it should "return true if a String exists in an Array of Strings" in {
    val searchTerm = "Positive"
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextString(Random.nextInt(50) + 1)) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, array))
  }

  it should "return false if a String doesn't exist in an Array of Strings" in {
    val searchTerm = "Negative"
    val array = Array.fill(Random.nextInt(100))(Random.nextString(Random.nextInt(50) + 1))
    assert(!Udfs.IsIn(searchTerm, array))
  }

  it should "return true if an integer exists in an Array of integers" in {
    val searchTerm = Random.nextInt()
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextInt()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, array))
  }

  it should "return false if an integer does not exist in an Array of integers" in {
    val searchTerm = 100
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextInt(10) + 1)
    assert(!Udfs.IsIn(searchTerm, array))
  }

  it should "return true if a float exists in an Array of floats" in {
    val searchTerm = Random.nextFloat()
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextFloat()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, array))
  }

  it should "return false if a float does not exist in an Array of floats" in {
    val searchTerm: Float = 1e30f
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextFloat() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, array))
  }

  it should "return true if a double exists in an Array of double" in {
    val searchTerm = Random.nextDouble()
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextDouble()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, array))
  }

  it should "return false if a double does not exist in an Array of double" in {
    val searchTerm: Double = 1e10d
    val array = Array.fill(Random.nextInt(100) + 1)(Random.nextDouble() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, array))
  }

  //List Tests
  it should "return true if a String exists in an List of Strings" in {
    val searchTerm = "Positive"
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextString(Random.nextInt(50) + 1)) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, list))
  }

  it should "return false if a String doesn't exist in an List of Strings" in {
    val searchTerm = "Negative"
    val list = List.fill(Random.nextInt(100))(Random.nextString(Random.nextInt(50) + 1))
    assert(!Udfs.IsIn(searchTerm, list))
  }

  it should "return true if an integer exists in an List of integers" in {
    val searchTerm = Random.nextInt()
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextInt()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, list))
  }

  it should "return false if an integer does not exist in an List of integers" in {
    val searchTerm = 100
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextInt(10) + 1)
    assert(!Udfs.IsIn(searchTerm, list))
  }

  it should "return true if a float exists in an List of floats" in {
    val searchTerm = Random.nextFloat()
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextFloat()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, list))
  }

  it should "return false if a float does not exist in an List of floats" in {
    val searchTerm: Float = 1e30f
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextFloat() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, list))
  }

  it should "return true if a double exists in an List of double" in {
    val searchTerm = Random.nextDouble()
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextDouble()) :+ searchTerm
    assert(Udfs.IsIn(searchTerm, list))
  }

  it should "return false if a double does not exist in an List of double" in {
    val searchTerm: Double = 1e10d
    val list = List.fill(Random.nextInt(100) + 1)(Random.nextDouble() * (1000 - 1) + 1)
    assert(!Udfs.IsIn(searchTerm, list))
  }

  //Set Tests
  it should "return true if a String exists in an Set of Strings" in {
    val searchTerm = "Positive"
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextString(Random.nextInt(50) + 1)) :+ searchTerm).to[Set]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a String doesn't exist in an Set of Strings" in {
    val searchTerm = "Negative"
    val set = (Seq.fill(Random.nextInt(100))(Random.nextString(Random.nextInt(50) + 1))).to[Set]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if an integer exists in an Set of integers" in {
    val searchTerm = Random.nextInt()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextInt()) :+ searchTerm).to[Set]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if an integer does not exist in an Set of integers" in {
    val searchTerm = 100
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextInt(10) + 1)).to[Set]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if a float exists in an Set of floats" in {
    val searchTerm = Random.nextFloat()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextFloat()) :+ searchTerm).to[Set]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a float does not exist in an Set of floats" in {
    val searchTerm: Float = 1e30f
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextFloat() * (1000 - 1) + 1)).to[Set]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if a double exists in an Set of double" in {
    val searchTerm = Random.nextDouble()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextDouble()) :+ searchTerm).to[Set]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a double does not exist in an Set of double" in {
    val searchTerm: Double = 1e10d
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextDouble() * (1000 - 1) + 1)).to[Set]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  //MutableSet Tests
  import scala.collection.mutable.{ Set => MutableSet }
  it should "return true if a String exists in a mutable Set of Strings" in {
    val searchTerm = "Positive"
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextString(Random.nextInt(50) + 1)) :+ searchTerm).to[MutableSet]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a String doesn't exist in a mutable Set of Strings" in {
    val searchTerm = "Negative"
    val set = (Seq.fill(Random.nextInt(100))(Random.nextString(Random.nextInt(50) + 1))).to[MutableSet]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if an integer exists in a mutable Set of integers" in {
    val searchTerm = Random.nextInt()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextInt()) :+ searchTerm).to[MutableSet]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if an integer does not exist in a mutable Set of integers" in {
    val searchTerm = 100
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextInt(10) + 1)).to[MutableSet]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if a float exists in a mutable Set of floats" in {
    val searchTerm = Random.nextFloat()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextFloat()) :+ searchTerm).to[MutableSet]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a float does not exist in a mutable Set of floats" in {
    val searchTerm: Float = 1e30f
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextFloat() * (1000 - 1) + 1)).to[MutableSet]
    assert(!Udfs.IsIn(searchTerm, set))
  }

  it should "return true if a double exists in a mutable Set of double" in {
    val searchTerm = Random.nextDouble()
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextDouble()) :+ searchTerm).to[MutableSet]
    assert(Udfs.IsIn(searchTerm, set))
  }

  it should "return false if a double does not exist in a mutable Set of double" in {
    val searchTerm: Double = 1e10d
    val set = (Seq.fill(Random.nextInt(100) + 1)(Random.nextDouble() * (1000 - 1) + 1)).to[MutableSet]
    assert(!Udfs.IsIn(searchTerm, set))
  }
}
