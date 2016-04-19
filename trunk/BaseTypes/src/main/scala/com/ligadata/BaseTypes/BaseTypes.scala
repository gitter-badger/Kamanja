/*
 * Copyright 2015 ligaDATA
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ligadata.BaseTypes

import java.util._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import com.ligadata.kamanja.metadata.TypeImplementation
import java.io.{ DataOutputStream, DataInputStream }

// helper class that implement TypeImplementation 
object IntImpl extends TypeImplementation[Int] {
  override def Input(value: String): Int = { // Converts String to Type T
    value.trim match {
      case null => 0
      case x: String if x.length() == 0 => 0
      case x: String => x.trim.toInt
    }
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Int): Unit = { // Add Type T to DataOutputStream
    dos.writeInt(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Int = { // get Type T from DataInputStream 
    dis.readInt
  }

  override def toString(value: Int): String = { // Convert Type T to String
    value.toString
  }

  override def Clone(value: Int): Int = { // Clone and return same type
    return value
  }
}

object LongImpl extends TypeImplementation[Long] {
  override def Input(value: String): Long = { // Converts String to Type T
    value.trim match {
      case null => 0
      case x: String if x.length() == 0 => 0
      case x: String => x.trim.toLong
    }
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Long): Unit = { // Add Type T to DataOutputStream
    dos.writeLong(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Long = { // get Type T from DataInputStream 
    dis.readLong
  }

  override def toString(value: Long): String = { // Convert Type T to String
    value.toString
  }

  override def Clone(value: Long): Long = { // Clone and return same type
    return value
  }
}

object FloatImpl extends TypeImplementation[Float] {
  override def Input(value: String): Float = { // Converts String to Type T
    value.trim match {
      case null => 0
      case x: String if x.length() == 0 => 0
      case x: String => x.trim.toFloat
    }
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Float): Unit = { // Add Type T to DataOutputStream
    dos.writeFloat(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Float = { // get Type T from DataInputStream 
    dis.readFloat
  }

  override def toString(value: Float): String = { // Convert Type T to String
    value.toString
  }

  override def Clone(value: Float): Float = { // Clone and return same type
    return value
  }
}

object DoubleImpl extends TypeImplementation[Double] {
  override def Input(value: String): Double = { // Converts String to Type T
    value.trim match {
      case null => 0
      case x: String if x.length() == 0 => 0
      case x: String => x.trim.toDouble
    }
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Double): Unit = { // Add Type T to DataOutputStream
    dos.writeDouble(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Double = { // get Type T from DataInputStream 
    dis.readDouble
  }

  override def toString(value: Double): String = { // Convert Type T to String
    value.toString
  }

  override def Clone(value: Double): Double = { // Clone and return same type
    return value
  }
}

// currently only changes boolean to 1 or 0.

object BoolImpl extends TypeImplementation[Boolean] {
  override def Input(value: String): Boolean = { // Converts String to Type T
    val v = value.trim
    if (v.compareToIgnoreCase("t") == 0 || v.compareToIgnoreCase("y") == 0 || v.compareToIgnoreCase("1") == 0 ||
      v.compareToIgnoreCase("on") == 0 || v.compareToIgnoreCase("yes") == 0 || v.compareToIgnoreCase("true") == 0)
      return true
    if (v.compareToIgnoreCase("f") == 0 || v.compareToIgnoreCase("n") == 0 || v.compareToIgnoreCase("0") == 0 ||
      v.compareToIgnoreCase("no") == 0 || v.compareToIgnoreCase("off") == 0 || v.compareToIgnoreCase("false") == 0)
      return false
    throw new Exception("Invalid boolean value:" + value)
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Boolean): Unit = { // Add Type T to DataOutputStream
    dos.writeBoolean(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Boolean = { // get Type T from DataInputStream 
    dis.readBoolean
  }

  override def toString(value: Boolean): String = { // Convert Type T to String
    if (value) 1.toString else 0.toString
  }

  override def Clone(value: Boolean): Boolean = { // Clone and return same type
    return value
  }
}

object StringImpl extends TypeImplementation[String] {
  override def Input(value: String): String = { // Converts String to Type T
    value
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: String): Unit = { // Add Type T to DataOutputStream
    if (value != null) dos.writeUTF(value)
    else dos.writeUTF("")
  }
  override def DeserializeFromDataInputStream(dis: DataInputStream): String = { // get Type T from DataInputStream 
    val value = dis.readUTF
    if (value != null) return value
    else return ""
  }

  override def toString(value: String): String = { // Convert Type T to String
    value
  }

  override def Clone(value: String): String = { // Clone and return same type
    return value
  }
}

object CharImpl extends TypeImplementation[Char] {
  override def Input(value: String): Char = { // Converts String to Type T
    value.trim match {
      case null => ' '
      case x: String if x.length() == 0 => ' '
      case x: String => x.trim.charAt(0)
    }
  }

  override def SerializeIntoDataOutputStream(dos: DataOutputStream, value: Char): Unit = { // Add Type T to DataOutputStream
    dos.writeChar(value)
  }

  override def DeserializeFromDataInputStream(dis: DataInputStream): Char = { // get Type T from DataInputStream 
    dis.readChar
  }

  override def toString(value: Char): String = { // Convert Type T to String
    value.toString
  }

  override def Clone(value: Char): Char = { // Clone and return same type
    return value
  }
}


