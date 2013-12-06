/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.collection

import scala.collection.immutable.IndexedSeq
import c3d.io.Util.b
import c3d.ProcessorType

/**
 * Accesses data types within an array of bytes.
 *
 * C3D files can be formatted for three different processor types:
 *  - Intel
 *  - DEC
 *  - SGI/MIPS
 *
 * Instances of this trait interpret an immutable array of bytes as thought the array contained data types stored by
 * a particular processor.  Thus, it provides a view onto an array of bytes that allows access to specific data types
 * (integer, unsigned integer and 4-byte floating point) as though stored by a given processor.
 *
 * Instances of `FormattedByteArray` can be constructed by calling the implicit method `withProcessor` on an
 * `ImmutableArray[Byte]`.  For example:
 * {{{
 *   val a = ImmutableArray[Byte](1.toByte, 2.toByte)
 *   val f: FormattedByteArray = a.forProcessor(c3d.ProcessorType.Intel)
 * }}}
 */
trait FormattedByteArray extends ImmutableArray[Byte] {

  /**
   * Gets a 2-byte integer value, at a given byte index.
   *
   * @param byteIndex byte-level index of the integer
   * @return 2-byte integer
   */
  def intAt(byteIndex: Int): Int

  /**
   * Gets a 2-byte unsigned integer value, at a given byte index.
   *
   * @param byteIndex byte-level index of the unsigned integer
   * @return 2-byte unsigned integer
   */
  def uintAt(byteIndex: Int): Int

  /**
   * Gets a 4-byte floating point value, at a given byte index.
   *
   * @param byteIndex byte-level index of the floating point value
   * @return 4-byte floating point value
   */
  def floatAt(byteIndex: Int): Float

  def slice(from: Int, until: Int): FormattedByteArray

}

object FormattedByteArray {

  implicit class ImmutableArrayConverter(a: ImmutableArray[Byte]) {
    def forProcessor(p: ProcessorType): FormattedByteArray = FormattedByteArray(a, p)
  }

  private [collection] def apply(a: ImmutableArray[Byte], p: ProcessorType): FormattedByteArray =
    new DefaultFormattedByteArray(a, formatFromProcessorType(p))

  private final class DefaultFormattedByteArray(a: ImmutableArray[Byte], format: Format) extends FormattedByteArray {
    def intAt(i: Int): Int     = format.int  (a(i), a(i+1))
    def uintAt(i: Int): Int    = format.uint (a(i), a(i+1))
    def floatAt(i: Int): Float = format.float(a(i), a(i+1), a(i+2), a(i+3))

    def length: Int = a.length
    def apply(i: Int): Byte = a(i)
    def slice(from: Int, until: Int): FormattedByteArray = new DefaultFormattedByteArray(a.slice(from, until), format)
    def toIndexedSeq: IndexedSeq[Byte] = a.toIndexedSeq
  }

  // fetches a format conversion to match a given processor type
  private def formatFromProcessorType(p: ProcessorType): Format = p match {
    case ProcessorType.Intel   => Intel
    case ProcessorType.DEC     => DEC
    case ProcessorType.SGIMIPS => SGIMIPS
  }

  // format conversions trait
  private sealed trait Format {
    def float(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float
    def int(b0: Byte, b1: Byte): Int
    def uint(b0: Byte, b1: Byte): Int
  }

  // basic little-endian conversion routines
  private def intL(b0: Byte, b1: Byte): Int  = (b0 & 0xFF) + (b1.toInt << 8)     // little-endian
  private def uintL(b0: Byte, b1: Byte): Int = (b0 & 0xFF) + ((b1 & 0xFF) << 8)  // little-endian
  private def floatL(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = {
    val i: Int = (b0 & 0xFF) | ((b1 & 0xFF) << 8) | ((b2 & 0xFF) << 16) | ((b3 & 0xFF) << 24)
    java.lang.Float.intBitsToFloat(i)
  }

  // intel format
  private object Intel extends Format {
    def float(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = floatL(b0, b1, b2, b3)
    def int(b0: Byte, b1: Byte): Int = intL(b0, b1)
    def uint(b0: Byte, b1: Byte): Int = uintL(b0, b1)
  }

  // DEC format
  private object DEC extends Format {
    def float(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = {
      val bf: Byte = if (b1 == b(0)) b(0) else b(1)
      floatL(b2, b3, b0, b(b1 - bf))
    }
    def int(b0: Byte, b1: Byte): Int = intL(b0, b1)
    def uint(b0: Byte, b1: Byte): Int = uintL(b0, b1)
  }

  // SGI/MIPS format
  private object SGIMIPS extends Format {  // just like Intel, but big-endian (swap byte order)
    def float(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = floatL(b3, b2, b1, b0)
    def int(b0: Byte, b1: Byte): Int = intL(b1, b0)
    def uint(b0: Byte, b1: Byte): Int = uintL(b1, b0)
  }

}