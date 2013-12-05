package c3d.io

import scala.collection.immutable._
import c3d.io.collection.ImmutableArray

/** Pairs a [[c3d.io.BinaryFormat]] with an `IndexedSeq[Byte]` to allow easy reading of C3D ints, uints and floats.
  * 
  * @param array `IndexedSeq[Byte]` to wrap
  * @param binaryFormat [[c3d.io.BinaryFormat]] to use when reading ints, uints and floats
  */
private [io] final class FormattedByteIndexedSeq(array: ImmutableArray[Byte], val binaryFormat: BinaryFormat)
    extends IndexedSeq[Byte] 
{
  def length: Int = array.length
  def apply(byteIdx: Int): Byte = array(byteIdx)
  def intAt(byteIdx: Int): Int = binaryFormat.bytesToInt(array(byteIdx), array(byteIdx+1))
  def uintAt(byteIdx: Int): Int = binaryFormat.bytesToUInt(array(byteIdx), array(byteIdx+1))
  def floatAt(byteIdx: Int): Float = binaryFormat.bytesToFloat(
    array(byteIdx), array(byteIdx+1), array(byteIdx+2), array(byteIdx+3))
  override def slice(from: Int, until: Int): FormattedByteIndexedSeq = 
    new FormattedByteIndexedSeq(array.slice(from, until), binaryFormat)
}
