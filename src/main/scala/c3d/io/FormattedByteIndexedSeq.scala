package c3d.io

import scala.collection.immutable._

/** Pairs a [[BinaryFormat]] with an `IndexedSeq[Byte]` to allow easy reading of ints, uints and floats.
  * 
  * @param is `IndexedSeq[Byte]` to wrap
  * @param bf [[BinaryFormat]] to use when reading ints, uints and floats
  */
class FormattedByteIndexedSeq(is: IndexedSeq[Byte], bf: BinaryFormat) extends IndexedSeq[Byte] {
  def length: Int = is.length
  def apply(byteIdx: Int): Byte = is(byteIdx)
  def intAt(byteIdx: Int): Int = bf.bytesToInt(is(byteIdx), is(byteIdx+1))
  def uintAt(byteIdx: Int): Int = bf.bytesToUInt(is(byteIdx), is(byteIdx+1))
  def floatAt(byteIdx: Int): Float = bf.bytesToFloat(is(byteIdx), is(byteIdx+1), is(byteIdx+2), is(byteIdx+3))
  override def slice(from: Int, until: Int): FormattedByteIndexedSeq = 
    new FormattedByteIndexedSeq(is.slice(from, until), bf)
}
