package c3d.iobase

/**
 */
trait ImmutableByteArray {
  def length: Int
  def byteAt(byteOffset: Int): Byte
}
