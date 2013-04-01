package c3d.iobase

/**
 * Immutable array-like structure of bytes.
 *
 * Specifies a wrapper trait which is expected to provide very fast random access to its contents.  Ideally, this trait
 * should be implemented by an array-backed class which is accessed immutably.
 */
trait ImmutableByteArray {
  def length: Int
  def byteAt(byteOffset: Int): Byte
  def apply(byteOffset: Int): Byte = byteAt(byteOffset)
}
