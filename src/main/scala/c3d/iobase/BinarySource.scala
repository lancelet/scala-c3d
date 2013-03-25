package c3d.iobase

import scalaz.Validation

/**
 */
trait BinarySource extends AutoCloseable {
  def length: Long
  def getBytes(byteOffset: Long, length: Int): Validation[String, ImmutableByteArray]
}
