package c3d.iobase.file

import java.nio.ByteBuffer
import c3d.iobase.ImmutableByteArray

/**
 * ImmutableByteArray from a ByteBuffer.
 */
case class ByteBufferImmutableByteArray(byteBuffer: ByteBuffer) extends ImmutableByteArray {
  private val byteArray: Array[Byte] = {
    byteBuffer.position(0)
    val a = Array.ofDim[Byte](byteBuffer.limit())
    byteBuffer.get(a)
    a
  }
  def length: Int = byteArray.length
  def byteAt(byteOffset: Int): Byte = byteArray(byteOffset)
}
