package c3d.iobase.file

import java.nio.ByteBuffer
import c3d.iobase.ImmutableByteArray

/**
 */
case class ByteBufferImmutableByteArray(byteBuffer: ByteBuffer) extends ImmutableByteArray {
  private val byteArray: Array[Byte] = {
    val a = Array.ofDim[Byte](byteBuffer.limit())
    byteBuffer.get(a)
    a
  }
  def length: Int = byteArray.length
  def byteAt(byteOffset: Int): Byte = byteArray(byteOffset)
}
