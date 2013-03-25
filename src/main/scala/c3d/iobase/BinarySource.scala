package c3d.iobase

import scalaz.Validation
import java.io.File
import c3d.iobase.file.FileBinarySource

/**
 * Source of binary data.
 *
 * Represents a source of ImmutableByteArray objects.  The binary source may be queried for ImmutableByteArray
 * objects at any byte offset.  The source may be "live", in that it can refer directly to an open file.  The source
 * is AutoCloseable so that it may be resource-managed.
 */
trait BinarySource extends AutoCloseable {
  def length: Long
  def getBytes(byteOffset: Long, length: Int): Validation[String, ImmutableByteArray]
}

object BinarySource {
  def fromFile(inputFile: File): BinarySource = new FileBinarySource(inputFile)
}
