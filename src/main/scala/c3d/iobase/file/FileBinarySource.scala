package c3d.iobase.file

import java.io.{RandomAccessFile, File}
import c3d.iobase.{ImmutableByteArray, BinarySource}
import scalaz.{Failure, Success, Validation}
import java.nio.channels.FileChannel
import java.nio.ByteBuffer

/**
 * BinarySource from a File using NIO.
 */
final class FileBinarySource(inputFile: File) extends BinarySource {

  private val fileChannelValidation: Validation[String, FileChannel] = {
    try {
      val raf = new RandomAccessFile(inputFile, "r")
      Success(raf.getChannel())
    } catch {
      case e: Exception => Failure(e.getMessage())
    }
  }

  val length: Long = inputFile.length()
  def getBytes(byteOffset: Long, length: Int): Validation[String, ImmutableByteArray] = {
    fileChannelValidation.flatMap { fc =>
      if (fc.isOpen()) {
        fc.position(byteOffset)
        val buffer = ByteBuffer.allocate(length)
        val readLength = fc.read(buffer)
        if (readLength != length) {
          Failure(s"Tried to read $length bytes, but only received $readLength bytes")
        } else {
          Success(ByteBufferImmutableByteArray(buffer))
        }
      } else {
        Failure("Attempted to retrieve data from a FileBinarySource that has already been closed")
      }
    }
  }

  def close(): Unit = for (fc <- fileChannelValidation) fc.close()
}
