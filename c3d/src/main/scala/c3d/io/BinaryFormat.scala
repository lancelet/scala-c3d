package c3d.io

import scala.collection.immutable._
import c3d.ProcessorType

/** Binary representation of `Float` and `Byte`. */
private [io] trait BinaryFormat {

  /** Converts 4 `Byte`s to a `Float`.
    * 
    * The bytes are supplied in the order they appear in the file.
    */
  def bytesToFloat(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float

  /** Converts 2 `Byte`s to an `Int`.
    * 
    * The bytes are supplied in the order they appear in the file.
    */
  def bytesToInt(b0: Byte, b1: Byte): Int

  /** Converts 2 `Byte`s to an unsigned `Int`.
    *
    * The bytes are supplied in the order they appear in the file.
    */
  def bytesToUInt(b0: Byte, b1: Byte): Int

}

private [io] object BinaryFormat {

  /** Little-endian conversion of 2 bytes to `Int`. */
  private def bytesToIntLittle(b0: Byte, b1: Byte): Int = (b0.toInt) + (b1.toInt << 8)

  /** Big-endian conversion of 2 bytes to `Int`. */
  private def bytesToUIntLittle(b0: Byte, b1: Byte): Int = (b0 & 0xFF) + ((b1 & 0xFF) << 8)

  /** BinaryFormat used by Intel processors. */
  object Intel extends BinaryFormat {
    def bytesToFloat(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = {
      val i = (b0 & 0xFF) + ((b1 & 0xFF) << 8) + ((b2 & 0xFF) << 16) + ((b3 & 0xFF) << 24)
      java.lang.Float.intBitsToFloat(i)
    }
    def bytesToInt(b0: Byte, b1: Byte): Int = bytesToIntLittle(b0, b1)
    def bytesToUInt(b0: Byte, b1: Byte): Int = bytesToUIntLittle(b0, b1)
  }

  /** BinaryFormat used by DEC processors. */
  object DEC extends BinaryFormat {
    def bytesToFloat(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = {
      throw new NotImplementedError("TODO: NOT YET IMPLEMENTED") // TODO
    }
    def bytesToInt(b0: Byte, b1: Byte): Int = bytesToIntLittle(b0, b1)
    def bytesToUInt(b0: Byte, b1: Byte): Int = bytesToUIntLittle(b0, b1)
  }

  /** BinaryFormat used by SGIMIPS processors. */
  object SGIMIPS extends BinaryFormat {
    def bytesToFloat(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Float = {
      throw new NotImplementedError("TODO: NOT YET IMPLEMENTED") // TODO
    }
    def bytesToInt(b0: Byte, b1: Byte): Int = bytesToIntLittle(b1, b0)    // swap b0, b1
    def bytesToUInt(b0: Byte, b1: Byte): Int = bytesToUIntLittle(b1, b0)  // swap b0, b1
  }

  /** Returns a [[BinaryFormat]] from a [[ProcessorType]]. */
  private [io] def fromProcessorType(p: ProcessorType): BinaryFormat = p match {
    case ProcessorType.Intel   => Intel
    case ProcessorType.DEC     => DEC
    case ProcessorType.SGIMIPS => SGIMIPS
  }

}
