package c3d.iobase

/**
 * Binary reading structure for the header section of a C3D file.
 */
class HeaderSection(bytes: ImmutableByteArray) {
  require(bytes.length == 512, "HeaderSection must be 512 bytes long")
  def hasMagicNumber: Boolean = (bytes(1) == HeaderSection.MagicNumber)
  def firstParameterBlock: Int = bytes(0) & 0xFF
}

object HeaderSection {
  val MagicNumber: Byte = 0x50.toByte
}
