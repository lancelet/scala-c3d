/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import java.io.File
import scala.util.{Failure, Success}
import c3d.ProcessorType
import c3d.io.collection.ImmutableArray
import c3d.io.Util.b
import c3d.io.{C3DIOException, FileUtils}

/**
 * Whole C3D file block.
 *
 * A `C3DBlock` can be created using methods on its companion object; for example:
 * {{{
 * val inFile: File = // ...
 * val block: C3DBlock = C3DBlock(inFile)
 * }}}
 */
trait C3DBlock {

  /**
   * Returns the whole file byte array.
   *
   * @return whole file
   */
  def wholeFile: ImmutableArray[Byte]

  /**
   * Returns an identifying name for the source of the block.
   *
   * When running operations on multiple C3D files, it is often useful to be able quickly to fetch an identifier which
   * states the source of the C3D data.  `sourceName` stores such an identifier.  It should not be relied upon in the
   * most general context however.  It is valid for `sourceName` to be an empty string.
   *
   * @return identifying name
   */
  def sourceName: String

  /**
   * Checks for the magic identifier byte.
   *
   * @return `true` if the block has the correct identifier byte; `false` otherwise
   */
  def hasMagicByte: Boolean

  /**
   * Returns the processor type used by the C3D file.
   *
   * @return processor type
   */
  def processorType: ProcessorType

  /**
   * Returns the immutable byte array corresponding to the entire parameter block.
   *
   * @return parameter block
   */
  def paramBlock: ImmutableArray[Byte]

}

object C3DBlock {

  /**
   * Creates a `C3DBlock` from an immutable array of bytes.
   *
   * @param sourceName string identifier used to name the source of the whole C3D file
   * @param wholeFile whole file, wrapped as an immutable array of bytes
   * @return c3d block
   */
  def apply(sourceName: String, wholeFile: ImmutableArray[Byte]): C3DBlock = new DefaultC3DBlock(wholeFile, sourceName)

  /**
   * Creates a `C3DBlock` by reading a file.
   *
   * @param file file to read
   * @return c3d block
   */
  def apply(file: File): C3DBlock = FileUtils.fileToImmutableArray(file) match {
    case Success(wholeFile) => apply(file.getCanonicalFile.getName, wholeFile)
    case Failure(e)         => throw e
  }

  private final val SectionSize: Int  = 512
  private final val IntelByte: Byte   = 84
  private final val DECByte: Byte     = 85
  private final val SGIMIPSByte: Byte = 86

  final class DefaultC3DBlock(data: ImmutableArray[Byte], val sourceName: String) extends C3DBlock {

    val wholeFile: ImmutableArray[Byte] = data

    val hasMagicByte: Boolean = data.length >= 2 && data(1) == b(0x50)

    lazy val paramBlock: ImmutableArray[Byte] = {
      val offset    = (data(0) - 1) * SectionSize         // offset of parameter section is 1st byte of file (1-based)
      val nDataSecs = data(offset + 2)                    // number of data sections (3rd byte of parameter section)
      val until     = offset + (SectionSize * nDataSecs)  // end of data section slice
      data.slice(offset, until)
    }

    lazy val processorType: ProcessorType = paramBlock(3) match {
      case IntelByte   => ProcessorType.Intel
      case DECByte     => ProcessorType.DEC
      case SGIMIPSByte => ProcessorType.SGIMIPS
      case _ => throw C3DIOException("Unknown processor type byte.")
    }

  }

}