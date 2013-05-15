package c3d.io

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.math.abs
import scalaz.{Failure, Success, Validation}
import c3d.ProcessorType
import Util.b

private [io] object ParamSectionReader {

  /** Returns the [[ProcessorType]] from a parameter section.
    * 
    * The processor type is stored in the 4th byte of the parameter section (in the header), and this value is
    * returned, after conversion to a [[ProcessorType]].
    * 
    * @param paramISeq `IndexedSeq[Byte]` corresponding to the parameter section
    * @return [[ProcessorType]] used in the parameter section
    */
  private [io] def processorType(paramISeq: IndexedSeq[Byte]): Validation[String, ProcessorType] = {
    try {
      val processorByte: Byte = paramISeq(3)
      ProcessorTypeIO.byteToProcessorType(processorByte) map {
        Success(_)
      } getOrElse {
        Failure(f"unknown processor type byte $processorByte%d")
      }
    } catch {
      case ioe: IndexOutOfBoundsException =>
        Failure("parameter section too small to contain processor type byte")
    }
  }

  /** Splits the parameter section into blocks corresponding to groups and parameters.
    * 
    * The parameter section contains blocks of data, each of which corresponds to either a group or a parameter.
    * The first block starts at byte 5 of the parameter section.  Within each block, the following structure
    * is present:
    * {{{
    *   Byte | Length | Description
    *   ---------------------------------------------------------------------------------------------------------
    *      1 |      1 | Number of characters in group or parameter name (1-127).  A negative value indicates that
    *        |        | the group or parameter is locked.
    *   ---------------------------------------------------------------------------------------------------------
    *      2 |      1 | Group ID number (for the group, or the group to which the parameter belongs).
    *   ---------------------------------------------------------------------------------------------------------
    *      3 |      N | Group or parameter name.
    *   ---------------------------------------------------------------------------------------------------------
    *    3+N |      2 | Signed integer offset pointing to the start of the next group or parameter.
    * }}}
    * The byte at (3+N) is used as the offset to the start of the next section.
    * 
    * This function splits the entire parameter section into a sequence of blocks, each of which represents
    * either a group or parameter.
    * 
    * @param bf binaryformat used to read signed integers
    * @param paramISeq `IndexedSeq[Byte]` corresponding to the parameter section
    * @return sequence of blocks corresponding to either groups or parameters
    */
  private [io] def chunkGroupsAndParams(bf: BinaryFormat)(paramISeq: IndexedSeq[Byte]): 
      Validation[String, Seq[IndexedSeq[Byte]]] = 
  {
    /** Tail-recursive accumulator to collect blocks.
      * 
      * @param blocks blocks that have already been accumulated
      * @param rem remainder of the paramISeq to be processed
      * @return accumulated sequence of blocks
      */
    @tailrec
    def accum(blocks: Seq[IndexedSeq[Byte]], rem: IndexedSeq[Byte]): Seq[IndexedSeq[Byte]] = {
      val nCharsInName = abs(rem(0))
      val offset = bf.bytesToUInt(rem(2+nCharsInName), rem(3+nCharsInName))
      val byteOffset = offset + 2 + nCharsInName
      if (byteOffset == 0)
        blocks :+ rem.slice(0, rem.length)
      else
        accum(blocks :+ rem.slice(0, byteOffset), rem.slice(byteOffset, rem.length))
    }

    // Here we try to apply the accumulator.  Any failures are likely to be IndexOutOfBoundsExceptions,
    //  which indicate invalid offsets within the parameter section.
    try {
      Success(accum(Seq.empty[IndexedSeq[Byte]], paramISeq.slice(4, paramISeq.length)))
    } catch {
      case ioe: IndexOutOfBoundsException =>
        Failure("could not chunk groups and parameters (probably an invalid offset)")
    }
  }

}

