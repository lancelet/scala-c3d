package c3d.io

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.math.abs
import scalaz.{Failure, Success, Validation}
import c3d.{Group, ProcessorType}
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
    * @param paramISeq the parameter section bytes
    * @return sequence of blocks corresponding to either groups or parameters
    */
  private [io] def chunkGroupsAndParams(paramISeq: FormattedByteIndexedSeq): 
      Validation[String, Seq[FormattedByteIndexedSeq]] =
  {
    /** Tail-recursive accumulator to collect blocks.
      * 
      * @param blocks blocks that have already been accumulated
      * @param rem remainder of the paramISeq to be processed
      * @return accumulated sequence of blocks
      */
    @tailrec
    def accum(blocks: Seq[FormattedByteIndexedSeq], rem: FormattedByteIndexedSeq): Seq[FormattedByteIndexedSeq] = {
      val nCharsInName = abs(rem(0))
      val offset = rem.uintAt(2 + nCharsInName)
      val byteOffset = offset + 2 + nCharsInName
      if (offset == 0)
        blocks :+ rem.slice(0, rem.length)
      else
        accum(blocks :+ rem.slice(0, byteOffset), rem.slice(byteOffset, rem.length))
    }

    // Here we try to apply the accumulator.  Any failures are likely to be IndexOutOfBoundsExceptions,
    //  which indicate invalid offsets within the parameter section.
    try {
      Success(accum(Seq.empty[FormattedByteIndexedSeq], paramISeq.slice(4, paramISeq.length)))
    } catch {
      case ioe: IndexOutOfBoundsException =>
        Failure("could not chunk groups and parameters (probably an invalid offset)")
    }
  }


  /** Partitions parameter section blocks into a sequence of groups and sequence of parameters.
    *
    * Given a sequence of blocks from the parameter section, this method splits them into two sequences: one
    * containing only groups and one containing only parameters.
    * 
    * Groups are identified by their having a negative group number (byte 1), while parameters have a 
    * positive group number.
    * 
    * @param blocks sequence of generic blocks (either groups OR parameters)
    * @return (groups, parameters)
    */
  private [io] def partitionToGroupsAndParams(blocks: Seq[FormattedByteIndexedSeq]): 
      (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = 
  {
    def groupId(block: FormattedByteIndexedSeq): Byte = block(1)
    def isGroup(block: FormattedByteIndexedSeq): Boolean = groupId(block) < 0
    blocks.partition(isGroup _)
  }

  /** Unassociated group, without any connected parameters.
    * 
    * This class passively reads group fields from a parameter section block.
    * 
    * @param block parameter section block
    */
  private [io] final class UnassociatedGroup(block: FormattedByteIndexedSeq) {
    private val nName: Int = abs(block(0))
    private val nDesc: Int = block(4 + nName)
    def name: String = block.slice(2, 2 + nName).map(_.toChar).mkString
    def description: String = block.slice(5 + nName, 5 + nName + nDesc).map(_.toChar).mkString
    def id: Int = { assert(block(1) < 0); -block(1) }
    def isLocked: Boolean = block(0) < 0

    {
      val lenExpected = nName + nDesc + 5
      require(block.length == lenExpected, 
        s"block was expected to be ${lenExpected} bytes long, but was ${block.length}")
    }
  }

  /** Unassociated parameter, without a connected group and without typing information.
    *
    * This class passively reads parameter fields from a parameter section block.
    * 
    * @param block parameter section block
    */
  private [io] final class UntypedParameter[T](block: FormattedByteIndexedSeq) {
    private def nName: Int = abs(block(0))
    private def nDesc: Int = block(6 + nName + nDims + data.length)
    private def descOfs: Int = 7 + nName + nDims + data.length
    private def nElem: Int = abs(byteLengthPerElement)
    private def nDims: Int = block(5 + nName)
    def name: String = block.slice(2, 2 + nName).map(_.toChar).mkString
    def description: String = block.slice(descOfs, descOfs + nDesc).map(_.toChar).mkString
    def groupId: Int = { assert(block(1) > 0); block(1) }
    def dimensions: IndexedSeq[Int] = new IndexedSeq[Int] {
      def length: Int = nDims
      def apply(idx: Int): Int = block(6 + nName + idx)
    }
    def byteLengthPerElement: Int = block(4 + nName)
    def data: IndexedSeq[Byte] = new IndexedSeq[Byte] {
      def length: Int = dimensions.product * nElem
      def apply(idx: Int): Byte = block(6 + nName + nDims + idx)
    }
    def isLocked: Boolean = block(0) < 0
  }

}

