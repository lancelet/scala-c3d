/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import scala.collection.Iterator
import scala.collection.immutable.IndexedSeq
import c3d.io.collection.ImmutableArray
import c3d.ProcessorType
import c3d.io.collection.FormattedByteArray._

/**
 * Splits the parameter section into blocks corresponding to groups and parameters.
 */
trait ChunkedParamBlock extends IndexedSeq[ChunkedParamBlock.PBChunk]

object ChunkedParamBlock {

  /**
   * Creates a chunked parameter block from a `C3DBlock`.
   *
   * @param c c3d block from which to create a chunked parameter block
   * @return chunked parameter block
   */
  def apply(c: C3DBlock): ChunkedParamBlock = new DefaultChunkedParamBlock(c)

  /**
   * Parameter block chunk.
   *
   * A chunk from the parameter block can represent either a group or a parameter.  Parameter block chunks should be
   * created from a `C3DBlock` using the `ChunkedParamBlock` companion object.
   */
  trait PBChunk {

    /** Checks whether the parameter or group is locked. */
    def isLocked: Boolean

    /** Checks whether the item is a parameter or group. */
    def isGroup: Boolean

    /** Returns the group ID (always positive). */
    def groupId: Int

    /** Returns the name of the group or parameter. */
    def name: String

    /** Returns the data corresponding to the whole block group or parameter. */
    def data: ImmutableArray[Byte]

  }

  private final class DefaultPBChunk(a: ImmutableArray[Byte], p: ProcessorType) extends PBChunk {

    // NOTE: For this class, `a` is an ImmutableArray[Byte] containing the remainder of the parameter section, not
    //       just the data corresponding to this one chunk.  The DefaultPBChunk itself acts as a cursor in an iterator
    //       (see hasNext() and next() below), which chunk off parts from `a` as required.

    private val nName: Int  = math.abs(a(0))
    private def length: Int = if (offset > 0) offset else a.length

    def offset: Int                     = 2 + nName + (a.forProcessor(p).uintAt(2 + nName) & 0xFFFF)
    def hasNext: Boolean                = offset > 2
    def next: DefaultPBChunk            = new DefaultPBChunk(a.slice(offset, a.length), p)

    def isLocked: Boolean               = a(0) < 0
    def isGroup: Boolean                = a(1) < 0
    def groupId: Int                    = math.abs(a(1))
    def name: String                    = a.slice(2, 2 + nName).toIndexedSeq.map(_.toChar).mkString
    def data: ImmutableArray[Byte]      = a.slice(0, length)
  }

  private final class ChunkIt(val pBlock: ImmutableArray[Byte], p: ProcessorType) extends Iterator[DefaultPBChunk] {
    private var chunk: DefaultPBChunk = new DefaultPBChunk(pBlock.slice(4, pBlock.length), p)
    def hasNext: Boolean = chunk.hasNext
    def next(): DefaultPBChunk = { val c = chunk; chunk = chunk.next; c }
  }

  private final class DefaultChunkedParamBlock(c: C3DBlock) extends ChunkedParamBlock {
    private val s: IndexedSeq[PBChunk] = new ChunkIt(c.paramBlock, c.processorType).toIndexedSeq
    def length: Int = s.length
    def apply(i: Int): PBChunk = s(i)
  }

}
