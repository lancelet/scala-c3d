/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

/**
 * A group.
 */
trait Group {

  /** Checks whether the group is locked. */
  def isLocked: Boolean

  /** Returns the group ID (always positive). */
  def groupId: Int

  /** Returns the name of the group. */
  def name: String

  /** Returns the description of the group. */
  def description: String

}

object Group {

  /**
   * Creates a group block from a chunked parameter block.
   *
   * The chunked parameter block must represent a group (ie. it must have `chunk.isGroup == true`).
   *
   * @param chunk chunk from which to create a group block
   * @return group block
   */
  def apply(chunk: ChunkedParamBlock.PBChunk): Group = new DefaultGroup(chunk)

  private final class DefaultGroup(chunk: ChunkedParamBlock.PBChunk) extends Group {
    assert(chunk.isGroup, "attempted to create a group from a parameter chunk")

    private def nDesc: Int     = chunk.data(4 + name.length) & 0xFFFF
    private def descFrom: Int  = 5 + name.length
    private def descUntil: Int = descFrom + nDesc

    def isLocked: Boolean   = chunk.isLocked
    def groupId: Int        = chunk.groupId
    def name: String        = chunk.name
    def description: String = chunk.data.slice(descFrom, descUntil).toIndexedSeq.map(_.toChar).mkString
  }

}