/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import scala.collection.immutable.IndexedSeq
import c3d.io.collection.ImmutableArray

/**
 * Parameter without detailed type access.
 */
trait GenericParameter {

  /** Checks whether the parameter is locked. */
  def isLocked: Boolean

  /** Returns the group ID (always positive). */
  def groupId: Int

  /** Returns the name of the parameter. */
  def name: String

  /** Returns the description of the parameter. */
  def description: String

  /** Returns the dimensions of the parameter. */
  def dimensions: IndexedSeq[Int]

  /** Returns the type of the parameter. */
  def paramType: c3d.Parameter.Type

  /** Returns the parameter data as an immutable byte array. */
  def data: ImmutableArray[Byte]

}

object GenericParameter {

  /**
   * Creates a generic parameter block from a chunked parameter block.
   *
   * The chunked parameter block must represent a parameter (ie. it must have `chunk.isGroup == false`).
   *
   * @param chunk chunk from which to create generic parameter
   * @return generic parameter
   */
  def apply(chunk: ChunkedParamBlock.PBChunk): GenericParameter = new DefaultGenericParameter(chunk)

  private final class DefaultGenericParameter(chunk: ChunkedParamBlock.PBChunk) extends GenericParameter {
    assert(!chunk.isGroup, "attempted to create a parameter from a group chunk")

    private def nDims: Int         = chunk.data(5 + name.length)
    private def nDesc: Int         = chunk.data(6 + name.length + nDims + data.length)
    private def elementNBytes: Int = chunk.data(4 + name.length)
    private def descOfs: Int       = 7 + name.length + nDims + data.length

    val data: ImmutableArray[Byte]    =
      chunk.data.slice(6 + name.length + nDims, 6 + name.length + nDims + dimensions.product * math.abs(elementNBytes))

    def isLocked: Boolean             = chunk.isLocked
    def groupId: Int                  = chunk.groupId
    def name: String                  = chunk.name
    val description: String           = getDescription(chunk.data, descOfs, nDesc)
    def dimensions: IndexedSeq[Int]   = getDimensions(chunk.data, 6 + name.length, 6 + name.length + nDims)
    def paramType: c3d.Parameter.Type = getParamType(elementNBytes)
  }

  private def getDescription(data: ImmutableArray[Byte], offset: Int, length: Int): String = {
    if (length == 0) "" else data.slice(offset, offset + length).toIndexedSeq.map(_.toChar).mkString
  }

  private def getDimensions(data: ImmutableArray[Byte], from: Int, until: Int): IndexedSeq[Int] = {
    if (until - from == 0) ScalarDimension else new LinDimensions(data.slice(from, until))
  }

  private object ScalarDimension extends IndexedSeq[Int] {
    def length: Int = 1
    def apply(i: Int): Int = 1
  }

  private final class LinDimensions(a: ImmutableArray[Byte]) extends IndexedSeq[Int] {
    def length: Int = a.length
    def apply(i: Int): Int = a(i)
  }

  private def getParamType(nBytes: Int): c3d.Parameter.Type = nBytes match {
    case -1 => c3d.Parameter.Type.Character
    case  1 => c3d.Parameter.Type.Byte
    case  2 => c3d.Parameter.Type.Integer
    case  4 => c3d.Parameter.Type.Float
  }

}