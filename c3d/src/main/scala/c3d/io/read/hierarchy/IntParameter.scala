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
import c3d.{ProcessorType, Parameter}
import c3d.io.collection.{FormattedByteArray, ImmutableArray}
import c3d.io.collection.FormattedByteArray._

trait IntParameter extends Parameter[Int]

object IntParameter {

  def apply(g: GenericParameter, p: ProcessorType): IntParameter = new DefaultIntParameter(g, p)

  final class DefaultIntParameter(g: GenericParameter, p: ProcessorType)
    extends AbstractParameterIndexing[Int] with IntParameter
  {
    assert(g.paramType == Parameter.Type.Integer, "not an integer parameter")

    def name: String                  = g.name
    def description: String           = g.description
    def isLocked: Boolean             = g.isLocked
    def dimensions: IndexedSeq[Int]   = g.dimensions
    def data: IndexedSeq[Int]         = new IntIndexedSeq(g.data, p)
    def parameterType: Parameter.Type = Parameter.Type.Integer
  }

  final class IntIndexedSeq(data: ImmutableArray[Byte], p: ProcessorType) extends IndexedSeq[Int] {
    private val fba: FormattedByteArray = data.forProcessor(p)
    val length: Int = fba.length / 2
    def apply(i: Int): Int = fba.intAt(i * 2)
  }

}
