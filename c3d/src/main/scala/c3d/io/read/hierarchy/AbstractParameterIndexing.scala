/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import c3d.Parameter
import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

/**
 * Provides indexing functionality for parameters.
 *
 * @tparam T type of the parameter
 */
abstract class AbstractParameterIndexing[T] extends Parameter[T] {

  // cumulative product of dimensions
  private lazy val cprod: Array[Int] = dimensions.scanLeft(1)(_ * _).drop(1).toArray

  def apply(idx: IndexedSeq[Int]): T = {
    require(idx.length == dimensions.length, "index and data dimensions must match")
    val flatIndex: Int = {
      // tail recursive flattened index
      @tailrec def accumIndex(flat: Int, d: Int): Int = {
        if (d == dimensions.length) {
          flat
        } else {
          checkIndex(idx(d), d)
          accumIndex(flat + idx(d) * cprod(d-1), d+1)
        }
      }
      checkIndex(idx(0), 0)
      accumIndex(idx(0), 1)
    }
    data(flatIndex)
  }

  def apply(i0: Int): T = {
    assert(dimensions.length == 1, "can only call 1D apply() on a 1D parameter")
    checkIndex(i0, 0)
    data(i0)
  }

  def apply(i0: Int, i1: Int): T = {
    assert(dimensions.length == 2, "can only call 2D apply() on a 2D parameter")
    checkIndex(i0, 0)
    checkIndex(i1, 1)
    data(i0 + i1 * cprod(0))
  }

  def apply(i0: Int, i1: Int, i2: Int): T = {
    assert(dimensions.length == 3, "can only call 3D apply() on a 3D parameter")
    checkIndex(i0, 0)
    checkIndex(i1, 1)
    checkIndex(i2, 2)
    data(i0 + i1 * cprod(0) + i2 * cprod(1))
  }

  private def checkIndex(i: Int, dimension: Int) {
    if (i < 0 || i >= dimensions(dimension))
      throw new IndexOutOfBoundsException(s"index for dimension $dimension must be: 0 <= i < ${dimensions(dimension)}")
  }

}
