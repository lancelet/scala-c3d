package c3d.io

import scala.annotation.tailrec
import scala.collection.immutable._
import c3d.Parameter

/** Provides some indexing capabilities for a [[Parameter[T]]].
  * 
  * If a parameter provides:
  *   - dimensions
  *   - data
  * then this class provides:
  *   - apply(idx)
  *   - apply(i0)
  *   - apply(i0, i1)
  *   - apply(i0, i1, i2)
  */
private [io] trait ParameterTemplate[T] extends Parameter[T] {

  // cumulative product of all dimensions
  private lazy val cprod: Array[Int] = dimensions.scanLeft(1)(_ * _).drop(1).toArray

  /** Checks the range of an index and throws an `IndexOutOfBoundsException` if it is invalid.
    * 
    * @param idx index to check
    * @param dim the dimension to which the index belongs
    */
  private def checkIndex(idx: Int, dim: Int) {
    if ((idx < 0) || (idx >= dimensions(dim)))
      throw new IndexOutOfBoundsException(s"index for dimension $dim must be: 0 <= index < ${dimensions(dim)}")
  }

  def apply(idx: IndexedSeq[Int]): T = {
    require(idx.length == dimensions.length, "index must match data dimensions")
    val flatIndex: Int = {
      // tail-recursive function to calculate the flattened index
      @tailrec def accumIndex(flat: Int, d: Int): Int = {
        if (d == dimensions.length) flat
        else {
          checkIndex(idx(d), d)
          accumIndex(flat + idx(d) * cprod(d-1), d+1)
        }
      }
      checkIndex(idx(0), 0)
      accumIndex(idx(0), 1)
    }
    data(flatIndex)
  }

  def apply(i0: Int, i1: Int): T = {
    assert(dimensions.length == 2, "two-dimensional apply() called on a non-two-dimensional Parameter")
    checkIndex(i0, 0); checkIndex(i1, 1)
    data(i0 + i1 * cprod(0))
  }

  def apply(i0: Int, i1: Int, i2: Int): T = {
    assert(dimensions.length == 3, "three-dimensional apply() called on a non-three-dimensional Parameter")
    checkIndex(i0, 0); checkIndex(i1, 1); checkIndex(i2, 2)
    data(i0 + i1 * cprod(0) + i2 * cprod(1))
  }
  
}
