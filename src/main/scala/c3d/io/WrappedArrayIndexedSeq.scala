package c3d.io

import scala.collection.immutable._

/** Wraps an Array[T] as an IndexedSeq[T].
  * 
  * This is a utility class to enforce immutability of arrays.  It also implements a slice operation on construction,
  * so that slices are highly efficient.
  * 
  * A wrap for a zero-length array may be constructed by setting from = until = 0.
  */
class WrappedArrayIndexedSeq[T](array: Array[T], from: Int, until: Int) extends IndexedSeq[T] {

  {
    val maxFrom = if (array.length > 0) array.length else 1  // case where array.length == 0
    require(from  >= 0    && from  <  maxFrom,      s"from must satisfy: 0 <= from < ${maxFrom}")
    require(until >= from && until <= array.length, s"until must satisfy: from <= until <= ${array.length}")
  }

  def length: Int = until - from

  def apply(idx: Int): T = {
    if (idx >= 0 && idx < length)
      array(idx + from)
    else
      throw new IndexOutOfBoundsException(s"Index must be in the range: 0 <= index < ${length}")
  }

  override def slice(from: Int, until: Int) = new WrappedArrayIndexedSeq(array, from, until)

}
