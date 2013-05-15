package c3d.io

import scala.collection.immutable._

/** Wraps an `Array[T]` as an `IndexedSeq[T]`.
  * 
  * This is a utility class to enforce immutability of arrays.  It also implements a slice operation on construction,
  * so that slices are highly efficient.
  * 
  * A wrap for a zero-length array may be constructed by setting `from` = `until` = 0.
  * 
  * @tparam T underlying type of the collection
  * @param array wrapped array
  * @param from array index corresponding to the zero index of the slice
  * @param until array index that is one beyond the end of the slice
  */
class WrappedArrayIndexedSeq[T](array: Array[T], from: Int, until: Int) extends IndexedSeq[T] {

  {
    val maxFrom = if (array.length > 0) array.length else 1  // case where array.length == 0
    require(from  >= 0    && from  <= maxFrom,
      s"from must satisfy: 0 <= from < ${maxFrom}: from=${from}")
    require(until >= from && until <= array.length,
      s"until must satisfy: from <= until <= ${array.length}: from=${from}, until=${until}")
  }

  def length: Int = until - from

  def apply(idx: Int): T = {
    if (idx >= 0 && idx < length)
      array(idx + from)
    else
      throw new IndexOutOfBoundsException(s"Index must be in the range: 0 <= index < ${length}")
  }

  override def slice(relFrom: Int, relUntil: Int) = {
    {
      val maxFrom = if (length > 0) length else 1
      require(relFrom  >= 0       && relFrom  <= maxFrom, 
        s"relFrom must satisfy: 0 <= relFrom < ${maxFrom}: relFrom=${relFrom}")
      require(relUntil >= relFrom && relUntil <= length,  
        s"relUntil must satisfy: relFrom <= relUntil <= ${length}: relFrom=${relFrom}, relUntil=${relUntil}")
    }
    new WrappedArrayIndexedSeq(array, from + relFrom, from + relUntil)
  }

}
