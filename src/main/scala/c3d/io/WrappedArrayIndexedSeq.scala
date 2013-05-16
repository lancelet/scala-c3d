package c3d.io

import scala.collection.immutable._

/** Wraps an `Array[T]` as an `IndexedSeq[T]`.
  * 
  * This is a utility class to enforce immutability of arrays.  It also implements a slice operation on construction,
  * so that slices are highly efficient.  When new slices are constructed, they reference offsets within
  * the original array, so that nesting of multiple slice operations does not impose a performance penalty.
  * 
  * A wrap for a zero-length array may be constructed by setting `from` = `until` = 0.
  * 
  * Instances of this class should be created using the apply methods available on its companion object.
  * 
  * @tparam T underlying type of the collection
  * @param array wrapped array
  * @param from array index corresponding to the zero index of the slice
  * @param until array index that is one beyond the end of the slice
  */
private [io] final class WrappedArrayIndexedSeq[T] private (array: Array[T], from: Int, until: Int) 
    extends IndexedSeq[T] 
{

  // this block performs checking of `from` and `until` against the original array
  {
    val maxFrom = if (array.length > 0) array.length else 1  // case where array.length == 0
    require(from  >= 0    && from  <= maxFrom,
      s"from must satisfy: 0 <= from < ${maxFrom}: from=${from}")
    require(until >= from && until <= array.length,
      s"until must satisfy: from <= until <= ${array.length}: from=${from}, until=${until}")
  }

  val length: Int = until - from

  def apply(idx: Int): T = {
    if (idx < 0 || idx >= length)
      throw new IndexOutOfBoundsException(s"Index must be in the range: 0 <= index < ${length}")
    else
      array(idx + from)
  }

  override def slice(relFrom: Int, relUntil: Int) = {
    // this block performs checking of `relFrom` and `relUntil` against the current `WrappedArrayIndexedSeq`
    {
      val maxFrom = if (length > 0) length else 1   // case where length == 0
      require(relFrom  >= 0       && relFrom  <= maxFrom, 
        s"relFrom must satisfy: 0 <= relFrom < ${maxFrom}: relFrom=${relFrom}")
      require(relUntil >= relFrom && relUntil <= length,  
        s"relUntil must satisfy: relFrom <= relUntil <= ${length}: relFrom=${relFrom}, relUntil=${relUntil}")
    }
    new WrappedArrayIndexedSeq(array, from + relFrom, from + relUntil)
  }

}

object WrappedArrayIndexedSeq {

  /** Creates a new `WrappedArrayIndexedSeq`.
    * 
    * @param array array to wrap
    * @param from from slice index
    * @param until until slice index
    * @tparam T type of the underlying array
    * @return new `WrappedArrayIndexedSeq`
    */
  def apply[T](array: Array[T], from: Int, until: Int): WrappedArrayIndexedSeq[T] =
    new WrappedArrayIndexedSeq(array, from, until)

  /** Creates a new un-sliced `WrappedArrayIndexedSeq`.
    * 
    * This method constructs a `WrappedArrayIndexedSeq` which wraps an entire underlying array without any
    * initial slicing.
    * 
    * @param array array to wrap
    * @tparam T type of the underlying array
    * @return new `WrappedArrayIndexedSeq`
    */
  def apply[T](array: Array[T]): WrappedArrayIndexedSeq[T] =
    new WrappedArrayIndexedSeq(array, 0, array.length)

}
