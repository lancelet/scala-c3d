package c3d.io.collection

import scala.collection.immutable.IndexedSeq
import scala.reflect.ClassTag

/**
 * Immutable array wrapper.
 *
 * Instances of this trait wrap arrays, providing a read-only, immutable interface.  When <code>ImmutableArray</code>
 * traits are instantiated, the array to be wrapped must be copied defensively <it>by the caller</it> if there is a
 * possibility that it might be modified.  The <code>ImmutableArray</code> wrapper never performs defensive copying
 * itself.
 *
 * An <code>ImmutableArray</code> can be constructed by calling the <code>apply()</code> methods on the companion
 * object.
 *
 * @tparam T type of the array
 */
trait ImmutableArray[@specialized(Byte) T] { outer =>
  def apply(index: Int): T
  def length: Int
  def slice(from: Int, until: Int): ImmutableArray[T]
  def toIndexedSeq: IndexedSeq[T]
}

object ImmutableArray {

  private type IA[T] = ImmutableArray[T]

  /**
   * Constructs a new instance of <code>ImmutableArray</code>, wrapping an existing array.
   *
   * The existing array is <it>not</it> defensively copied by this method.  It should be defensively copied by the
   * caller if there is any chance of it being modified outside of this method.
   *
   * @param array array to wrap
   * @tparam T type of the array elements
   * @return wrapped array
   */
  def apply[T](array: Array[T]): IA[T] = new BasicIA[T](array)

  /**
   * Constructs a new instance of <code>ImmutableArray</code> from a sequence of elements.
   *
   * @param items sequence of elements
   * @tparam T type of the elements
   * @return new <code>ImmutableArray</code>
   */
  def apply[T:ClassTag](items: T*): IA[T] = apply[T](items.toArray)

  private def doSlice[T](ia: IA[T], array: Array[T], from: Int, until: Int, offset: Int = 0): IA[T] = {
    val maxFrom = if (ia.length > 0) ia.length - 1 else 0
    if (from > until) throw new IllegalArgumentException("from > until")
    if (from < 0) throw new IllegalArgumentException("from < 0")
    if (from > maxFrom) throw new IllegalArgumentException(s"from was $from; cannot be > $maxFrom")
    if (until > ia.length) throw new IllegalArgumentException(s"until was $until; cannot be > ${ia.length}")
    new SlicedIA[T](array, from + offset, until + offset)
  }

  /** Abstract ImmutableArray, only providing toString(). */
  private abstract class AbstractIA[T] extends IA[T] {
    override def toString: String = {
      if (length == 0) {
        "ImmutableArray()"
      } else {
        val sb = new StringBuilder
        sb.append("ImmutableArray(")
        var i: Int = 0
        while (i <= 10 && i < length) {
          sb.append(this(i))
          if (i < length - 1) sb.append(", ")
          i += 1
        }
        if (length > 10) sb.append("...")
        sb.append(")")
        sb.toString
      }
    }
    def toIndexedSeq: IndexedSeq[T] = new IndexedSeqIA[T](this)
  }

  /** Basic ImmutableArray, with no slicing over the underlying array. */
  private final class BasicIA[T](array: Array[T]) extends AbstractIA[T] {
    def length: Int = array.length
    def apply(index: Int): T = array(index)
    def slice(from: Int, until: Int): IA[T] = doSlice[T](this, array, from, until)
  }

  /** Sliced ImmutableArray, which has an offset into its underlying array. */
  private final class SlicedIA[T](array: Array[T], from: Int, until: Int) extends AbstractIA[T] {
    val length: Int = until - from
    def apply(index: Int): T = { checkIndex(index); array(index + from) }
    def slice(from: Int, until: Int): IA[T] = doSlice[T](this, array, from, until, this.from)
    private def checkIndex(i: Int): Unit = if (i < 0 || i >= length) throwIobe(i)
    private def throwIobe(i: Int): Unit = throw new IndexOutOfBoundsException(s"invalid index $i; range [0, $length)")
  }

  /** ImmutableArray as an IndexedSeq. */
  private final class IndexedSeqIA[T](ia: IA[T]) extends IndexedSeq[T] {
    def apply(index: Int): T = ia(index)
    def length: Int = ia.length
    override def slice(from: Int, until: Int): IndexedSeq[T] = ia.slice(from, until).toIndexedSeq
  }

}
