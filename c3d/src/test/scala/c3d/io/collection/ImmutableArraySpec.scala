/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.collection

import org.scalatest.FunSpec
import org.scalautils.Equality
import c3d.io.Util.b

class ImmutableArraySpec extends FunSpec {


  /**
   * Equality tests.
   *
   * This functionality isn't really needed, except for testing.  Consequently, it's placed here rather than in the
   * source itself.
   */
  implicit def immutableArrayEq[T] = new Equality[ImmutableArray[T]] {
    def areEqual(a: ImmutableArray[T], b: Any): Boolean = {
      b match {
        case ia: ImmutableArray[T] => elementsEqual(a, ia)
        case _ => false
      }
    }
    private def elementsEqual(a: ImmutableArray[T], b: ImmutableArray[T]): Boolean = {
      if (a.length != b.length) return false
      var i: Int = 0
      while (i < a.length) { if (a(i) != b(i)) return false; i += 1 }
      return true
    }
  }

  describe("An ImmutableArray") {

    it ("can be empty") {
      val a = ImmutableArray()
      assert(a === ImmutableArray())
      assert(a.length === 0)
      assert(a.toString === "ImmutableArray()")
      intercept[IndexOutOfBoundsException] { a(0) }
    }

    it ("has element-wise equality (only for testing)") {
      assert(ImmutableArray(1, 2, 3) === ImmutableArray(1, 2, 3))
      assert(ImmutableArray(1, 2, 3) !== ImmutableArray(1, 2, 3, 4))
      assert(ImmutableArray(1, 2, 3) !== ImmutableArray(1, 2, 4))
    }

    it ("has a length") {
      assert(ImmutableArray().length === 0)
      assert(ImmutableArray(1, 2, 3).length === 3)
    }

    it ("accesses elements") {
      val ia = ImmutableArray[Byte](b(1), b(2), b(3))
      assert(ia(0) === 1)
      assert(ia(1) === 2)
      assert(ia(2) === 3)
    }

    it ("throws an IndexOutOfBoundsException for invalid indices") {
      val ia = ImmutableArray(1, 2, 3)
      intercept[IndexOutOfBoundsException] { ia(-1) }
      intercept[IndexOutOfBoundsException] { ia(3)  }
    }

    it ("can slice") {
      val ia = ImmutableArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val slice = ia.slice(2, 7) // 3, 4, 5, 6, 7
      assert(ia.slice(2, 7) === ImmutableArray(3, 4, 5, 6, 7))
    }

    it ("throws an IllegalArgumentException for slice when from > until") {
      val a = ImmutableArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      intercept[IllegalArgumentException] { a.slice(2, 0) }
    }

    it ("can slice twice") {
      val a = ImmutableArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val b = a.slice(2, 7) // 3, 4, 5, 6, 7
      val c = b.slice(1, 4) // 4, 5, 6
      assert(c === ImmutableArray(4, 5, 6))
    }

    it ("allows a zero-length slice") {
      val a = ImmutableArray(1, 2, 3, 4, 5)
      val b = a.slice(3, 3)
      assert(b === ImmutableArray())
    }

    it ("allows multiple slices of a zero length slice") {
      val a = ImmutableArray(1, 2, 3, 4, 5)
      val b = a.slice(3, 3)
      val c = b.slice(0, 0)
      val d = c.slice(0, 0)
      val e = d.slice(0, 0)
      assert(e === ImmutableArray())
    }

    it ("throws an IndexOutOfBoundsException for bad indices after slicing") {
      val a = ImmutableArray(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val b = a.slice(2, 7) // 3, 4, 5, 6, 7
      val c = b.slice(1, 4) // 4, 5, 6
      intercept[IndexOutOfBoundsException] { c(-1) }
      intercept[IndexOutOfBoundsException] { c(3)  }
    }

    it ("throws an IllegalArgumentException for negative slices") {
      val a = ImmutableArray(1, 2, 3, 4, 5)
      intercept[IllegalArgumentException] { a.slice(-2, 2) }
    }

    it ("throws an IllegalArgumentException for slice indices too large") {
      val a = ImmutableArray(1, 2, 3, 4, 5)
      intercept[IllegalArgumentException] { a.slice(2, 6) }
      intercept[IllegalArgumentException] { a.slice(6, 6) }
    }

  }

}
