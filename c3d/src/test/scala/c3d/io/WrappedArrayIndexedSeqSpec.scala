package c3d.io

import org.scalatest.FunSpec

class WrappedArrayIndexedSeqSpec extends FunSpec {

  private def wrap123: WrappedArrayIndexedSeq[Int] = {
    val array = Array[Int](1, 2, 3)
    WrappedArrayIndexedSeq[Int](array)
  }

  describe("A WrappedArrayIndexedSeq") {

    it("should compute length correctly") {
      assert(wrap123.length === 3)
    }

    it("should permit access to all elements") {
      val wrap = wrap123
      assert(wrap(0) === 1)
      assert(wrap(1) === 2)
      assert(wrap(2) === 3)
    }

    it("should throw an IndexOutOfBoundsException if the index is out of range") {
      val wrap = wrap123
      intercept[IndexOutOfBoundsException] { wrap(-1) }
      intercept[IndexOutOfBoundsException] { wrap(3)  }
    }

    it("should slice correctly") {
      val w12 = wrap123.slice(0, 2)
      assert(w12.length === 2)
      assert(w12(0) === 1)
      assert(w12(1) === 2)

      val w23 = wrap123.slice(1, 3)
      assert(w23.length === 2)
      assert(w23(0) === 2)
      assert(w23(1) === 3)

      val w00 = wrap123.slice(2, 2)
      assert(w00.length === 0)
    }

    it("should be able to slice twice") {
      val w2 = wrap123.slice(1, 3).slice(1, 2)
      assert(w2.length === 1)
      assert(w2(0) === 3)
    }

    it("should fail to construct if from or until are invalid") {
      val array = Array(1, 2, 3)
      intercept[SliceException] { WrappedArrayIndexedSeq(array, -1, 3) }  // from < 0
      intercept[SliceException] { WrappedArrayIndexedSeq(array,  4, 3) }  // from > length
      intercept[SliceException] { WrappedArrayIndexedSeq(array,  2, 1) }  // until <= from
      intercept[SliceException] { WrappedArrayIndexedSeq(array,  1, 4) }  // until > length
    }

    it("should construct even if array.length == 0") {
      val array = Array.empty[Int]
      val wrap = WrappedArrayIndexedSeq(array, 0, 0)
      assert(wrap.length === 0)
    }

  }

}
