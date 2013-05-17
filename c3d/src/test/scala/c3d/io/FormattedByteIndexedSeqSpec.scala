package c3d.io

import scala.collection.immutable._
import org.scalatest.FunSpec
import Util.b

class FormattedByteIndexedSeqSpec extends FunSpec {

  describe("FormattedByteIndexedSeq") {

    def testSeq: IndexedSeq[Byte] = { 
      val iis = IndexedSeq[Int](
        0xC3, 0x7B, 0x14, 0xC0, // Intel float -2.3200538f
        0x0C, 0xFF,             // Intel signed int -244
        0x36, 0x07,             // Intel signed int 1846
        0x0C, 0xFF,             // Intel unsigned int 65292
        0x36, 0x07              // Intel unsigned int 1846
      )
      iis map (b(_))
    }
    def testfb: FormattedByteIndexedSeq = new FormattedByteIndexedSeq(testSeq, BinaryFormat.Intel)

    it("should allow access to the correct BinaryFormat") {
      assert(testfb.binaryFormat === BinaryFormat.Intel)
    }

    it("should correctly read bytes") {
      assert(testfb(2) === b(0x14))
    }

    it("should have the correct length") {
      assert(testfb.length === 12)
    }

    it("should allow slicing to the same type") {
      assert(testfb.slice(0, testfb.length).isInstanceOf[FormattedByteIndexedSeq])
    }

    it("should correctly read signed integers") {
      assert(testfb.intAt(4) === -244)
      assert(testfb.intAt(6) === 1846)
    }

    it("should correctly read unsigned integers") {
      assert(testfb.uintAt(8) === 65292)
      assert(testfb.uintAt(10) === 1846)
    }

    it("should correctly read floats") {
      assert(testfb.floatAt(0) === -2.3200538f)
    }

  }

}
