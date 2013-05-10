package c3d.io

import org.scalatest.FunSpec
import c3d.ProcessorType
import Util.b

class BinaryFormatSpec extends FunSpec {

  describe("BinaryFormat") {

    it("should read Intel floating point numbers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.Intel)
      assert(bf.bytesToFloat(b(0xC3), b(0x7B), b(0x14), b(0xC0)) === -2.3200538f)
    }

    it("should read Intel signed integers (positive and negative)") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.Intel)
      assert(bf.bytesToInt(b(0x0C), b(0xFF)) === -244)
      assert(bf.bytesToInt(b(0x36), b(0x07)) === 1846)
    }

    it("should read Intel unsigned integers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.Intel)
      assert(bf.bytesToUInt(b(0x0C), b(0xFF)) === 65292)
      assert(bf.bytesToUInt(b(0x36), b(0x07)) === 1846)
    }

  }

}
