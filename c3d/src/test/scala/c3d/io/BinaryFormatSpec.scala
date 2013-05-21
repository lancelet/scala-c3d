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
      // little-endian
      assert(bf.bytesToInt(b(0x0C), b(0xFF)) === -244)
      assert(bf.bytesToInt(b(0x36), b(0x07)) === 1846)
    }

    it("should read Intel unsigned integers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.Intel)
      // little-endian
      assert(bf.bytesToUInt(b(0x0C), b(0xFF)) === 65292)
      assert(bf.bytesToUInt(b(0x36), b(0x07)) === 1846)
    }

    it("should read DEC floating point numbers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.DEC)
      assert(bf.bytesToFloat(b(0xAA), b(0x3E), b(0xAB), b(0xAA)) === 0.0833333f)
    }

    it("should read DEC signed integers (positive and negative)") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.DEC)
      // little-endian
      assert(bf.bytesToInt(b(0x0C), b(0xFF)) === -244)
      assert(bf.bytesToInt(b(0x36), b(0x07)) === 1846)
    }

    it("should read DEC unsigned integers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.DEC)
      // little-endian
      assert(bf.bytesToUInt(b(0x0C), b(0xFF)) === 65292)
      assert(bf.bytesToUInt(b(0x36), b(0x07)) === 1846)
    }

    it("should read SGI floating point numbers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.SGIMIPS)
      assert(bf.bytesToFloat(b(0x3D), b(0xAA), b(0xAA), b(0xAB)) === 0.0833333f)
    }

    it("should read SGI signed integers (positive and negative)") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.SGIMIPS)
      // big-endian
      assert(bf.bytesToInt(b(0xFF), b(0x0C)) === -244)
      assert(bf.bytesToInt(b(0x07), b(0x36)) === 1846)
    }

    it("should read SGI unsigned integers") {
      val bf = BinaryFormat.fromProcessorType(ProcessorType.SGIMIPS)
      // big-endian
      assert(bf.bytesToUInt(b(0xFF), b(0x0C)) === 65292)
      assert(bf.bytesToUInt(b(0x07), b(0x36)) === 1846)
    }

  }

}
