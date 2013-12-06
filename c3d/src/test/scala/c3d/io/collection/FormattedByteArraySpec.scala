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
import c3d.ProcessorType
import c3d.io.Util.b

class FormattedByteArraySpec extends FunSpec {

  import FormattedByteArray._   // implicit .forProcessor() method

  describe("A FormattedByteArray") {

    it("supports Intel:float") {
      val f = ImmutableArray(b(0xC3), b(0x7B), b(0x14), b(0xC0)).forProcessor(ProcessorType.Intel)
      assert(f.floatAt(0) === -2.3200538f)
    }

    it("supports Intel:int") {
      val f = ImmutableArray(b(0x0C), b(0xFF), b(0x36), b(0x07), b(0xC2), b(0x01)).forProcessor(ProcessorType.Intel)
      assert(f.intAt(0) === -244)
      assert(f.intAt(2) === 1846)
      assert(f.intAt(4) ===  450)
    }

    it("supports Intel:uint") {
      val f = ImmutableArray(b(0x0C), b(0xFF), b(0x36), b(0x07), b(0xC2), b(0x01)).forProcessor(ProcessorType.Intel)
      assert(f.uintAt(0) === 65292)
      assert(f.uintAt(2) ===  1846)
      assert(f.uintAt(4) ===   450)
    }

    it("supports DEC:float") {
      val f = ImmutableArray(b(0xAA), b(0x3E), b(0xAB), b(0xAA)).forProcessor(ProcessorType.DEC)
      assert(f.floatAt(0) === 0.083333336f)
    }

    it("supports DEC:int") {
      val f = ImmutableArray(b(0x0C), b(0xFF), b(0x36), b(0x07), b(0xC2), b(0x01)).forProcessor(ProcessorType.DEC)
      assert(f.intAt(0) === -244)
      assert(f.intAt(2) === 1846)
      assert(f.intAt(4) ===  450)
    }

    it("supports DEC:uint") {
      val f = ImmutableArray(b(0x0C), b(0xFF), b(0x36), b(0x07), b(0xC2), b(0x01)).forProcessor(ProcessorType.DEC)
      assert(f.uintAt(0) === 65292)
      assert(f.uintAt(2) ===  1846)
      assert(f.uintAt(4) ===   450)
    }

    it("supports SGI:float") {
      val f = ImmutableArray(b(0x3D), b(0xAA), b(0xAA), b(0xAB)).forProcessor(ProcessorType.SGIMIPS)
      assert(f.floatAt(0) === 0.083333336f)
    }

    it("supports SGI:int") {
      val f = ImmutableArray(b(0xFF), b(0x0C), b(0x07), b(0x36), b(0x01), b(0xC2)).forProcessor(ProcessorType.SGIMIPS)
      assert(f.intAt(0) === -244)
      assert(f.intAt(2) === 1846)
      assert(f.intAt(4) ===  450)
    }

    it("supports SGI:uint") {
      val f = ImmutableArray(b(0xFF), b(0x0C), b(0x07), b(0x36), b(0x01), b(0xC2)).forProcessor(ProcessorType.SGIMIPS)
      assert(f.uintAt(0) === 65292)
      assert(f.uintAt(2) ===  1846)
      assert(f.uintAt(4) ===   450)
    }

  }

}
