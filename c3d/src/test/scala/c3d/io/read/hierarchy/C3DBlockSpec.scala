/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import org.scalatest.FunSpec
import c3d.io.C3DFileSource
import c3d.io.Util.b
import c3d.io.collection.ImmutableArray
import c3d.ProcessorType

class C3DBlockSpec extends FunSpec with C3DFileSource {

  describe("C3DBlock") {

    it("identifies the magic byte") {
      val s1 = C3DBlock("test valid",   ImmutableArray(b(0x01), b(0x50)))
      val s2 = C3DBlock("test invalid", ImmutableArray(b(0x01), b(0x49)))
      assert(s1.hasMagicByte === true)
      assert(s2.hasMagicByte === false)
    }

    it("extracts the parameter block") {
      val s = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = s.paramBlock
      assert(pb.length       === (9 * 512))
      assert(pb(0)           === b(0x01))
      assert(pb(1)           === b(0x50))
      assert(pb(2)           === b(0x09))
      assert(pb(3)           === b(0x54))
      assert(pb(9 * 512 - 1) === b(0x00))
    }

    it("extracts the processor type") {
      val intel1 = C3DBlock("EB015PI", Sample01.EB015PI)
      val intel2 = C3DBlock("EB015PR", Sample01.EB015PR)
      val sgi1   = C3DBlock("EB015SI", Sample01.EB015SI)
      val sgi2   = C3DBlock("EB015SR", Sample01.EB015SR)
      val dec1   = C3DBlock("EB015VI", Sample01.EB015VI)
      val dec2   = C3DBlock("EB015VR", Sample01.EB015VR)
      assert(intel1.processorType === ProcessorType.Intel)
      assert(intel2.processorType === ProcessorType.Intel)
      assert(sgi1.processorType   === ProcessorType.SGIMIPS)
      assert(sgi2.processorType   === ProcessorType.SGIMIPS)
      assert(dec1.processorType   === ProcessorType.DEC)
      assert(dec2.processorType   === ProcessorType.DEC)
    }

  }

}
