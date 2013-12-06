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


class GroupSpec extends FunSpec with C3DFileSource {

  private def testGroupBlocks: IndexedSeq[Group] = {
    val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
    val pb = ChunkedParamBlock(c3dBlock)
    pb.filter(_.isGroup).map(Group(_))
  }

  describe("A Group") {

    it("fetches locks") {
      val expected = List(false, false, false, false, false)
      assert(testGroupBlocks.map(_.isLocked) === expected)
    }

    it("fetches group IDs") {
      val expected = List(1, 2, 3, 4, 5)
      assert(testGroupBlocks.map(_.groupId) === expected)
    }

    it("fetches names") {
      val expected = List("POINT", "ANALOG", "FORCE_PLATFORM", "FPLOC", "SUBJECT")
      assert(testGroupBlocks.map(_.name) === expected)
    }

    it("fetches descriptions") {
      val expected = List("3-D point parameters", "Analog data parameters", "Force platform parameters",
                          "FP LOC PARAMETERS", "Subject Parameters")
      assert(testGroupBlocks.map(_.description) === expected)
    }

  }

}
