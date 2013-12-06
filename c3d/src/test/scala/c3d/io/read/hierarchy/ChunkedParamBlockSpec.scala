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


class ChunkedParamBlockSpec extends FunSpec with C3DFileSource {

  describe("A ChunkedParamBlock") {

    it("fetches the correct number of groups and parameters") {
      val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = ChunkedParamBlock(c3dBlock)
      assert(pb.length === 42)
    }

    it("fetches locks") {
      val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = ChunkedParamBlock(c3dBlock)
      val expected = List(false, false, false, false, false, false, false, false, false, false, false, false, false,
                          false, false, false, false, false, false, false, false, false, false, false, false, false,
                          false, false, false, false, false, false, false, false, false, true, true, true, true, true,
                          true, true)
      assert(pb.map(_.isLocked) === expected)
    }

    it("fetches group / parameter flags") {
      val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = ChunkedParamBlock(c3dBlock)
      val expected = List(true, true, true, false, false, false, false, false, false, false, false, false, false,
                          false, false, false, false, false, true, false, false, false, true, false, false, false,
                          false, false, false, false, false, false, false, false, false, false, false, false, false,
                          false, false, false)
      assert(pb.map(_.isGroup) === expected)
    }

    it("fetches group IDs") {
      val expected = List(1, 2, 3, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 1, 5, 5, 5, 5,
        5, 3, 3, 1, 1, 1, 1, 1, 1, 2, 2)
      val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = ChunkedParamBlock(c3dBlock)
      assert(pb.map(_.groupId) === expected)
    }

    it("fetches names") {
      val expected = List("POINT", "ANALOG", "FORCE_PLATFORM", "DESCRIPTIONS", "X_SCREEN", "Y_SCREEN", "LABELS",
                          "DESCRIPTIONS", "SCALE", "GEN_SCALE", "OFFSET", "UNITS", "USED", "TYPE", "CORNERS", "ORIGIN",
                          "CHANNEL", "ZERO", "FPLOC", "OBJ", "MAX", "INT", "SUBJECT", "NAME", "NUMBER", "PROJECT",
                          "LABELS", "WEIGHT", "HEIGHT", "GENDER", "DATE_OF_BIRTH", "TARGET_RADIUS", "TRANSLATION",
                          "ROTATION", "UNITS", "USED", "FRAMES", "SCALE", "DATA_START", "RATE", "USED", "RATE")
      val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
      val pb = ChunkedParamBlock(c3dBlock)
      assert(pb.map(_.name) === expected)
    }

  }

}
