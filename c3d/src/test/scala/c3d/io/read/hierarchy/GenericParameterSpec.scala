/**
 *    __ __  __
 *   /    _)|  \
 *   \__ __)|__/  for SCALA!
 *
 * This file is part of the scala-c3d library.  This library is distributed under the Apache 2.0 license (ALv2).
 * Copyright (C) 2013 Dr Jonathan S Merritt.
 */

package c3d.io.read.hierarchy

import c3d.io.C3DFileSource
import org.scalatest.FunSpec


class GenericParameterSpec extends FunSpec with C3DFileSource {

  private def testPBs: IndexedSeq[GenericParameter] = {
    val c3dBlock = C3DBlock("EB015PI", Sample08.EB015PI)
    val pb = ChunkedParamBlock(c3dBlock)
    pb.filter(!_.isGroup).map(GenericParameter(_))
  }

  describe("A GenericParameter") {

    it("fetches locks") {
      val expected = List(false, false, false, false, false, false, false, false, false, false, false, false, false,
                          false, false, false, false, false, false, false, false, false, false, false, false, false,
                          false, false, false, false, true, true, true, true, true, true, true)
      assert(testPBs.map(_.isLocked) === expected)
    }

    it("fetches groupIDs") {
      val expected = List(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 1, 5, 5, 5, 5, 5, 3, 3, 1, 1,
                          1, 1, 1, 1, 2, 2)
      assert(testPBs.map(_.groupId) === expected)
    }

    it("fetches names") {
      val expected = List("DESCRIPTIONS", "X_SCREEN", "Y_SCREEN", "LABELS", "DESCRIPTIONS", "SCALE", "GEN_SCALE",
                          "OFFSET", "UNITS", "USED", "TYPE", "CORNERS", "ORIGIN", "CHANNEL", "ZERO", "OBJ", "MAX",
                          "INT", "NAME", "NUMBER", "PROJECT", "LABELS", "WEIGHT", "HEIGHT", "GENDER", "DATE_OF_BIRTH",
                          "TARGET_RADIUS", "TRANSLATION", "ROTATION", "UNITS", "USED", "FRAMES", "SCALE", "DATA_START",
                          "RATE", "USED", "RATE")
      assert(testPBs.map(_.name) === expected)
    }

    it("fetches descriptions") {
      val expected = List("  Point descriptions", "  Lab. axis along X-screen axis", "  Lab. axis along Y-screen axis",
                          "  Analog labels", "  Analog descriptions", "  Analog scale factors",
                          "  General scale factor", "  Analog offsets", "  Analog units", "  Number of FP's used",
                          "  Force platform type", "  Corner locations", "  FP relative to geometrical",
                          "  Analog channels used", "  Frame interval to have zero offset", "OBJ COORD",
                          "DIMENSION OF OBJ FOR FP", "C3D INTERVAL SAMPLING", "", "", "", "Point labels",
                          "UNITS=kg", "UNITS=mm", "M,F", "MONTH/DAY/YEAR", "SEGMENT ENDPOINT TARGET RADIUS",
                          "Location of center of force plate", "Transformation from PCS to LCS",
                          "  Distance measurement units", "* Number of points used", "* Number of video frames",
                          "* Point data scale factor", "* First record of video/analog data",
                          "* Video data frame rate", "* Number of analog channels used", "* Analog data frame rate")
      assert(testPBs.map(_.description) === expected)
    }

    it("fetches dimensions") {
      val expected = List(List(32, 20), List(2), List(2), List(4, 32), List(32, 32), List(32), List(1), List(32),
                          List(4, 32), List(1), List(2), List(3, 4, 2), List(3, 2), List(6, 2), List(2), List(3, 4, 2),
                          List(1), List(2), List(25), List(1), List(30), List(4, 48), List(1), List(1), List(1),
                          List(8), List(1), List(3, 2), List(3, 3, 2), List(4), List(1), List(1), List(1), List(1),
                          List(1), List(1), List(1))
      assert(testPBs.map(_.dimensions) === expected)
    }

    it("fetches types") {
      import c3d.Parameter.Type._
      val expected = List(Character, Character, Character, Character, Character, Float, Float, Integer, Character,
                          Integer, Integer, Float, Float, Integer, Integer, Float, Integer, Integer, Character,
                          Integer, Character, Character, Float, Float, Character, Character, Float, Float, Float,
                          Character, Integer, Integer, Float, Integer, Float, Integer, Float)
      assert(testPBs.map(_.paramType) === expected)
    }

  }

}
