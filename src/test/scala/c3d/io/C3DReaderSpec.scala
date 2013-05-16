package c3d.io

import c3d.C3D
import org.scalatest.FunSpec
import scala.collection.immutable._
import Util.b

class C3DReaderSpec extends FunSpec with C3DFileSource {

  describe("C3DReader") {

    import C3DReader._

    it("should identify the magic byte correctly") {
      assert(hasMagicByte(IndexedSeq(b(0x01), b(0x50))) === true)
      assert(hasMagicByte(IndexedSeq(b(0x01), b(0x49))) === false)
    }

    it("should extract the parameter section blocks correctly") {
      paramSectionIndexedSeq(Sample08.EB015PI).fold(
        error => fail(),
        pb => {
          assert(pb.length === (9 * 512))
          assert(pb(0)           === b(0x01))
          assert(pb(1)           === b(0x50))
          assert(pb(2)           === b(0x09))
          assert(pb(3)           === b(0x54))
          assert(pb(9 * 512 - 1) === b(0x00))
        }
      )
    }

    it("should correctly read a C3D file") {
      // successful reading
      val c3dV = C3DReader.read(Sample08.EB015PI)
      assert(c3dV.isSuccess)
      val c3d: C3D = c3dV.getOrElse(fail())

      // check group and parameter names
      val groupNames = Set("POINT", "ANALOG", "FORCE_PLATFORM", "FPLOC", "SUBJECT")
      val pointNames = Set("DESCRIPTIONS", "X_SCREEN", "Y_SCREEN", "LABELS", "UNITS", "USED", "FRAMES", "SCALE", 
        "DATA_START", "RATE")
      val analogNames = Set("LABELS", "DESCRIPTIONS", "SCALE", "GEN_SCALE", "OFFSET", "UNITS", "USED", "RATE")
      val fpNames = Set("USED", "TYPE", "CORNERS", "ORIGIN", "CHANNEL", "ZERO", "TRANSLATION", "ROTATION")
      val fplocNames = Set("OBJ", "MAX", "INT")
      val subjNames = Set("NAME", "NUMBER", "PROJECT", "WEIGHT", "HEIGHT", "GENDER", "DATE_OF_BIRTH", "TARGET_RADIUS")
      // group and parameter names
      assert(c3d.groups.map(_.name) === groupNames)
      assert(c3d.groups.find(_.name == "POINT").get.parameters.map(_.name) === pointNames)
      assert(c3d.groups.find(_.name == "ANALOG").get.parameters.map(_.name) === analogNames)
      assert(c3d.groups.find(_.name == "FORCE_PLATFORM").get.parameters.map(_.name) === fpNames)
      assert(c3d.groups.find(_.name == "FPLOC").get.parameters.map(_.name) === fplocNames)
      assert(c3d.groups.find(_.name == "SUBJECT").get.parameters.map(_.name) === subjNames)
    }

  }

}
