package c3d.io

import c3d.{C3D, Parameter}
import org.scalatest.FunSpec
import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz.std.AllInstances._
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

    it("should correctly read C3D file groups and parameters") {
      // successful reading
      val c3dV = C3DReader.read(Sample08.EB015PI)
      assert(c3dV.isSuccess)
      val c3d: C3D = c3dV.getOrElse(fail())

      // check group and parameter names
      val groupNames = Seq("POINT", "ANALOG", "FORCE_PLATFORM", "FPLOC", "SUBJECT")
      val pointNames = Seq("DESCRIPTIONS", "X_SCREEN", "Y_SCREEN", "LABELS", "UNITS", "USED", "FRAMES", "SCALE", 
        "DATA_START", "RATE")
      val analogNames = Seq("LABELS", "DESCRIPTIONS", "SCALE", "GEN_SCALE", "OFFSET", "UNITS", "USED", "RATE")
      val fpNames = Seq("USED", "TYPE", "CORNERS", "ORIGIN", "CHANNEL", "ZERO", "TRANSLATION", "ROTATION")
      val fplocNames = Seq("OBJ", "MAX", "INT")
      val subjNames = Seq("NAME", "NUMBER", "PROJECT", "WEIGHT", "HEIGHT", "GENDER", "DATE_OF_BIRTH", "TARGET_RADIUS")
      // group and parameter names
      assert(c3d.groups.map(_.name) === groupNames)
      assert(c3d.groups.find(_.name == "POINT").get.parameters.map(_.name) === pointNames)
      assert(c3d.groups.find(_.name == "ANALOG").get.parameters.map(_.name) === analogNames)
      assert(c3d.groups.find(_.name == "FORCE_PLATFORM").get.parameters.map(_.name) === fpNames)
      assert(c3d.groups.find(_.name == "FPLOC").get.parameters.map(_.name) === fplocNames)
      assert(c3d.groups.find(_.name == "SUBJECT").get.parameters.map(_.name) === subjNames)
    }

    it("should allow typed access to groups and parameters via getParameter") {
      val c3dV = C3DReader.read(Sample08.EB015PI)
      assert(c3dV.isSuccess)
      c3dV map { c3d =>
        // should fail to get a parameter of the wrong type
        assert(c3d.getParameter[Int]("Point", "x_screen").isEmpty, "should not find X_SCREEN as an Int")
        // should succeed if the parameter is the correct type
        val xScreenO = c3d.getParameter[Char]("Point", "x_screen")
        assert(xScreenO.isDefined, "X_SCREEN parameter should have been found")
        xScreenO map { xScreen =>
          assert(xScreen.name === "X_SCREEN")
          assert(xScreen.description === "  Lab. axis along X-screen axis")
          assert(xScreen.isLocked === false)
          assert(xScreen.dimensions === Seq(2))
          assert(xScreen.data === "+Y".toSeq)
          assert(xScreen.parameterType === Parameter.Type.Character)
        }
      }
    }

    it("should index multi-dimensional parameters correctly") {
      val c3dV = C3DReader.read(Sample08.EB015PI)
      val pl = c3dV.getOrElse(fail()).getParameter[Char]("POINT", "LABELS").getOrElse(fail())
      // check for an error if the sequence is the wrong size
      intercept[IllegalArgumentException] { pl(IndexedSeq(0, 0, 0)) }
      intercept[IllegalArgumentException] { pl(IndexedSeq(0)) }
      // first line
      assert(pl(IndexedSeq(0, 0)) === 'R')
      assert(pl(IndexedSeq(1, 0)) === 'F')
      assert(pl(IndexedSeq(2, 0)) === 'T')
      assert(pl(IndexedSeq(3, 0)) === '1')
      // last line
      assert(pl(IndexedSeq(0, 37)) === 'L')
      assert(pl(IndexedSeq(1, 37)) === 'S')
    }

    it("should read string parameters") {
      val c3d = C3DReader.read(Sample08.EB015PI).getOrElse(fail())
      // should fail on non-String parameters
      assert(c3d.getParameter[String]("POINT", "DATA_START").isEmpty)
      // should succeed on string parameters
      val pd = c3d.getParameter[String]("POINT", "DESCRIPTIONS").getOrElse(fail())
      assert(pd(IndexedSeq(1)).trim === "INSTEP")
    }

  }

}
