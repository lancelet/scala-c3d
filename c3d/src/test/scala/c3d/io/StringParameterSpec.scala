package c3d.io

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import org.scalatest.FunSpec
import c3d.{C3D, Parameter}

class StringParameterSpec extends FunSpec with C3DFileSource {

  describe("StringParameter") {

    it("should allow access to a parameter containing a single string") {
      val c3d = C3D.read(Sample08.EB015PI).getOrElse(fail())
      val unitsCharParam = c3d.getParameter[Char]("POINT", "UNITS").getOrElse(fail())
      val unitsStringParam = StringParameter(unitsCharParam)
      assert(unitsStringParam.name === "UNITS")
      assert(unitsStringParam.description === "  Distance measurement units")
      assert(unitsStringParam.isLocked === false)
      assert(unitsStringParam.dimensions === IndexedSeq(1))
      assert(unitsStringParam.data === IndexedSeq("mm  "))
      assert(unitsStringParam.apply(IndexedSeq(0)) === "mm  ")
      assert(unitsStringParam.parameterType === Parameter.Type.String)
    }

    it("should allow access to a parameter containing multiple strings") {
      val c3d = C3D.read(Sample08.EB015PI).getOrElse(fail())
      val ptDescrCharParam = c3d.getParameter[Char]("POINT", "DESCRIPTIONS").getOrElse(fail())
      val ptDescrStringParam = StringParameter(ptDescrCharParam)
      assert(ptDescrStringParam.name === "DESCRIPTIONS")
      assert(ptDescrStringParam.description === "  Point descriptions")
      assert(ptDescrStringParam.isLocked === false)
      assert(ptDescrStringParam.dimensions === IndexedSeq(20))
      assert(ptDescrStringParam.data === IndexedSeq("DIST/LAT FOOT", "INSTEP", "PROX LAT FOOT", "SHANK", "SHANK", 
        "SHANK", "SHANK", "ANKLE", "KNEE", "DISTAL FOOT", "*", "*", "*", "*", "*", "*", "*", "*", "*", "TARGET").
        map( s => f"$s%-32s" ))
      assert(ptDescrStringParam.apply(IndexedSeq(7)) === f"${"ANKLE"}%-32s")
      assert(ptDescrStringParam.parameterType === Parameter.Type.String)
    }

  }

}
