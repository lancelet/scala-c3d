package c3d.io

import c3d.Vec3D
import scala.collection.immutable._
import org.scalatest.FunSpec

class RotMatrixSpec extends FunSpec {
  
  describe("RotMatrix") {

    it("should be correctly constructed from basis vectors") {
      val xHat = Vec3D(0, 0, 1)
      val yHat = Vec3D(1, 0, 0)
      val zHat = Vec3D(0, 1, 0)
      val m = RotMatrix.fromBasisVectors(xHat, yHat, zHat)
      val q = Vec3D(1, 0, 0.5f)
      val qPrimeExpected = Vec3D(0.5f, 1, 0)
      assert(m(q) === qPrimeExpected)
    }
    
  }

}