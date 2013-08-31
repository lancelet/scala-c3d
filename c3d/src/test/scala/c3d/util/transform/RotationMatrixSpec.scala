package c3d.util.transform

import c3d.Vec3D
import scala.collection.immutable._
import org.scalatest.FunSpec

class RotationMatrixSpec extends FunSpec {
  
  describe("RotationMatrix") {

    it("should be correctly constructed from basis vectors") {
      val xHat = Vec3D(0, 0, 1)
      val yHat = Vec3D(1, 0, 0)
      val zHat = Vec3D(0, 1, 0)
      val m = RotationMatrix.fromBasisVectors(xHat, yHat, zHat)
      val q = Vec3D(1, 0, 0.5f)
      val qPrimeExpected = Vec3D(0.5f, 1, 0)
      assert(m(q) === qPrimeExpected)
    }
    
  }

}
