package c3d.io

import scala.collection.immutable._
import org.scalatest.FunSpec

class RotMatrixSpec extends FunSpec {
  
  describe("RotMatrix") {

    it("should be correctly constructed from basis vectors") {
      val xHat = DefaultVec3D(0, 0, 1)
      val yHat = DefaultVec3D(1, 0, 0)
      val zHat = DefaultVec3D(0, 1, 0)
      val m = RotMatrix.fromBasisVectors(xHat, yHat, zHat)
      val q = DefaultVec3D(1, 0, 0.5f)
      val qPrimeExpected = DefaultVec3D(0.5f, 1, 0)
      assert(m(q) === qPrimeExpected)
    }
    
  }

}