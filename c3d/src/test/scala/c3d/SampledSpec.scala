package c3d

import scala.collection.immutable._
import org.scalatest.FunSpec
import c3d.{Vec3D => V}

class SampledSpec extends FunSpec {
  
  describe("Sampled") {

    it("should be able to re-sample a sequence of floats that does divide evenly") {
      val s0 = SIndexedSeq(IndexedSeq(1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f), 9.0f)
      val s1 = s0.resampleByAveraging(3)
      assert(s1.rate   === 3.0f)
      assert(s1.length === 3)
      assert(s1 === IndexedSeq(2f, 5f, 8f))
    }    
    
    it("should be able to re-sample a sequence of floats that does not divide evenly") {
      val s0 = SIndexedSeq(IndexedSeq(1f, 2f, 3f, 4f, 5f, 6f, 7f, 8f, 9f, 10f), 9.0f)
      val s1 = s0.resampleByAveraging(3)
      assert(s1.rate   === 3.0f)
      assert(s1.length === 4)
      assert(s1 === IndexedSeq(2f, 5f, 8f, 10f))
    }
    
    it("should be able to re-sample a sequence of Vec3Ds") {
      val s0 = SIndexedSeq(IndexedSeq(V(1,2,3), V(4,5,6), V(7,8,9), V(3,2,3), V(5,3,2)), 9.0f)
      val s1 = s0.resampleByAveraging(3)
      assert(s1.rate   === 3.0f)
      assert(s1.length === 2)
      assert(s1 === IndexedSeq(V(4,5,6), V(4,2.5f,2.5f)))
    }
    
  }
  
}