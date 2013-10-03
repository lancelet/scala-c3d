package c3d.util.transform

import c3d.Vec3D
import c3d.Point
import c3d.Marker
import scala.collection.immutable._
import org.scalatest.FunSpec

class AveragePointSpec extends FunSpec {

  describe("AveragePoint") {
    
    it ("should correctly compute point averages") {
      
      val pt1 = new Point {
        def name: String = "pt1"
        def description: String = ""
        def rate: Float = 1.0f
        def asMarker: Marker = ???
        def length = data.length
        def apply(i: Int): Option[Vec3D] = data(i)
        private [this] val data: IndexedSeq[Option[Vec3D]] = IndexedSeq(
          Some(Vec3D(1.0f, 2.0f, 3.0f)),
          Some(Vec3D(4.0f, 5.0f, 6.0f))
        )
      }
      
      val pt2 = new Point {
        def name: String = "pt2"
        def description: String = ""
        def rate: Float = 1.0f
        def asMarker: Marker = ???
        def length = data.length
        def apply(i: Int): Option[Vec3D] = data(i)
        private [this] val data: IndexedSeq[Option[Vec3D]] = IndexedSeq(
          Some(Vec3D(7.0f, 3.0f, 1.0f)),
          Some(Vec3D(4.0f, 2.0f, 2.0f))
        )
      }
      
      val avgPt = AveragePoint("avg", "", IndexedSeq(pt1, pt2))
      val expected: IndexedSeq[Option[Vec3D]] = IndexedSeq(
        Some(Vec3D(4.0f, 2.5f, 2.0f)),
        Some(Vec3D(4.0f, 3.5f, 4.0f))
      )
      assert(avgPt === expected)
      
    }
    
  }
  
}