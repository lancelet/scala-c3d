package c3d.util.transform

import scala.annotation.tailrec
import scala.collection.immutable._
import c3d._

/**
 * A Point which is found as the average of a cloud of points.
 */
final case class AveragePoint (
  name: String,
  description: String,
  pointCloud: IndexedSeq[Point]
) extends Point {
  def rate: Float = pointCloud.head.rate
  def length: Int = pointCloud.head.length
  def apply(i: Int): Option[Vec3D] = {
    @tailrec def avgPt(x: Float, y: Float, z: Float, curPtIndex: Int): Option[Vec3D] = {
      if (curPtIndex == nPoints) {
        Some(Vec3D(x / nFloat, y / nFloat, z / nFloat))
      } else {
        pointCloud(curPtIndex).apply(i) match {
          case None    => None
          case Some(v) => avgPt(x + v.x, y + v.y, z + v.z, curPtIndex + 1)
        }
      }
    }
    avgPt(0.0f, 0.0f, 0.0f, 0)
  }
  private [this] val nPoints: Int = pointCloud.length
  private [this] val nFloat: Float = nPoints.toFloat
  lazy val asMarker: Marker = AveragePoint.AveragePointMarker(this)
}

object AveragePoint {
  
  private final case class AveragePointMarker(ap: AveragePoint) extends Marker {
    def name: String        = ap.name
    def description: String = ap.description
    def rate: Float         = ap.rate
    def offset: Int         = ap.indexWhere(_.isDefined)
    def length: Int         = ap.lastIndexWhere(_.isDefined) - offset + 1
    @tailrec def apply(index: Int): Vec3D = {
      ap(index + offset) match {
        case Some(x) => x
        case None    => apply(index - 1)
      }
    }
  }
  
}