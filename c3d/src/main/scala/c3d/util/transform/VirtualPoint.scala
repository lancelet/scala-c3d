package c3d.util.transform

import scala.annotation.tailrec
import scala.collection.immutable._
import c3d._

final case class VirtualPoint (
  name: String,
  description: String,
  referenceOrigin: Vec3D,
  referenceCloud: IndexedSeq[Vec3D],
  trackedPoints: IndexedSeq[Point]
) extends Point {
  private def vec3DIsNonNaN(v: Vec3D): Boolean = (!v.x.isNaN) && (!v.y.isNaN) && (!v.z.isNaN)
  require(referenceCloud.forall(vec3DIsNonNaN(_)), "reference cloud points were NaN")
  def rate: Float = trackedPoints.head.rate
  def length: Int = trackedPoints.head.length
  def apply(i: Int): Option[Vec3D] = {
    val markerPtOpt: IndexedSeq[Option[Vec3D]] = trackedPoints.map(_(i))
    if (markerPtOpt.forall(_.isDefined)) {
      val markerPoints: IndexedSeq[Vec3D] = markerPtOpt.map(_.get)
      val transform: XForm = Veldpaus.veldpaus(referenceCloud zip markerPoints)
      Some(transform(referenceOrigin))
    } else {
      None
    }
  }
  lazy val asMarker: Marker = VirtualPoint.VirtualPointMarker(this)
}

object VirtualPoint {
  private final case class VirtualPointMarker(vp: VirtualPoint) extends Marker {
    def name: String        = vp.name
    def description: String = vp.description
    def rate: Float         = vp.rate
    def offset: Int         = vp.indexWhere(_.isDefined)
    def length: Int         = vp.lastIndexWhere(_.isDefined) - offset + 1
    @tailrec def apply(index: Int): Vec3D = {
      val ptOpt = vp(index + offset)
      if (ptOpt.isDefined) ptOpt.get else apply(index - 1)
    }
  }  
}
