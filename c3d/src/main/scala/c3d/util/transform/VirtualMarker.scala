package c3d.util.transform

import scala.collection.immutable._
import c3d.Marker
import c3d.Vec3D

final case class VirtualMarker(
  name: String,
  description: String,
  referencePoint: Vec3D,
  referenceMarkerPoints: IndexedSeq[Vec3D],
  markers: IndexedSeq[Marker]
) extends Marker {

  val rate: Float = markers.head.rate
  require(markers.forall(_.rate == rate), "all markers must have the same sampling rate")
  
  val offset: Int = markers.map(_.offset).max
  val length: Int = markers.map(m => m.offset + m.length - offset).min + 1
  def apply(i: Int): Vec3D = {
    val markerPoints: IndexedSeq[Vec3D] = markers.map(m => markerPt(m, i))
    val transform: XForm = Veldpaus.veldpaus(referenceMarkerPoints zip markerPoints)
    transform(referencePoint)
  }
  
  private def markerPt(m: Marker, i: Int): Vec3D = m(offset - m.offset + i)
  
}