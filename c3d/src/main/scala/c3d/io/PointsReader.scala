package c3d.io

import scala.collection.immutable._
import c3d._

/**
 * TODO: Merge common elements from PointsReader and AnalogReader into a parent trait.
 */
private[io] final case class PointsReader(parameterSection: ParameterSection, dataSection: FormattedByteIndexedSeq)
  extends Points {

  private final val rp: RequiredParameters = parameterSection.requiredParameters
  
  private lazy val pointMap: Map[String, Point] = points.map(c => (c.name.trim.toUpperCase, c)).toMap
  
  private final case class ReadPoint(pointIndex: Int) extends Point {

    // in floating-point storage, 3D point coordinates have already been multiplied by the scaling factor
    private val scale: Float = if (PointStats.usesFloat) 1.0f else rp.pointScale
    
    def name: String = rp.pointLabels(pointIndex)
    def description: String = {
      // sometimes, the descriptions parameter isn't large enough to contain descriptions for all points
      if (rp.pointDescriptions.dimensions(0) > pointIndex) rp.pointDescriptions(pointIndex) else ""
    }
    def length: Int = rp.pointFrames
    def apply(index: Int): Option[Vec3D] = {
      val w4: Int = if (PointStats.usesFloat) {
        dataSection.floatAt(getDataByteIndex(index) + 3 * PointStats.dataItemSize).toInt
      } else {
        dataSection.intAt(getDataByteIndex(index) + 3 * PointStats.dataItemSize)
      }
      if (w4 == -1) {
        None
      } else {
        val r: Int => Float = DataReader.dataReader
        val dbi: Int = getDataByteIndex(index)
        val x: Float = scale * r(dbi)
        val y: Float = scale * r(dbi + PointStats.dataItemSize)
        val z: Float = scale * r(dbi + (2 * PointStats.dataItemSize))
        Some(DefaultVec3D(x, y, z))
      }
    }
   
    private def getDataByteIndex(sampleIndex: Int): Int = {
      assert((sampleIndex >= 0) && (sampleIndex < length), s"sampleIndex must satisfy: 0 <= sampleIndex < $length")
      (sampleIndex * PointStats.dataStride) + (pointIndex * 4 * PointStats.dataItemSize)
    }
  }
  
  private final object ReadPointsIndexedSeq extends IndexedSeq[Point] {
    def apply(pointIndex: Int): Point = ReadPoint(pointIndex)
    def length: Int = rp.pointUsed
  }
  
  private final object PointStats {
    lazy val usesFloat: Boolean = rp.pointScale < 0.0
    lazy val dataItemSize: Int = if (usesFloat) 4 else 2
    lazy val analogSamplesPer3DFrame: Int = (rp.analogRate / rp.pointRate).toInt
    lazy val pointBlockSize: Int = 4 * rp.pointUsed * dataItemSize
    lazy val dataStride: Int = (rp.pointUsed * 4 + analogSamplesPer3DFrame * rp.analogUsed) * dataItemSize
  }
  
  private final object DataReader {
    
    private def floatR(index: Int): Float = dataSection.floatAt(index)
    private def intR(index: Int): Float = dataSection.intAt(index)
    private def uintR(index: Int): Float = dataSection.uintAt(index)
    
    val dataReader: Int => Float = {
      val usesFloat: Boolean = PointStats.usesFloat
      val isSigned: Boolean = rp.analogFormat == "SIGNED"
      if (usesFloat) floatR else (if (isSigned) intR else uintR)
    }    
    
  }
  
  def points: IndexedSeq[Point] = ReadPointsIndexedSeq
  def getPointByName(name: String): Option[Point] = pointMap.get(name.trim.toUpperCase)
  def samplingRate: Float = rp.pointRate
  def totalSamples: Int = rp.pointFrames
}

