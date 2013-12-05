package c3d.io

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable.{Map => MMap}
import c3d._

private[io] final case class PointsReader(parameterSection: ParameterSection, dataSection: FormattedByteIndexedSeq)
  extends Points with DataReaderBase {

  def points: IndexedSeq[Point] = ReadPointsIndexedSeq
  def getPointByName(name: String): Option[Point] = nameMap.get(name.trim.toUpperCase)
  def getPointByDescription(description: String): Option[Point] = descriptionMap.get(description.trim.toUpperCase)
  def rate: Float = rp.pointRate
  def totalSamples: Int = rp.pointFrames  
  
  private lazy val nameMap: Map[String, Point] = {
    // keep only the first instance of any given name where a point is actually defined
    val mmap: MMap[String, Point] = MMap.empty[String, Point]
    for {
      p <- existingPoints
      name = p.name.trim.toUpperCase
      if (!mmap.contains(name))
    } mmap.put(name, p)
    Map.empty[String, Point] ++ mmap
  } 
    
  private lazy val descriptionMap: Map[String, Point] = {
    // keep only the first instance of any given description where a point is actually defined
    val mmap: MMap[String, Point] = MMap.empty[String, Point]
    for {
      p <- existingPoints
      description = p.description.trim.toUpperCase
      if (!mmap.contains(description))
    } mmap.put(description, p)
    Map.empty[String, Point] ++ mmap
  }

  // existingPoints have at least one defined sample
  private [this] val existingPoints: Seq[Point] = points.filter(_.exists(_.isDefined))
  
  private final case class ReadPoint(pointIndex: Int) extends Point {

    // in floating-point storage, 3D point coordinates have already been multiplied by the scaling factor
    private val scale: Float = if (DataStats.usesFloat) 1.0f else rp.pointScale
    
    def name: String = rp.pointLabels(pointIndex)
    def description: String = {
      // sometimes, the descriptions parameter isn't large enough to contain descriptions for all points
      if (rp.pointDescriptions.dimensions(0) > pointIndex) rp.pointDescriptions(pointIndex) else ""
    }
    def length: Int = rp.pointFrames
    def apply(index: Int): Option[Vec3D] = {
      val w4: Int = ItemReader.pointw4Reader(getDataByteIndex(index) + 3 * DataStats.dataItemSize)
      if (w4 == -1) {
        None
      } else {
        val r: Int => Float = ItemReader.dataReader
        val dbi: Int = getDataByteIndex(index)
        val x: Float = scale * r(dbi)
        val y: Float = scale * r(dbi + DataStats.dataItemSize)
        val z: Float = scale * r(dbi + (2 * DataStats.dataItemSize))
        Some(Vec3D(x, y, z))
      }
    }
    def rate: Float = rp.pointRate
    def asMarker: Marker = new Marker { // TODO: include marker gap filler
      def name: String = ReadPoint.this.name
      def description: String = ReadPoint.this.description
      def rate: Float = ReadPoint.this.rate
      val offset: Int = ReadPoint.this.indexWhere(_.isDefined)
      def length: Int = ReadPoint.this.lastIndexWhere(_.isDefined) - offset + 1
      @tailrec def apply(index: Int): Vec3D = {
        val ptOpt = ReadPoint.this.apply(index + offset)
        // for temporary gap filling, we just go backward through the Point data until we find a valid value
        if (ptOpt.isDefined) ptOpt.get else apply(index - 1)  // TODO: better gap filling
      }
    }
    
    private def getDataByteIndex(sampleIndex: Int): Int = {
      assert((sampleIndex >= 0) && (sampleIndex < length), s"sampleIndex must satisfy: 0 <= sampleIndex < $length")
      (sampleIndex * DataStats.dataStride) + (pointIndex * 4 * DataStats.dataItemSize)
    }
    
  }
  
  private final object ReadPointsIndexedSeq extends IndexedSeq[Point] {
    def apply(pointIndex: Int): Point = ReadPoint(pointIndex)
    def length: Int = rp.pointUsed
  }
    
}

