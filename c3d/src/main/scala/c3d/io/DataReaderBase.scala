package c3d.io

import scala.collection.immutable._
import c3d._

/**
 * Base trait for AnalogReader and PointsReader that provides some common functionality.
 */
private[io] trait DataReaderBase {
  
  def parameterSection: ParameterSection
  def dataSection: FormattedByteIndexedSeq
  
  protected final val rp: RequiredParameters = parameterSection.requiredParameters
  
  protected final object DataStats {
    lazy val usesFloat: Boolean = rp.pointScale < 0.0
    lazy val dataItemSize: Int = if (usesFloat) 4 else 2
    lazy val analogSamplesPer3DFrame: Int = (rp.analogRate / rp.pointRate).toInt
    lazy val totalAnalogSamples: Int = analogSamplesPer3DFrame * rp.pointFrames
    lazy val pointBlockSize: Int = 4 * rp.pointUsed * dataItemSize
    lazy val analogStride: Int = rp.analogUsed * dataItemSize
    lazy val dataStride: Int = (rp.pointUsed * 4 + analogSamplesPer3DFrame * rp.analogUsed) * dataItemSize
  }

  protected final object ItemReader {
    
    private def floatR(index: Int): Float = dataSection.floatAt(index)
    private def intR(index: Int): Float = dataSection.intAt(index).toFloat
    private def uintR(index: Int): Float = dataSection.uintAt(index).toFloat
    
    private def floatI(index: Int): Int = dataSection.floatAt(index).toInt
    private def intI(index: Int): Int = dataSection.intAt(index)
    
    val dataReader: Int => Float = {
      val isSigned: Boolean = rp.analogFormat == "SIGNED"
      if (DataStats.usesFloat) floatR else (if (isSigned) intR else uintR)
    }
    
    val pointw4Reader: Int => Int = {
      if (DataStats.usesFloat) floatI else intI
    }
    
  }
  
}