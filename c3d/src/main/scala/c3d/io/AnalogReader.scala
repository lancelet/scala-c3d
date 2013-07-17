package c3d.io

import scala.collection.immutable._
import c3d._

private[io] final case class AnalogReader(parameterSection: ParameterSection, dataSection: FormattedByteIndexedSeq)
  extends Analog {
  
  
  def channels: IndexedSeq[AnalogChannel] = ReadAnalogChannelIndexedSeq    
  def getChannelByName(name: String): Option[AnalogChannel] = channelMap.get(name.toUpperCase)
  def samplingRate: Float = rp.analogRate
  def totalSamples: Int = AnalogStats.totalAnalogSamples
 
  
  private lazy val channelMap: Map[String, AnalogChannel] = channels.map(c => (c.name.toUpperCase, c)).toMap
  
  
  private final val rp: RequiredParameters = parameterSection.requiredParameters  
  
  
  private final object AnalogStats {
    lazy val usesFloat: Boolean = rp.pointScale < 0.0
    lazy val dataItemSize: Int = if (usesFloat) 4 else 2
    lazy val analogSamplesPer3DFrame: Int = (rp.analogRate / rp.pointRate).toInt
    lazy val totalAnalogSamples: Int = analogSamplesPer3DFrame * rp.pointFrames
    lazy val pointBlockSize: Int = 4 * rp.pointUsed * dataItemSize
    lazy val analogStride: Int = rp.analogUsed * dataItemSize
    lazy val dataStride: Int = (rp.pointUsed * 4 + analogSamplesPer3DFrame * rp.analogUsed) * dataItemSize
  }

  
  private final case class ReadAnalogChannel(channelIndex: Int, dataReader: Int => Float) extends AnalogChannel {

    private val scale: Float = rp.analogGenScale * rp.analogScale(channelIndex)
    private val offset: Float = rp.analogOffset(channelIndex)

    def name: String = rp.analogLabels(channelIndex)
    def description: String = rp.analogDescriptions(channelIndex)
    def length: Int = AnalogStats.totalAnalogSamples
    def apply(index: Int): Float = scale * (dataReader(getDataByteIndex(index)) - offset)

    private def getDataByteIndex(sampleIndex: Int): Int = {
      assert((sampleIndex >= 0) && (sampleIndex < length), s"sampleIndex must satisfy: 0 <= sampleIndex < $length")
      val closest3DFrame: Int = sampleIndex / AnalogStats.analogSamplesPer3DFrame
      val offsetWithinFrame: Int = sampleIndex % AnalogStats.analogSamplesPer3DFrame
      (
        closest3DFrame * AnalogStats.dataStride +      // jump to closest 3D frame
        AnalogStats.pointBlockSize +                   // skip over 3D point data for this frame
        offsetWithinFrame * AnalogStats.analogStride + // skip over any preceding analog blocks within the frame
        channelIndex * AnalogStats.dataItemSize        // find the correct channel
        )
    }

  }

  
  private final object DataReader {

    private def floatR(index: Int): Float = dataSection.floatAt(index)
    private def intR(index: Int): Float = dataSection.intAt(index)
    private def uintR(index: Int): Float = dataSection.uintAt(index)
    
    val dataReader: Int => Float = {
      val usesFloat: Boolean = AnalogStats.usesFloat
      val isSigned: Boolean = rp.analogFormat == "SIGNED"
      if (usesFloat) floatR else (if (isSigned) intR else uintR)
    }

  }

  
  private final object ReadAnalogChannelIndexedSeq extends IndexedSeq[AnalogChannel] {
    def apply(index: Int): AnalogChannel = ReadAnalogChannel(index, DataReader.dataReader)
    def length: Int = rp.analogUsed
  }
  

}


