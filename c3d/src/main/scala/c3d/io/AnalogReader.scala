package c3d.io

import scala.collection.immutable._
import c3d._

private[io] final case class AnalogReader(parameterSection: ParameterSection, dataSection: FormattedByteIndexedSeq)
  extends Analog with DataReaderBase {
  
  
  def channels: IndexedSeq[AnalogChannel] = ReadAnalogChannelIndexedSeq    
  def getChannelByName(name: String): Option[AnalogChannel] = channelMap.get(name.trim.toUpperCase)
  def samplingRate: Float = rp.analogRate
  def totalSamples: Int = DataStats.totalAnalogSamples
 
  
  private lazy val channelMap: Map[String, AnalogChannel] = channels.map(c => (c.name.trim.toUpperCase, c)).toMap
  
  
  private final case class ReadAnalogChannel(channelIndex: Int) extends AnalogChannel {

    private val scale: Float = rp.analogGenScale * rp.analogScale(channelIndex)
    private val offset: Float = rp.analogOffset(channelIndex)

    def name: String = rp.analogLabels(channelIndex)
    def description: String = rp.analogDescriptions(channelIndex)
    def length: Int = DataStats.totalAnalogSamples
    def apply(index: Int): Float = scale * (ItemReader.dataReader(getDataByteIndex(index)) - offset)

    private def getDataByteIndex(sampleIndex: Int): Int = {
      assert((sampleIndex >= 0) && (sampleIndex < length), s"sampleIndex must satisfy: 0 <= sampleIndex < $length")
      val closest3DFrame: Int = sampleIndex / DataStats.analogSamplesPer3DFrame
      val offsetWithinFrame: Int = sampleIndex % DataStats.analogSamplesPer3DFrame
      (
        closest3DFrame * DataStats.dataStride +      // jump to closest 3D frame
        DataStats.pointBlockSize +                   // skip over 3D point data for this frame
        offsetWithinFrame * DataStats.analogStride + // skip over any preceding analog blocks within the frame
        channelIndex * DataStats.dataItemSize        // find the correct channel
        )
    }

  }
  
  
  private final object ReadAnalogChannelIndexedSeq extends IndexedSeq[AnalogChannel] {
    def apply(index: Int): AnalogChannel = ReadAnalogChannel(index)
    def length: Int = rp.analogUsed
  }
  

}


