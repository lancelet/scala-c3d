package c3d.io

import scala.collection.immutable._
import c3d._
import scala.reflect.runtime.universe._

private[io] final case class PlatformReader(parameterSection: ParameterSection, analog: Analog) extends Platform {  
  
  
  /**
   * Base trait providing some common force-plate functionality.
   */
  trait FPBase {
    
    def plateIndex: Int
    
    val origin: Vec3D = {
      val op: Parameter[Float] = getReqParameter[Float]("FORCE_PLATFORM", "ORIGIN")
      DefaultVec3D(op(0, plateIndex), op(1, plateIndex), op(2, plateIndex))
    }
    
    val corners: IndexedSeq[Vec3D] = {
      val cp: Parameter[Float] = getReqParameter[Float]("FORCE_PLATFORM", "CORNERS")
      for (i <- 0 until 4) yield DefaultVec3D(cp(0, i, plateIndex), cp(1, i, plateIndex), cp(2, i, plateIndex))
    }
    
  }
  
  
  /**
   * Type 2 force plate.
   * 
   * Type 2 force plates reference force and moment channels directly in the analog data.
   */
  private final case class Type2(plateIndex: Int) extends ForcePlate with FPBase {

    private val channels: Array[AnalogChannel] = {
      val channelNumbers: Parameter[Int] = getReqParameter[Int]("FORCE_PLATFORM", "CHANNEL")
      def getChannel(n: Int): AnalogChannel = analog.channels(channelNumbers(n, plateIndex) - 1)
      (0 to 5).map(getChannel).to[Array]
    }
    
    private final case class ChannelVec3DISeq(xc: AnalogChannel, yc: AnalogChannel, zc: AnalogChannel)
    extends IndexedSeq[Vec3D] {
      assert((xc.length == yc.length) && (xc.length == zc.length), "all channels must be the same length")
      def length: Int = xc.length
      def apply(index: Int): Vec3D = DefaultVec3D(xc(index), yc(index), zc(index))
    }
        
    def forceInFPCoords: IndexedSeq[Vec3D] = ChannelVec3DISeq(channels(0), channels(1), channels(2))
    def momentInFPCoords: IndexedSeq[Vec3D] = ChannelVec3DISeq(channels(3), channels(4), channels(5))
    
  }
  

  /**
   * Type 4 force plate.
   * 
   * Type 4 force plates are similar to Type 2, except that the output from a Type 2 plate then passes through a
   * calibration matrix.
   */
  private final case class Type4(plateIndex: Int) extends ForcePlate with FPBase {
    
    private val t2: Type2 = Type2(plateIndex)

    private val calibrationMatrix: Array[Float] = {
      val cm: Parameter[Float] = getReqParameter[Float]("FORCE_PLATFORM", "CAL_MATRIX")
      val a = Array.ofDim[Float](36)
      for {
        r <- 0 until 6
        c <- 0 until 6
      } (a(c + r*6) = cm(r, c, plateIndex))
      a
    }
    def m(r: Int, c: Int): Float = calibrationMatrix(c + r*6)

    private final case class CalMatrixISeq(startMatrixRow: Int) extends IndexedSeq[Vec3D] {
      private val r = startMatrixRow
      private val s = startMatrixRow + 1
      private val t = startMatrixRow + 2
      def length: Int = analog.totalSamples
      def apply(index: Int): Vec3D = {
        val fx = t2.forceInFPCoords(index).x
        val fy = t2.forceInFPCoords(index).y
        val fz = t2.forceInFPCoords(index).z
        val mx = t2.momentInFPCoords(index).x
        val my = t2.momentInFPCoords(index).y
        val mz = t2.momentInFPCoords(index).z
        val x = m(r, 0) * fx + m(r, 1) * fy + m(r, 2) * fz + m(r, 3) * mx + m(r, 4) * my + m(r, 5) * mz
        val y = m(s, 0) * fx + m(s, 1) * fy + m(s, 2) * fz + m(s, 3) * mx + m(s, 4) * my + m(s, 5) * mz
        val z = m(t, 0) * fx + m(t, 1) * fy + m(t, 2) * fz + m(t, 3) * mx + m(t, 4) * my + m(t, 5) * mz
        DefaultVec3D(x, y, z)
      }
    }
    
    def forceInFPCoords: IndexedSeq[Vec3D] = CalMatrixISeq(0)
    def momentInFPCoords: IndexedSeq[Vec3D] = CalMatrixISeq(3)
    
  }
  
  
  private object ForcePlateIndexedSeq extends IndexedSeq[ForcePlate] {
    def length: Int = parameterSection.getParameter[Int]("FORCE_PLATFORM", "USED").map(_(0)).getOrElse(0)
    def apply(index: Int): ForcePlate = {
      require((index >= 0) && (index < length), s"index must satisfy: 0 <= index < $length")
      val typ: Int = getReqParameter[Int]("FORCE_PLATFORM", "TYPE").apply(index)
      if (typ == 2) {
        Type2(index)
      } else if (typ == 4) {
        Type4(index)
      } else {
        throw new UnsupportedForcePlateException(typ)
      }
    }
  }
  
  
  def plates: IndexedSeq[ForcePlate] = ForcePlateIndexedSeq

  
  private def getReqParameter[T: TypeTag](groupName: String, paramName: String): Parameter[T] = {
    val paramOpt = parameterSection.getParameter[T](groupName, paramName)
    paramOpt.getOrElse(
      throw RequiredParameterNotFoundException(groupName, paramName))
  }  
  
  
}