package c3d.io

import scala.collection.immutable._
import c3d._
import scala.reflect.runtime.universe._
import c3d.util.transform.RotationMatrix

private[io] final case class PlatformReader(parameterSection: ParameterSection, analog: Analog) extends Platforms {  
  
  
  /**
   * Base trait providing some common force-plate functionality.
   */
  trait FPBase {
    
    def plateIndex: Int
    def forceInFPCoords: SIndexedSeq[Vec3D]
    def momentInFPCoords: SIndexedSeq[Vec3D]
    def rate: Float = getReqParameter[Float]("ANALOG", "RATE").apply(0)
    
    /**
     * Origin for Type 2 and Type 4 force platforms is a vector from the origin of the force platform coordinate
     * system to the point at the geometric center of the force platform working surface, expressed in the force
     * platform coordinate system.
     */
    val origin: Vec3D = {
      val op: Parameter[Float] = getReqParameter[Float]("FORCE_PLATFORM", "ORIGIN")
      Vec3D(op(0, plateIndex), op(1, plateIndex), op(2, plateIndex))
    }
    
    val corners: IndexedSeq[Vec3D] = {
      val cp: Parameter[Float] = getReqParameter[Float]("FORCE_PLATFORM", "CORNERS")
      for (i <- 0 until 4) yield Vec3D(cp(0, i, plateIndex), cp(1, i, plateIndex), cp(2, i, plateIndex))
    }
    
    val center: Vec3D = (corners(0) + corners(1) + corners(2) + corners(3)) * 0.25f
    
    val rotWorldToPlate: RotationMatrix = {
      val xHat = ((corners(0) - corners(1)) + (corners(3) - corners(2))) * 0.5f
      val yHat = ((corners(0) - corners(3)) + (corners(1) - corners(2))) * 0.5f
      RotationMatrix.fromBasisVectorsXY(xHat, yHat)
    }
    val rotPlateToWorld: RotationMatrix = rotWorldToPlate.inv
   
    object PwaIndexedSeq extends SIndexedSeq[Vec3D] {
      def length: Int = forceInFPCoords.length
      def apply(i: Int): Vec3D = {
        // find the component of the moment that is not parallel to the force
        val f = force(i)
        val fHat = f.asUnit
        val mo = momentAtOrigin(i)
        val mt = mo - (fHat * (mo dot fHat))
        // find r
        val rHat = (fHat cross mo).asUnit
        val r = rHat * (mt.mag / f.mag)
        // intersect with force plate plane (x-y plane, z=0)
        val s = -r.z / fHat.z
        val p = r + fHat * s
        center + p
      }
      def rate: Float = PlatformReader.this.rate
    }    
    
    object ForceIndexedSeq extends SIndexedSeq[Vec3D] {
      def length: Int = forceInFPCoords.length
      def apply(i: Int): Vec3D = rotPlateToWorld(forceInFPCoords(i))
      def rate: Float = PlatformReader.this.rate
    }
    
    object MomentAtOriginIndexedSeq extends SIndexedSeq[Vec3D] {
      def length: Int = momentInFPCoords.length
      def apply(i: Int): Vec3D = rotPlateToWorld(momentInFPCoords(i) - (origin cross forceInFPCoords(i)))
      def rate: Float = PlatformReader.this.rate
    }
    
    object MomentIndexedSeq extends SIndexedSeq[Vec3D] {
      def length: Int = momentInFPCoords.length
      def apply(i: Int): Vec3D = {
        val r = pwa(i) - center
        momentAtOrigin(i) - (r cross force(i))
      }
      def rate: Float = PlatformReader.this.rate
    }
       
    def pwa: SIndexedSeq[Vec3D] = PwaIndexedSeq
    def force: SIndexedSeq[Vec3D] = ForceIndexedSeq
    def moment: SIndexedSeq[Vec3D] = MomentIndexedSeq
    def momentAtOrigin: SIndexedSeq[Vec3D] = MomentAtOriginIndexedSeq
    
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
    extends SIndexedSeq[Vec3D] {
      assert((xc.length == yc.length) && (xc.length == zc.length), "all channels must be the same length")
      def length: Int = xc.length
      def apply(index: Int): Vec3D = Vec3D(xc(index), yc(index), zc(index))
      def rate: Float = PlatformReader.this.rate
    }
        
    def forceInFPCoords: SIndexedSeq[Vec3D] = ChannelVec3DISeq(channels(0), channels(1), channels(2))
    def momentInFPCoords: SIndexedSeq[Vec3D] = ChannelVec3DISeq(channels(3), channels(4), channels(5))
    
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

    private final case class CalMatrixISeq(startMatrixRow: Int) extends SIndexedSeq[Vec3D] {
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
        Vec3D(x, y, z)
      }
      def rate: Float = PlatformReader.this.rate
    }
    
    def forceInFPCoords: SIndexedSeq[Vec3D] = CalMatrixISeq(0)
    def momentInFPCoords: SIndexedSeq[Vec3D] = CalMatrixISeq(3)
    
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
  def rate: Float = getReqParameter[Float]("ANALOG", "RATE").apply(0)

  
  private def getReqParameter[T: TypeTag](groupName: String, paramName: String): Parameter[T] = {
    val paramOpt = parameterSection.getParameter[T](groupName, paramName)
    paramOpt.getOrElse(
      throw RequiredParameterNotFoundException(groupName, paramName))
  }  
  
  
}