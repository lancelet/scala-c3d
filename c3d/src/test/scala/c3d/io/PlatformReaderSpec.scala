package c3d.io

import scala.collection.immutable._
import org.scalatest.FunSpec
import c3d._

class PlatformReaderSpec extends FunSpec with C3DFileSource {

  import PlatformReader._
  
  private def platformReader(wholeFile: IndexedSeq[Byte]): PlatformReader = {
    val parameterSection = ParamSectionReader.read(C3DReader.getParameterSection(wholeFile))
    val dataSection = C3DReader.getDataSection(wholeFile, parameterSection)
    val analog = AnalogReader(parameterSection, dataSection)
    PlatformReader(parameterSection, analog)
  }

  private val fp2mz_samples_640_to_650: IndexedSeq[Float] = IndexedSeq(-8259.44f, -8403.5f, -8355.48f, -8547.561f,
    -8547.561f, -8499.54f, -8451.5205f, -8595.58f, -8403.5f, -8499.54f, -8451.5205f)

  private val fp2fz_samples_640_to_650: IndexedSeq[Float] = IndexedSeq(-463.904f, -466.956f, -471.534f, -476.875f,
    -482.979f, -482.979f, -485.268f, -490.609f, -492.898f, -495.187f, -500.528f)
    

  describe("PlatformReader") {
    
    it("should contain the correct number of force platforms (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      assert(pr.plates.length === 2)
    }
    
    it("should find the correct length of force and moment samples for each platform (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      for (plate <- pr.plates) {
        assert(plate.forceInFPCoords.length === 1800)
        assert(plate.momentInFPCoords.length === 1800)
      }
    }
    
    it("should contain correct Mz components for force platform 2 (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p2: ForcePlate = pr.plates(1)
      val mz: IndexedSeq[Float] = p2.momentInFPCoords.map(_.z)
      val slice: IndexedSeq[Float] = mz.slice(640, 651)  // frame (640 / 4) = 160 (161 in 1-based)
      assert(slice === fp2mz_samples_640_to_650)
    }
    
    it("should contain correct Fz components for force platform 2 (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p2: ForcePlate = pr.plates(1)
      val fz: IndexedSeq[Float] = p2.forceInFPCoords.map(_.z)
      val slice: IndexedSeq[Float] = fz.slice(640, 651)
      assert(slice === fp2fz_samples_640_to_650)
    }
    
    it("should have the correct force plate origin (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p1: ForcePlate = pr.plates(0)
      val p2: ForcePlate = pr.plates(1)
      val expectedOrigin1 = DefaultVec3D(-4.4f, 1.9f, -21.6f)
      val expectedOrigin2 = DefaultVec3D(-4.06f, 3.81f, -20.06f)
      assert(p1.origin === expectedOrigin1)
      assert(p2.origin === expectedOrigin2)
    }
    
    it("should correctly retrieve the force plate corners (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p1: ForcePlate = pr.plates(0)
      val p2: ForcePlate = pr.plates(1)
      val expectedCorners1 = IndexedSeq(
        DefaultVec3D(520.0451f, 1242.1694f, 0.62186754f),
        DefaultVec3D(57.04628f, 1243.1996f, 0.6211077f),
        DefaultVec3D(58.1765f, 1751.1963f, 2.081213f),
        DefaultVec3D(521.17535f, 1750.1661f, 2.0819728f))
      val expectedCorners2 = IndexedSeq(
        DefaultVec3D(53.655487f, 1139.9977f, 1.9204264f),
        DefaultVec3D(516.6432f, 1143.3159f, 1.2880275f),
        DefaultVec3D(520.2825f, 635.33014f, 0.18136889f),
        DefaultVec3D(57.29477f, 632.01184f, 0.81376773f))
      assert(p1.corners === expectedCorners1)
      assert(p2.corners === expectedCorners2)
    }
    
    it("should contain correct world Mz components for force platform 2 (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p2 = pr.plates(1)
      val mzWorld: IndexedSeq[Float] = p2.moment.map(_.z)
      val slice: IndexedSeq[Float] = mzWorld.slice(640, 651)
      val expectedMzWorld: IndexedSeq[Float] = IndexedSeq(3963.6323f, 4030.9893f, 3931.2227f, 3943.3105f, 3934.6538f, 
        3973.2725f, 4056.248f, 4025.6084f, 4062.0537f, 3970.9888f, 4093.3125f)
      assert(slice === expectedMzWorld)
    }

    it("should contain correct world Fz components for force platform 2 (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p2 = pr.plates(1)
      val fzWorld: IndexedSeq[Float] = p2.force.map(_.z)
      val slice: IndexedSeq[Float] = fzWorld.slice(640, 651)
      val expectedFzWorld: IndexedSeq[Float] = IndexedSeq(463.82132f, 466.87238f, 471.44937f, 476.7882f, 482.89157f,
        482.89243f, 485.18396f, 490.52142f, 492.8132f, 495.10004f, 500.44354f)
      assert(slice === expectedFzWorld)
    }
    
    it("should contain correct world pwa components for force platform 2 (Sample08.EB015PI)") {
      val pr = platformReader(Sample08.EB015PI)
      val p2 = pr.plates(1)
      val pwaY: IndexedSeq[Float] = p2.pwa.map(_.y)
      val slice: IndexedSeq[Float] = pwaY.slice(640, 651)
      val expectedPwaY: IndexedSeq[Float] = IndexedSeq(736.95294f, 736.5673f, 736.93054f, 737.75977f, 738.83716f, 
        738.0668f, 737.2706f, 738.80206f, 738.51355f, 739.1481f, 740.2363f)
      assert(slice === expectedPwaY)
    }
    
  }
  
}