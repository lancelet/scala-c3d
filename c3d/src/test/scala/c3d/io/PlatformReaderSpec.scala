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
    
  }
  
}