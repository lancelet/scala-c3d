package c3d.io

import scala.collection.immutable._
import org.scalatest.FunSpec

class AnalogReaderSpec extends FunSpec with C3DFileSource {

  import AnalogReader._

  private def analogReader(wholeFile: IndexedSeq[Byte]): AnalogReader = {
    val parameterSection = ParamSectionReader.read(C3DReader.getParameterSection(wholeFile))
    val dataSection = C3DReader.getDataSection(wholeFile, parameterSection)
    AnalogReader(parameterSection, dataSection)
  }

  private val fz1samples1370to1380: IndexedSeq[Float] = IndexedSeq(-801.288f, -802.03204f, -802.03204f,
    -803.52f, -804.26404f, -803.52f, -803.52f, -803.52f, -801.288f, -804.26404f, -805.008f)
      
  describe("AnalogReader") {
        
    it("should report the correct number of channels (Sample08.EB015PI)") {
      assert(analogReader(Sample08.EB015PI).channels.length === 16)
    }
    
    it("should list the channel names correctly (Sample08.EB015PI)") {
      val expectedNames: Seq[String] = Seq("FX1", "FY1", "FZ1", "MX1", "MY1", "MZ1", "CH7", "CH8", "FX2", "FY2", "FZ2",
          "MX2", "MY2", "MZ2", "CH15", "CH16")
      val names: Seq[String] = analogReader(Sample08.EB015PI).channels.map(_.name.trim)
      assert(names === expectedNames)
    }

    it("should list the channel descriptions correctly (Sample08.EB015PI)") {
      val expectedDescriptions: Seq[String] = Seq(
        "FORCE X-COMP 1",
        "FORCE Y-COMP 1",
        "FORCE Z-COMP 1",
        "X-MOMENT 1",
        "Y-MOMENT 1",
        "Z-MOMENT 1",
        "X-ACCELEROMETER",
        "Y-ACCELEROMETER",
        "FORCE X-COMP 2",
        "FORCE Y-COMP 2",
        "FORCE Z-COMP 2",
        "X-MOMENT 2",
        "Y-MOMENT 2",
        "Z-MOMENT 2",
        "Z-ACCELEROMETER",
        "FORCE TRANSDUCER")
      val descriptions: Seq[String] = analogReader(Sample08.EB015PI).channels.map(_.description.trim)
      assert(descriptions === expectedDescriptions)
    }
    
    it("should correctly find the length of each channel (Sample08.EB015PI)") {
      val analogSamples: Int = 1800
      for (channel <- analogReader(Sample08.EB015PI).channels) assert(channel.length === analogSamples)
    }
    
    it("should contain correct analog channel samples (Sample08.EB015PI)") {
      val fileSamples = analogReader(Sample08.EB015PI).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)
    }
    
    it("should contain correct analog channel samples (Sample01.EB015PI)") {
      val fileSamples = analogReader(Sample01.EB015PI).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }

    it("should contain correct analog channel samples (Sample01.EB015PR)") {
      val fileSamples = analogReader(Sample01.EB015PR).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }    

    it("should contain correct analog channel samples (Sample01.EB015SI)") {
      val fileSamples = analogReader(Sample01.EB015SI).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }    
    
    it("should contain correct analog channel samples (Sample01.EB015SR)") {
      val fileSamples = analogReader(Sample01.EB015SR).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }    

    it("should contain correct analog channel samples (Sample01.EB015VI)") {
      val fileSamples = analogReader(Sample01.EB015VI).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }        

    it("should contain correct analog channel samples (Sample01.EB015VR)") {
      val fileSamples = analogReader(Sample01.EB015VR).getChannelByName("FZ1 ").getOrElse(fail()).slice(1370, 1381)
      assert(fileSamples === fz1samples1370to1380)      
    }            
    
  }
  
}