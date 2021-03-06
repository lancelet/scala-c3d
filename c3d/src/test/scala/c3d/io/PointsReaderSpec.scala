package c3d.io

import scala.collection.immutable._
import org.scalatest.FunSpec
import c3d._

class PointsReaderSpec extends FunSpec with C3DFileSource {

  private def pointsReader(wholeFile: IndexedSeq[Byte]): PointsReader = {
    val parameterSection = ParamSectionReader.read(C3DReader.getParameterSection(wholeFile))
    val dataSection = C3DReader.getDataSection(wholeFile, parameterSection)
    PointsReader(parameterSection, dataSection)
  }

  // for the EB015PI and associated datasets
  private val lth1_samples_39_to_48: IndexedSeq[Option[Vec3D]] = IndexedSeq(
    None,
    None,
    Some(Vec3D(-85.583336f, 72.333336f, 675.0834f)),
    Some(Vec3D(-84.083336f, 75.250000f, 676.5834f)),
    Some(Vec3D(-83.166670f, 79.250000f, 678.3334f)),
    Some(Vec3D(-82.416670f, 82.416670f, 680.0000f)),
    Some(Vec3D(-81.833336f, 85.583336f, 682.5834f)),
    Some(Vec3D(-81.916670f, 89.083336f, 684.9167f)),
    Some(Vec3D(-82.000000f, 92.583336f, 687.0000f)),
    Some(Vec3D(-81.833336f, 95.166670f, 690.0000f)))
  
  describe("PointsReader") {
    
    it("should report the correct number of points (Sample08.EB015PI)") {
      assert(pointsReader(Sample08.EB015PI).points.length === 26)
    }
    
    it("should list the point names correctly (Sample08.EB015PI)") { 
      val expectedNames: Seq[String] = Seq("RFT1", "RFT2", "RFT3", "LFT1", "LFT2", "LFT3", "RSK1", "RSK2", "RSK3",   
        "RSK4", "LSK1", "LSK2", "LSK3", "LSK4", "RTH1", "RTH2", "RTH3", "RTH4", "LTH1", "LTH2", "LTH3", "LTH4",
        "PV1", "PV2", "PV3", "pv4")
      val names: Seq[String] = pointsReader(Sample08.EB015PI).points.map(_.name.trim)
      assert(names === expectedNames)
    }
    
    it("should list the point descriptions correctly (Sample08.EB015PI)") {
      val expectedDescriptions: Seq[String] = Seq(
        "DIST/LAT FOOT",
        "INSTEP",
        "PROX LAT FOOT",
        "SHANK",
        "SHANK",
        "SHANK",
        "SHANK",
        "ANKLE",
        "KNEE",
        "DISTAL FOOT",
        "*",
        "*",
        "*",
        "*",
        "*",
        "*",
        "*",
        "*",
        "*",
        "TARGET",
        "",
        "",
        "",
        "",
        "",
        ""
      )
      val descriptions: Seq[String] = pointsReader(Sample08.EB015PI).points.map(_.description.trim)
      assert(descriptions === expectedDescriptions)
    }
    
    it("should find the correct sampling rate (Sample08.EB015PI)") {
      val pr = pointsReader(Sample08.EB015PI)
      assert(pr.rate === 50.0f)
      for (point <- pr.points) assert(point.rate == 50.0f)
    }
    
    it("should correctly find the length of each point (Sample08.EB015PI)") {
      val pointSamples: Int = 450
      val pr = pointsReader(Sample08.EB015PI)
      for (point <- pr.points) assert(point.length === pointSamples)
      assert(pr.totalSamples === pointSamples)
    }
    
    it("should correctly convert a Point to a Marker (Sample08.EB015PI)") {
      val pr = pointsReader(Sample08.EB015PI)
      val pv3: Marker = pr.getPointByName("PV3").getOrElse(fail()).asMarker
      assert(pv3.name.trim === "PV3")
      assert(pv3.description.trim === "")
      assert(pv3.rate === 50.0f)
      assert(pv3.offset === 47)
      assert(pv3.length === 403)
      val expectedSlice = IndexedSeq(
        Vec3D( 7.750000f, 3.2500000f, 936.5000f),
        Vec3D( 9.416667f, 3.5833335f, 939.9167f),
        Vec3D(12.333334f, 4.2500000f, 942.5834f),
        Vec3D(12.583334f, 4.5833335f, 946.5000f))
      assert(pv3.slice(0, 4) === expectedSlice)
      assert(pv3(pv3.length - 1) === Vec3D(106.333336f, 2125.8335f, 936.5834f))
    }
    
    it("should contain correct point data (Sample08.EB015PI)") {
      val fileSamples = pointsReader(Sample08.EB015PI).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }

    it("should contain correct point data (Sample01.EB015PI)") {
      val fileSamples = pointsReader(Sample01.EB015PI).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }

    it("should contain correct point data (Sample01.EB015PR)") {
      val fileSamples = pointsReader(Sample01.EB015PR).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }
    
    it("should contain correct point data (Sample01.EB015SI)") {
      val fileSamples = pointsReader(Sample01.EB015SI).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }
    
    it("should contain correct point data (Sample01.EB015SR)") {
      val fileSamples = pointsReader(Sample01.EB015SR).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }

    it("should contain correct point data (Sample01.EB015VI)") {
      val fileSamples = pointsReader(Sample01.EB015VI).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }

    it("should contain correct point data (Sample01.EB015VR)") {
      val fileSamples = pointsReader(Sample01.EB015VR).getPointByName("LTH1").getOrElse(fail()).slice(39, 49)
      assert(fileSamples === lth1_samples_39_to_48)
    }    
    
  }
    
}
