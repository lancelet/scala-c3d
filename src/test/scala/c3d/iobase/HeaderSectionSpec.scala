package c3d.iobase

import org.scalatest.FunSpec
import resource.managed
import java.io.File

/**
 * Spec for HeaderSection.
 */
class HeaderSectionSpec extends FunSpec {

  /**
   * Call a function on the header section of a specified example file.
   *
   * @param sampleFile the sample C3D file on whose header the function should be executed
   * @param fn the function to call
   */
  def withSampleHeader(sampleFile: File)(fn: HeaderSection => Unit): Unit = {
    for {
      binarySource <- managed(BinarySource.fromFile(sampleFile))
    } {
      binarySource.getBytes(0, 512).fold(
        failure => fail(s"unexpected failure to retrieve header section from ${sampleFile.getName}"),
        success => fn(new HeaderSection(success))
      )
    }
  }

  describe("A HeaderSection") {

    it("should identify the C3D file magic number correctly") {
      // use a known-good header to check a correct magic number
      withSampleHeader(C3DExampleFiles.Sample08.eb015pi) { header => assert(header.hasMagicNumber === true) }

      // a slightly offset block doesn't contain a correct magic number
      for {
        binarySource <- managed(BinarySource.fromFile(C3DExampleFiles.Sample08.eb015pi))
      } {
        binarySource.getBytes(2, 512).fold(
          failure => fail("unexpected failture to retrieve the section of the file after the header"),
          success => {
            val notARealHeader = new HeaderSection(success)
            assert(notARealHeader.hasMagicNumber === false)
          }
        )
      }
    }

    it("should point to the correct parameter block") {
      withSampleHeader(C3DExampleFiles.Sample08.eb015pi) { header => assert(header.firstParameterBlock === 2) }
      withSampleHeader(C3DExampleFiles.Sample08.testbpi) { header => assert(header.firstParameterBlock === 11) }
      withSampleHeader(C3DExampleFiles.Sample08.testdpi) { header => assert(header.firstParameterBlock === 7) }
    }

  }

}
