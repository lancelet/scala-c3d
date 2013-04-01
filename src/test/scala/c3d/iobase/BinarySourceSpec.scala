package c3d.iobase

import org.scalatest.FunSpec
import java.io.File
import resource.managed

/**
 * Spec for BinarySource.
 */
class BinarySourceSpec extends FunSpec {

  describe("A BinarySource") {

    it("should fail to read from a non-existent file") {
      for {
        binarySource <- managed(BinarySource.fromFile(new File("resources/not-a-c3d-file.c3d")))
      } {
        assert(binarySource.getBytes(0, 1).isFailure)
      }
    }

    it("should correctly read the header section from an example C3D file") {
      for {
        binarySource <- managed(BinarySource.fromFile(C3DExampleFiles.Sample08.eb015pi))
      } {
        binarySource.getBytes(0, 512).fold(
          failure => fail("Did not read the header section as expected"),
          iba => { // iba = ImmutableByteArray
            // Check for the correct size of the header section (512 bytes should have been read)
            assert(iba.length === 512)
            // Check that the first two bytes of the file contain the correct magic numbers
            assert(iba.byteAt(0) === 0x02)
            assert(iba.byteAt(1) === 0x50)
            // Check the last byte
            assert(iba.byteAt(511) === 0x00)
          }
        )
      }
    }

    it("should fail to read past the end of a file") {
      for {
        binarySource <- managed(BinarySource.fromFile(C3DExampleFiles.Sample08.eb015pi))
      } {
        assert(binarySource.getBytes(binarySource.length - 500, 512).isFailure)
      }
    }

  }

}
