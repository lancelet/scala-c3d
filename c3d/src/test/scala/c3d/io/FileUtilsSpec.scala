package c3d.io

import java.io.{File, IOException}
import org.scalatest.FunSpec

class FileUtilsSpec extends FunSpec {

  describe("FileUtils") {

    describe("fileToIndexedSeq") {

      it("should fail to load a non-existent file") {
        val nonExistentFile = new File("./c3d.org-example-files/NotAFile.c3d.NOT")
        assert(nonExistentFile.exists === false, "Supposedly non-existent file really exists!  Check test code.")
        assert(FileUtils.fileToIndexedSeq(nonExistentFile).isFailure)
      }

      it("should correctly load an example C3D file") {
        val eb015pi = new File("./c3d.org-example-files/sample08/EB015PI.c3d")
        if (eb015pi.exists == false) {
          cancel("File ./c3d.org-example-files/sample08/EB015PI.c3d not found.  " +
            "You must run the fetch-c3d-example-files sbt task.")
        }
        val iseq: IndexedSeq[Byte] = FileUtils.fileToIndexedSeq(eb015pi) recover { 
          case err => fail(s"Could not read file: ${err}") 
        } getOrElse { fail("Should not reach this code.") }
        assert(iseq.length  === 156672)
        assert(iseq(0)      === 0x02.toByte)
        assert(iseq(1)      === 0x50.toByte)
        assert(iseq(806)    === 0x4E.toByte)
        assert(iseq(156671) === 0x00.toByte)
      }

    }

  }

}
