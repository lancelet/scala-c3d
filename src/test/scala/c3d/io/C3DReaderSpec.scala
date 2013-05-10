package c3d.io

import org.scalatest.FunSpec
import scala.collection.immutable._

class C3DReaderSpec extends FunSpec with C3DFileSource {

  describe("C3DReader") {

    import C3DReader._

    it("should identify the magic byte correctly") {
      assert(hasMagicByte(IndexedSeq(b(0x01), b(0x50))) === true)
      assert(hasMagicByte(IndexedSeq(b(0x01), b(0x49))) === false)
    }

    it("should extract the parameter section blocks correctly") {
      paramSectionIndexedSeq(Sample08.EB015PI).fold(
        error => fail(),
        pb => {
          assert(pb.length === (9 * 512))
          assert(pb(0)           === b(0x01))
          assert(pb(1)           === b(0x50))
          assert(pb(2)           === b(0x09))
          assert(pb(3)           === b(0x54))
          assert(pb(9 * 512 - 1) === b(0x00))
        }
      )
    }

  }

}
