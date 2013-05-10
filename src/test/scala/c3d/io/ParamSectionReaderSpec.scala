package c3d.io

import org.scalatest.FunSpec
import c3d.ProcessorType

class ParamSectionReaderSpec extends FunSpec with C3DFileSource {

  describe("ParamSectionReader") {

    import ParamSectionReader._

    it("should correctly identify the processor type") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps =>
        processorType(ps).fold(
          error => fail(),
          processorType => assert(processorType === ProcessorType.Intel)
        )
      }
    }

  }

}









