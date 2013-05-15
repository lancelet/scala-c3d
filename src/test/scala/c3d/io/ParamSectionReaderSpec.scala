package c3d.io

import scala.collection.immutable._
import c3d.ProcessorType
import org.scalatest.FunSpec
import scalaz.std.AllInstances._  // use Validation in for comprehensions


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

    it("should chunk the parameter section correctly into groups and parameters") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        for {
          pt: ProcessorType <- processorType(ps)
          bf: BinaryFormat = BinaryFormat.fromProcessorType(pt)
          gp: Seq[IndexedSeq[Byte]] <- chunkGroupsAndParams(ps)
        } {
          assert(gp.length === 43)
        }
      }
    }

  }

}









