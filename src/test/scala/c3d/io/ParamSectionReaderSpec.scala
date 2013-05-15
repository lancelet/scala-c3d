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
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
        } {
          assert(gp.length === 43)
        }
      }
    }

    it("should be able to partition groups and parameters") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
          gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
          gs = gsps._1
          ps = gsps._2
        } {
          assert(gs.length === 5)
          assert(ps.length === 38)
        }
      }
    }

  }

}









