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
        val testValidation = for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
        } yield {
          assert(gp.length === 43)
        }
        assert(testValidation.isSuccess)
      }
    }

    it("should be able to partition groups and parameters") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        val testValidation = for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
          gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
          gs = gsps._1
          ps = gsps._2
        } yield {
          assert(gs.length === 5)
          assert(ps.length === 38)
        }
        assert(testValidation.isSuccess)
      }
    }

    it("should be able to construct unassociated groups") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        val testValidation = for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
          gsps = partitionToGroupsAndParams(gp)
          groupBlocks = gsps._1
          paramBlocks = gsps._2
        } yield {
          val groups = groupBlocks.map(new UnassociatedGroup(_))
          assert(groups.length === 5)
          val pt = groups(0)
          assert(pt.name === "POINT")
          assert(pt.description === "3-D point parameters")
          assert(pt.id === 1)
          assert(pt.isLocked === false)
        }
        assert(testValidation.isSuccess)
      }
    }

  }

}









