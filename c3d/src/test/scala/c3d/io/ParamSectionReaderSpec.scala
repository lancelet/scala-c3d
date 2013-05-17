package c3d.io

import scala.collection.immutable._
import c3d.{Group, Parameter, ProcessorType}
import org.scalatest.FunSpec
import scalaz.std.AllInstances._  // use Validation in for comprehensions
import scala.reflect.runtime.universe._

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
          assert(gp.length === 42)
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
          assert(ps.length === 37)
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

    it("should be able to construct unassociated, untyped parameters") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        val testValidation = for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
          gsps = partitionToGroupsAndParams(gp)
          groupBlocks = gsps._1
          paramBlocks = gsps._2
        } yield {
          val params = paramBlocks.map(new UntypedParameter(_))
          assert(params.length === 37)
          val d = params(0)
          assert(d.name === "DESCRIPTIONS")
          assert(d.description === "  Point descriptions")
          assert(d.groupId === 1)
          assert(d.dimensions === IndexedSeq(32,20))
          assert(d.byteLengthPerElement === -1)
          assert(d.data.length === 32 * 20)
          assert(d.data.slice(0, 13).map(_.toChar).mkString === "DIST/LAT FOOT")
          assert(d.isLocked === false)
        }
        assert(testValidation.isSuccess)
      }
    }

    it("should be able to construct unassociated, typed parameters") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        val testValidation = for {
          gp: Seq[FormattedByteIndexedSeq] <- chunkGroupsAndParams(ps)
          gsps = partitionToGroupsAndParams(gp)
          groupBlocks = gsps._1
          paramBlocks = gsps._2
        } yield {
          val params = paramBlocks.map(new UntypedParameter(_).asUnassociatedParameter)
          assert(params.length === 37)
          val d = params(0).asInstanceOf[UnassociatedParameter[Char]]
          assert(d.parameterType === Parameter.Type.Character)
          assert(d.name === "DESCRIPTIONS")
          assert(d.description === "  Point descriptions")
          assert(d.groupId === 1)
          assert(d.dimensions === IndexedSeq(32,20))
          assert(d.data.length === 32 * 20)
          assert(d.data.slice(0, 13).mkString === "DIST/LAT FOOT")
          assert(d.isLocked === false)
        }
        assert(testValidation.isSuccess)
      }
    }

    it("should read in the entire parameter section") {
      C3DReader.paramSectionIndexedSeq(Sample08.EB015PI).map { ps: FormattedByteIndexedSeq =>
        val testValidation = for {
          groups <- read(ps)
        } yield {
          def get(name: String): Group = groups.find(_.name == name).get
          def getp[T](group: String, name: String): Parameter[T] = get(group).parameters.
            find(_.name == name).get.asInstanceOf[Parameter[T]]
          assert(groups.size === 5)
          // check the names of all groups
          assert(groups.map(_.name) === Seq("POINT", "ANALOG", "FORCE_PLATFORM", "FPLOC", "SUBJECT"))
          // check the names of parameters belonging to all groups
          assert(get("POINT").parameters.map(_.name) ===
            Seq("DESCRIPTIONS", "X_SCREEN", "Y_SCREEN", "LABELS", "UNITS", "USED", "FRAMES", "SCALE", "DATA_START", 
              "RATE"))
          assert(get("ANALOG").parameters.map(_.name) ===
            Seq("LABELS", "DESCRIPTIONS", "SCALE", "GEN_SCALE", "OFFSET", "UNITS", "USED", "RATE"))
          assert(get("FORCE_PLATFORM").parameters.map(_.name) ===
            Seq("USED", "TYPE", "CORNERS", "ORIGIN", "CHANNEL", "ZERO", "TRANSLATION", "ROTATION"))
          assert(get("FPLOC").parameters.map(_.name) ===
            Seq("OBJ", "MAX", "INT"))
          assert(get("SUBJECT").parameters.map(_.name) ===
            Seq("NAME", "NUMBER", "PROJECT", "WEIGHT", "HEIGHT", "GENDER", "DATE_OF_BIRTH", "TARGET_RADIUS"))
          // check various parameter types
          assert(getp[Float]("POINT", "RATE").data(0) === 50.0f) // float
          assert(getp[Int]("POINT", "DATA_START").data(0) === 11) // int
          assert(getp[Char]("POINT", "UNITS").data.mkString === "mm  ") // char
        }
        assert(testValidation.isSuccess)
      }
    }

  }

}









