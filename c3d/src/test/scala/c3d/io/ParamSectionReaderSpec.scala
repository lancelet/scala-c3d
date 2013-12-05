package c3d.io

import scala.collection.immutable._
import c3d.{Group, Parameter, ProcessorType}
import org.scalatest.FunSpec
import scalaz.std.AllInstances._
import scala.reflect.runtime.universe._
import Util.b
import c3d.ParameterSection

class ParamSectionReaderSpec extends FunSpec with C3DFileSource {

  describe("ParamSectionReader") {

    import ParamSectionReader._

    it("should chunk the parameter section correctly into groups and parameters") {
      val (ps, procType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val gp: Seq[FormattedByteIndexedSeq] = chunkGroupsAndParams(ps)
      assert(gp.length === 42)
    }

    it("should be able to partition groups and parameters") {
      val (ps, procType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val gp: Seq[FormattedByteIndexedSeq] = chunkGroupsAndParams(ps)
      val gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
      val groups = gsps._1
      val params = gsps._2
      assert(groups.length === 5)
      assert(params.length === 37)
    }

    it("should be able to construct unassociated groups") {
      val (ps, procType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val gp: Seq[FormattedByteIndexedSeq] = chunkGroupsAndParams(ps)
      val gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
      val groups = gsps._1.map(new UnassociatedGroup(_))
      assert(groups.length === 5)
      val pt = groups(0)
      assert(pt.name === "POINT")
      assert(pt.description === "3-D point parameters")
      assert(pt.id === 1)
      assert(pt.isLocked === false)
    }

    it("should be able to construct unassociated, untyped parameters") {
      val (ps, procType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val gp: Seq[FormattedByteIndexedSeq] = chunkGroupsAndParams(ps)
      val gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
      val params = gsps._2.map(new UntypedParameter(_))
      assert(params.length === 37)

      val d = params(0)
      assert(d.name === "DESCRIPTIONS")
      assert(d.description === "  Point descriptions")
      assert(d.groupId === 1)
      assert(d.dimensions === IndexedSeq(32, 20))
      assert(d.byteLengthPerElement === -1)
      assert(d.data.length === 32 * 20)
      assert(d.data.slice(0, 13).map(_.toChar).mkString === "DIST/LAT FOOT")
      assert(d.isLocked === false)

      val e = params(31)
      assert(e.name === "FRAMES")
      assert(e.description === "* Number of video frames")
      assert(e.groupId === 1)
      assert(e.dimensions === IndexedSeq(1))
      assert(e.byteLengthPerElement === 2)
      assert(e.data.length === 2)
      assert(e.data === IndexedSeq(b(0xC2), b(0x01)))
      assert(e.isLocked === true)
    }

    it("should be able to construct unassociated, typed parameters") {
      val (ps, procType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val gp: Seq[FormattedByteIndexedSeq] = chunkGroupsAndParams(ps)
      val gsps: (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = partitionToGroupsAndParams(gp)
      val params = gsps._2.map(new UntypedParameter(_).asUnassociatedParameter)

      assert(params.length === 37)

      val d = params(0).asInstanceOf[UnassociatedParameter[Char]]
      assert(d.parameterType === Parameter.Type.Character)
      assert(d.name === "DESCRIPTIONS")
      assert(d.description === "  Point descriptions")
      assert(d.groupId === 1)
      assert(d.dimensions === IndexedSeq(32, 20))
      assert(d.data.length === 32 * 20)
      assert(d.data.slice(0, 13).mkString === "DIST/LAT FOOT")
      assert(d.isLocked === false)

      val e = params(31).asInstanceOf[UnassociatedParameter[Int]]
      assert(e.parameterType === Parameter.Type.Integer)
      assert(e.name === "FRAMES")
      assert(e.description === "* Number of video frames")
      assert(e.groupId === 1)
      assert(e.dimensions === IndexedSeq(1))
      assert(e.data.length === 1)
      assert(e.data === IndexedSeq(450))
      assert(e.isLocked === true)
    }

    it("should read in the entire parameter section") {
      val (paramISeq, processorType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val ps = read(paramISeq, processorType)

      def get(name: String): Group = ps.groups.find(_.name == name).get
      def getp[T](group: String, name: String): Parameter[T] = get(group).parameters.
        find(_.name == name).get.asInstanceOf[Parameter[T]]

      assert(ps.groups.size === 5)
      // check the names of all groups
      assert(ps.groups.map(_.name) === Seq("POINT", "ANALOG", "FORCE_PLATFORM", "FPLOC", "SUBJECT"))
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

    it("should be able to read parameters stored with zero dimensions (scalars)") {
      val (paramISeq, processorType) = C3DReader.getParameterSection(Sample08.EB015PI)
      val ps: ParameterSection = read(paramISeq, processorType)
      val groups: Seq[Group] = ps.groups
      val analog: Group = groups.find(_.name == "ANALOG").getOrElse(fail())
      val used: Parameter[Int] = analog.parameters.find(_.name == "USED").getOrElse(fail()).
        asInstanceOf[Parameter[Int]]
      assert(used(IndexedSeq(0)) === 16)
    }

  }

}
