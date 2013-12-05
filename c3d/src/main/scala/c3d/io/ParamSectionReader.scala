package c3d.io

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.math.abs
import scala.reflect.runtime.universe._
import c3d._
import Util.b
import c3d.io.collection.ImmutableArray

private [io] object ParamSectionReader {

  /** Splits the parameter section into blocks corresponding to groups and parameters.
    * 
    * The parameter section contains blocks of data, each of which corresponds to either a group or a parameter.
    * The first block starts at byte 5 of the parameter section.  Within each block, the following structure
    * is present:
    * {{{
    *   Byte | Length | Description
    *   ---------------------------------------------------------------------------------------------------------
    *      1 |      1 | Number of characters in group or parameter name (1-127).  A negative value indicates that
    *        |        | the group or parameter is locked.
    *   ---------------------------------------------------------------------------------------------------------
    *      2 |      1 | Group ID number (for the group, or the group to which the parameter belongs).
    *   ---------------------------------------------------------------------------------------------------------
    *      3 |      N | Group or parameter name.
    *   ---------------------------------------------------------------------------------------------------------
    *    3+N |      2 | Signed integer offset pointing to the start of the next group or parameter.
    * }}}
    * The byte at (3+N) is used as the offset to the start of the next section.
    * 
    * This function splits the entire parameter section into a sequence of blocks, each of which represents
    * either a group or parameter.
    * 
    * @param paramISeq the parameter section bytes
    * @return sequence of blocks corresponding to either groups or parameters
    */
  private [io] def chunkGroupsAndParams(paramISeq: FormattedByteIndexedSeq): Seq[FormattedByteIndexedSeq] =
  {
    /** Tail-recursive accumulator to collect blocks.
      * 
      * @param blocks blocks that have already been accumulated
      * @param rem remainder of the paramISeq to be processed
      * @return accumulated sequence of blocks
      */
    @tailrec
    def accum(blocks: Seq[FormattedByteIndexedSeq], rem: FormattedByteIndexedSeq): Seq[FormattedByteIndexedSeq] = {
      val nCharsInName = abs(rem(0))              // # of characters in the name
      val groupId = rem(1)                        // ID of the group (or the group to which a parameter belongs)
      val offset = rem.uintAt(2 + nCharsInName) & 0xFFFF // offset to start of next block (relative to its own index)
      val byteOffset = offset + 2 + nCharsInName  // offset to the next block (relative to current block)
      if (offset == 0) {   // if the offset to the next block is zero, we terminate...
        if (groupId == 0)  // if the groupId of the current block is zero then we don't add the current block
          blocks
        else
          blocks :+ rem.slice(0, rem.length)
      } else {
        accum(blocks :+ rem.slice(0, byteOffset), rem.slice(byteOffset, rem.length))
      }
    }

    accum(Seq.empty[FormattedByteIndexedSeq], paramISeq.slice(4, paramISeq.length))
  }


  /** Partitions parameter section blocks into a sequence of groups and sequence of parameters.
    *
    * Given a sequence of blocks from the parameter section, this method splits them into two sequences: one
    * containing only groups and one containing only parameters.
    * 
    * Groups are identified by their having a negative group number (byte 1), while parameters have a 
    * positive group number.
    * 
    * @param blocks sequence of generic blocks (either groups OR parameters)
    * @return (groups, parameters)
    */
  private [io] def partitionToGroupsAndParams(blocks: Seq[FormattedByteIndexedSeq]): 
      (Seq[FormattedByteIndexedSeq], Seq[FormattedByteIndexedSeq]) = 
  {
    def groupId(block: FormattedByteIndexedSeq): Byte = block(1)
    def isGroup(block: FormattedByteIndexedSeq): Boolean = groupId(block) < 0
    blocks.partition(isGroup _)
  }

  
  /** Unassociated group, without any connected parameters.
    * 
    * Normal [[Group]]s contain a sequence of parameters.  However, when reading groups from the file, they are
    * associated with their parameters using integer group IDs.  This "unassociated" group class represents a
    * group without its associated parameters, and still containing the group ID.
    * 
    * This class reads its properties (name, description, ID, and a locked flag) lazily, fetching them from
    * an underlying [[FormattedByteIndexedSeq]] when requested.
    * 
    * @param block parameter section block corresponding to a group
    */
  private [io] final class UnassociatedGroup(block: FormattedByteIndexedSeq) {
    private val nName: Int = abs(block(0))              // # of characters in name
    private val nDesc: Int = block(4 + nName) & 0xFFFF  // # of characters in description
    def name: String = block.slice(2, 2 + nName).map(_.toChar).mkString
    def description: String = block.slice(5 + nName, 5 + nName + nDesc).map(_.toChar).mkString
    def id: Int = { assert(block(1) < 0); -block(1) }  // ID of a group must be negative
    def isLocked: Boolean = block(0) < 0

    {
      val lenExpected = nName + nDesc + 5
      require(block.length == lenExpected, 
        s"block was expected to be ${lenExpected} bytes long, but was ${block.length}")
    }
  }

  
  /** Converts a type which may appear in a parameter to a [[Parameter.Type]].
    * 
    * Strings are converted to [[Parameter.Type.Character]].
    * 
    * @tparam T type (Char, String, Int, Byte or Float)
    * @return corresponding [[Parameter.Type]]
    */
  private [io] def typeToParameterType[T:TypeTag]: Option[Parameter.Type] = typeOf[T] match {
    case t if t =:= typeOf[Char]   => Some(Parameter.Type.Character)
    case t if t =:= typeOf[String] => Some(Parameter.Type.Character)
    case t if t =:= typeOf[Int]    => Some(Parameter.Type.Integer)
    case t if t =:= typeOf[Byte]   => Some(Parameter.Type.Byte)
    case t if t =:= typeOf[Float]  => Some(Parameter.Type.Float)
    case _ => None
  }

  
  /** Untyped parameter, without a connected group.
    *
    * Normal [[Parameter]]s are contained as a sequence within their owning group.  However, when reading parameters
    * from the file, they are associated with their group using its group ID.  This "unassociated" parameter type
    * represents a parameter without being embedded in its group, yet still containing the group ID.  Further, normal
    * parameters are typed, whereas this class sees its data as an `IndexedSeq[Byte]`.
    * 
    * @param block parameter section block corresponding to a parameter
    */
  private [io] final class UntypedParameter(val block: FormattedByteIndexedSeq) {
    private def nName: Int = abs(block(0))                                    // # of characters in name
    private def nDesc: Int = block(6 + nName + nDims + data.length) & 0xFF    // # of characters in description
    private def descOfs: Int = 7 + nName + nDims + data.length                // offset to the start of the description
    private def nElem: Int = abs(byteLengthPerElement)                        // # of bytes in an element of the data
    private def nDims: Int = block(5 + nName)                                 // number of dimensions
    def name: String = block.slice(2, 2 + nName).map(_.toChar).mkString
    def description: String = if (nDesc > 0) block.slice(descOfs, descOfs + nDesc).map(_.toChar).mkString else ""
    def groupId: Int = { assert(block(1) > 0); block(1) }
    def dimensions: IndexedSeq[Int] = {
      if (nDims == 0) { // handle the case of a "scalar" dimension, where nDims is recorded as zero in the C3D file
        UntypedParameter.ScalarDimension
      } else { // handle a normal array (can still be a "scalar", or a multi-dimensional array)
        new IndexedSeq[Int] {
          def length: Int = nDims
          def apply(idx: Int): Int = block(6 + nName + idx) & 0xFF
        }
      }
    }
    def byteLengthPerElement: Int = block(4 + nName)
    def data: IndexedSeq[Byte] = new IndexedSeq[Byte] {
      def length: Int = dimensions.product * nElem
      def apply(idx: Int): Byte = block(6 + nName + nDims + idx)
    }
    def isLocked: Boolean = block(0) < 0
    def asUnassociatedParameter: UnassociatedParameter[_] = {
      byteLengthPerElement match {
        case -1 => new UnassociatedParameter[Char](this)
        case  1 => new UnassociatedParameter[Byte](this)
        case  2 => new UnassociatedParameter[Int](this)
        case  4 => new UnassociatedParameter[Float](this)
      }
    }
  }
  private [io] object UntypedParameter {
    /** This object is `IndexedSeq[Int](1)`, used to represent scalars. */
    object ScalarDimension extends IndexedSeq[Int] {
      val length: Int = 1
      def apply(idx: Int): Int = {
        if (idx != 0) throw new IndexOutOfBoundsException("scalar parameter: idx must be 0")
        1
      }
    }
  }

  
  /** Unassociated parameter, without a connected group, but with typing information.
    * 
    * Normal [[Parameter]]s are contained as a sequence within their owning group.  However, when reading parameters
    * from the file, they are associated with their group using its group ID.  This "unassociated" parameter type
    * represents a parameter without being embedded in a group, yet still containing the group ID.
    *
    * This class functions as a kind of second-stage interpretation of a group, after it has already been read using
    * the [[UntypedParameter]] class.
    *
    * @param uParam untyped parameter
    */
  private [io] final class UnassociatedParameter[T:TypeTag](uParam: UntypedParameter) {
    def name: String = uParam.name
    def description: String = uParam.description
    def groupId: Int = uParam.groupId
    def dimensions: IndexedSeq[Int] = uParam.dimensions
    def parameterType: Parameter.Type = typeToParameterType[T].get
    def data: IndexedSeq[T] = {
      import Parameter.Type
      val typ: Parameter.Type = typeToParameterType[T].get
      val iSeq: IndexedSeq[_] = typ match {
        case Type.Byte      => uParam.data
        case Type.Character => UnassociatedParameter.ByteToCharIndexedSeq(uParam.data)
        case Type.Integer   => UnassociatedParameter.ByteToIntIndexedSeq(uParam.data, uParam.block.binaryFormat)
        case Type.Float     => UnassociatedParameter.ByteToFloatIndexedSeq(uParam.data, uParam.block.binaryFormat)
        case _ => throw new AssertionError("unexpected parameter type (should be Byte, Char, Int or Float)")
      }
      iSeq.asInstanceOf[IndexedSeq[T]]
    }
    def isLocked: Boolean = uParam.isLocked

    // check that the number of bytes per element matches the expected values
    {
      val expectedByteLengthPerElement: Int = typeOf[T] match {
        case t if t =:= typeOf[Char]  => -1
        case t if t =:= typeOf[Byte]  =>  1
        case t if t =:= typeOf[Int]   =>  2
        case t if t =:= typeOf[Float] =>  4
      }
      require(expectedByteLengthPerElement == uParam.byteLengthPerElement,
        s"expected ${expectedByteLengthPerElement} bytes per element, but found ${uParam.byteLengthPerElement}")
      require(uParam.data.length % uParam.byteLengthPerElement == 0,
        "length of data is not an even multiple of the number of bytes per element")
    }
  }
  private [io] object UnassociatedParameter {
    /** Interprets an `IndexedSeq[Byte]` as an `IndexedSeq[Char]`. */
    final case class ByteToCharIndexedSeq(bis: IndexedSeq[Byte]) extends IndexedSeq[Char] {
      def length: Int = bis.length
      def apply(idx: Int): Char = bis(idx).toChar
    }
    /** Interprets an `IndexedSeq[Byte]` as an `IndexedSeq[Int]`. */
    final case class ByteToIntIndexedSeq(bis: IndexedSeq[Byte], bf: BinaryFormat) extends IndexedSeq[Int] {
      val length: Int = bis.length / 2
      def apply(idx: Int): Int = { val i0 = 2 * idx; bf.bytesToInt(bis(i0), bis(i0+1)) }
    }
    /** Interprets an `IndexedSeq[Byte]` as an `IndexedSeq[Float]`. */
    final case class ByteToFloatIndexedSeq(bis: IndexedSeq[Byte], bf: BinaryFormat) extends IndexedSeq[Float] {
      val length: Int = bis.length / 4
      def apply(idx: Int): Float = { val i0 = 4 * idx; bf.bytesToFloat(bis(i0), bis(i0+1), bis(i0+2), bis(i0+3)) }
    }
  }

  
  /** Concrete case-class implementation of a C3D Group. */
  private [io] final case class ReadGroup(
    name:        String, 
    description: String, 
    isLocked:    Boolean, 
    parameters:  Seq[Parameter[_]]
  ) extends Group {

    def getParameter[T:TypeTag](parameter: String, signed: ParameterSign = ParameterSign.Default,
      signConventions: ParameterSignConventions = ParameterSign.DefaultParameterSignConventions): Option[Parameter[T]] =
    {
      synchronized {
      parameters.find(_.name.toUpperCase == parameter.toUpperCase) flatMap { p: Parameter[_] =>
        val expectedType: Option[Parameter.Type] = ParamSectionReader.typeToParameterType[T]
        expectedType.flatMap { t =>
          if (p.parameterType == t) {
            if (typeOf[T] == typeOf[String]) { // special handling for strings
              val charParam: Parameter[Char] = p.asInstanceOf[Parameter[Char]]
              Some(StringParameter(charParam).asInstanceOf[Parameter[T]])
            } else if (typeOf[T] == typeOf[Int]) { // special handling for signed vs unsigned ints
              val sign: ParameterSign = {
                if (signed == ParameterSign.Default) signConventions.signForParameter(name, parameter) else signed
              }
              assert(sign != ParameterSign.Default, "Default parameter sign specified, but default value not found" +
                s" in ParameterSignConventions for ${name.toUpperCase}:${parameter.toUpperCase}.")
              if (sign == ParameterSign.Signed)
                Some(p.asInstanceOf[Parameter[T]])
              else
                Some((new UIntParameter(p.asInstanceOf[Parameter[Int]])).asInstanceOf[Parameter[T]])
            } else {
              Some(p.asInstanceOf[Parameter[T]])
            }
          } else {
            None
          }
        }
      }
      }
    }
    
  }

  
  /** Concrete case-class implementation of a C3D Parameter. */
  private [io] final case class ReadParameter[T](
    name:          String, 
    description:   String, 
    isLocked:      Boolean, 
    dimensions:    IndexedSeq[Int],
    data:          IndexedSeq[T],
    parameterType: Parameter.Type
  ) extends Parameter[T] with ParameterTemplate[T]

  
  /** Reads in the parameter section groups. */
  private [io] def readGroups(paramISeq: FormattedByteIndexedSeq): Seq[Group] = {
    
    // find byte blocks corresponding with groups and parameters
    val (groupBlocks, paramBlocks) = partitionToGroupsAndParams(chunkGroupsAndParams(paramISeq))
    
    // construct unassociated groups and parameters (not connected to each other yet)
    val uGroups = groupBlocks.map(new UnassociatedGroup(_))
    val uParams = paramBlocks.map(new UntypedParameter(_).asUnassociatedParameter)

    // for each group, collect its associated parameters
    def getParamsForGroup(g: UnassociatedGroup): Seq[Parameter[_]] = {
      for (p <- uParams if p.groupId == g.id) yield ReadParameter(
        p.name, p.description, p.isLocked,
        p.dimensions,
        p.data,
        p.parameterType
      )
    }
    val groupSeq = for (g <- uGroups) yield ReadGroup(g.name, g.description, g.isLocked, getParamsForGroup(g))
    
    groupSeq
  }

  
  /** Concrete case-class implementation of a C3D ParameterSection. */
  private[io] final case class ReadParameterSection(
    val groups: Seq[Group],
    val processorType: ProcessorType) extends ParameterSection {

    def getParameter[T: TypeTag](
      groupName: String,
      parameterName: String,
      signed: ParameterSign = ParameterSign.Default,
      signConventions: ParameterSignConventions = ParameterSign.DefaultParameterSignConventions): Option[Parameter[T]] =
      {
        val groupNameUpper = groupName.toUpperCase
        for {
          group <- groups.find(_.name.toUpperCase == groupNameUpper)
          param <- group.getParameter[T](parameterName, signed, signConventions)
        } yield param
      }

    def requiredParameters: RequiredParameters = new RequiredParameters {
      private def getRequiredParameter[T: TypeTag](groupName: String, parameterName: String): Parameter[T] = {
        getParameter(groupName, parameterName).getOrElse(
          throw RequiredParameterNotFoundException(groupName, parameterName))
      }
      private def getRequiredScalar[T: TypeTag](groupName: String, parameterName: String): T = {
        getRequiredParameter(groupName, parameterName).apply(0)
      }
      lazy val pointDataStart: Int = getRequiredScalar[Int]("POINT", "DATA_START")
      lazy val pointRate: Float = getRequiredScalar[Float]("POINT", "RATE")
      lazy val pointFrames: Int = getRequiredScalar[Int]("POINT", "FRAMES")
      lazy val pointUsed: Int = getRequiredScalar[Int]("POINT", "USED")
      lazy val pointScale: Float = getRequiredScalar[Float]("POINT", "SCALE")
      lazy val pointLabels: Parameter[String] = getRequiredParameter[String]("POINT", "LABELS")
      lazy val pointDescriptions: Parameter[String] = getRequiredParameter[String]("POINT", "DESCRIPTIONS")
      lazy val analogRate: Float = getRequiredScalar[Float]("ANALOG", "RATE")
      lazy val analogUsed: Int = getRequiredScalar[Int]("ANALOG", "USED")
      lazy val analogGenScale: Float = getRequiredScalar[Float]("ANALOG", "GEN_SCALE")
      lazy val analogFormat: String = getParameter[String]("ANALOG", "FORMAT").map(_(0)).getOrElse("SIGNED")
      lazy val analogScale: Parameter[Float] = getRequiredParameter[Float]("ANALOG", "SCALE")
      lazy val analogOffset: Parameter[Int] = getRequiredParameter[Int]("ANALOG", "OFFSET")
      lazy val analogLabels: Parameter[String] = getRequiredParameter[String]("ANALOG", "LABELS")
      lazy val analogDescriptions: Parameter[String] = getRequiredParameter[String]("ANALOG", "DESCRIPTIONS")
    }

  }
  
  
  /** Read in the entire parameter section. */
  private [io] def read(paramISeq: FormattedByteIndexedSeq, processorType: ProcessorType): ParameterSection = {
    val groups = readGroups(paramISeq)
    ReadParameterSection(groups, processorType)
  }  
  
}

