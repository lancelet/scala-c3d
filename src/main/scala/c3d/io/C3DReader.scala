package c3d.io

import c3d.{C3D, Group, Parameter, ProcessorType, StringParameter}
import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz.{Failure, Success, Validation}
import scalaz.std.AllInstances._
import Util.b

object C3DReader {

  /** Checks for a C3D file magic byte.
    * 
    * The second byte of all C3D files should be 0x50.  This function returns `true` if `iseq` has the correct byte,
    * and `false` otherwise.
    * 
    * @param c3dISeq entire C3D file
    * @return true if the magic byte is present
    */
  private [io] def hasMagicByte(c3dISeq: IndexedSeq[Byte]): Boolean = (c3dISeq.length >= 2) && (c3dISeq(1) == b(0x50))

  /** Returns the `FormattedByteIndexedSeq` containing the parameter section.
    * 
    * This method fetches a `FormattedByteIndexedSeq` which corresponds to the entire parameter section.
    * Additionally, the method checks within the parameter section to find out the processor type being used.
    * 
    * The first byte of the file contains a 1-based offset to the start of the parameter section.  This offset is 
    * specified as a multiple of 512-byte blocks.  Then, within the parameter section itself, the 3rd byte specifies 
    * the total number of 512-byte parameter blocks in the file.  Within the parameter section header, the processor
    * type is also specified.
    * 
    * @param c3dISeq entire C3D file
    * @return parameter section `IndexedSeq[Byte]`
    */
  private [io] def paramSectionIndexedSeq(c3dISeq: IndexedSeq[Byte]): Validation[String, FormattedByteIndexedSeq] = {
    try {
      val paramSecByteOffset = (c3dISeq(0) - 1) * 512  // first byte of file is 1-based parameter section offset
      val nParamSections = c3dISeq(paramSecByteOffset + 2)  // 3rd byte of parameter section = # of param sections
      val until = paramSecByteOffset + 512 * nParamSections
      val iseq = c3dISeq.slice(paramSecByteOffset, until) // pick out the param section
      for {
        processorType <- ParamSectionReader.processorType(iseq)
      } yield {
        val binaryFormat = BinaryFormat.fromProcessorType(processorType)
        new FormattedByteIndexedSeq(iseq, binaryFormat)
      }
    } catch {
      // most probable failure is an attempt to access indices outside of the bounds of the file, if any of the
      //  pointers happen to be corrupted.
      case ex @ (_:IndexOutOfBoundsException | _:SliceException) => 
        Failure("could not return the portion of the file corresponding to the parameter section")
    }
  }

  /** Concrete case-class implementation of a C3D top-level object. */
  private [io] final case class ReadC3D(groups: Seq[Group], override val processorType: ProcessorType) extends C3D {

    def getParameter[T:TypeTag](group: String, parameter: String): Option[Parameter[T]] = {
      groups.find { // find the named group
        _.name.toUpperCase == group.toUpperCase
      } flatMap { g: Group => // find the named parameter
        g.parameters.find(_.name.toUpperCase == parameter.toUpperCase)
      } flatMap { p: Parameter[_] => // check that the parameter type conforms with that expected
        // handle string parameters by searching for a Parameter[Char] first
        val expectedType: Type = if (typeOf[T] == typeOf[String]) typeOf[Char] else typeOf[T]
        if (p.parameterType == expectedType) {
          if (typeOf[T] == typeOf[String]) {  // special handling for strings
            val charParam: Parameter[Char] = p.asInstanceOf[Parameter[Char]]
            Some(StringParameter(charParam).asInstanceOf[Parameter[T]])
          } else {
            Some(p.asInstanceOf[Parameter[T]])
          }
        } else {
          None
        }
      }
    }

  }

  /** Reads a C3D file from an `IndexedSeq[Byte]`.
    * 
    * @param c3dISeq `IndexedSeq[Byte]` representing the entire C3D file.
    * @return the C3D file that has been read
    */
  def read(c3dISeq: IndexedSeq[Byte]): Validation[String, C3D] = {
    // read groups and parameters (the parameter section)
    val paramSecISeqV = paramSectionIndexedSeq(c3dISeq)
    val processorTypeV = paramSecISeqV flatMap { ParamSectionReader.processorType _ }
    val groupsV = paramSecISeqV flatMap { ParamSectionReader.read _ }

    // assemble the C3D object
    for {
      groups: Seq[Group] <- groupsV
      processorType: ProcessorType <- processorTypeV
    } yield {
      ReadC3D(groups, processorType)
    }
  }

}
