package c3d.io

import java.io.File
import c3d.{C3D, Group, Parameter, ParameterSign, ParameterSignConventions, ProcessorType}
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

  /** Returns the `FormattedByteIndexedSeq` corresponding to the data (3D Point data + Analog data) section.
    * 
    * @param c3dISeq `IndexedSeq` corresponding to the entire C3D file
    * @param groups `Seq` of `Group`s corresponding to the groups of parameters in the file
    * @param processorType processor type for the file
    * @return `IndexedSeq` containing entire data
    */
  private [io] def dataSectionIndexedSeq(c3dISeq: IndexedSeq[Byte], groups: Seq[Group], processorType: ProcessorType): 
      Validation[String, FormattedByteIndexedSeq] = 
  {
    val optionResult: Option[FormattedByteIndexedSeq] = for {
      pointGroup: Group  <- groups.find(_.name.toUpperCase == "POINT")
      analogGroup: Group <- groups.find(_.name.toUpperCase == "ANALOG")
      pointStart: Int    <- pointGroup.getParameter[Int]("DATA_START").map(_(0))
      pointRate: Int     <- pointGroup.getParameter[Int]("RATE").map(_(0))
      pointFrames: Int   <- pointGroup.getParameter[Int]("FRAMES").map(_(0))
      pointUsed: Int     <- pointGroup.getParameter[Int]("USED").map(_(0))
      pointScale: Float  <- pointGroup.getParameter[Float]("SCALE").map(_(0))
      analogRate: Int    <- analogGroup.getParameter[Int]("RATE").map(_(0))
      analogUsed: Int    <- analogGroup.getParameter[Int]("USED").map(_(0))
    } yield {
      // figure out the total size of the data section
      val usesFloat: Boolean = pointScale < 0.0f  // when POINT:SCALE < 0 it indicates that a float format is used
      val itemSizeInBytes: Int = if (usesFloat) 2 else 4  // 2 bytes for ints, 4 bytes for floats
      assert(analogRate % pointRate == 0, "POINT:RATE should divide evenly into ANALOG:RATE")
      val nAnalogPer3DFrame: Int = analogRate / pointRate
      val ptPayloadPerFrame: Int = pointUsed * 4 * itemSizeInBytes
      val analogPayloadPerFrame: Int = nAnalogPer3DFrame * analogUsed * itemSizeInBytes
      val totalBytes = pointFrames * (ptPayloadPerFrame + analogPayloadPerFrame)

      // snip out the section corresponding to all of the data
      val startIndex: Int = (pointStart - 1) * 512
      val slicedSequence: IndexedSeq[Byte] = c3dISeq.slice(startIndex, startIndex + totalBytes)

      // convert to a FormattedByteIndexedSeq
      val binaryFormat = BinaryFormat.fromProcessorType(processorType)
      new FormattedByteIndexedSeq(slicedSequence, binaryFormat)
    }
    // convert to a scalaz.Validation
    optionResult match {
      case Some(r) => Success(r)
      case None    => Failure("Could not retrieve data section.")
    }
  }

  /** Concrete case-class implementation of a C3D top-level object. */
  private [io] final case class ReadC3D(groups: Seq[Group], override val processorType: ProcessorType) extends C3D {

    def getParameter[T:TypeTag](group: String, parameter: String,
      signed: ParameterSign, signConventions: ParameterSignConventions): Option[Parameter[T]] = 
    {
      groups.find { // find the named group
        _.name.toUpperCase == group.toUpperCase
      } flatMap { g: Group => 
        g.getParameter[T](parameter, signed, signConventions)
      }
    }

    def getAnalogChannel(index: Int): IndexedSeq[Float] = IndexedSeq.empty[Float] // TODO: Complete method

  }

  /** Reads a C3D file from a `File`.
    * 
    * @param file C3D `File`.
    * return the C3D file that has been read
    */
  def read(file: File): Validation[String, C3D] = {
    FileUtils.fileToIndexedSeq(file) match {
      case scala.util.Success(c3dISeq) => read(c3dISeq)
      case scala.util.Failure(e) => Failure(e.toString)
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
