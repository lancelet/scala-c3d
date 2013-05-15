package c3d.io

import scala.collection.immutable._
import scalaz.{Failure, Success, Validation}
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
      case iob: IndexOutOfBoundsException => 
        Failure("could not return the portion of the file corresponding to the parameter section")
    }
  }

}
