package c3d.io

import scalaz.{Failure, Success, Validation}

object C3DReader {


  /** Converts an `Integer` to a `Byte`. */
  private [io] def b(i: Int): Byte = i.toByte


  /** Checks for a C3D file magic byte.
    * 
    * The second byte of all C3D files should be 0x50.  This function returns `true` if `iseq` has the correct byte,
    * and `false` otherwise.
    * 
    * @param c3dISeq entire C3D file
    * @return true if the magic byte is present
    */
  private [io] def hasMagicByte(c3dISeq: IndexedSeq[Byte]): Boolean = (c3dISeq.length >= 2) && (c3dISeq(1) == b(0x50))


  /** Returns the `IndexedSeq[Byte]` containing the parameter section.
    * 
    * The first byte of the file contains a 1-based offset to the start of the parameter section.  This offset is 
    * specified as a multiple of 512-byte blocks.  Then, within the parameter section itself, the 3rd byte specifies 
    * the total number of 512-byte parameter blocks in the file.  This method performs these computations and returns 
    * the `IndexedSeq[Byte]` slice corresponding to the entire parameter section of the file.
    * 
    * @param c3dISeq entire C3D file
    * @return parameter section `IndexedSeq[Byte]`
    */
  private [io] def paramSectionIndexedSeq(c3dISeq: IndexedSeq[Byte]): Validation[String, IndexedSeq[Byte]] = {
    try {
      val paramSecByteOffset = (c3dISeq(0) - 1) * 512  // first byte of file is 1-based parameter section offset
      val nParamSections = c3dISeq(paramSecByteOffset + 2)  // 3rd byte of parameter section = # of param sections
      val until = paramSecByteOffset + 512 * nParamSections
      Success(c3dISeq.slice(paramSecByteOffset, until))
    } catch {
      // most probable failure is an attempt to access indices outside of the bounds of the file, if any of the
      //  pointers happen to be corrupted.
      case iob: IndexOutOfBoundsException => 
        Failure("could not return the portion of the file corresponding to the parameter section")
    }
  }


}
