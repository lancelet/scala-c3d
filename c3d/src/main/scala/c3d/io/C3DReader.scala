package c3d.io

import java.io.File
import c3d._
import scala.collection.immutable._
import scala.reflect.runtime.universe._
import Util.b
import c3d.io.collection.ImmutableArray

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
  
  
  /** Size of a section in a C3D file = 512 bytes. */
  private [io] final val sectionSize: Int = 512
  
  
  /** Fetches the parameter section from the file. */
  private [io] def getParameterSection(wholeFile: ImmutableArray[Byte]): (FormattedByteIndexedSeq, ProcessorType) = {
    
    val section: ImmutableArray[Byte] = {
      val ofs  = (wholeFile(0) - 1) * sectionSize  // first byte is 1-based offset of parameter section
      val nsec = wholeFile(ofs + 2)                // 3rd byte of parameter section is the number of sections
      val until = ofs + (sectionSize * nsec)
      wholeFile.slice(ofs, until)
    }
 
    val processorType: ProcessorType = ProcessorTypeIO.byteToProcessorType(section(3)).getOrElse(
       throw C3DIOException("Unknown processor type byte.")
    )
    
    val binaryFormat = BinaryFormat.fromProcessorType(processorType)
    (new FormattedByteIndexedSeq(section, binaryFormat), processorType)
    
  }

  
  /** Fetches the data section of the file (3D point data + analog data). */
  private[io] def getDataSection(wholeFile: ImmutableArray[Byte],
                                 parameterSection: ParameterSection): FormattedByteIndexedSeq =
  {
    val rp = parameterSection.requiredParameters

    // figure out total size of data section
    val totalDataSectionBytes: Int = {
      val itemSizeInBytes: Int = {
        val usesFloat: Boolean = rp.pointScale < 0.0f
        if (usesFloat) 4 else 2
      }
      val nAnalogPer3DFrame: Int = (rp.analogRate / rp.pointRate).toInt
      val pointBytesPerFrame: Int = rp.pointUsed * 4 * itemSizeInBytes
      val analogBytesPerFrame: Int = nAnalogPer3DFrame * rp.analogUsed * itemSizeInBytes
      rp.pointFrames * (pointBytesPerFrame + analogBytesPerFrame)
    }

    // slice region corresponding to the data section
    val startIndex: Int = (rp.pointDataStart - 1) * sectionSize
    val slice: ImmutableArray[Byte] = wholeFile.slice(startIndex, startIndex + totalDataSectionBytes)

    // convert to a FormattedByteIndexedSeq
    val binaryFormat = BinaryFormat.fromProcessorType(parameterSection.processorType)
    new FormattedByteIndexedSeq(slice, binaryFormat)

  }
  

  /** Concrete case-class implementation of a C3D top-level object. */
  private [io] final case class ReadC3D(
    source: String,
    parameterSection: ParameterSection,
    dataSection: FormattedByteIndexedSeq) extends C3D
  {
    val analog: Analog = AnalogReader(parameterSection, dataSection)
    val platforms: Platforms = PlatformReader(parameterSection, analog)
    val points: Points = PointsReader(parameterSection, dataSection)
  }

  /** Reads a C3D file from a `File`.
    * 
    * @param file C3D `File`.
    * return the C3D file that has been read
    */
  def read(file: File): C3D = {
    FileUtils.fileToIndexedSeq(file) match {
      case scala.util.Success(c3dArray) => read(file.getCanonicalFile().getName, c3dArray)
      case scala.util.Failure(e)        => throw e
    }
  }

  /** Reads a C3D file from an `IndexedSeq[Byte]`.
    * 
    * @param source source (file) information
    * @param c3dArray `IndexedSeq[Byte]` representing the entire C3D file.
    * @return the C3D file that has been read
    */
  def read(source: String, c3dArray: ImmutableArray[Byte]): C3D = {
    // read groups and parameters (the parameter section)
    val (paramISeq, processorType) = getParameterSection(c3dArray)
    val parameterSection = ParamSectionReader.read(paramISeq, processorType)
    val dataSection = getDataSection(c3dArray, parameterSection)

    // assemble C3D object
    ReadC3D(source, parameterSection, dataSection)
  }

}
