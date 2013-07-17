package c3d.io

import java.io.File
import c3d._
import scala.collection.immutable._
import scala.reflect.runtime.universe._
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
  
  
  /** Size of a section in a C3D file = 512 bytes. */
  private [io] final val sectionSize: Int = 512
  
  
  /** Fetches the parameter section from the file. */
  private [io] def getParameterSection(wholeFile: IndexedSeq[Byte]): FormattedByteIndexedSeq = {
    
    val section: IndexedSeq[Byte] = {
      val ofs  = (wholeFile(0) - 1) * sectionSize  // first byte is 1-based offset of parameter section
      val nsec = wholeFile(ofs + 2)                // 3rd byte of parameter section is the number of sections
      val until = ofs + (sectionSize * nsec)
      wholeFile.slice(ofs, until)
    }
 
    val processorType: ProcessorType = ProcessorTypeIO.byteToProcessorType(section(3)).getOrElse(
       throw C3DIOException("Unknown processor type byte.")
    )
    
    val binaryFormat = BinaryFormat.fromProcessorType(processorType)
    new FormattedByteIndexedSeq(section, binaryFormat)
    
  }

  
  /** Fetches the data section of the file (3D point data + analog data). */
  private[io] def getDataSection(
    wholeFile: IndexedSeq[Byte],
    parameterSection: ParameterSection): FormattedByteIndexedSeq = {
    
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
    val slice: IndexedSeq[Byte] = wholeFile.slice(startIndex, startIndex + totalDataSectionBytes)

    // convert to a FormattedByteIndexedSeq
    val binaryFormat = BinaryFormat.fromProcessorType(parameterSection.processorType)
    new FormattedByteIndexedSeq(slice, binaryFormat)

  }
  

  /** Concrete case-class implementation of a C3D top-level object. */
  private [io] final case class ReadC3D(
    parameterSection: ParameterSection,
    dataSection: FormattedByteIndexedSeq) extends C3D
  {

    private val rp = parameterSection.requiredParameters
    
    def getAnalogChannel(channelIndex: Int): IndexedSeq[Float] = {
      require(channelIndex >= 0 && channelIndex < rp.analogUsed)
      new IndexedSeq[Float] {
        private val scale: Float = rp.analogGenScale * rp.analogScale(channelIndex)
        private val offset: Float = rp.analogOffset(channelIndex)
        def length: Int = totalAnalogSamples
        def apply(index: Int): Float = {
          assert((index >= 0) && (index < length), s"index must satisfy: 0 <= index < ${length}")
          val closest3DFrame: Int = index / analogSamplesPer3DFrame
          val rem: Int = index % analogSamplesPer3DFrame
          val dataByteIndex: Int = (dataStride * closest3DFrame) + (4 * rp.pointUsed * dataItemSize) + 
            (analogStride * rem) + (channelIndex * dataItemSize)
          if (usesFloat) {
            // floating point values
            (dataSection.floatAt(dataByteIndex) - offset) * scale
          } else {
            // integer values
            if (rp.analogFormat == "SIGNED") {
              (dataSection.intAt(dataByteIndex) - offset) * scale
            } else {
              (dataSection.uintAt(dataByteIndex) - offset) * scale
            }
          }
        }
      }
    }

    def analogSamplingRate: Float = rp.analogRate

    def forcePlates: IndexedSeq[ForcePlate] = fPlates
    private val fPlates: IndexedSeq[ForcePlate] = new IndexedSeq[ForcePlate] {
      def length: Int = parameterSection.getParameter[Int]("FORCE_PLATFORM", "USED").map(_(0)).getOrElse(0)
      def apply(i: Int): ForcePlate = new ForcePlate {
        private val typ: Int = getPNoFail[Int]("FORCE_PLATFORM", "TYPE").apply(i)
        assert(typ == 4, "Only type 4 force plates are supported so far.")
        private val channels: Parameter[Int] = getPNoFail[Int]("FORCE_PLATFORM", "CHANNEL")
        private val cFx: IndexedSeq[Float] = getAnalogChannel(channels(0,i) - 1)
        private val cFy: IndexedSeq[Float] = getAnalogChannel(channels(1,i) - 1)
        private val cFz: IndexedSeq[Float] = getAnalogChannel(channels(2,i) - 1)
        private val cMx: IndexedSeq[Float] = getAnalogChannel(channels(3,i) - 1)
        private val cMy: IndexedSeq[Float] = getAnalogChannel(channels(4,i) - 1)
        private val cMz: IndexedSeq[Float] = getAnalogChannel(channels(5,i) - 1)
        private val cm: Parameter[Float] = getPNoFail[Float]("FORCE_PLATFORM", "CAL_MATRIX")
        private val m: IndexedSeq[IndexedSeq[Float]] = IndexedSeq(
          IndexedSeq(cm(0,0,i), cm(0,1,i), cm(0,2,i), cm(0,3,i), cm(0,4,i), cm(0,5,i)),
          IndexedSeq(cm(1,0,i), cm(1,1,i), cm(1,2,i), cm(1,3,i), cm(1,4,i), cm(1,5,i)),
          IndexedSeq(cm(2,0,i), cm(2,1,i), cm(2,2,i), cm(2,3,i), cm(2,4,i), cm(2,5,i)),
          IndexedSeq(cm(3,0,i), cm(3,1,i), cm(3,2,i), cm(3,3,i), cm(3,4,i), cm(3,5,i)),
          IndexedSeq(cm(4,0,i), cm(4,1,i), cm(4,2,i), cm(4,3,i), cm(4,4,i), cm(4,5,i)),
          IndexedSeq(cm(5,0,i), cm(5,1,i), cm(5,2,i), cm(5,3,i), cm(5,4,i), cm(5,5,i))
        )
        val force: IndexedSeq[Vec3D] = new IndexedSeq[Vec3D] {
          def length: Int = totalAnalogSamples
          def apply(idx: Int): Vec3D = {
            val fvec: (Float, Float, Float) = {
              val fx = cFx(idx)
              val fy = cFy(idx)
              val fz = cFz(idx)
              val mx = cMx(idx)
              val my = cMy(idx)
              val mz = cMz(idx)
              val afx = m(0)(0)*fx + m(0)(1)*fy + m(0)(2)*fz + m(0)(3)*mx + m(0)(4)*my + m(0)(5)*mz
              val afy = m(1)(0)*fx + m(1)(1)*fy + m(1)(2)*fz + m(1)(3)*mx + m(1)(4)*my + m(1)(5)*mz
              val afz = m(2)(0)*fx + m(2)(1)*fy + m(2)(2)*fz + m(2)(3)*mx + m(2)(4)*my + m(2)(5)*mz
              (afx, afy, afz)
            }
            val xv = fvec._1
            val yv = fvec._2
            val zv = fvec._3
            new Vec3D {
              val x = xv
              val y = yv
              val z = zv
            }
          }
        }
      }
    }

    // Retrieves parameters that are absolutely required
    private def getPNoFail[T:TypeTag](group: String, parameter: String): Parameter[T] = {
      parameterSection.getParameter[T](group, parameter).getOrElse {
        // TODO: Replace with nicer exception
        throw new IllegalArgumentException(
          s"${group.toUpperCase}:${parameter.toUpperCase} parameter not found"
        )
      }
    }
    // things derived from the parameters
    private lazy val usesFloat: Boolean = rp.pointScale < 0.0
    private lazy val dataItemSize: Int = if (usesFloat) 4 else 2
    private lazy val analogSamplesPer3DFrame: Int = (rp.analogRate / rp.pointRate).toInt
    private lazy val dataStride: Int = (rp.pointUsed * 4 + analogSamplesPer3DFrame * rp.analogUsed) * dataItemSize
    private lazy val analogStride: Int = rp.analogUsed * dataItemSize
    private lazy val totalAnalogSamples: Int = analogSamplesPer3DFrame * rp.pointFrames

    def getParameter[T: TypeTag](
        groupName: String, 
        parameterName: String, 
        signed: c3d.ParameterSign,
        signConventions: c3d.ParameterSignConventions): Option[c3d.Parameter[T]] = 
          parameterSection.getParameter(groupName, parameterName, signed, signConventions)
    def groups: scala.collection.immutable.Seq[c3d.Group] = parameterSection.groups
    def processorType: c3d.ProcessorType = parameterSection.processorType
    def requiredParameters: RequiredParameters = parameterSection.requiredParameters
  }

  /** Reads a C3D file from a `File`.
    * 
    * @param file C3D `File`.
    * return the C3D file that has been read
    */
  def read(file: File): C3D = {
    FileUtils.fileToIndexedSeq(file) match {
      case scala.util.Success(c3dISeq) => read(c3dISeq)
      case scala.util.Failure(e)       => throw e
    }
  }

  /** Reads a C3D file from an `IndexedSeq[Byte]`.
    * 
    * @param c3dISeq `IndexedSeq[Byte]` representing the entire C3D file.
    * @return the C3D file that has been read
    */
  def read(c3dISeq: IndexedSeq[Byte]): C3D = {
    // read groups and parameters (the parameter section)
    val parameterSection = ParamSectionReader.read(getParameterSection(c3dISeq))
    val dataSection = getDataSection(c3dISeq, parameterSection)

    // assemble C3D object
    ReadC3D(parameterSection, dataSection)
  }

}
