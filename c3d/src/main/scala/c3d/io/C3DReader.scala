package c3d.io

import java.io.File
import c3d.{C3D, ForcePlate, Group, Parameter, ParameterSign, ParameterSignConventions, ProcessorType, Vec3D}
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
      pointRate: Float   <- pointGroup.getParameter[Float]("RATE").map(_(0))
      pointFrames: Int   <- pointGroup.getParameter[Int]("FRAMES").map(_(0))
      pointUsed: Int     <- pointGroup.getParameter[Int]("USED").map(_(0))
      pointScale: Float  <- pointGroup.getParameter[Float]("SCALE").map(_(0))
      analogRate: Float  <- analogGroup.getParameter[Float]("RATE").map(_(0))
      analogUsed: Int    <- analogGroup.getParameter[Int]("USED").map(_(0))
    } yield {
      // figure out the total size of the data section
      val usesFloat: Boolean = pointScale < 0.0f  // when POINT:SCALE < 0 it indicates that a float format is used
      val itemSizeInBytes: Int = if (usesFloat) 4 else 2  // 2 bytes for ints, 4 bytes for floats
      val nAnalogPer3DFrame: Int = (analogRate / pointRate).toInt
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
  private [io] final case class ReadC3D(groups: Seq[Group], override val processorType: ProcessorType,
    dataSection: FormattedByteIndexedSeq) extends C3D
  {

    def getParameter[T:TypeTag](group: String, parameter: String,
      signed: ParameterSign, signConventions: ParameterSignConventions): Option[Parameter[T]] = 
    {
      groups.find { // find the named group
        _.name.toUpperCase == group.toUpperCase
      } flatMap { g: Group => 
        g.getParameter[T](parameter, signed, signConventions)
      }
    }

    def getAnalogChannel(channelIndex: Int): IndexedSeq[Float] = {
      require(channelIndex >= 0 && channelIndex < analogUsed)
      new IndexedSeq[Float] {
        private val scale: Float = analogGenScale * analogScale(channelIndex)
        private val offset: Float = analogOffset(channelIndex)
        def length: Int = totalAnalogSamples
        def apply(index: Int): Float = {
          val dataByteIndex: Int = (dataStride * index) + ((4 + channelIndex) * dataItemSize)
          if (usesFloat) {
            // floating point values
            (dataSection.floatAt(dataByteIndex) - offset) * scale
          } else {
            // integer values
            if (analogFormat == "SIGNED") {
              (dataSection.intAt(dataByteIndex) - offset) * scale
            } else {
              (dataSection.uintAt(dataByteIndex) - offset) * scale
            }
          }
        }
      }
    }

    def analogSamplingRate: Float = analogRate

    def forcePlates: IndexedSeq[ForcePlate] = fPlates
    private val fPlates: IndexedSeq[ForcePlate] = new IndexedSeq[ForcePlate] {
      def length: Int = getParameter[Int]("FORCE_PLATFORM", "USED").map(_(0)).getOrElse(0)
      def apply(i: Int): ForcePlate = new ForcePlate {
        private val typ: Int = getPNoFail[Int]("FORCE_PLATFORM", "TYPE").apply(i)
        assert(typ == 4, "Only type 4 force plates are supported so far.")
        private val channels: Parameter[Int] = getPNoFail[Int]("FORCE_PLATFORM", "CHANNEL")
        private val cFx: IndexedSeq[Float] = getAnalogChannel(channels(i, 0))
        private val cFy: IndexedSeq[Float] = getAnalogChannel(channels(i, 1))
        private val cFz: IndexedSeq[Float] = getAnalogChannel(channels(i, 2))
        private val cMx: IndexedSeq[Float] = getAnalogChannel(channels(i, 3))
        private val cMy: IndexedSeq[Float] = getAnalogChannel(channels(i, 4))
        private val cMz: IndexedSeq[Float] = getAnalogChannel(channels(i, 5))
        private val cm: Parameter[Float] = getPNoFail[Float]("FORCE_PLATFORM", "CAL_MATRIX")
        private val m: IndexedSeq[IndexedSeq[Float]] = IndexedSeq(
          IndexedSeq(cm(i,0,0), cm(i,0,1), cm(i,0,2), cm(i,0,3), cm(i,0,4), cm(i,0,5)),
          IndexedSeq(cm(i,1,0), cm(i,1,1), cm(i,1,2), cm(i,1,3), cm(i,1,4), cm(i,1,5)),
          IndexedSeq(cm(i,2,0), cm(i,2,1), cm(i,2,2), cm(i,2,3), cm(i,2,4), cm(i,2,5)),
          IndexedSeq(cm(i,3,0), cm(i,3,1), cm(i,3,2), cm(i,3,3), cm(i,3,4), cm(i,3,5)),
          IndexedSeq(cm(i,4,0), cm(i,4,1), cm(i,4,2), cm(i,4,3), cm(i,4,4), cm(i,4,5)),
          IndexedSeq(cm(i,5,0), cm(i,5,1), cm(i,5,2), cm(i,5,3), cm(i,5,4), cm(i,5,5))
        )
        def getForceVector(sampleIndex: Int): Vec3D = new Vec3D {
          val fvec: (Float, Float, Float) = {
            val fx = cFx(sampleIndex)
            val fy = cFy(sampleIndex)
            val fz = cFz(sampleIndex)
            val mx = cMx(sampleIndex)
            val my = cMy(sampleIndex)
            val mz = cMz(sampleIndex)
            val mfx = m(0)(0)*fx + m(0)(1)*fy + m(0)(2)*fz + m(0)(3)*mx + m(0)(4)*my + m(0)(5)*mz
            val mfy = m(1)(0)*fx + m(1)(1)*fy + m(1)(2)*fz + m(1)(3)*mx + m(1)(4)*my + m(1)(5)*mz
            val mfz = m(2)(0)*fx + m(2)(1)*fy + m(2)(2)*fz + m(2)(3)*mx + m(2)(4)*my + m(2)(5)*mz
            (mfx, mfy, mfz)
          }
          val x = fvec._1
          val y = fvec._2
          val z = fvec._3
        }
      }
    }

    // Retrieves parameters that are absolutely required
    private def getPNoFail[T:TypeTag](group: String, parameter: String): Parameter[T] = {
      getParameter[T](group, parameter).getOrElse {
        // TODO: Replace with nicer exception
        throw new IllegalArgumentException(
          s"${group.toUpperCase}:${parameter.toUpperCase} parameter not found"
        )
      }
    }
    // parameters that should definitely be present
    private lazy val pointScale:     Float  = getPNoFail[Float]("POINT", "SCALE").apply(0)
    private lazy val pointRate:      Float  = getPNoFail[Float]("POINT", "RATE").apply(0)
    private lazy val pointUsed:      Int    = getPNoFail[Int]("POINT", "USED").apply(0)
    private lazy val analogRate:     Float  = getPNoFail[Float]("ANALOG", "RATE").apply(0)
    private lazy val analogUsed:     Int    = getPNoFail[Int]("ANALOG", "USED").apply(0)
    private lazy val analogGenScale: Float  = getPNoFail[Float]("ANALOG", "GEN_SCALE").apply(0)
    private lazy val analogFormat:   String = getPNoFail[String]("ANALOG", "FORMAT").apply(0)
    private lazy val analogScale: Parameter[Float] = getPNoFail[Float]("ANALOG", "SCALE")
    private lazy val analogOffset: Parameter[Int] = getPNoFail[Int]("ANALOG", "OFFSET") // TODO: OFFSET CAN BE INT
    // things derived from the parameters
    private lazy val usesFloat: Boolean = pointScale < 0.0
    private lazy val dataItemSize: Int = if (usesFloat) 4 else 2
    private lazy val analogSamplesPer3DFrame: Int = (analogRate / pointRate).toInt
    private lazy val dataStride: Int = (pointUsed * 4 + analogSamplesPer3DFrame * analogUsed) * dataItemSize
    private lazy val totalAnalogSamples: Int = {
      val tas = analogSamplesPer3DFrame * pointUsed
      println(s"totalAnalogSamples = $tas")
      tas
    }
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
    val dataSecV: Validation[String, FormattedByteIndexedSeq] = 
      processorTypeV flatMap { processorType =>
        groupsV flatMap { groups =>
          dataSectionIndexedSeq(c3dISeq, groups, processorType)
        }
      }

    // assemble the C3D object
    for {
      groups: Seq[Group] <- groupsV
      processorType: ProcessorType <- processorTypeV
      data: FormattedByteIndexedSeq <- dataSecV
    } yield {
      ReadC3D(groups, processorType, data)
    }
  }

}
