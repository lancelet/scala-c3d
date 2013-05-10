package c3d.io

import scala.collection.immutable._
import scalaz.{Failure, Success, Validation}
import c3d.ProcessorType
import Util.b

private [io] object ParamSectionReader {

  /** Returns the [[ProcessorType]] from a parameter section.
    * 
    * @param paramISeq `IndexedSeq[Byte]` corresponding to the parameter section
    * @return [[ProcessorType]] used in the parameter section
    */
  private [io] def processorType(paramISeq: IndexedSeq[Byte]): Validation[String, ProcessorType] = {
    try {
      val processorByte: Byte = paramISeq(3)
      ProcessorTypeIO.byteToProcessorType(processorByte) map {
        Success(_)
      } getOrElse {
        Failure(f"unknown processor type byte $processorByte%d")
      }
    } catch {
      case ioe: IndexOutOfBoundsException =>
        Failure("parameter section too small to contain processor type byte")
    }
  }

}

