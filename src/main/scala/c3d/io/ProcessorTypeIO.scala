package c3d.io

import scala.collection.immutable._
import c3d.ProcessorType
import Util.b

private [io] object ProcessorTypeIO {

  private val bytesToProcessors: Map[Byte, ProcessorType] = Map(
    b(84) -> ProcessorType.Intel,
    b(85) -> ProcessorType.DEC,
    b(86) -> ProcessorType.SGIMIPS
  )

  /** Returns a [[ProcessorType]] from a `Byte`.
    * 
    * [[ProcessorType]]s are encoded using `Byte`s as follows:
    *   - 84 = Intel
    *   - 85 = DEC
    *   - 86 = SGIMIPS
    * 
    * @param b byte that encodes a [[ProcessorType]]
    * @return [[ProcessorType]] corresponding to the byte provided
    */
  private [io] def byteToProcessorType(b: Byte): Option[ProcessorType] = bytesToProcessors.get(b)

  /** Returns a `Byte` corresponding to a given [[ProcessorType]].
    * 
    * [[ProcessorTypes]]s are encoded using `Byte`s as described in the [[byteToProcessorType]] method.
    *
    * @param p [[ProcessorType]] to be encoded as a `Byte`
    * @return `Byte` corresponding to the [[ProcessorType]] provided
    */
  private [io] def processorTypeToByte(p: ProcessorType): Byte = bytesToProcessors.map(_.swap).get(p).get

}
