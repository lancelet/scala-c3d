package c3d

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import scalaz.Validation

trait Parameter[T] {
  def name: String
  def description: String
  def isLocked: Boolean
  def dimensions: IndexedSeq[Int]
  def data: IndexedSeq[T]
  /** Column-major access. */
  def apply(idx: IndexedSeq[Int]): T
  def parameterType: Type
}

trait Group {
  def name: String
  def description: String
  def isLocked: Boolean
  def parameters: Seq[Parameter[_]]
}

trait ProcessorType
object ProcessorType {
  object Intel extends ProcessorType
  object DEC extends ProcessorType
  object SGIMIPS extends ProcessorType
}

trait C3D {
  def groups: Seq[Group]
  def processorType: ProcessorType = ProcessorType.Intel
  def getParameter[T:TypeTag](group: String, parameter: String): Option[Parameter[T]]
}

object C3D {
  def read(c3dISeq: IndexedSeq[Byte]): Validation[String, C3D] = c3d.io.C3DReader.read(c3dISeq)
}
