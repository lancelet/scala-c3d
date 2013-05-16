package c3d

import scala.collection.immutable._

trait Parameter[T] {
  def name: String
  def description: String
  def isLocked: Boolean
  def dimensions: IndexedSeq[Int]
  def data: IndexedSeq[T]
  def apply(idx: IndexedSeq[Int]): T
}

trait Group {
  def name: String
  def description: String
  def isLocked: Boolean
  def parameters: Set[Parameter[_]]
}

trait ProcessorType
object ProcessorType {
  object Intel extends ProcessorType
  object DEC extends ProcessorType
  object SGIMIPS extends ProcessorType
}

trait C3D {
  def groups: Set[Group]
  def processorType: ProcessorType = ProcessorType.Intel
  def getParameter[T](group: String, parameter: String): Option[Parameter[T]]
}
