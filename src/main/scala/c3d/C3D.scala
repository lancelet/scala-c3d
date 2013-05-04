package c3d

import scala.collection.immutable._

trait Parameter[T] {
  def name: String
  def description: String
  def dimensions: IndexedSeq[Int]
  def data: IndexedSeq[T]
  def apply(idx: IndexedSeq[Int]): T
}

trait Group {
  def name: String
  def description: String
  def parameters: Set[Parameter[_]]
}

trait C3D {
  def groups: Set[Group]
}
