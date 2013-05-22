package c3d

import java.io.File
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
  def parameterType: Parameter.Type

  def apply(i0: Int, i1: Int): T
  def apply(i0: Int, i1: Int, i2: Int): T
}
object Parameter {
  sealed trait Type
  object Type {
    object String extends Type
    object Character extends Type
    object Byte extends Type
    object Integer extends Type
    object Float extends Type
  }
}

trait Group {
  def name: String
  def description: String
  def isLocked: Boolean
  def parameters: Seq[Parameter[_]]
}

sealed trait ProcessorType
object ProcessorType {
  object Intel extends ProcessorType
  object DEC extends ProcessorType
  object SGIMIPS extends ProcessorType
}

trait C3D {
  def groups: Seq[Group]
  def processorType: ProcessorType = ProcessorType.Intel
  def getParameter[T:TypeTag](group: String, parameter: String, 
    signed: ParameterSign = ParameterSign.Default,
    signConventions: ParameterSignConventions = ParameterSign.DefaultParameterSignConventions): Option[Parameter[T]]
}

object C3D {
  def read(file: File): Validation[String, C3D] = c3d.io.C3DReader.read(file)
  def read(c3dISeq: IndexedSeq[Byte]): Validation[String, C3D] = c3d.io.C3DReader.read(c3dISeq)
}
