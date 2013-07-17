package c3d

import java.io.File
import scala.collection.immutable._
import scala.reflect.runtime.universe._

trait Parameter[T] {
  def name: String
  def description: String
  def isLocked: Boolean
  def dimensions: IndexedSeq[Int]
  def data: IndexedSeq[T]
  /** Column-major access. */
  def apply(idx: IndexedSeq[Int]): T
  def parameterType: Parameter.Type

  def apply(i0: Int): T
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
  def getParameter[T: TypeTag](
    parameter: String,
    signed: ParameterSign = ParameterSign.Default,
    signConventions: ParameterSignConventions = ParameterSign.DefaultParameterSignConventions): Option[Parameter[T]]
}

trait RequiredParameters {
  def pointDataStart: Int
  def pointRate: Float
  def pointFrames: Int
  def pointUsed: Int
  def pointScale: Float
  def analogRate: Float
  def analogUsed: Int
  def analogGenScale: Float
  def analogFormat: String
  def analogScale: Parameter[Float]
  def analogOffset: Parameter[Int]
  def analogLabels: Parameter[String]
  def analogDescriptions: Parameter[String]
}

trait ParameterSection {
  def groups: Seq[Group]
  def processorType: ProcessorType
  def getParameter[T: TypeTag](
    groupName: String,
    parameterName: String,
    signed: ParameterSign = ParameterSign.Default,
    signConventions: ParameterSignConventions = ParameterSign.DefaultParameterSignConventions): Option[Parameter[T]]
  def requiredParameters: RequiredParameters
}

sealed trait ProcessorType
object ProcessorType {
  object Intel extends ProcessorType
  object DEC extends ProcessorType
  object SGIMIPS extends ProcessorType
}

trait Vec3D {
  def x: Float
  def y: Float
  def z: Float
  def mag: Float = math.sqrt(x*x + y*y + z*z).toFloat
  def cross(v: Vec3D): Vec3D = ???
  def +(v: Vec3D): Vec3D = ???
  def -(v: Vec3D): Vec3D = ???
}

trait AnalogChannel extends IndexedSeq[Float] {
  def name: String
  def description: String
}

trait Analog {
  def channels: IndexedSeq[AnalogChannel]
  def getChannelByName(name: String): Option[AnalogChannel]
  def samplingRate: Float
  def totalSamples: Int
}

trait Platform {
  def plates: IndexedSeq[ForcePlate]
}

trait ForcePlate {
  def forceInFPCoords: IndexedSeq[Vec3D]
  def momentInFPCoords: IndexedSeq[Vec3D]
  def origin: Vec3D
}

trait C3D {
  def parameterSection: ParameterSection
  def analog: Analog
  def platform: Platform
}

object C3D {
  def read(file: File): C3D = c3d.io.C3DReader.read(file)
  def read(c3dISeq: IndexedSeq[Byte]): C3D = c3d.io.C3DReader.read(c3dISeq)
}
