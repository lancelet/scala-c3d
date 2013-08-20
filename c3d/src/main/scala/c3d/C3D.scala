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
  def pointLabels: Parameter[String]
  def pointDescriptions: Parameter[String]
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
  def dot(v: Vec3D): Float = ???
  def +(v: Vec3D): Vec3D = ???
  def -(v: Vec3D): Vec3D = ???
  def /(s: Float): Vec3D = ???
  def *(s: Float): Vec3D = ???
  def asUnit: Vec3D = ???
}

trait AnalogChannel extends SIndexedSeq[Float] {
  def name: String
  def description: String
}

trait Analog {
  def channels: IndexedSeq[AnalogChannel]
  def getChannelByName(name: String): Option[AnalogChannel]
  def rate: Float
  def totalSamples: Int
}

trait Platforms {
  def plates: IndexedSeq[ForcePlate]
  def rate: Float
}

trait ForcePlate {
  def pwa: SIndexedSeq[Vec3D]      // point of wrench application, expressed in world coordinates
  def force: SIndexedSeq[Vec3D]    // force, expressed in world coordinates
  def moment: SIndexedSeq[Vec3D]   // moment, expressed in world coordinates, at the pwa
  def momentAtOrigin: SIndexedSeq[Vec3D]   // moment, expressed in world coordinates, at geo centre
  def forceInFPCoords: SIndexedSeq[Vec3D]  // force, in fp coordinates
  def momentInFPCoords: SIndexedSeq[Vec3D] // moment, at fp coordinates
  def origin: Vec3D               // from fp coord origin to geometric centre of working surface (in fp coords)
  def corners: IndexedSeq[Vec3D]
  def rate: Float
}

trait Point extends IndexedSeq[Option[Vec3D]] {
  def name: String
  def description: String
  def rate: Float
}

trait Points {
  def points: IndexedSeq[Point]
  def getPointByName(name: String): Option[Point]
  def rate: Float
  def totalSamples: Int
}

trait C3D {
  def parameterSection: ParameterSection
  def analog: Analog
  def platforms: Platforms
  def points: Points
}

object C3D {
  def read(file: File): C3D = c3d.io.C3DReader.read(file)
  def read(c3dISeq: IndexedSeq[Byte]): C3D = c3d.io.C3DReader.read(c3dISeq)
}
