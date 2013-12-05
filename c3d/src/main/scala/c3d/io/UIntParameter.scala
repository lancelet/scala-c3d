package c3d.io

import scala.collection.immutable._
import c3d.Parameter
import c3d.io.collection.ImmutableArray

/** Converts a [[Parameter[Int]]] to an unsigned [[Parameter[Int]]].
  * 
  * @param intParam integer parameter to convert to an unsigned integer parameter
  * 
  * @return unsigned integer parameter
  */
private [io] final case class UIntParameter(intParam: Parameter[Int])
    extends Parameter[Int] with ParameterTemplate[Int]
{
  def name: String = intParam.name
  def description: String = intParam.description
  def isLocked: Boolean = intParam.isLocked
  def dimensions: IndexedSeq[Int] = intParam.dimensions
  def data: IndexedSeq[Int] = new IndexedSeq[Int] {
    def length: Int = intParam.data.length
    def apply(idx: Int): Int = intParam.data(idx) & 0xFFFF
  }
  def parameterType: Parameter.Type = intParam.parameterType
}
