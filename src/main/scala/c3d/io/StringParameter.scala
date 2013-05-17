package c3d.io

import scala.collection.immutable._
import scala.reflect.runtime.universe._
import c3d.Parameter

/** Converts a [[Parameter[Char]]] to a [[Parameter[String]]].
  * 
  * To perform the conversion, the first dimension (column-major) of the data array is converted from a `Seq[Char]` to
  * a `String`.  This effectively drops the number of dimensions of the parameter by one.
  * 
  * @param charParam character parameter to convert into a string parameter
  */
final case class StringParameter(charParam: Parameter[Char]) extends Parameter[String] {
  def name: String = charParam.name
  def description: String = charParam.description
  def isLocked: Boolean = charParam.isLocked
  val dimensions: IndexedSeq[Int] = {
    val tail = charParam.dimensions.tail
    if (tail.isEmpty) IndexedSeq(1) else tail
  }
  def data: IndexedSeq[String] = {
    assert(charParam.data.length % charParam.dimensions(0) == 0, "data cannot be divided into even chunks")
    charParam.data.grouped(charParam.dimensions(0)).map(_.mkString).toIndexedSeq
  }
  def apply(idx: IndexedSeq[Int]): String = {
    val coef = dimensions.scanLeft(1)(_ * _)
    val flatIndex = (for ((i, c) <- idx zip coef) yield i * c).sum
    data(flatIndex)
  }
  val parameterType: Type = typeOf[String]
}
