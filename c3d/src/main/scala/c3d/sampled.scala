package c3d

import scala.collection.immutable._
import scala.annotation.tailrec
import scala.math.Numeric

trait CanAverage[A] {
  def average(it: Iterator[A]): A
}
trait FloatCanAverage extends CanAverage[Float] {
  def average(it: Iterator[Float]): Float = {
    @tailrec def avgAccum(accum: Float, n: Int, it: Iterator[Float]): Float = {
      if (!it.hasNext) {
        if (n == 0) 0f else (accum / n)
      } else {
        avgAccum(accum + it.next, n + 1, it)
      }
    }
    avgAccum(0f, 0, it)
  }
}
trait Vec3DCanAverage extends CanAverage[Vec3D] {
  def average(it: Iterator[Vec3D]): Vec3D = {
    @tailrec def avgAccum(x: Float, y: Float, z: Float, n: Int, it: Iterator[Vec3D]): Vec3D = {
      if (!it.hasNext) {
        if (n == 0) Vec3D(0,0,0) else Vec3D(x / n, y / n, z / n)
      } else {
        val v = it.next
        avgAccum(x + v.x, y + v.y, z + v.z, n + 1, it)
      }
    }
    avgAccum(0f, 0f, 0f, 0, it)
  }
}
object CanAverage {
  implicit object FloatCanAverage extends FloatCanAverage
  implicit object Vec3DCanAverage extends Vec3DCanAverage
}

trait Sampled[A] { self: IndexedSeq[A] =>
  def rate: Float
  def resampleByAveraging[B >: A](intFactor: Int)(implicit avg: CanAverage[B]): SIndexedSeq[B] =
    new SIndexedSeq[B] {
      def rate: Float = self.rate / intFactor
      def length: Int = self.length / intFactor + (if (self.length % intFactor != 0) 1 else 0)
      def apply(i: Int): B = avg.average(self.slice(i * intFactor, (i + 1) * intFactor).iterator)
    }
}

trait SIndexedSeq[A] extends IndexedSeq[A] with Sampled[A]
object SIndexedSeq {
  def apply[A](is: IndexedSeq[A], myRate: Float): SIndexedSeq[A] = new SIndexedSeq[A] {
    def length: Int = is.length
    def apply(i: Int): A = is.apply(i)
    def rate: Float = myRate
  }
}