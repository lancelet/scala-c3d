package c3d.util.transform

import scala.collection.breakOut
import scala.collection.immutable._
import c3d.Vec3D
import scala.annotation.tailrec
import org.ejml.data.DenseMatrix64F
import org.ejml.ops.CommonOps
import org.ejml.ops.MatrixFeatures

object Veldpaus {

  /** Computes a rigid body transformation from corresponding marker
   *  coordinates.
   *  
   *  The rigid body transformation between two sets of corresponding points is
   *  calculated in a least-squares fashion.  The transformation is given in
   *  the form:
   *  {{{
   *    y = s * R * x + v
   *  }}}
   *  where `x` are the original coordinates and `y` are the transformed
   *  coordinates, `R` is a rotation matrix and `v` is a translation vector.
   *  
   *  The algorithm implemented by this method is described in the paper:
   *  
   *  - Veldpaus FE, Woltring HJ, and Dortmans LJMG (1988) A least-squares
   *      algorithm for the equiform transformation from spatial marker
   *      coordinates.  J Biomech 21(1):45-54.
   *      
   *  @param points a traversable collection of corresponding points
   *  @param tol tolerance for convergence of an iterative part of the
   *    algorithm.  The default value of 1.0e-10 is taken from
   *    Veldpaus et al. (1988). */
  def veldpaus(points: Traversable[(Vec3D, Vec3D)], tol: Double = 1.0e-10): Result = {
    
    // find x,y coordinates, centroids, and positions relative to centroids
    val (xc, yc) = points.unzip
    val xbar: Vec3D = vmean(xc)  // mean of original coordinates
    val ybar: Vec3D = vmean(yc)  // mean of transformed coordinates
    val xprime: IndexedSeq[Vec3D] = xc.map(_ - xbar)(breakOut)
    val yprime: IndexedSeq[Vec3D] = yc.map(_ - ybar)(breakOut)
    
    // calculate cross-dispersion matrix G, GTG and the adjoint of GTG
    val G: DenseMatrix64F = {
      val g = new DenseMatrix64F(3, 3)
      var index = 0
      while (index < xprime.length) {
        val xv: Vec3D = xprime(index)
        val yv: Vec3D = yprime(index)
        val x: DenseMatrix64F = new DenseMatrix64F(3, 1, true, xv.x, xv.y, xv.z)
        val y: DenseMatrix64F = new DenseMatrix64F(3, 1, true, yv.x, yv.y, yv.z)
        CommonOps.multAddTransB(y, x, g) // g = g + y * x^T
        index += 1
      }
      CommonOps.divide(xprime.length, g)
      g
    }
    val Ga: DenseMatrix64F = adjoint3(G)
    val GTG: DenseMatrix64F = {
      val m = new DenseMatrix64F(3, 3)
      CommonOps.multInner(G, m)
      m
    }
    val GTGa: DenseMatrix64F = adjoint3(GTG)

    // check rank of G; G must be at least rank 2 in order to find a solution
    def printDebugInfo {
      println("debugging info: rank decomposition failed for points:")
      for {
        ptPair <- points
        a = ptPair._1
        b = ptPair._2
      } println(s"$a -> $b")
    }
    val rank = try {
      MatrixFeatures.rank(G)
    } catch {
      case re: RuntimeException => {
        printDebugInfo
        throw re
      }
    }
    if (rank < 2) {
      printDebugInfo
      throw VeldpausException("Rank of cross-dispersion matrix was < 2; no rigid-body solution is possible.")
    }
    
    // find g1, g2, g3 values; these are invariants of GTG and G, used for calculating the rotation matrix
    val g1sq: Double = CommonOps.trace(GTG)
    val g2sq: Double = CommonOps.trace(GTGa)
    val g1: Double = math.sqrt(g1sq)
    val g2: Double = math.sqrt(g2sq)
    val g3: Double = CommonOps.det(G)

    // compute matrix invariants beta1 and beta2; iterating non-linear
    //  equations using the Newton-Raphson method
    val (beta1, beta2) = {
      val h1 = g2 / g1sq       // sqrt(GTGa.trace) / GTG.trace
      val h2 = g1 * g3 / g2sq  // sqrt(GTG.trace) * det(G) / GTGa.trace
      var xx: Double = 1
      var yy: Double = 1
      var dx: Double = 0
      var dy: Double = 0
      var err = 10.0 * tol
      while (err > tol) {
        val d = xx * yy - h1 * h2
        val help1 = 0.5 * (1.0 - xx * xx + 2.0 * h1 * yy)
        val help2 = 0.5 * (1.0 - yy * yy + 2.0 * h2 * xx)
        val dx = (yy * help1 + h1 * help2) / d
        val dy = (h2 * help1 + xx * help2) / d
        xx = xx + dx
        yy = yy + dy
        err = dx * dx / (xx * xx) + dy * dy / (yy * yy)
      }
      (g1 * xx, g2 * yy)
    }
    
    // calculate R and v
    val R: RotationMatrix = {
      val m = new DenseMatrix64F(3, 3)
      val A = new DenseMatrix64F(3, 3)
      val B = new DenseMatrix64F(3, 3)
      val I = CommonOps.identity(3)
      CommonOps.add(Ga,  beta1, G, A)  // A = Ga  + beta1 * G
      CommonOps.add(GTG, beta2, I, B)  // B = GTG + beta2 * I
      CommonOps.invert(B)              // B = B^{-1}
      CommonOps.mult(A, B, m)          // m = A * B = (Ga + beta1 * G) * (GTG + beta2 * I)^(-1)
      RotationMatrix.fromDenseMatrix64F(m)
    }
    val v: Vec3D = ybar - R(xbar)
    
    // calculate scale factor s
    val s: Float = {
      val A = new DenseMatrix64F(3, 3)
      var index = 0
      while (index < xprime.length) {
        val xpp: Vec3D = xprime(index)
        val xp: DenseMatrix64F = new DenseMatrix64F(3, 1, true, xpp.x, xpp.y, xpp.z)
        CommonOps.multAddTransB(xp, xp, A) // A = A + xp * xp^T
        index += 1
      }
      CommonOps.divide(xprime.length, A)
      val B: RotationMatrix = R.t * RotationMatrix.fromDenseMatrix64F(G)
      (B.trace / CommonOps.trace(A)).toFloat
    }
    
    Result(R, v, s, xbar, ybar)
  }

  /** 
   * Adjoint of 3x3 matrix.
   * 
   * @param m matrix for which to compute the adjoint
   * @param r 3x3 matrix to populate with the adjoint (if not supplied, a new matrix is created by default)
   * @return adjoint matrix 
   */
  private def adjoint3(m: DenseMatrix64F, r: DenseMatrix64F = new DenseMatrix64F(3, 3)): DenseMatrix64F = {
    require(m.numCols == 3 && m.numRows == 3)
    def a(i: Int, j: Int): Double    = m.unsafe_get(i, j)
    def s(i: Int, j: Int, v: Double) = r.unsafe_set(i, j, v)
    var x: Double = 0.0
    var y: Double = 0.0
    var z: Double = 0.0
    def xp(c1: Int, c2: Int) {
      x = a(1, c1) * a(2, c2) - a(2, c1) * a(1, c2)
      y = a(2, c1) * a(0, c2) - a(0, c1) * a(2, c2)
      z = a(0, c1) * a(1, c2) - a(1, c1) * a(0, c2)
    }
    xp(1, 2)
    s(0, 0, x)
    s(1, 0, y)
    s(2, 0, z)
    xp(2, 0)
    s(0, 1, x)
    s(1, 1, y)
    s(2, 1, z)
    xp(0, 1)
    s(0, 2, x)
    s(1, 2, y)
    s(2, 2, z)
    r
  }
  
  /**
   * Mean of a Traversable of Vec3D.
   * 
   * @param vecs Traversable of Vec3D objects for which to find the mean
   * @return mean
   */
  private def vmean(vecs: Traversable[Vec3D]): Vec3D = {
    @tailrec def accum(x: Double, y: Double, z: Double, n: Int, vs: Traversable[Vec3D]): Vec3D = {
      if (vs.isEmpty) Vec3D((x / n).toFloat, (y / n).toFloat, (z / n).toFloat)
      else {
        val v = vs.head
        accum(x + v.x, y + v.y, z + v.z, n + 1, vs.tail)
      }
    }
    accum(0.0, 0.0, 0.0, 0, vecs)
  }
  
  final case class Result(
    R: RotationMatrix,
    v: Vec3D,
    s: Float,
    xbar: Vec3D,
    ybar: Vec3D
  ) extends XForm {
    def apply(x: Vec3D): Vec3D = R(x) * s + v
  }
  
  final case class VeldpausException(msg: String) extends Exception(msg)
  
}
