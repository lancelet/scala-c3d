package c3d.util.transform

import c3d.Vec3D
import org.ejml.data.DenseMatrix64F

final case class RotationMatrix(
  m11: Float, m12: Float, m13: Float,
  m21: Float, m22: Float, m23: Float,
  m31: Float, m32: Float, m33: Float) {

  def apply(v: Vec3D): Vec3D = Vec3D(
    m11 * v.x + m12 * v.y + m13 * v.z,
    m21 * v.x + m22 * v.y + m23 * v.z,
    m31 * v.x + m32 * v.y + m33 * v.z)
  
  def *(m: RotationMatrix): RotationMatrix = RotationMatrix (
    m11*m.m11 + m12*m.m21 + m13*m.m31, m11*m.m12 + m12*m.m22 + m13*m.m32, m11*m.m13 + m12*m.m23 + m13*m.m33,
    m21*m.m11 + m22*m.m21 + m23*m.m31, m21*m.m12 + m22*m.m22 + m23*m.m32, m21*m.m13 + m22*m.m23 + m23*m.m33,
    m31*m.m11 + m32*m.m21 + m33*m.m31, m31*m.m12 + m32*m.m22 + m33*m.m32, m31*m.m13 + m32*m.m23 + m33*m.m33
  )
  
  def inv: RotationMatrix = RotationMatrix(m11, m21, m31, m12, m22, m32, m13, m23, m33)
  def t: RotationMatrix = inv
  
  def trace: Float = m11 + m22 + m33
}

object RotationMatrix {
  
  /**
   * Constructs a rotation matrix from basis vectors.
   * 
   * Input vectors are normalized and orthogonality is enforced during construction.
   * 
   * @param x vector in original coordinates of x-axis in rotated coordinates
   * @param y vector in original coordinates of y-axis in rotated coordinates
   * @param z vector in original coordinates of z-axis in rotated coordinates
   */
  def fromBasisVectors(x: Vec3D, y: Vec3D, z: Vec3D): RotationMatrix = {
    val xp = x.asUnit
    val zp = (xp cross y).asUnit
    val yp = zp cross xp
    RotationMatrix(xp.x, xp.y, xp.z, yp.x, yp.y, yp.z, zp.x, zp.y, zp.z)
  }
  
  /**
   * Constructs a rotation matrix from X-Y basis vectors.
   * 
   * Input vectors are normalized and orthogonality is enforced during construction.
   * 
   * @param x vector in original coordinates of x-axis in rotated coordinates
   * @param y vector in original coordinates of y-axis in rotated coordinates
   */
  def fromBasisVectorsXY(x: Vec3D, y: Vec3D): RotationMatrix = fromBasisVectors(x, y, x cross y)
  
  /**
   * Constructs a rotation matrix from an EJML DenseMatrix64F.
   * 
   * @param m EJML DenseMatrix64F that must have size (3x3)
   */
  def fromDenseMatrix64F(m: DenseMatrix64F): RotationMatrix = {
    require(m.numCols == 3 && m.numRows == 3)
    RotationMatrix(
      m.unsafe_get(0, 0).toFloat, m.unsafe_get(0, 1).toFloat, m.unsafe_get(0, 2).toFloat,
      m.unsafe_get(1, 0).toFloat, m.unsafe_get(1, 1).toFloat, m.unsafe_get(1, 2).toFloat,
      m.unsafe_get(2, 0).toFloat, m.unsafe_get(2, 1).toFloat, m.unsafe_get(2, 2).toFloat
    )
  }
  
}
