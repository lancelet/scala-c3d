package c3d.io

import c3d.Vec3D

private[io] final case class RotMatrix(
  m11: Float, m12: Float, m13: Float,
  m21: Float, m22: Float, m23: Float,
  m31: Float, m32: Float, m33: Float) {

  def apply(v: Vec3D): Vec3D = {
    Vec3D(
      m11 * v.x + m12 * v.y + m13 * v.z,
      m21 * v.x + m22 * v.y + m23 * v.z,
      m31 * v.x + m32 * v.y + m33 * v.z)
  }
  
  def inv: RotMatrix = RotMatrix(m11, m21, m31, m12, m22, m32, m13, m23, m33)
  
}

object RotMatrix {
  
  /**
   * Constructs a rotation matrix from basis vectors.
   * 
   * Input vectors are normalized and orthogonality is enforced during construction.
   * 
   * @param x vector in original coordinates of x-axis in rotated coordinates
   * @param y vector in original coordinates of y-axis in rotated coordinates
   * @param z vector in original coordinates of z-axis in rotated coordinates
   */
  def fromBasisVectors(x: Vec3D, y: Vec3D, z: Vec3D): RotMatrix = {
    val xp = x.asUnit
    val zp = (xp cross y).asUnit
    val yp = zp cross xp
    RotMatrix(xp.x, xp.y, xp.z, yp.x, yp.y, yp.z, zp.x, zp.y, zp.z)
  }
  
  /**
   * Constructs a rotation matrix from X-Y basis vectors.
   * 
   * Input vectors are normalized and orthogonality is enforced during construction.
   * 
   * @param x vector in original coordinates of x-axis in rotated coordinates
   * @param y vector in original coordinates of y-axis in rotated coordinates
   */
  def fromBasisVectorsXY(x: Vec3D, y: Vec3D): RotMatrix = fromBasisVectors(x, y, x cross y)
  
}
