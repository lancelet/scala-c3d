package c3d.io

import c3d.Vec3D

private[io] final case class DefaultVec3D(x: Float, y: Float, z: Float) extends Vec3D {
  override def cross(v: Vec3D): Vec3D = DefaultVec3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  override def +(v: Vec3D): Vec3D = DefaultVec3D(x + v.x, y + v.y, z + v.z)
  override def -(v: Vec3D): Vec3D = DefaultVec3D(x - v.x, y - v.y, z - v.z)
  override def /(s: Float): Vec3D = DefaultVec3D(x / s, y / s, z / s)
  override def *(s: Float): Vec3D = DefaultVec3D(x * s, y * s, z * s)
  override def asUnit: Vec3D = {
    val m = this.mag
    DefaultVec3D(x / m, y / m, z / m)
  }
}