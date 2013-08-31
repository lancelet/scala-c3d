package c3d

trait Vec3D {
  
  def x: Float
  def y: Float
  def z: Float
  
  def mag: Float = math.sqrt(x * x + y * y + z * z).toFloat
  def dot(v: Vec3D): Float = x * v.x + y * v.y + z * v.z
  
  def cross(v: Vec3D): Vec3D
  def +(v: Vec3D): Vec3D
  def -(v: Vec3D): Vec3D
  def /(s: Float): Vec3D
  def *(s: Float): Vec3D
  def asUnit: Vec3D
  
}

object Vec3D {
  
  private final case class DefaultVec3D(x: Float, y: Float, z: Float) extends Vec3D {
    override lazy val mag = math.sqrt(x * x + y * y + z * z).toFloat
    def cross(v: Vec3D): Vec3D = DefaultVec3D(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
    def +(v: Vec3D): Vec3D = DefaultVec3D(x + v.x, y + v.y, z + v.z)
    def -(v: Vec3D): Vec3D = DefaultVec3D(x - v.x, y - v.y, z - v.z)
    def /(s: Float): Vec3D = DefaultVec3D(x / s, y / s, z / s)
    def *(s: Float): Vec3D = DefaultVec3D(x * s, y * s, z * s)
    def asUnit: Vec3D = DefaultVec3D(x / mag, y / mag, z / mag)
  }

  def apply(x: Float, y: Float, z: Float): Vec3D = DefaultVec3D(x, y, z)
  
}