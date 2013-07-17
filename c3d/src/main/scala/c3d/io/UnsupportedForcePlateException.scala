package c3d.io

final case class UnsupportedForcePlateException(typ: Int) extends C3DIOException {
  override def toString(): String = {
    s"unsupported force plate type $typ"
  }
}