package c3d.io

import java.io.IOException

trait C3DIOException extends IOException

object C3DIOException {
  
  private final case class GenericC3DIOException(message: String = "") extends C3DIOException {
    override def toString: String = message
  }

  def apply(message: String): C3DIOException = new GenericC3DIOException(message)
  
}
