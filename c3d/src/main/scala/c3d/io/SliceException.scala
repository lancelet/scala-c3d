package c3d.io

/** Thrown to indicate that a slice on a [[WrappedArrayIndexedSeq]] has failed.
  * 
  * @param allowedFrom minimum allowed `from` value
  * @param allowedUntil maximum allowed `until` value
  * @param actualFrom requested `from` value for the slice operation
  * @param actualUntil requested `until` value for the slice operation
  */
private [io] final case class SliceException(allowedFrom: Int, allowedUntil: Int, actualFrom: Int, actualUntil: Int) 
    extends RuntimeException
{
  override def toString(): String = {
    s"permitted slice: [$allowedFrom, $allowedUntil], actual slice: [$actualFrom, $actualUntil]"
  }
}
