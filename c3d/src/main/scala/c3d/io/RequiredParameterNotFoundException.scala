package c3d.io

final case class RequiredParameterNotFoundException(groupName: String, parameterName: String)
  extends C3DIOException {
  override def toString(): String =
    s"Required parameter ${groupName.toUpperCase}:${parameterName.toUpperCase} was not found."
}
