package c3d

import scala.collection.immutable._

/** Stores the parameter sign conventions for reading a C3D file. */
trait ParameterSignConventions {
  /** Returns whether a parameter should be treated as [[ParameterSign.Signed]] or [[ParameterSign.Unsigned]] 
    * by default.
    * 
    * @param group group name
    * @param parameter parameter name
    * 
    * @return [[Signed]] or [[Unsigned]]
    */
  def signForParameter(group: String, parameter: String): ParameterSign
}

/** Specifies whether to fetch a numeric parameter as [[ParameterSign.Default]], [[ParameterSign.Signed]] or 
  * [[ParameterSign.Unsigned]]. */
sealed trait ParameterSign
object ParameterSign {

  /** Parameter should use default sign convention based upon the C3D spec. */
  object Default  extends ParameterSign
  /** Parameter should be treated as signed. */
  object Signed   extends ParameterSign
  /** Parameter should be treated as unsigned. */
  object Unsigned extends ParameterSign

  /** Default parameter sign conventions from the C3D spec. */
  object DefaultParameterSignConventions extends ParameterSignConventions {
    // Set of parameters that should be treated as unsigned by default
    private val unsignedByDefault: Set[String] = Set(
      "POINT:USED",
      "POINT:DATA_START",
      "POINT:FRAMES",
      "ANALOG:USED",
      "ANALOG:SCALE",
      "FORCE_PLATFORM:ZERO"
    )
    def signForParameter(group: String, parameter: String): ParameterSign =
      if (unsignedByDefault.contains(s"$group:$parameter")) Signed else Unsigned
  }
}
