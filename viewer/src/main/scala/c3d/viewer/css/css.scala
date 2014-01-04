package c3d.viewer.css

import javafx.css._
import javafx.scene.paint.Paint
import javafx.geometry.Insets
import javafx.scene.text.Font

/**
 * Wrapper for `CssMetaData`.
 *
 * @param name name of the property
 * @param property function which takes an `S` and returns the styleable object property
 * @param converter style converter for the property
 * @tparam S subclass of `Styleable` to which the CSS meta data applies
 * @tparam V type of the `Styleable` data
 */
sealed class Css[S <: Styleable, V] (
  name: String,
  property: S => StyleableObjectProperty[V],
  converter: StyleConverter[_, V]
) extends CssMetaData[S, V](name, converter) {
  def isSettable(s: S): Boolean = !property(s).isBound
  def getStyleableProperty(s: S): StyleableProperty[V] = property(s)
}

/**
 * CSS `Paint` property.
 *
 * @param name name of the property
 * @param property function which takes an `S` and returns the styleable object property
 * @tparam S subclass of `Styleable` to which the CSS meta data applies
 */
final class CssPaint[S <: Styleable] (
  name: String,
  property: S => StyleableObjectProperty[Paint]
) extends Css[S, Paint](name, property, StyleConverter.getPaintConverter)

/**
 * CSS `Insets` property.
 *
 * @param name name of the property
 * @param property function which takes an `S` and returns the styleable object property
 * @tparam S subclass of `Styleable` to which the CSS meta data applies
 */
final class CssInsets[S <: Styleable] (
  name: String,
  property: S => StyleableObjectProperty[Insets]
) extends Css[S, Insets](name, property, StyleConverter.getInsetsConverter)

/**
 * CSS `Font` property.
 *
 * @param name name of the property
 * @param property function which takes an `S` and returns the styleable object property
 * @tparam S subclass of `Styleable` to which the CSS meta data applies
 */
final class CssFont[S <: Styleable] (
  name: String,
  property: S => StyleableObjectProperty[Font]
) extends FontCssMetaData[S](name, Font.getDefault) {
  def isSettable(s: S): Boolean = !property(s).isBound
  def getStyleableProperty(s: S): StyleableProperty[Font] = property(s)
}
