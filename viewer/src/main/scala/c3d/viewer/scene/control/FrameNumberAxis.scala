package c3d.viewer.scene.control

import javafx.scene.control.Control
import javafx.beans.property.{ObjectProperty, IntegerProperty, SimpleIntegerProperty}
import javafx.scene.text.Font
import javafx.css._
import javafx.scene.paint.{Paint, Color}
import javafx.geometry.Insets
import c3d.viewer.css._


final class FrameNumberAxis extends Control {
  import FrameNumberAxis._

  val startFrameProperty: IntegerProperty = new SimpleIntegerProperty(this, "startFrame", 0)
  def setStartFrame(s: Int): Unit = startFrameProperty.set(s)
  def getStartFrame: Int = startFrameProperty.get

  val endFrameProperty: IntegerProperty = new SimpleIntegerProperty(this, "endFrame", 180)
  def setEndFrame(e: Int): Unit = endFrameProperty.set(e)
  def getEndFrame: Int = endFrameProperty.get

  private val font = new SimpleStyleableObjectProperty(FontCssMD, this, "font", Font.getDefault)
  def fontProperty: ObjectProperty[Font] = font
  def setFont(f: Font): Unit = font.set(f)
  def getFont: Font = font.get

  private val textFill = new SimpleStyleableObjectProperty[Paint](TextFillCssMD, this, "textFill", Color.WHITE)
  def textFillProperty: ObjectProperty[Paint] = textFill
  def setTextFill(t: Paint): Unit = textFill.set(t)
  def getTextFill: Paint = textFill.get

  val majorTickProperty: IntegerProperty = new SimpleIntegerProperty(this, "majorTick", 1)
  def setMajorTick(m: Int): Unit = majorTickProperty.set(m)
  def getMajorTick: Int = majorTickProperty.get

  private val labelInsets = new SimpleStyleableObjectProperty[Insets](LabelInsetsCssMD, this, "labelInsets", Insets.EMPTY)
  def labelInsetsProperty: ObjectProperty[Insets] = labelInsets
  def setLabelInsets(i: Insets): Unit = labelInsets.set(i)
  def getLabelInsets: Insets = labelInsets.get

  override protected def getUserAgentStylesheet = getClass.getResource("framenumberaxis.css").toString

  private def initialize() {
    getStyleClass.setAll(DefaultStyleClass)
  }
  initialize()

}


object FrameNumberAxis {

  final val DefaultStyleClass = "frame-number-axis"

  final val FontCssMD: CssFont[FrameNumberAxis] = new CssFont("-fx-font", _.font)
  final val TextFillCssMD: CssPaint[FrameNumberAxis] = new CssPaint("-fx-text-fill", _.textFill)
  final val LabelInsetsCssMD: CssInsets[FrameNumberAxis] = new CssInsets("-fx-label-insets", _.labelInsets)

}
