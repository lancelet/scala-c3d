package c3d.viewer.scene.control.skin

import c3d.viewer.scene.control.FrameNumberAxis
import javafx.scene.control.SkinBase
import javafx.scene.layout.Pane
import javafx.scene.canvas.Canvas
import javafx.scene.text.{Font, Text}
import math._
import javafx.geometry.Bounds
import javafx.beans.value.{ObservableValue, ChangeListener}
import javafx.scene.paint.{Color, Paint}

class FrameNumberAxisSkin(private val control: FrameNumberAxis) extends SkinBase[FrameNumberAxis](control) {

  private val rootPane = new Pane
  private val canvas = new Canvas
  private val dummyText = new Text

  private def range = control.getEndFrame - control.getStartFrame

  private def maxNDigits = max(control.getStartFrame.toString.length, control.getEndFrame.toString.length)

  private def dummyBounds: Bounds = {
    dummyText.setText("8" * maxNDigits)
    dummyText.setFont(control.getFont)
    dummyText.getLayoutBounds
  }

  private def labelRawWidth = dummyBounds.getWidth

  private def labelRawHeight = dummyBounds.getHeight

  private def labelWidthWithInsets = labelRawWidth + control.getInsets.getLeft + control.getInsets.getRight

  private def labelHeightWithInsets = labelRawHeight + control.getInsets.getTop + control.getInsets.getBottom

  private def majorTick: Int = {
    val nFit = rootPane.getWidth / labelWidthWithInsets
    if (nFit < 1) {
      -1
    } else {
      val rawTick = range / nFit
      val tenPow = floor(log10(rawTick)).toInt
      val msd = floor(rawTick / pow(10, tenPow)).toInt
      val (fitMsd, fitTenPow) = if (msd <= 2) (2, tenPow) else if (msd <= 5) (5, tenPow) else (1, tenPow + 1)
      (fitMsd * pow(10, fitTenPow)).toInt
    }
  }

  private def updateMajorTick = control.setMajorTick(majorTick)

  private def setPrefHeight = rootPane.setPrefHeight(labelHeightWithInsets)

  private def renderTicks: Unit = {
    // bail if we can't render any ticks
    if (control.getMajorTick < 1) return
    // render ticks
    val g = canvas.getGraphicsContext2D
    g.clearRect(0, 0, canvas.getWidth, canvas.getHeight)
    if (true) {
      g.setFill(Color.RED)
      g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    }
    g.setFont(control.getFont)
    g.setFill(control.getTextFill)
    val startTick = control.getMajorTick * floor(control.getStartFrame / control.getMajorTick).toInt
    val endTick   = control.getMajorTick * ceil(control.getEndFrame / control.getMajorTick).toInt
    val insets = control.getLabelInsets
    for (tick <- startTick until (endTick, control.getMajorTick)) {
      val xCenter = (tick - control.getStartFrame) / range * canvas.getWidth
      dummyText.setText(tick.toString)
      val bounds = dummyText.getLayoutBounds
      val txtWidth = bounds.getWidth + insets.getLeft + insets.getRight
      val xLeft = xCenter - txtWidth / 2 + insets.getLeft
      val xRight = xCenter + txtWidth / 2 - insets.getRight
      if (xLeft > 0 && xRight < canvas.getWidth) g.fillText(tick.toString, xLeft, bounds.getHeight - insets.getTop)
    }
  }

  private def update {
    updateMajorTick
    setPrefHeight
    renderTicks
  }

  private def initialize {
    val numberListener = new ChangeListener[Number] {
      def changed(ov: ObservableValue[_ <: Number], oldNum: Number, newNum: Number) = update
    }
    val fontListener = new ChangeListener[Font] {
      def changed(ov: ObservableValue[_ <: Font], oldFont: Font, newFont: Font) = update
    }
    val paintListener = new ChangeListener[Paint] {
      def changed(ov: ObservableValue[_ <: Paint], oldPaint: Paint, newPaint: Paint) = update
    }
    getChildren.addAll(rootPane)
    rootPane.getChildren.addAll(canvas)
    canvas.widthProperty.bind(rootPane.widthProperty)
    canvas.heightProperty.bind(rootPane.heightProperty)
    control.startFrameProperty.addListener(numberListener)
    control.endFrameProperty.addListener(numberListener)
    control.fontProperty.addListener(fontListener)
    control.textFillProperty.addListener(paintListener)
    rootPane.widthProperty.addListener(numberListener)
    rootPane.heightProperty.addListener(numberListener)
    update
  }
  initialize

}
