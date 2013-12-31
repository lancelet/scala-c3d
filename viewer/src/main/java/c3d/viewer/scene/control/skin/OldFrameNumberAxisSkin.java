package c3d.viewer.scene.control.skin;

import c3d.viewer.scene.control.OldFrameNumberAxis;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Bounds;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.control.SkinBase;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.scene.text.Text;


public class OldFrameNumberAxisSkin extends SkinBase<OldFrameNumberAxis> {

    public OldFrameNumberAxisSkin(OldFrameNumberAxis control) {
        super(control);
        this.control = control;
        initialize();
    }

    private final OldFrameNumberAxis control;

    private final Pane rootPane = new Pane();
    private final Canvas canvas = new Canvas();

    private void initialize() {
        ChangeListener<Number> numberListener = new ChangeListener<Number>() {
            @Override public void changed(ObservableValue<? extends Number> ov, Number oldNum, Number newNum) {
                updateMajorTick();
                renderTicks();
            }
        };
        ChangeListener<Font> fontListener = new ChangeListener<Font>() {
            @Override public void changed(ObservableValue<? extends Font> ov, Font oldFont, Font newFont) {
                updateMajorTick();
                renderTicks();
            }
        };
        ChangeListener<Paint> paintListener = new ChangeListener<Paint>() {
            @Override public void changed(ObservableValue<? extends Paint> ov, Paint oldPaint, Paint newPaint) {
                renderTicks();
            }
        };
        getChildren().addAll(rootPane);
        rootPane.getChildren().addAll(canvas);
        canvas.widthProperty().bind(rootPane.widthProperty());
        canvas.heightProperty().bind(rootPane.heightProperty());
        control.startFrameProperty().addListener(numberListener);
        control.endFrameProperty().addListener(numberListener);
        control.fontProperty().addListener(fontListener);
        control.textFillProperty().addListener(paintListener);
        rootPane.widthProperty().addListener(numberListener);
        rootPane.heightProperty().addListener(numberListener);
        updateMajorTick();
        renderTicks();
    }

    private int getRange() { return control.getEndFrame() - control.getStartFrame() + 1; }

    private int getNumDigits() {
        int startLength = (String.valueOf(control.getStartFrame())).length();
        int endLength   = (String.valueOf(control.getEndFrame())).length();
        return Math.max(startLength, endLength);
    }

    private static String getSampleLabel(int nDigits, char c) {
        StringBuffer sb = new StringBuffer(nDigits);
        for (int i=0; i < nDigits; i++) {
            sb.append(c);
        }
        return sb.toString();
    }

    private final Text dummyText = new Text();

    private double getLabelWidth() {
        dummyText.setText(getSampleLabel(getNumDigits(), '8'));
        dummyText.setFont(control.getFont());
        return dummyText.getLayoutBounds().getWidth() * control.getTickHScaleForGap();
    }

    private void updateMajorTick() {
        double nCanFit = rootPane.getWidth() / getLabelWidth();
        if (nCanFit < 1) {
            control.setMajorTick(-1);
            return;
        }
        double rawTick = getRange() / nCanFit;
        int powerOfTen = 0;
        while (rawTick >= 10) {
            powerOfTen += 1;
            rawTick /= 10;
        }
        int mostSignificantDigit = (int)Math.floor(rawTick);
        if (mostSignificantDigit <= 2) {
            mostSignificantDigit = 2;
        } else if (mostSignificantDigit <= 5) {
            mostSignificantDigit = 5;
        } else {
            mostSignificantDigit = 1;
            powerOfTen += 1;
        }
        int majorTick = (int)(mostSignificantDigit * Math.pow(10.0, powerOfTen));
        control.setMajorTick(majorTick);
    }

    private void renderTicks() {
        // Set rootPane preferred height
        dummyText.setFont(control.getFont());
        dummyText.setText("8");
        rootPane.setPrefHeight(dummyText.getLayoutBounds().getHeight());
        // if we can't render any ticks, return now
        if (control.getMajorTick() < 1) {
            return;
        }
        // Render ticks in canvas (majorTick size should already be set)
        final GraphicsContext g = canvas.getGraphicsContext2D();
        // Clear canvas
        g.clearRect(0, 0, canvas.getWidth(), canvas.getHeight());
        // Fill dummy rect
        if (false) {
            g.setFill(Color.RED);
            g.fillRect(0, 0, canvas.getWidth(), canvas.getHeight());
        }
        g.setFont(control.getFont());
        g.setFill(control.getTextFill());
        final double canvasWidth = canvas.getWidth();
        final double range = (double)getRange();
        final int majorTick = control.getMajorTick();
        int tick = majorTick * (int)Math.floor(control.getStartFrame() / majorTick);
        while (tick < control.getEndFrame()) {
            final String txt = Integer.toString(tick);
            final double xCenter = (tick - control.getStartFrame()) / range * canvasWidth;
            dummyText.setText(txt);
            Bounds bounds = dummyText.getLayoutBounds();
            final double txtWidth = bounds.getWidth();
            final double xLeft = xCenter - txtWidth / 2;
            final double xRight = xCenter + txtWidth / 2;
            if (xLeft > 0 && xRight < canvasWidth) {
                g.fillText(txt, xLeft, bounds.getHeight());
            }
            tick += majorTick;
        }
    }

}
