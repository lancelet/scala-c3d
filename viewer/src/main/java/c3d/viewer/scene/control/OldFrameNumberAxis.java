package c3d.viewer.scene.control;

import javafx.beans.property.*;
import javafx.css.*;
import javafx.scene.control.Control;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class OldFrameNumberAxis extends Control {

    /***************************************************************************
     *                                                                         *
     * Constructors                                                            *
     *                                                                         *
     **************************************************************************/

    public OldFrameNumberAxis() {
        initialize();
    }

    private void initialize() {
        getStyleClass().setAll(DEFAULT_STYLE_CLASS);
    }

    /***************************************************************************
     *                                                                         *
     * Properties                                                              *
     *                                                                         *
     **************************************************************************/

    /**
     * Start frame of the axis (inclusive).
     */
    private final IntegerProperty startFrame = new SimpleIntegerProperty(this, "startFrame", 0);
    public int getStartFrame() { return startFrame.get(); }
    public void setStartFrame(int sf) { startFrame.set(sf); }
    public IntegerProperty startFrameProperty() { return startFrame; }

    /**
     * End frame of the axis (inclusive).
     */
    private final IntegerProperty endFrame = new SimpleIntegerProperty(this, "endFrame", 180);
    public int getEndFrame() { return endFrame.get(); }
    public void setEndFrame(int ef) { endFrame.set(ef); }
    public IntegerProperty endFrameProperty() { return endFrame; }

    /**
     * Font.
     */
    private final ObjectProperty<Font> font = new SimpleStyleableObjectProperty<>(FONT_CSS_META_DATA,
            this, "font", Font.getDefault());
    public Font getFont() { return font.get(); }
    public void setFont(Font f) { font.set(f); }
    public ObjectProperty<Font> fontProperty() { return font; }

    /**
     * Text fill.
     */
    private final ObjectProperty<Paint> textFill = new SimpleStyleableObjectProperty<>(TEXTFILL_CSS_META_DATA,
            this, "textFill", Color.WHITE);
    public Paint getTextFill() { return textFill.get(); }
    public void setTextFill(Paint p) { textFill.set(p); }
    public ObjectProperty<Paint> textFillProperty() { return textFill; }

    /**
     * Major tick size.  This should be treated as read-only for all external classes except the skin.
     */
    private final IntegerProperty majorTick = new SimpleIntegerProperty(this, "majorTick", 1);
    public int getMajorTick() { return majorTick.get(); }
    public void setMajorTick(int m) { majorTick.set(m); }
    public IntegerProperty majorTickProperty() { return majorTick; }

    /**
     * Additional scaling of horizontal gap around tick.  1.0 = no scaling; > 1.0 increases tick spacing.
     */
    private final DoubleProperty tickHScaleForGap = new SimpleStyleableDoubleProperty(TICKHSCALEFORGAP_CSS_META_DATA,
            this, "tickHScaleForGap", 1.0);
    public double getTickHScaleForGap() { return tickHScaleForGap.get(); }
    public void setTickHScaleForGap(double s) { tickHScaleForGap.set(s); }
    public DoubleProperty tickHScaleForGapProperty() { return tickHScaleForGap; }

    /***************************************************************************
     *                                                                         *
     * Stylesheet Handling                                                     *
     *                                                                         *
     **************************************************************************/

    @Override
    protected String getUserAgentStylesheet() {
        return this.getClass().getResource("framenumberaxis.css").toString();
    }

    private static final String DEFAULT_STYLE_CLASS = "frame-number-axis";

    private static final FontCssMetaData<OldFrameNumberAxis> FONT_CSS_META_DATA =
        new FontCssMetaData<OldFrameNumberAxis>("-fx-font", Font.getDefault()) {
            @Override
            public boolean isSettable(OldFrameNumberAxis oldFrameNumberAxis) {
                return !oldFrameNumberAxis.font.isBound();
            }
            @Override
            public StyleableProperty<Font> getStyleableProperty(OldFrameNumberAxis oldFrameNumberAxis) {
                return (StyleableProperty<Font>) oldFrameNumberAxis.font;
            }
        };

    private static final CssMetaData<OldFrameNumberAxis, Paint> TEXTFILL_CSS_META_DATA =
        new CssMetaData<OldFrameNumberAxis, Paint>("-fx-text-fill", StyleConverter.getPaintConverter()) {
            @Override
            public boolean isSettable(OldFrameNumberAxis oldFrameNumberAxis) {
                return !oldFrameNumberAxis.textFill.isBound();
            }
            @Override
            public StyleableProperty<Paint> getStyleableProperty(OldFrameNumberAxis oldFrameNumberAxis) {
                return (StyleableProperty<Paint>) oldFrameNumberAxis.textFill;
            }
        };

    private static final CssMetaData<OldFrameNumberAxis, Number> TICKHSCALEFORGAP_CSS_META_DATA =
        new CssMetaData<OldFrameNumberAxis, Number>("-tickhscaleforgap", StyleConverter.getSizeConverter()) {
            @Override
            public boolean isSettable(OldFrameNumberAxis oldFrameNumberAxis) {
                return !oldFrameNumberAxis.tickHScaleForGap.isBound();
            }

            @Override
            public StyleableProperty<Number> getStyleableProperty(OldFrameNumberAxis oldFrameNumberAxis) {
                return (StyleableProperty<Number>) oldFrameNumberAxis.tickHScaleForGap;
            }
        };

    private static final List<CssMetaData<? extends Styleable, ?> > cssMetaDataList;
    static {
        List<CssMetaData<? extends Styleable, ?> > temp = new ArrayList<>(Control.getClassCssMetaData());
        temp.add(FONT_CSS_META_DATA);
        temp.add(TEXTFILL_CSS_META_DATA);
        temp.add(TICKHSCALEFORGAP_CSS_META_DATA);
        cssMetaDataList = Collections.unmodifiableList(temp);
    }
    public static List<CssMetaData<? extends Styleable, ?> > getClassCssMetaData() { return cssMetaDataList; }
    protected List<CssMetaData<? extends Styleable, ?> > getControlCssMetaData() { return getClassCssMetaData(); }

}
