---
title: "CSS"
slug: "css"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- NodeClass /* selector by Node's class */
- .someclass /* selector by class */
- #someId /* selector by id */
- [selector1] > [selector2] /* selector for a direct child of a node matching selector1 that matches selector2 */
- [selector1] [selector2] /* selector for a descendant of a node matching selector1 that matches selector2 */

## Using CSS for styling
CSS can be applied in multiple places:

* inline (`Node.setStyle`)
* in a stylesheet
   * to a `Scene`
     * as user agent stylesheet (not demonstrated here)
     * as "normal" stylesheet for the `Scene`
   * to a `Node`

This allows to change styleable properties of `Nodes`. The following example demonstrates this:

**Application class**

    import javafx.application.Application;
    import javafx.scene.Scene;
    import javafx.scene.layout.HBox;
    import javafx.scene.layout.Region;
    import javafx.scene.layout.VBox;
    import javafx.scene.paint.Color;
    import javafx.stage.Stage;
    
    public class StyledApplication extends Application {
    
        @Override
        public void start(Stage primaryStage) {
            
            Region region1 = new Region();
            Region region2 = new Region();
            Region region3 = new Region();
            Region region4 = new Region();
            Region region5 = new Region();
            Region region6 = new Region();
            
            // inline style
            region1.setStyle("-fx-background-color: yellow;");
            
            // set id for styling
            region2.setId("region2");
            
            // add class for styling
            region2.getStyleClass().add("round");
            region3.getStyleClass().add("round");
            
            HBox hBox = new HBox(region3, region4, region5);
            
            VBox vBox = new VBox(region1, hBox, region2, region6);
    
            Scene scene = new Scene(vBox, 500, 500);
            
            // add stylesheet for root
            scene.getStylesheets().add(getClass().getResource("style.css").toExternalForm());
            
            // add stylesheet for hBox
            hBox.getStylesheets().add(getClass().getResource("inlinestyle.css").toExternalForm());
            
            scene.setFill(Color.BLACK);
            
            primaryStage.setScene(scene);
            primaryStage.show();
        }
    
        public static void main(String[] args) {
            launch(args);
        }
    
    }


**inlinestyle.css**

<!-- language: lang-css -->

    * {
        -fx-opacity: 0.5;
    }
    
    HBox {
        -fx-spacing: 10;
    }
    
    Region {
        -fx-background-color: white;
    }

**style.css**

<!-- language: lang-css -->

    Region {
        width: 50;
        height: 70;
        
        -fx-min-width: width;
        -fx-max-width: width;
        
        -fx-min-height: height;
        -fx-max-height: height;
        
        -fx-background-color: red;
    }
    
    VBox {
        -fx-spacing: 30;
        -fx-padding: 20;
    }
    
    #region2 {
        -fx-background-color: blue;
    }

## Extending Rectangle adding new stylable properties
<!-- if version [gte JavaFX 8] -->
The following example demonstrates how to add custom properties that can be styled from css to a custom `Node`.  

Here 2 `DoubleProperty`s are added to the `Rectangle` class to allow setting the `width` and `height` from CSS.

The following CSS could be used for styling the custom node:

<!-- language: lang-css -->

     StyleableRectangle {
        -fx-fill: brown;
        -fx-width: 20;
        -fx-height: 25;
        -fx-cursor: hand;
    }

### Custom Node

    import java.util.ArrayList;
    import java.util.Arrays;
    import java.util.Collections;
    import java.util.List;
    import javafx.beans.property.DoubleProperty;
    import javafx.css.CssMetaData;
    import javafx.css.SimpleStyleableDoubleProperty;
    import javafx.css.StyleConverter;
    import javafx.css.Styleable;
    import javafx.css.StyleableDoubleProperty;
    import javafx.css.StyleableProperty;
    import javafx.scene.paint.Paint;
    import javafx.scene.shape.Rectangle;

    public class StyleableRectangle extends Rectangle {
        
        // declaration of the new properties
        private final StyleableDoubleProperty styleableWidth = new SimpleStyleableDoubleProperty(WIDTH_META_DATA, this, "styleableWidth");
        private final StyleableDoubleProperty styleableHeight = new SimpleStyleableDoubleProperty(HEIGHT_META_DATA, this, "styleableHeight");
    
        public StyleableRectangle() {
            bind();
        }
    
        public StyleableRectangle(double width, double height) {
            super(width, height);
            initStyleableSize();
            bind();
        }
    
        public StyleableRectangle(double width, double height, Paint fill) {
            super(width, height, fill);
            initStyleableSize();
            bind();
        }
    
        public StyleableRectangle(double x, double y, double width, double height) {
            super(x, y, width, height);
            initStyleableSize();
            bind();
        }
        
        private void initStyleableSize() {
            styleableWidth.set(getWidth());
            styleableHeight.set(getHeight());
        }
        
        private final static List<CssMetaData<? extends Styleable, ?>> CLASS_CSS_META_DATA;
        
        // css metadata for the width property
        // specify property name as -fx-width and
        // use converter for numbers
        private final static CssMetaData<StyleableRectangle, Number> WIDTH_META_DATA = new CssMetaData<StyleableRectangle, Number>("-fx-width", StyleConverter.getSizeConverter()) {
    
            @Override
            public boolean isSettable(StyleableRectangle styleable) {
                // property can be set iff the property is not bound
                return !styleable.styleableWidth.isBound();
            }
    
            @Override
            public StyleableProperty<Number> getStyleableProperty(StyleableRectangle styleable) {
                // extract the property from the styleable
                return styleable.styleableWidth;
            }
        };
        
        // css metadata for the height property
        // specify property name as -fx-height and
        // use converter for numbers
        private final static CssMetaData<StyleableRectangle, Number> HEIGHT_META_DATA = new CssMetaData<StyleableRectangle, Number>("-fx-height", StyleConverter.getSizeConverter()) {
    
            @Override
            public boolean isSettable(StyleableRectangle styleable) {
                return !styleable.styleableHeight.isBound();
            }
    
            @Override
            public StyleableProperty<Number> getStyleableProperty(StyleableRectangle styleable) {
                return styleable.styleableHeight;
            }
        };
        
        static {
            // combine already available properties in Rectangle with new properties
            List<CssMetaData<? extends Styleable, ?>> parent = Rectangle.getClassCssMetaData();
            List<CssMetaData<? extends Styleable, ?>> additional = Arrays.asList(HEIGHT_META_DATA, WIDTH_META_DATA);

            // create arraylist with suitable capacity
            List<CssMetaData<? extends Styleable, ?>> own = new ArrayList(parent.size()+ additional.size());

            // fill list with old and new metadata
            own.addAll(parent);
            own.addAll(additional);
            
            // make sure the metadata list is not modifiable
            CLASS_CSS_META_DATA = Collections.unmodifiableList(own); 
        }
        
        // make metadata available for extending the class
        public static List<CssMetaData<? extends Styleable, ?>> getClassCssMetaData() {
            return CLASS_CSS_META_DATA;
        }
        
        // returns a list of the css metadata for the stylable properties of the Node
        @Override
        public List<CssMetaData<? extends Styleable, ?>> getCssMetaData() {
            return CLASS_CSS_META_DATA;
        }
        
        private void bind() {
            this.widthProperty().bind(this.styleableWidth);
            this.heightProperty().bind(this.styleableHeight);
        }
    
    
        // -------------------------------------------------------------------------
        // ----------------------- PROPERTY METHODS --------------------------------
        // -------------------------------------------------------------------------
        
        public final double getStyleableHeight() {
            return this.styleableHeight.get();
        }
    
        public final void setStyleableHeight(double value) {
            this.styleableHeight.set(value);
        }
    
        public final DoubleProperty styleableHeightProperty() {
            return this.styleableHeight;
        }
    
        public final double getStyleableWidth() {
            return this.styleableWidth.get();
        }
    
        public final void setStyleableWidth(double value) {
            this.styleableWidth.set(value);
        }
    
        public final DoubleProperty styleableWidthProperty() {
            return this.styleableWidth;
        }
    
    }

<!-- end version if -->

