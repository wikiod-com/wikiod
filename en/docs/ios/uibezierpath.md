---
title: "UIBezierPath"
slug: "uibezierpath"
draft: false
images: []
weight: 9748
type: docs
toc: true
---

## Designing and drawing a Bezier Path
This example shows the process from designing the shape you want to drawing it on a view. A specific shap is used but the concepts you learn can be applied to any shape.

# How to draw a [Bézier path][1] in a custom view

These are the main steps:

1. Design the outline of the shape you want.
2. Divide the outline path into segments of lines, arcs, and curves.
3. Build that path programmatically.
4. Draw the path either in `drawRect` or using a `CAShapeLayer`.

# Design shape outline

You could do anything, but as an example I have chosen the shape below. It could be a popup key on a keyboard.

[![enter image description here][2]][2]

# Divide the path into segments

Look back at your shape design and break it down into simpler elements of lines (for straight lines), arcs (for circles and round corners), and curves (for anything else).

Here is what our example design would look like:

[![enter image description here][3]][3]

- Black are line segments
- Light blue are arc segments
- Red are curves
- Orange dots are the control points for the curves
- Green dots are the points between path segments
- Dotted lines show the bounding rectangle
- Dark blue numbers are the segments in the order that they will be added programmatically

# Build the path programmatically

We'll arbitrarily start in the bottom left corner and work clockwise. I'll use the grid in the image to get the x and y values for the points. I'll hardcode everything here, but of course you wouldn't do that in a real project.

The basic process is:

1. Create a new `UIBezierPath`
2. Choose a starting point on the path with `moveToPoint`
3. Add segments to the path
 - line: `addLineToPoint`
 - arc: `addArcWithCenter`
 - curve: `addCurveToPoint`
4. Close the path with `closePath`

Here is the code to make the path in the image above.

    func createBezierPath() -> UIBezierPath {
        
        // create a new path
        let path = UIBezierPath()
        
        // starting point for the path (bottom left)
        path.moveToPoint(CGPoint(x: 2, y: 26))
        
        // *********************
        // ***** Left side *****
        // *********************
        
        // segment 1: line
        path.addLineToPoint(CGPoint(x: 2, y: 15))
        
        // segment 2: curve
        path.addCurveToPoint(CGPoint(x: 0, y: 12), // ending point
            controlPoint1: CGPoint(x: 2, y: 14),
            controlPoint2: CGPoint(x: 0, y: 14))
        
        // segment 3: line
        path.addLineToPoint(CGPoint(x: 0, y: 2))
        
        // *********************
        // ****** Top side *****
        // *********************
        
        // segment 4: arc
        path.addArcWithCenter(CGPoint(x: 2, y: 2), // center point of circle
            radius: 2, // this will make it meet our path line
            startAngle: CGFloat(M_PI), // π radians = 180 degrees = straight left
            endAngle: CGFloat(3*M_PI_2), // 3π/2 radians = 270 degrees = straight up
            clockwise: true) // startAngle to endAngle goes in a clockwise direction
        
        // segment 5: line
        path.addLineToPoint(CGPoint(x: 8, y: 0))
        
        // segment 6: arc
        path.addArcWithCenter(CGPoint(x: 8, y: 2),
            radius: 2,
            startAngle: CGFloat(3*M_PI_2), // straight up
            endAngle: CGFloat(0), // 0 radians = straight right
            clockwise: true)
        
        // *********************
        // ***** Right side ****
        // *********************
        
        // segment 7: line
        path.addLineToPoint(CGPoint(x: 10, y: 12))
        
        // segment 8: curve
        path.addCurveToPoint(CGPoint(x: 8, y: 15), // ending point
            controlPoint1: CGPoint(x: 10, y: 14),
            controlPoint2: CGPoint(x: 8, y: 14))
        
        // segment 9: line
        path.addLineToPoint(CGPoint(x: 8, y: 26))
        
        // *********************
        // **** Bottom side ****
        // *********************
        
        // segment 10: line
        path.closePath() // draws the final line to close the path
        
        return path
    }

Note: Some of the above code can be reduced by adding a line and an arc in a single command (since the arc has an implied starting point). See [here][4] for more details.

# Draw the path

We can draw the path either in a layer or in `drawRect`.

**Method 1: Draw path in a layer**

Our custom class looks like this. We add our Bezier path to a new `CAShapeLayer` when the view is initialized. 

    import UIKit
    class MyCustomView: UIView {
    
        override init(frame: CGRect) {
            super.init(frame: frame)
            setup()
        }
    
        required init?(coder aDecoder: NSCoder) {
            super.init(coder: aDecoder)
            setup()
        }
        
        func setup() {
            
            // Create a CAShapeLayer
            let shapeLayer = CAShapeLayer()
            
            // The Bezier path that we made needs to be converted to 
            // a CGPath before it can be used on a layer.
            shapeLayer.path = createBezierPath().CGPath
            
            // apply other properties related to the path
            shapeLayer.strokeColor = UIColor.blueColor().CGColor
            shapeLayer.fillColor = UIColor.whiteColor().CGColor
            shapeLayer.lineWidth = 1.0
            shapeLayer.position = CGPoint(x: 10, y: 10)
            
            // add the new layer to our custom view
            self.layer.addSublayer(shapeLayer)
        }
    
        func createBezierPath() -> UIBezierPath {
            
            // see previous code for creating the Bezier path
        }
    }

And creating our view in the View Controller like this

    override func viewDidLoad() {
        super.viewDidLoad()
        
        // create a new UIView and add it to the view controller
        let myView = MyCustomView()
        myView.frame = CGRect(x: 100, y: 100, width: 50, height: 50)
        myView.backgroundColor = UIColor.yellowColor()
        view.addSubview(myView)
        
    }

We get...

[![enter image description here][5]][5]

Hmm, that's a little small because I hardcoded all the numbers in. I can scale the path size up, though, like this:

    let path = createBezierPath()
    let scale = CGAffineTransformMakeScale(2, 2)
    path.applyTransform(scale)
    shapeLayer.path = path.CGPath

[![enter image description here][6]][6]

**Method 2: Draw path in `drawRect`**

Using `drawRect` is slower than drawing to the layer, so this is not the recommended method if you don't need it.

Here is the revised code for our custom view:

    import UIKit
    class MyCustomView: UIView {
        
        override func drawRect(rect: CGRect) {
            
            // create path (see previous code)
            let path = createBezierPath()
            
            // fill
            let fillColor = UIColor.whiteColor()
            fillColor.setFill()
            
            // stroke
            path.lineWidth = 1.0
            let strokeColor = UIColor.blueColor()
            strokeColor.setStroke()
            
            // Move the path to a new location
            path.applyTransform(CGAffineTransformMakeTranslation(10, 10))
            
            // fill and stroke the path (always do these last)
            path.fill()
            path.stroke()
            
        }
        
        func createBezierPath() -> UIBezierPath {
            
            // see previous code for creating the Bezier path
        }
    }

which gives us the same result...

[![enter image description here][7]][7]

# Further study

Excellent articles for understanding Bezier paths.

- [Thinking like a Bézier path][4] (Everything I've ever read from this author is good and the inspiration for my example above came from here.)
- [Coding Math: Episode 19 - Bezier Curves][8] (entertaining and good visual illustrations)
- [Bezier Curves][9] (how they are used in graphics applications)
- [Bezier Curves][10] (good description of how the mathematical formulas are derived)
 
# Notes

- This example originally comes from [this Stack Overflow answer][11].
- In your actual projects you probably shouldn't use hard coded numbers, but rather get the sizes from your view's bounds.


  [1]: https://developer.apple.com/library/ios/documentation/2DDrawing/Conceptual/DrawingPrintingiOS/BezierPaths/BezierPaths.html
  [2]: http://i.stack.imgur.com/geckR.png
  [3]: http://i.stack.imgur.com/4mkcI.png
  [4]: http://ronnqvi.st/thinking-like-a-bzier-path/
  [5]: http://i.stack.imgur.com/oELys.png
  [6]: http://i.stack.imgur.com/LXvPc.png
  [7]: http://i.stack.imgur.com/2hqMa.png
  [8]: https://www.youtube.com/watch?v=dXECQRlmIaE
  [9]: https://www.youtube.com/watch?v=Qu-QK3uoMdY
  [10]: https://www.youtube.com/watch?v=2HvH9cmHbG4
  [11]: http://stackoverflow.com/a/34659468/3681880

## How to apply corner radius to rectangles drawn by UIBezierPath

*Corner radius for all 4 edges:*

[![enter image description here][1]][1]

     UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) cornerRadius: 11];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*Corner radius for top-left edge:*

[![enter image description here][2]][2] 

     UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerTopLeft cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*Corner radius for top-right edge:*

 [![enter image description here][3]][3]
 

    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerTopRight cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*corner radius for bottom-left edge:*

 [![enter image description here][4]][4]
 

    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerBottomLeft cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*corner radius for bottom-right edge:*

 [![enter image description here][5]][5]
 

     UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerBottomRight cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*corner radius for bottom edges:*

[![enter image description here][6]][6]
 

    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerBottomLeft | UIRectCornerBottomRight cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

*corner radius for top edges:*

[![enter image description here][7]][7]
 
 

    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRoundedRect: CGRectMake(x,y,width,height) byRoundingCorners: UIRectCornerTopLeft | UIRectCornerTopRight cornerRadii: CGSizeMake(11, 11)];
    [rectanglePath closePath];
    [UIColor.grayColor setFill];
    [rectanglePath fill];


  [1]: http://i.stack.imgur.com/O8qTg.png
  [2]: http://i.stack.imgur.com/OEOJa.png
  [3]: http://i.stack.imgur.com/1xhp8.png
  [4]: http://i.stack.imgur.com/yqd87.png
  [5]: http://i.stack.imgur.com/AIgP0.png
  [6]: http://i.stack.imgur.com/4BrrS.png
  [7]: http://i.stack.imgur.com/lecWo.png

## How to apply shadows to UIBezierPath
Consider a simple rectangle that is drawn by the bezier path.

[![enter image description here][1]][1]

     UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRect: CGRectMake(x,y,width,height)];
     [UIColor.grayColor setFill];
     [rectanglePath fill];

  *Basic Outer-fill shadow:*

[![enter image description here][2]][2]

    CGContextRef context = UIGraphicsGetCurrentContext();
    
    NSShadow* shadow = [[NSShadow alloc] init];
    [shadow setShadowColor: UIColor.blackColor];
    [shadow setShadowOffset: CGSizeMake(7.1, 5.1)];
    [shadow setShadowBlurRadius: 5];
    
    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRect: CGRectMake(x,y,width,height)];
    CGContextSaveGState(context);
    CGContextSetShadowWithColor(context, shadow.shadowOffset, shadow.shadowBlurRadius, [shadow.shadowColor CGColor]);
    [UIColor.grayColor setFill];
    [rectanglePath fill];
    CGContextRestoreGState(context);

*Basic Inner fill shadow:*

[![enter image description here][3]][3]

    CGContextRef context = UIGraphicsGetCurrentContext();
    
    NSShadow* shadow = [[NSShadow alloc] init];
    [shadow setShadowColor: UIColor.blackColor];
    [shadow setShadowOffset: CGSizeMake(9.1, -7.1)];
    [shadow setShadowBlurRadius: 6];
    
    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRect: CGRectMake(x,y,width,height)];
    [UIColor.grayColor setFill];
    [rectanglePath fill];
    
    CGContextSaveGState(context);
    UIRectClip(rectanglePath.bounds);
    CGContextSetShadowWithColor(context, CGSizeZero, 0, NULL);
    
    CGContextSetAlpha(context, CGColorGetAlpha([shadow.shadowColor CGColor]));
    CGContextBeginTransparencyLayer(context, NULL);
    {
        UIColor* opaqueShadow = [shadow.shadowColor colorWithAlphaComponent: 1];
        CGContextSetShadowWithColor(context, shadow.shadowOffset, shadow.shadowBlurRadius, [opaqueShadow CGColor]);
        CGContextSetBlendMode(context, kCGBlendModeSourceOut);
        CGContextBeginTransparencyLayer(context, NULL);
    
        [opaqueShadow setFill];
        [rectanglePath fill];
    
        CGContextEndTransparencyLayer(context);
    }
    CGContextEndTransparencyLayer(context);
    CGContextRestoreGState(context);


  [1]: http://i.stack.imgur.com/imDTW.png
  [2]: http://i.stack.imgur.com/mqiIV.png
  [3]: http://i.stack.imgur.com/C1rZk.png


## How to create a simple shapes using UIBezierPath

*For a simple circle:*

[![enter image description here][1]][1]
 

    UIBezierPath* ovalPath = [UIBezierPath bezierPathWithOvalInRect: CGRectMake(0,0,50,50)];
    [UIColor.grayColor setFill];
    [ovalPath fill];

Swift:

    let ovalPath = UIBezierPath(ovalInRect: CGRect(x: 0, y: 0, width: 50, height: 50))
    UIColor.grayColor().setFill()
    ovalPath.fill()

*For a simple Rectangle:*

 
[![enter image description here][2]][2]
 

    UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRect: CGRectMake(0,0,50,50)];
    [UIColor.grayColor setFill];
    [rectanglePath fill];

Swift:

    let rectanglePath = UIBezierPath(rect: CGRect(x: 0, y: 0, width: 50, height: 50))
    UIColor.grayColor().setFill()
    rectanglePath.fill()

*For a simple Line:*


 [![enter image description here][3]][3]

    UIBezierPath* bezierPath = [UIBezierPath bezierPath];
    [bezierPath moveToPoint: CGPointMake(x1,y1)];
    [bezierPath addLineToPoint: CGPointMake(x2,y2)];
    [UIColor.blackColor setStroke];
    bezierPath.lineWidth = 1;
    [bezierPath stroke];

  Swift:
 
    let bezierPath = UIBezierPath()
    bezierPath.moveToPoint(CGPoint(x: x1, y: y1))
    bezierPath.addLineToPoint(CGPoint(x: x2, y: y2))
    UIColor.blackColor().setStroke()
    bezierPath.lineWidth = 1
    bezierPath.stroke()


*For a half circle:*

 [![enter image description here][4]][4]

 

     CGRect ovalRect = CGRectMake(x,y,width,height);
    UIBezierPath* ovalPath = [UIBezierPath bezierPath];
    [ovalPath addArcWithCenter: CGPointMake(0, 0) radius: CGRectGetWidth(ovalRect) / 2 startAngle: 180 * M_PI/180 endAngle: 0 * M_PI/180 clockwise: YES];
    [ovalPath addLineToPoint: CGPointMake(0, 0)];
    [ovalPath closePath];
    
    CGAffineTransform ovalTransform = CGAffineTransformMakeTranslation(CGRectGetMidX(ovalRect), CGRectGetMidY(ovalRect));
    ovalTransform = CGAffineTransformScale(ovalTransform, 1, CGRectGetHeight(ovalRect) / CGRectGetWidth(ovalRect));
    [ovalPath applyTransform: ovalTransform];
    
    [UIColor.grayColor setFill];
    [ovalPath fill];

Swift:

    let ovalRect = CGRect(x: 0, y: 0, width: 50, height: 50)
    let ovalPath = UIBezierPath()
    ovalPath.addArcWithCenter(CGPoint.zero, radius: ovalRect.width / 2, startAngle: 180 * CGFloat(M_PI)/180, endAngle: 0 * CGFloat(M_PI)/180, clockwise: true)
    ovalPath.addLineToPoint(CGPoint.zero)
    ovalPath.closePath()
    
    var ovalTransform = CGAffineTransformMakeTranslation(CGRectGetMidX(ovalRect), CGRectGetMidY(ovalRect))
    ovalTransform = CGAffineTransformScale(ovalTransform, 1, ovalRect.height / ovalRect.width)
    ovalPath.applyTransform(ovalTransform)
    
    UIColor.grayColor().setFill()
    ovalPath.fill()


*For a simple triangle:*

[![enter image description here][5]][5]

 

    UIBezierPath* polygonPath = [UIBezierPath bezierPath];
    [polygonPath moveToPoint: CGPointMake(x1, y1)];
    [polygonPath addLineToPoint: CGPointMake(x2, y2)];
    [polygonPath addLineToPoint: CGPointMake(x3, y2)];
    [polygonPath closePath];
    [UIColor.grayColor setFill];
    [polygonPath fill];

Swift:

    let polygonPath = UIBezierPath()
    polygonPath.moveToPoint(CGPoint(x: x1, y: y1))
    polygonPath.addLineToPoint(CGPoint(x: x2, y: y2))
    polygonPath.addLineToPoint(CGPoint(x: x3, y: y3))
    polygonPath.closePath()
    UIColor.grayColor().setFill()
    polygonPath.fill()


  [1]: http://i.stack.imgur.com/ymoay.png
  [2]: http://i.stack.imgur.com/C1GFH.png
  [3]: http://i.stack.imgur.com/KIAOK.png
  [4]: http://i.stack.imgur.com/3Nywj.png
  [5]: http://i.stack.imgur.com/5r6IE.png



## UIBezierPath + AutoLayout
For bezier path to get resized based on the view frame, override the drawRect of view that you are drawing the bezier path :

    - (void)drawRect:(CGRect)frame
    {
        UIBezierPath* rectanglePath = [UIBezierPath bezierPathWithRect: CGRectMake(CGRectGetMinX(frame), CGRectGetMinY(frame), CGRectGetWidth(frame), CGRectGetHeight(frame))];
        [UIColor.grayColor setFill];
        [rectanglePath fill];
    }



## pie view & column view with UIBezierPath
* pie view  
[![pie view][1]][1]
```objc
- (void)drawRect:(CGRect)rect {

    NSArray *data = @[@30, @15, @5, @17, @3, @10, @20];

    // 1. context
    CGContextRef cxtRef = UIGraphicsGetCurrentContext();

    CGPoint center = CGPointMake(150, 150);
    CGFloat radius = 150;
    __block CGFloat startAngle = 0;
    [data enumerateObjectsUsingBlock:^(NSNumber * _Nonnull obj, NSUInteger idx, BOOL * _Nonnull stop) {

        // 2. create path
        CGFloat endAngle = obj.floatValue / 100 * M_PI * 2 + startAngle;
        UIBezierPath *circlePath = [UIBezierPath bezierPathWithArcCenter:center radius:radius startAngle:startAngle endAngle:endAngle clockwise:YES];
        [circlePath addLineToPoint:center];

        // 3. add path
        CGContextAddPath(cxtRef, circlePath.CGPath);

        // set color
        [[UIColor colorWithRed:((float)arc4random_uniform(256) / 255.0) green:((float)arc4random_uniform(256) / 255.0) blue:((float)arc4random_uniform(256) / 255.0) alpha:1.0] setFill];

        // 4. render
        CGContextDrawPath(cxtRef, kCGPathFill);

        // reset angle
        startAngle = endAngle;
    }];
}
```
```swift

override func draw(_ rect: CGRect) {
    // define data to create pie chart
    let data: [Int] = [30, 15, 5, 17, 3, 10, 20]
    
    // 1. find center of draw rect
    let center: CGPoint = CGPoint(x: rect.midX, y: rect.midY)
    
    // 2. calculate radius of pie
    let radius = min(rect.width, rect.height) / 2.0
    
    var startAngle: CGFloat = 0.0
    for value in data {
      
      // 3. calculate end angle for slice
      let endAngle = CGFloat(value) / 100.0 * CGFloat.pi * 2.0 + startAngle
      
      // 4. create UIBezierPath for slide
      let circlePath = UIBezierPath(arcCenter: center, radius: radius, startAngle: startAngle, endAngle: endAngle, clockwise: true)
      
      // 5. add line to center to close path
      circlePath.addLine(to: center)
      
      // 6. set fill color for current slice
      UIColor(red: (CGFloat(arc4random_uniform(256)) / 255.0), green: (CGFloat(arc4random_uniform(256)) / 255.0), blue: (CGFloat(arc4random_uniform(256)) / 255.0), alpha: 1.0).setFill()
      
      // 7. fill slice path
      circlePath.fill()
      
      // 8. set end angle as start angle for next slice
      startAngle = endAngle
    }
  }
```
* column view  
[![column view][2]][2]
```objc
- (void)drawRect:(CGRect)rect {

    NSArray *data = @[@300, @150.65, @55.3, @507.7, @95.8, @700, @650.65];

    // 1.
    CGContextRef cxtRef = UIGraphicsGetCurrentContext();

    NSInteger columnCount = 7;
    CGFloat width = self.bounds.size.width / (columnCount + columnCount - 1);
    for (NSInteger i = 0; i < columnCount; i++) {

        // 2.
        CGFloat height = [data[i] floatValue] / 1000 * self.bounds.size.height;  // floatValue
        CGFloat x = 0 + width * (2 * i);
        CGFloat y = self.bounds.size.height - height;
        UIBezierPath *rectPath = [UIBezierPath bezierPathWithRect:CGRectMake(x, y, width, height)];
        CGContextAddPath(cxtRef, rectPath.CGPath);

        // 3.
        [[UIColor colorWithRed:((float)arc4random_uniform(256) / 255.0) green:((float)arc4random_uniform(256) / 255.0) blue:((float)arc4random_uniform(256) / 255.0) alpha:1.0] setFill];
        CGContextDrawPath(cxtRef, kCGPathFill);
    }
}
```
```swift
override func draw(_ rect: CGRect) {
    // define data for chart
    let data: [CGFloat] = [300, 150.65, 55.3, 507.7, 95.8, 700, 650.65]
    
    // 1. calculate number of columns
    let columnCount = data.count
    
    // 2. calculate column width
    let columnWidth = rect.width / CGFloat(columnCount + columnCount - 1)
    
    for (columnIndex, value) in data.enumerated() {
      // 3. calculate column height
      let columnHeight = value / 1000.0 * rect.height
      
      // 4. calculate column origin
      let columnOrigin = CGPoint(x: (columnWidth * 2.0 * CGFloat(columnIndex)), y: (rect.height - columnHeight))
      
      // 5. create path for column
      let columnPath = UIBezierPath(rect: CGRect(origin: columnOrigin, size: CGSize(width: columnWidth, height: columnHeight)))
      
      // 6. set fill color for current column
      UIColor(red: (CGFloat(arc4random_uniform(256)) / 255.0), green: (CGFloat(arc4random_uniform(256)) / 255.0), blue: (CGFloat(arc4random_uniform(256)) / 255.0), alpha: 1.0).setFill()
      
      // 7. fill column path
      columnPath.fill()
    }
  }
```

  [1]: http://i.stack.imgur.com/oh0DL.png
  [2]: http://i.stack.imgur.com/ayD5Y.png

