---
title: "CALayer"
slug: "calayer"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

## Adding Transforms to a CALayer (translate, rotate, scale)
# Basics

There are a number of different transforms you can do on a layer, but the basic ones are

- translate (move)
- scale
- rotate

[![basic transforms][1]][1]

To do transforms on a `CALayer`, you set the layer's `transform` property to a `CATransform3D` type. For example, to translate a layer, you would do something like this:

    myLayer.transform = CATransform3DMakeTranslation(20, 30, 0)

The word `Make` is used in the name for creating the initial transform: CATransform3D**Make**Translation. Subsequent transforms that are applied omit the `Make`. See, for example, this rotation followed by a translation:

    let rotation = CATransform3DMakeRotation(CGFloat(30.0 * M_PI / 180.0), 20, 20, 0)
    myLayer.transform = CATransform3DTranslate(rotation, 20, 30, 0)

Now that we have the basis of how to make a transform, let's look at some examples of how to do each one. First, though, I'll show how I set up the project in case you want to play around with it, too.

# Setup

For the following examples I set up a Single View Application and added a `UIView` with a light blue background to the storyboard. I hooked up the view to the view controller with the following code:

    import UIKit
    
    class ViewController: UIViewController {
        
        var myLayer = CATextLayer()
        @IBOutlet weak var myView: UIView!
        
        override func viewDidLoad() {
            super.viewDidLoad()
            
            // setup the sublayer
            addSubLayer()
            
            // do the transform
            transformExample()
        }
        
        func addSubLayer() {
            myLayer.frame = CGRect(x: 0, y: 0, width: 100, height: 40)
            myLayer.backgroundColor = UIColor.blueColor().CGColor
            myLayer.string = "Hello"
            myView.layer.addSublayer(myLayer)
        }
        
        //******** Replace this function with the examples below ********

        func transformExample() {
            
            // add transform code here ...
            
            
        }
    
    } 

[There are many different kinds of `CALayer`][2], but I chose to use `CATextLayer` so that the transforms will be more clear visually.

# Translate

The translation transform moves the layer. The basic syntax is 

    CATransform3DMakeTranslation(tx: CGFloat, ty: CGFloat, tz: CGFloat)

where `tx` is the change in the x coordinates, `ty` is the change in y, and `tz` is the change in z. 

**Example**

[![translate example][3]][3]

In iOS the origin of the coordinate system is in the top left, so if we wanted to move the layer 90 points to the right and 50 points down, we would do the following:

    myLayer.transform = CATransform3DMakeTranslation(90, 50, 0)

Notes

- Remember that you can paste this into the `transformExample()` method in the project code above.
- Since we are just going to deal with two dimensions here, `tz` is set to `0`. 
- The red line in the image above goes from the center of the original location to the center of the new location. That's because transforms are done in relation to the anchor point and the anchor point by default is in the center of the layer.

# Scale

The scale transform stretches or squishes the layer.  The basic syntax is 

    CATransform3DMakeScale(sx: CGFloat, sy: CGFloat, sz: CGFloat)

where `sx`, `sy`, and `sz` are the numbers by which to scale (multiply) the x, y, and z coordinates respectively. 

**Example**

[![scale example][4]][4]

If we wanted to half the width and triple the height, we would do the following

    myLayer.transform = CATransform3DMakeScale(0.5, 3.0, 1.0)

Notes

- Since we are only working in two dimensions, we just multiply the z coordinates by 1.0 to leave them unaffected.
- The red dot in the image above represents the anchor point. Notice how the scaling is done in relation to the anchor point. That is, everything is either stretched toward or away from the anchor point.

# Rotate

The rotation transform rotates the layer around the anchor point (the center of the layer by default). The basic syntax is 

    CATransform3DMakeRotation(angle: CGFloat, x: CGFloat, y: CGFloat, z: CGFloat)

where `angle` is the angle in radians that the layer should be rotated and `x`, `y`, and `z` are the axes about which to rotate. Setting an axis to 0 cancels a rotation around that particular axis.

**Example**

[![rotate example][5]][5]

If we wanted to rotate a layer clockwise 30 degrees, we would do the following:

    let degrees = 30.0
    let radians = CGFloat(degrees * M_PI / 180)
    myLayer.transform = CATransform3DMakeRotation(radians, 0.0, 0.0, 1.0)

Notes

- Since we are working in two dimentions, we only want the xy plane to be rotated around the z axis. Thus we set `x` and `y` to `0.0` and set `z` to `1.0`.
- This rotated the layer in a clockwise direction. We could have rotated counterclockwise by setting `z` to `-1.0`.
- The red dot shows where the anchor point is. The rotation is done around the anchor point.

# Multiple transforms

In order to combine multiple transforms we could use concatination like this

    CATransform3DConcat(a: CATransform3D, b: CATransform3D)

However, we will just do one after another. The first transform will use the `Make` in its name. The following transforms will not use `Make`, but they will take the previous transform as a parameter.

**Example**

[![multiple transforms example][6]][6]

This time we combine all three of the previous transforms.

    let degrees = 30.0
    let radians = CGFloat(degrees * M_PI / 180)
    
    // translate
    var transform = CATransform3DMakeTranslation(90, 50, 0)
    
    // rotate
    transform = CATransform3DRotate(transform, radians, 0.0, 0.0, 1.0)
    
    // scale
    transform = CATransform3DScale(transform, 0.5, 3.0, 1.0)
    
    // apply the transforms
    myLayer.transform = transform

Notes

- The order that the transforms are done in matters. 
- Everything was done in relation to the anchor point (red dot).

# A Note about Anchor Point and Position

We did all our transforms above without changing the anchor point. Sometimes it is necessary to change it, though, like if you want to rotate around some other point besides the center. However, this can be a little tricky.

The anchor point and position are both at the same place. The anchor point is expressed as a unit of the layer's coordinate system (default is `0.5, 0.5`) and the position is expressed in the superlayer's coordinate system. They can be set like this

    myLayer.anchorPoint = CGPoint(x: 0.0, y: 1.0)
    myLayer.position = CGPoint(x: 50, y: 50)

If you only set the anchor point without changing the position, then the frame changes so that the position will be in the right spot. Or more precisely, the frame is recalculated based on the new anchor point and old position. This usually gives unexpected results. The following two articles have an excellent discussion of this.

- [About the anchorPoint][7]
- [Translate rotate translate?][8]

# See also

- [Border, rounded corners, and shadow on a `CALayer`][9]
- [Using a border with a Bezier path for a layer][10]

*This example originally comes from [this Stack Overflow example][11].*


  [1]: http://i.stack.imgur.com/M6hZN.png
  [2]: http://www.raywenderlich.com/90488/calayer-in-ios-with-swift-10-examples
  [3]: http://i.stack.imgur.com/yoEez.png
  [4]: http://i.stack.imgur.com/W9W99.png
  [5]: http://i.stack.imgur.com/Kbnun.png
  [6]: http://i.stack.imgur.com/w9osU.png
  [7]: http://ronnqvi.st/about-the-anchorpoint/
  [8]: http://ronnqvi.st/translate-rotate-translate/
  [9]: http://stackoverflow.com/a/34984063/3681880
  [10]: http://stackoverflow.com/a/34659468/3681880
  [11]: http://stackoverflow.com/a/34438890/3681880

## Emitter View with custom image
For example we will create view that contains emitter layer and animates particles.


    import QuartzCore
    
    class ConfettiView: UIView {
        // main emitter layer
        var emitter: CAEmitterLayer!

        // array of color to emit
        var colors: [UIColor]!

        // intensity of appearance
        var intensity: Float!
        
        private var active :Bool!
        
        required init?(coder aDecoder: NSCoder) {
            super.init(coder: aDecoder)
            setup()
        }
        
        override init(frame: CGRect) {
            super.init(frame: frame)
            setup()
        }
        
        func setup() {
            // initialization
            colors = [UIColor.redColor(),
                      UIColor.greenColor(),
                      UIColor.blueColor()
                      ]
            intensity = 0.2
            
            active = false
        }
        
        func startConfetti() {
            emitter = CAEmitterLayer()
            
            emitter.emitterPosition = CGPoint(x: frame.size.width / 2.0, y: -20)
            emitter.emitterShape = kCAEmitterLayerLine
            emitter.emitterSize = CGSize(width: frame.size.width, height: 1)
            
            var cells = [CAEmitterCell]()
            for color in colors {
                cells.append(confettiWithColor(color))
            }
            
            emitter.emitterCells = cells
            layer.addSublayer(emitter)
            active = true
        }
        
        func stopConfetti() {
            emitter?.birthRate = 0
            active = false
        }
        
        func confettiWithColor(color: UIColor) -> CAEmitterCell {
            let confetti = CAEmitterCell()

            confetti.birthRate = 10.0 * intensity
            confetti.lifetime = 180.0 * intensity
            confetti.lifetimeRange = 0
            confetti.color = color.CGColor
            confetti.velocity = CGFloat(350.0 * intensity)
            confetti.velocityRange = CGFloat(40.0 * intensity)
            confetti.emissionLongitude = CGFloat(M_PI)
            confetti.emissionRange = CGFloat(M_PI_4)
            confetti.spin = CGFloat(3.5 * intensity)
            confetti.spinRange = CGFloat(4.0 * intensity)
            
            // WARNING: A layer can set this property to a CGImageRef to display the image as its contents.
            confetti.contents = UIImage(named: "confetti")?.CGImage
            return confetti
        }
        
        internal func isActive() -> Bool {
            return self.active
        }
    }

You need to add "confetti" image or define rect with **confetti.contentsRect**

## Shadows
You can use 5 properties on each layer to configure your shadows:

- `shadowOffset` - this property moves your shadow left/right or up/down
```
self.layer.shadowOffset = CGSizeMake(-1, -1); // 1px left and up

self.layer.shadowOffset = CGSizeMake(1, 1); // 1px down and right
```

- `shadowColor` - this sets the color of your shadow

```
self.layer.shadowColor = [UIColor blackColor].CGColor;
```

- `shadowOpacity` - this is the opacity of the shadow, from `0` to `1`
```
self.layer.shadowOpacity = 0.2;
```

- `shadowRadius` - this is the blur radius (equivalent of the blur property in Sketch or Photoshop)
```
self.layer.shadowRadius = 6;
```
- `shadowPath` - this is an important property for performance, when unset iOS bases the shadow on the alpha channel of the view, which can be performance intensive with a complex PNG with alpha. This property lets you force a shape for your shadow and be more performant because of it.

**Objective-C**
```
self.layer.shadowPath = [UIBezierPath bezierPathWithOvalInRect:CGRectMake(0,0,100,100)]; //this does a circular shadow
```
**Swift 3**
```
self.layer.shadowPath = UIBezierPath(ovalIn: CGRect(x: 0, y: 0, width: 100, height: 100)).cgPath
```

## Creating particles with CAEmitterLayer

The **CAEmitterLayer** class provides a particle emitter system for Core Animation. The particles are defined by instances of **CAEmitterCell**.

The particles are drawn above the layer’s background color and border.

            var emitter = CAEmitterLayer()
            
            emitter.emitterPosition = CGPoint(x: frame.size.width / 2.0, y: -20)
            emitter.emitterShape = kCAEmitterLayerLine
            emitter.emitterSize = CGSize(width: frame.size.width, height: 1)

            emitter.emitterCells = cells
            layer.addSublayer(emitter)

## How to add a UIImage to a CALayer
You can add an image to a view's `layer` simply by using its `contents` property:

    myView.layer.contents = UIImage(named: "star")?.CGImage

- Note that the `UIImage` needs to be converted to a `CGImage`.

If you wish to add the image in its own layer, you can do it like this:

    let myLayer = CALayer()
    let myImage = UIImage(named: "star")?.CGImage
    myLayer.frame = myView.bounds
    myLayer.contents = myImage
    myView.layer.addSublayer(myLayer)

# Modifying the appearance

The above code produces a view like this. The light blue is the `UIView` and the dark blue star is the `UIImage`. 

[![star image on a CALayer][1]][1]

As you can see, though, it looks pixelated. This is because the `UIImage` is smaller than the `UIView` so it is being scaled to fill the view, which is the default it you don't specify anything else. 

The examples below show variations on the layer's `contentsGravity` property. The code looks like this:

    myView.layer.contents = UIImage(named: "star")?.CGImage
    myView.layer.contentsGravity = kCAGravityTop
    myView.layer.geometryFlipped = true

In iOS, you may want to set the [`geometryFlipped` property][2] to `true` if you are doing anything with top or bottom gravity, otherwise it will be the opposite of what you expect. (Only the gravity is flipped vertically, not the content rendering. If you are having trouble with the content being flipped, see [this Stack Overflow answer][3].)

There are two `UIView` examples below for every `contentsGravity` setting, one view is larger than the `UIImage` and the other is smaller. This way you can see the effects of the scaling and gravity.

**`kCAGravityResize`**

This is the default.

[![kCAGravityResize][4]][4]

**`kCAGravityResizeAspect`**

[![kCAGravityResizeAspect][5]][5]

**`kCAGravityResizeAspectFill`**

[![kCAGravityResizeAspectFill][6]][6]

**`kCAGravityCenter`**

[![kCAGravityCenter][7]][7]

**`kCAGravityTop`**

[![kCAGravityTop][8]][8]

**`kCAGravityBottom`**

[![kCAGravityBottom][9]][9]

**`kCAGravityLeft`**

[![kCAGravityLeft][10]][10]

**`kCAGravityRight`**

[![kCAGravityRight][11]][11]

**`kCAGravityTopLeft`**

[![kCAGravityTopLeft][12]][12]

**`kCAGravityTopRight`**

[![kCAGravityTopRight][13]][13]

**`kCAGravityBottomLeft`**

[![kCAGravityBottomLeft][14]][14]

**`kCAGravityBottomRight`**

[![kCAGravityBottomRight][15]][15]

# Related

- [Content mode property of a view][16]
- [Drawing a `UIImage` in `drawRect` with `CGContextDrawImage`][3]
- [CALayer Tutorial: Getting Started][17]

# Notes

- This example comes originally from [this Stack Overflow answer][18].


  [1]: http://i.stack.imgur.com/j1NW1.png
  [2]: https://developer.apple.com/library/ios/documentation/GraphicsImaging/Reference/CALayer_class/#//apple_ref/occ/instp/CALayer/geometryFlipped
  [3]: http://stackoverflow.com/a/34407999/3681880
  [4]: http://i.stack.imgur.com/rfejR.png
  [5]: http://i.stack.imgur.com/w5zBS.png
  [6]: http://i.stack.imgur.com/7406s.png
  [7]: http://i.stack.imgur.com/1imPL.png
  [8]: http://i.stack.imgur.com/Ge3i4.png
  [9]: http://i.stack.imgur.com/H00Hn.png
  [10]: http://i.stack.imgur.com/kjgww.png
  [11]: http://i.stack.imgur.com/uUtkW.png
  [12]: http://i.stack.imgur.com/VdJUV.png
  [13]: http://i.stack.imgur.com/U7jLd.png
  [14]: http://i.stack.imgur.com/FDktZ.png
  [15]: http://i.stack.imgur.com/dEf1W.png
  [16]: http://stackoverflow.com/a/32151862/3681880
  [17]: http://www.raywenderlich.com/90488/calayer-in-ios-with-swift-10-examples
  [18]: http://stackoverflow.com/a/34603690/3681880

## Disable Animations
`CALayer` property animations are enabled by default. When this is undesirable, they can be disabled as follows.

**Swift**

    CATransaction.begin()
    CATransaction.setDisableActions(true)
    
    // change layer properties that you don't want to animate
    
    CATransaction.commit()

**Objective-C**

    [CATransaction begin];
    [CATransaction setDisableActions:YES];
    
    // change layer properties that you don't want to animate
    
    [CATransaction commit];

## Rounded corners
    layer.masksToBounds = true;
    layer.cornerRadius = 8;

## Creating a CALayer
You can create a CALayer and set its frame like this:

Swift: 

    let layer = CALayer()
    layer.frame = CGRect(x: 0, y: 0, width: 60, height: 80)

Objective-C:

    CALayer *layer = [[CALayer alloc] init];
    layer.frame = CGRectMake(0, 0, 60, 80);


You can then add it as a sublayer to an existing CALayer:
    
Swift:

    existingLayer.addSublayer(layer)

Objective-C:

    [existingLayer addSublayer:layer];

Note:

To do this you need to include the QuartzCore framework. 
    
Swift:

     @import QuartzCore

Objective-C

    #import <QuartzCore/QuartzCore.h>


