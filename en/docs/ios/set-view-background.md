---
title: "Set View Background"
slug: "set-view-background"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Set View background
Objective C:

    view.backgroundColor = [UIColor redColor];

Swift:

    view.backgroundColor! = UIColor.redColor()

Swift 3

    view.backgroundColor = UIColor.redColor

## Fill background Image of a UIView
**Objective-C**
   

     UIGraphicsBeginImageContext(self.view.frame.size);
     [[UIImage imageNamed:@"image.png"] drawInRect:self.view.bounds];
     UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
     UIGraphicsEndImageContext();     
     self.view.backgroundColor = [UIColor colorWithPatternImage:image];

## Set View backround with image


    self.view.backgroundColor = [UIColor colorWithPatternImage:[UIImage imageNamed:@"Background.png"]];

## Creating a gradient background view
To create a background with a gradient you can use the [CAGradientLayer][1] class:

Swift 3.1:

    func createGradient() { 
        let caLayer = CAGradientLayer()
        caLayer.colors = [UIColor.white, UIColor.green, UIColor.blue]
        caLayer.locations = [0, 0.5, 1]
        caLayer.bounds = self.bounds
        self.layer.addSublayer(caLayer) 
    }

This can be called on viewDidLoad() like so:

    override func viewDidLoad() {
        super.viewDidLoad()
        createGradient()
    }

The CAGradientLayer locations and bounds variables can take multiple values to create a gradient layer with how ever many colors you desire. From the documentation: 

> By default, the colors are spread uniformly across the layer, but you can optionally specify locations for control over the color positions through the gradient.


  [1]: https://developer.apple.com/reference/quartzcore/cagradientlayer

