---
title: "UIImage"
slug: "uiimage"
draft: false
images: []
weight: 9839
type: docs
toc: true
---

Apple developer topic for [UIImage][1] 


  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIImage_Class/

## Creating UIImage
# With local image

## Swift

    let image = UIImage(named: "imageFromBundleOrAsset")

## Objective-C

    UIImage *image = [UIImage imageNamed:@"imageFromBundleOrAsset"];


># Note
> The method [`imageNamed`][1] caches the image's contents to memory.
> Loading many large images that way can cause low memory warnings which
> can lead the app to be terminated. This can be fixed by utilising the
> method `imageWithContentsOfFile` of `UIImage`, which doesn't use
> caching.

# With NSData

## Swift
    
    let imageData = Data(base64Encoded: imageString, options: Data.Base64DecodingOptions.ignoreUnknownCharacters)

    let image = UIImage(data: imageData!)

# With UIColor

## Swift

    let color = UIColor.red
    let size = CGSize(width: 200, height: 200)
        
    UIGraphicsBeginImageContextWithOptions(size, false, 0.0)
    UIGraphicsGetCurrentContext()!.setFillColor(color.cgColor)
    UIGraphicsGetCurrentContext()!.fill(CGRect(origin: .zero, size: size))
    let colorImage = UIGraphicsGetImageFromCurrentImageContext()
    UIGraphicsEndImageContext()

## Objective-C

    UIColor *color=[UIColor redColor];
    CGRect frame = CGRectMake(0, 0, 80, 100);
    UIGraphicsBeginImageContext(frame.size);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetFillColorWithColor(context, [color CGColor]);
    CGContextFillRect(context, frame);
    UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();

# With file content

# Objective-C

Example:

 

    UIImage *image = [UIImage imageWithContentsOfFile:[[NSBundle mainBundle] pathForResource:[cellCountry objectForKey:@"Country_Flag"] ofType:nil]];

Using Array:

Example:

 

    NSMutableArray *imageArray = [[NSMutableArray alloc] init];
    
    for (int imageNumber = 1; self.myPhoto != nil; imageNumber++) {
        NSString *fileName = [NSString stringWithFormat:@"%@.jpg", self.myPhoto];
    
        // check if a file exists
        if ([UIImage imageNamed:fileName]) {
            // if it exists, add it to the array
            [imageArray addObject:[UIImage imageWithContentsOfFile:[[NSBundle mainBundle]pathForResource:[NSString stringWithFormat:@"%@", fileName] ofType:@""]]];
        } else {
            break;
        }
    }

//Using image array for animations here:

    self.myImageView.animationImages = imageArray;

  [1]: https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIImage_Class/index.html#//apple_ref/occ/clm/UIImage/imageNamed:

## Comparing Images
> The `isEqual:` method is the only reliable way to determine whether two
> images contain the same image data. The image objects you create may
> be different from each other, even when you initialize them with the
> same cached image data. The only way to determine their equality is to
> use the `isEqual:` method, which compares the actual image data. Listing
> 1 illustrates the correct and incorrect ways to compare images.
>
> Source: [Apple Documentation][1]

## Swift

    // Load the same image twice.
    let image1 = UIImage(named: "MyImage")
    let image2 = UIImage(named: "MyImage")
    
    // The image objects may be different, but the contents are still equal
    if let image1 = image1, image1.isEqual(image2) {
        // Correct. This technique compares the image data correctly.
    }
    
    if image1 == image2 {
        // Incorrect! Direct object comparisons may not work.
    }

## Objective-C

    // Load the same image twice.
    UIImage* image1 = [UIImage imageNamed:@"MyImage"];
    UIImage* image2 = [UIImage imageNamed:@"MyImage"];
     
    // The image objects may be different, but the contents are still equal
    if ([image1 isEqual:image2]) {
       // Correct. This technique compares the image data correctly.
    }
     
    if (image1 == image2) {
       // Incorrect! Direct object comparisons may not work.
    }


  [1]:https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIImage_Class/

## Gradient Image with Colors
Creating Gradient `UIImage` with colors in `CGRect`

Swift:

    extension UIImage {
        static func gradientImageWithBounds(bounds: CGRect, colors: [CGColor]) -> UIImage {
            let gradientLayer = CAGradientLayer()
            gradientLayer.frame = bounds
            gradientLayer.colors = colors
            
            UIGraphicsBeginImageContext(gradientLayer.bounds.size)
            gradientLayer.render(in: UIGraphicsGetCurrentContext()!)
            let image = UIGraphicsGetImageFromCurrentImageContext()
            UIGraphicsEndImageContext()
            return image!
        }
    }

Usage: 

    let image = UIImage.gradientImageWithBounds(CGRect(x: 0, y: 0, width: 200, height: 200), colors: [UIColor.yellowColor().CGColor, UIColor.blueColor().CGColor])

Objective-C:

    + (UIImage *)gradientImageWithBounds:(CGRect)bounds colors:(NSArray *)colors {
        CAGradientLayer *gradientLayer = [CAGradientLayer layer];
        gradientLayer.frame = bounds;
        gradientLayer.colors = colors;
        
        UIGraphicsBeginImageContext(gradientLayer.bounds.size);
        [gradientLayer renderInContext:UIGraphicsGetCurrentContext()];
        UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        return image;
    }

## Convert UIImage to/from base64 encoding
**Encoding**

    //convert the image to NSData first
    let imageData:NSData = UIImagePNGRepresentation(image)!
    // convert the NSData to base64 encoding
    let strBase64:String = imageData.base64EncodedStringWithOptions(.Encoding64CharacterLineLength)

**Decoding**

    let dataDecoded:NSData = NSData(base64EncodedString: strBase64, options: NSDataBase64DecodingOptions(rawValue: 0))!
    let decodedimage:UIImage = UIImage(data: dataDecoded)!

## Creating and Initializing Image Objects with file contents
Creating and returning an image object by loading the image data from the file at the specified path.

Example:

     UIImage *image = [UIImage imageWithContentsOfFile:[[NSBundle mainBundle] pathForResource:[cellCountry objectForKey:@"Country_Flag"] ofType:nil]];

Using Array:

Example

     NSMutableArray *imageArray = [[NSMutableArray alloc] init];

    for (int imageNumber = 1; self.myPhoto != nil; imageNumber++) {
        NSString *fileName = [NSString stringWithFormat:@"%@.jpg", self.myPhoto];

        // check if a file exists
        if ([UIImage imageNamed:fileName]) {
            // if it exists, add it to the array
            [imageArray addObject:[UIImage imageWithContentsOfFile:[[NSBundle mainBundle]pathForResource:[NSString stringWithFormat:@"%@", fileName] ofType:@""]]];
        } else {
            break;
        }
    }

    //Using image array for animations here
    self.myImageView.animationImages = imageArray;

## Resizable image with caps
In the example of a message bubble illustrated below: the corners of the image should remain unchanged which is specified by `UIEdgeInsets`, but the borders and center of the image should expand to cover the new size.

[![original image][1]][1]

[![resized image][2]][2]

     let insets = UIEdgeInsetsMake(12.0, 20.0, 22.0, 12.0)
     let image = UIImage(named: "test")
     image?.resizableImageWithCapInsets(insets, resizingMode: .Stretch)


  [1]: http://i.stack.imgur.com/F4le4.png
  [2]: http://i.stack.imgur.com/DkDO3.png

## Gradient Background Layer for Bounds
    + (CALayer *)gradientBGLayerForBounds:(CGRect)bounds colors:(NSArray *)colors
    {
        CAGradientLayer * gradientBG = [CAGradientLayer layer];
        gradientBG.frame = bounds;
        gradientBG.colors = colors;
        return gradientBG;
    }

## Take a Snapshot of a UIView
    //Here self.webView is the view whose screenshot I need to take
    //The screenshot is saved in jpg format in the application directory to avoid any loss of quality in retina display devices i.e. all current devices running iOS 10    
    UIGraphicsBeginImageContextWithOptions(self.webView.bounds.size, NO, [UIScreen mainScreen].scale);
    [self.webView.layer renderInContext:UIGraphicsGetCurrentContext()];
    UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    NSString  *jpgPath = [NSHomeDirectory() stringByAppendingPathComponent:@"Documents/Test.jpg"];
    [UIImageJPEGRepresentation(image, 1.0) writeToFile:jpgPath atomically:YES];
    UIImage *pop=[[UIImage alloc]initWithContentsOfFile:jpgPath];
    //pop is the final image in jpg format and high quality with the exact resolution of the view you selected in pixels and not just points  

 




## Apply UIColor to UIImage
Use same UIImage with multiple theme base app by just applying UIColor to UIImage instance as following.

    // *** Create an UIImage instance with RenderingMode AlwaysTemplate ***
    UIImage *imgMenu = [[UIImage imageNamed:@"iconMenu"] imageWithRenderingMode:UIImageRenderingModeAlwaysTemplate];

    // *** Now Apply `tintColor` to `UIImageView` of UIImageView or UIButton and convert image in given color ***
    [btn setImage:imgMenu forState:UIControlStateNormal]; // Set UIImage in UIButton.

    [button.imageView setTintColor:[UIColor blueColor]]; // It changes image color of UIButton to blue color

Now lets say you want to do same with UIImageView then use following code
    
    [imageView setImage:imgMenu]; // Assign UIImage to UIImageView
    [imageView setTintColor:[UIColor greenColor]]; // Change imageview image color to green.
    [imageView setTintColor:[UIColor redColor]]; // Change imageview image color to red.

## Change UIImage Color
**Swift**
Add this extension to UIImage :

    extension UIImage {
        func maskWithColor(color: UIColor) -> UIImage? {
        
            let maskImage = self.CGImage
            let width = self.size.width
            let height = self.size.height
            let bounds = CGRectMake(0, 0, width, height)
        
            let colorSpace = CGColorSpaceCreateDeviceRGB()
            let bitmapInfo = CGBitmapInfo(rawValue: CGImageAlphaInfo.PremultipliedLast.rawValue)
            let bitmapContext = CGBitmapContextCreate(nil, Int(width), Int(height), 8, 0, colorSpace, bitmapInfo.rawValue) //needs rawValue of bitmapInfo
        
            CGContextClipToMask(bitmapContext, bounds, maskImage)
            CGContextSetFillColorWithColor(bitmapContext, color.CGColor)
            CGContextFillRect(bitmapContext, bounds)
        
            //is it nil?
            if let cImage = CGBitmapContextCreateImage(bitmapContext) {
                let coloredImage = UIImage(CGImage: cImage)
            
                return coloredImage
            
            } else {
                return nil
            }
        }
    }
Then, to change the color of your UIImage

    my_image.maskWithColor(UIColor.blueColor())

Found [at this link][1]


  [1]: http://stackoverflow.com/questions/31803157/how-to-color-a-uiimage-in-swift "This post"

## Create UIImage with UIColor
Swift 
=====

    let color = UIColor.redColor()
    let size = CGSize(width: 200, height: 200)

    UIGraphicsBeginImageContextWithOptions(size, false, 0.0)
    CGContextSetFillColorWithColor(UIGraphicsGetCurrentContext(), color.CGColor)
    CGContextFillRect(UIGraphicsGetCurrentContext(), CGRect(origin: .zero, size: size))
    let colorImage = UIGraphicsGetImageFromCurrentImageContext()
    UIGraphicsEndImageContext()

Swift 3
=======

    let color = UIColor.red()
    let size = CGSize(width: 200, height: 200)

    UIGraphicsBeginImageContextWithOptions(size, false, 0.0)
    if let context = UIGraphicsGetCurrentContext() {
        context.setFillColor(color.cgColor)
        context.fill(CGRect(origin: .zero, size: size))
        let colorImage = UIGraphicsGetImageFromCurrentImageContext()
    }
    UIGraphicsEndImageContext()

Objective-C:
===========

Add this method as an extension of `UIImage`:

    + (UIImage *)createImageWithColor: (UIColor *)color {
        CGRect rect=CGRectMake(0.0f, 0.0f, 1.0f, 1.0f);
        UIGraphicsBeginImageContext(rect.size);
        CGContextRef context = UIGraphicsGetCurrentContext();
        CGContextSetFillColorWithColor(context, [color CGColor]);
        CGContextFillRect(context, rect);
    
        UIImage *theImage = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        return theImage;
    }

