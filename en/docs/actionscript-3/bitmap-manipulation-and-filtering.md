---
title: "Bitmap Manipulation and Filtering"
slug: "bitmap-manipulation-and-filtering"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

In this topic you can learn a bit about manipulating **bitmapdata** and visual processing, working with pixels and getting started with effects filters.

## Threshold (monochrome) effect
----------

## required: ##

 1. understanding Bitmap and Bitmap data

----------

**what is threshold**

> This adjustment takes all the pixels in an image
> and…pushes them to either pure white or pure black

**what we have to do**

> here is a **[Live Demo][1]** of this example with some additional
> changes like using a UI to changing threshold level in runtime.
>
> [![enter image description here][2]][2]

**threshold in action script 3**
<sub>[from as3 official documentation][3]</sub>

> Tests pixel values in an image against a specified threshold and sets
> pixels that pass the test to new color values. Using the threshold()
> method, you can isolate and replace color ranges in an image and
> perform other logical operations on image pixels.
> 
> **The threshold() method's test logic is as follows:**
> 
>  1. If ((pixelValue & mask) operation (threshold & mask)), then set the
>     pixel to color;
>  2. Otherwise, if copySource == true, then set the pixel to
>     corresponding pixel value from sourceBitmap.

i just commented the following code with exactly names as quoted description.

    import flash.display.BitmapData;
    import flash.display.Bitmap;
    import flash.geom.Rectangle;
    import flash.geom.Point;
    
    var bmd:BitmapData = new wildcat(); // instantied a bitmapdata from library a wildcat
    var bmp:Bitmap = new Bitmap(bmd); // our display object to previewing bitmapdata on stage
    addChild(bmp);
    monochrome(bmd); // invoking threshold function
    
    /**
        @param bmd, input bitmapData that should be monochromed
    */
    function monochrome(bmd:BitmapData):void {
        var bmd_copy:BitmapData = bmd.clone(); // holding a pure copy of bitmapdata for comparation steps
        // this is our "threshold" in description above, source pixels will be compared with this value
        var level:uint = 0xFFAAAAAA; // #AARRGGBB. in this case i used RGB(170,170,170) with an alpha of 1. its not median but standard
        // A rectangle that defines the area of the source image to use as input.
        var rect:Rectangle = new Rectangle(0,0,bmd.width,bmd.height);
        // The point within the destination image (the current BitmapData instance) that corresponds to the upper-left corner of the source rectangle.
        var dest:Point = new Point();
        // thresholding will be done in two section
        // the last argument is "mask", which exists in both sides of comparation
        // first, modifying pixels which passed comparation and setting them all with "color" white (0xFFFFFFFF)
        bmd.bitmapData.threshold(bmd_copy, rect, dest, ">", level, 0xFFFFFFFF, 0xFFFFFFFF);
        // then, remaining pixels and make them all with "color" black (0xFF000000)
        bmd.bitmapData.threshold(bmd_copy, rect, dest, "<=", level, 0xFF000000, 0xFFFFFFFF);
        // Note: as we have no alpha channel in our default BitmapData (pixelValue), we left it to its full value, a white mask (0xffffffff)
    }


  [1]: http://www.fastswf.com/inDKZD8
  [2]: https://i.stack.imgur.com/4ZgER.png
  [3]: http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/display/BitmapData.html#threshold()

