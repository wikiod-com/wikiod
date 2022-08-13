---
title: "Drawing Bitmaps"
slug: "drawing-bitmaps"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Draw a display object into bitmap data
A helper function to create a bitmap copy of an object.  This can be used to convert vector objects, text or complex nested Sprite's to a flattened bitmap.

    function makeBitmapCopy(displayObj:IBitmapDrawable, transparent:Boolean = false, bgColor:uint = 0x00000000, smooth:Boolean = true):Bitmap {

        //create an empty bitmap data that matches the width and height of the object you wish to draw
        var bmd:BitmapData = new BitmapData(displayObj.width, displayObj.height, transparent, bgColor);
                
        //draw the object to the bitmap data
        bmd.draw(displayObj, null, null, null, null, smooth);
                
        //assign that bitmap data to a bitmap object
        var bmp:Bitmap = new Bitmap(bmd, "auto", smooth);
                
        return bmp;
    }

Usage:

    var txt:TextField = new TextField();
    txt.text = "Hello There";

    var bitmap:Bitmap = makeBitmapCopy(txt, true); //second param true to keep transparency
    addChild(bitmap);

## Draw a display object with any coordinates of registration point
        public function drawDisplayObjectUsingBounds(source:DisplayObject):BitmapData {
            var bitmapData:BitmapData;//declare a BitmapData
            var bounds:Rectangle = source.getBounds(source);//get the source object actual size
            //round bounds to integer pixel values (to aviod 1px stripes left off)
            bounds = new Rectangle(Math.floor(bounds.x), Math.floor(bounds.y), Math.ceil(bounds.width), Math.ceil(bounds.height));

            //to avoid Invalid BitmapData error which occures if width or height is 0
            //(ArgumentError: Error #2015)
            if((bounds.width>0) && (bounds.height>0)){
                //create a BitmapData
                bitmapData = new BitmapData(bounds.width, bounds.height, true, 0x00000000);                
                var matrix:Matrix = new Matrix();//create a transform matrix
                //translate if to fit the upper-left corner of the source
                matrix.translate(-bounds.x, -bounds.y);
                bitmapData.draw(source, matrix);//draw the source
                return bitmapData;//return the result (exit point)
            }
            //if no result is created - return an empty BitmapData
            return new BitmapData(1, 1, true, 0x00000000);
        }

A side note: For `getBounds()` to return valid values the object has to have stage access at least once, otherwise the values are bogus. A code can be added to ensure that passed `source` has stage, and if not, it can be added to stage then removed again.

## Animating a sprite sheet
A sprite sheet by definition is a bitmap that contains a certain animation. Old games use grid type sprite sheet, that is, every frame occupies an equal region, and frames are aligned by the edges to form a rectangle, probably with some spaces unoccupied. Later, in order to minimize the bitmap size, sprite sheets start to be "packed" by removing extra whitespace around the rectangle that contains each frame, but still each frame is a rectangle to simplify copying operations. 

In order to animate a sprite sheet, two techniques can be used. First, you can use `BitmapData.copyPixels()` to copy a certain region of your sprite sheet to a displayed `Bitmap`, producing an animated character. This approach is better if you use a single displayed `Bitmap` that hosts the entire picture.

    var spriteSheet:BitmapData;
    var frames:Vector.<Rectangle>; // regions of spriteSheet that represent frames
    function displayFrameAt(frame:int,buffer:BitmapData,position:Point):void {
        buffer.copyPixels(spriteSheet,frames[frame],position,null,null,true);
    }

The second technique can be used if you have a lot of `Sprite`s or `Bitmap`s on the display list, and they share the same sprite sheet. Here, the target buffer is no longer a single object, but each object has its own buffer, so the proper strategy is to manipulate the bitmaps' `bitmapData` property. Prior to doing this, however, the sprite sheet should be cut apart into individual frames.

    public class Stuff extends Bitmap {
        static var spriteSheet:Vector.<BitmapData>;
        function displayFrame(frame:int) {
            this.bitmapData=spriteSheet[frame]; 
        }
        // ...
    }

