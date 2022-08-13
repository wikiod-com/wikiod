---
title: "Optimizing Performance"
slug: "optimizing-performance"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Vectors instead of Arrays
Flash Player 10 introduced the Vector.<*> generic list type that was faster than the Array. However, this is not entirely true. Only the following Vector types are faster than the Array counterparts, due to the way they are implemented in Flash Player.

- `Vector.<int>` - Vector of 32-bit integers
- `Vector.<uint>` - Vector of 32-bit unsigned integers
- `Vector.<Double>` - Vector of 64-bit floats

In all other cases, using an Array will be more performant than using Vectors, for all operations (creation, manipulation, etc). However, if you wish to "strongly type" your code then you can use Vectors despite the slowdown. FlashDevelop has a syntax that enables code completion drop-downs to work even for Arrays, by using `/*ObjectType*/Array`.


    var wheels:Vector.<Wheel> // strongly typed, but slow

    var wheels:/*Wheel*/Array // weakly typed, but faster

## Text
Rendering text consumes a lot of CPU. Fonts are rendered in a fashion similar to vector graphics and contain many vector points for every character. Altering text frame-by-frame *will* degrade performance. The "Cache as bitmap" flag is extremely useful if used correctly, meaning you must avoid:

* Altering the text frequently.
* Transforming the text field (rotating, scaling).

Simple techniques like wrapping text updates in an `if` statement will make a major difference:

    if (currentScore !== oldScore) {
        field.text = currentScore;
    }

Text can be rendered using the anti-aliased renderer built into Flash, or using "device fonts". Using "device fonts" makes text render much faster, although it makes text appear jagged (aliased). Also, device fonts requires the font to be pre-installed by your end user, or the text may "disappear" on the user's PC although it appears fine on yours.

    field.embedFonts = false; // uses "device fonts"

## Vector and for each vs arrays and for
Using the `Vector.<T>` type and the `for each` loop is more performant than a conventional array and `for` loop:

Good:

    var list:Vector.<Sprite> = new <Sprite>[];

    for each(var sprite:Sprite in list) {
        sprite.x += 1;
    }

Bad:

    var list:Array = [];
    
    for (var i:int = 0; i < list.length; i++) {
        var sprite:Sprite = list[i];

        sprite.x += 1;
    }

## Fast array item removal
If you do not require an array to be in any particular order, a little trick with `pop()` will afford you enormous performance gains compared to `splice()`.

When you `splice()` an array, the index of subsequent elements in that array needs to be reduced by 1. This process can consume a large chunk of time if the array is large and the object you are removing is nearer the beginning of that array.

If you do not care about the order of the elements in the array, you can instead replace the item you want to remove with an item that you `pop()` from the end of the array. This way, the indexes of all the other items in the array remains the same and the process does not degrade in performance as the length of your array grows.

Example:

    function slowRemove(list:Array, item:*):void {
        var index:int = list.indexOf(item);
        
        if (index >= 0) list.splice(index, 1);
    }

    function fastRemove(list:Array, item:*):void {
        var index:int = list.indexOf(item);

        if (index >= 0) {
            if (index === list.length - 1) list.pop();

            else {
                // Replace item to delete with last item.
                list[index] = list.pop();
            }
        }
    }

## Reusing and pooling graphics
Creating and configuring `Sprite` and `TextField` objects at runtime can be costly if you are creating hundreds of thousands of these on a single frame. Therefore a common trick is "pooling" these objects for later reuse. Remember we are not just trying to optimize the creation time (`new Sprite()`) but also the configuration (setting of default properties).

Lets say we were building a list component using hundreds of TextField objects. When you need to create a new object, check if an existing object can be reused. 

    var pool:Array = [];

    if (pool.length > 0){

        // reuse an existing TextField
        var label = pool.pop();

    }else{
        // create a new TextField
        label = new TextField();
        
        // initialize your TextField over here
        label.setDefaultTextFormat(...);
        label.multiline = false;
        label.selectable = false;
    }

    // add the TextField into the holder so it appears on-screen
    // you will need to layout it and set its "text" and other stuff seperately
    holder.addChild(label);

Later, when you are destroying your component (or removing it from screen), remember to add unused labels back into the pool.

    foreach (var label in allLabels){
        label.parent.removeChild(label); // remove from parent Sprite
        pool.push(label); // add to pool
    }

In most cases it is best to create a pool per usage instead of a global pool. Disadvantages to a creating a global pool is you need to re-initialize the object everytime to retrieve it from the pool, to negate the settings done by other functions. This is equally costly and pretty much negates the performance boost of using pooling in the first place.

## Vector based graphics
Vector based graphics are represented by a plethora of data that must be computed by the CPU (vector points, arcs, colors, etc). Anything other than simple shapes with minimal points and straight lines will consume vast amounts of CPU resource.

There is a "Cache as Bitmap" flag that can be turned on. This flag stores the result of drawing the vector based DisplayObject for much faster redraws. The pitfall of this is that if there are any transformations applied to the object, the entire thing needs to be redrawn and re-cached. This can be slower than not turning it on at all if there are frame-by-frame transformations applied (rotation, scaling, etc).

Generally, rendering graphics using bitmaps is far more performant than using vector graphics. Libraries such as flixel take advantage of this for rendering sprites on a "canvas" without reducing framerate.

