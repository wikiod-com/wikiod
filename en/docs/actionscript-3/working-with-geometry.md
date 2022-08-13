---
title: "Working with Geometry"
slug: "working-with-geometry"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Getting the angle between two points
Using vanilla mathematics:
    
    var from:Point = new Point(100, 50);
    var to:Point = new Point(80, 95);
    
    var angle:Number = Math.atan2(to.y - from.y, to.x - from.x);

Using a new vector representing the difference between the first two:

    var difference:Point = to.subtract(from);

    var angle:Number = Math.atan2(difference.y, difference.x);

> Note: `atan2()` returns radians, not degrees.

## Getting the distance between two points
Using vanilla mathematics:

    var from:Point = new Point(300, 10);
    var to:Point = new Point(75, 40);
    
    var a:Number = to.x - from.x;
    var b:Number = to.y - from.y;
    
    var distance:Number = Math.sqrt(a * a + b * b);

Using inbuilt functionality of [`Point`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/geom/Point.html):

    var distance:Number = to.subtract(from).length; // or
    var distance:Number = Point.distance(to, from);

## Determine if a point is inside a rectangle area
You can test whether a point is inside a rectangle using [`Rectangle.containsPoint()`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/geom/Rectangle.html#containsPoint()):

    var point:Point = new Point(5, 5);
    var rectangle:Rectangle = new Rectangle(0, 0, 10, 10);

    var contains:Boolean = rectangle.containsPoint(point); // true

## Converting radians to degrees
    var degrees:Number = radians * 180 / Math.PI;

## Converting degrees to radians
    var radians:Number = degrees / 180 * Math.PI;

## Moving a point along an angle
Assuming you have the angle you'd like to move in and an object with `x` and `y` values you want to move:

    var position:Point = new Point(10, 10);
    var angle:Number = 1.25;

You can move along the `x` axis with `Math.cos`:

    position.x += Math.cos(angle);

And the `y` axis with `Math.sin`:

    position.y += Math.sin(angle);

You can of course multiply the result of `Math.cos` and `Math.sin` by the distance you want to travel:

    var distance:int = 20;

    position.x += Math.cos(angle) * distance;
    position.y += Math.sin(angle) * distance;

> Note: The input angle must be in radians.
    

## The value of a circle in degrees and radians
* A whole circle is `360` degrees or `Math.PI * 2` radians.
* Half of those values follows to be `180` degrees or `Math.PI` radians.
* A quarter is then `90` degrees or `Math.PI / 2` radians.

To get a segment as a percentage of a whole circle in radians:

    function getSegment(percent:Number):Number {
        return Math.PI * 2 * percent;
    }

    var tenth:Number = getSegment(0.1); // One tenth of a circle in radians.

