---
title: "Random Value Generation"
slug: "random-value-generation"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Random number between min and max values
    function randomMinMax(min:Number, max:Number):Number {
        return (min + (Math.random() * Math.abs(max - min)));
    }

This function is called by passing a range of minimum and maximum values.

Example:

    randomMinMax(1, 10);

Example outputs:
- 1.661770915146917
- 2.5521070677787066
- 9.436270965728909

## Create a random color
To get *any* random color:

    function randomColor():uint
    {
        return Math.random() * 0xFFFFFF;
    }

If you need more control over the red, green and blue channels:

    var r:uint = Math.random() * 0xFF;
    var g:uint = Math.random() * 0xFF;
    var b:uint = Math.random() * 0xFF;

    var color:uint = r << 16 | g << 8 | b;

Here you can specify your own range for `r`, `g` and `b` (this example is from 0-255).

## Random number between 0 and 1
    Math.random();

produces an evenly distributed random number between 0 (inclusive) and 1 (exclusive)

Example output:
- 0.22282187035307288
- 0.3948539895936847
- 0.9987191134132445


## Random value from an array
Assuming we have an array `myArray`:

    var value:* = myArray[int(Math.random() * myArray.length)];

Note we use `int` to cast the result of `Math.random()` to an int because values like `2.4539543` would not be a valid array index.

## Random angle, in degrees
    function randomAngle():Number {
        return (Math.random() * 360);
    }

Example outputs:
- 31.554428357630968
- 230.4078639484942
- 312.7964010089636

## Determining the success of a "percent chance" operation
If you need to roll for a `true` or `false` in an "x% chance" situation, use:

    function roll(chance:Number):Boolean {
        return Math.random() >= chance;
    }

Used like:

    var success:Boolean = roll(0.5); // True 50% of the time.
    var again:Boolean = roll(0.25); // True 25% of the time.

## Randomly loop through alphabet
    var alphabet:Vector.<String> = new <String>[ "A", "B", "C", "D", "E", "F", "G",
                                                 "H", "I", "J", "K", "L", "M", "N",
                                                 "O", "P", "Q", "R", "S", "T", "U",
                                                 "V", "W", "X", "Y", "Z" ];
    
    while (alphabet.length > 0)
    {
        var letter:String = alphabet.splice(int(Math.random() *
                                                alphabet.length), 1)[0];
        trace(letter);
    }

Example output:

> V, M, F, E, D, U, S, L, X, K, Q, H, A, I, W, N, P, Y, J, C, T, O, R, G, B, Z

## Random point inside a circle
First define the circle radius and its center:

    var radius:Number = 100;
    var center:Point = new Point(35, 70);

Then generate a random angle in *radians* from the center:

    var angle:Number = Math.random() * Math.PI * 2;

Then generate an effective radius of the returned point, so it'll be inside given `radius`. A simple `Math.random()*radius` won't do, because with this distribution the produces points will end up in the inner circle of half radius half of the time, but the square of that circle is a quarter of original. To create a proper distribution, the function should be like this:

    var rad:Number=(Math.random()+Math.random())*radius; // yes, two separate calls to random
    if (rad>radius) { rad=2*radius-rad; }

This function produces a value that has its probability function linearly increasing from 0 at zero to maximum at `radius`. It happens because a sum of random values has a [probability density function][1] equal to [convolution][2] of all the random values' individual density functions. This is some extended maths for an average grade person, but a kind GIF is presented to draw a graph of convolution function of two uniformed distribution density functions explained as "[box signals][3]". The `if` operator folds the resultant function over its maximum, leaving only a sawtooth-shaped graph.

This function is selected because the square of a circle strip located between `radius=r` and `radius=r+dr` increases linearly with increasing `r` and very small constant `dr` so that `dr*dr<<r`. Therefore, the amount of points generated close at the center is smaller than the amount of points generated at the edge of the circle by the same margin as the radius of center area is smaller than the radius of the whole circle. So overall, points are evenly distributed across the entire circle.

Now, get your random position:

    var result:Point = new Point(
        center.x + Math.cos(angle) * rad,
        center.y + Math.sin(angle) * rad
    );

To get a random point ON the circle (on the edge of the circle of a given radius), use `radius` instead of `rad`.

PS: The example ended up being overloaded by explanation of maths.


  [1]: https://en.wikipedia.org/wiki/Probability_density_function
  [2]: https://en.wikipedia.org/wiki/Convolution
  [3]: https://en.wikipedia.org/wiki/File:Convolution_of_box_signal_with_itself2.gif

## Random angle, in radians

    function randomAngleRadians():Number
    {
        return Math.random() * Math.PI * 2;
    }

Example outputs:

* 5.490068569213088
* 3.1984284719180205
* 4.581117863808207

## Randomize An Array
    var alphabet:Array = [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ];
    
    for (var i:int=alphabet.length-1;i>0;i--) {
        var j:int=Math.floor(Math.random()*(i+1));
        var swap=alphabet[j];
        alphabet[j]=alphabet[i];
        alphabet[i]=swap;
    }
    trace(alphabet);    

Example output

> B,Z,D,R,U,N,O,M,I,L,C,J,P,H,W,S,Q,E,K,T,F,V,X,Y,G,A

This method is known as [Fisher-Yates array shuffle][1].


  [1]: https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle

