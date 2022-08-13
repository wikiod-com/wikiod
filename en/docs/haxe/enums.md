---
title: "Enums"
slug: "enums"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Syntax
 - enum *identifier* { *constructors* }

## Capturing enum values
Values passed as enum constructor arguments can be captured into variables by use of **[pattern matching][1]**.

Assume the following enum:

    enum Color {
        RGB(r : Int, g : Int, b : Int);
        HSV(h : Int, s : Float, v : Float);
    }

The red channel value can be captured as follows:

    var color = Color.RGB(255, 127, 0);
    var red = switch (color) {
        // Match the Color.RGB constructor and capture value into `r`
        case Color.RGB(r, _, _):
            // Return the captured red value
            r;
        // Catch-all for matching remaining constructors
        case _:
            // Return -1
            -1;
    }

Try the example on [try.haxe.org][2].

## References

 - ["Pattern matching", Haxe manual][1]
 - ["Variable capture", Haxe manual][3]


  [1]: https://haxe.org/manual/lf-pattern-matching.html
  [2]: http://try.haxe.org/#5E213
  [3]: https://haxe.org/manual/lf-pattern-matching-variable-capture.html

## Matching enum constructors
Enum constructors can be matched using **[pattern matching][1]**.

Assume the following enum:

    enum Color {
        Red;
        Green;
        Blue;
        RGB(r : Int, g : Int, b : Int);
    }

Colours with only a green channel value can be matched as follows:

    var color = Color.RGB(0, 127, 0);
    var isGreenOnly = switch (color) {
        // Match Green or RGB with red and blue values at 0
        case Color.RGB(0, _, 0) | Color.Green: true;
        case _: false;
    }

Try the example on [try.haxe.org][2].

## References

 - ["Pattern matching", Haxe manual][1]
 - ["Enum matching", Haxe manual][3]
 - ["Or patterns", Haxe manual][4]


  [1]: https://haxe.org/manual/lf-pattern-matching.html
  [2]: http://try.haxe.org/#ce180
  [3]: https://haxe.org/manual/lf-pattern-matching-enums.html
  [4]: https://haxe.org/manual/lf-pattern-matching-or.html

## Overview
Haxe's enumeration types are [**algebraic data types**][1] (ADT). Their primary use is for describing data structures. [Enums][2] are denoted by the `enum` keyword and contain one or more **enum constructors**.

    enum Color {
        Red;
        Green;
        Blue;
        RGB(r : Int, g : Int, b : Int);
    }

The above enum can be instantiated as follows:

    var c1 = Color.Red;
    var c2 = Color.RGB(255, 0, 0);

Try the example on [try.haxe.org][3].

## References

 - ["Enum instance", Haxe manual][2]


  [1]: https://en.wikipedia.org/wiki/Algebraic_data_type
  [2]: http://%20http://haxe.org/manual/types-enum-instance.html
  [3]: http://try.haxe.org/#85820

