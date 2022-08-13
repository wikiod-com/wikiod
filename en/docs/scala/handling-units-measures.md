---
title: "Handling units (measures)"
slug: "handling-units-measures"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Syntax
  - class Meter(val meters: Double) extends AnyVal
  - type Meter = Double

It is recommended to use value classes for units or a dedicated library for them.

## Type aliases
    type Meter = Double

This simple approach has serious drawbacks for unit handling as every other type that is a `Double` will be compatible with it:

    type Second = Double
    var length: Meter = 3
    val duration: Second = 1
    length = duration
    length = 0d

All of the above compiles, so in this case units can only be used for marking input/output types for the readers of the code (only the intent).

## Value classes
    case class Meter(meters: Double) extends AnyVal
    case class Gram(grams: Double) extends AnyVal

Value classes provide a type-safe way to encode units, even if they require a bit more characters to use them:

    var length = Meter(3)
    var weight = Gram(4)
    //length = weight //type mismatch; found : Gram required: Meter

By extending `AnyVal`s, there is no runtime penalty for using them, on the JVM level, those are regular primitive types (`Double`s in this case).

In case you want to automatically generate other units (like `Velocity` aka `MeterPerSecond`), this approach is not the best, though there are libraries that can be used in those cases too:

 - [Squants](http://www.squants.com/)
 - [units](https://github.com/KarolS/units)
 - [ScalaQuantity](https://github.com/zzorn/ScalaQuantity)

