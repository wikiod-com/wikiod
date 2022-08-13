---
title: "Working with numerical values"
slug: "working-with-numerical-values"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Whether a number is an even value
    function isEven(n:Number):Boolean {
        return ((n & 1) == 0);
    }

Examples:

    isEven(1); // false
    isEven(2); // true

    isEven(1.1); // false
    isEven(1.2); // false
    isEven(2.1); // true
    isEven(2.2); // true

## Whether a number is an odd value
    function isOdd(n:Number):Boolean {
        return ((n & 1) == 1);
    }

Examples:

    isOdd(1); // true
    isOdd(2); // false
    
    isOdd(1.1); // true
    isOdd(1.2); // true
    isOdd(2.1); // false
    isOdd(2.2); // false

## Rounding to nearest X
To round a value to the nearest multiple of x:

    function roundTo(value:Number, to:Number):Number {
        return Math.round(value / to) * to;
    }

Example:

    roundTo(8, 5); // 10
    roundTo(17, 3); // 18

## Roundoff errors of floating point numbers
    /**
     * @param n Number to be rounded.
     * @param precision Decimal places.
     * @return Rounded Number
     */
    function roundDecimal(n:Number, precision:Number):Number {
        var factor:int = Math.pow(10, precision);
        return (Math.round(n * factor) / factor);
    }

Examples:

    trace(0.9 - 1); // -0.09999999999999998

    trace(roundDecimal(0.9 - 1, 1));     // -0.1
    trace(roundDecimal(0.9 - 1, 2));     // -0.1

    trace(roundDecimal(0.9 - 1.123, 1)); // -0.2
    trace(roundDecimal(0.9 - 1.123, 2)); // -0.22
    trace(roundDecimal(0.9 - 1.123, 3)); // -0.223

## Displaying numbers with required precision
    var a:Number=0.123456789;
    trace(a);                    // 0.123456789
    trace(a.toPrecision(4));     // 0.1235
    trace(a.toFixed(4));         // 0.1235
    trace(a.toExponential(4));   // 1.2345e-1
    trace(a.toString(16));       // 0 - works for integer part only
    var b:Number=12345678.9876543; // a bigger number to display rounding
    trace(b);                    // 12345678.9876543
    trace(b.toPrecision(4));     // 1.235e+7
    trace(b.toFixed(4));         // 12345678.9877
    trace(b.toExponential(4));   // 1.2345e+7
    trace(b.toString(16));       // bc614e
    b=1.0e+16;
    trace(b.toString(36));       // 2qgpckvng1s


