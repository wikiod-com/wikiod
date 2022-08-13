---
title: "Numeric Types"
slug: "numeric-types"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
- Real numbers must begin with one or more digits followed by a period followed by one or more digits. 
- **~** is the operator to denote negative numbers
- **div** is the operator for integer division.
- **/** is the operator for real division.

## Integer
**Integer Basics**

    Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
    - 6;
    val it = 6 : int
    - ~6;
    val it = ~6 : int
    - 6 + ~6;
    val it = 0 : int
**Integer Division**
 
    - 6 div 3;
    val it = 2 : int
    - 6 div 4;
    val it = 0 : int
    - 3 div 6;
    val it = 0 : int

**Integer Value Bounds**

Using [Integer Basis Library Functions][1]

    - Int.maxInt;
    val it = SOME 1073741823 : int option
    - Int.minInt;
    val it = SOME ~1073741824 : int option


  [1]: http://sml-family.org/Basis/integer.html

## Real
**Real Number Basics**

    - 6.0;
    val it = 6.0 : real
    - ~6.0;
    val it = ~6.0 : real
    - 6.0 + ~6.0;
    val it = 0.0 : real
    - 6.0 / 3.0;
    val it = 2.0 : real
    - 4.0 / 6.0;
    val it = 0.666666666667 : real

**Real Value Bounds**

Using [Real Basis Library Functions][1]

    - Real.maxFinite;
    val it = 1.79769313486E308 : real
    - Real.minPos;
    val it = 4.94065645841E~324 : real
    - Real.minNormalPos;
    val it = 2.22507385851E~308 : real

**Infinity**

    - Real.posInf;
    val it = inf : real
    - Real.negInf;
    val it = ~inf : real

  [1]: http://sml-family.org/Basis/real.html



## Coercion of Real Values to Integers
**Rounding**

Values midway between two integers go toward the nearest even value.

    - round(4.5);
    val it = 4 : int
    - round(3.5);
    val it = 4 : int

**Truncation**

    val it = 4 : int
    - trunc(4.5);
    val it = 4 : int
    - trunc(3.5);
    val it = 3 : int

**Floor and Ceiling**

    - ceil(4.5);
    val it = 5 : int
    - floor(4.5);
    val it = 4 : int


## Arithmetic Operator Error with Mixed Numeric Types
**Cannot add Integer and Real***

    - 5 + 1.0;
    stdIn:1.2-10.4 Error: operator and operand don't agree [overload conflict]
      operator domain: [+ ty] * [+ ty]
      operand:         [+ ty] * real
      in expression:
        5 + 1.0



## Coersion of Integer Value to Real
    - real(6);
    val it = 6.0 : real

