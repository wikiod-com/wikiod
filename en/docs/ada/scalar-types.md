---
title: "Scalar Types"
slug: "scalar-types"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In Ada's hierarchy of types, elementary types have sets of logically indivisible values. Among these types are the access types (pointer types) and the scalar types. The scalar types can be categorised as *enumeration*, *character*, and *numeric*. These types form the subject of this topic.
In addition to the sets of values, types have set of operations applicable to the respective scalars, such as *successor*, or `"+"`.

## Syntax
 1. **type** … **is** …

## Parameters
| Ellipsis | What |
| ------ | ------ |
| … (1)  | to receive the type's name   |
| … (2)  | to receive the type's characteristics using keywords: **delta**, **digits**, **range**  |

All scalar type definitions except enumeration and modular integers may include a **range** constraint.

A range constraint specifies a lower bound and an upper bound of the set of values to include in the type.
For fixed point types, specifying a range is mandatory: values of these types will be understood to be multiples of a small fraction of two, for example, of 1/2<sup>5</sup>. The smaller these fractions become, the more precise the representation, at the cost of range that can be represented using the bits available.

Further aspects of type definitions may be given, such as a desired `Size` in bits and other representational items. Ada 2012 adds aspects of contract based programming like `Static_Predicate`.

## Enumeration
    type Fruit is (Banana, Orange, Pear);

    Choice : Fruit := Banana;

A character type is an enumeration that includes a character literal:

    type Roman_Numeral is
        ('I', 'V', 'X', 'L', 'C', 'D', 'M', Unknown);`


## Singed Integer
    type Grade is range 0 .. 15;
    
    B   : Grade := 11;
    C   : Grade := 8;
    Avg : Grade := (B + C) / 2;  -- Avg = 9


## Modular Integer
These are the “bit fiddling” types. They have logical operators, too, such as **xor**, and they “wrap around” at the upper bound, to 0 again.

    type Bits is mod 2**24;
    
    L : Bits := 2#00001000_01010000_11001100# or 7;


## Floating Point
A floating point type is characterised by its (decimal) digits which state the minimal precision requested.

    type Distance is digits 8;
    
    Earth : Distance := 40_075.017;


## Fixed Point (Ordinary)
A fixed point type definition specifies a *delta*, and a range. Together, they describe how precisely real values should be approximated as they are represented by powers of two, not using floating point hardware.

    Shoe_Ounce : constant := 2.54 / 64.0;
    type Thickness is delta Shoe_Ounce range 0.00 .. 1.00;
    
    Strop : Thickness := 0.1;  -- could actually be 0.09375
 

## Fixed Point (Decimal)
Decimal fixed point types are typically used in accounting. They are characterised by both a *delta* and a number of decimal digits. Their arithmetical operations reflect the rules of accounting.

    type Money is delta 0.001 digits 10;
    
    Oil_Price : Money := 56.402;
    Loss      : Money := 0.002 / 3; -- is 0.000

