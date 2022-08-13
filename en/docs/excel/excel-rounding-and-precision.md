---
title: "Excel rounding and precision"
slug: "excel-rounding-and-precision"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Several Excel formulas deal with rounding and precision of non-integer numbers.  This is separate from using cell formatting that affects the display of numeric data.  In some cases just using cell formatting is sufficient, but in complex calculations, strict rules for rounding and precision are required to obtain consistent and correct results.

## Syntax
 - =ROUND(number, num_digits)
 - =ROUNDUP(number, num_digits)
 - =ROUNDDOWN(number, num_digits)
 - =MROUND(number, multiple)
 - =TRUNC(number, [num_digits])
 - =INT(number)
 - =CEILING(number, significance)
 - =FLOOR(number, significance)
 - =EVEN(number)
 - =ODD(number)
 - =FIXED(number, [decimals], [no_commas])

## Parameters
| Parameters | Details |
| ------ | ------ |
| number | number to be rounded. Could be a cell like B2 or a constant like 3.14159   |
| num_digits | which place to be rounded 2. Omitted or 0 means round to a whole number. 1 or 2 means round to tenths or hundredths. -1 or -3 means round to tens or thousands. |
| multiple| The multiple to which you want to round number.   |
| significance| The multiple to which you want to round number.   |
| decimals| The number of digits to the right of the decimal point. *(Optional - defaults to 2)*   |
| no_commas|  A logical value that, if `TRUE`, prevents `FIXED` from including commas in the returned text. *(Optional - defaults to `FALSE`)*  |
| [ ... ] | Parameters in [square brackets] are optional. |


The values displayed to the user can be presented with specific formatting that does not affect the actual data values. For example, displayed data could be formatted as a percentage.  See https://www.wikiod.com/excel/cell-formatting for details. 

## Using the CEILING & FLOOR functions
The `CEILING` function rounds a number up, away from zero, to the nearest multiple of significance. The `FLOOR` function does the same by rounds the number down towards zero.  

An example of when `CEILING` could be be used is if you want to avoid using pennies in your prices and your product is priced at $4.42, use the formula `=CEILING(4.42,0.05)` to round prices up to the nearest nickel.

For example:

    =CEILING(2.2, 1)
    =FLOOR(2.2, 1)
    =CEILING(-4.8, 2)
    =FLOOR(-4.8, 2)
    =CEILING(0.456, 0.01)
    =FLOOR(0.456, 0.01)

Would return:
    
    3
    2
    -4
    -6
    0.46
    0.45

    

## Using the ROUND function
The `ROUND` function rounds a value. The number of decimal places to round to is specified by a positive value in the `num_digits` parameter. A negative value for the `num_digits` will round the integer portion of the value left of the decimal point, e.g. to the nearest 10 (for -1) or to the nearest 1000 (for -3). 

Here's a table showing how round may be used. 

| Starting with |ROUND(b,2) | ROUND(b,1) | ROUND(b) | ROUND(b,-1) |
| ----- | ----- | ----- | ----- | ----- |
| 23.10651375 | 23.11 | 23.1 | 23 | 20 |
| 19.16818924 | 19.17 | 19.2 | 19 | 20 |
| 3.92748883 | 3.93 | 3.9 | 4 | 0 |
| 31.38208409 | 31.38 | 31.4 | 31 | 30 |
| 38.34235561 | 38.34 | 38.3 | 38 | 40 |
| 7.682632495 | 7.68 | 7.7 | 8 | 10 |
| 35.39315416 | 35.39 | 35.4 | 35 | 40 |
| 20.47004449 | 20.47 | 20.5 | 20 | 20 |
| 20.49775276 | 20.5 | 20.5 | 20 | 20 |
| 2.288822497 | 2.29 | 2.3 | 2 | 0 |

Additional similar functions are also available to control the direction of rounding:

 - `ROUNDUP` - Always rounds a number up, away from zero.
 - `ROUNDDOWN` - Always rounds a number down, towards zero.

## Using the TRUNC & INT functions
The excel formula `TRUNC` is used to truncate a number to a given number of decimal places, specified by the optional `num_digits` parameter. If this parameter is defined as a negative value it will truncate the integer portion of the value. If the parameter is omitted then the default value is `0` which removes the decimal portion of the number. 

The `INT` function works in a smilar way to `TRUNC` in that it removes the decimal portion of a number by rounding it down to leave the integer portion. The difference between the two is when performing the operation on a negative number; `TRUNC` will strip the decimal, however `INT` will round the value down away from zero.

For example:

    =TRUNC(123.456,2)
    =TRUNC(123.4357,-1)
    =TRUNC(-123.123)
    =INT(567.89)
    =INT(-567.89)

Will display:

    123.45
    120.00
    -123.00
    567.00
    -568.00

## Using the MROUND function
The Excel function `MROUND` is used to round a number to an interval other than a power of 10. 

These examples show `MROUND` to the nearest quarter and to the nearest even number. 

| Starting with | MROUND(b,0.25) | MROUND(b,2) |
| ----- | ----- | ----- |
| 23.93195211 | 24.00 | 24 |
| 2.793135388 | 2.75 | 2 |
| 21.93903064 | 22.00 | 22 |
| 13.74193739 | 13.75 | 14 |
| 16.77047412 | 16.75 | 16 |
| 13.03922302 | 13.00 | 14 |
| 17.06132896 | 17.00 | 18 |
| 16.11741694 | 16.00 | 16 |
| 33.48249592 | 33.50 | 34 |
| 37.29656687 | 37.25 | 38 |

A similar result can be obtained using the `EVEN` and `ODD` functions which round a number **up** to the nearest even or odd number respectively.

## Using the FIXED Function
The `FIXED` function rounds a number to the specified number of decimals defined by the `decimals` parameter, formats the number in decimal format using a comma as a separator unless specified as not required defined by the parameter `no_commas`, and returns the result as text. The `decimals` parameter is optional and defaults to two decimal places. The `no_commas` parameter is also optional and defaults to `FALSE`.

For example:

    =FIXED(1234.567, 1)
    =FIXED(1234.567, -1)
    =FIXED(1234.567, 1, TRUE)
    =FIXED(1234.567)

Would return:

    1,234.6
    1,230
    1234.6
    1234.57

