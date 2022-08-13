---
title: "Working with numbers"
slug: "working-with-numbers"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Progress ABL supports three number formats: 32 and 64 bit integers and floats.

## Operators
Progress supports + / - * as operators. They cannot be overloaded. Division always returns a decimal. If any of the numbers in a calculation is a decimal a decimal will be returned. Otherwise an `INTEGER` or `INT64`.

There's no `+=` or `++` operator. To increase or decrease a variable you have to assign it to itself plus or minus something. So to add 1 to a variable you do: `i = i + 1.`

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE j AS INTEGER     NO-UNDO.
    
    i = 3.
    j = 2.
    
    DISPLAY i + j. // 3 + 2 = 5
    
    DISPLAY i - j. // 3 - 2 = 1
    
    DISPLAY i / j. // 3 / 2 = 1.5
    
    DISPLAY INTEGER(i / j). //Integer(3/2) = 2.
    
    DISPLAY i * j. //3 x 2 = 6

## More mathematical functions
**EXP** - Returns the result of raising a number to a power.

> EXP( base, exponent)

    MESSAGE EXP(10, 2) VIEW-AS ALERT-BOX. // Messages 100

**SQRT** - Returns the square root of a number.

> SQRT( number)

    MESSAGE "The square root of 256 is " SQRT(256) VIEW-AS ALERT-BOX. // Messages 16

**MODULO** - Determines the remainder after division.

> expression MODULO base

    DISPLAY 52 MODULO 12. //Displays 4

**ROUND** - Rounds a decimal expression to a specified number of places after the decimal point.

> ROUND( number, precision)

    DISPLAY ROUND(67.12345, 6) FORMAT "99.99999". // 67.12345
    DISPLAY ROUND(67.12345, 5) FORMAT "99.99999". // 67.12345
    DISPLAY ROUND(67.12345, 4) FORMAT "99.99999". // 67.12350
    DISPLAY ROUND(67.12345, 3) FORMAT "99.99999". // 67.12300
    DISPLAY ROUND(67.12345, 2) FORMAT "99.99999". // 67.12000
    DISPLAY ROUND(67.12345, 1) FORMAT "99.99999". // 67.10000
    DISPLAY ROUND(67.12345, 0) FORMAT "99.99999". // 67.00000

**TRUNCATE** Truncates a decimal expression to a specified number of decimal places, returning a decimal value.

> TRUNCATE( number, places)

    DISPLAY TRUNCATE(67.12345, 6) FORMAT "99.99999". // 67.12345
    DISPLAY TRUNCATE(67.12345, 5) FORMAT "99.99999". // 67.12345
    DISPLAY TRUNCATE(67.12345, 4) FORMAT "99.99999". // 67.12340
    DISPLAY TRUNCATE(67.12345, 3) FORMAT "99.99999". // 67.12300
    DISPLAY TRUNCATE(67.12345, 2) FORMAT "99.99999". // 67.12000
    DISPLAY TRUNCATE(67.12345, 1) FORMAT "99.99999". // 67.10000
    DISPLAY TRUNCATE(67.12345, 0) FORMAT "99.99999". // 67.00000

**ABSOLUTE** - Returns the absolute value of a number

    DISPLAY ABS(10 - 12). //Displays 2
    DISPLAY ABS(-2) = ABS(2). //Displays yes

**MINIMUM** and **MAXIMUM** - returns the smalles and largest number

> MINIMUM(number1, number2, ... numbern)
>
> MAXIMUM(number1, number2, ... numbern)

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE j AS INTEGER     NO-UNDO.
    DEFINE VARIABLE k AS INTEGER     NO-UNDO.
    
    i = 40.
    j = 45.
    k = 56.
    
    DISPLAY MINIMUM(i, j, k) MAXIMUM(i, j, k). // Displays 40 and 56




## Comparing numbers

There are standard functions built in for comparing equality, inequality etc.

| Name                 | Symbol| Alternative | Example       |
| -------------------- |-------|-------------|---------------|
| Equal                | =     | EQ          | i = j         | 
| Not equal            | <>    | NE          | i <> j        | 
| Less than            | &lt;  | LT          | i &lt; j      | 
| less than or equal   | &lt;= | LE          | i &lt;= j     | 
| Greater than         | &gt;= | GT          | i &gt; j      |
| Greater than or equal| &ge;= | GE          | i &gt;= j     | 

The symbol can be exchanged with the alternative and vice versa. So `var1 <> var2` is the same thing as `var1 NE var2`.

You can compare a float with an integer but you cannot compare for instance a date with an integer.

## Random number generator
**RANDOM** - generates a random number

> RANDOM(low, high)
>
> Generates a pseudo random integer between low and high

    // Example that generates 20 random numbers between 1 and 20 (1 and 20 included)
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    DO i = 1 TO 20.
        DISPLAY i RANDOM(1, 20).
        PAUSE.
    END.







