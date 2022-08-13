---
title: "String Interpolation"
slug: "string-interpolation"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Syntax
 - $"content {expression} content"
 - $"content {expression:format} content"
 - $"content {expression} {{content in braces}} content}"
 - $"content {expression:format} {{content in braces}} content}"

String interpolation is a shorthand for the `string.Format()` method that makes it easier to build strings with variable and expression values inside of them.

    var name = "World";
    var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
    var newWay = $"Hello, {name}!";                   // returns "Hello, World"

## Format dates in strings
    var date = new DateTime(2015, 11, 11);
    var str = $"It's {date:MMMM d, yyyy}, make a wish!";
    System.Console.WriteLine(str);

You can also use the [`DateTime.ToString`][1] method to format the `DateTime` object. This will produce the same output as the code above.

    var date = new DateTime(2015, 11, 11);
    var str = date.ToString("MMMM d, yyyy");
    str = "It's " + str + ", make a wish!";
    Console.WriteLine(str);

**Output:**
>It's November 11, 2015, make a wish!

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/DpRwV5)

[Live Demo using DateTime.ToString](https://dotnetfiddle.net/YnV9J0)

> **Note:** `MM` stands for months and `mm` for minutes. Be very careful when using these as mistakes can introduce bugs that may be difficult to discover.


  [1]: https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx

## Padding the output
String can be formatted to accept a padding parameter that will specify how many character positions the inserted string will use :

    ${value, padding}

> **NOTE:** Positive padding values indicate left padding and negative
> padding values indicate right padding.

**Left Padding**
----

A left padding of 5 (adds 3 spaces before the value of number, so it takes up a total of 5 character positions in the resulting string.)

    var number = 42;
    var str = $"The answer to life, the universe and everything is {number, 5}.";
    //str is "The answer to life, the universe and everything is    42.";
    //                                                           ^^^^^
    System.Console.WriteLine(str);
    
**Output:**
       
    The answer to life, the universe and everything is    42.
[Live Demo on .NET Fiddle](https://dotnetfiddle.net/PpZXmk)

**Right Padding**
----

Right padding, which uses a negative padding value, will add spaces to the end of the current value.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, -5}.";
    //str is "The answer to life, the universe and everything is 42   .";
    //                                                           ^^^^^
    System.Console.WriteLine(str);

**Output:**

    The answer to life, the universe and everything is 42   .

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/QtKjGF)

**Padding with Format Specifiers**
----

You can also use existing formatting specifiers in conjunction with padding.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
    //str is "The answer to life, the universe and everything is 42.1 ";
    //                                                           ^^^^^

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/34ZxP0)



## Expressions
Full expressions can also be used in interpolated strings.

    var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"
    
    string world = "world";
    var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/u9lzeg)



## Simple Usage
    var name = "World";
    var str = $"Hello, {name}!";
    //str now contains: "Hello, World!";

## Behind the scenes

Internally this 

    $"Hello, {name}!" 

Will be compiled to something like this:

    string.Format("Hello, {0}!", name);

    


## Formatting numbers in strings
You can use a colon and the [standard numeric format syntax](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) to control how numbers are formatted.

    var decimalValue = 120.5;

    var asCurrency = $"It costs {decimalValue:C}";
    // String value is "It costs $120.50" (depending on your local currency settings)

    var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
    // String value is "Exactly 120.500"

    var integerValue = 57;

    var prefixedIfNecessary = $"{integerValue:D5}";
    // String value is "00057"


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/z2XbG7)

