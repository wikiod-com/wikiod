---
title: "Type Conversion"
slug: "type-conversion"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Type conversion is converting one type of data to another type. It is also known as Type Casting. In C#, type casting has two forms:

**Implicit type conversion** - These conversions are performed by C# in a type-safe manner. For example, are conversions from smaller to larger integral types and conversions from derived classes to base classes.

**Explicit type conversion** - These conversions are done explicitly by users using the pre-defined functions. Explicit conversions require a cast operator.

## MSDN implicit operator example
    class Digit
    {
        public Digit(double d) { val = d; }
        public double val;

        // User-defined conversion from Digit to double
        public static implicit operator double(Digit d)
        {
            Console.WriteLine("Digit to double implict conversion called");
            return d.val;
        }
        //  User-defined conversion from double to Digit
        public static implicit operator Digit(double d)
        {
            Console.WriteLine("double to Digit implict conversion called");
            return new Digit(d);
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Digit dig = new Digit(7);
            //This call invokes the implicit "double" operator
            double num = dig;
            //This call invokes the implicit "Digit" operator
            Digit dig2 = 12;
            Console.WriteLine("num = {0} dig2 = {1}", num, dig2.val);
            Console.ReadLine();
        }
    }

**Output:**

>Digit to double implict conversion called  
>double to Digit implict conversion called  
>num = 7 dig2 = 12  

[Live Demo on .NET Fiddle][1]


  [1]: https://dotnetfiddle.net/n1AeWS

## Explicit Type Conversion
    using System;
    namespace TypeConversionApplication 
    {
       class ExplicitConversion 
       {
          static void Main(string[] args) 
          {
             double d = 5673.74; 
             int i;
             
             // cast double to int.
             i = (int)d;
             Console.WriteLine(i);
             Console.ReadKey();
          }
       }
    }

