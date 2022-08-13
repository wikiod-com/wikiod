---
title: "Conversion de types"
slug: "conversion-de-types"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

La conversion de type consiste à convertir un type de données en un autre type. Il est également connu sous le nom de moulage de type. En C#, la conversion de type a deux formes :

**Conversion de type implicite** - Ces conversions sont effectuées par C# de manière sécurisée. Par exemple, il s'agit des conversions de types intégraux plus petits vers des types intégraux plus grands et des conversions de classes dérivées vers des classes de base.

**Conversion de type explicite** - Ces conversions sont effectuées explicitement par les utilisateurs à l'aide des fonctions prédéfinies. Les conversions explicites nécessitent un opérateur cast.

## Exemple d'opérateur implicite MSDN
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

**Production:**

>Conversion implicite de chiffre en double appelée
> conversion implicite double à chiffres appelée
>num = 7 chiffre2 = 12

[Démo en direct sur .NET Fiddle][1]


[1] : https://dotnetfiddle.net/n1AeWS

## Conversion de type explicite
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

