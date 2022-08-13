---
title: "Tür Dönüştürme"
slug: "tur-donusturme"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Tür dönüştürme, bir veri türünü başka bir türe dönüştürmektir. Tip Döküm olarak da bilinir. C#'ta tip dökümünün iki biçimi vardır:

**Örtülü tür dönüştürme** - Bu dönüştürmeler, C# tarafından tür açısından güvenli bir şekilde gerçekleştirilir. Örneğin, küçükten büyüğe integral türlerine dönüştürmeler ve türetilmiş sınıflardan temel sınıflara dönüştürmeler.

**Açık tür dönüştürme** - Bu dönüştürmeler, önceden tanımlanmış işlevleri kullanan kullanıcılar tarafından açıkça yapılır. Açık dönüştürmeler, bir yayın operatörü gerektirir.

## MSDN örtük operatör örneği
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

**Çıktı:**

> Rakamdan çifte örtük dönüştürme denir
> çift basamaklı örtük dönüştürme denir
>sayı = 7 dig2 = 12

[.NET Fiddle'da Canlı Demo][1]


[1]: https://dotnetfiddle.net/n1AeWS

## Açık Tür Dönüşümü
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

