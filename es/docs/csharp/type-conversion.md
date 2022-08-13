---
title: "Conversión de tipo"
slug: "conversion-de-tipo"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

La conversión de tipo es convertir un tipo de datos a otro tipo. También se conoce como Fundición Tipo. En C#, la conversión de tipos tiene dos formas:

**Conversión de tipos implícita**: C# realiza estas conversiones de forma segura. Por ejemplo, son conversiones de tipos integrales más pequeños a más grandes y conversiones de clases derivadas a clases base.

**Conversión de tipo explícito**: los usuarios realizan explícitamente estas conversiones mediante las funciones predefinidas. Las conversiones explícitas requieren un operador de conversión.

## Ejemplo de operador implícito de MSDN
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

**Producción:**

>Conversión implícita de dígito a doble llamada
>Conversión implícita de doble a dígito llamada
>num = 7 dig2 = 12

[Demostración en vivo en .NET Fiddle][1]


[1]: https://dotnetfiddle.net/n1AeWS

## Conversión de tipo explícito
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

