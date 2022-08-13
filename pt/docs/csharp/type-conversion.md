---
title: "Conversão de tipo"
slug: "conversao-de-tipo"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

A conversão de tipo está convertendo um tipo de dados em outro tipo. Também é conhecido como Type Casting. Em C#, a conversão de tipos tem duas formas:

**Conversão de tipo implícita** - Essas conversões são executadas pelo C# de maneira segura para o tipo. Por exemplo, são conversões de tipos integrais menores para maiores e conversões de classes derivadas para classes base.

**Conversão de tipo explícita** - Essas conversões são feitas explicitamente pelos usuários usando as funções predefinidas. As conversões explícitas exigem um operador de conversão.

## Exemplo de operador implícito do MSDN
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

**Resultado:**

>Dígito para conversão implícita dupla chamada
>conversão implícita de duplo para dígito chamada
>num = 7 dig2 = 12

[Demonstração ao vivo no .NET Fiddle][1]


[1]: https://dotnetfiddle.net/n1AeWS

## Conversão de tipo explícita
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

