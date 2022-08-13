---
title: "Entero grande"
slug: "entero-grande"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Cuándo usar ##
Los objetos `BigInteger` son, por su propia naturaleza, muy pesados ​​en RAM. En consecuencia, sólo deberían utilizarse cuando sea absolutamente necesario, es decir, para números en una escala verdaderamente astronómica.

Además de esto, todas las operaciones aritméticas en estos objetos son un orden de magnitud más lentas que sus contrapartes primitivas, este problema se complica aún más a medida que el número crece, ya que no tienen un tamaño fijo. Por lo tanto, es factible que un 'BigInteger' deshonesto provoque un bloqueo al consumir toda la RAM disponible.

## Alternativas ##

Si la velocidad es imperativa para su solución, puede ser más eficiente implementar esta funcionalidad usted mismo usando una clase que envuelva un `Byte[]` y sobrecargando los operadores necesarios usted mismo. Sin embargo, esto requiere una cantidad significativa de esfuerzo adicional.



## Calcular el primer número de Fibonacci de 1000 dígitos
Incluya `using System.Numerics` y agregue una referencia a `System.Numerics` al proyecto.

    using System;
    using System.Numerics;
    
    namespace Euler_25
    {
        class Program
        {
            static void Main(string[] args)
            {
                BigInteger l1 = 1;
                BigInteger l2 = 1;
                BigInteger current = l1 + l2;
                while (current.ToString().Length < 1000)
                {
                    l2 = l1;
                    l1 = current;
                    current = l1 + l2;
                }
                Console.WriteLine(current);
            }
        }
    }

Este algoritmo simple itera a través de los números de Fibonacci hasta que alcanza uno de al menos 1000 dígitos decimales de longitud, luego lo imprime. Este valor es significativamente mayor de lo que podría contener incluso un 'ulong'.

Teóricamente, el único límite en la clase `BigInteger` es la cantidad de RAM que puede consumir su aplicación.

Nota: `BigInteger` solo está disponible en .NET 4.0 y superior.

