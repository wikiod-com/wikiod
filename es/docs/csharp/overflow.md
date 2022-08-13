---
title: "Desbordamiento"
slug: "desbordamiento"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Desbordamiento de enteros
Hay una capacidad máxima que un entero puede almacenar. Y cuando superes ese límite, volverá al lado negativo. Para `int`, es `2147483647`

    int x = int.MaxValue;                //MaxValue is 2147483647
    x = unchecked(x + 1);                //make operation explicitly unchecked so that the example also works when the check for arithmetic overflow/underflow is enabled in the project settings 
    Console.WriteLine(x);                //Will print -2147483648
    Console.WriteLine(int.MinValue);     //Same as Min value

Para cualquier número entero fuera de este rango, use el espacio de nombres System.Numerics que tiene un tipo de datos
Entero grande. Consulte el siguiente enlace para obtener más información https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx

## Desbordamiento durante el funcionamiento
El desbordamiento también ocurre durante la operación. En el siguiente ejemplo, x es un `int`, 1 es un `int` por defecto. Por lo tanto, la suma es una suma `int`. Y el resultado será un `int`. Y se desbordará.

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1;                     //It will be overflown
    Console.WriteLine(y);               //Will print -2147483648
    Console.WriteLine(int.MinValue);    //Same as Min value

Puede evitar eso usando 1L. Ahora 1 será un "largo" y la suma será una suma "larga"

    int x = int.MaxValue;               //MaxValue is 2147483647
    long y = x + 1L;                    //It will be OK
    Console.WriteLine(y);               //Will print 2147483648


## Ordenar importa
Hay desbordamiento en el siguiente código.

    int x = int.MaxValue;
    Console.WriteLine(x + x + 1L);  //prints -1

Mientras que en el siguiente código no hay desbordamiento

    int x = int.MaxValue;
    Console.WriteLine(x + 1L + x);  //prints 4294967295

Esto se debe al orden de izquierda a derecha de las operaciones. En el primer fragmento de código `x + x` se desborda y luego se convierte en `largo`. Por otro lado, 'x + 1L' se convierte en 'largo' y luego se agrega 'x' a este valor.


