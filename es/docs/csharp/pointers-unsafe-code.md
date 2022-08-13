---
title: "Punteros y código no seguro"
slug: "punteros-y-codigo-no-seguro"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Introducción al código no seguro
C# permite usar variables de puntero en una función de bloque de código cuando está marcada con el modificador `inseguro`. El código no seguro o el código no administrado es un bloque de código que utiliza una variable de puntero.

Un puntero es una variable cuyo valor es la dirección de otra variable, es decir, la dirección directa de la ubicación de la memoria. similar a cualquier variable o constante, debe declarar un puntero antes de poder usarlo para almacenar cualquier dirección de variable.

La forma general de una declaración de puntero es:

    type *var-name;

Las siguientes son declaraciones de puntero válidas:

    int    *ip;    /* pointer to an integer */
    double *dp;    /* pointer to a double */
    float  *fp;    /* pointer to a float */
    char   *ch     /* pointer to a character */
El siguiente ejemplo ilustra el uso de punteros en C#, usando el modificador inseguro:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          static unsafe void Main(string[] args)
          {
             int var = 20;
             int* p = &var;
             Console.WriteLine("Data is: {0} ",  var);
             Console.WriteLine("Address is: {0}",  (int)p);
             Console.ReadKey();
          }
       }
    }
Cuando el código anterior fue compilado y ejecutado, produce el siguiente resultado:

    Data is: 20
    Address is: 99215364

En lugar de declarar un método completo como no seguro, también puede declarar una parte del código como no segura:

    // safe code
    unsafe
    {
        // you can use pointers here
    }
    // safe code

## Acceder a los elementos de la matriz usando un puntero
En C#, un nombre de matriz y un puntero a un tipo de datos igual que los datos de la matriz no son el mismo tipo de variable. Por ejemplo, `int *p` y `int[] p`, no son del mismo tipo. Puede incrementar la variable de puntero `p` porque no está fijada en la memoria, pero una dirección de matriz está fijada en la memoria y no puede incrementar eso.

Por lo tanto, si necesita acceder a una matriz de datos usando una variable de puntero, como lo hacemos tradicionalmente en C o C++, necesita arreglar el puntero usando la palabra clave fixed.

El siguiente ejemplo demuestra esto:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe static void Main()
          {
             int[]  list = {10, 100, 200};
             fixed(int *ptr = list)
             
             /* let us have array address in pointer */
             for ( int i = 0; i < 3; i++)
             {
                Console.WriteLine("Address of list[{0}]={1}",i,(int)(ptr + i));
                Console.WriteLine("Value of list[{0}]={1}", i, *(ptr + i));
             }
             
             Console.ReadKey();
          }
       }
    }

Cuando el código anterior fue compilado y ejecutado, produce el siguiente resultado:

    Address of list[0] = 31627168
    Value of list[0] = 10
    Address of list[1] = 31627172
    Value of list[1] = 100
    Address of list[2] = 31627176
    Value of list[2] = 200

## Compilar código no seguro
Para compilar código no seguro, debe especificar el modificador de línea de comandos `/unsafe` con el compilador de línea de comandos.

Por ejemplo, para compilar un programa llamado prog1.cs que contiene código no seguro, desde la línea de comando, dé el comando:

    csc /unsafe prog1.cs

Si está utilizando Visual Studio IDE, debe habilitar el uso de código no seguro en las propiedades del proyecto.

[![ingrese la descripción de la imagen aquí][1]][1]

Para hacer esto:

- Abra las propiedades del proyecto haciendo doble clic en el nodo de propiedades en el
Explorador de la solución.
- Haz clic en la pestaña Construir.
- Seleccione la opción "Permitir
código inseguro"


[1]: https://i.stack.imgur.com/2aPFY.png

## Recuperar el valor de los datos usando un puntero
Puede recuperar los datos almacenados en la ubicación a la que hace referencia la variable de puntero, utilizando el método ToString(). El siguiente ejemplo demuestra esto:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          public static void Main()
          {
             unsafe
             {
                int var = 20;
                int* p = &var;
                Console.WriteLine("Data is: {0} " , var);
                Console.WriteLine("Data is: {0} " , p->ToString());
                Console.WriteLine("Address is: {0} " , (int)p);
             }
             
             Console.ReadKey();
          }
       }
    }
Cuando el código anterior fue compilado y ejecutado, produce el siguiente resultado:

    Data is: 20
    Data is: 20
    Address is: 77128984

## Pasando punteros como parámetros a métodos
Puede pasar una variable de puntero a un método como parámetro. El siguiente ejemplo lo ilustra:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe void swap(int* p, int *q)
          {
             int temp = *p;
             *p = *q;
             *q = temp;
          }
          
          public unsafe static void Main()
          {
             TestPointer p = new TestPointer();
             int var1 = 10;
             int var2 = 20;
             int* x = &var1;
             int* y = &var2;
             
             Console.WriteLine("Before Swap: var1:{0}, var2: {1}", var1, var2);
             p.swap(x, y);
    
             Console.WriteLine("After Swap: var1:{0}, var2: {1}", var1, var2);
             Console.ReadKey();
          }
       }
    }

Cuando el código anterior se compila y ejecuta, produce el siguiente resultado:

    Before Swap: var1: 10, var2: 20
    After Swap: var1: 20, var2: 10

