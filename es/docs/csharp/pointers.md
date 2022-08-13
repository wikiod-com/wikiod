---
title: "Punteros"
slug: "punteros"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# Punteros y `inseguro`

Debido a su naturaleza, los punteros producen código no verificable. Por lo tanto, el uso de cualquier tipo de puntero requiere un contexto "inseguro".

El tipo `System.IntPtr` es un envoltorio seguro para un `void*`. Está pensado como una alternativa más conveniente a `void*` cuando no se requiere un contexto inseguro para realizar la tarea en cuestión.

# Comportamiento indefinido

Al igual que en C y C++, el uso incorrecto de los punteros puede invocar un comportamiento indefinido, con posibles efectos secundarios como la corrupción de la memoria y la ejecución de código no deseado. Debido a la naturaleza no verificable de la mayoría de las operaciones con punteros, el uso correcto de los punteros es enteramente responsabilidad del programador.

# Tipos que admiten punteros

A diferencia de C y C++, no todos los tipos de C# tienen tipos de puntero correspondientes. Un tipo 'T' puede tener un tipo de puntero correspondiente si se aplican los dos criterios siguientes:

- `T` es un tipo de estructura o un tipo de puntero.
- `T` contiene solo miembros que satisfacen estos dos criterios recursivamente.

## Punteros para acceder a la matriz
Este ejemplo demuestra cómo se pueden usar los punteros para el acceso similar a C a las matrices de C#.

    unsafe
    {
        var buffer = new int[1024];
        fixed (int* p = &buffer[0])
        {
            for (var i = 0; i < buffer.Length; i++)
            {
                *(p + i) = i;
            }
        }
    }

Se requiere la palabra clave `unsafe` porque el acceso del puntero no emitirá ninguna verificación de límites que normalmente se emite cuando se accede a las matrices de C# de la manera habitual.

La palabra clave `fixed` le dice al compilador de C# que emita instrucciones para anclar el objeto de una manera segura para las excepciones. La fijación es necesaria para garantizar que el recolector de basura no mueva la matriz en la memoria, ya que eso invalidaría cualquier puntero que apunte dentro de la matriz.

## Aritmética de punteros
La suma y la resta en punteros funcionan de manera diferente a los números enteros. Cuando un puntero aumenta o disminuye, la dirección a la que apunta aumenta o disminuye según el tamaño del tipo de referencia.

Por ejemplo, el tipo `int` (alias de `System.Int32`) tiene un tamaño de 4. Si se puede almacenar un `int` en la dirección 0, el subsiguiente `int` se puede almacenar en la dirección 4, y así sucesivamente. . En codigo:

    var ptr = (int*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 4
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8

De manera similar, el tipo `largo` (alias de `System.Int64`) tiene un tamaño de 8. Si un `largo` se puede almacenar en la dirección 0, el subsiguiente `largo` se puede almacenar en la dirección 8, y así sucesivamente. En codigo:

    var ptr = (long*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 16

El tipo `void` es especial y los punteros `void` también son especiales y se usan como punteros generales cuando el tipo no se conoce o no importa. Debido a su naturaleza independiente del tamaño, los punteros 'vacíos' no se pueden incrementar ni disminuir:

    var ptr = (void*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));

## El asterisco es parte del tipo
En C y C++, el asterisco en la declaración de una variable apuntadora es *parte de la expresión* que se declara. En C#, el asterisco en la declaración es *parte del tipo*.

En C, C++ y C#, el siguiente fragmento declara un puntero `int`:

    int* a;

En C y C++, el siguiente fragmento declara un puntero `int` y una variable `int`. En C#, declara dos punteros `int`:

    int* a, b; 

En C y C++, el siguiente fragmento declara dos punteros `int`. En C#, no es válido:

    int *a, *b;

## vacío*
C# hereda de C y C++ el uso de `void*` como puntero independiente del tipo y del tamaño.

    void* ptr;

Cualquier tipo de puntero se puede asignar a `void*` usando una conversión implícita:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;

Lo contrario requiere una conversión explícita:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;
    int* p2 = (int*)ptr;

## Acceso de miembros usando ->
C# hereda de C y C++ el uso del símbolo `->` como medio para acceder a los miembros de una instancia a través de un puntero escrito.

Considere la siguiente estructura:

    struct Vector2
    {
        public int X;
        public int Y;
    }

Este es un ejemplo del uso de `->` para acceder a sus miembros:

    Vector2 v;
    v.X = 5;
    v.Y = 10;

    Vector2* ptr = &v;
    int x = ptr->X;
    int y = ptr->Y;
    string s = ptr->ToString();

    Console.WriteLine(x); // prints 5
    Console.WriteLine(y); // prints 10
    Console.WriteLine(s); // prints Vector2

## Indicaciones genéricas
Los criterios que debe cumplir un tipo para admitir punteros (ver *Comentarios*) no se pueden expresar en términos de restricciones genéricas. Por lo tanto, cualquier intento de declarar un puntero a un tipo proporcionado a través de un parámetro de tipo genérico fallará.

    void P<T>(T obj) 
        where T : struct
    {
        T* ptr = &obj; // compile-time error
    }


