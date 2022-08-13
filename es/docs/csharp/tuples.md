---
title: "tuplas"
slug: "tuplas"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Acceso a elementos de tupla
Para acceder a los elementos de la tupla, utilice las propiedades `Item1`-`Item8`. Solo estarán disponibles las propiedades con un número de índice menor o igual al tamaño de la tupla (es decir, no se puede acceder a la propiedad `Item3` en `Tuple<T1,T2>`).

    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());
    var item1 = tuple.Item1; // "foo"
    var item2 = tuple.Item2; // 123
    var item3 = tuple.Item3; // true
    var item4 = tuple.Item4; // new My Class()

## Creando tuplas
Las tuplas se crean usando tipos genéricos `Tuple<T1>`-`Tuple<T1,T2,T3,T4,T5,T6,T7,T8>`. Cada uno de los tipos representa una tupla que contiene de 1 a 8 elementos. Los elementos pueden ser de diferentes tipos.

    // tuple with 4 elements
    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());

Las tuplas también se pueden crear usando métodos estáticos `Tuple.Create`. En este caso, el Compilador de C# deduce los tipos de los elementos.

    // tuple with 4 elements
    var tuple = Tuple.Create("foo", 123, true, new MyClass());
<!-- si la versión [gte 7.0] -->
Desde C# 7.0, las tuplas se pueden crear fácilmente usando [ValueTuple][1].

    var tuple = ("foo", 123, true, new MyClass());

Los elementos se pueden nombrar para una descomposición más fácil.

    (int number, bool flag, MyClass instance) tuple = (123, true, new MyClass());

<!-- versión final si -->


[1]: https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

## Comparando y ordenando Tuplas
Las tuplas se pueden comparar en función de sus elementos.

Como ejemplo, un enumerable cuyos elementos son del tipo `Tupla` se puede ordenar en función de los operadores de comparación definidos en un elemento específico:

    List<Tuple<int, string>> list = new List<Tuple<int, string>>();
    list.Add(new Tuple<int, string>(2, "foo"));
    list.Add(new Tuple<int, string>(1, "bar"));
    list.Add(new Tuple<int, string>(3, "qux"));

    list.Sort((a, b) => a.Item2.CompareTo(b.Item2)); //sort based on the string element

    foreach (var element in list) {
        Console.WriteLine(element);
    }
    
    // Output:
    // (1, bar)
    // (2, foo)
    // (3, qux)

O para invertir el uso de clasificación:

    list.Sort((a, b) => b.Item2.CompareTo(a.Item2));

## Devolver múltiples valores de un método
Las tuplas se pueden usar para devolver múltiples valores de un método sin usar parámetros. En el siguiente ejemplo, `AddMultiply` se usa para devolver dos valores (suma, producto).

    void Write()
    {
        var result = AddMultiply(25, 28);
        Console.WriteLine(result.Item1);
        Console.WriteLine(result.Item2);
    }
 
    Tuple<int, int> AddMultiply(int a, int b)
    {
        return new Tuple<int, int>(a + b, a * b);
    }

Producción:

> 53
> 700


Ahora C# 7.0 ofrece una forma alternativa de devolver múltiples valores de métodos usando tuplas de valor [Más información sobre la estructura `ValueTuple`][1].


[1]: https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

