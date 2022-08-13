---
title: "iteradores"
slug: "iteradores"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

Un iterador es un método, un descriptor de acceso u operador que realiza una iteración personalizada sobre una matriz o clase de colección mediante la palabra clave yield.

## Creando iteradores usando Yield
Los iteradores *producen* enumeradores. En C#, los enumeradores se producen mediante la definición de métodos, propiedades o indexadores que contienen declaraciones de "rendimiento".

La mayoría de los métodos devolverán el control a la persona que llama a través de sentencias `return` normales, que eliminan todos los estados locales de ese método. Por el contrario, los métodos que usan sentencias `yield` les permiten devolver múltiples valores a la persona que llama a pedido mientras *preservan* el estado local mientras devuelven esos valores. Estos valores devueltos constituyen una secuencia. Hay dos tipos de sentencias `yield` usadas dentro de los iteradores:

- `retorno de rendimiento`, que devuelve el control a la persona que llama pero conserva el estado. El receptor de la llamada continuará la ejecución desde esta línea cuando se le devuelva el control.

- `Ruptura de rendimiento`, que funciona de manera similar a una declaración normal de `retorno`: esto significa el final de la secuencia. Las declaraciones `return` normales en sí mismas son ilegales dentro de un bloque iterador.


Este ejemplo a continuación demuestra un método iterador que se puede usar para generar la [secuencia de Fibonacci][1]:

    IEnumerable<int> Fibonacci(int count)
    {
        int prev = 1;
        int curr = 1;
        
        for (int i = 0; i < count; i++)
        {
            yield return prev;
            int temp = prev + curr;
            prev = curr;
            curr = temp;
        }
    }

Este iterador luego se puede usar para producir un enumerador de la secuencia de Fibonacci que puede ser consumido por un método de llamada. El siguiente código demuestra cómo se pueden enumerar los primeros diez términos dentro de la secuencia de Fibonacci:

    void Main()
    {
        foreach (int term in Fibonacci(10))
        {
            Console.WriteLine(term);
        }
    }

**Producción**

    1
    1
    2
    3
    5
    8
    13
    21
    34
    55

[1]: https://en.wikipedia.org/wiki/Fibonacci_number

## Ejemplo de iterador numérico simple

Un caso de uso común para los iteradores es realizar alguna operación sobre una colección de números. El siguiente ejemplo demuestra cómo cada elemento dentro de una matriz de números puede imprimirse individualmente en la consola.

Esto es posible porque los arreglos implementan la interfaz `IEnumerable`, lo que permite a los clientes obtener un iterador para el arreglo usando el método `GetEnumerator()`. Este método devuelve un *enumerador*, que es un cursor de solo lectura y solo hacia adelante sobre cada número en la matriz.

    int[] numbers = { 1, 2, 3, 4, 5 };

    IEnumerator iterator = numbers.GetEnumerator();

    while (iterator.MoveNext())
    {
        Console.WriteLine(iterator.Current);
    }

**Producción**

    1
    2
    3
    4
    5

También es posible lograr los mismos resultados usando una declaración `foreach`:

    foreach (int number in numbers)
    {
        Console.WriteLine(number);
    }



