---
title: "Palabra clave de rendimiento"
slug: "palabra-clave-de-rendimiento"
draft: false
images: []
weight: 8758
type: docs
toc: true
---

Cuando usa la palabra clave yield en una declaración, indica que el método, el operador o el accesor get en el que aparece es un iterador. El uso de yield para definir un iterador elimina la necesidad de una clase adicional explícita (la clase que contiene el estado de una enumeración) cuando implementa el patrón IEnumerable e IEnumerator para un tipo de colección personalizado.

## Sintaxis
- retorno de rendimiento [TIPO]
- pausa de rendimiento

Poner la palabra clave `yield` en un método con el tipo de valor devuelto `IEnumerable`, `IEnumerable<T>`, `IEnumerator` o `IEnumerator<T>` le dice al compilador que genere una implementación del tipo de valor devuelto (`IEnumerable ` o `IEnumerator`) que, cuando se repite, ejecuta el método hasta cada "rendimiento" para obtener cada resultado.

La palabra clave `yield` es útil cuando desea devolver "el siguiente" elemento de una secuencia teóricamente ilimitada, por lo que sería imposible calcular toda la secuencia de antemano, o cuando calcular la secuencia completa de valores antes de devolver llevaría a una pausa no deseada para el usuario.

`Ruptura de rendimiento` también se puede utilizar para terminar la secuencia en cualquier momento.

Como la palabra clave `yield` requiere un tipo de interfaz de iterador como tipo de devolución, como `IEnumerable<T>`, no puede usar esto en un método asíncrono ya que devuelve un objeto `Task<IEnumerable<T>>`.

**Otras lecturas**

- https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx

## Uso sencillo
La palabra clave `yield` se utiliza para definir una función que devuelve un `IEnumerable` o un `IEnumerator` (así como sus variantes genéricas derivadas) cuyos valores se generan de forma perezosa cuando el autor de la llamada itera sobre la colección devuelta. Lea más sobre el propósito en la [sección de comentarios] (https://www.wikiod.com/es/docs/c%23/61/yield-keyword#remarks).

El siguiente ejemplo tiene una declaración de retorno de rendimiento que está dentro de un bucle `for`.

    public static IEnumerable<int> Count(int start, int count)
    {
        for (int i = 0; i <= count; i++)
        {
            yield return start + i;
        }
    }

Entonces puedes llamarlo:

    foreach (int value in Count(start: 4, count: 10))
    {
        Console.WriteLine(value);
    }

**Salida de consola**

    4
    5
    6
    ...
    14


[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/qtKObr)

Cada iteración del cuerpo de la instrucción `foreach` crea una llamada a la función iteradora `Count`. Cada llamada a la función de iterador continúa con la siguiente ejecución de la sentencia `yield return`, que se produce durante la próxima iteración del bucle `for`.

## Comprobación correcta de argumentos
Un método iterador no se ejecuta hasta que se enumera el valor devuelto. Por lo tanto, es ventajoso afirmar condiciones previas fuera del iterador.

    public static IEnumerable<int> Count(int start, int count)
    {
        // The exception will throw when the method is called, not when the result is iterated
        if (count < 0)
            throw new ArgumentOutOfRangeException(nameof(count));

        return CountCore(start, count);
    }

    private static IEnumerable<int> CountCore(int start, int count)
    {
        // If the exception was thrown here it would be raised during the first MoveNext()
        // call on the IEnumerator, potentially at a point in the code far away from where
        // an incorrect value was passed.
        for (int i = 0; i < count; i++)
        {
            yield return start + i;
        }
    }

**Código del lado de la llamada (Uso):**
        
    // Get the count
    var count = Count(1,10);
    // Iterate the results
    foreach(var x in count)
    {
        Console.WriteLine(x);
    }
**Producción:**
>1
>2
>3
>4
>5
>6
>7
>8
>9
>10

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/yIYxo6)

Cuando un método usa `yield` para generar un enumerable, el compilador crea una máquina de estado que, cuando se repite, ejecutará el código hasta un `yield`. Luego devuelve el elemento producido y guarda su estado.

Esto significa que no descubrirá argumentos inválidos (pasando `null`, etc.) cuando llame al método por primera vez (porque eso crea la máquina de estado), solo cuando intente acceder al primer elemento (porque solo entonces el código dentro del método ejecutado por la máquina de estado). Al envolverlo en un método normal que primero verifica los argumentos, puede verificarlos cuando se llama al método. Este es un ejemplo de fracaso rápido.

Cuando se usa C# 7+, la función `CountCore` se puede ocultar convenientemente en la función `Count` como una _función local_. Consulte el ejemplo [aquí] (https://www.wikiod.com/es/docs/c%23/1936/c-sharp-7-0-features/6330/local-functions#t=201607251321358412005#t=201607251057101259341).

## Terminación anticipada
Puede ampliar la funcionalidad de los métodos `yield` existentes pasando uno o más valores o elementos que podrían definir una condición de finalización dentro de la función llamando a `yield break` para detener la ejecución del ciclo interno.

    public static IEnumerable<int> CountUntilAny(int start, HashSet<int> earlyTerminationSet)
    {
        int curr = start;

        while (true)
        {
            if (earlyTerminationSet.Contains(curr))
            {
                // we've hit one of the ending values
                yield break;
            }

            yield return curr;

            if (curr == Int32.MaxValue)
            {
                // don't overflow if we get all the way to the end; just stop
                yield break;
            }

            curr++;
        }
    }

El método anterior iteraría desde una posición dada de `inicio` hasta que se encontrara uno de los valores dentro de `earlyTerminationSet`.

    // Iterate from a starting point until you encounter any elements defined as 
    // terminating elements
    var terminatingElements = new HashSet<int>{ 7, 9, 11 };
    // This will iterate from 1 until one of the terminating elements is encountered (7)
    foreach(var x in CountUntilAny(1,terminatingElements))
    {
        // This will write out the results from 1 until 7 (which will trigger terminating)
        Console.WriteLine(x);
    }
**Producción:**
>1
>2
>3
>4
>5
>6

[Demostración en vivo en .NET Fiddle](https://dotnetfiddle.net/pctiOz)

## Uso más pertinente
    public IEnumerable<User> SelectUsers()
    {
        // Execute an SQL query on a database.
        using (IDataReader reader = this.Database.ExecuteReader(CommandType.Text, "SELECT Id, Name FROM Users"))
        {
            while (reader.Read())
            {
                int id = reader.GetInt32(0);
                string name = reader.GetString(1);
                yield return new User(id, name);
            }
        }
    }

Hay otras formas de obtener un `IEnumerable<User>` de una base de datos SQL, por supuesto; esto solo demuestra que puede usar `yield` para convertir cualquier cosa que tenga una semántica de "secuencia de elementos" en un `IEnumerable<T> ` que alguien puede iterar.

## Evaluación perezosa
Solo cuando la sentencia `foreach` pasa al siguiente elemento, el bloque iterador evalúa hasta la siguiente sentencia `yield`.

Considere el siguiente ejemplo:

    private IEnumerable<int> Integers()
    {
        var i = 0;
        while(true)
        {
            Console.WriteLine("Inside iterator: " + i);
            yield return i;
            i++;
        }
    }
    
    private void PrintNumbers()
    {
        var numbers = Integers().Take(3);
        Console.WriteLine("Starting iteration");

        foreach(var number in numbers)
        {
            Console.WriteLine("Inside foreach: " + number);
        }
    }


Esto generará:

>Iniciando iteración
> Iterador interior: 0
>Foreach interior: 0
> iterador interior: 1
>Foreach interior: 1
> Iterador interior: 2
>Foreach interior: 2

[Ver demostración][1]

Como consecuencia:

- "Iniciando iteración" se imprime primero a pesar de que se llamó al método iterador antes de que la línea lo imprimiera porque la línea `Integers().Take(3);` en realidad no inicia la iteración (no se llama a `IEnumerator.MoveNext()` se hizo)
- Las líneas que se imprimen en la consola se alternan entre la que está dentro del método iterador y la que está dentro de `foreach`, en lugar de que todas las que están dentro del método iterador se evalúen primero.
- Este programa finaliza debido al método `.Take()`, aunque el método iterador tiene un `while true` del que nunca sale.


[1]: https://dotnetfiddle.net/2qGV0B

## Prueba... finalmente
Si un método de iterador tiene un rendimiento dentro de `try...finally`, entonces el `IEnumerator` devuelto ejecutará la declaración `finally` cuando se llame a `Dispose`, siempre que el punto actual de evaluación esté dentro del bloque `probar`.

Dada la función:

    private IEnumerable<int> Numbers()
    {
        yield return 1;
        try
        {
            yield return 2;
            yield return 3;
        }
        finally
        {
            Console.WriteLine("Finally executed");
        }
    }

Al llamar:

    private void DisposeOutsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Luego imprime:
>1

[Ver demostración][1]

Al llamar:

    private void DisposeInsideTry()
    {
        var enumerator = Numbers().GetEnumerator();

        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.MoveNext();
        Console.WriteLine(enumerator.Current);
        enumerator.Dispose();
    }

Luego imprime:
>1
>2
>Finalmente ejecutado

[Ver demostración][2]


[1]: https://dotnetfiddle.net/MJt7dt
[2]: https://dotnetfiddle.net/HlMroV

## Uso de yield para crear un IEnumerator<T> al implementar IEnumerable<T>
La interfaz `IEnumerable<T>` tiene un único método, `GetEnumerator()`, que devuelve un `IEnumerator<T>`.

Si bien la palabra clave `yield` se puede usar para crear directamente un `IEnumerable<T>`, *también* se puede usar exactamente de la misma manera para crear un `IEnumerator<T>`. Lo único que cambia es el tipo de retorno del método.

Esto puede ser útil si queremos crear nuestra propia clase que implemente `IEnumerable<T>`:

    public class PrintingEnumerable<T> : IEnumerable<T>
    {
        private IEnumerable<T> _wrapped;
    
        public PrintingEnumerable(IEnumerable<T> wrapped)
        {
            _wrapped = wrapped;
        }
    
        // This method returns an IEnumerator<T>, rather than an IEnumerable<T>
        // But the yield syntax and usage is identical.
        public IEnumerator<T> GetEnumerator()
        {
            foreach(var item in _wrapped)
            {
                Console.WriteLine("Yielding: " + item);
                yield return item;
            }
        }
    
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }

(Tenga en cuenta que este ejemplo en particular es solo ilustrativo y podría implementarse de manera más limpia con un solo método iterador que devuelva un `IEnumerable<T>`).
    

## Evaluación ansiosa
La palabra clave `yield` permite una evaluación perezosa de la colección. La carga forzada de toda la colección en la memoria se denomina **evaluación ansiosa**.

El siguiente código muestra esto:

    IEnumerable<int> myMethod()
    {
        for(int i=0; i <= 8675309; i++)
        {
            yield return i;
        }
    }
    ...
    // define the iterator
    var it = myMethod.Take(3);
    // force its immediate evaluation
    // list will contain 0, 1, 2
    var list = it.ToList();

Llamar a `ToList`, `ToDictionary` o `ToArray` forzará la evaluación inmediata de la enumeración, recuperando todos los elementos en una colección.

## Devuelve otro Enumerable dentro de un método que devuelve Enumerable
    public IEnumerable<int> F1()
    {
        for (int i = 0; i < 3; i++)
            yield return i;
    
        //return F2(); // Compile Error!!
        foreach (var element in F2())
            yield return element;
    }
    
    public int[] F2()
    {
        return new[] { 3, 4, 5 };
    }

## Ejemplo de evaluación perezosa: números de Fibonacci
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Numerics; // also add reference to System.Numberics
    
    namespace ConsoleApplication33
    {
        class Program
        {
            private static IEnumerable<BigInteger> Fibonacci()
            {
                BigInteger prev = 0;
                BigInteger current = 1;
                while (true)
                {
                    yield return current;
                    var next = prev + current;
                    prev = current;
                    current = next;
                }
            }
    
            static void Main()
            {
                // print Fibonacci numbers from 10001 to 10010
                var numbers = Fibonacci().Skip(10000).Take(10).ToArray();
                Console.WriteLine(string.Join(Environment.NewLine, numbers));
            }
        }
    }

Cómo funciona bajo el capó (recomiendo descompilar el archivo .exe resultante en la herramienta IL Disaambler):
1. El compilador de C# genera una clase que implementa `IEnumerable<BigInteger>` y `IEnumerator<BigInteger>` (`<Fibonacci>d__0` en ildasm).
2. Esta clase implementa una máquina de estado. El estado consta de la posición actual en el método y los valores de las variables locales.
3. El código más interesante está en el método `bool IEnumerator.MoveNext()`. Básicamente, lo que hace `MoveNext()`:
- Restaura el estado actual. Variables como `prev` y `current` se convierten en campos de nuestra clase (`<current>5__2` y `<prev>5__1` en ildasm). En nuestro método tenemos dos posiciones (`<>1__state`): primero en la llave de apertura, segundo en `retorno de rendimiento`.
- Ejecuta el código hasta el siguiente `retorno de rendimiento` o `pausa de rendimiento`/`}`.
- Para 'retorno de rendimiento', el valor resultante se guarda, por lo que la propiedad 'Actual' puede devolverlo. Se devuelve `verdadero`. En este punto, el estado actual se guarda de nuevo para la próxima invocación de `MoveNext`.
- Para el método `yield break`/`}` simplemente devuelve `false`, lo que significa que la iteración está hecha.

También tenga en cuenta que el número 10001 tiene una longitud de 468 bytes. La máquina de estado solo guarda las variables `actual` y `anterior` como campos. Mientras que si quisiéramos guardar todos los números en la secuencia desde el primero hasta el 10000, el tamaño de la memoria consumida será de más de 4 megabytes. Por lo tanto, la evaluación perezosa, si se usa correctamente, puede reducir la huella de memoria en algunos casos.

## La diferencia entre descanso y descanso de rendimiento
El uso de "interrupción de rendimiento" en lugar de "interrupción" podría no ser tan obvio como podría pensarse. Hay muchos malos ejemplos en Internet donde el uso de los dos es intercambiable y realmente no demuestra la diferencia.

La parte confusa es que ambas palabras clave (o frases clave) solo tienen sentido dentro de los bucles (`foreach`, `while`...) Entonces, ¿cuándo elegir una sobre la otra?

Es importante darse cuenta de que una vez que usa la palabra clave [`yield`](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx) en un método, convierte efectivamente el método en un [iterador]( https://msdn.microsoft.com/en-us/library/mt639331.aspx). El único propósito de dicho método es iterar sobre una colección finita o infinita y producir (salir) sus elementos. Una vez que se cumple el propósito, no hay razón para continuar con la ejecución del método. A veces, sucede naturalmente con el último paréntesis de cierre del método `}`. Pero a veces, desea finalizar el método prematuramente. En un método normal (sin iteración), usaría la palabra clave [`return`](https://msdn.microsoft.com/en-us/library/1h3swy84.aspx). Pero no puedes usar `return` en un iterador, tienes que usar `yield break`. En otras palabras, `retorno de rendimiento` para un iterador es lo mismo que `retorno` para un método estándar. Mientras que la instrucción [`break`](https://msdn.microsoft.com/en-us/library/adbctcz4.aspx) solo finaliza el bucle más cercano.

Veamos algunos ejemplos:

```
    /// <summary>
    /// Yields numbers from 0 to 9
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9}</returns>
    public static IEnumerable<int> YieldBreak()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Indicates that the iteration has ended, everything 
                // from this line on will be ignored
                yield break;
            }
        }
        yield return 10; // This will never get executed
    }
```

```
    /// <summary>
    /// Yields numbers from 0 to 10
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9,10}</returns>
    public static IEnumerable<int> Break()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Terminates just the loop
                break;
            }
        }
        // Execution continues
        yield return 10;
    }
```

