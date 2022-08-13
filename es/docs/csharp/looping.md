---
title: "Bucle"
slug: "bucle"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

## Bucle Foreach
foreach iterará sobre cualquier objeto de una clase que implemente `IEnumerable` (tenga en cuenta que `IEnumerable<T>` se hereda de él). Dichos objetos incluyen algunos incorporados, pero no se limitan a: `List<T>`, `T[]` (matrices de cualquier tipo), `Dictionary<TKey, TSource>`, así como interfaces como `IQueryable` y `IColección`, etc.

**sintaxis**

    foreach(ItemType itemVariable in enumerableObject)
        statement;

**comentarios**

1. El tipo `ItemType` no necesita coincidir con el tipo preciso de los elementos, solo debe ser asignable desde el tipo de los elementos
2. En lugar de `ItemType`, alternativamente se puede usar `var`, que inferirá el tipo de elementos del enumerableObject al inspeccionar el argumento genérico de la implementación `IEnumerable`
3. La declaración puede ser un bloque, una sola declaración o incluso una declaración vacía (`;`)
4. Si `enumerableObject` no está implementando `IEnumerable`, el código no se compilará
5. Durante cada iteración, el elemento actual se convierte en `ItemType` (incluso si no se especifica pero el compilador lo infiere a través de `var`) y si el elemento no se puede convertir, se lanzará una `InvalidCastException`.

Considere este ejemplo:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    foreach(var name in list)
    {
        Console.WriteLine("Hello " + name);
    }

es equivalente a:

    var list = new List<string>();
    list.Add("Ion");
    list.Add("Andrei");
    IEnumerator enumerator;
    try
    {
        enumerator = list.GetEnumerator();
        while(enumerator.MoveNext())
        {
            string name = (string)enumerator.Current;
            Console.WriteLine("Hello " + name);
        }
    }
    finally
    {
        if (enumerator != null)
            enumerator.Dispose();
    }

## En bucle
Un For Loop es excelente para hacer cosas durante un cierto período de tiempo. Es como un ciclo while pero el incremento se incluye con la condición.

Un For Loop se configura así:

    for (Initialization; Condition; Increment)
    {
        // Code
    }

> Inicialización: crea una nueva variable local que solo se puede usar en el ciclo.
> Condición: el ciclo solo se ejecuta cuando la condición es verdadera.
> Incremento: cómo cambia la variable cada vez que se ejecuta el bucle.

Un ejemplo:

    for (int i = 0; i < 5; i++)
    {
        Console.WriteLine(i);
    }

Producción:

> 0
> 1
> 2
> 3
> 4

También puede omitir espacios en el For Loop, pero debe tener todos los puntos y comas para que funcione.

    int input = Console.ReadLine();    

    for ( ; input < 10; input + 2)
    {
        Console.WriteLine(input);
    }

Salida para 3:
>3
>5
>7
>9
>11

## Do - Bucle Mientras
Es similar a un bucle `while`, excepto que prueba la condición al *final* del cuerpo del bucle. El bucle Do - While ejecuta el bucle una vez independientemente de si la condición es verdadera o no.

    int[] numbers = new int[] { 6, 7, 8, 10 };
        
    // Sum values from the array until we get a total that's greater than 10,
    // or until we run out of values.
    int sum = 0;
    int i = 0;
    do
    {
        sum += numbers[i];
        i++;
    } while (sum <= 10 && i < numbers.Length);
        
    System.Console.WriteLine(sum); // 13


## Estilos de bucle
**Tiempo**

El tipo de bucle más trivial. El único inconveniente es que no hay una pista intrínseca para saber dónde se encuentra en el circuito.

    /// loop while the condition satisfies
    while(condition)
    {
        /// do something
    }

**Hacer**

Similar a `while`, pero la condición se evalúa al final del bucle en lugar de al principio. Esto da como resultado la ejecución de los bucles al menos una vez.

    do
    {
        /// do something
    } while(condition) /// loop while the condition satisfies


**Para**

Otro estilo de bucle trivial. Mientras se realiza un bucle, un índice (`i`) aumenta y puede usarlo. Por lo general, se usa para manejar matrices.

    for ( int i = 0; i < array.Count; i++ )
    {
        var currentItem = array[i];
        /// do something with "currentItem"
    }

**Para cada**

Forma modernizada de recorrer objetos 'IEnumerable'. Menos mal que no tiene que pensar en el índice del elemento o el número de elementos de la lista.

    foreach ( var item in someList )
    {
        /// do something with "item"
    }

**Método Foreach**

Mientras que los otros estilos se utilizan para seleccionar o actualizar los elementos de las colecciones, este estilo se suele utilizar para *llamar a un método* inmediatamente para todos los elementos de una colección.

    list.ForEach(item => item.DoSomething());

    // or
    list.ForEach(item => DoSomething(item));

    // or using a method group
    list.ForEach(Console.WriteLine);

    // using an array
    Array.ForEach(myArray, Console.WriteLine);

Es importante tener en cuenta que este método solo está disponible en instancias de `List<T>` y como método estático en `Array`; **no** forma parte de Linq.

**Linq Paralelo Foreach**

Al igual que Linq Foreach, excepto que este hace el trabajo de manera paralela. Lo que significa que todos los elementos de la colección ejecutarán la acción dada al mismo tiempo, simultáneamente.

    collection.AsParallel().ForAll(item => item.DoSomething());

    /// or
    collection.AsParallel().ForAll(item => DoSomething(item));

## Bucles anidados
    // Print the multiplication table up to 5s
    for (int i = 1; i <= 5; i++)
    {
        for (int j = 1; j <= 5; j++)
        {
            int product = i * j;
            Console.WriteLine("{0} times {1} is {2}", i, j, product);
        }
    }

## descanso
A veces, la condición del bucle debe verificarse en el medio del bucle. Podría decirse que el primero es más elegante que el segundo:

    for (;;)
    {
        // precondition code that can change the value of should_end_loop expression
    
        if (should_end_loop)
            break;
    
        // do something
    }

Alternativa:

    bool endLoop = false;
    for (; !endLoop;)
    {
        // precondition code that can set endLoop flag
    
        if (!endLoop)
        {
            // do something
        }
    }

Nota: En bucles anidados y/o `switch` se debe usar más que un simple `break`.

## Bucle mientras
    int n = 0;
    while (n < 5) 
    {
        Console.WriteLine(n);
        n++;
    }

Producción:

> 0
> 1
> 2
> 3
> 4

IEnumerators se puede iterar con un bucle while:

    // Call a custom method that takes a count, and returns an IEnumerator for a list
    // of strings with the names of theh largest city metro areas.
    IEnumerator<string> largestMetroAreas = GetLargestMetroAreas(4);

    while (largestMetroAreas.MoveNext())
    {
        Console.WriteLine(largestMetroAreas.Current);
    }

Salida de muestra:

> Tokio/Yokohama
> Metro de Nueva York
> São Paulo
> Seúl/Incheon

## Seguir
Además de `romper`, también existe la palabra clave `continuar`. En lugar de romper completamente el ciclo, simplemente omitirá la iteración actual. Podría ser útil si no desea que se ejecute algún código si se establece un valor particular.

Aquí hay un ejemplo simple:

    for (int i = 1; i <= 10; i++)
    {
        if (i < 9)
            continue;

        Console.WriteLine(i);
    }

Resultará en:

    9
    10

**Nota:** `Continue` suele ser más útil en bucles while o do-while. Los bucles for, con condiciones de salida bien definidas, pueden no beneficiarse tanto.

