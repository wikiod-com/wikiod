---
title: "LINQ paralelo (PLINQ)"
slug: "linq-paralelo-plinq"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Sintaxis
- ParallelEnumerable.Agregate(función)
- ParallelEnumerable.Agregate(seed, func)
- ParallelEnumerable.Agregate(seed, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.Agregate(seedFactory, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.All(predicado)
- ParallelEnumerable.Any()
- ParallelEnumerable.Any(predicado)
- ParallelEnumerable.AsEnumerable()
- ParallelEnumerable.AsOrdered()
- ParallelEnumerable.AsParallel()
- ParallelEnumerable.AsSequential()
- ParallelEnumerable.AsUnordered()
- ParallelEnumerable.Average(selector)
- ParallelEnumerable.Cast()
- ParallelEnumerable.Concat(segundo)
- ParallelEnumerable.Contains(valor)
- ParallelEnumerable.Contains(valor, comparar)
- ParallelEnumerable.Count()
- ParallelEnumerable.Count(predicado)
- ParallelEnumerable.DefaultIfEmpty()
- ParallelEnumerable.DefaultIfEmpty(valor predeterminado)
- ParallelEnumerable.Distinct()
- ParallelEnumerable.Distinct(comparar)
- ParallelEnumerable.ElementAt(índice)
- ParallelEnumerable.ElementAtOrDefault(índice)
- ParallelEnumerable.Empty()
- ParallelEnumerable.Excepto (segundo)
- ParallelEnumerable.Excepto (segundo, comparar)
- ParaleloEnumerable.Primero()
- ParallelEnumerable.First(predicado)
- ParallelEnumerable.FirstOrDefault()
- ParallelEnumerable.FirstOrDefault(predicado)
- ParallelEnumerable.ForAll(acción)
- ParallelEnumerable.GroupBy(keySelector)
- ParallelEnumerable.GroupBy(keySelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, elementSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, resultSelector)
- ParallelEnumerable.GroupBy(keySelector, resultSelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector, comparador)
- ParallelEnumerable.GroupJoin(inner, outsideKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.GroupJoin(inner, outsideKeySelector, innerKeySelector, resultSelector, comparer)
- ParallelEnumerable.Intersect(segundo)
- ParallelEnumerable.Intersect(segundo, comparador)
- ParallelEnumerable.Join(inner, outsideKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.Join(inner, outsideKeySelector, innerKeySelector, resultSelector, comparer)
- ParallelEnumerable.Last()
- ParallelEnumerable.Last (predicado)
- ParallelEnumerable.LastOrDefault()
- ParallelEnumerable.LastOrDefault(predicado)
- ParallelEnumerable.LongCount()
- ParallelEnumerable.LongCount(predicado)
- ParallelEnumerable.Max()
- ParallelEnumerable.Max(selector)
- ParallelEnumerable.Min()
- ParallelEnumerable.Min(selector)
- ParallelEnumerable.OfType()
- ParallelEnumerable.OrderBy(keySelector)
- ParallelEnumerable.OrderBy(keySelector, comparador)
- ParallelEnumerable.OrderByDescending(keySelector)
- ParallelEnumerable.OrderByDescending(keySelector, comparador)
- ParallelEnumerable.Range (inicio, recuento)
- ParallelEnumerable.Repeat(elemento, cuenta)
- ParallelEnumerable.Reverse()
- ParallelEnumerable.Select(selector)
- ParallelEnumerable.SelectMany(selector)
- ParallelEnumerable.SelectMany(selector de colección, selector de resultado)
- ParallelEnumerable.SequenceEqual(segundo)
- ParallelEnumerable.SequenceEqual(segundo, comparador)
- ParallelEnumerable.Single()
- ParallelEnumerable.Single (predicado)
- ParallelEnumerable.SingleOrDefault()
- ParallelEnumerable.SingleOrDefault(predicado)
- ParallelEnumerable.Skip(contar)
- ParallelEnumerable.SkipWhile(predicado)
- ParaleloEnumerable.Suma()
- ParallelEnumerable.Suma(selector)
- ParallelEnumerable.Tomar (contar)
- ParallelEnumerable.TakeWhile(predicado)
- ParallelEnumerable.ThenBy(keySelector)
- ParallelEnumerable.ThenBy(keySelector, comparador)
- ParallelEnumerable.ThenByDescending(keySelector)
- ParallelEnumerable.ThenByDescending(keySelector, comparador)
- ParallelEnumerable.ToArray()
- ParallelEnumerable.ToDictionary(keySelector)
- ParallelEnumerable.ToDictionary(keySelector, comparar)
- ParallelEnumerable.ToDictionary(elementSelector)
- ParallelEnumerable.ToDictionary(elementSelector, comparar)
- ParallelEnumerable.ToList()
- ParallelEnumerable.ToLookup(keySelector)
- ParallelEnumerable.ToLookup(keySelector, comparar)
- ParallelEnumerable.ToLookup(keySelector, elementSelector)
- ParallelEnumerable.ToLookup(keySelector, elementSelector, comparador)
- ParallelEnumerable.Union (segundo)
- ParallelEnumerable.Union (segundo, comparar)
- ParaleloEnumerable.Dónde(predicado)
- ParallelEnumerable.WithCancellation(token de cancelación)
- ParallelEnumerable.WithDegreeOfParallelism(gradoDeParalelismo)
- ParallelEnumerable.WithExecutionMode (modo de ejecución)
- ParallelEnumerable.WithMergeOptions(mergeOptions)
- ParallelEnumerable.Zip (segundo, selector de resultados)

## Ejemplo sencillo
Este ejemplo muestra cómo se puede usar PLINQ para calcular los números pares entre 1 y 10,000 usando varios subprocesos. ¡Tenga en cuenta que la lista resultante no estará ordenada!

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 4, 26, 28, 30, ... }
    // Order will vary with different runs


## ConGradoDeParalelismo
El grado de paralelismo es el número máximo de tareas que se ejecutan simultáneamente y que se utilizarán para procesar la consulta.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .WithDegreeOfParallelism(4)
                              .Where(x => x % 2 == 0);



## Según lo ordenado
Este ejemplo muestra cómo se puede usar PLINQ para calcular los números pares entre 1 y 10,000 usando varios subprocesos. El orden se mantendrá en la lista resultante, sin embargo, tenga en cuenta que `AsOrdered` puede perjudicar el rendimiento de una gran cantidad de elementos, por lo que se prefiere el procesamiento desordenado cuando sea posible.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .AsOrdered()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 2, 4, 6, 8, ..., 10000 }


## ComoDesordenado
Las secuencias ordenadas pueden perjudicar el rendimiento cuando se trata de una gran cantidad de elementos. Para mitigar esto, es posible llamar a `AsUnordered` cuando el orden de la secuencia ya no es necesario.

    var sequence = Enumerable.Range(1, 10000).Select(x => -1 * x); // -1, -2, ...
    var evenNumbers = sequence.AsParallel()
                              .OrderBy(x => x)
                              .Take(5000)
                              .AsUnordered()
                              .Where(x => x % 2 == 0) // This line won't be affected by ordering
                              .ToList();

