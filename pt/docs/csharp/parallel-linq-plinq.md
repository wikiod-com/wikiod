---
title: "LINQ Paralelo (PLINQ)"
slug: "linq-paralelo-plinq"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Sintaxe
- ParallelEnumerable.Aggregate(func)
- ParallelEnumerable.Aggregate(seed, func)
- ParallelEnumerable.Aggregate(seed, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.Aggregate(seedFactory, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.All(predicado)
- ParallelEnumerable.Any()
- ParallelEnumerable.Any(predicado)
- ParallelEnumerable.AsEnumerable()
- ParallelEnumerable.AsOrdered()
- ParallelEnumerable.AsParallel()
- ParallelEnumerable.AsSequential()
- ParallelEnumerable.AsUnordered()
- ParallelEnumerable.Average(seletor)
- ParallelEnumerable.Cast()
- ParallelEnumerable.Concat(segundo)
- ParallelEnumerable.Contains(value)
- ParallelEnumerable.Contains(valor, comparar)
- ParallelEnumerable.Count()
- ParallelEnumerable.Count(predicado)
- ParallelEnumerable.DefaultIfEmpty()
- ParallelEnumerable.DefaultIfEmpty(defaultValue)
- ParallelEnumerable.Distinct()
- ParallelEnumerable.Distinct(compare)
- ParallelEnumerable.ElementAt(index)
- ParallelEnumerable.ElementAtOrDefault(index)
- ParallelEnumerable.Empty()
- ParallelEnumerable.Except(segundo)
- ParallelEnumerable.Except(segundo, comparar)
- ParallelEnumerable.First()
- ParallelEnumerable.First(predicado)
- ParallelEnumerable.FirstOrDefault()
- ParallelEnumerable.FirstOrDefault(predicado)
- ParallelEnumerable.ForAll(action)
- ParallelEnumerable.GroupBy(keySelector)
- ParallelEnumerable.GroupBy(keySelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, elementSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, resultSelector)
- ParallelEnumerable.GroupBy(keySelector, resultSelector, comparador)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector, comparador)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector, comparador)
- ParallelEnumerable.Intersect(segundo)
- ParallelEnumerable.Intersect(segundo, comparador)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparador)
- ParallelEnumerable.Last()
- ParallelEnumerable.Last(predicado)
- ParallelEnumerable.LastOrDefault()
- ParallelEnumerable.LastOrDefault(predicado)
- ParallelEnumerable.LongCount()
- ParallelEnumerable.LongCount(predicado)
- ParallelEnumerable.Max()
- ParallelEnumerable.Max(seletor)
- ParallelEnumerable.Min()
- ParallelEnumerable.Min(seletor)
- ParallelEnumerable.OfType()
- ParallelEnumerable.OrderBy(keySelector)
- ParallelEnumerable.OrderBy(keySelector, comparador)
- ParallelEnumerable.OrderByDescending(keySelector)
- ParallelEnumerable.OrderByDescending(keySelector, comparador)
- ParallelEnumerable.Range(in??cio, contagem)
- ParallelEnumerable.Repeat(element, count)
- ParallelEnumerable.Reverse()
- ParallelEnumerable.Select(seletor)
- ParallelEnumerable.SelectMany(seletor)
- ParallelEnumerable.SelectMany(collectionSelector, resultSelector)
- ParallelEnumerable.SequenceEqual(segundo)
- ParallelEnumerable.SequenceEqual(segundo, comparador)
- ParallelEnumerable.Single()
- ParallelEnumerable.Single(predicado)
- ParallelEnumerable.SingleOrDefault()
- ParallelEnumerable.SingleOrDefault(predicado)
- ParallelEnumerable.Skip(count)
- ParallelEnumerable.SkipWhile(predicado)
- ParallelEnumerable.Sum()
- ParallelEnumerable.Sum(seletor)
- ParallelEnumerable.Take(count)
- ParallelEnumerable.TakeWhile(predicado)
- ParallelEnumerable.ThenBy(keySelector)
- ParallelEnumerable.ThenBy(keySelector, comparador)
- ParallelEnumerable.ThenByDescending(keySelector)
- ParallelEnumerable.ThenByDescending(keySelector, comparador)
- ParallelEnumerable.ToArray()
- ParallelEnumerable.ToDictionary(keySelector)
- ParallelEnumerable.ToDictionary(keySelector, compare)
- ParallelEnumerable.ToDictionary(elementSelector)
- ParallelEnumerable.ToDictionary(elementSelector, compare)
- ParallelEnumerable.ToList()
- ParallelEnumerable.ToLookup(keySelector)
- ParallelEnumerable.ToLookup(keySelector, compare)
- ParallelEnumerable.ToLookup(keySelector, elementSelector)
- ParallelEnumerable.ToLookup(keySelector, elementSelector, comparador)
- ParallelEnumerable.Union(segundo)
- ParallelEnumerable.Union(segundo, comparar)
- ParallelEnumerable.Where(predicado)
- ParallelEnumerable.WithCancellation(cancellationToken)
- ParallelEnumerable.WithDegreeOfParallelism(degreeOfParallelism)
- ParallelEnumerable.WithExecutionMode(executionMode)
- ParallelEnumerable.WithMergeOptions(mergeOptions)
- ParallelEnumerable.Zip(second, resultSelector)

## Exemplo simples
Este exemplo mostra como PLINQ pode ser usado para calcular os n??meros pares entre 1 e 10.000 usando v??rios threads. Observe que a lista resultante n??o ser?? ordenada!

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 4, 26, 28, 30, ... }
    // Order will vary with different runs


## WithDegreeOfParalelism
O grau de paralelismo ?? o n??mero m??ximo de tarefas executadas simultaneamente que ser??o usadas para processar a consulta.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .WithDegreeOfParallelism(4)
                              .Where(x => x % 2 == 0);



## Conforme pedido
Este exemplo mostra como PLINQ pode ser usado para calcular os n??meros pares entre 1 e 10.000 usando v??rios threads. A ordem ser?? mantida na lista resultante, mas lembre-se de que `AsOrdered` pode prejudicar o desempenho de um grande n??mero de elementos, portanto, o processamento n??o ordenado ?? prefer??vel quando poss??vel.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .AsOrdered()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 2, 4, 6, 8, ..., 10000 }


## Como n??o ordenado
Sequ??ncias ordenadas podem prejudicar o desempenho ao lidar com um grande n??mero de elementos. Para mitigar isso, ?? poss??vel chamar `AsUnordered` quando a ordem da sequ??ncia n??o for mais necess??ria.

    var sequence = Enumerable.Range(1, 10000).Select(x => -1 * x); // -1, -2, ...
    var evenNumbers = sequence.AsParallel()
                              .OrderBy(x => x)
                              .Take(5000)
                              .AsUnordered()
                              .Where(x => x % 2 == 0) // This line won't be affected by ordering
                              .ToList();

