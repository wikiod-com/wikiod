---
title: "LINQ parallèle (PLINQ)"
slug: "linq-parallele-plinq"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Syntaxe
- ParallelEnumerable.Aggregate(func)
- ParallelEnumerable.Aggregate(seed, func)
- ParallelEnumerable.Aggregate (seed, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.Aggregate(seedFactory, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.All(prédicat)
- ParallèleEnumerable.Any()
- ParallelEnumerable.Any(prédicat)
- ParallelEnumerable.AsEnumerable()
- ParallelEnumerable.AsOrdered()
- ParallèleEnumerable.AsParallel()
- ParallèleEnumerable.AsSequential()
- ParallelEnumerable.AsUnordered()
- ParallelEnumerable.Average (sélecteur)
- ParallelEnumerable.Cast()
- ParallelEnumerable.Concat (seconde)
- ParallelEnumerable.Contains(value)
- ParallelEnumerable.Contains(value, comparer)
-ParallelEnumerable.Count()
- ParallelEnumerable.Count (prédicat)
- ParallèleEnumerable.DefaultIfEmpty()
-ParallelEnumerable.DefaultIfEmpty(defaultValue)
- ParallèleEnumerable.Distinct()
- ParallelEnumerable.Distinct(comparer)
-ParallelEnumerable.ElementAt(index)
-ParallelEnumerable.ElementAtOrDefault(index)
- ParallèleEnumerable.Empty()
- ParallelEnumerable.Except(second)
- ParallelEnumerable.Except(second, comparer)
- ParallèleEnumerable.Premier()
- ParallelEnumerable.First(prédicat)
- ParallèleEnumerable.FirstOrDefault()
- ParallelEnumerable.FirstOrDefault(prédicat)
- ParallèleEnumerable.ForAll(action)
-ParallelEnumerable.GroupBy(keySelector)
-ParallelEnumerable.GroupBy(keySelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, elementSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, resultSelector)
- ParallelEnumerable.GroupBy(keySelector, resultSelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector, comparer)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
- ParallelEnumerable.Intersect(second)
- ParallelEnumerable.Intersect(second, comparer)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
- ParallèleEnumerable.Last()
- ParallelEnumerable.Last(prédicat)
- ParallèleEnumerable.LastOrDefault()
- ParallelEnumerable.LastOrDefault(prédicat)
-ParallelEnumerable.LongCount()
- ParallelEnumerable.LongCount (prédicat)
- ParallèleEnumerable.Max()
- ParallelEnumerable.Max (sélecteur)
- ParallèleEnumerable.Min()
- ParallelEnumerable.Min(sélecteur)
- ParallèleEnumerable.OfType()
-ParallelEnumerable.OrderBy(keySelector)
- ParallelEnumerable.OrderBy(keySelector, comparer)
- ParallelEnumerable.OrderByDescending(keySelector)
- ParallelEnumerable.OrderByDescending(keySelector, comparer)
- ParallelEnumerable.Range (début, nombre)
- ParallelEnumerable.Repeat(element, count)
- ParallèleEnumerable.Reverse()
- ParallelEnumerable.Select(sélecteur)
- ParallelEnumerable.SelectMany(sélecteur)
- ParallelEnumerable.SelectMany(collectionSelector, resultSelector)
- ParallelEnumerable.SequenceEqual (seconde)
- ParallelEnumerable.SequenceEqual(deuxième, comparateur)
- ParallèleEnumerable.Single()
- ParallelEnumerable.Single(prédicat)
- ParallèleEnumerable.SingleOrDefault()
- ParallelEnumerable.SingleOrDefault(prédicat)
- ParallèleEnumerable.Skip(count)
- ParallelEnumerable.SkipWhile(prédicat)
- ParallèleEnumerable.Sum()
- ParallelEnumerable.Sum(sélecteur)
- ParallelEnumerable.Take(count)
- ParallelEnumerable.TakeWhile(prédicat)
-ParallelEnumerable.ThenBy(keySelector)
- ParallelEnumerable.ThenBy(keySelector, comparer)
- ParallelEnumerable.ThenByDescending(keySelector)
- ParallelEnumerable.ThenByDescending(keySelector, comparer)
- ParallèleEnumerable.ToArray()
-ParallelEnumerable.ToDictionary(keySelector)
- ParallelEnumerable.ToDictionary(keySelector, comparer)
-ParallelEnumerable.ToDictionary(elementSelector)
- ParallelEnumerable.ToDictionary(elementSelector, comparer)
- ParallèleEnumerable.ToList()
-ParallelEnumerable.ToLookup(keySelector)
- ParallelEnumerable.ToLookup(keySelector, comparer)
- ParallelEnumerable.ToLookup(keySelector, elementSelector)
- ParallelEnumerable.ToLookup(keySelector, elementSelector, comparer)
- ParallelEnumerable.Union(deuxième)
- ParallelEnumerable.Union(second, comparer)
- ParallelEnumerable.Where(prédicat)
- ParallelEnumerable.WithCancellation(cancellationToken)
- ParallelEnumerable.WithDegreeOfParallelism(degreeOfParallelism)
- ParallelEnumerable.WithExecutionMode(executionMode)
-ParallelEnumerable.WithMergeOptions(mergeOptions)
- ParallelEnumerable.Zip(second, resultSelector)

## Exemple simple
Cet exemple montre comment PLINQ peut être utilisé pour calculer les nombres pairs entre 1 et 10 000 en utilisant plusieurs threads. Notez que la liste résultante ne sera pas commandée !

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 4, 26, 28, 30, ... }
    // Order will vary with different runs


## WithDegreeOfParallelism
Le degré de parallélisme est le nombre maximal de tâches exécutées simultanément qui seront utilisées pour traiter la requête.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .WithDegreeOfParallelism(4)
                              .Where(x => x % 2 == 0);



##Commandé
Cet exemple montre comment PLINQ peut être utilisé pour calculer les nombres pairs entre 1 et 10 000 en utilisant plusieurs threads. L'ordre sera maintenu dans la liste résultante, mais gardez à l'esprit que `AsOrdered` peut nuire aux performances pour un grand nombre d'éléments, donc le traitement non ordonné est préféré lorsque cela est possible.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .AsOrdered()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 2, 4, 6, 8, ..., 10000 }


## AsUnordered
Les séquences ordonnées peuvent nuire aux performances lorsqu'elles traitent un grand nombre d'éléments. Pour atténuer cela, il est possible d'appeler `AsUnordered` lorsque l'ordre de la séquence n'est plus nécessaire.

    var sequence = Enumerable.Range(1, 10000).Select(x => -1 * x); // -1, -2, ...
    var evenNumbers = sequence.AsParallel()
                              .OrderBy(x => x)
                              .Take(5000)
                              .AsUnordered()
                              .Where(x => x % 2 == 0) // This line won't be affected by ordering
                              .ToList();

