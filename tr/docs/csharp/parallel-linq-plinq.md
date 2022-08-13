---
title: "Paralel LINQ (PLINQ)"
slug: "paralel-linq-plinq"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Sözdizimi
- ParallelEnumerable.Aggregate(işlev)
- ParallelEnumerable.Aggregate(tohum, func)
- ParallelEnumerable.Aggregate(tohum, updateAccumulatorFunc, CombineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.Aggregate(seedFactory, updateAccumulatorFunc, CombineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.All(yüklem)
- ParallelEnumerable.Any()
- ParallelEnumerable.Any(yüklem)
- ParallelEnumerable.AsEnumerable()
- ParallelEnumerable.AsOrdered()
- ParallelEnumerable.AsParallel()
- ParallelEnumerable.AsSequential()
- ParallelEnumerable.AsUnordered()
- ParallelEnumerable.Average(seçici)
- ParallelEnumerable.Cast()
- ParallelEnumerable.Concat(saniye)
- ParallelEnumerable.Contains(değer)
- ParallelEnumerable.Contains(değer, karşılaştır)
- ParallelEnumerable.Count()
- ParallelEnumerable.Count(yüklem)
- ParallelEnumerable.DefaultIfEmpty()
- ParallelEnumerable.DefaultIfEmpty(defaultValue)
- ParallelEnumerable.Distinct()
- ParallelEnumerable.Distinct(karşılaştır)
- ParallelEnumerable.ElementAt(index)
- ParallelEnumerable.ElementAtOrDefault(index)
- ParallelEnumerable.Empty()
- ParallelEnumerable.Except(saniye)
- ParallelEnumerable.Except(ikinci, karşılaştır)
- ParallelEnumerable.First()
- ParallelEnumerable.First(yüklem)
- ParallelEnumerable.FirstOrDefault()
- ParallelEnumerable.FirstOrDefault(yüklem)
- ParallelEnumerable.ForAll(eylem)
- ParallelEnumerable.GroupBy(keySelector)
- ParallelEnumerable.GroupBy(keySelector, karşılaştırıcı)
- ParallelEnumerable.GroupBy(keySelector, elementSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, karşılaştırıcı)
- ParallelEnumerable.GroupBy(keySelector, resultSelector)
- ParallelEnumerable.GroupBy(keySelector, sonuçSelector, karşılaştırıcı)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector, karşılaştırıcı)
- ParallelEnumerable.GroupJoin(iç, dışKeySelector, innerKeySelector, sonuçSelector)
- ParallelEnumerable.GroupJoin(iç, dışKeySelector, innerKeySelector, sonuçSelector, karşılaştırıcı)
- ParallelEnumerable.Intersect(saniye)
- ParallelEnumerable.Intersect(saniye, karşılaştırıcı)
- ParallelEnumerable.Join(iç, dışKeySelector, innerKeySelector, sonuçSelector)
- ParallelEnumerable.Join(inner, externalKeySelector, innerKeySelector, sonuçSelector, karşılaştırıcı)
- ParallelEnumerable.Last()
- ParallelEnumerable.Last(yüklem)
- ParallelEnumerable.LastOrDefault()
- ParallelEnumerable.LastOrDefault(yüklem)
- ParallelEnumerable.LongCount()
- ParallelEnumerable.LongCount(yüklem)
- ParallelEnumerable.Max()
- ParallelEnumerable.Max(seçici)
- ParallelEnumerable.Min()
- ParallelEnumerable.Min(seçici)
- ParallelEnumerable.OfType()
- ParallelEnumerable.OrderBy(keySelector)
- ParallelEnumerable.OrderBy(keySelector, karşılaştırıcı)
- ParallelEnumerable.OrderByDescending(keySelector)
- ParallelEnumerable.OrderByDescending(keySelector, karşılaştırıcı)
- ParallelEnumerable.Range(başlat, say)
- ParallelEnumerable.Repeat(eleman, sayım)
- ParallelEnumerable.Reverse()
- ParallelEnumerable.Select(selektör)
- ParallelEnumerable.SelectMany(seçici)
- ParallelEnumerable.SelectMany(collectionSelector, resultSelector)
- ParallelEnumerable.SequenceEqual(saniye)
- ParallelEnumerable.SequenceEqual(ikinci, karşılaştırıcı)
- ParallelEnumerable.Single()
- ParallelEnumerable.Single(yüklem)
- ParallelEnumerable.SingleOrDefault()
- ParallelEnumerable.SingleOrDefault(yüklem)
- ParallelEnumerable.Skip(sayım)
- ParallelEnumerable.SkipWhile(yüklem)
- ParallelEnumerable.Sum()
- ParallelEnumerable.Sum(seçici)
- ParallelEnumerable.Take(sayım)
- ParallelEnumerable.TakeWhile(yüklem)
- ParallelEnumerable.ThenBy(keySelector)
- ParallelEnumerable.ThenBy(keySelector, karşılaştırıcı)
- ParallelEnumerable.ThenByDescending(keySelector)
- ParallelEnumerable.ThenByDescending(keySelector, karşılaştırıcı)
- ParallelEnumerable.ToArray()
- ParallelEnumerable.ToDictionary(keySelector)
- ParallelEnumerable.ToDictionary(keySelector, karşılaştır)
- ParallelEnumerable.ToDictionary(elementSelector)
- ParallelEnumerable.ToDictionary(elementSelector, karşılaştır)
- ParallelEnumerable.ToList()
- ParallelEnumerable.ToLookup(keySelector)
- ParallelEnumerable.ToLookup(keySelector, karşılaştır)
- ParallelEnumerable.ToLookup(keySelector, elementSelector)
- ParallelEnumerable.ToLookup(keySelector, elementSelector, karşılaştırıcı)
- ParallelEnumerable.Union(saniye)
- ParallelEnumerable.Union(ikinci, karşılaştır)
- ParallelEnumerable.Where(yüklem)
- ParallelEnumerable.WithCancellation(cancellationToken)
- ParallelEnumerable.WithDegreeOfParallelism(degreeOfParallelism)
- ParallelEnumerable.WithExecutionMode(executionMode)
- ParallelEnumerable.WithMergeOptions(mergeOptions)
- ParallelEnumerable.Zip(saniye, sonuçSelector)

## Basit örnek
Bu örnek, birden çok iş parçacığı kullanarak 1 ile 10.000 arasındaki çift sayıları hesaplamak için PLINQ'un nasıl kullanılabileceğini gösterir. Ortaya çıkan listenin sipariş edilmeyeceğini unutmayın!

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 4, 26, 28, 30, ... }
    // Order will vary with different runs


## Paralellik Derecesi ile
Paralellik derecesi, sorguyu işlemek için kullanılacak eşzamanlı olarak yürütülen görevlerin maksimum sayısıdır.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .WithDegreeOfParallelism(4)
                              .Where(x => x % 2 == 0);



##Sipariş Edildiği Gibi
Bu örnek, birden çok iş parçacığı kullanarak 1 ile 10.000 arasındaki çift sayıları hesaplamak için PLINQ'un nasıl kullanılabileceğini gösterir. Sonuç listesinde sıralama korunacaktır, ancak "AsOrdered"ın çok sayıda öğe için performansa zarar verebileceğini unutmayın, bu nedenle mümkün olduğunda sırasız işleme tercih edilir.

    var sequence = Enumerable.Range(1, 10000);
    var evenNumbers = sequence.AsParallel()
                              .AsOrdered()
                              .Where(x => x % 2 == 0)
                              .ToList();

    // evenNumbers = { 2, 4, 6, 8, ..., 10000 }


## Sırasız
Çok sayıda öğeyle uğraşırken sıralı diziler performansa zarar verebilir. Bunu azaltmak için, sıra sırası artık gerekli olmadığında 'AsUnordered'ı çağırmak mümkündür.

    var sequence = Enumerable.Range(1, 10000).Select(x => -1 * x); // -1, -2, ...
    var evenNumbers = sequence.AsParallel()
                              .OrderBy(x => x)
                              .Take(5000)
                              .AsUnordered()
                              .Where(x => x % 2 == 0) // This line won't be affected by ordering
                              .ToList();

