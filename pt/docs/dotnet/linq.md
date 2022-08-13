---
title: "LINQ"
slug: "linq"
draft: false
images: []
weight: 9322
type: docs
toc: true
---

LINQ (Language Integrated Query) é uma expressão que recupera dados de uma fonte de dados. O LINQ simplifica essa situação oferecendo um modelo consistente para trabalhar com dados em vários tipos de fontes e formatos de dados. Em uma consulta LINQ, você está sempre trabalhando com objetos. Você usa os mesmos padrões básicos de codificação para consultar e transformar dados em documentos XML, bancos de dados SQL, conjuntos de dados ADO.NET, coleções .NET e qualquer outro formato para o qual um provedor esteja disponível. LINQ pode ser usado em C# e VB.

## Sintaxe
- Public static TSource Aggregate\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, TSource, TSource\> func)
- Public static TAccumulate Aggregate\<TSource, TAccumulate\>(esta IEnumerable\<TSource\> source, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func)
- Public static TResult Aggregate\<TSource, TAccumulate, TResult\>(esta IEnumerable\<TSource\> source, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func, Func\<TAccumulate, TResult\> resultSelector)
- public static Boolean All\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static Boolean Any\<TSource\>(este IEnumerable\<TSource\> source)
- public static Boolean Any\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> AsEnumerable\<TSource\>(este IEnumerable\<TSource\> source)
- média decimal estática pública (esta fonte IEnumerable\<Decimal\>)
- Public static Double Average(esta fonte IEnumerable\<Double\>)
- Public static Double Average(este IEnumerable\<Int32\> source)
- Public static Double Average(este IEnumerable\<Int64\> source)
- public static Nullable\<Decimal\> Average(este IEnumerable\<Nullable\<Decimal\>\> source)
- public static Nullable\<Double\> Average(este IEnumerable\<Nullable\<Double\>\> source)
- public static Nullable\<Double\> Average(este IEnumerable\<Nullable\<Int32\>\> source)
- public static Nullable\<Double\> Average(este IEnumerable\<Nullable\<Int64\>\> source)
- public static Nullable\<Single\> Average(este IEnumerable\<Nullable\<Single\>\> source)
- Public static Single Average(esta IEnumerable\<Single\> source)
- Public static Decimal Average\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor  Decimal\>)
- Public static Double Average\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Double\> seletor)
- Public static Double Average\<TSource\>(esta fonte IEnumerable\<TSource\>, seletor Func\<TSource, Int32\>)
- Public static Double Average\<TSource\>(esta fonte IEnumerable\<TSource\>, seletor Func\<TSource, Int64\>)
- public static Nullable\<Decimal\> Average\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> seletor)
- public static Nullable\<Double\> Average\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> seletor)
- public static Nullable\<Double\> Average\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> seletor)
- public static Nullable\<Double\> Average\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> seletor)
- public static Nullable\<Single\> Average\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Nullable\<Single\>\> seletor)
- Public static Single Average\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Single\> seletor)
- public static IEnumerable\<TResult\> Cast\<TResult\>(esta fonte IEnumerable)
- public static IEnumerable\<TSource\> Concat\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo)
- public static Boolean Contains\<TSource\>(este IEnumerable\<TSource\> source, valor TSource)
- public static Boolean Contains\<TSource\>(este IEnumerable\<TSource\> source, TSource value, IEqualityComparer\<TSource\> comparador)
- Public static Int32 Count\<TSource\>(este IEnumerable\<TSource\> source)
- Public static Int32 Count\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(este IEnumerable\<TSource\> source)
- public static IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(este IEnumerable\<TSource\> source, TSource defaultValue)
- public static IEnumerable\<TSource\> Distinct\<TSource\>(este IEnumerable\<TSource\> source)
- public static IEnumerable\<TSource\> Distinct\<TSource\>(este IEnumerable\<TSource\> source, IEqualityComparer\<TSource\> comparador)
- public static TSource ElementAt\<TSource\>(esta fonte IEnumerable\<TSource\>, índice Int32)
- public static TSource ElementAtOrDefault\<TSource\>(esta fonte IEnumerable\<TSource\>, índice Int32)
- public static IEnumerable\<TResult\> Empty\<TResult\>()
- public static IEnumerable\<TSource\> Except\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Except\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static TSource First\<TSource\>(este IEnumerable\<TSource\> source)
- public static TSource First\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static TSource FirstOrDefault\<TSource\>(este IEnumerable\<TSource\> source)
- público estático TSource FirstOrDefault\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector)
- public static IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> seletor de resultados)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(este IEnumerable\<TOuter\> externo,IEnumerable\<TInner\> interno, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector)
- public static IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(este IEnumerable\<TOuter\> externo, IEnumerable\<TInner\> interno, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(este IEnumerable\<TOuter\> externo, IEnumerable\<TInner\> interno, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(este IEnumerable\<TOuter\> externo, IEnumerable\<TInner\> interno, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static TSource Last\<TSource\>(este IEnumerable\<TSource\> source)
- public static TSource Last\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static TSource LastOrDefault\<TSource\>(este IEnumerable\<TSource\> source)
- público estático TSource LastOrDefault\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static Int64 LongCount\<TSource\>(este IEnumerable\<TSource\> source)
- public static Int64 LongCount\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static Decimal Max(este IEnumerable\<Decimal\> source)
- public static Double Max(este IEnumerable\<Double\> source)
- public static Int32 Max(este IEnumerable\<Int32\> source)
- public static Int64 Max(este IEnumerable\<Int64\> source)
- public static Nullable\<Decimal\> Max(este IEnumerable\<Nullable\<Decimal\>\> source)
- public static Nullable\<Double\> Max(this IEnumerable\<Nullable\<Double\>\> source)
- public static Nullable\<Int32\> Max(este IEnumerable\<Nullable\<Int32\>\> source)
- public static Nullable\<Int64\> Max(este IEnumerable\<Nullable\<Int64\>\> source)
- public static Nullable\<Single\> Max(este IEnumerable\<Nullable\<Single\>\> source)
- public static Single Max(este IEnumerable\<Single\> source)
- público static TSource Max\<TSource\>(este IEnumerable\<TSource\> source)
- público static Decimal Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Decimal\>)
- public static Double Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Double\>)
- público static Int32 Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor  Int32\>)
- public static Int64 Max\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int64\> seletor)
- public static Nullable\<Decimal\> Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> seletor)
- public static Nullable\<Double\> Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> seletor)
- public static Nullable\<Int32\> Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> seletor)
- public static Nullable\<Int64\> Max\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> seletor)
- public static Nullable\<Single\> Max\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Nullable\<Single\>\> seletor)
- público static Single Max\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Single\> seletor)
- público static TResult Max\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TResult\> seletor)
- public static Decimal Min(este IEnumerable\<Decimal\> source)
- public static Double Min(este IEnumerable\<Double\> source)
- public static Int32 Min(este IEnumerable\<Int32\> source)
- public static Int64 Min(este IEnumerable\<Int64\> source)
- public static Nullable\<Decimal\> Min(this IEnumerable\<Nullable\<Decimal\>\> source)
- public static Nullable\<Double\> Min(this IEnumerable\<Nullable\<Double\>\> source)
- public static Nullable\<Int32\> Min(this IEnumerable\<Nullable\<Int32\>\> source)
- public static Nullable\<Int64\> Min(this IEnumerable\<Nullable\<Int64\>\> source)
- public static Nullable\<Single\> Min(this IEnumerable\<Nullable\<Single\>\> source)
- public static Single Min(este IEnumerable\<Single\> source)
- public static TSource Min\<TSource\>(este IEnumerable\<TSource\> source)
- public static Decimal Min\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Decimal\>)
- public static Double Min\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Double\>)
- public static Int32 Min\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32\> seletor)
- public static Int64 Min\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int64\> seletor)
- public static Nullable\<Decimal\> Min\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> seletor)
- public static Nullable\<Double\> Min\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Nullable\<Double\>\> seletor)
- public static Nullable\<Int32\> Min\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> seletor)
- public static Nullable\<Int64\> Min\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> seletor)
- public static Nullable\<Single\> Min\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Nullable\<Single\>\> seletor)
- public static Single Min\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Single\> seletor)
- public static TResult Min\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TResult\> seletor)
- public static IEnumerable\<TResult\> OfType\<TResult\>(esta fonte IEnumerable)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparador)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparador)
- public static IEnumerable\<Int32\> Range(Int32 start, Int32 count)
- public static IEnumerable\<TResult\> Repeat\<TResult\>(elemento TResult, contagem Int32)
- public static IEnumerable\<TSource\> Reverse\<TSource\>(este IEnumerable\<TSource\> source)
- public static IEnumerable\<TResult\> Select\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, TResult\> seletor)
- public static IEnumerable\<TResult\> Select\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32, TResult\> seletor)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, IEnumerable\<TResult\>\> seletor)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32, IEnumerable\<TResult\>\> seletor)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource, TCollection , TResult\> seletor de resultados)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource , TCollection, TResult\> seletor de resultados)
- public static Boolean SequenceEqual\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo)
- public static Boolean SequenceEqual\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static TSource Single\<TSource\>(este IEnumerable\<TSource\> source)
- public static TSource Single\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static TSource SingleOrDefault\<TSource\>(este IEnumerable\<TSource\> source)
- public static TSource SingleOrDefault\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> Skip\<TSource\>(este IEnumerable\<TSource\> fonte, contagem Int32)
- IEnumerable estático público\<TSource\> SkipWhile\<TSource\>(este IEnumerable\<TSource\> origem, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> SkipWhile\<TSource\>(este IEnumerable\<TSource\> fonte, Func\<TSource, Int32, Boolean\> predicado)
- Public static Decimal Sum(esta IEnumerable\<Decimal\> source)
- Public static Double Sum(este IEnumerable\<Double\> source)
- Public static Int32 Sum(this IEnumerable\<Int32\> source)
- Public static Int64 Sum(this IEnumerable\<Int64\> source)
- public static Nullable\<Decimal\> Sum(this IEnumerable\<Nullable\<Decimal\>\> source)
- public static Nullable\<Double\> Sum(this IEnumerable\<Nullable\<Double\>\> source)
- public static Nullable\<Int32\> Sum(this IEnumerable\<Nullable\<Int32\>\> source)
- public static Nullable\<Int64\> Sum(this IEnumerable\<Nullable\<Int64\>\> source)
- public static Nullable\<Single\> Sum(este IEnumerable\<Nullable\<Single\>\> source)
- Public static Single Sum(esta IEnumerable\<Single\> source)
- Public static Decimal Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Decimal\>)
- Public static Double Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, seletor Double\>)
- Public static Int32 Sum\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32\> seletor)
- public static Int64 Sum\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int64\> seletor)
- public static Nullable\<Decimal\> Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> seletor)
- public static Nullable\<Double\> Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> seletor)
- public static Nullable\<Int32\> Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> seletor)
- public static Nullable\<Int64\> Sum\<TSource\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> seletor)
- public static Nullable\<Single\> Sum\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Nullable\<Single\>\> seletor)
- public static Single Sum\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Single\> seletor)
- public static IEnumerable\<TSource\> Take\<TSource\>(este IEnumerable\<TSource\> source, contagem Int32)
- IEnumerable estático público\<TSource\> TakeWhile\<TSource\>(este IEnumerable\<TSource\> fonte, Func\<TSource, Boolean\> predicado)
- IEnumerable estático público\<TSource\> TakeWhile\<TSource\>(este IEnumerable\<TSource\> fonte, Func\<TSource, Int32, Boolean\> predicado)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(essa fonte IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(essa fonte IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparador)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(essa fonte IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(essa fonte IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparador)
- public static TSource[] ToArray\<TSource\>(este IEnumerable\<TSource\> source)
- Public static Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- Public static Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparador)
- public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparador)
- Public static List\<TSource\> ToList\<TSource\>(este IEnumerable\<TSource\> source)
- public static ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(esta fonte IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparador)
- public static ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- public static ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(este IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TSource\> Union\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Union\<TSource\>(este IEnumerable\<TSource\> primeiro, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static IEnumerable\<TSource\> Where\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> Where\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32, Boolean\> predicado)
- public static IEnumerable\<TResult\> Zip\<TFirst, TSecond, TResult\>(este IEnumerable\<TFirst\> first, IEnumerable\<TSecond\> segundo, Func\<TFirst, TSecond, TResult\> resultSelector)

* Veja também [LINQ][1].

Os métodos internos do LINQ são métodos de extensão para a interface `IEnumerable<T>` que residem na classe `System.Linq.Enumerable` no assembly `System.Core`. Eles estão disponíveis no .NET Framework 3.5 e posterior.

O LINQ permite modificação, transformação e combinação simples de vários `IEnumerable`s usando uma sintaxe funcional ou semelhante a consulta.

Embora os métodos LINQ padrão possam funcionar em qualquer `IEnumerable<T>`, incluindo os arrays simples e `List<T>`s, eles também podem ser usados ​​em objetos de banco de dados, onde o conjunto de expressões LINQ pode ser transformado em muitos casos para SQL se o objeto de dados o suportar. Consulte [LINQ to SQL](https://msdn.microsoft.com/en-us/library/bb425822.aspx).

Para os métodos que comparam objetos (como `Contains` e `Except`), `IEquatable<T>.Equals` é usado se o tipo T da coleção implementar essa interface. Caso contrário, os padrões `Equals` e `GetHashCode` do tipo (possivelmente substituídos das implementações padrão de `Object`) são usados. Há também sobrecargas para esses métodos que permitem especificar um `IEqualityComparer<T>` personalizado.

Para os métodos `...OrDefault`, `default(T)` é usado para gerar valores padrão.

Referência oficial: [classe enumerável](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx)

## Avaliação preguiçosa
Praticamente toda consulta que retorna um `IEnumerable<T>` não é avaliada imediatamente; em vez disso, a lógica é atrasada até que a consulta seja iterada. Uma implicação é que cada vez que alguém itera sobre um `IEnumerable<T>` criado a partir de uma dessas consultas, por exemplo, `.Where()`, a lógica de consulta completa é repetida. Se o predicado for de longa duração, isso pode causar problemas de desempenho.

Uma solução simples (quando você sabe ou pode controlar o tamanho aproximado da sequência resultante) é armazenar totalmente os resultados usando `.ToArray()` ou `.ToList()`. `.ToDictionary()` ou `.ToLookup()` podem cumprir a mesma função. É claro que também é possível iterar em toda a sequência e armazenar em buffer os elementos de acordo com outra lógica personalizada.

## `ToArray()` ou `ToList()`?

Ambos `.ToArray()` e `.ToList()` percorrem todos os elementos de uma seqüência `IEnumerable<T>` e salvam os resultados em uma coleção armazenada na memória. Use as seguintes diretrizes para determinar qual escolher:

* Algumas APIs podem exigir um `T[]` ou um `List<T>`.
* `.ToList()` normalmente roda mais rápido e gera menos lixo que `.ToArray()`, porque o último deve copiar todos os elementos em uma nova coleção de tamanho fixo mais uma vez que o primeiro, em quase todos os casos.
* Elementos podem ser adicionados ou removidos do `List<T>` retornado por `.ToList()`, enquanto o `T[]` retornado de `.ToArray()` permanece um tamanho fixo durante toda a sua vida. Em outras palavras, `List<T>` é mutável e `T[]` é imutável.
* O `T[]` retornado de`.ToArray()` usa menos memória do que o `List<T>` retornado de `.ToList()`, então se o resultado for ser armazenado por um longo tempo, prefira `.ToArray()`. Chamar `List<T>.TrimExcess()` tornaria a diferença de memória estritamente acadêmica, ao custo de eliminar a vantagem de velocidade relativa de `.ToList()`.


[1]: https://www.wikiod.com/pt/linq/comecando-com-linq

## SelectMany (mapa plano)
[`Enumerable.Select`][1] retorna um elemento de saída para cada elemento de entrada.
Enquanto [`Enumerable.SelectMany`][2] produz um número variável de elementos de saída para cada elemento de entrada. Isso significa que a sequência de saída pode conter mais ou menos elementos do que na sequência de entrada.

[`Expressões Lambda`][3] passadas para `Enumerable.Select` devem retornar um único item. As expressões lambda passadas para `Enumerable.SelectMany` devem produzir uma sequência filho. Essa sequência filho pode conter um número variável de elementos para cada elemento na sequência de entrada.

**Exemplo**
   
    class Invoice
    {
        public int Id { get; set; }
    }

    class Customer
    {
        public Invoice[] Invoices {get;set;}
    }

    var customers = new[] {
        new Customer {
            Invoices = new[] {
                new Invoice {Id=1},
                new Invoice {Id=2},
            }
        },
        new Customer {
            Invoices = new[] {
                new Invoice {Id=3},
                new Invoice {Id=4},
            }
        },
        new Customer {
            Invoices = new[] {
                new Invoice {Id=5},
                new Invoice {Id=6},
            }
        }
    };
    
    var allInvoicesFromAllCustomers = customers.SelectMany(c => c.Invoices);
    
    Console.WriteLine(
        string.Join(",", allInvoicesFromAllCustomers.Select(i => i.Id).ToArray()));

**Resultado:**
>1,2,3,4,5,6

[Ver demonstração][4]

`Enumerable.SelectMany` também pode ser obtido com uma consulta baseada em sintaxe usando duas cláusulas `from` consecutivas:

    var allInvoicesFromAllCustomers
        = from customer in customers
          from invoice in customer.Invoices
          select invoice;


[1]: https://msdn.microsoft.com/en-us/library/bb548891(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/bb534336(v=vs.100).aspx
[3]: https://www.wikiod.com/pt/docs/c%23/46/lambda-expressions
[4]: https://dotnetfiddle.net/XKGtBr

## Onde (filtro)
Este método retorna um IEnumerable com todos os elementos que atendem a expressão lambda

**Exemplo**

    var personNames = new[] 
    {
        "Foo", "Bar", "Fizz", "Buzz"
    };
    
    var namesStartingWithF = personNames.Where(p => p.StartsWith("F"));
    Console.WriteLine(string.Join(",", namesStartingWithF));

**Resultado:**

>Foo, Fizz

[Ver demonstração][1]


[1]: https://dotnetfiddle.net/nTbZI0

## Algum
Retorna `true` se a coleção tiver algum elemento que atenda à condição na expressão lambda:
   

    var numbers = new[] {1,2,3,4,5};
    
    var isNotEmpty = numbers.Any();
    Console.WriteLine(isNotEmpty); //True

    var anyNumberIsOne = numbers.Any(n => n == 1);
    Console.WriteLine(anyNumberIsOne); //True

    var anyNumberIsSix = numbers.Any(n => n == 6);
    Console.WriteLine(anyNumberIsSix); //False    

    var anyNumberIsOdd = numbers.Any(n => (n & 1) == 1);
    Console.WriteLine(anyNumberIsOdd); //True
    
    var anyNumberIsNegative = numbers.Any(n => n < 0);
    Console.WriteLine(anyNumberIsNegative); //False


## Junte-se ao grupo
    class Developer
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
    
    class Project
    {
        public int DeveloperId { get; set; }
        public string Name { get; set; }
    }

    var developers = new[] {
        new Developer {
            Id = 1,
            Name = "Foobuzz"
        },
        new Developer {
            Id = 2,
            Name = "Barfizz"
        }
    };
    
    var projects = new[] {
        new Project {
            DeveloperId = 1,
            Name = "Hello World 3D"
        },
        new Project {
            DeveloperId = 1,
            Name = "Super Fizzbuzz Maker"
        },
        new Project {
            DeveloperId = 2,
            Name = "Citizen Kane - The action game"
        },
        new Project {
            DeveloperId = 2,
            Name = "Pro Pong 2016"
        }
    };
    
    var grouped = developers.GroupJoin(
        inner: projects,
        outerKeySelector: dev => dev.Id,
        innerKeySelector: proj => proj.DeveloperId,
        resultSelector: 
            (dev, projs) => new {
                DeveloperName = dev.Name, 
                ProjectNames = projs.Select(p => p.Name).ToArray()});
        
    foreach(var item in grouped)
    {
        Console.WriteLine(
            "{0}'s projects: {1}", 
            item.DeveloperName,
            string.Join(", ", item.ProjectNames));
    }

    //Foobuzz's projects: Hello World 3D, Super Fizzbuzz Maker
    //Barfizz's projects: Citizen Kane - The action game, Pro Pong 2016

## Exceto
    var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var evenNumbersBetweenSixAndFourteen = new[] { 6, 8, 10, 12 };

    var result = numbers.Except(evenNumbersBetweenSixAndFourteen);

    Console.WriteLine(string.Join(",", result));

    //1, 2, 3, 4, 5, 7, 9

## Fecho eclair
<!-- if versão <.NET> [gte 4.0] -->

    var tens = new[] {10,20,30,40,50};
    var units = new[] {1,2,3,4,5};
    
    var sums = tens.Zip(units, (first, second) => first + second);
    
    Console.WriteLine(string.Join(",", sums));

    //11,22,33,44,55

<!-- versão final if -->

## Agregado (dobra)
Gerando um novo objeto em cada etapa:

    var elements = new[] {1,2,3,4,5};
    
    var commaSeparatedElements = elements.Aggregate(
        seed: "",
        func: (aggregate, element) => $"{aggregate}{element},");
        
    Console.WriteLine(commaSeparatedElements);  //1,2,3,4,5,
    
Usando o mesmo objeto em todas as etapas:

    var commaSeparatedElements2 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"));
        
    Console.WriteLine(commaSeparatedElements2.ToString());  //1,2,3,4,5,

Usando um seletor de resultado:

    var commaSeparatedElements3 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"),
        resultSelector: (seed) => seed.ToString());
    Console.WriteLine(commaSeparatedElements3);  //1,2,3,4,5,

Se uma semente for omitida, o primeiro elemento se tornará a semente:

    var seedAndElements = elements.Select(n=>n.ToString());
    var commaSeparatedElements4 = seedAndElements.Aggregate(
        func: (aggregate, element) => $"{aggregate}{element},");
    
    Console.WriteLine(commaSeparatedElements4);  //12,3,4,5,






## Procurar
    var persons = new[] {
        new { Name="Fizz", Job="Developer"},
        new { Name="Buzz", Job="Developer"},
        new { Name="Foo", Job="Astronaut"},
        new { Name="Bar", Job="Astronaut"},
    };
    
    var groupedByJob = persons.ToLookup(p => p.Job);
    
    foreach(var theGroup in groupedByJob)
    {
        Console.WriteLine(
            "{0} are {1}s", 
            string.Join(",", theGroup.Select(g => g.Name).ToArray()),
            theGroup.Key);
    }
    
    //Fizz,Buzz are Developers
    //Foo,Bar are Astronauts

## Intersecção
    var numbers1to10 = new[] {1,2,3,4,5,6,7,8,9,10};
    var numbers5to15 = new[] {5,6,7,8,9,10,11,12,13,14,15};
    
    var numbers5to10 = numbers1to10.Intersect(numbers5to15);
    
    Console.WriteLine(string.Join(",", numbers5to10));

    //5,6,7,8,9,10

## Concatenar
    var numbers1to5 = new[] {1, 2, 3, 4, 5};
    var numbers4to8 = new[] {4, 5, 6, 7, 8};
    
    var numbers1to8 = numbers1to5.Concat(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,4,5,6,7,8

Observe que as duplicatas são mantidas no resultado. Se isso for indesejável, use `Union`.

## Tudo
    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersAreOdd = numbers.All(n => (n & 1) == 1);
    Console.WriteLine(allNumbersAreOdd); //False
    
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

Observe que o método `All` funciona verificando se o primeiro elemento é avaliado como `false` de acordo com o predicado. Portanto, o método retornará `true` para *qualquer* predicado caso o conjunto esteja vazio:

    var numbers = new int[0];
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

## Soma
    var numbers = new[] {1,2,3,4};
    
    var sumOfAllNumbers = numbers.Sum();
    Console.WriteLine(sumOfAllNumbers); //10

    var cities = new[] {
        new {Population = 1000},
        new {Population = 2500},
        new {Population = 4000}
    };
    
    var totalPopulation = cities.Sum(c => c.Population);
    Console.WriteLine(totalPopulation); //7500

## Sequência Igual
    var numbers = new[] {1,2,3,4,5};
    var sameNumbers = new[] {1,2,3,4,5};
    var sameNumbersInDifferentOrder = new[] {5,1,4,2,3};
    
    var equalIfSameOrder = numbers.SequenceEqual(sameNumbers);
    Console.WriteLine(equalIfSameOrder); //True
    
    var equalIfDifferentOrder = numbers.SequenceEqual(sameNumbersInDifferentOrder);
    Console.WriteLine(equalIfDifferentOrder); //False

## Min
    var numbers = new[] {1,2,3,4};
    
    var minNumber = numbers.Min();
    Console.WriteLine(minNumber); //1
    
    var cities = new[] {
        new {Population = 1000},
        new {Population = 2500},
        new {Population = 4000}
    };
    
    var minPopulation = cities.Min(c => c.Population);
    Console.WriteLine(minPopulation); //1000

## Distinto
    var numbers = new[] {1, 1, 2, 2, 3, 3, 4, 4, 5, 5};
    var distinctNumbers = numbers.Distinct();
    
    Console.WriteLine(string.Join(",", distinctNumbers));

    //1,2,3,4,5

## Contar
    IEnumerable<int> numbers = new[] {1,2,3,4,5,6,7,8,9,10};

    var numbersCount = numbers.Count();
    Console.WriteLine(numbersCount); //10
    
    var evenNumbersCount = numbers.Count(n => (n & 1) == 0);
    Console.WriteLine(evenNumbersCount); //5

## Elenco
`Cast` é diferente dos outros métodos de `Enumerable` porque é um método de extensão para `IEnumerable`, não para `IEnumerable<T>`. Assim, pode ser usado para converter instâncias do primeiro em instâncias do segundo.

Isso não compila, pois `ArrayList` não implementa `IEnumerable<T>`:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.First());

Isso funciona como esperado:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.Cast<int>().First()); //1

`Cast` **não** realiza conversões de conversão. O seguinte compila, mas lança `InvalidCastException` em tempo de execução:

    var numbers = new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Cast<decimal>().ToArray();
    
A maneira correta de realizar uma conversão de conversão em uma coleção é a seguinte:

    var numbers= new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Select(n => (decimal)n).ToArray();

## Selecione (mapa)
    var persons = new[] 
    {
        new {Id = 1, Name = "Foo"},
        new {Id = 2, Name = "Bar"},
        new {Id = 3, Name = "Fizz"},
        new {Id = 4, Name = "Buzz"}
    };
    
    var names = persons.Select(p => p.Name);
    Console.WriteLine(string.Join(",", names.ToArray()));

    //Foo,Bar,Fizz,Buzz

Este tipo de função é geralmente chamado de `map` em linguagens de programação funcionais.

## Ordenar por
    var persons = new[] 
    {
        new {Id = 1, Name = "Foo"},
        new {Id = 2, Name = "Bar"},
        new {Id = 3, Name = "Fizz"},
        new {Id = 4, Name = "Buzz"}
    };
    
    var personsSortedByName = persons.OrderBy(p => p.Name);
    
    Console.WriteLine(string.Join(",", personsSortedByName.Select(p => p.Id).ToArray()));

    //2,4,3,1

## Ordenar Por Descendente
    var persons = new[] 
    {
        new {Id = 1, Name = "Foo"},
        new {Id = 2, Name = "Bar"},
        new {Id = 3, Name = "Fizz"},
        new {Id = 4, Name = "Buzz"}
    };
    
    var personsSortedByNameDescending = persons.OrderByDescending(p => p.Name);
    
    Console.WriteLine(string.Join(",", personsSortedByNameDescending.Select(p => p.Id).ToArray()));

    //1,3,4,2

## Contém
    var numbers = new[] {1,2,3,4,5};
    Console.WriteLine(numbers.Contains(3)); //True
    Console.WriteLine(numbers.Contains(34)); //False

## Primeiro (localizar)
    var numbers = new[] {1,2,3,4,5};
    
    var firstNumber = numbers.First();
    Console.WriteLine(firstNumber); //1
    
    var firstEvenNumber = numbers.First(n => (n & 1) == 0);
    Console.WriteLine(firstEvenNumber); //2
    
O seguinte lança `InvalidOperationException` com a mensagem "Sequence contém nenhum elemento correspondente":

    var firstNegativeNumber = numbers.First(n => n < 0);

## Solteiro
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.Single();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.Single(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

O seguinte lança `InvalidOperationException`, pois há mais de um elemento na sequência:

    var theOnlyNumberInNumbers = numbers.Single();
    var theOnlyNegativeNumber = numbers.Single(n => n < 0);

## Último
    var numbers = new[] {1,2,3,4,5};
    
    var lastNumber = numbers.Last();
    Console.WriteLine(lastNumber); //5
    
    var lastEvenNumber = numbers.Last(n => (n & 1) == 0);
    Console.WriteLine(lastEvenNumber); //4
    
O seguinte lança `InvalidOperationException`:

    var lastNegativeNumber = numbers.Last(n => n < 0);

## LastOrDefault
    var numbers = new[] {1,2,3,4,5};
    
    var lastNumber = numbers.LastOrDefault();
    Console.WriteLine(lastNumber); //5
    
    var lastEvenNumber = numbers.LastOrDefault(n => (n & 1) == 0);
    Console.WriteLine(lastEvenNumber); //4
    
    var lastNegativeNumber = numbers.LastOrDefault(n => n < 0);
    Console.WriteLine(lastNegativeNumber); //0

    var words = new[] { "one", "two", "three", "four", "five" };

    var lastWord = words.LastOrDefault();
    Console.WriteLine(lastWord); // five

    var lastLongWord = words.LastOrDefault(w => w.Length > 4);
    Console.WriteLine(lastLongWord); // three

    var lastMissingWord = words.LastOrDefault(w => w.Length > 5);
    Console.WriteLine(lastMissingWord); // null

## ÚnicoOrPadrão
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.SingleOrDefault();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.SingleOrDefault(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

    var theOnlyNegativeNumber = numbers.SingleOrDefault(n => n < 0);
    Console.WriteLine(theOnlyNegativeNumber);  //0

O seguinte lança `InvalidOperationException`:

    var theOnlyNumberInNumbers = numbers.SingleOrDefault();

## FirstOrDefault
    var numbers = new[] {1,2,3,4,5};
    
    var firstNumber = numbers.FirstOrDefault();
    Console.WriteLine(firstNumber); //1
    
    var firstEvenNumber = numbers.FirstOrDefault(n => (n & 1) == 0);
    Console.WriteLine(firstEvenNumber); //2
    
    var firstNegativeNumber = numbers.FirstOrDefault(n => n < 0);
    Console.WriteLine(firstNegativeNumber); //0

    var words = new[] { "one", "two", "three", "four", "five" };

    var firstWord = words.FirstOrDefault();
    Console.WriteLine(firstWord); // one

    var firstLongWord = words.FirstOrDefault(w => w.Length > 3);
    Console.WriteLine(firstLongWord); // three

    var firstMissingWord = words.FirstOrDefault(w => w.Length > 5);
    Console.WriteLine(firstMissingWord); // null

## Pular
Skip enumerará os primeiros N itens sem devolvê-los.
Uma vez que o número de item N+1 é alcançado, Skip começa a retornar cada item enumerado:


    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersExceptFirstTwo = numbers.Skip(2);
    Console.WriteLine(string.Join(",", allNumbersExceptFirstTwo.ToArray()));

    //3,4,5

## Leva
Este método pega os primeiros `n` elementos de um enumerável.

    var numbers = new[] {1,2,3,4,5};
    
    var threeFirstNumbers = numbers.Take(3);
    Console.WriteLine(string.Join(",", threeFirstNumbers.ToArray()));

    //1,2,3

## Marcha ré
    var numbers = new[] {1,2,3,4,5};
    var reversed = numbers.Reverse();

    Console.WriteLine(string.Join(",", reversed.ToArray()));

    //5,4,3,2,1

## OfType
    var mixed = new object[] {1,"Foo",2,"Bar",3,"Fizz",4,"Buzz"};
    var numbers = mixed.OfType<int>();

    Console.WriteLine(string.Join(",", numbers.ToArray()));

    //1,2,3,4

## Máx.
    var numbers = new[] {1,2,3,4};
    
    var maxNumber = numbers.Max();
    Console.WriteLine(maxNumber); //4
    
    var cities = new[] {
        new {Population = 1000},
        new {Population = 2500},
        new {Population = 4000}
    };
    
    var maxPopulation = cities.Max(c => c.Population);
    Console.WriteLine(maxPopulation); //4000

## Média

    var numbers = new[] {1,2,3,4};
    
    var averageNumber = numbers.Average();
    Console.WriteLine(averageNumber); 
    // 2,5

Este método calcula a média de enumeráveis ​​de números.

    var cities = new[] {
        new {Population = 1000},
        new {Population = 2000},
        new {Population = 4000}
    };
    
    var averagePopulation = cities.Average(c => c.Population);
    Console.WriteLine(averagePopulation);
    // 2333,33

Este método calcula a média de enumeráveis ​​usando a função delegada.

## Agrupar por
    var persons = new[] {
        new { Name="Fizz", Job="Developer"},
        new { Name="Buzz", Job="Developer"},
        new { Name="Foo", Job="Astronaut"},
        new { Name="Bar", Job="Astronaut"},
    };
    
    var groupedByJob = persons.GroupBy(p => p.Job);
    
    foreach(var theGroup in groupedByJob)
    {
        Console.WriteLine(
            "{0} are {1}s", 
            string.Join(",", theGroup.Select(g => g.Name).ToArray()),
            theGroup.Key);
    }

    //Fizz,Buzz are Developers
    //Foo,Bar are Astronauts

Agrupe as faturas por país, gerando um novo objeto com o número de registro, total pago e média paga

    var a = db.Invoices.GroupBy(i => i.Country)
              .Select(g => new { Country = g.Key,
                                 Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });

                             
                             
Se queremos apenas os totais, nenhum grupo

    var a = db.Invoices.GroupBy(i => 1)
              .Select(g => new { Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });
                             
Se precisarmos de várias contagens

    var a = db.Invoices.GroupBy(g => 1)
              .Select(g => new { High = g.Count(i => i.Paid >= 1000),
                                 Low = g.Count(i => i.Paid < 1000),
                                 Sum = g.Sum(i => i.Paid) });


## Para Dicionário
Retorna um novo dicionário da fonte `IEnumerable` usando a função keySelector fornecida para determinar as chaves. Irá lançar um `ArgumentException` se keySelector não for injetivo (retorna um valor único para cada membro da coleção de origem.) Existem sobrecargas que permitem especificar o valor a ser armazenado, bem como a chave.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
    };

Especificar apenas uma função de seletor de chave criará um `Dictionary<TKey,TVal>` com `TKey` o tipo de retorno do seletor de chave, `TVal` o tipo de objeto original e o objeto original como o valor armazenado.

    var personsById = persons.ToDictionary(p => p.Id);
    // personsById is a Dictionary<int,object>

    Console.WriteLine(personsById[1].Name); //Fizz
    Console.WriteLine(personsById[2].Name); //Buzz

Especificar uma função seletora de valor também criará um `Dictionary<TKey,TVal>` com `TKey` ainda o tipo de retorno do seletor de chave, mas `TVal` agora o tipo de retorno da função seletora de valor e o valor retornado como o valor armazenado.
    
    var namesById = persons.ToDictionary(p => p.Id, p => p.Name);
    //namesById is a Dictionary<int,string>

    Console.WriteLine(namesById[3]); //Foo
    Console.WriteLine(namesById[4]); //Bar

Como dito acima, as chaves retornadas pelo seletor de chaves devem ser únicas. O seguinte lançará uma exceção.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
        new { Name="Oops", Id=4}
    };

    var willThrowException = persons.ToDictionary(p => p.Id)

Se uma chave exclusiva não puder ser fornecida para a coleção de origem, considere usar ToLookup. Na superfície, ToLookup se comporta de maneira semelhante a ToDictionary, no entanto, na pesquisa resultante, cada chave é emparelhada com uma coleção de valores com chaves correspondentes.

## União
    var numbers1to5 = new[] {1,2,3,4,5};
    var numbers4to8 = new[] {4,5,6,7,8};
    
    var numbers1to8 = numbers1to5.Union(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,6,7,8

Observe que as duplicatas são removidas do resultado. Se isso for indesejável, use `Concat` em vez disso.

## ParaArray
    var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
    var someNumbers = numbers.Where(n => n < 6);
    
    Console.WriteLine(someNumbers.GetType().Name);
    //WhereArrayIterator`1
    
    var someNumbersArray = someNumbers.ToArray();
    
    Console.WriteLine(someNumbersArray.GetType().Name);
    //Int32[]

## Listar
    var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
    var someNumbers = numbers.Where(n => n < 6);
    
    Console.WriteLine(someNumbers.GetType().Name);
    //WhereArrayIterator`1
    
    var someNumbersList = someNumbers.ToList();
    
    Console.WriteLine(
        someNumbersList.GetType().Name + " - " +
        someNumbersList.GetType().GetGenericArguments()[0].Name);
    //List`1 - Int32

## ElementoAt
    var names = new[] {"Foo","Bar","Fizz","Buzz"};
    
    var thirdName = names.ElementAt(2);
    Console.WriteLine(thirdName); //Fizz
    
    //The following throws ArgumentOutOfRangeException

    var minusOnethName = names.ElementAt(-1);
    var fifthName = names.ElementAt(4);

## ElementAtOrDefault
    var names = new[] {"Foo","Bar","Fizz","Buzz"};
    
    var thirdName = names.ElementAtOrDefault(2);
    Console.WriteLine(thirdName); //Fizz
    
    var minusOnethName = names.ElementAtOrDefault(-1);
    Console.WriteLine(minusOnethName); //null
    
    var fifthName = names.ElementAtOrDefault(4);
    Console.WriteLine(fifthName); //null

## SkipWhile
    var numbers = new[] {2,4,6,8,1,3,5,7};
    
    var oddNumbers = numbers.SkipWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", oddNumbers.ToArray()));

    //1,3,5,7

## Tome Enquanto
    var numbers = new[] {2,4,6,1,3,5,7,8};
    
    var evenNumbers = numbers.TakeWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", evenNumbers.ToArray()));

    //2,4,6

## DefaultIfEmpty
    var numbers = new[] {2,4,6,8,1,3,5,7};
    
    var numbersOrDefault = numbers.DefaultIfEmpty();
    Console.WriteLine(numbers.SequenceEqual(numbersOrDefault)); //True
    
    var noNumbers = new int[0];
    
    var noNumbersOrDefault = noNumbers.DefaultIfEmpty();
    Console.WriteLine(noNumbersOrDefault.Count()); //1
    Console.WriteLine(noNumbersOrDefault.Single()); //0
    
    var noNumbersOrExplicitDefault = noNumbers.DefaultIfEmpty(34);
    Console.WriteLine(noNumbersOrExplicitDefault.Count()); //1
    Console.WriteLine(noNumbersOrExplicitDefault.Single()); //34

## Juntar
    class Developer
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
    
    class Project
    {
        public int DeveloperId { get; set; }
        public string Name { get; set; }
    }

    var developers = new[] {
        new Developer {
            Id = 1,
            Name = "Foobuzz"
        },
        new Developer {
            Id = 2,
            Name = "Barfizz"
        }
    };
    
    var projects = new[] {
        new Project {
            DeveloperId = 1,
            Name = "Hello World 3D"
        },
        new Project {
            DeveloperId = 1,
            Name = "Super Fizzbuzz Maker"
        },
        new Project {
            DeveloperId = 2,
            Name = "Citizen Kane - The action game"
        },
        new Project {
            DeveloperId = 2,
            Name = "Pro Pong 2016"
        }
    };
    
    var denormalized = developers.Join(
        inner: projects,
        outerKeySelector: dev => dev.Id,
        innerKeySelector: proj => proj.DeveloperId,
        resultSelector: 
            (dev, proj) => new {
                ProjectName = proj.Name,
                DeveloperName = dev.Name});
        
    foreach(var item in denormalized)
    {
        Console.WriteLine("{0} by {1}", item.ProjectName, item.DeveloperName);
    }

    //Hello World 3D by Foobuzz
    //Super Fizzbuzz Maker by Foobuzz
    //Citizen Kane - The action game by Barfizz
    //Pro Pong 2016 by Barfizz

## Vazio
Para criar um IEnumerable vazio de int:

    IEnumerable<int> emptyList = Enumerable.Empty<int>(); 

Este IEnumerable<T> vazio é armazenado em cache para cada Tipo T, para que:

    Enumerable.Empty<decimal>() == Enumerable.Empty<decimal>(); // This is True
    Enumerable.Empty<int>() == Enumerable.Empty<decimal>();     // This is False




## EntãoPor
`ThenBy` só pode ser usado após uma cláusula `OrderBy` permitindo ordenar usando vários critérios

    var persons = new[] 
    {
        new {Id = 1, Name = "Foo", Order = 1},
        new {Id = 1, Name = "FooTwo", Order = 2},
        new {Id = 2, Name = "Bar", Order = 2},
        new {Id = 2, Name = "BarTwo", Order = 1},
        new {Id = 3, Name = "Fizz", Order = 2},
        new {Id = 3, Name = "FizzTwo", Order = 1},  
    };
    
    var personsSortedByName = persons.OrderBy(p => p.Id).ThenBy(p => p.Order);
    
    Console.WriteLine(string.Join(",", personsSortedByName.Select(p => p.Name)));
    //This will display : 
    //Foo,FooTwo,BarTwo,Bar,FizzTwo,Fizz

     

## Variar
Os dois parâmetros para `Range` são o *primeiro* número e a *contagem* de elementos a serem produzidos (não o último número).

    // prints 1,2,3,4,5,6,7,8,9,10
    Console.WriteLine(string.Join(",", Enumerable.Range(1, 10)));

    // prints 10,11,12,13,14
    Console.WriteLine(string.Join(",", Enumerable.Range(10, 5)));




## Junção Externa Esquerda
    class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set; }
    }

    class Pet
    {
        public string Name { get; set; }
        public Person Owner { get; set; }
    }

    public static void Main(string[] args)
    {
        var magnus = new Person { FirstName = "Magnus", LastName = "Hedlund" };
        var terry = new Person { FirstName = "Terry", LastName = "Adams" };

        var barley = new Pet { Name = "Barley", Owner = terry };

        var people = new[] { magnus, terry };
        var pets = new[] { barley };

        var query =
            from person in people
            join pet in pets on person equals pet.Owner into gj
            from subpet in gj.DefaultIfEmpty()
            select new
            {
                person.FirstName,
                PetName = subpet?.Name ?? "-" // Use - if he has no pet
            };

        foreach (var p in query)
            Console.WriteLine($"{p.FirstName}: {p.PetName}");
    }

## Repetir
`Enumerable.Repeat` gera uma sequência de um valor repetido. Neste exemplo, ele gera "Hello" 4 vezes.
        
    var repeats = Enumerable.Repeat("Hello", 4);
       
    foreach (var item in repeats)
    {
        Console.WriteLine(item);
    }

    /* output:
        Hello
        Hello
        Hello
        Hello
    */



