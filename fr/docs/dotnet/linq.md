---
title: "LINQ"
slug: "linq"
draft: false
images: []
weight: 9322
type: docs
toc: true
---

LINQ (Language Integrated Query) est une expression qui récupère des données à partir d'une source de données. LINQ simplifie cette situation en offrant un modèle cohérent pour travailler avec des données sur différents types de sources et de formats de données. Dans une requête LINQ, vous travaillez toujours avec des objets. Vous utilisez les mêmes modèles de codage de base pour interroger et transformer des données dans des documents XML, des bases de données SQL, des ensembles de données ADO.NET, des collections .NET et tout autre format pour lequel un fournisseur est disponible. LINQ peut être utilisé en C# et VB.

## Syntaxe
- public static TSource Aggregate\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, TSource, TSource\> func)
- public static TAccumulate Aggregate\<TSource, TAccumulate\>(cette source IEnumerable\<TSource\>, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func)
- public static TResult Aggregate\<TSource, TAccumulate, TResult\>(this IEnumerable\<TSource\> source, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func, Func\<TAccumulate, TResult\> resultSelector)
- public static Boolean All\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static Boolean Any\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Boolean Any\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public statique IEnumerable\<TSource\> AsEnumerable\<TSource\>(cette source IEnumerable\<TSource\>)
- Moyenne décimale statique publique (cette source IEnumerable\<Decimal\>)
- double moyenne publique statique (cette source IEnumerable\<Double\>)
- double moyenne statique publique (cette source IEnumerable\<Int32\>)
- double moyenne statique publique (cette source IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Average (cette source IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Moyenne (cette source IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Double\> Moyenne (cette source IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Double\> Moyenne (cette source IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Average (cette source IEnumerable\<Nullable\<Single\>\>)
- Moyenne unique statique publique (cette source IEnumerable\<Single\>)
- public static Decimal Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Decimal\> sélecteur)
- Public static Double Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Double\> sélecteur)
- Public static Double Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, sélecteur Int32\>)
- Public static Double Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, sélecteur Int64\>)
- public static Nullable\<Decimal\> Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> sélecteur)
- public static Nullable\<Double\> Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> sélecteur)
- public static Nullable\<Double\> Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> sélecteur)
- public static Nullable\<Double\> Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> sélecteur)
- public static Nullable\<Single\> Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> sélecteur)
- public static Single Average\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Single\> sélecteur)
- public statique IEnumerable\<TResult\> Cast\<TResult\>(cette source IEnumerable)
- public statique IEnumerable\<TSource\> Concat\<TSource\>(cet IEnumerable\<TSource\> en premier, IEnumerable\<TSource\> en second)
- public static Boolean Contient\<TSource\>(cette source IEnumerable\<TSource\>, valeur TSource)
- public static Boolean Contient\<TSource\>(cette source IEnumerable\<TSource\>, la valeur TSource, le comparateur IEqualityComparer\<TSource\>)
- Public static Int32 Count\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Int32 Count\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public statique IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(cette source IEnumerable\<TSource\>)
- public statique IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(cette source IEnumerable\<TSource\>, TSource defaultValue)
- public statique IEnumerable\<TSource\> Distinct\<TSource\>(cette source IEnumerable\<TSource\>)
- public static IEnumerable\<TSource\> Distinct\<TSource\>(cette source IEnumerable\<TSource\>, IEqualityComparer\<TSource\> comparateur)
- public static TSource ElementAt\<TSource\>(cette source IEnumerable\<TSource\>, index Int32)
- public static TSource ElementAtOrDefault\<TSource\>(cette source IEnumerable\<TSource\>, index Int32)
- public statique IEnumerable\<TResult\> Empty\<TResult\>()
- public static IEnumerable\<TSource\> Except\<TSource\>(cet IEnumerable\<TSource\> en premier, IEnumerable\<TSource\> en second)
- public static IEnumerable\<TSource\> Except\<TSource\>(ce IEnumerable\<TSource\> premier, IEnumerable\<TSource\> second, IEqualityComparer\<TSource\> comparateur)
- public static TSource First\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource First\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static TSource FirstOrDefault\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource FirstOrDefault\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public statique IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public statique IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparateur)
- public statique IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector)
- public statique IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector, IEqualityComparer\<TKey\> comparateur)
- public statique IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> resultSelector)
- public statique IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> resultSelector, IEqualityComparer\<TKey\> comparateur)
- public statique IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector)
- public statique IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparateur)
- public static IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> external,IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector)
- public statique IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> external, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparateur)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(cet IEnumerable\<TSource\> en premier, IEnumerable\<TSource\> en second)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(ce IEnumerable\<TSource\> premier, IEnumerable\<TSource\> second, IEqualityComparer\<TSource\> comparateur)
- public statique IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> external, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector)
- public statique IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> external, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outerKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector, IEqualityComparer\<TKey\> comparateur)
- public static TSource Last\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource Last\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static TSource LastOrDefault\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource LastOrDefault\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static Int64 LongCount\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Int64 LongCount\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static Decimal Max (cette source IEnumerable\<Decimal\>)
- Double Max public statique (cette source IEnumerable\<Double\>)
- public statique Int32 Max (cette source IEnumerable\<Int32\>)
- public statique Int64 Max (cette source IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Max (cette source IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Max(cette source IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Int32\> Max (cette source IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Int64\> Max (cette source IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Max (cette source IEnumerable\<Nullable\<Single\>\>)
- Single Max public statique (cette source IEnumerable\<Single\>)
- public static TSource Max\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Decimal Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Decimal\> sélecteur)
- public statique Double Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Double\> sélecteur)
- public static Int32 Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32\> sélecteur)
- public static Int64 Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int64\> sélecteur)
- public static Nullable\<Decimal\> Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> sélecteur)
- public static Nullable\<Double\> Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> sélecteur)
- public static Nullable\<Int32\> Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> sélecteur)
- public static Nullable\<Int64\> Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> sélecteur)
- public static Nullable\<Single\> Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> sélecteur)
- public static Single Max\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Single\> sélecteur)
- public static TResult Max\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TResult\> sélecteur)
- public static Decimal Min (cette source IEnumerable\<Decimal\>)
- public statique Double Min (cette source IEnumerable\<Double\>)
- public statique Int32 Min (cette source IEnumerable\<Int32\>)
- public statique Int64 Min (cette source IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Min (cette source IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Min(cette source IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Int32\> Min (cette source IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Int64\> Min (cette source IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Min (cette source IEnumerable\<Nullable\<Single\>\>)
- Public static Single Min (cette source IEnumerable\<Single\>)
- public static TSource Min\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Decimal Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Decimal\> sélecteur)
- public static Double Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Double\> sélecteur)
- public static Int32 Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32\> sélecteur)
- public static Int64 Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int64\> sélecteur)
- public static Nullable\<Decimal\> Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> sélecteur)
- public static Nullable\<Double\> Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> sélecteur)
- public static Nullable\<Int32\> Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> sélecteur)
- public static Nullable\<Int64\> Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> sélecteur)
- public static Nullable\<Single\> Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> sélecteur)
- public static Single Min\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Single\> sélecteur)
- public static TResult Min\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TResult\> sélecteur)
- public statique IEnumerable\<TResult\> OfType\<TResult\>(cette source IEnumerable)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparateur)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparateur)
- public static IEnumerable\<Int32\> Range(Int32 start, Int32 count)
- public statique IEnumerable\<TResult\> Repeat\<TResult\>(élément TResult, nombre Int32)
- public statique IEnumerable\<TSource\> Reverse\<TSource\>(cette source IEnumerable\<TSource\>)
- public statique IEnumerable\<TResult\> Select\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, TResult\> sélecteur)
- public static IEnumerable\<TResult\> Select\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, TResult\> sélecteur)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, IEnumerable\<TResult\>\> sélecteur)
- public statique IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, IEnumerable\<TResult\>\> sélecteur)
- public statique IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource, TCollection , TResult\> resultSelector)
- public statique IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource , TCollection, TResult\> resultSelector)
- public static Boolean SequenceEqual\<TSource\>(ce IEnumerable\<TSource\> en premier, IEnumerable\<TSource\> en second)
- public static Boolean SequenceEqual\<TSource\>(ce IEnumerable\<TSource\> premier, IEnumerable\<TSource\> second, IEqualityComparer\<TSource\> comparateur)
- public static TSource Single\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource Single\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static TSource SingleOrDefault\<TSource\>(cette source IEnumerable\<TSource\>)
- public static TSource SingleOrDefault\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public statique IEnumerable\<TSource\> Skip\<TSource\>(cette source IEnumerable\<TSource\>, nombre Int32)
- public static IEnumerable\<TSource\> SkipWhile\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static IEnumerable\<TSource\> SkipWhile\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, Boolean\> prédicat)
- public static Decimal Sum (cette source IEnumerable\<Decimal\>)
- Double somme statique publique (cette source IEnumerable\<Double\>)
- Public static Int32 Sum (cette source IEnumerable\<Int32\>)
- Public static Int64 Sum (cette source IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Sum(cette source IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Sum(cette source IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Int32\> Sum(cette source IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Int64\> Sum(cette source IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Sum(cette source IEnumerable\<Nullable\<Single\>\>)
- Somme unique statique publique (cette source IEnumerable\<Single\>)
- public static Decimal Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Decimal\> sélecteur)
- Public static Double Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Double\> sélecteur)
- Public static Int32 Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32\> sélecteur)
- public static Int64 Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int64\> sélecteur)
- public static Nullable\<Decimal\> Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> sélecteur)
- public static Nullable\<Double\> Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> sélecteur)
- public static Nullable\<Int32\> Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> sélecteur)
- public static Nullable\<Int64\> Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> sélecteur)
- public static Nullable\<Single\> Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> sélecteur)
- Public static Single Sum\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Single\> sélecteur)
- public statique IEnumerable\<TSource\> Take\<TSource\>(cette source IEnumerable\<TSource\>, nombre Int32)
- public static IEnumerable\<TSource\> TakeWhile\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public statique IEnumerable\<TSource\> TakeWhile\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, Boolean\> prédicat)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(cette source IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(cette source IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparateur)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(cette source IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(cette source IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparateur)
- public static TSource[] ToArray\<TSource\>(cette source IEnumerable\<TSource\>)
- public static Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparateur)
- public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparateur)
- public static List\<TSource\> ToList\<TSource\>(cette source IEnumerable\<TSource\>)
- public static ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector)
- public static ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparateur)
- public static ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- public static ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(cette source IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparateur)
- public statique IEnumerable\<TSource\> Union\<TSource\>(cet IEnumerable\<TSource\> en premier, IEnumerable\<TSource\> en second)
- public static IEnumerable\<TSource\> Union\<TSource\>(ce IEnumerable\<TSource\> premier, IEnumerable\<TSource\> second, IEqualityComparer\<TSource\> comparateur)
- public static IEnumerable\<TSource\> Where\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Boolean\> prédicat)
- public static IEnumerable\<TSource\> Where\<TSource\>(cette source IEnumerable\<TSource\>, Func\<TSource, Int32, Boolean\> prédicat)
- public static IEnumerable\<TResult\> Zip\<TFirst, TSecond, TResult\>(this IEnumerable\<TFirst\> first, IEnumerable\<TSecond\> second, Func\<TFirst, TSecond, TResult\> resultSelector)

* Voir aussi [LINQ][1].

Les méthodes intégrées LINQ sont des méthodes d'extension pour l'interface `IEnumerable<T>` qui résident dans la classe `System.Linq.Enumerable` dans l'assembly `System.Core`. Ils sont disponibles dans .NET Framework 3.5 et versions ultérieures.

LINQ permet une modification, une transformation et une combinaison simples de divers "IEnumerable" à l'aide d'une syntaxe de type requête ou fonctionnelle.

Bien que les méthodes LINQ standard puissent fonctionner sur n'importe quel `IEnumerable<T>`, y compris les tableaux simples et `List<T>`s, elles peuvent également être utilisées sur des objets de base de données, où l'ensemble des expressions LINQ peut être transformé dans de nombreux cas. à SQL si l'objet de données le prend en charge. Voir [LINQ to SQL](https://msdn.microsoft.com/en-us/library/bb425822.aspx).

Pour les méthodes qui comparent des objets (telles que `Contains` et `Except`), `IEquatable<T>.Equals` est utilisé si le type T de la collection implémente cette interface. Sinon, les `Equals` et `GetHashCode` standard du type (éventuellement remplacés par les implémentations `Object` par défaut) sont utilisés. Il existe également des surcharges pour ces méthodes qui permettent de spécifier un `IEqualityComparer<T>` personnalisé.

Pour les méthodes `...OrDefault`, `default(T)` est utilisé pour générer des valeurs par défaut.

Référence officielle : [Classe Enumerable](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx)

## Évaluation paresseuse
Pratiquement toutes les requêtes qui renvoient un `IEnumerable<T>` ne sont pas évaluées immédiatement ; à la place, la logique est retardée jusqu'à ce que la requête soit itérée. Une implication est que chaque fois que quelqu'un itère sur un `IEnumerable<T>` créé à partir de l'une de ces requêtes, par exemple, `.Where()`, la logique de requête complète est répétée. Si le prédicat est de longue durée, cela peut entraîner des problèmes de performances.

Une solution simple (lorsque vous connaissez ou pouvez contrôler la taille approximative de la séquence résultante) consiste à tamponner entièrement les résultats en utilisant `.ToArray()` ou `.ToList()`. `.ToDictionary()` ou `.ToLookup()` peuvent remplir le même rôle. On peut aussi, bien sûr, itérer sur toute la séquence et tamponner les éléments selon une autre logique personnalisée.

## `ToArray()` ou `ToList()` ?

`.ToArray()` et `.ToList()` parcourent tous les éléments d'une séquence `IEnumerable<T>` et enregistrent les résultats dans une collection stockée en mémoire. Utilisez les directives suivantes pour déterminer lequel choisir :

* Certaines API peuvent nécessiter un `T[]` ou un `List<T>`.
* `.ToList()` s'exécute généralement plus rapidement et génère moins de déchets que `.ToArray()`, car ce dernier doit copier tous les éléments dans une nouvelle collection de taille fixe une fois de plus que le premier, dans presque tous les cas.
* Des éléments peuvent être ajoutés ou supprimés de la `List<T>` renvoyée par `.ToList()`, tandis que le `T[]` renvoyé par `.ToArray()` conserve une taille fixe tout au long de sa durée de vie. En d'autres termes, `List<T>` est modifiable et `T[]` est immuable.
* Le `T[]` renvoyé par `.ToArray()` utilise moins de mémoire que le `List<T>` renvoyé par `.ToList()`, donc si le résultat doit être stocké pendant une longue période, préférez `.VersTableau()`. Appeler `List<T>.TrimExcess()` rendrait la différence de mémoire strictement académique, au prix de l'élimination de l'avantage de vitesse relative de `.ToList()`.


[1] : https://www.wikiod.com/fr/linq/demarrer-avec-linq

## SelectMany (carte plate)
[`Enumerable.Select`][1] renvoie un élément de sortie pour chaque élément d'entrée.
Alors que [`Enumerable.SelectMany`][2] produit un nombre variable d'éléments de sortie pour chaque élément d'entrée. Cela signifie que la séquence de sortie peut contenir plus ou moins d'éléments qu'il n'y en avait dans la séquence d'entrée.

Les [`expressions lambda`][3] transmises à `Enumerable.Select` doivent renvoyer un seul élément. Les expressions lambda transmises à `Enumerable.SelectMany` doivent produire une séquence enfant. Cette séquence enfant peut contenir un nombre variable d'éléments pour chaque élément de la séquence d'entrée.

**Exemple**
   
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

**Production:**
>1,2,3,4,5,6

[Voir la démo][4]

`Enumerable.SelectMany` peut également être obtenu avec une requête basée sur la syntaxe utilisant deux clauses `from` consécutives :

    var allInvoicesFromAllCustomers
        = from customer in customers
          from invoice in customer.Invoices
          select invoice;


[1] : https://msdn.microsoft.com/en-us/library/bb548891(v=vs.100).aspx
[2] : https://msdn.microsoft.com/en-us/library/bb534336(v=vs.100).aspx
[3] : https://www.wikiod.com/fr/docs/c%23/46/lambda-expressions
[4] : https://dotnetfiddle.net/XKGtBr

## Où (filtre)
Cette méthode renvoie un IEnumerable avec tous les éléments qui répondent à l'expression lambda

**Exemple**

    var personNames = new[] 
    {
        "Foo", "Bar", "Fizz", "Buzz"
    };
    
    var namesStartingWithF = personNames.Where(p => p.StartsWith("F"));
    Console.WriteLine(string.Join(",", namesStartingWithF));

**Production:**

> Foo, Fizz

[Voir la démo][1]


[1] : https://dotnetfiddle.net/nTbZI0

## N'importe quel
Renvoie "true" si la collection contient des éléments qui remplissent la condition dans l'expression lambda :
   

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


## GroupJoin
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

## À l'exception
    var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var evenNumbersBetweenSixAndFourteen = new[] { 6, 8, 10, 12 };

    var result = numbers.Except(evenNumbersBetweenSixAndFourteen);

    Console.WriteLine(string.Join(",", result));

    //1, 2, 3, 4, 5, 7, 9

## Zipper
<!-- si version <.NET> [gte 4.0] -->

    var tens = new[] {10,20,30,40,50};
    var units = new[] {1,2,3,4,5};
    
    var sums = tens.Zip(units, (first, second) => first + second);
    
    Console.WriteLine(string.Join(",", sums));

    //11,22,33,44,55

<!-- fin de version si -->

## Agrégat (pli)
Génération d'un nouvel objet à chaque étape :

    var elements = new[] {1,2,3,4,5};
    
    var commaSeparatedElements = elements.Aggregate(
        seed: "",
        func: (aggregate, element) => $"{aggregate}{element},");
        
    Console.WriteLine(commaSeparatedElements);  //1,2,3,4,5,
    
Utilisation du même objet à toutes les étapes :

    var commaSeparatedElements2 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"));
        
    Console.WriteLine(commaSeparatedElements2.ToString());  //1,2,3,4,5,

Utilisation d'un sélecteur de résultat :

    var commaSeparatedElements3 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"),
        resultSelector: (seed) => seed.ToString());
    Console.WriteLine(commaSeparatedElements3);  //1,2,3,4,5,

Si une graine est omise, le premier élément devient la graine :

    var seedAndElements = elements.Select(n=>n.ToString());
    var commaSeparatedElements4 = seedAndElements.Aggregate(
        func: (aggregate, element) => $"{aggregate}{element},");
    
    Console.WriteLine(commaSeparatedElements4);  //12,3,4,5,






## Pour rechercher
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

## Intersection
    var numbers1to10 = new[] {1,2,3,4,5,6,7,8,9,10};
    var numbers5to15 = new[] {5,6,7,8,9,10,11,12,13,14,15};
    
    var numbers5to10 = numbers1to10.Intersect(numbers5to15);
    
    Console.WriteLine(string.Join(",", numbers5to10));

    //5,6,7,8,9,10

## Concat
    var numbers1to5 = new[] {1, 2, 3, 4, 5};
    var numbers4to8 = new[] {4, 5, 6, 7, 8};
    
    var numbers1to8 = numbers1to5.Concat(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,4,5,6,7,8

Notez que les doublons sont conservés dans le résultat. Si cela n'est pas souhaitable, utilisez `Union` à la place.

## Tout
    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersAreOdd = numbers.All(n => (n & 1) == 1);
    Console.WriteLine(allNumbersAreOdd); //False
    
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

Notez que la méthode `All` fonctionne en vérifiant que le premier élément est évalué comme `false` selon le prédicat. Par conséquent, la méthode renverra `true` pour *tout* prédicat dans le cas où l'ensemble est vide :

    var numbers = new int[0];
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

## Somme
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

## SéquenceÉgal
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

## Distinct
    var numbers = new[] {1, 1, 2, 2, 3, 3, 4, 4, 5, 5};
    var distinctNumbers = numbers.Distinct();
    
    Console.WriteLine(string.Join(",", distinctNumbers));

    //1,2,3,4,5

## Compter
    IEnumerable<int> numbers = new[] {1,2,3,4,5,6,7,8,9,10};

    var numbersCount = numbers.Count();
    Console.WriteLine(numbersCount); //10
    
    var evenNumbersCount = numbers.Count(n => (n & 1) == 0);
    Console.WriteLine(evenNumbersCount); //5

## Moulage
`Cast` est différent des autres méthodes de `Enumerable` en ce sens qu'il s'agit d'une méthode d'extension pour `IEnumerable`, pas pour `IEnumerable<T>`. Ainsi, il peut être utilisé pour convertir des instances du premier en instances du second.

Cela ne compile pas puisque `ArrayList` n'implémente pas `IEnumerable<T>` :

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.First());

Cela fonctionne comme prévu :

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.Cast<int>().First()); //1

`Cast` n'effectue **pas** de conversions. Ce qui suit compile mais lève `InvalidCastException` lors de l'exécution :

    var numbers = new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Cast<decimal>().ToArray();
    
La bonne façon d'effectuer une conversion de distribution en collection est la suivante :

    var numbers= new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Select(n => (decimal)n).ToArray();

## Sélectionner (carte)
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

Ce type de fonction est généralement appelé `map` dans les langages de programmation fonctionnels.

## Commandé par
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

## OrderByDescending
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

## Contient
    var numbers = new[] {1,2,3,4,5};
    Console.WriteLine(numbers.Contains(3)); //True
    Console.WriteLine(numbers.Contains(34)); //False

## Premier (trouver)
    var numbers = new[] {1,2,3,4,5};
    
    var firstNumber = numbers.First();
    Console.WriteLine(firstNumber); //1
    
    var firstEvenNumber = numbers.First(n => (n & 1) == 0);
    Console.WriteLine(firstEvenNumber); //2
    
Ce qui suit lance `InvalidOperationException` avec le message "La séquence ne contient aucun élément correspondant":

    var firstNegativeNumber = numbers.First(n => n < 0);

## Seul
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.Single();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.Single(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

Ce qui suit lève `InvalidOperationException` car il y a plus d'un élément dans la séquence :

    var theOnlyNumberInNumbers = numbers.Single();
    var theOnlyNegativeNumber = numbers.Single(n => n < 0);

## Dernier
    var numbers = new[] {1,2,3,4,5};
    
    var lastNumber = numbers.Last();
    Console.WriteLine(lastNumber); //5
    
    var lastEvenNumber = numbers.Last(n => (n & 1) == 0);
    Console.WriteLine(lastEvenNumber); //4
    
Ce qui suit génère `InvalidOperationException` :

    var lastNegativeNumber = numbers.Last(n => n < 0);

## DernierOuDéfaut
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

## SingleOrDefault
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.SingleOrDefault();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.SingleOrDefault(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

    var theOnlyNegativeNumber = numbers.SingleOrDefault(n => n < 0);
    Console.WriteLine(theOnlyNegativeNumber);  //0

Ce qui suit génère `InvalidOperationException` :

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

## Sauter
Skip énumérera les N premiers éléments sans les retourner.
Une fois le numéro d'élément N+1 atteint, Skip commence à renvoyer chaque élément énuméré :


    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersExceptFirstTwo = numbers.Skip(2);
    Console.WriteLine(string.Join(",", allNumbersExceptFirstTwo.ToArray()));

    //3,4,5

## Prendre
Cette méthode prend les premiers `n` éléments d'un énumérable.

    var numbers = new[] {1,2,3,4,5};
    
    var threeFirstNumbers = numbers.Take(3);
    Console.WriteLine(string.Join(",", threeFirstNumbers.ToArray()));

    //1,2,3

## Inverse
    var numbers = new[] {1,2,3,4,5};
    var reversed = numbers.Reverse();

    Console.WriteLine(string.Join(",", reversed.ToArray()));

    //5,4,3,2,1

## DeType
    var mixed = new object[] {1,"Foo",2,"Bar",3,"Fizz",4,"Buzz"};
    var numbers = mixed.OfType<int>();

    Console.WriteLine(string.Join(",", numbers.ToArray()));

    //1,2,3,4

## Max
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

## Moyen

    var numbers = new[] {1,2,3,4};
    
    var averageNumber = numbers.Average();
    Console.WriteLine(averageNumber); 
    // 2,5

Cette méthode calcule la moyenne d'énumérables de nombres.

    var cities = new[] {
        new {Population = 1000},
        new {Population = 2000},
        new {Population = 4000}
    };
    
    var averagePopulation = cities.Average(c => c.Population);
    Console.WriteLine(averagePopulation);
    // 2333,33

Cette méthode calcule la moyenne des énumérables à l'aide de la fonction déléguée.

## Par groupe
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

Regrouper les factures par pays, en générant un nouvel objet avec le nombre d'enregistrements, le total payé et la moyenne payée

    var a = db.Invoices.GroupBy(i => i.Country)
              .Select(g => new { Country = g.Key,
                                 Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });

                             
                             
Si nous ne voulons que les totaux, aucun groupe

    var a = db.Invoices.GroupBy(i => 1)
              .Select(g => new { Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });
                             
Si nous avons besoin de plusieurs comptes

    var a = db.Invoices.GroupBy(g => 1)
              .Select(g => new { High = g.Count(i => i.Paid >= 1000),
                                 Low = g.Count(i => i.Paid < 1000),
                                 Sum = g.Sum(i => i.Paid) });


## AuDictionnaire
Renvoie un nouveau dictionnaire à partir de la source "IEnumerable" à l'aide de la fonction keySelector fournie pour déterminer les clés. Lèvera une `ArgumentException` si keySelector n'est pas injectif (renvoie une valeur unique pour chaque membre de la collection source.) Il existe des surcharges qui permettent de spécifier la valeur à stocker ainsi que la clé.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
    };

Spécifier uniquement une fonction de sélecteur de clé créera un `Dictionary<TKey,TVal>` avec `TKey` le type de retour du sélecteur de clé, `TVal` le type d'objet d'origine et l'objet d'origine comme valeur stockée.

    var personsById = persons.ToDictionary(p => p.Id);
    // personsById is a Dictionary<int,object>

    Console.WriteLine(personsById[1].Name); //Fizz
    Console.WriteLine(personsById[2].Name); //Buzz

Spécifier également une fonction de sélection de valeur créera un `Dictionary<TKey,TVal>` avec `TKey` toujours le type de retour du sélecteur de clé, mais `TVal` maintenant le type de retour de la fonction de sélection de valeur, et la valeur renvoyée comme la valeur stockée.
    
    var namesById = persons.ToDictionary(p => p.Id, p => p.Name);
    //namesById is a Dictionary<int,string>

    Console.WriteLine(namesById[3]); //Foo
    Console.WriteLine(namesById[4]); //Bar

Comme indiqué ci-dessus, les clés renvoyées par le sélecteur de clé doivent être uniques. Ce qui suit lèvera une exception.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
        new { Name="Oops", Id=4}
    };

    var willThrowException = persons.ToDictionary(p => p.Id)

Si une clé unique ne peut pas être donnée pour la collection source, envisagez d'utiliser ToLookup à la place. En surface, ToLookup se comporte de la même manière que ToDictionary, cependant, dans la recherche résultante, chaque clé est associée à une collection de valeurs avec des clés correspondantes.

## Syndicat
    var numbers1to5 = new[] {1,2,3,4,5};
    var numbers4to8 = new[] {4,5,6,7,8};
    
    var numbers1to8 = numbers1to5.Union(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,6,7,8

Notez que les doublons sont supprimés du résultat. Si cela n'est pas souhaitable, utilisez `Concat` à la place.

## VersTableau
    var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
    var someNumbers = numbers.Where(n => n < 6);
    
    Console.WriteLine(someNumbers.GetType().Name);
    //WhereArrayIterator`1
    
    var someNumbersArray = someNumbers.ToArray();
    
    Console.WriteLine(someNumbersArray.GetType().Name);
    //Int32[]

## Lister
    var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
    var someNumbers = numbers.Where(n => n < 6);
    
    Console.WriteLine(someNumbers.GetType().Name);
    //WhereArrayIterator`1
    
    var someNumbersList = someNumbers.ToList();
    
    Console.WriteLine(
        someNumbersList.GetType().Name + " - " +
        someNumbersList.GetType().GetGenericArguments()[0].Name);
    //List`1 - Int32

## ElementAt
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

## Sauter pendant
    var numbers = new[] {2,4,6,8,1,3,5,7};
    
    var oddNumbers = numbers.SkipWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", oddNumbers.ToArray()));

    //1,3,5,7

## PrendrePendant
    var numbers = new[] {2,4,6,1,3,5,7,8};
    
    var evenNumbers = numbers.TakeWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", evenNumbers.ToArray()));

    //2,4,6

## Par défautSiVide
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

## Rejoindre
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

## Vide
Pour créer un IEnumerable vide de int :

    IEnumerable<int> emptyList = Enumerable.Empty<int>(); 

Ce IEnumerable<T> vide est mis en cache pour chaque Type T, de sorte que :

    Enumerable.Empty<decimal>() == Enumerable.Empty<decimal>(); // This is True
    Enumerable.Empty<int>() == Enumerable.Empty<decimal>();     // This is False




## Puis par
`ThenBy` ne peut être utilisé qu'après une clause `OrderBy` permettant de commander en utilisant plusieurs critères

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

     

## Intervalle
Les deux paramètres de `Range` sont le *premier* nombre et le *nombre* d'éléments à produire (pas le dernier nombre).

    // prints 1,2,3,4,5,6,7,8,9,10
    Console.WriteLine(string.Join(",", Enumerable.Range(1, 10)));

    // prints 10,11,12,13,14
    Console.WriteLine(string.Join(",", Enumerable.Range(10, 5)));




## Jointure externe gauche
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

## Répéter
`Enumerable.Repeat` génère une séquence d'une valeur répétée. Dans cet exemple, il génère "Hello" 4 fois.
        
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



