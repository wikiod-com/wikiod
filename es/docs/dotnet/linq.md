---
title: "LINQ"
slug: "linq"
draft: false
images: []
weight: 9322
type: docs
toc: true
---

LINQ (Language Integrated Query) es una expresión que recupera datos de una fuente de datos. LINQ simplifica esta situación al ofrecer un modelo consistente para trabajar con datos en varios tipos de fuentes y formatos de datos. En una consulta LINQ, siempre está trabajando con objetos. Utiliza los mismos patrones de codificación básicos para consultar y transformar datos en documentos XML, bases de datos SQL, conjuntos de datos ADO.NET, colecciones .NET y cualquier otro formato para el que esté disponible un proveedor. LINQ se puede utilizar en C# y VB.

## Sintaxis
- Public static TSource Aggregate\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TSource, TSource\> func)
- public static TAccumulate Aggregate\<TSource, TAccumulate\>(this IEnumerable\<TSource\> source, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func)
- Public Static TResult Aggregate\<TSource, TAccumulate, TResult\>(this IEnumerable\<TSource\> source, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func, Func\<TAccumulate, TResult\> resultSelector)
- booleano estático público All\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- Public static Boolean Any\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static Boolean Any\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<TSource\> AsEnumerable\<TSource\>(esta fuente IEnumerable\<TSource\>)
- Promedio decimal estático público (esta fuente IEnumerable\<Decimal\>)
- Promedio doble estático público (esta fuente IEnumerable\<Double\>)
- Promedio doble estático público (esta fuente IEnumerable\<Int32\>)
- Promedio doble estático público (esta fuente IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Average (esta fuente IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Average (esta fuente IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Double\> Average(esta fuente IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Double\> Average (esta fuente IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Average (esta fuente IEnumerable\<Nullable\<Single\>\>)
- Promedio único estático público (esta fuente IEnumerable\<Single\>)
- Public static Decimal Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Decimal\> selector)
- Promedio doble estático público\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Double\> selector)
- Promedio doble estático público\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int32\> selector)
- Promedio doble estático público\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int64\> selector)
- public static Nullable\<Decimal\> Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> selector)
- public static Nullable\<Double\> Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> selector)
- public static Nullable\<Double\> Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> selector)
- public static Nullable\<Double\> Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> selector)
- public static Nullable\<Single\> Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> selector)
- Public static Single Average\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Single\> selector)
- public static IEnumerable\<TResult\> Cast\<TResult\>(esta fuente IEnumerable)
- public static IEnumerable\<TSource\> Concat\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo)
- Booleano estático público Contiene\<TSource\>(esta fuente IEnumerable\<TSource\>, valor TSource)
- Booleano estático público Contiene\<TSource\>(esta fuente IEnumerable\<TSource\>, valor TSource, comparador IEqualityComparer\<TSource\>)
- public static Int32 Count\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static Int32 Count\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(esta fuente IEnumerable\<TSource\>, TSource defaultValue)
- public static IEnumerable\<TSource\> Distinct\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static IEnumerable\<TSource\> Distinct\<TSource\>(esta fuente IEnumerable\<TSource\>, comparador IEqualityComparer\<TSource\>)
- TSource ElementAt\<TSource\> estático público (esta fuente IEnumerable\<TSource\>, índice Int32)
- TSource ElementAtOrDefault\<TSource\> estático público (esta fuente IEnumerable\<TSource\>, índice Int32)
- public static IEnumerable\<TResult\> Empty\<TResult\>()
- public static IEnumerable\<TSource\> Except\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Except\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- TSource público estático First\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource First\<TSource\>(this IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static TSource FirstOrDefault\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource FirstOrDefault\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource , TElemento\> selector de elementos)
- public static IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResultado\> selectorresultado)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> selector de resultado)
- public static IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> outside,IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outsideKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector)
- public static IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(este IEnumerable\<TOuter\> exterior, IEnumerable\<TInner\> interior, Func\<TOuter, TKey\> outsideKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Intersect\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> outside, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outsideKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(this IEnumerable\<TOuter\> outside, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> outsideKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector, IEqualityComparer\<TKey\> comparador)
- public static TSource Last\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource Last\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static TSource LastOrDefault\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource LastOrDefault\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static Int64 LongCount\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static Int64 LongCount\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- Máximo decimal estático público (esta fuente IEnumerable\<Decimal\>)
- Double Max estático público (esta fuente IEnumerable\<Double\>)
- public static Int32 Max (esta fuente IEnumerable\<Int32\>)
- Int64 Max público estático (esta fuente IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Max(esta fuente IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Max(esta fuente IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Int32\> Max(esta fuente IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Int64\> Max(esta fuente IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Max(esta fuente IEnumerable\<Nullable\<Single\>\>)
- Max individual estático público (esta fuente IEnumerable\<Single\>)
- public static TSource Max\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static Decimal Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Decimal\> selector)
- público estático Double Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Double\> selector)
- public static Int32 Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int32\> selector)
- public static Int64 Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int64\> selector)
- public static Nullable\<Decimal\> Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> selector)
- public static Nullable\<Double\> Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> selector)
- public static Nullable\<Int32\> Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> selector)
- public static Nullable\<Int64\> Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> selector)
- public static Nullable\<Single\> Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> selector)
- público estático Single Max\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Single\> selector)
- público estático TResult Max\<TSource, TResult\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TResult\> selector)
- Min decimal estático público (esta fuente IEnumerable\<Decimal\>)
- Double Min estático público (esta fuente IEnumerable\<Double\>)
- Int32 Min estático público (esta fuente IEnumerable\<Int32\>)
- Int64 Min estático público (esta fuente IEnumerable\<Int64\>)
- Nullable\<Decimal\> Min estático público (esta fuente IEnumerable\<Nullable\<Decimal\>\>)
- Nullable\<Double\> Min estático público (esta fuente IEnumerable\<Nullable\<Double\>\>)
- Nullable\<Int32\> Min estático público (esta fuente IEnumerable\<Nullable\<Int32\>\>)
- Public static Nullable\<Int64\> Min(esta fuente IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Min(esta fuente IEnumerable\<Nullable\<Single\>\>)
- Min único estático público (esta fuente IEnumerable\<Single\>)
- pública estática TSource Min\<TSource\>(esta fuente IEnumerable\<TSource\>)
- Public static Decimal Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Decimal\> selector)
- Double Min\<TSource\> público estático (esta fuente IEnumerable\<TSource\>, Func\<TSource, Double\> selector)
- public static Int32 Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int32\> selector)
- pública estática Int64 Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int64\> selector)
- public static Nullable\<Decimal\> Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> selector)
- public static Nullable\<Double\> Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> selector)
- public static Nullable\<Int32\> Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> selector)
- public static Nullable\<Int64\> Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> selector)
- public static Nullable\<Single\> Min\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> selector)
- Min\<TSource\> estático público único (esta fuente IEnumerable\<TSource\>, Func\<TSource, Single\> selector)
- público estático TResult Min\<TSource, TResult\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TResult\> selector)
- public static IEnumerable\<TResult\> OfType\<TResult\>(esta fuente IEnumerable)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparer)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparer)
- Rango de IEnumerable\<Int32\> estático público (inicio de Int32, recuento de Int32)
- public static IEnumerable\<TResult\> Repeat\<TResult\>(elemento TResult, recuento Int32)
- public static IEnumerable\<TSource\> Reverse\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static IEnumerable\<TResult\> Select\<TSource, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, TResult\> selector)
- public static IEnumerable\<TResult\> Select\<TSource, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, Int32, TResult\> selector)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, IEnumerable\<TResult\>\> selector)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, Int32, IEnumerable\<TResult\>\> selector)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource, TCollection , TResultado\> selectorresultado)
- public static IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(this IEnumerable\<TSource\> source, Func\<TSource, Int32, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource , TColección, TResultado\> selector de resultado)
- Public static Boolean SequenceEqual\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo)
- Public static Boolean SequenceEqual\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- pública estática TSource Single\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource Single\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static TSource SingleOrDefault\<TSource\>(esta fuente IEnumerable\<TSource\>)
- public static TSource SingleOrDefault\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Boolean\> predicado)
- public static IEnumerable\<TSource\> Skip\<TSource\>(this IEnumerable\<TSource\> source, Int32 count)
- public static IEnumerable\<TSource\> SkipWhile\<TSource\>(this IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<TSource\> SkipWhile\<TSource\>(this IEnumerable\<TSource\> source, Func\<TSource, Int32, Boolean\> predicate)
- Suma decimal estática pública (esta fuente IEnumerable\<Decimal\>)
- Suma doble estática pública (esta fuente IEnumerable\<Double\>)
- public static Int32 Sum (esta fuente IEnumerable\<Int32\>)
- Public static Int64 Sum (esta fuente IEnumerable\<Int64\>)
- public static Nullable\<Decimal\> Sum (esta fuente IEnumerable\<Nullable\<Decimal\>\>)
- public static Nullable\<Double\> Sum (esta fuente IEnumerable\<Nullable\<Double\>\>)
- public static Nullable\<Int32\> Sum (esta fuente IEnumerable\<Nullable\<Int32\>\>)
- public static Nullable\<Int64\> Sum (esta fuente IEnumerable\<Nullable\<Int64\>\>)
- public static Nullable\<Single\> Sum (esta fuente IEnumerable\<Nullable\<Single\>\>)
- Suma única pública estática (esta fuente IEnumerable\<Single\>)
- Public static Decimal Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Decimal\> selector)
- Public static Double Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Double\> selector)
- public static Int32 Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int32\> selector)
- public static Int64 Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Int64\> selector)
- public static Nullable\<Decimal\> Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Decimal\>\> selector)
- public static Nullable\<Double\> Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Double\>\> selector)
- public static Nullable\<Int32\> Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int32\>\> selector)
- public static Nullable\<Int64\> Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Int64\>\> selector)
- public static Nullable\<Single\> Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Nullable\<Single\>\> selector)
- Public static Single Sum\<TSource\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, Single\> selector)
- public static IEnumerable\<TSource\> Take\<TSource\>(this IEnumerable\<TSource\> source, Int32 count)
- public static IEnumerable\<TSource\> TakeWhile\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<TSource\> TakeWhile\<TSource\>(este IEnumerable\<TSource\> source, Func\<TSource, Int32, Boolean\> predicate)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(this IOrderedEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(this IOrderedEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparer)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(this IOrderedEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- public static IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(esta fuente IOrderedEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> comparador)
- public static TSource[] ToArray\<TSource\>(esta fuente IEnumerable\<TSource\>)
- Public static Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- Diccionario estático público\<TKey, TSource\> ToDictionary\<TSource, TKey\>(esta fuente IEnumerable\<TSource\>, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparador)
- Public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- Public static Dictionary\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparador)
- Public static List\<TSource\> ToList\<TSource\>(esta fuente IEnumerable\<TSource\>)
- ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector)
- ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> comparer)
- ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(this IEnumerable\<TSource\> source, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> comparador)
- public static IEnumerable\<TSource\> Union\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo)
- public static IEnumerable\<TSource\> Union\<TSource\>(este IEnumerable\<TSource\> primero, IEnumerable\<TSource\> segundo, IEqualityComparer\<TSource\> comparador)
- public static IEnumerable\<TSource\> Where\<TSource\>(this IEnumerable\<TSource\> source, Func\<TSource, Boolean\> predicate)
- public static IEnumerable\<TSource\> Where\<TSource\>(this IEnumerable\<TSource\> source, Func\<TSource, Int32, Boolean\> predicate)
- public static IEnumerable\<TResult\> Zip\<TFirst, TSecond, TResult\>(this IEnumerable\<TFirst\> first, IEnumerable\<TSecond\> second, Func\<TFirst, TSegend, TResult\> resultSelector)

* Véase también [LINQ][1].

Los métodos integrados de LINQ son métodos de extensión para la interfaz `IEnumerable<T>` que residen en la clase `System.Linq.Enumerable` en el ensamblado `System.Core`. Están disponibles en .NET Framework 3.5 y versiones posteriores.

LINQ permite la modificación, transformación y combinación simples de varios 'IEnumerable' utilizando una sintaxis similar a una consulta o funcional.

Si bien los métodos estándar de LINQ pueden funcionar en cualquier `IEnumerable<T>`, incluidas las matrices simples y `List<T>`s, también se pueden usar en objetos de base de datos, donde el conjunto de expresiones LINQ se puede transformar en muchos casos. a SQL si el objeto de datos lo admite. Consulte [LINQ to SQL](https://msdn.microsoft.com/en-us/library/bb425822.aspx).

Para los métodos que comparan objetos (como `Contains` y `Except`), se usa `IEquatable<T>.Equals` si el tipo T de la colección implementa esa interfaz. De lo contrario, se utilizan los estándares `Equals` y `GetHashCode` del tipo (posiblemente anulados de las implementaciones predeterminadas de `Object`). También hay sobrecargas para estos métodos que permiten especificar un `IEqualityComparer<T>` personalizado.

Para los métodos `...OrDefault`, se usa `default(T)` para generar valores predeterminados.

Referencia oficial: [clase enumerable](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx)

## Evaluación perezosa
Prácticamente todas las consultas que devuelven `IEnumerable<T>` no se evalúan inmediatamente; en cambio, la lógica se retrasa hasta que se itera la consulta. Una implicación es que cada vez que alguien itera sobre un `IEnumerable<T>` creado a partir de una de estas consultas, por ejemplo, `.Where()`, se repite la lógica de consulta completa. Si el predicado es de ejecución prolongada, esto puede ser una causa de problemas de rendimiento.

Una solución simple (cuando conoce o puede controlar el tamaño aproximado de la secuencia resultante) es almacenar en búfer los resultados usando `.ToArray()` o `.ToList()`. `.ToDictionary()` o `.ToLookup()` pueden cumplir el mismo rol. Por supuesto, también se puede iterar sobre toda la secuencia y almacenar en búfer los elementos de acuerdo con otra lógica personalizada.

## `ToArray()` o `ToList()`?

Tanto `.ToArray()` como `.ToList()` recorren todos los elementos de una secuencia `IEnumerable<T>` y guardan los resultados en una colección almacenada en la memoria. Use las siguientes pautas para determinar cuál elegir:

* Algunas API pueden requerir una `T[]` o una `List<T>`.
* `.ToList()` generalmente se ejecuta más rápido y genera menos basura que `.ToArray()`, porque este último debe copiar todos los elementos en una nueva colección de tamaño fijo una vez más que el primero, en casi todos los casos.
* Los elementos se pueden agregar o quitar de la `List<T>` devuelta por `.ToList()`, mientras que la `T[]` devuelta por `.ToArray()` permanece en un tamaño fijo a lo largo de su vida útil. En otras palabras, `List<T>` es mutable y `T[]` es inmutable.
* La `T[]` devuelta por `.ToArray()` usa menos memoria que la `List<T>` devuelta por `.ToList()`, por lo que si el resultado se almacenará durante mucho tiempo, prefiera `.ToArray()`. Llamar a `List<T>.TrimExcess()` haría que la diferencia de memoria fuera estrictamente académica, a costa de eliminar la ventaja de velocidad relativa de `.ToList()`.


[1]: https://www.wikiod.com/es/linq/empezando-con-linq

## SelectMany (mapa plano)
[`Enumerable.Select`][1] devuelve un elemento de salida para cada elemento de entrada.
Mientras que [`Enumerable.SelectMany`][2] produce un número variable de elementos de salida para cada elemento de entrada. Esto significa que la secuencia de salida puede contener más o menos elementos que los que había en la secuencia de entrada.

[`Expresiones lambda`][3] pasadas a `Enumerable.Select` deben devolver un único elemento. Las expresiones Lambda pasadas a `Enumerable.SelectMany` deben generar una secuencia secundaria. Esta secuencia secundaria puede contener un número variable de elementos para cada elemento de la secuencia de entrada.

**Ejemplo**
   
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

**Producción:**
>1,2,3,4,5,6

[Ver demostración][4]

`Enumerable.SelectMany` también se puede lograr con una consulta basada en la sintaxis usando dos cláusulas `from` consecutivas:

    var allInvoicesFromAllCustomers
        = from customer in customers
          from invoice in customer.Invoices
          select invoice;


[1]: https://msdn.microsoft.com/en-us/library/bb548891(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/bb534336(v=vs.100).aspx
[3]: https://www.wikiod.com/es/docs/c%23/46/lambda-expresiones
[4]: https://dotnetfiddle.net/XKGtBr

## Dónde (filtro)
Este método devuelve un IEnumerable con todos los elementos que cumplen con la expresión lambda

**Ejemplo**

    var personNames = new[] 
    {
        "Foo", "Bar", "Fizz", "Buzz"
    };
    
    var namesStartingWithF = personNames.Where(p => p.StartsWith("F"));
    Console.WriteLine(string.Join(",", namesStartingWithF));

**Producción:**

>Foo,Fizz

[Ver demostración][1]


[1]: https://dotnetfiddle.net/nTbZI0

## Ningún
Devuelve `true` si la colección tiene elementos que cumplen la condición en la expresión lambda:
   

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


## Unirse al grupo
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

## Excepto
    var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var evenNumbersBetweenSixAndFourteen = new[] { 6, 8, 10, 12 };

    var result = numbers.Except(evenNumbersBetweenSixAndFourteen);

    Console.WriteLine(string.Join(",", result));

    //1, 2, 3, 4, 5, 7, 9

## Cremallera
<!-- si versión <.NET> [gte 4.0] -->

    var tens = new[] {10,20,30,40,50};
    var units = new[] {1,2,3,4,5};
    
    var sums = tens.Zip(units, (first, second) => first + second);
    
    Console.WriteLine(string.Join(",", sums));

    //11,22,33,44,55

<!-- versión final si -->

## Agregado (pliegue)
Generando un nuevo objeto en cada paso:

    var elements = new[] {1,2,3,4,5};
    
    var commaSeparatedElements = elements.Aggregate(
        seed: "",
        func: (aggregate, element) => $"{aggregate}{element},");
        
    Console.WriteLine(commaSeparatedElements);  //1,2,3,4,5,
    
Usando el mismo objeto en todos los pasos:

    var commaSeparatedElements2 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"));
        
    Console.WriteLine(commaSeparatedElements2.ToString());  //1,2,3,4,5,

Usando un selector de resultados:

    var commaSeparatedElements3 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"),
        resultSelector: (seed) => seed.ToString());
    Console.WriteLine(commaSeparatedElements3);  //1,2,3,4,5,

Si se omite una semilla, el primer elemento se convierte en la semilla:

    var seedAndElements = elements.Select(n=>n.ToString());
    var commaSeparatedElements4 = seedAndElements.Aggregate(
        func: (aggregate, element) => $"{aggregate}{element},");
    
    Console.WriteLine(commaSeparatedElements4);  //12,3,4,5,






## Para buscar
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

## Intersección
    var numbers1to10 = new[] {1,2,3,4,5,6,7,8,9,10};
    var numbers5to15 = new[] {5,6,7,8,9,10,11,12,13,14,15};
    
    var numbers5to10 = numbers1to10.Intersect(numbers5to15);
    
    Console.WriteLine(string.Join(",", numbers5to10));

    //5,6,7,8,9,10

## Conectar
    var numbers1to5 = new[] {1, 2, 3, 4, 5};
    var numbers4to8 = new[] {4, 5, 6, 7, 8};
    
    var numbers1to8 = numbers1to5.Concat(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,4,5,6,7,8

Tenga en cuenta que los duplicados se mantienen en el resultado. Si esto no es deseable, use `Union` en su lugar.

## Todos
    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersAreOdd = numbers.All(n => (n & 1) == 1);
    Console.WriteLine(allNumbersAreOdd); //False
    
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

Tenga en cuenta que el método `Todos` funciona comprobando que el primer elemento se evalúe como `falso` según el predicado. Por lo tanto, el método devolverá `verdadero` para *cualquier* predicado en el caso de que el conjunto esté vacío:

    var numbers = new int[0];
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

## Suma
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

## Secuencia Igual
    var numbers = new[] {1,2,3,4,5};
    var sameNumbers = new[] {1,2,3,4,5};
    var sameNumbersInDifferentOrder = new[] {5,1,4,2,3};
    
    var equalIfSameOrder = numbers.SequenceEqual(sameNumbers);
    Console.WriteLine(equalIfSameOrder); //True
    
    var equalIfDifferentOrder = numbers.SequenceEqual(sameNumbersInDifferentOrder);
    Console.WriteLine(equalIfDifferentOrder); //False

## Minuto
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

## Emitir
`Cast` es diferente de los otros métodos de `Enumerable` en que es un método de extensión para `IEnumerable`, no para `IEnumerable<T>`. Por lo tanto, se puede utilizar para convertir instancias del primero en instancias del último.

Esto no compila ya que `ArrayList` no implementa `IEnumerable<T>`:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.First());

Esto funciona como se esperaba:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.Cast<int>().First()); //1

`Cast` **no** realiza conversiones. Lo siguiente compila pero lanza `InvalidCastException` en tiempo de ejecución:

    var numbers = new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Cast<decimal>().ToArray();
    
La forma correcta de realizar una conversión de conversión a una colección es la siguiente:

    var numbers= new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Select(n => (decimal)n).ToArray();

## Seleccionar (mapa)
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

Este tipo de función suele llamarse `mapa` en los lenguajes de programación funcional.

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

## OrdenDescendente
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

## Contiene
    var numbers = new[] {1,2,3,4,5};
    Console.WriteLine(numbers.Contains(3)); //True
    Console.WriteLine(numbers.Contains(34)); //False

## Primero (buscar)
    var numbers = new[] {1,2,3,4,5};
    
    var firstNumber = numbers.First();
    Console.WriteLine(firstNumber); //1
    
    var firstEvenNumber = numbers.First(n => (n & 1) == 0);
    Console.WriteLine(firstEvenNumber); //2
    
Lo siguiente arroja `InvalidOperationException` con el mensaje "La secuencia no contiene ningún elemento coincidente":

    var firstNegativeNumber = numbers.First(n => n < 0);

## Único
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.Single();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.Single(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

Lo siguiente lanza `InvalidOperationException` ya que hay más de un elemento en la secuencia:

    var theOnlyNumberInNumbers = numbers.Single();
    var theOnlyNegativeNumber = numbers.Single(n => n < 0);

## Ultimo
    var numbers = new[] {1,2,3,4,5};
    
    var lastNumber = numbers.Last();
    Console.WriteLine(lastNumber); //5
    
    var lastEvenNumber = numbers.Last(n => (n & 1) == 0);
    Console.WriteLine(lastEvenNumber); //4
    
Lo siguiente lanza `InvalidOperationException`:

    var lastNegativeNumber = numbers.Last(n => n < 0);

## último o predeterminado
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

## Único o Predeterminado
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.SingleOrDefault();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.SingleOrDefault(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

    var theOnlyNegativeNumber = numbers.SingleOrDefault(n => n < 0);
    Console.WriteLine(theOnlyNegativeNumber);  //0

Lo siguiente lanza `InvalidOperationException`:

    var theOnlyNumberInNumbers = numbers.SingleOrDefault();

## Primero o Predeterminado
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

## Saltar
Skip enumerará los primeros N elementos sin devolverlos.
Una vez que se alcanza el número de artículo N+1, Skip comienza a devolver todos los artículos enumerados:


    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersExceptFirstTwo = numbers.Skip(2);
    Console.WriteLine(string.Join(",", allNumbersExceptFirstTwo.ToArray()));

    //3,4,5

## Tomar
Este método toma los primeros elementos `n` de un enumerable.

    var numbers = new[] {1,2,3,4,5};
    
    var threeFirstNumbers = numbers.Take(3);
    Console.WriteLine(string.Join(",", threeFirstNumbers.ToArray()));

    //1,2,3

## Reverso
    var numbers = new[] {1,2,3,4,5};
    var reversed = numbers.Reverse();

    Console.WriteLine(string.Join(",", reversed.ToArray()));

    //5,4,3,2,1

## de tipo
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

## Promedio

    var numbers = new[] {1,2,3,4};
    
    var averageNumber = numbers.Average();
    Console.WriteLine(averageNumber); 
    // 2,5

Este método calcula el promedio de enumerables de números.

    var cities = new[] {
        new {Population = 1000},
        new {Population = 2000},
        new {Population = 4000}
    };
    
    var averagePopulation = cities.Average(c => c.Population);
    Console.WriteLine(averagePopulation);
    // 2333,33

Este método calcula el promedio de enumerables usando la función delegada.

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

Agrupar facturas por país, generando un nuevo objeto con el número de registro, total pagado y promedio pagado

    var a = db.Invoices.GroupBy(i => i.Country)
              .Select(g => new { Country = g.Key,
                                 Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });

                             
                             
Si queremos solo los totales, no hay grupo

    var a = db.Invoices.GroupBy(i => 1)
              .Select(g => new { Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });
                             
Si necesitamos varias cuentas

    var a = db.Invoices.GroupBy(g => 1)
              .Select(g => new { High = g.Count(i => i.Paid >= 1000),
                                 Low = g.Count(i => i.Paid < 1000),
                                 Sum = g.Sum(i => i.Paid) });


## AlDiccionario
Devuelve un nuevo diccionario de la fuente `IEnumerable` utilizando la función keySelector proporcionada para determinar las claves. Lanzará una `ArgumentException` si keySelector no es inyectable (devuelve un valor único para cada miembro de la colección de origen). Hay sobrecargas que permiten especificar el valor que se almacenará, así como la clave.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
    };

Especificar solo una función de selector de clave creará un `Diccionario<TKey,TVal>` con `TKey` el Tipo de retorno del selector de clave, `TVal` el Tipo de objeto original y el objeto original como el valor almacenado.

    var personsById = persons.ToDictionary(p => p.Id);
    // personsById is a Dictionary<int,object>

    Console.WriteLine(personsById[1].Name); //Fizz
    Console.WriteLine(personsById[2].Name); //Buzz

Especificar una función de selector de valor también creará un `Dictionary<TKey,TVal>` con `TKey` todavía el tipo de retorno del selector de clave, pero `TVal` ahora es el tipo de retorno de la función de selector de valor, y el valor devuelto como el valor almacenado.
    
    var namesById = persons.ToDictionary(p => p.Id, p => p.Name);
    //namesById is a Dictionary<int,string>

    Console.WriteLine(namesById[3]); //Foo
    Console.WriteLine(namesById[4]); //Bar

Como se indicó anteriormente, las claves devueltas por el selector de claves deben ser únicas. Lo siguiente lanzará una excepción.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
        new { Name="Oops", Id=4}
    };

    var willThrowException = persons.ToDictionary(p => p.Id)

Si no se puede proporcionar una clave única para la colección de origen, considere usar ToLookup en su lugar. En la superficie, ToLookup se comporta de manera similar a ToDictionary; sin embargo, en la búsqueda resultante, cada clave se empareja con una colección de valores con claves coincidentes.

## Unión
    var numbers1to5 = new[] {1,2,3,4,5};
    var numbers4to8 = new[] {4,5,6,7,8};
    
    var numbers1to8 = numbers1to5.Union(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,6,7,8

Tenga en cuenta que los duplicados se eliminan del resultado. Si esto no es deseable, use `Concat` en su lugar.

## A la matriz
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

## Elemento en
    var names = new[] {"Foo","Bar","Fizz","Buzz"};
    
    var thirdName = names.ElementAt(2);
    Console.WriteLine(thirdName); //Fizz
    
    //The following throws ArgumentOutOfRangeException

    var minusOnethName = names.ElementAt(-1);
    var fifthName = names.ElementAt(4);

## Elemento en o por defecto
    var names = new[] {"Foo","Bar","Fizz","Buzz"};
    
    var thirdName = names.ElementAtOrDefault(2);
    Console.WriteLine(thirdName); //Fizz
    
    var minusOnethName = names.ElementAtOrDefault(-1);
    Console.WriteLine(minusOnethName); //null
    
    var fifthName = names.ElementAtOrDefault(4);
    Console.WriteLine(fifthName); //null

## Saltar Mientras
    var numbers = new[] {2,4,6,8,1,3,5,7};
    
    var oddNumbers = numbers.SkipWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", oddNumbers.ToArray()));

    //1,3,5,7

## TomarMientras
    var numbers = new[] {2,4,6,1,3,5,7,8};
    
    var evenNumbers = numbers.TakeWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", evenNumbers.ToArray()));

    //2,4,6

## Predeterminado si está vacío
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

## Unirse
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

## Vacío
Para crear un IEnumerable vacío de int:

    IEnumerable<int> emptyList = Enumerable.Empty<int>(); 

Este IEnumerable<T> vacío se almacena en caché para cada Tipo T, de modo que:

    Enumerable.Empty<decimal>() == Enumerable.Empty<decimal>(); // This is True
    Enumerable.Empty<int>() == Enumerable.Empty<decimal>();     // This is False




## Entonces por
`ThenBy` solo se puede usar después de una cláusula `OrderBy` que permite ordenar usando múltiples criterios

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

     

## Rango
Los dos parámetros de `Rango` son el *primer* número y el *recuento* de elementos a producir (no el último número).

    // prints 1,2,3,4,5,6,7,8,9,10
    Console.WriteLine(string.Join(",", Enumerable.Range(1, 10)));

    // prints 10,11,12,13,14
    Console.WriteLine(string.Join(",", Enumerable.Range(10, 5)));




## Izquierda combinación externa
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
`Enumerable.Repeat` genera una secuencia de un valor repetido. En este ejemplo genera "Hola" 4 veces.
        
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



