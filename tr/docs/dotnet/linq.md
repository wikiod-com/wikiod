---
title: "LINQ"
slug: "linq"
draft: false
images: []
weight: 9322
type: docs
toc: true
---

LINQ (Dil Entegre Sorgu), bir veri kaynağından veri alan bir ifadedir. LINQ, çeşitli veri kaynakları ve biçimleri arasında verilerle çalışmak için tutarlı bir model sunarak bu durumu basitleştirir. Bir LINQ sorgusunda her zaman nesnelerle çalışırsınız. XML belgelerinde, SQL veritabanlarında, ADO.NET Veri Kümelerinde, .NET koleksiyonlarında ve bir sağlayıcının uygun olduğu diğer tüm biçimlerde verileri sorgulamak ve dönüştürmek için aynı temel kodlama modellerini kullanırsınız. LINQ, C# ve VB'de kullanılabilir.

## Sözdizimi
- genel statik TSource Aggregate\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TSource, TSource\> func)
- genel statik TAccumulate Aggregate\<TSource, TAccumulate\>(bu IEnumerable\<TSource\> kaynağı, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func)
- genel statik TResult Aggregate\<TSource, TAccumulate, TResult\>(bu IEnumerable\<TSource\> kaynağı, TAccumulate seed, Func\<TAccumulate, TSource, TAccumulate\> func, Func\<TAccumulate, TResult\> resultSelector)
- genel statik Boolean All\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik Boolean Any\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Boolean Any\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<TSource\> AsEnumerable\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Ondalık Ortalama(bu IEnumerable\<Decimal\> kaynağı)
- genel statik Çift Ortalama(bu IEnumerable\<Double\> kaynağı)
- genel statik Çift Ortalama(bu IEnumerable\<Int32\> kaynağı)
- genel statik Çift Ortalama(bu IEnumerable\<Int64\> kaynağı)
- public static Nullable\<Decimal\> Ortalama(bu IEnumerable\<Nullable\<Decimal\>\> kaynağı)
- public static Nullable\<Double\> Ortalama(bu IEnumerable\<Nullable\<Double\>\> kaynağı)
- public static Nullable\<Double\> Ortalama(bu IEnumerable\<Nullable\<Int32\>\> kaynağı)
- public static Nullable\<Double\> Ortalama(bu IEnumerable\<Nullable\<Int64\>\> kaynağı)
- public static Nullable\<Single\> Ortalama(bu IEnumerable\<Nullable\<Single\>\> kaynağı)
- genel statik Tek Ortalama(bu IEnumerable\<Single\> kaynağı)
- genel statik Ondalık Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Decimal\> seçici)
- genel statik Çift Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Double\> seçici)
- genel statik Çift Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32\> seçicisi)
- genel statik Çift Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int64\> seçicisi)
- public static Nullable\<Decimal\> Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Decimal\>\> seçici)
- public static Nullable\<Double\> Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Double\>\> seçici)
- public static Nullable\<Double\> Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int32\>\> seçici)
- public static Nullable\<Double\> Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int64\>\> seçici)
- public static Nullable\<Single\> Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Single\>\> seçici)
- genel statik Tek Ortalama\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Single\> seçici)
- genel statik IEnumerable\<TResult\> Cast\<TResult\>(bu IEnumerable kaynağı)
- genel statik IEnumerable\<TSource\> Concat\<TSource\>(bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> saniye)
- genel statik Boolean İçerir\<TSource\>(bu IEnumerable\<TSource\> kaynağı, TSource değeri)
- genel statik Boolean İçerir\<TSource\>(bu IEnumerable\<TSource\> kaynağı, TSource değeri, IEqualityComparer\<TSource\> karşılaştırıcısı)
- genel statik Int32 Count\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Int32 Count\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik IEnumerable\<TSource\> DefaultIfEmpty\<TSource\>(bu IEnumerable\<TSource\> kaynağı, TSource defaultValue)
- genel statik IEnumerable\<TSource\> Distinct\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik IEnumerable\<TSource\> Distinct\<TSource\>(bu IEnumerable\<TSource\> kaynağı, IEqualityComparer\<TSource\> karşılaştırıcısı)
- genel statik TSource ElementAt\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Int32 dizini)
- genel statik TSource ElementAtOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Int32 dizini)
- genel statik IEnumerable\<TResult\> Empty\<TResult\>()
- genel statik IEnumerable\<TSource\>\<TSource\> dışında (bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> saniye)
- genel statik IEnumerable\<TSource\>\<TSource\> dışında (bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> ikinci, IEqualityComparer\<TSource\> karşılaştırıcı)
- genel statik TSource First\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource First\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik TSource FirstOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource FirstOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik IEnumerable\<IGrouping\<TKey, TSource\>\> GroupBy\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector)
- genel statik IEnumerable\<IGrouping\<TKey, TElement\>\> GroupBy\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource , TElement\> elementSelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> sonuçSeçici)
- genel statik IEnumerable\<TResult\> GroupBy\<TSource, TKey, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TKey, IEnumerable\<TSource\> , TResult\> resultSelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector)
- genel statik IEnumerable\<TResult\> GroupBy\<TSource, TKey, TElement, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , Func\<TKey, IEnumerable\<TElement\>, TResult\> resultSelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(bu IEnumerable\<TOuter\> external,IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> externalKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector)
- genel statik IEnumerable\<TResult\> GroupJoin\<TOuter, TInner, TKey, TResult\>(bu IEnumerable\<TOuter\> dış, IEnumerable\<TInner\> iç, Func\<TOuter, TKey\> externalKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, IEnumerable\<TInner\>, TResult\> resultSelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<TSource\> Intersect\<TSource\>(bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> ikinci)
- genel statik IEnumerable\<TSource\> Intersect\<TSource\>(önce bu IEnumerable\<TSource\>, ikinci olarak IEnumerable\<TSource\>, IEqualityComparer\<TSource\> karşılaştırıcısı)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(bu IEnumerable\<TOuter\> external, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> externalKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector)
- public static IEnumerable\<TResult\> Join\<TOuter, TInner, TKey, TResult\>(bu IEnumerable\<TOuter\> external, IEnumerable\<TInner\> inner, Func\<TOuter, TKey\> externalKeySelector, Func \<TInner, TKey\> innerKeySelector, Func\<TOuter, TInner, TResult\> resultSelector, IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik TSource Last\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource Last\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik TSource LastOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource LastOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik Int64 LongCount\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Int64 LongCount\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik Decimal Max(bu IEnumerable\<Decimal\> kaynağı)
- genel statik Double Max(bu IEnumerable\<Double\> kaynağı)
- genel statik Int32 Max(bu IEnumerable\<Int32\> kaynağı)
- genel statik Int64 Max(bu IEnumerable\<Int64\> kaynağı)
- public static Nullable\<Decimal\> Max(bu IEnumerable\<Nullable\<Decimal\>\> kaynağı)
- public static Nullable\<Double\> Max(bu IEnumerable\<Nullable\<Double\>\> kaynağı)
- public static Nullable\<Int32\> Max(bu IEnumerable\<Nullable\<Int32\>\> kaynağı)
- public static Nullable\<Int64\> Max(bu IEnumerable\<Nullable\<Int64\>\> kaynağı)
- public static Nullable\<Single\> Max(bu IEnumerable\<Nullable\<Single\>\> kaynağı)
- genel statik Single Max(bu IEnumerable\<Single\> kaynağı)
- genel statik TSource Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Decimal Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Decimal\> seçici)
- genel statik Double Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Double\> seçici)
- genel statik Int32 Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32\> seçici)
- genel statik Int64 Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int64\> seçici)
- public static Nullable\<Decimal\> Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Decimal\>\> seçici)
- public static Nullable\<Double\> Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Double\>\> seçici)
- public static Nullable\<Int32\> Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int32\>\> seçici)
- public static Nullable\<Int64\> Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int64\>\> seçici)
- public static Nullable\<Single\> Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Single\>\> seçici)
- genel statik Single Max\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Single\> seçici)
- genel statik TResult Max\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TResult\> seçici)
- genel statik Ondalık Min (bu IEnumerable\<Decimal\> kaynağı)
- genel statik Double Min(bu IEnumerable\<Double\> kaynağı)
- genel statik Int32 Min(bu IEnumerable\<Int32\> kaynağı)
- genel statik Int64 Min(bu IEnumerable\<Int64\> kaynağı)
- public static Nullable\<Decimal\> Min(bu IEnumerable\<Nullable\<Decimal\>\> kaynağı)
- public static Nullable\<Double\> Min(bu IEnumerable\<Nullable\<Double\>\> kaynağı)
- public static Nullable\<Int32\> Min(bu IEnumerable\<Nullable\<Int32\>\> kaynağı)
- public static Nullable\<Int64\> Min(bu IEnumerable\<Nullable\<Int64\>\> kaynağı)
- public static Nullable\<Single\> Min(bu IEnumerable\<Nullable\<Single\>\> kaynağı)
- genel statik Tek Min (bu IEnumerable\<Single\> kaynağı)
- genel statik TSource Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Decimal Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Decimal\> seçici)
- genel statik Double Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Double\> seçici)
- genel statik Int32 Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32\> seçici)
- genel statik Int64 Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int64\> seçici)
- public static Nullable\<Decimal\> Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Decimal\>\> seçici)
- public static Nullable\<Double\> Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Double\>\> seçici)
- public static Nullable\<Int32\> Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int32\>\> seçici)
- public static Nullable\<Int64\> Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int64\>\> seçici)
- public static Nullable\<Single\> Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Single\>\> seçici)
- genel statik Tek Min\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Single\> seçici)
- genel statik TResult Min\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TResult\> seçici)
- genel statik IEnumerable\<TResult\> OfType\<TResult\>(bu IEnumerable kaynağı)
- genel statik IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik IOrderedEnumerable\<TSource\> OrderBy\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> karşılaştırıcısı)
- genel statik IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik IOrderedEnumerable\<TSource\> OrderByDescending\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> karşılaştırıcısı)
- genel statik IEnumerable\<Int32\> Aralık(Int32 başlangıç, Int32 sayısı)
- genel statik IEnumerable\<TResult\> Repeat\<TResult\>(TResult öğesi, Int32 sayısı)
- genel statik IEnumerable\<TSource\> Reverse\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik IEnumerable\<TResult\> Select\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TResult\> seçici)
- genel statik IEnumerable\<TResult\> Select\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, TResult\> seçici)
- genel statik IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, IEnumerable\<TResult\>\> seçici)
- genel statik IEnumerable\<TResult\> SelectMany\<TSource, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, IEnumerable\<TResult\>\> seçici)
- genel statik IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource, TCollection , TResult\> sonuçSeçici)
- genel statik IEnumerable\<TResult\> SelectMany\<TSource, TCollection, TResult\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, IEnumerable\<TCollection\>\> collectionSelector, Func\<TSource , TCollection, TResult\> resultSelector)
- genel statik Boolean SequenceEqual\<TSource\>(bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> saniye)
- genel statik Boolean SequenceEqual\<TSource\>(önce bu IEnumerable\<TSource\>, ikinci IEnumerable\<TSource\>, IEqualityComparer\<TSource\> karşılaştırıcı)
- genel statik TSource Single\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource Single\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik TSource SingleOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik TSource SingleOrDefault\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<TSource\> Skip\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Int32 sayısı)
- genel statik IEnumerable\<TSource\> SkipWhile\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<TSource\> SkipWhile\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, Boolean\> yüklemi)
- genel statik Ondalık Toplamı(bu IEnumerable\<Decimal\> kaynağı)
- genel statik Double Sum(bu IEnumerable\<Double\> kaynağı)
- genel statik Int32 Sum(bu IEnumerable\<Int32\> kaynağı)
- genel statik Int64 Sum(bu IEnumerable\<Int64\> kaynağı)
- public static Nullable\<Decimal\> Sum(bu IEnumerable\<Nullable\<Decimal\>\> kaynağı)
- public static Nullable\<Double\> Sum(bu IEnumerable\<Nullable\<Double\>\> kaynağı)
- public static Nullable\<Int32\> Sum(bu IEnumerable\<Nullable\<Int32\>\> kaynağı)
- public static Nullable\<Int64\> Sum(bu IEnumerable\<Nullable\<Int64\>\> kaynağı)
- public static Nullable\<Single\> Sum(bu IEnumerable\<Nullable\<Single\>\> kaynağı)
- genel statik Tekli Toplam(bu IEnumerable\<Single\> kaynağı)
- genel statik Decimal Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Decimal\> seçici)
- genel statik Double Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Double\> seçici)
- genel statik Int32 Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32\> seçici)
- genel statik Int64 Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int64\> seçici)
- public static Nullable\<Decimal\> Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Decimal\>\> seçici)
- public static Nullable\<Double\> Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Double\>\> seçici)
- public static Nullable\<Int32\> Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int32\>\> seçici)
- public static Nullable\<Int64\> Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Int64\>\> seçici)
- public static Nullable\<Single\> Sum\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Nullable\<Single\>\> seçici)
- genel statik Tekli Toplam\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Single\> seçici)
- genel statik IEnumerable\<TSource\> Take\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Int32 sayısı)
- genel statik IEnumerable\<TSource\> TakeWhile\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- genel statik IEnumerable\<TSource\> TakeWhile\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, Boolean\> yüklemi)
- genel statik IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(bu IOrderedEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik IOrderedEnumerable\<TSource\> ThenBy\<TSource, TKey\>(bu IOrderedEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> karşılaştırıcısı)
- genel statik IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(bu IOrderedEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik IOrderedEnumerable\<TSource\> ThenByDescending\<TSource, TKey\>(bu IOrderedEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IComparer\<TKey\> karşılaştırıcısı)
- genel statik TSource[] ToArray\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik Sözlük\<TKey, TSource\> ToDictionary\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik Dictionary\<TKey, TSource\> ToDictionary\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> karşılaştırıcısı)
- genel statik Sözlük\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- genel statik Sözlük\<TKey, TElement\> ToDictionary\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik Liste\<TSource\> ToList\<TSource\>(bu IEnumerable\<TSource\> kaynağı)
- genel statik ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector)
- genel statik ILookup\<TKey, TSource\> ToLookup\<TSource, TKey\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, IEqualityComparer\<TKey\> karşılaştırma aracı)
- genel statik ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector )
- genel statik ILookup\<TKey, TElement\> ToLookup\<TSource, TKey, TElement\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, TKey\> keySelector, Func\<TSource, TElement\> elementSelector , IEqualityComparer\<TKey\> karşılaştırıcı)
- genel statik IEnumerable\<TSource\> Union\<TSource\>(bu IEnumerable\<TSource\> önce, IEnumerable\<TSource\> saniye)
- genel statik IEnumerable\<TSource\> Union\<TSource\>(önce bu IEnumerable\<TSource\>, ikinci IEnumerable\<TSource\>, IEqualityComparer\<TSource\> karşılaştırıcısı)
- public static IEnumerable\<TSource\> Where\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Boolean\> yüklemi)
- public static IEnumerable\<TSource\> Where\<TSource\>(bu IEnumerable\<TSource\> kaynağı, Func\<TSource, Int32, Boolean\> yüklemi)
- genel statik IEnumerable\<TResult\> Zip\<TFirst, TSecond, TResult\>(bu IEnumerable\<TFirst\> önce, IEnumerable\<TSecond\> saniye, Func\<TFirst, TSecond, TResult\> resultSelector)

* Ayrıca bkz. [LINQ][1].

LINQ yerleşik yöntemleri, "System.Core" derlemesindeki "System.Linq.Enumerable" sınıfında yaşayan "IEnumerable<T>" arabirimi için genişletme yöntemleridir. .NET Framework 3.5 ve sonraki sürümlerinde kullanılabilirler.

LINQ, sorgu benzeri veya işlevsel bir sözdizimi kullanarak çeşitli IEnumerable'ların basit bir şekilde değiştirilmesine, dönüştürülmesine ve birleştirilmesine izin verir.

Standart LINQ yöntemleri, basit diziler ve 'List<T>'ler dahil olmak üzere herhangi bir 'IEnumerable<T>' üzerinde çalışabilirken, LINQ ifadelerinin birçok durumda dönüştürülebildiği veritabanı nesnelerinde de kullanılabilirler. veri nesnesi destekliyorsa SQL'e. Bkz. [LINQ to SQL](https://msdn.microsoft.com/en-us/library/bb425822.aspx).

Nesneleri karşılaştıran yöntemler için ("Contains" ve "Except" gibi), koleksiyonun T türü bu arabirimi uygularsa "IEquatable<T>.Equals" kullanılır. Aksi takdirde, türün standart "Equals" ve "GetHashCode"u (muhtemelen varsayılan "Object" uygulamalarından geçersiz kılınır) kullanılır. Bu yöntemler için özel bir "IEqualityComparer<T>" belirtmeye izin veren aşırı yüklemeler de vardır.

`...OrDefault` yöntemleri için, varsayılan değerleri oluşturmak için `default(T)` kullanılır.

Resmi referans: [Sayılandırılabilir sınıf](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx)

## Tembel Değerlendirme
Hemen hemen bir "IEnumerable<T>" döndüren her sorgu hemen değerlendirilmez; bunun yerine mantık, sorgu tekrarlanana kadar ertelenir. Bunun bir anlamı, birisinin bu sorgulardan birinden oluşturulan bir "IEnumerable<T>" üzerinde her yinelemede, örneğin ".Where()", tam sorgu mantığının tekrarlanmasıdır. Yüklem uzun süredir devam ediyorsa, bu performans sorunlarının bir nedeni olabilir.

Basit bir çözüm (sonuçlanan dizinin yaklaşık boyutunu bildiğiniz veya kontrol edebildiğiniz zaman), sonuçları '.ToArray()' veya '.ToList()' kullanarak tam olarak arabelleğe almaktır. `.ToDictionary()` veya `.ToLookup()` aynı rolü yerine getirebilir. Tabii ki, tüm diziyi yineleyebilir ve öğeleri diğer özel mantığa göre ara belleğe alabilir.

## `ToArray()` veya `ToList()`?

Hem ".ToArray()" hem de ".ToList()" bir "IEnumerable<T>" dizisinin tüm öğeleri arasında döngü yapar ve sonuçları bellekte depolanan bir koleksiyona kaydeder. Hangisini seçeceğinizi belirlemek için aşağıdaki yönergeleri kullanın:

* Bazı API'ler bir "T[]" veya "Liste<T>" gerektirebilir.
* `.ToList()` genellikle `.ToArray()` işlevinden daha hızlı çalışır ve daha az çöp üretir, çünkü ikincisi tüm öğeleri hemen hemen her durumda öncekinden bir kez daha yeni bir sabit boyutlu koleksiyona kopyalamalıdır.
* Öğeler, '.ToList()' tarafından döndürülen 'List<T>'ye eklenebilir veya buradan çıkarılabilir, oysa '.ToArray()' öğesinden döndürülen 'T[]', kullanım ömrü boyunca sabit bir boyutta kalır. Başka bir deyişle, 'List<T>' değişkendir ve 'T[]' değişmezdir.
* '.ToArray()' öğesinden döndürülen 'T[]', '.ToList()' öğesinden döndürülen 'List<T>'den daha az bellek kullanır, bu nedenle sonuç uzun süre saklanacaksa, tercih edin `.ToArray()`. 'List<T>.TrimExcess()' çağrılması, '.ToList()'in göreli hız avantajını ortadan kaldırma pahasına, bellek farkını kesinlikle akademik hale getirecektir.


[1]: https://www.wikiod.com/tr/linq/linqe-baslarken

## SelectMany (düz harita)
[`Enumerable.Select`][1], her giriş öğesi için bir çıkış öğesi döndürür.
Oysa [`Enumerable.SelectMany`][2], her giriş öğesi için değişken sayıda çıkış öğesi üretir. Bu, çıktı dizisinin girdi dizisindekinden daha fazla veya daha az eleman içerebileceği anlamına gelir.

[`Lambda ifadeleri`][3], `Enumerable.Select` öğesine geçirilen tek bir öğe döndürmelidir. "Enumerable.SelectMany" öğesine iletilen Lambda ifadeleri bir alt dizi oluşturmalıdır. Bu alt dizi, giriş dizisindeki her öğe için değişen sayıda öğe içerebilir.

**Örnek**
   
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

**Çıktı:**
>1,2,3,4,5,6

[Demoyu Görüntüle][4]

"Enumerable.SelectMany", iki ardışık "from" yan tümcesi kullanılarak sözdizimi tabanlı bir sorguyla da elde edilebilir:

    var allInvoicesFromAllCustomers
        = from customer in customers
          from invoice in customer.Invoices
          select invoice;


[1]: https://msdn.microsoft.com/en-us/library/bb548891(v=vs.100).aspx
[2]: https://msdn.microsoft.com/en-us/library/bb534336(v=vs.100).aspx
[3]: https://www.wikiod.com/tr/docs/c%23/46/lambda-expressions
[4]: https://dotnetfiddle.net/XKGtBr

## Nerede (filtre)
Bu yöntem, lambda ifadesini karşılayan tüm öğeleri içeren bir IEnumerable döndürür.

**Örnek**

    var personNames = new[] 
    {
        "Foo", "Bar", "Fizz", "Buzz"
    };
    
    var namesStartingWithF = personNames.Where(p => p.StartsWith("F"));
    Console.WriteLine(string.Join(",", namesStartingWithF));

**Çıktı:**

>Foo, Fizz

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/nTbZI0

## Hiç
Koleksiyonda lambda ifadesindeki koşulu karşılayan herhangi bir öğe varsa "true" değerini döndürür:
   

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


## Gruba Katıl
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

## Hariç
    var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    var evenNumbersBetweenSixAndFourteen = new[] { 6, 8, 10, 12 };

    var result = numbers.Except(evenNumbersBetweenSixAndFourteen);

    Console.WriteLine(string.Join(",", result));

    //1, 2, 3, 4, 5, 7, 9

## Posta kodu
<!-- eğer sürüm <.NET> [gte 4.0] -->

    var tens = new[] {10,20,30,40,50};
    var units = new[] {1,2,3,4,5};
    
    var sums = tens.Zip(units, (first, second) => first + second);
    
    Console.WriteLine(string.Join(",", sums));

    //11,22,33,44,55

<!-- eğer --> son sürüm

## Toplama (katlama)
Her adımda yeni bir nesne oluşturma:

    var elements = new[] {1,2,3,4,5};
    
    var commaSeparatedElements = elements.Aggregate(
        seed: "",
        func: (aggregate, element) => $"{aggregate}{element},");
        
    Console.WriteLine(commaSeparatedElements);  //1,2,3,4,5,
    
Tüm adımlarda aynı nesneyi kullanma:

    var commaSeparatedElements2 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"));
        
    Console.WriteLine(commaSeparatedElements2.ToString());  //1,2,3,4,5,

Bir sonuç seçici kullanma:

    var commaSeparatedElements3 = elements.Aggregate(
        seed: new StringBuilder(),
        func: (seed, element) => seed.Append($"{element},"),
        resultSelector: (seed) => seed.ToString());
    Console.WriteLine(commaSeparatedElements3);  //1,2,3,4,5,

Bir tohum atlanırsa, ilk eleman tohum olur:

    var seedAndElements = elements.Select(n=>n.ToString());
    var commaSeparatedElements4 = seedAndElements.Aggregate(
        func: (aggregate, element) => $"{aggregate}{element},");
    
    Console.WriteLine(commaSeparatedElements4);  //12,3,4,5,






## Aramak
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

## Kesişim
    var numbers1to10 = new[] {1,2,3,4,5,6,7,8,9,10};
    var numbers5to15 = new[] {5,6,7,8,9,10,11,12,13,14,15};
    
    var numbers5to10 = numbers1to10.Intersect(numbers5to15);
    
    Console.WriteLine(string.Join(",", numbers5to10));

    //5,6,7,8,9,10

## Uyum
    var numbers1to5 = new[] {1, 2, 3, 4, 5};
    var numbers4to8 = new[] {4, 5, 6, 7, 8};
    
    var numbers1to8 = numbers1to5.Concat(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,4,5,6,7,8

Yinelenenlerin sonuçta tutulduğunu unutmayın. Bu istenmiyorsa, bunun yerine 'Birlik' kullanın.

## Herşey
    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersAreOdd = numbers.All(n => (n & 1) == 1);
    Console.WriteLine(allNumbersAreOdd); //False
    
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

'Tümü' yönteminin, yüklemeye göre 'yanlış' olarak değerlendirilecek ilk öğeyi kontrol ederek çalıştığını unutmayın. Bu nedenle, kümenin boş olması durumunda yöntem *herhangi bir* yüklem için "true" değerini döndürür:

    var numbers = new int[0];
    var allNumbersArePositive = numbers.All(n => n > 0);
    Console.WriteLine(allNumbersArePositive); //True

## Toplam
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

## SıraEşit
    var numbers = new[] {1,2,3,4,5};
    var sameNumbers = new[] {1,2,3,4,5};
    var sameNumbersInDifferentOrder = new[] {5,1,4,2,3};
    
    var equalIfSameOrder = numbers.SequenceEqual(sameNumbers);
    Console.WriteLine(equalIfSameOrder); //True
    
    var equalIfDifferentOrder = numbers.SequenceEqual(sameNumbersInDifferentOrder);
    Console.WriteLine(equalIfDifferentOrder); //False

## Minimum
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

## Belirgin
    var numbers = new[] {1, 1, 2, 2, 3, 3, 4, 4, 5, 5};
    var distinctNumbers = numbers.Distinct();
    
    Console.WriteLine(string.Join(",", distinctNumbers));

    //1,2,3,4,5

## Saymak
    IEnumerable<int> numbers = new[] {1,2,3,4,5,6,7,8,9,10};

    var numbersCount = numbers.Count();
    Console.WriteLine(numbersCount); //10
    
    var evenNumbersCount = numbers.Count(n => (n & 1) == 0);
    Console.WriteLine(evenNumbersCount); //5

## Oyuncu kadrosu
"Cast", "IEnumerable<T>" için değil, "IEnumerable" için bir uzantı yöntemi olması bakımından diğer "Sayılandırılabilir" yöntemlerinden farklıdır. Böylece öncekinin örneklerini sonrakinin örneklerine dönüştürmek için kullanılabilir.

"ArrayList", "IEnumerable<T>" öğesini uygulamadığından bu derleme yapılmaz:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.First());

Bu beklendiği gibi çalışır:

    var numbers = new ArrayList() {1,2,3,4,5};
    Console.WriteLine(numbers.Cast<int>().First()); //1

"Yayınla", dönüştürme yayınlarını **yapmaz**. Aşağıdakiler derlenir ancak çalışma zamanında 'InvalidCastException'ı atar:

    var numbers = new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Cast<decimal>().ToArray();
    
Bir koleksiyona dönüştürme işlemi gerçekleştirmenin doğru yolu aşağıdaki gibidir:

    var numbers= new int[] {1,2,3,4,5};
    decimal[] numbersAsDecimal = numbers.Select(n => (decimal)n).ToArray();

## Seç (harita)
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

Bu tür bir işleve, işlevsel programlama dillerinde genellikle "harita" denir.

## Tarafından sipariş
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

## Azalan Sırala
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

## İçerir
    var numbers = new[] {1,2,3,4,5};
    Console.WriteLine(numbers.Contains(3)); //True
    Console.WriteLine(numbers.Contains(34)); //False

## İlk (bul)
    var numbers = new[] {1,2,3,4,5};
    
    var firstNumber = numbers.First();
    Console.WriteLine(firstNumber); //1
    
    var firstEvenNumber = numbers.First(n => (n & 1) == 0);
    Console.WriteLine(firstEvenNumber); //2
    
Aşağıdaki, "Sıra eşleşen öğe içermiyor" mesajıyla birlikte "InvalidOperationException" öğesini atar:

    var firstNegativeNumber = numbers.First(n => n < 0);

## Bekar
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.Single();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.Single(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

Sırada birden fazla öğe olduğundan, aşağıdaki `InvalidOperationException` öğesini atar:

    var theOnlyNumberInNumbers = numbers.Single();
    var theOnlyNegativeNumber = numbers.Single(n => n < 0);

## Son
    var numbers = new[] {1,2,3,4,5};
    
    var lastNumber = numbers.Last();
    Console.WriteLine(lastNumber); //5
    
    var lastEvenNumber = numbers.Last(n => (n & 1) == 0);
    Console.WriteLine(lastEvenNumber); //4
    
Aşağıdaki, `InvalidOperationException` öğesini atar:

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

## Tek VeyaVarsayılan
    var oneNumber = new[] {5};
    var theOnlyNumber = oneNumber.SingleOrDefault();
    Console.WriteLine(theOnlyNumber);  //5

    var numbers = new[] {1,2,3,4,5};
    
    var theOnlyNumberSmallerThanTwo = numbers.SingleOrDefault(n => n < 2);
    Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

    var theOnlyNegativeNumber = numbers.SingleOrDefault(n => n < 0);
    Console.WriteLine(theOnlyNegativeNumber);  //0

Aşağıdaki, `InvalidOperationException` öğesini atar:

    var theOnlyNumberInNumbers = numbers.SingleOrDefault();

## İlkVeyaVarsayılan
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

## Atlamak
Atla, ilk N öğeyi iade etmeden numaralandırır.
N+1 numaralı öğeye ulaşıldığında, Skip, numaralandırılmış her öğeyi döndürmeye başlar:


    var numbers = new[] {1,2,3,4,5};
    
    var allNumbersExceptFirstTwo = numbers.Skip(2);
    Console.WriteLine(string.Join(",", allNumbersExceptFirstTwo.ToArray()));

    //3,4,5

## Almak
Bu yöntem, bir numaralandırılabilir öğeden ilk "n" öğelerini alır.

    var numbers = new[] {1,2,3,4,5};
    
    var threeFirstNumbers = numbers.Take(3);
    Console.WriteLine(string.Join(",", threeFirstNumbers.ToArray()));

    //1,2,3

## Tersi
    var numbers = new[] {1,2,3,4,5};
    var reversed = numbers.Reverse();

    Console.WriteLine(string.Join(",", reversed.ToArray()));

    //5,4,3,2,1

## Türü
    var mixed = new object[] {1,"Foo",2,"Bar",3,"Fizz",4,"Buzz"};
    var numbers = mixed.OfType<int>();

    Console.WriteLine(string.Join(",", numbers.ToArray()));

    //1,2,3,4

## Maks
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

## Ortalama

    var numbers = new[] {1,2,3,4};
    
    var averageNumber = numbers.Average();
    Console.WriteLine(averageNumber); 
    // 2,5

Bu yöntem, sayılabilir sayıların ortalamasını hesaplar.

    var cities = new[] {
        new {Population = 1000},
        new {Population = 2000},
        new {Population = 4000}
    };
    
    var averagePopulation = cities.Average(c => c.Population);
    Console.WriteLine(averagePopulation);
    // 2333,33

Bu yöntem, temsilci işlevini kullanarak numaralandırılabilirlerin ortalamasını hesaplar.

## GroupBy
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

Faturaları ülkeye göre gruplandırın, kayıt sayısı, toplam ödenen ve ortalama ödenen miktar ile yeni bir nesne oluşturun

    var a = db.Invoices.GroupBy(i => i.Country)
              .Select(g => new { Country = g.Key,
                                 Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });

                             
                             
Sadece toplamları istiyorsak, grup yok

    var a = db.Invoices.GroupBy(i => 1)
              .Select(g => new { Count = g.Count(),
                                 Total = g.Sum(i => i.Paid),
                                 Average = g.Average(i => i.Paid) });
                             
Birkaç sayıya ihtiyacımız varsa

    var a = db.Invoices.GroupBy(g => 1)
              .Select(g => new { High = g.Count(i => i.Paid >= 1000),
                                 Low = g.Count(i => i.Paid < 1000),
                                 Sum = g.Sum(i => i.Paid) });


## Sözlüğe
Anahtarları belirlemek için sağlanan keySelector işlevini kullanarak "IEnumerable" kaynağından yeni bir sözlük döndürür. keySelector injective değilse (kaynak koleksiyonun her bir üyesi için benzersiz bir değer döndürür) bir 'ArgumentException' atar.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
    };

Yalnızca bir tuş seçici işlevi belirtmek, tuş seçicinin dönüş Türü 'TKey', orijinal nesne Türü 'TVal' ve depolanan değer olarak orijinal nesne ile bir 'Dictionary<TKey,TVal>' oluşturacaktır.

    var personsById = persons.ToDictionary(p => p.Id);
    // personsById is a Dictionary<int,object>

    Console.WriteLine(personsById[1].Name); //Fizz
    Console.WriteLine(personsById[2].Name); //Buzz

Bir değer seçici işlevinin de belirtilmesi, 'TKey' hala tuş seçicinin dönüş tipi olan bir 'Dictionary<TKey,TVal>' oluşturacaktır, ancak 'TVal' şimdi değer seçici işlevinin dönüş tipidir ve döndürülen değer şu şekildedir: saklanan değer.
    
    var namesById = persons.ToDictionary(p => p.Id, p => p.Name);
    //namesById is a Dictionary<int,string>

    Console.WriteLine(namesById[3]); //Foo
    Console.WriteLine(namesById[4]); //Bar

Yukarıda belirtildiği gibi, anahtar seçici tarafından döndürülen anahtarlar benzersiz olmalıdır. Aşağıdakiler bir istisna atar.

    var persons = new[] {
        new { Name="Fizz", Id=1},
        new { Name="Buzz", Id=2},
        new { Name="Foo", Id=3},
        new { Name="Bar", Id=4},
        new { Name="Oops", Id=4}
    };

    var willThrowException = persons.ToDictionary(p => p.Id)

Kaynak koleksiyon için benzersiz bir anahtar verilemiyorsa, bunun yerine ToLookup kullanmayı düşünün. Yüzeyde ToLookup, ToDictionary'ye benzer şekilde davranır, ancak sonuçta ortaya çıkan Aramada her anahtar, eşleşen anahtarlara sahip bir değerler koleksiyonuyla eşleştirilir.

## Birlik
    var numbers1to5 = new[] {1,2,3,4,5};
    var numbers4to8 = new[] {4,5,6,7,8};
    
    var numbers1to8 = numbers1to5.Union(numbers4to8);
    
    Console.WriteLine(string.Join(",", numbers1to8));

    //1,2,3,4,5,6,7,8

Yinelenenlerin sonuçtan kaldırıldığını unutmayın. Bu istenmiyorsa, bunun yerine 'Concat' kullanın.

## Sıralamak
    var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
    var someNumbers = numbers.Where(n => n < 6);
    
    Console.WriteLine(someNumbers.GetType().Name);
    //WhereArrayIterator`1
    
    var someNumbersArray = someNumbers.ToArray();
    
    Console.WriteLine(someNumbersArray.GetType().Name);
    //Int32[]

## Listeye
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

## Atla
    var numbers = new[] {2,4,6,8,1,3,5,7};
    
    var oddNumbers = numbers.SkipWhile(n => (n & 1) == 0);
    
    Console.WriteLine(string.Join(",", oddNumbers.ToArray()));

    //1,3,5,7

## Alırken
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

## Katılmak
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

## Boş
Boş bir IEnumerable int oluşturmak için:

    IEnumerable<int> emptyList = Enumerable.Empty<int>(); 

Bu boş IEnumerable<T>, her Tip T için önbelleğe alınır, böylece:

    Enumerable.Empty<decimal>() == Enumerable.Empty<decimal>(); // This is True
    Enumerable.Empty<int>() == Enumerable.Empty<decimal>();     // This is False




## SonraTarafından
'ThenBy', yalnızca birden fazla kriter kullanarak sipariş verilmesine izin veren bir 'OrderBy' yan tümcesinden sonra kullanılabilir

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

     

## Menzil
"Aralık" için iki parametre *ilk* sayı ve üretilecek öğelerin *sayımıdır (son sayı değil).

    // prints 1,2,3,4,5,6,7,8,9,10
    Console.WriteLine(string.Join(",", Enumerable.Range(1, 10)));

    // prints 10,11,12,13,14
    Console.WriteLine(string.Join(",", Enumerable.Range(10, 5)));




## Sol dış katılma
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

## Tekrar et
"Enumerable.Repeat", tekrarlanan bir değer dizisi oluşturur. Bu örnekte 4 kez "Merhaba" üretir.
        
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



