---
title: "LINQ Sorguları"
slug: "linq-sorgular"
draft: false
images: []
weight: 6793
type: docs
toc: true
---

LINQ, **L**anguage **IN**entegre **Q**uery anlamına gelen bir kısaltmadır. Çeşitli veri kaynakları ve biçimleri arasında verilerle çalışmak için tutarlı bir model sunarak bir sorgu dilini bütünleştiren bir kavramdır; XML belgelerinde, SQL veritabanlarında, ADO.NET Veri Kümelerinde, .NET koleksiyonlarında ve bir LINQ sağlayıcısının mevcut olduğu diğer tüm biçimlerde verileri sorgulamak ve dönüştürmek için aynı temel kodlama modellerini kullanırsınız.

## Sözdizimi
- Sorgu sözdizimi:
    - from \<range variable\> in \<collection\>
    - [from \<range variable\> in \<collection\>, ...]
    - \<filter, joining, grouping, aggregate operators, ...\> \<lambda expression\>
    - \<select or groupBy operator\> \<formulate the result\>

- Yöntem sözdizimi:

    - Enumerable.Aggregate(func)
    - Enumerable.Aggregate(seed, func)
    - Enumerable.Aggregate(seed, func, resultSelector)
    - Enumerable.All(predicate)
    - Enumerable.Any()
    - Enumerable.Any(predicate)
    - Enumerable.AsEnumerable()
    - Enumerable.Average()
    - Enumerable.Average(selector)
    - Enumerable.Cast\<Result\>()
    - Enumerable.Concat(second)
    - Enumerable.Contains(value)
    - Enumerable.Contains(value, comparer)
    - Enumerable.Count()
    - Enumerable.Count(predicate)
    - Enumerable.DefaultIfEmpty()
    - Enumerable.DefaultIfEmpty(defaultValue)
    - Enumerable.Distinct()
    - Enumerable.Distinct(comparer)
    - Enumerable.ElementAt(index)
    - Enumerable.ElementAtOrDefault(index)
    - Enumerable.Empty()
    - Enumerable.Except(second)
    - Enumerable.Except(second, comparer)
    - Enumerable.First()
    - Enumerable.First(predicate)
    - Enumerable.FirstOrDefault()
    - Enumerable.FirstOrDefault(predicate)
    - Enumerable.GroupBy(keySelector)
    - Enumerable.GroupBy(keySelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector)
    - Enumerable.GroupBy(keySelector, comparer)
    - Enumerable.GroupBy(keySelector, resultSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector)
    - Enumerable.GroupBy(keySelector, elementSelector, comparer)
    - Enumerable.GroupBy(keySelector, elementSelector, resultSelector, comparer)
    - Enumerable.Intersect(second)
    - Enumerable.Intersect(second, comparer)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
    - Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
    - Enumerable.Last()
    - Enumerable.Last(predicate)
    - Enumerable.LastOrDefault()
    - Enumerable.LastOrDefault(predicate)
    - Enumerable.LongCount()
    - Enumerable.LongCount(predicate)
    - Enumerable.Max()
    - Enumerable.Max(selector)
    - Enumerable.Min()
    - Enumerable.Min(selector)
    - Enumerable.OfType\<TResult\>()
    - Enumerable.OrderBy(keySelector)
    - Enumerable.OrderBy(keySelector, comparer)
    - Enumerable.OrderByDescending(keySelector)
    - Enumerable.OrderByDescending(keySelector, comparer)
    - Enumerable.Range(start, count)
    - Enumerable.Repeat(element, count)
    - Enumerable.Reverse()
    - Enumerable.Select(selector)
    - Enumerable.SelectMany(selector)
    - Enumerable.SelectMany(collectionSelector, resultSelector)
    - Enumerable.SequenceEqual(second)
    - Enumerable.SequenceEqual(second, comparer)
    - Enumerable.Single()
    - Enumerable.Single(predicate)
    - Enumerable.SingleOrDefault()
    - Enumerable.SingleOrDefault(predicate)
    - Enumerable.Skip(count)
    - Enumerable.SkipWhile(predicate)
    - Enumerable.Sum()
    - Enumerable.Sum(selector)
    - Enumerable.Take(count)
    - Enumerable.TakeWhile(predicate)
    - orderedEnumerable.ThenBy(keySelector)
    - orderedEnumerable.ThenBy(keySelector, comparer)
    - orderedEnumerable.ThenByDescending(keySelector)
    - orderedEnumerable.ThenByDescending(keySelector, comparer)
    - Enumerable.ToArray()
    - Enumerable.ToDictionary(keySelector)
    - Enumerable.ToDictionary(keySelector, elementSelector)
    - Enumerable.ToDictionary(keySelector, comparer)
    - Enumerable.ToDictionary(keySelector, elementSelector, comparer)
    - Enumerable.ToList()
    - Enumerable.ToLookup(keySelector)
    - Enumerable.ToLookup(keySelector, elementSelector)
    - Enumerable.ToLookup(keySelector, comparer)
    - Enumerable.ToLookup(keySelector, elementSelector, comparer)
    - Enumerable.Union(second)
    - Enumerable.Union(second, comparer)
    - Enumerable.Where(predicate)
    - Enumerable.Zip(second, resultSelector)

LINQ sorgularını kullanmak için System.Linq'i içe aktarmanız gerekir.

Yöntem Sözdizimi daha güçlü ve esnektir, ancak Sorgu Sözdizimi daha basit ve daha tanıdık olabilir. Sorgu sözdiziminde yazılan tüm sorgular, derleyici tarafından işlevsel sözdizimine çevrilir, bu nedenle performans aynıdır.

Sorgu nesneleri kullanılıncaya kadar değerlendirilmez, böylece performans cezası olmadan değiştirilebilir veya eklenebilirler.

## Zincirleme yöntemleri
[Birçok LINQ işlevi][1] hem bir "IEnumerable<TSource>" üzerinde çalışır hem de bir "IEnumerable<TResult>" döndürür. "TSource" ve "TResult" tür parametreleri, söz konusu yönteme ve kendisine geçirilen işlevlere bağlı olarak aynı türe atıfta bulunabilir veya bulunmayabilir.

Buna birkaç örnek

    public static IEnumerable<TResult> Select<TSource, TResult>(
        this IEnumerable<TSource> source,
        Func<TSource, TResult> selector
    )

    public static IEnumerable<TSource> Where<TSource>(
        this IEnumerable<TSource> source,
        Func<TSource, int, bool> predicate
    )

    public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(
        this IEnumerable<TSource> source,
        Func<TSource, TKey> keySelector
    )

Bazı yöntem zincirleme işlemleri, ilerlemeden önce tüm bir kümenin çalışılmasını gerektirebilirken, LINQ, [ertelenmiş yürütme] avantajından yararlanır(https://www.wikiod.com/tr/docs/c%23/68/linq-queries/8001/deferred- yürütme) [verim dönüşü <sup>**MSDN**</sup>](https://blogs.msdn.microsoft.com/oldnewthing/20080812-00/?p=21273/) kullanarak Numaralandırılabilir ve perde arkasında bir Numaralandırıcı. LINQ'da zincirleme işlemi, esasen, [numaralandırılabiliri numaralandırarak] (https://www.wikiod.com/tr/docs/c%23/68/) gerçekleşene kadar ertelenen orijinal küme için bir numaralandırılabilir (yineleyici) oluşturmaktır linq-sorgular/17356/numaralandırılabilir).

Bu, bu işlevlerin [akıcı bir şekilde zincirleme <sup>**wiki**</sup>](https://en.wikipedia.org/wiki/Fluent_interface) olmasına olanak tanır, burada bir işlev doğrudan diğerinin sonucu üzerinde etki edebilir. Bu kod stili, tek bir ifadede birçok dizi tabanlı işlemi gerçekleştirmek için kullanılabilir.

Örneğin, bir diziyi tek bir ifadede dönüştürmek, filtrelemek ve sıralamak için "Seç", "Nerede" ve "OrderBy"yi birleştirmek mümkündür.

    var someNumbers = { 4, 3, 2, 1 };

    var processed = someNumbers
            .Select(n => n * 2)   // Multiply each number by 2
            .Where(n => n != 6)   // Keep all the results, except for 6
            .OrderBy(n => n);     // Sort in ascending order

**Çıktı:**
>2
>4
>8

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/3Gta8X)

Genel "IEnumerable<T>" türünü hem genişleten hem de döndüren işlevler, tek bir ifadede zincirleme yan tümceler olarak kullanılabilir. Bu akıcı programlama tarzı güçlüdür ve kendi [uzantı yöntemlerinizi][2] oluştururken dikkate alınmalıdır.


[1]: https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx
[2]: https://www.wikiod.com/tr/docs/c%23/20/extension-methods#t=201607220826369208865

## First, FirstOrDefault, Last, LastOrDefault, Single ve SingleOrDefault
Altı yöntemin tümü, dizi türünün tek bir değerini döndürür ve bir yüklemle veya yüklem olmadan çağrılabilir.

"Yükleme" ile eşleşen öğelerin sayısına veya "yüklem" sağlanmazsa, kaynak dizideki öğelerin sayısına bağlı olarak, aşağıdaki gibi davranırlar:

# Öncelikle()

* Bir dizinin ilk öğesini veya sağlanan "yüklem" ile eşleşen ilk öğeyi döndürür.
* Dizi hiçbir öğe içermiyorsa, "Sıra hiç öğe içermiyor" mesajıyla birlikte bir "InvalidOperationException" atılır.
* Dizi, sağlanan "yüklem" ile eşleşen hiçbir öğe içermiyorsa, "Sıra eşleşen öğe içermiyor" mesajıyla birlikte bir "InvalidOperationException" atılır.

**Örnek**

    // Returns "a":
    new[] { "a" }.First();
    
    // Returns "a":
    new[] { "a", "b" }.First();
    
    // Returns "b":
    new[] { "a", "b" }.First(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.First(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new string[0].First();

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/ESYLcU)


# FirstOrDefault()

* Bir dizinin ilk öğesini veya sağlanan "yüklem" ile eşleşen ilk öğeyi döndürür.
* Dizi hiçbir öğe içermiyorsa veya sağlanan "yüklem" ile eşleşen hiçbir öğe yoksa, [`default(T)`](https://www.wikiod.com/tr/docs/c%23/) kullanılarak dizi türünün varsayılan değerini döndürür 26/keywords/109/default#t=201702071640321629621).

**Örnek**

    // Returns "a":
    new[] { "a" }.FirstOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.FirstOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.FirstOrDefault(x => x.Equals("b"));
    
    // Returns "ba":
    new[] { "ba", "be" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.FirstOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new string[0].FirstOrDefault();

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/XJ93lr)


# Son()

* Bir dizinin son öğesini veya sağlanan "yüklem" ile eşleşen son öğeyi döndürür.
* Dizi hiçbir öğe içermiyorsa, "Sıra hiçbir öğe içermiyor" mesajıyla birlikte bir "InvalidOperationException" atılır.
* Dizi, sağlanan "yüklem" ile eşleşen hiçbir öğe içermiyorsa, "Sıra eşleşen öğe içermiyor" mesajıyla birlikte bir "InvalidOperationException" atılır.

**Örnek**

    // Returns "a":
    new[] { "a" }.Last();
    
    // Returns "b":
    new[] { "a", "b" }.Last();
    
    // Returns "a":
    new[] { "a", "b" }.Last(x => x.Equals("a"));
    
    // Returns "be":
    new[] { "ba", "be" }.Last(x => x.Contains("b"));
    
    // Throws InvalidOperationException:
    new[] { "ca", "ce" }.Last(x => x.Contains("b"));

    // Throws InvalidOperationException:
    new string[0].Last(); 


# LastOrDefault()

* Bir dizinin son öğesini veya sağlanan "yüklem" ile eşleşen son öğeyi döndürür.
* Dizi hiçbir öğe içermiyorsa veya sağlanan "yüklem" ile eşleşen hiçbir öğe yoksa, "varsayılan(T)" kullanarak dizi türünün varsayılan değerini döndürür.

**Örnek**

    // Returns "a":
    new[] { "a" }.LastOrDefault();
    
    // Returns "b":
    new[] { "a", "b" }.LastOrDefault();
    
    // Returns "a":
    new[] { "a", "b" }.LastOrDefault(x => x.Equals("a"));
    
     // Returns "be":
    new[] { "ba", "be" }.LastOrDefault(x => x.Contains("b"));
    
    // Returns null:
    new[] { "ca", "ce" }.LastOrDefault(x => x.Contains("b")); 
    
    // Returns null:
    new string[0].LastOrDefault();


# Bekar()

* Dizi, tam olarak bir öğe içeriyorsa veya sağlanan "yüklem" ile tam olarak eşleşen bir öğe içeriyorsa, o öğe döndürülür.
* Dizi hiçbir öğe içermiyorsa veya sağlanan "yüklem" ile eşleşen hiçbir öğe yoksa, "Sıra hiçbir öğe içermiyor" mesajıyla birlikte bir "InvalidOperationException" atılır.
* Dizi birden fazla öğe içeriyorsa veya sağlanan "yüklem" ile eşleşen birden fazla öğe içeriyorsa, "Sıra birden fazla öğe içeriyor" mesajıyla birlikte bir "InvalidOperationException" atılır.
* __Not:__ Dizinin tam olarak bir eleman içerip içermediğini değerlendirmek için en fazla iki eleman numaralandırılmalıdır.

**Örnek**

    // Returns "a":
    new[] { "a" }.Single();
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "b" }.Single();
    
    // Returns "b":
    new[] { "a", "b" }.Single(x => x.Equals("b"));
    
    // Throws InvalidOperationException:
    new[] { "a", "b" }.Single(x => x.Equals("c"));
    
    // Throws InvalidOperationException:
    new string[0].Single(); 
    
    // Throws InvalidOperationException because sequence contains more than one element:
    new[] { "a", "a" }.Single();


# SingleOrDefault()
* Dizi, tam olarak bir öğe içeriyorsa veya sağlanan "yüklem" ile tam olarak eşleşen bir öğe içeriyorsa, o öğe döndürülür.
* Dizi hiçbir öğe içermiyorsa veya sağlanan "yüklem" ile eşleşen hiçbir öğe yoksa, "varsayılan(T)" döndürülür.
* Dizi birden fazla öğe içeriyorsa veya sağlanan "yüklem" ile eşleşen birden fazla öğe içeriyorsa, "Sıra birden fazla öğe içeriyor" mesajıyla birlikte bir "InvalidOperationException" atılır.
* Dizi, sağlanan "yüklem" ile eşleşen hiçbir öğe içermiyorsa, "varsayılan(T)" kullanarak dizi türünün varsayılan değerini döndürür.
* __Not:__ Dizinin tam olarak bir eleman içerip içermediğini değerlendirmek için en fazla iki eleman numaralandırılmalıdır.

**Örnek**

    // Returns "a":
    new[] { "a" }.SingleOrDefault();

    // returns "a"
    new[] { "a", "b" }.SingleOrDefault(x => x == "a"); 

    // Returns null:
    new[] { "a", "b" }.SingleOrDefault(x => x == "c");

    // Throws InvalidOperationException:
    new[] { "a", "a" }.SingleOrDefault(x => x == "a");

    // Throws InvalidOperationException:
    new[] { "a", "b" }.SingleOrDefault();

    // Returns null:
    new string[0].SingleOrDefault();

# Öneriler

- Bir dizinin herhangi bir öğe içerip içermediğini kontrol etmek için 'FirstOrDefault', 'LastOrDefault' veya 'SingleOrDefault' kullanabilseniz de, 'Any' veya 'Count' daha güvenilirdir. Bunun nedeni, dizinin ilk/son/tek öğesinin değeri eşit olarak 'varsayılan(T) olabileceğinden, bu üç yöntemden birinden 'default(T)' dönüş değerinin dizinin boş olduğunu kanıtlamamasıdır. )`

- Hangi yöntemlerin kodunuzun amacına en uygun olduğuna karar verin. Örneğin, yalnızca koleksiyonda yükleminiz &mdash; aksi takdirde 'İlk'i kullanın; "Single" olarak, dizide birden fazla eşleşen öğe varsa bir istisna atar. Bu elbette "*OrDefault"-muadilleri için de geçerlidir.

- Verimlilikle ilgili olarak: Bir sorgu tarafından döndürülen yalnızca bir öğe (`Tek`) veya yalnızca bir veya sıfır ("SingleOrDefault`) öğe olduğundan emin olmak genellikle uygun olsa da, bu yöntemlerin her ikisi de daha fazlasını gerektirir ve genellikle sorguyla ikinci bir eşleşme olmadığından emin olmak için incelenecek koleksiyonun tamamı. Bu, örneğin, ilk eşleşmeyi bulduktan sonra karşılanabilen "İlk" yönteminin davranışından farklıdır.

## Hariç
Hariç tutma yöntemi, ilk koleksiyonda yer alan ancak ikinci koleksiyonda yer almayan öğelerin kümesini döndürür. Varsayılan [`IEqualityComparer`][1] iki küme içindeki öğeleri karşılaştırmak için kullanılır. Argüman olarak bir [`IEqualityComparer`][1] kabul eden bir aşırı yükleme var.

**Örnek:**

    int[] first = { 1, 2, 3, 4 };
    int[] second = { 0, 2, 3, 5 };
    
    IEnumerable<int> inFirstButNotInSecond = first.Except(second);
    // inFirstButNotInSecond = { 1, 4 }

**Çıktı:**
>1
>4
 
[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/m3EqTQ)

Bu durumda ".Except(second)", "second" dizisindeki öğeleri, yani 2 ve 3'ü (0 ve 5 "birinci" dizide bulunmaz ve atlanır) hariç tutar.

'Hariç' ifadesinin 'Farklı' anlamına geldiğini unutmayın (yani, tekrarlanan öğeleri kaldırır). Örneğin:
    
    int[] third = { 1, 1, 1, 2, 3, 4 };
    
    IEnumerable<int> inThirdButNotInSecond = third.Except(second);
    // inThirdButNotInSecond = { 1, 4 }

**Çıktı:**
>1
>4

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/VlXBUp)

Bu durumda, 1 ve 4 öğeleri yalnızca bir kez döndürülür.

---

[`IEquatable`][2] uygulamak veya bir [`IEqualityComparer`][1] işlevi sağlamak, öğeleri karşılaştırmak için farklı bir yöntemin kullanılmasına izin verecektir.
['GetHashCode'][3] yönteminin de geçersiz kılınması gerektiğini unutmayın, böylece ['IEquatable'][2] uygulamasına göre özdeş olan "nesne" için özdeş bir karma kod döndürür.

***IEquatable ile Örnek:***

    class Holiday : IEquatable<Holiday>
    {
        public string Name { get; set; }
    
        public bool Equals(Holiday other)
        {
            return Name == other.Name;
        }
    
        // GetHashCode must return true whenever Equals returns true.
        public override int GetHashCode()
        {
            //Get hash code for the Name field if it is not null.
            return Name?.GetHashCode() ?? 0;
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            List<Holiday> holidayDifference = new List<Holiday>();

            List<Holiday> remoteHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Hanukkah" },
                new Holiday { Name = "Ramadan" }
            };

            List<Holiday> localHolidays = new List<Holiday>
            {
                new Holiday { Name = "Xmas" },
                new Holiday { Name = "Ramadan" }
            };

            holidayDifference = remoteHolidays
                .Except(localHolidays)
                .ToList();

            holidayDifference.ForEach(x => Console.WriteLine(x.Name));
        }
    }

Çıktı:

> Hanuka

[.NET Fiddle'da Canlı Demo][4]


[1]: https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.object.gethashcode(v=vs.110).aspx
[4]: https://dotnetfiddle.net/9ilGqy

## SeçMany
SelectMany linq yöntemi, bir 'IEnumerable<IEnumerable<T>>' öğesini bir 'IEnumerable<T>' içine 'düzleştirir'. "IEnumerable" kaynağında bulunan "IEnumerable" örnekleri içindeki tüm T öğeleri, tek bir "IEnumerable" içinde birleştirilecektir.

    var words = new [] { "a,b,c", "d,e", "f" };
    var splitAndCombine = words.SelectMany(x => x.Split(','));
    // returns { "a", "b", "c", "d", "e", "f" }

Girdi öğelerini dizilere dönüştüren bir seçici işlevi kullanırsanız, sonuç, bu dizilerin birer birer döndürülen öğeleri olacaktır.

'Select()'ten farklı olarak, çıktıdaki öğe sayısının giriştekiyle aynı olması gerekmediğini unutmayın.

**Daha fazla gerçek dünya örneği**

    class School
    {
        public Student[] Students { get; set; }
    }
    
    class Student 
    {
        public string Name { get; set; }
    }    
      
    var schools = new [] {
        new School(){ Students = new [] { new Student { Name="Bob"}, new Student { Name="Jack"} }},
        new School(){ Students = new [] { new Student { Name="Jim"}, new Student { Name="John"} }}
    };
                   
    var allStudents = schools.SelectMany(s=> s.Students);
                 
    foreach(var student in allStudents)
    {
        Console.WriteLine(student.Name);
    }

Çıktı:

>Bob
> Jack
>Jim
    John

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/LNyymI)

## Hiç
"Any", bir koleksiyonun **any** öğesinin bir koşulla eşleşip eşleşmediğini kontrol etmek için kullanılır.
<br/>*ayrıca bkz: [.All](https://www.wikiod.com/tr/docs/c%23/68/linq-queries/2773/all#t=201707041340119289445), [Herhangi biri ve FirstOrDefault: en iyi uygulama] (https://www.wikiod.com/tr/docs/c%23/68/linq-queries/16731/any-and-firstordefault-best-practice#t=201707041441456087738)*

## 1. Boş parametre ##
**Herhangi biri**: Koleksiyonda herhangi bir öğe varsa "true", koleksiyon boşsa "false" değerini döndürür:

    var numbers = new List<int>();
    bool result = numbers.Any(); // false

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.Any(); //true

## 2. Parametre olarak lambda ifadesi ##
**Herhangi biri**: Koleksiyonda lambda ifadesindeki koşulu karşılayan bir veya daha fazla öğe varsa "doğru" değerini döndürür:

    var arrayOfStrings = new string[] { "a", "b", "c" };
    arrayOfStrings.Any(item => item == "a");    // true
    arrayOfStrings.Any(item => item == "d");    // false
    
## 3. Boş koleksiyon ##
**Herhangi biri**: Koleksiyon boşsa ve bir lambda ifadesi sağlanmışsa "yanlış" değerini döndürür:
  
    var numbers = new List<int>();
    bool result = numbers.Any(i => i >= 0); // false

**Not:**
'Any', koşulla eşleşen bir öğe bulduğu anda koleksiyonun yinelenmesini durduracaktır. Bu, koleksiyonun mutlaka tam olarak numaralandırılmayacağı anlamına gelir; yalnızca koşulla eşleşen ilk öğeyi bulmaya yetecek kadar numaralandırılacaktır.

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/IQ4wG4)


## KATILIYOR
Birleştirmeler, ortak bir anahtar aracılığıyla verileri tutan farklı listeleri veya tabloları birleştirmek için kullanılır.

SQL'de olduğu gibi, LINQ'da aşağıdaki Birleştirme türleri desteklenir: <br/>
**İç, Sol, Sağ, Çapraz** ve **Tam Dış** Birleştirme.

Aşağıdaki örneklerde aşağıdaki iki liste kullanılmıştır:

    var first = new List<string>(){ "a","b","c"}; // Left data
    var second = new List<string>(){ "a", "c", "d"}; // Right data

## (İç birleşim

    var result = from f in first
                 join s in second on f equals s
                 select new { f, s };

    var result = first.Join(second, 
                            f => f, 
                            s => s,
                            (f, s) => new { f, s });

    // Result: {"a","a"}
    //         {"c","c"}

## Sol dış katılma
    var leftOuterJoin = from f in first
                        join s in second on f equals s into temp
                        from t in temp.DefaultIfEmpty()
                        select new { First = f, Second = t};

    // Or can also do:
    var leftOuterJoin = from f in first
                        from s in second.Where(x => x == f).DefaultIfEmpty()
                        select new { First = f, Second = s};

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  


    // Left outer join method syntax
    var leftOuterJoinFluentSyntax = first.GroupJoin(second,
                                          f => f,
                                          s => s,
                                          (f, s) => new { First = f, Second = s })
                                       .SelectMany(temp => temp.Second.DefaultIfEmpty(),
                                          (f, s) => new { First = f.First, Second = s });




## Sağ Dış Birleştirme
    var rightOuterJoin = from s in second
                         join f in first on s equals f into temp
                         from t in temp.DefaultIfEmpty()
                         select new {First=t,Second=s};

    // Result: {"a","a"}
    //         {"c","c"}  
    //         {null,"d"}  


## Çapraz Birleştirme

    var CrossJoin = from f in first
                    from s in second
                    select new { f, s };

    // Result: {"a","a"}
    //         {"a","c"}  
    //         {"a","d"}  
    //         {"b","a"}
    //         {"b","c"}  
    //         {"b","d"}  
    //         {"c","a"}
    //         {"c","c"}  
    //         {"c","d"}

## Tam Dış Birleştirme

    var fullOuterjoin = leftOuterJoin.Union(rightOuterJoin);

    // Result: {"a","a"}
    //         {"b", null}  
    //         {"c","c"}  
    //         {null,"d"}

## **Pratik örnek** ##
Yukarıdaki örnekler basit bir veri yapısına sahiptir, böylece farklı LINQ birleşimlerini teknik olarak anlamaya odaklanabilirsiniz, ancak gerçek dünyada katılmanız gereken sütunları olan tablolar olacaktır.

Aşağıdaki örnekte, kullanılan yalnızca bir 'Region' sınıfı vardır, gerçekte aynı anahtarı tutan iki veya daha fazla farklı tabloya katılırsınız (bu örnekte 'birinci' ve 'ikinci' ortak anahtar 'ID' ile birleştirilir ).

**Örnek:** Aşağıdaki veri yapısını göz önünde bulundurun:

    public class Region 
    {
        public Int32 ID;
        public string RegionDescription;
        
        public Region(Int32 pRegionID, string pRegionDescription=null)
        {
            ID = pRegionID; RegionDescription = pRegionDescription;
        }
    }

Şimdi verileri hazırlayın (yani verilerle doldurun):

    // Left data
    var first = new List<Region>() 
                     { new Region(1), new Region(3), new Region(4) }; 
    // Right data
    var second = new List<Region>() 
                     { 
                        new Region(1, "Eastern"),  new Region(2, "Western"),
                        new Region(3, "Northern"), new Region(4, "Southern")
                     }; 

Bu örnekte 'ilk'in herhangi bir bölge açıklaması içermediğini görebilirsiniz, bu nedenle onlara 'ikinci'den katılmak istiyorsunuz. Sonra iç birleştirme şöyle görünür:

    // do the inner join
    var result = from f in first
                 join s in second on f.ID equals s.ID
                 select new { f.ID, s.RegionDescription };
 

     // Result: {1,"Eastern"}
     //         {3, Northern}  
     //         {4,"Southern"}  

Bu sonuç anında anonim nesneler yarattı, bu iyi, ama biz zaten uygun bir sınıf oluşturduk - bu yüzden onu belirtebiliriz: `select new { f.ID, s.RegionDescription };` yerine `select' diyebiliriz aynı verileri döndürecek ancak diğer nesnelerle uyumluluğu sürdürecek 'Bölge' türünde nesneler oluşturacak olan yeni Bölge(f.ID, s.RegionDescription);`.



[.NET fiddle'da canlı demo](https://dotnetfiddle.net/pP6enP)


## Atla ve Al
Skip yöntemi, kaynak koleksiyonun başlangıcından itibaren bir dizi öğeyi hariç tutan bir koleksiyon döndürür. Hariç tutulan öğelerin sayısı, bağımsız değişken olarak verilen sayıdır. Koleksiyonda bağımsız değişkende belirtilenden daha az öğe varsa, boş bir koleksiyon döndürülür.

Take yöntemi, kaynak koleksiyonun başlangıcından itibaren birkaç öğe içeren bir koleksiyon döndürür. Dahil edilen öğelerin sayısı, argüman olarak verilen sayıdır. Koleksiyonda argümanda belirtilenden daha az öğe varsa, döndürülen koleksiyon, kaynak koleksiyonla aynı öğeleri içerecektir.

    var values = new [] { 5, 4, 3, 2, 1 };

    var skipTwo        = values.Skip(2);         // { 3, 2, 1 }
    var takeThree      = values.Take(3);         // { 5, 4, 3 }
    var skipOneTakeTwo = values.Skip(1).Take(2); // { 4, 3 }
    var takeZero       = values.Take(0);         // An IEnumerable<int> with 0 items

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/U2b76y)

**Atla ve Al**, sonuçları sayfalara ayırmak için yaygın olarak birlikte kullanılır, örneğin:

    IEnumerable<T> GetPage<T>(IEnumerable<T> collection, int pageNumber, int resultsPerPage) {
        int startIndex = (pageNumber - 1) * resultsPerPage;
        return collection.Skip(startIndex).Take(resultsPerPage);
    }


> **Uyarı:** LINQ to Entities yalnızca [sıralı sorgularda][1] Atlamayı destekler. Skip'i sipariş vermeden kullanmayı denerseniz, "'Skip' yöntemi yalnızca LINQ to Entities'de sıralanmış girişler için desteklenir. 'Skip' yönteminden önce 'OrderBy' yöntemi çağrılmalıdır. '"


[1]: https://www.wikiod.com/tr/docs/c%23/68/linq-queries/4389/query-ordering#t=201607261110520529272

## Linq sorgusu içinde değişken tanımlama (let anahtar sözcüğü)
Bir linq ifadesi içinde bir değişken tanımlamak için **let** anahtar sözcüğünü kullanabilirsiniz. Bu genellikle ara alt sorguların sonuçlarını depolamak için yapılır, örneğin:

     int[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

     var aboveAverages = from number in numbers
                         let average = numbers.Average()
                         let nSquared = Math.Pow(number,2)
                         where nSquared > average
                         select number;

     Console.WriteLine("The average of the numbers is {0}.", numbers.Average());

     foreach (int n in aboveAverages)
     {
       Console.WriteLine("Query result includes number {0} with square of {1}.", n, Math.Pow(n,2));
     }

**Çıktı:**

>Sayıların ortalaması 4,5'tir.
>Sorgu sonucu, karesi 9 olan 3 sayısını içerir.
>Sorgu sonucu, karesi 16 olan 4 sayısını içerir.
>Sorgu sonucu, karesi 25 olan 5 sayısını içerir.
>Sorgu sonucu, karesi 36 olan 6 sayısını içerir.
>Sorgu sonucu, karesi 49 olan 7 sayısını içerir.
>Sorgu sonucu, 64'ün karesi olan 8 sayısını içerir.
>Sorgu sonucu, karesi 81 olan 9 sayısını içerir.

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/zbjrHZ

## Posta kodu
'Zip' uzantı yöntemi iki koleksiyon üzerinde etkilidir. İki serideki her bir elemanı pozisyona göre eşleştirir. Bir "Func" örneğiyle, iki C# koleksiyonundaki öğeleri çiftler halinde işlemek için "Zip" kullanırız. Serinin boyutu farklıysa, daha büyük serinin ekstra öğeleri yok sayılır.

"C# in a Özet" kitabından bir örnek almak gerekirse,

    int[] numbers = { 3, 5, 7 };
    string[] words = { "three", "five", "seven", "ignored" };
    IEnumerable<string> zip = numbers.Zip(words, (n, w) => n + "=" + w);

**Çıktı:**

>3=üç
>5=beş
>7=yedi

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/nIA5E9

## Aralık ve Tekrar
Basit diziler oluşturmak için "Sayılandırılabilir" üzerindeki "Aralık" ve "Tekrar" statik yöntemleri kullanılabilir.

**Menzil**
---------

'Enumerable.Range()', bir başlangıç ​​değeri ve bir sayı verilen bir tamsayı dizisi oluşturur.

    // Generate a collection containing the numbers 1-100 ([1, 2, 3, ..., 98, 99, 100])
    var range = Enumerable.Range(1,100);

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/jA0VB1)

**Tekrar et**
------

'Enumerable.Repeat()', bir öğe ve gereken tekrar sayısı verilen bir yinelenen öğeler dizisi oluşturur.

    // Generate a collection containing "a", three times (["a","a","a"])
    var repeatedValues = Enumerable.Repeat("a", 3);

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/KpZfpt)

## Herşey
"All", bir koleksiyonun tüm öğelerinin bir koşulla eşleşip eşleşmediğini kontrol etmek için kullanılır.
<br/>*ayrıca bkz: [.Herhangi bir](https://www.wikiod.com/tr/docs/c%23/68/linq-queries/5098/any#t=201707041342119744775)*
## 1. Boş parametre ##

**Tümü**: boş parametre ile kullanılmasına izin verilmez.

## 2. Parametre olarak lambda ifadesi ##

**Tümü**: Koleksiyonun tüm öğeleri lambda ifadesini karşılıyorsa "doğru", aksi takdirde "yanlış" döndürür:

    var numbers = new List<int>(){ 1, 2, 3, 4, 5};
    bool result = numbers.All(i => i < 10); // true
    bool result = numbers.All(i => i >= 3); // false

## 3. Boş koleksiyon ##

**Tümü**: Koleksiyon boşsa ve bir lambda ifadesi sağlanmışsa "true" değerini döndürür:

    var numbers = new List<int>();
    bool result = numbers.All(i => i >= 0); // true

**Not:**
"Tümü", koşulla eşleşmeyen **değil** bir öğe bulduğu anda koleksiyonun yinelenmesini durduracaktır. Bu, koleksiyonun mutlaka tam olarak numaralandırılmayacağı anlamına gelir; yalnızca koşulla **eşleşmeyen** ilk öğeyi bulmaya yetecek kadar numaralandırılacaktır.


## Temel Bilgiler
LINQ, koleksiyonları (veya dizileri) sorgulamak için büyük ölçüde faydalıdır.

Örneğin, aşağıdaki örnek veriler verilmiştir:

    var classroom = new Classroom
    {
        new Student { Name = "Alice", Grade = 97, HasSnack = true  },
        new Student { Name = "Bob",   Grade = 82, HasSnack = false },
        new Student { Name = "Jimmy", Grade = 71, HasSnack = true  },
        new Student { Name = "Greg",  Grade = 90, HasSnack = false },
        new Student { Name = "Joe",   Grade = 59, HasSnack = false }
    }

LINQ sözdizimini kullanarak bu verileri "sorgulayabiliriz". Örneğin, bugün bir şeyler atıştıran tüm öğrencileri almak için:

    var studentsWithSnacks = from s in classroom.Students
                             where s.HasSnack
                             select s;

Veya 90 veya üzeri nota sahip öğrencileri almak ve tam 'Student' nesnesini değil, yalnızca adlarını döndürmek için:

    var topStudentNames = from s in classroom.Students
                          where s.Grade >= 90
                          select s.Name;

LINQ özelliği, aynı işlevleri yerine getiren, neredeyse aynı performansa sahip, ancak çok farklı yazılmış iki sözdiziminden oluşur. Yukarıdaki örnekteki sözdizimine **sorgu sözdizimi** denir. Ancak aşağıdaki örnekte **yöntem sözdizimi** gösterilmektedir. Yukarıdaki örnekte olduğu gibi aynı veriler döndürülecektir, ancak sorgunun yazılma şekli farklıdır.

    var topStudentNames = classroom.Students
                                   .Where(s => s.Grade >= 90)
                                   .Select(s => s.Name);
                                        

## Toplama
'Topla' Bir dizi üzerinde bir akümülatör işlevi uygular.
 
    int[] intList = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    int sum = intList.Aggregate((prevSum, current) => prevSum + current);
    // sum = 55

- İlk adımda `prevSum = 1`
- İkinci `prevSum = prevSum(at
ilk adım) + 2`
- i. adımda `prevSum = prevSum((i-1) noktasında
adım) + dizinin i-inci elemanı'


    string[] stringList = { "Hello", "World", "!" };
    string joinedString = stringList.Aggregate((prev, current) => prev + " " + current);
    // joinedString = "Hello World !"


----------

İkinci bir "Agrega" aşırı yüklemesi, aynı zamanda, ilk akümülatör değeri olan bir "tohum" parametresini de alır. Bu, bir koleksiyondaki birden fazla koşulu bir kereden fazla yinelemeden hesaplamak için kullanılabilir.

    List<int> items = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

"Öğelerin" toplanması için hesaplamak istediğimiz

1. Toplam ".Sayı"
2. Çift sayıların miktarı
3. Her bir öğeyi toplayın

'Agrega' kullanılarak şu şekilde yapılabilir:

    var result = items.Aggregate(new { Total = 0, Even = 0, FourthItems = new List<int>() },
                    (accumelative,item) =>
                    new {
                        Total = accumelative.Total + 1,
                        Even = accumelative.Even + (item % 2 == 0 ? 1 : 0),
                        FourthItems = (accumelative.Total + 1)%4 == 0 ? 
                            new List<int>(accumelative.FourthItems) { item } : 
                            accumelative.FourthItems 
                    });
    // Result:
    // Total = 12
    // Even = 6
    // FourthItems = [4, 8, 12]

_Özellikler salt okunur olduğundan, çekirdek olarak adsız bir türün kullanılmasının her öğe için yeni bir nesne başlatması gerektiğine dikkat edin. Özel bir sınıf kullanarak, bilgi basitçe atanabilir ve "yeni" gerekmez (yalnızca ilk "tohum" parametresi verildiğinde_

## SelectMany: Bir dizi diziyi düzleştirme
    var sequenceOfSequences = new [] { new [] { 1, 2, 3 }, new [] { 4, 5 }, new [] { 6 } };
    var sequence = sequenceOfSequences.SelectMany(x => x);
    // returns { 1, 2, 3, 4, 5, 6 }

Varsa veya bir dizi dizisi oluşturuyorsanız, ancak sonucu tek bir uzun dizi olarak istiyorsanız, `SelectMany()` kullanın.

LINQ Sorgu Sözdiziminde:

    var sequence = from subSequence in sequenceOfSequences
                   from item in subSequence
                   select item;

Bir koleksiyon koleksiyonunuz varsa ve aynı anda ebeveyn ve alt koleksiyondaki veriler üzerinde çalışmak istiyorsanız, `SelectMany` ile de mümkündür.

Basit sınıfları tanımlayalım

    public class BlogPost
    {
        public int Id { get; set; }
        public string Content { get; set; }
        public List<Comment> Comments { get; set; }
    }

    public class Comment
    {
        public int Id { get; set; }
        public string Content { get; set; }
    }

Aşağıdaki koleksiyonumuz olduğunu varsayalım.

    List<BlogPost> posts = new List<BlogPost>()
    {
        new BlogPost()
        {
            Id = 1,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 1,
                    Content = "It's really great!",
                },
                new Comment()
                {
                    Id = 2,
                    Content = "Cool post!"
                }
            }
        },
        new BlogPost()
        {
            Id = 2,
            Comments = new List<Comment>()
            {
                new Comment()
                {
                    Id = 3,
                    Content = "I don't think you're right",
                },
                new Comment()
                {
                    Id = 4,
                    Content = "This post is a complete nonsense"
                }
            }
        }
    };

Şimdi, bu yorumla ilişkili 'BlogPost'un 'Kimliği' ile birlikte 'İçerik' yorumlarını seçmek istiyoruz. Bunu yapmak için uygun `SelectMany` aşırı yüklemesini kullanabiliriz.

    var commentsWithIds = posts.SelectMany(p => p.Comments, (post, comment) => new { PostId = post.Id, CommentContent = comment.Content });

`yorumlarWithIds` şuna benziyor

    {
        PostId = 1,
        CommentContent = "It's really great!"
    },
    {
        PostId = 1,
        CommentContent = "Cool post!"
    },
    {
        PostId = 2,
        CommentContent = "I don't think you're right"
    },
    {
        PostId = 2,
        CommentContent = "This post is a complete nonsense"
    }




## Belirgin
Bir "IEnumerable"dan benzersiz değerler döndürür. Benzersizlik, varsayılan eşitlik karşılaştırıcısı kullanılarak belirlenir.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

    var distinct = array.Distinct();
    // distinct = { 1, 2, 3, 4, 5 }

Özel bir veri türünü karşılaştırmak için, "IEquatable<T>" arabirimini uygulamamız ve tür için "GetHashCode" ve "Equals" yöntemlerini sağlamamız gerekir. Veya eşitlik karşılaştırıcısı geçersiz kılınabilir:

    class SSNEqualityComparer : IEqualityComparer<Person> {
        public bool Equals(Person a, Person b) => return a.SSN == b.SSN;
        public int GetHashCode(Person p) => p.SSN;
    }

    List<Person> people;

    distinct = people.Distinct(SSNEqualityComparer);

## Türe göre sorgu toplama / yazılacak öğeler
    interface IFoo { }
    class Foo : IFoo { }
    class Bar : IFoo { }

----------
    var item0 = new Foo();
    var item1 = new Foo();
    var item2 = new Bar();
    var item3 = new Bar();
    var collection = new IFoo[] { item0, item1, item2, item3 };

"OfType"ı Kullanma

    var foos = collection.OfType<Foo>(); // result: IEnumerable<Foo> with item0 and item1
    var bars = collection.OfType<Bar>(); // result: IEnumerable<Bar> item item2 and item3
    var foosAndBars = collection.OfType<IFoo>(); // result: IEnumerable<IFoo> with all four items

'Nerede' kullanma

    var foos = collection.Where(item => item is Foo); // result: IEnumerable<IFoo> with item0 and item1
    var bars = collection.Where(item => item is Bar); // result: IEnumerable<IFoo> with item2 and item3

'Yayınla' kullanma

    var bars = collection.Cast<Bar>();                // throws InvalidCastException on the 1st item
    var foos = collection.Cast<Foo>();                // throws InvalidCastException on the 3rd item
    var foosAndBars = collection.Cast<IFoo>();        // OK 
    

## GroupBy
GroupBy, bir "IEnumerable<T>" öğe koleksiyonunu farklı gruplara ayırmanın kolay bir yoludur.
## Basit Örnek ##
Bu ilk örnekte, tek ve çift öğeler olmak üzere iki grup elde ettik.

    List<int> iList = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var grouped = iList.GroupBy(x => x % 2 == 0);

    //Groups iList into odd [13579] and even[2468] items 
           
    foreach(var group in grouped)
    {
        foreach (int item in group)
        {
            Console.Write(item); // 135792468  (first odd then even)
        }
    }

## Daha Karmaşık Örnek ##

Örnek olarak yaşa göre bir insan listesini gruplandıralım.
İlk olarak, Name ve Age olmak üzere iki özelliği olan bir Person nesnesi oluşturacağız.

    public class Person
    {
        public int Age {get; set;}
        public string Name {get; set;}
    }

Ardından çeşitli isim ve yaştaki kişilerden oluşan örnek listemizi oluşturuyoruz.

    List<Person> people = new List<Person>();
    people.Add(new Person{Age = 20, Name = "Mouse"});
    people.Add(new Person{Age = 30, Name = "Neo"});
    people.Add(new Person{Age = 40, Name = "Morpheus"});
    people.Add(new Person{Age = 30, Name = "Trinity"});
    people.Add(new Person{Age = 40, Name = "Dozer"});
    people.Add(new Person{Age = 40, Name = "Smith"});

Ardından, kişi listemizi yaşa göre gruplandırmak için bir LINQ sorgusu oluşturuyoruz.

    var query = people.GroupBy(x => x.Age);

Bunu yaparak, her grup için Yaşı görebilir ve gruptaki her kişinin bir listesine sahip olabiliriz.

    foreach(var result in query)
    {
        Console.WriteLine(result.Key);
                    
        foreach(var person in result)
            Console.WriteLine(person.Name);
    }

Bu, aşağıdaki çıktıyla sonuçlanır:

    20
    Mouse
    30
    Neo
    Trinity
    40
    Morpheus
    Dozer
    Smith

[.NET Fiddle'daki canlı demo][1] ile oynayabilirsiniz


[1]: https://dotnetfiddle.net/VFOZ1x

## Numaralandırılabiliri Numaralandırma
IEnumerable<<a>T> arabirimi, tüm genel numaralandırıcılar için temel arabirimdir ve LINQ'yu anlamanın önemli bir parçasıdır. Özünde, diziyi temsil eder.

Bu temel arabirim, [Collection<<a>T>](https://msdn.microsoft.com/en-us/library/ms132397(v=vs.110).aspx gibi) tüm genel koleksiyonlar tarafından devralınır. ), [Dizi](https://msdn.microsoft.com/en-us/library/system.array(v=vs.110).aspx), [Liste<<a>T>](https:// msdn.microsoft.com/en-us/library/6sh2ey19(v=vs.110).aspx), [Dictionary<TKey, TValue> Class](https://msdn.microsoft.com/en-us/library/ xfhwa508(v=vs.110).aspx) ve [HashSet<<a>T>](https://msdn.microsoft.com/en-us/library/bb359438(v=vs.110).aspx) .

Diziyi temsil etmenin yanı sıra, IEnumerable<<a>T> öğesinden miras alan herhangi bir sınıf, bir IEnumerator<<a>T> sağlamalıdır. Numaralandırıcı, numaralandırılabilir için yineleyiciyi ortaya çıkarır ve bu birbirine bağlı iki arayüz ve fikir, "numaralandırılabiliri numaralandır" deyişinin kaynağıdır.

"Sayılanabilir olanı numaralandırmak" önemli bir deyimdir. Numaralandırılabilir, nasıl yineleneceğine ilişkin bir yapıdır, herhangi bir materyalize nesne tutmaz. Örneğin, sıralama yaparken, bir numaralandırılabilir alan sıralamak için alanın ölçütlerini tutabilir, ancak `.OrderBy()`nin kendi içinde kullanılması yalnızca *nasıl* sıralanacağını bilen bir IEnumerable<<a>T> döndürür. Kümeyi yinelemede olduğu gibi, nesneleri gerçekleştirecek bir çağrı kullanmak, numaralandırma olarak bilinir (örneğin `.ToList()`). Numaralandırma işlemi, seriler arasında hareket etmek ve ilgili nesneleri (sıralı, filtrelenmiş, yansıtılmış, vb.) döndürmek için numaralandırılabilir *nasıl* tanımını kullanır.

Yalnızca numaralandırılabilir bir kez numaralandırıldıktan sonra, nesnelerin gerçekleşmesine neden olur; bu, [zaman karmaşıklığı](https://en.wikipedia.org/wiki/Time_complexity) gibi metriklerin (seri boyutuyla ilgili ne kadar sürmesi gerektiği) ) ve uzaysal karmaşıklık (seri boyutuna göre ne kadar alan kullanması gerektiği) ölçülebilir.

IEnumerable<<a>T> öğesinden miras alan kendi sınıfınızı oluşturmak, numaralandırılabilir olması gereken temel seriye bağlı olarak biraz karmaşık olabilir. Genel olarak mevcut jenerik koleksiyonlardan birini kullanmak en iyisidir. Bununla birlikte, temel yapı olarak tanımlanmış bir diziye sahip olmadan IEnumerable<<a>T> arabiriminden miras almak da mümkündür.

Örneğin, temel dizi olarak Fibonacci serisini kullanmak. 'Where' çağrısının basitçe bir 'IEnumerable' oluşturduğunu ve bu numaralandırılabilir numaralandırma çağrısı yapılana kadar değerlerden herhangi birinin gerçekleşmediğini unutmayın.

    void Main()
    {
        Fibonacci Fibo = new Fibonacci();
        IEnumerable<long> quadrillionplus = Fibo.Where(i => i > 1000000000000);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(quadrillionplus.Take(2).Sum());
        Console.WriteLine(quadrillionplus.Skip(2).First());
    
        IEnumerable<long> fibMod612 = Fibo.OrderBy(i => i % 612);
        Console.WriteLine("Enumerable built");
        Console.WriteLine(fibMod612.First());//smallest divisible by 612
    }
    
    public class Fibonacci : IEnumerable<long>
    {
        private int max = 90;
    
        //Enumerator called typically from foreach
        public IEnumerator GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
        
        //Enumerable called typically from linq
        IEnumerator<long> IEnumerable<long>.GetEnumerator() {
            long n0 = 1;
            long n1 = 1;
            Console.WriteLine("Enumerating the Enumerable");
            for(int i = 0; i < max; i++){
                yield return n0+n1;
                n1 += n0;
                n0 = n1-n0;
            }
        }
    }

Çıktı

    Enumerable built
    Enumerating the Enumerable
    4052739537881
    Enumerating the Enumerable
    4052739537881
    Enumerable built
    Enumerating the Enumerable
    14930352

İkinci kümenin (fibMod612) gücü, tüm Fibonacci sayı kümemizi sıralamak için çağrı yapmamıza rağmen, `.First()` kullanılarak yalnızca bir değer alındığından, zaman karmaşıklığının yalnızca O(n) olmasıydı. Sıralama algoritmasının yürütülmesi sırasında karşılaştırılması gereken 1 değer. Bunun nedeni, numaralandırıcımızın yalnızca 1 değer istemesi ve bu nedenle tüm numaralandırılabilirin gerçekleşmesi gerekmemesidir. '.First()' yerine '.Take(5)' kullansaydık, numaralandırıcı 5 değer isterdi ve en fazla 5 değerin gerçekleştirilmesi gerekirdi. Tüm bir seti sipariş etmek *ve ardından* ilk 5 değeri almakla karşılaştırıldığında, ilkesi çok fazla uygulama süresi ve alanı kazandırır.



## Neresi
Belirtilen yüklemin kendileri için doğru olduğu öğelerin bir alt kümesini döndürür.

    List<string> trees = new List<string>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

## Yöntem sözdizimi

    // Select all trees with name of length 3
    var shortTrees = trees.Where(tree => tree.Length == 3); // Oak, Elm

## Sorgu sözdizimi

    var shortTrees = from tree in trees
                     where tree.Length == 3
                     select tree; // Oak, Elm


## Range'i çeşitli Linq yöntemleriyle kullanma
Enumerable sınıfını, döngüler için Linq one astarlarına dönüştürmek için Linq sorgularının yanında kullanabilirsiniz.

**Örnek Seç**

Bunu yapmaya karşı:


    var asciiCharacters = new List<char>();
    for (var x = 0; x < 256; x++)
    {
        asciiCharacters.Add((char)x);
    }

Bunu yapabilirsiniz:

    var asciiCharacters = Enumerable.Range(0, 256).Select(a => (char) a);


**Nerede Örnek**

Bu örnekte 100 sayı üretilecek ve hatta bir tane çıkarılacak.

    var evenNumbers = Enumerable.Range(1, 100).Where(a => a % 2 == 0);



## Yuvalanmış döngüler yerine SelectMany kullanma
2 liste verildi

    var list1 = new List<string> { "a", "b", "c" };
    var list2 = new List<string> { "1", "2", "3", "4" };

tüm permütasyonların çıktısını almak istiyorsanız, iç içe geçmiş döngüleri kullanabilirsiniz.

    var result = new List<string>();
    foreach (var s1 in list1)
        foreach (var s2 in list2)
            result.Add($"{s1}{s2}");

SelectMany kullanarak aynı işlemi yapabilirsiniz.

    var result = list1.SelectMany(x => list2.Select(y => $"{x}{y}", x, y)).ToList();


## İçerir
MSDN:
> Bir dizinin belirli bir öğe içerip içermediğini belirler.
> belirtilen `IEqualityComparer<T>`


    List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
    var result1 = numbers.Contains(4); // true
    var result2 = numbers.Contains(8); // false

    List<int> secondNumberCollection = new List<int> { 4, 5, 6, 7 };
    // Note that can use the Intersect method in this case
    var result3 = secondNumberCollection.Where(item => numbers.Contains(item)); // will be true only for 4,5


Kullanıcı tanımlı bir nesne kullanarak:

    public class Person
    {
       public string Name { get; set; }
    }

    List<Person> objects = new List<Person>
    {
        new Person { Name = "Nikki"},
        new Person { Name = "Gilad"},
        new Person { Name = "Phil"},
        new Person { Name = "John"}
    };

    //Using the Person's Equals method - override Equals() and GetHashCode() - otherwise it
    //will compare by reference and result will be false
    var result4 = objects.Contains(new Person { Name = "Phil" }); // true

'Enumerable.Contains(değer, karşılaştırıcı)' aşırı yüklenmesini kullanma:

    public class Compare : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name;
        }
        public int GetHashCode(Person codeh)
        {
            return codeh.Name.GetHashCode();
        }
    }

    var result5 = objects.Contains(new Person { Name = "Phil" }, new Compare()); // true

**````İçerir`````nin akıllıca bir kullanımı, birden çok ''``if```` cümlesini bir ''``İçerir```` çağrısıyla değiştirmek olacaktır.**

Yani bunu yapmak yerine:

    if(status == 1 || status == 3 || status == 4)
    {
        //Do some business operation
    }
    else
    {
        //Do something else
    }
 
Bunu yap:
    
    if(new int[] {1, 3, 4 }.Contains(status)
    {
        //Do some business operaion
    }
    else 
    {
        //Do something else
    }

## GroupBy bir veya daha fazla alan
Bir Film modelimiz olduğunu varsayalım:

    public class Film {
        public string Title { get; set; }
        public string Category { get; set; }
        public int Year { get; set; }
    }

Kategori mülküne göre gruplandır:

    foreach (var grp in films.GroupBy(f => f.Category)) {
        var groupCategory = grp.Key;
        var numberOfFilmsInCategory = grp.Count();
    }

Kategori ve Yıla Göre Gruplandırın:

    foreach (var grp in films.GroupBy(f => new { Category = f.Category, Year = f.Year })) {
        var groupCategory = grp.Key.Category;
        var groupYear = grp.Key.Year;
        var numberOfFilmsInCategory = grp.Count();
    }



## Sorgu Sıralaması - OrderBy() ThenBy() OrderByDescending() ThenByDescending()
    string[] names= { "mark", "steve", "adam" };

**Artan:**

*Sorgu Sözdizimi*

    var sortedNames =
        from name in names
        orderby name
        select name;

*Yöntem Sözdizimi*

    var sortedNames = names.OrderBy(name => name);

sortedNames, adları aşağıdaki sırayla içerir:
"adam", "mark", "steve"

**Azalan:**


*Sorgu Sözdizimi*

    var sortedNames =
        from name in names
        orderby name descending
        select name;

*Yöntem Sözdizimi*

    var sortedNames = names.OrderByDescending(name => name);

sortedNames, adları aşağıdaki sırayla içerir:
"steve", "mark", "adam"

**Birkaç alana göre sıralayın**

    Person[] people =
    {
        new Person { FirstName = "Steve", LastName = "Collins", Age = 30},
        new Person { FirstName = "Phil" , LastName = "Collins", Age = 28},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 29},
        new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 15}
    };

*Sorgu Sözdizimi*

    var sortedPeople = from person in people
                       orderby person.LastName, person.FirstName, person.Age descending
                       select person;

*Yöntem Sözdizimi*

     sortedPeople = people.OrderBy(person => person.LastName)
                          .ThenBy(person => person.FirstName)
                          .ThenByDescending(person => person.Age);

*Sonuç*

    1. Adam Ackerman 29
    2. Adam Ackerman 15
    3. Phil Collins  28
    4. Steve Collins 30



## Sözlüğe
"ToDictionary()" LINQ yöntemi, belirli bir "IEnumerable<T>" kaynağına dayalı bir "Dictionary<TKey, TElement>" koleksiyonu oluşturmak için kullanılabilir.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, User> usersById = users.ToDictionary(x => x.Id);

Bu örnekte, "ToDictionary"ye iletilen tek argüman, her öğenin anahtarını döndüren "Func<TSource, TKey>" türündedir.

Bu, aşağıdaki işlemi gerçekleştirmenin kısa bir yoludur:

    Dictionary<int, User> usersById = new Dictionary<int User>();
    foreach (User u in users) 
    {
      usersById.Add(u.Id, u);
    }

Ayrıca, 'Func<TSource, TElement>' türünde olan ve her giriş için eklenecek 'Değer'i döndüren 'ToDictionary' yöntemine ikinci bir parametre iletebilirsiniz.

    IEnumerable<User> users = GetUsers();
    Dictionary<int, string> userNamesById = users.ToDictionary(x => x.Id, x => x.Name);

Anahtar değerlerini karşılaştırmak için kullanılan 'IComparer'ı belirtmek de mümkündür. Bu, anahtar bir dize olduğunda ve büyük/küçük harfe duyarsız eşleşmesini istediğinizde yararlı olabilir.

    IEnumerable<User> users = GetUsers();
    Dictionary<string, User> usersByCaseInsenstiveName = users.ToDictionary(x => x.Name, StringComparer.InvariantCultureIgnoreCase);

    var user1 = usersByCaseInsenstiveName["john"];
    var user2 = usersByCaseInsenstiveName["JOHN"];
    user1 == user2; // Returns true

Not: "ToDictionary" yöntemi, tüm anahtarların benzersiz olmasını gerektirir, yinelenen anahtarlar olmamalıdır. Varsa, bir istisna atılır: `ArgumentException: Aynı anahtara sahip bir öğe zaten eklendi.` Aynı anahtara sahip birden fazla öğeniz olacağını bildiğiniz bir senaryonuz varsa, kullanmak daha iyidir. [`ToLookup`](https://www.wikiod.com/tr/docs/c%23/68/linq-queries/14871/tolookup) yerine.




## Atla
"SkipWhile()", ilk eşleşme olmayana kadar öğeleri hariç tutmak için kullanılır (bu, çoğu kişi için sezgisel olabilir)

    int[] list = { 42, 42, 6, 6, 6, 42 };
    var result = list.SkipWhile(i => i == 42); 
    // Result: 6, 6, 6, 42

## DefaultIfEmpty
DefaultIfEmpty, Sıra hiç öğe içermiyorsa bir Varsayılan Öğe döndürmek için kullanılır. Bu Öğe, Türün Varsayılanı veya bu Türün kullanıcı tanımlı bir örneği olabilir. Örnek:

    var chars = new List<string>() { "a", "b", "c", "d" };

    chars.DefaultIfEmpty("N/A").FirstOrDefault(); // returns "a";
    
    chars.Where(str => str.Length > 1)
         .DefaultIfEmpty("N/A").FirstOrDefault(); // return "N/A"

    chars.Where(str => str.Length > 1)
            .DefaultIfEmpty().First(); // returns null;

**Sol Birleştirmelerde Kullanım**:
--------------------

'DefaultIfEmpty' ile, geleneksel Linq Join, eşleşme bulunmazsa varsayılan bir nesne döndürebilir. Böylece bir SQL'in Sol Birleştirmesi olarak hareket eder. Örnek:

    var leftSequence = new List<int>() { 99, 100, 5, 20, 102, 105 };
    var rightSequence = new List<char>() { 'a', 'b', 'c', 'i', 'd' };
    
    var numbersAsChars = from l in leftSequence
                         join r in rightSequence
                         on l equals (int)r into leftJoin
                         from result in leftJoin.DefaultIfEmpty('?')
                         select new
                         {
                             Number = l,
                             Character = result
                         };
    
    foreach(var item in numbersAsChars)
    {
        Console.WriteLine("Num = {0} ** Char = {1}", item.Number, item.Character);
    }

    ouput: 

    Num = 99         Char = c
    Num = 100        Char = d
    Num = 5          Char = ?
    Num = 20         Char = ?
    Num = 102        Char = ?
    Num = 105        Char = i

Bir 'DefaultIfEmpty' kullanılması durumunda (varsayılan bir değer belirtmeden) ve bu, doğru sırada eşleşen öğe olmamasına neden olacaksa, özelliklerine erişmeden önce nesnenin 'boş' olmadığından emin olunmalıdır. Aksi takdirde, bir `NullReferenceException` ile sonuçlanacaktır. Örnek:

    var leftSequence = new List<int> { 1, 2, 5 };
    var rightSequence = new List<dynamic>()
        {
            new { Value = 1 },
            new { Value = 2 },
            new { Value = 3 },
            new { Value = 4 },
        };

    var numbersAsChars = (from l in leftSequence
                            join r in rightSequence
                            on l equals r.Value into leftJoin
                            from result in leftJoin.DefaultIfEmpty()
                            select new
                            {
                                Left = l,
                                // 5 will not have a matching object in the right so result 
                                // will be equal to null. 
                                // To avoid an error use:
                                //    -  C# 6.0 or above - ?. 
                                //    -  Under           - result == null ? 0 : result.Value
                                Right = result?.Value
                            }).ToList();



## SıraEşit
"SequenceEqual", iki "IEnumerable<T>" dizisini birbiriyle karşılaştırmak için kullanılır.


    int[] a = new int[] {1, 2, 3};
    int[] b = new int[] {1, 2, 3};
    int[] c = new int[] {1, 3, 2};

    bool returnsTrue = a.SequenceEqual(b);
    bool returnsFalse = a.SequenceEqual(c);

## ElementAt ve ElementAtOrDefault
'ElementAt', öğeyi 'n' dizininde döndürür. "n", numaralandırılabilir aralığın içinde değilse, bir "ArgumentOutOfRangeException" oluşturur.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAt(2);  // 3
    numbers.ElementAt(10); // throws ArgumentOutOfRangeException

"ElementAtOrDefault", öğeyi "n" dizininde döndürür. "n", numaralandırılabilir aralığı içinde değilse, bir "varsayılan(T)" döndürür.

    int[] numbers  = { 1, 2, 3, 4, 5 };
    numbers.ElementAtOrDefault(2);  // 3
    numbers.ElementAtOrDefault(10); // 0 = default(int)

Hem "ElementAt" hem de "ElementAtOrDefault", kaynağın bir "IList<T>" olduğu durumlar için optimize edilmiştir ve bu durumlarda normal dizin oluşturma kullanılacaktır.

"ElementAt" için, sağlanan dizin "IList<T>" boyutundan büyükse, listenin bir "ArgumentOutOfRangeException" atması gerektiğini (ancak teknik olarak garanti edilmediğini) unutmayın.


## Birden çok diziye katılma
"Müşteri", "Satın Alma" ve "Satın AlmaÖğesi" varlıklarını aşağıdaki gibi düşünün:

    public class Customer
    {
       public string Id { get; set } // A unique Id that identifies customer    
       public string Name  {get; set; }
    }

    public class Purchase
    {
       public string Id { get; set }
       public string CustomerId {get; set; }
       public string Description { get; set; }
    }

    public class PurchaseItem
    {
       public string Id { get; set }
       public string PurchaseId {get; set; }
       public string Detail { get; set; }
    }

Yukarıdaki varlıklar için aşağıdaki örnek verileri göz önünde bulundurun:

    var customers = new List<Customer>()             
     {
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer1"            
        },
                
        new Customer() {
            Id = Guid.NewGuid().ToString(),
            Name = "Customer2"            
        }
     };        
        
     var purchases = new List<Purchase>() 
     {
         new Purchase() {                
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[0].Id,
             Description = "Customer1-Purchase2"            
         },
         
         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase1"            
         },

         new Purchase() {
             Id = Guid.NewGuid().ToString(),
             CustomerId = customers[1].Id,
             Description = "Customer2-Purchase2"            
         }
      };
        
     var purchaseItems = new List<PurchaseItem>() 
     {
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[0].Id,
             Detail = "Purchase1-PurchaseItem1"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem1"            
         },
         
         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[1].Id,
             Detail = "Purchase2-PurchaseItem2"            
         },

         new PurchaseItem() {                
             Id = Guid.NewGuid().ToString(),
             PurchaseId= purchases[3].Id,
             Detail = "Purchase3-PurchaseItem1"
         }
     };

Şimdi, aşağıdaki linq sorgusunu düşünün:

    var result = from c in customers
                join p in purchases on c.Id equals p.CustomerId           // first join
                join pi in purchaseItems on p.Id equals pi.PurchaseId     // second join
                select new
                {
                   c.Name, p.Description, pi.Detail
                };


Yukarıdaki sorgunun sonucunu çıkarmak için:

    foreach(var resultItem in result)
    {
        Console.WriteLine($"{resultItem.Name}, {resultItem.Description}, {resultItem.Detail}");
    }
        
Sorgunun çıktısı şöyle olacaktır:

> Müşteri1, Müşteri1-Satın Alma1, Satın Alma1-Satın AlmaÖğe1
> 
> Müşteri1, Müşteri1-Satın Alma2, Satın Alma2-Satın AlmaÖğe1
> 
> Müşteri1, Müşteri1-Satın Alma2, Satın Alma2-Satın AlmaÜrün2
> 
> Müşteri2, Müşteri2-Satın Alma2, Satın Alma3-Satın AlmaÖğe1

[.NET Fiddle'da Canlı Demo][1]


[1]: https://dotnetfiddle.net/Db8uqp

## Birden çok tuşa katılma
  

      PropertyInfo[] stringProps = typeof (string).GetProperties();//string properties
      PropertyInfo[] builderProps = typeof(StringBuilder).GetProperties();//stringbuilder properties
        
        var query =
            from s in stringProps
            join b in builderProps
                on new { s.Name, s.PropertyType } equals new { b.Name, b.PropertyType }
            select new
            {
                s.Name,
                s.PropertyType,
                StringToken = s.MetadataToken,
                StringBuilderToken = b.MetadataToken
            };

Yukarıdaki "join"deki anonim türlerin aynı özellikleri içermesi gerektiğini unutmayın, çünkü nesneler yalnızca tüm özellikleri eşit olduğunda eşit kabul edilir. Aksi takdirde sorgu derlenmez.

## Toplam
'Enumerable.Sum' uzantı yöntemi, sayısal değerlerin toplamını hesaplar.

Koleksiyonun öğelerinin kendileri sayı olması durumunda, toplamı doğrudan hesaplayabilirsiniz.

    int[] numbers = new int[] { 1, 4, 6 };
    Console.WriteLine( numbers.Sum() ); //outputs 11

Öğelerin türünün karmaşık bir tür olması durumunda, hesaplanması gereken değeri belirtmek için bir lambda ifadesi kullanabilirsiniz:

    var totalMonthlySalary = employees.Sum( employee => employee.MonthlySalary );

Toplam uzatma yöntemi aşağıdaki türlerle hesaplayabilir:

-Int32
-Int64
- Bekar
- Çift Kişilik
- Ondalık

Koleksiyonunuzun null yapılabilir türler içermesi durumunda, boş öğeler için varsayılan bir değer ayarlamak üzere boş birleştirme operatörünü kullanabilirsiniz:

    int?[] numbers = new int?[] { 1, null, 6 };
    Console.WriteLine( numbers.Sum( number => number ?? 0 ) ); //outputs 7

## Aramak
> ToLookup, indekslemeye izin veren bir veri yapısı döndürür. O bir
> uzatma yöntemi. Dizine eklenebilen bir ILookup örneği üretir
> veya bir foreach döngüsü kullanılarak numaralandırılmıştır. Girişler birleştirilir
> her tuşta gruplamalar. - nokta ağları

    string[] array = { "one", "two", "three" };
    //create lookup using string length as key
    var lookup = array.ToLookup(item => item.Length);
    
    //join the values whose lengths are 3
    Console.WriteLine(string.Join(",",lookup[3]));
    //output: one,two

Başka bir örnek:

    int[] array = { 1,2,3,4,5,6,7,8 };
    //generate lookup for odd even numbers (keys will be 0 and 1)
    var lookup = array.ToLookup(item => item % 2);
    
    //print even numbers after joining
    Console.WriteLine(string.Join(",",lookup[0]));
    //output: 2,4,6,8

    //print odd numbers after joining
    Console.WriteLine(string.Join(",",lookup[1]));
    //output: 1,3,5,7

## Any and First(OrDefault) - en iyi uygulama
'Any' ve 'FirstOrDefault'un ne yaptığını açıklamayacağım çünkü bunlarla ilgili zaten iki iyi örnek var. https://www.wikiod.com/tr/docs/c%23/68/linq-queries/5098/any#t=201707200324548979636 ve https://www.wikiod.com/tr/docs/c%23/68/linq-queries/329 adresine bakın /first-firstordefault-last-lastordefault-single-and-singleordefault#t=201707200328069088515 daha fazla bilgi için.

Kodda sıklıkla gördüğüm ve **kaçınılması gereken** bir kalıp

    if (myEnumerable.Any(t=>t.Foo == "Bob"))
    {
        var myFoo = myEnumerable.First(t=>t.Foo == "Bob");
        //Do stuff
    }

Böyle daha verimli yazılabilirdi

    var myFoo = myEnumerable.FirstOrDefault(t=>t.Foo == "Bob");
    if (myFoo != null)
    {
        //Do stuff
    }

İkinci örnek kullanılarak, koleksiyon yalnızca bir kez aranır ve ilkiyle aynı sonucu verir. Aynı fikir 'Single' için de uygulanabilir.

## Toplama ve Sayıya Göre Gruplama
Örnek bir sınıf alalım:

    public class Transaction
    {
        public string Category { get; set; }
        public DateTime Date { get; set; }
        public decimal Amount { get; set; }
    }

Şimdi, işlemlerin bir listesini ele alalım:

    var transactions = new List<Transaction>
    {
       new Transaction { Category = "Saving Account", Amount = 56, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Saving Account", Amount = 10, Date = DateTime.Today.AddDays(-10) },
       new Transaction { Category = "Credit Card", Amount = 15, Date = DateTime.Today.AddDays(1) },
       new Transaction { Category = "Credit Card", Amount = 56, Date = DateTime.Today },
       new Transaction { Category = "Current Account", Amount = 100, Date = DateTime.Today.AddDays(5) },
    };

Kategori bazında miktar ve sayı toplamını hesaplamak istiyorsanız, GroupBy'yi aşağıdaki gibi kullanabilirsiniz:

    var summaryApproach1 = transactions.GroupBy(t => t.Category)
                               .Select(t => new
                               {
                                   Category = t.Key,
                                   Count = t.Count(),
                                   Amount = t.Sum(ta => ta.Amount),
                               }).ToList();
    
    Console.WriteLine("-- Summary: Approach 1 --");
    summaryApproach1.ForEach(
                row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

Alternatif olarak, bunu tek adımda yapabilirsiniz:

    var summaryApproach2 = transactions.GroupBy(t => t.Category, (key, t) =>
    {
            var transactionArray = t as Transaction[] ?? t.ToArray();
            return new
            {
                Category = key,
                Count = transactionArray.Length,
                Amount = transactionArray.Sum(ta => ta.Amount),
            };
    }).ToList();

    Console.WriteLine("-- Summary: Approach 2 --");
    summaryApproach2.ForEach(
    row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

Yukarıdaki sorguların her ikisi için çıktı aynı olacaktır:

> Kategori: Birikimli Hesap, Tutar: 66, Sayı: 2
>
> Kategori: Kredi Kartı, Tutar: 71, Sayı: 2
>
> Kategori: Cari Hesap, Tutar: 100, Sayı: 1

[.NET Fiddle'da Canlı Demo][1]


[1]: https://dotnetfiddle.net/1PfLGq#

## Tarafından sipariş
> Belirli bir değere göre bir koleksiyon sipariş eder.

Değer bir **tamsayı**, **double** veya **kayan** olduğunda, *minimum değer* ile başlar, bu, sıfırdan önce negatif değerleri ve ardından pozitif değerleri alacağınız anlamına gelir (bkz. Örnek 1).

Bir **char** ile sipariş verdiğinizde, yöntem, koleksiyonu sıralamak için karakterlerin *ascii değerlerini* karşılaştırır (bkz. Örnek 2).

**dizeleri** sıraladığınızda, OrderBy yöntemi onları [KültürBilgisi][1]'ne bakarak karşılaştırır, ancak normalde alfabedeki *ilk harf* ile başlar (a,b,c...).

Bu tür bir sıraya artan denir, bunun tersini istiyorsanız azalan gerekir (bkz. OrderByDescending).

**Örnek 1:**

    int[] numbers = {2, 1, 0, -1, -2};
    IEnumerable<int> ascending = numbers.OrderBy(x => x);
    // returns {-2, -1, 0, 1, 2}

**Örnek 2:**

     char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
     IEnumerable<char> ascending = letters.OrderBy(x => x);
     // returns { ' ', '!', '+', '1', '9', '?', 'A', 'B', 'Y', 'Z', '[', 'a', 'b', 'y', 'z', '{' }

**Örnek:**

    class Person
    {
       public string Name { get; set; }
       public int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var youngestPerson = people.OrderBy(x => x.Age).First();
    var name = youngestPerson.Name; // Bob


[1]: https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Seç - Öğeleri dönüştürme
Select, IEnumerable uygulayan herhangi bir veri yapısındaki her öğeye bir dönüşüm uygulamanıza olanak tanır.

Aşağıdaki listede her dizenin ilk karakterini almak:

    List<String> trees = new List<String>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };
    
Normal (lambda) sözdizimini kullanma

    //The below select stament transforms each element in tree into its first character.
    IEnumerable<String> initials = trees.Select(tree => tree.Substring(0, 1));
    foreach (String initial in initials) {
        System.Console.WriteLine(initial);
    }

**Çıktı:**
>O
>B
>B
>E
>H
>M

[.NET Fiddle'da Canlı Demo][1]

LINQ Sorgu Sözdizimini Kullanma

    initials = from tree in trees
               select tree.Substring(0, 1);


[1]: https://dotnetfiddle.net/yYLT0K

## Birlik
Varsayılan eşitlik karşılaştırıcısını kullanarak farklı bir koleksiyon oluşturmak için iki koleksiyonu birleştirir

    int[] numbers1 = { 1, 2, 3 };
    int[] numbers2 = { 2, 3, 4, 5 };
    
    var allElement = numbers1.Union(numbers2);   // AllElement now contains 1,2,3,4,5


[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/oet2Uq)

## Say ve LongCount
"Sayım", bir "IEnumerable<T>" içindeki öğelerin sayısını döndürür. 'Count' ayrıca, saymak istediğiniz öğeleri filtrelemenize izin veren isteğe bağlı bir yüklem parametresini de ortaya çıkarır.

    int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };
    
    int n = array.Count(); // returns the number of elements in the array
    int x = array.Count(i => i > 2); // returns the number of elements in the array greater than 2

"LongCount", "Count" ile aynı şekilde çalışır ancak "long" dönüş türüne sahiptir ve "int.MaxValue"dan daha uzun "IEnumerable<T>" dizilerini saymak için kullanılır.

    int[] array = GetLargeArray();
    
    long n = array.LongCount(); // returns the number of elements in the array
    long x = array.LongCount(i => i > 100); // returns the number of elements in the array greater than 100


## Kademeli olarak bir sorgu oluşturma
LINQ **ertelenmiş yürütme** kullandığından, gerçekte değerleri içermeyen ancak değerlendirildiğinde değerleri döndüren bir sorgu nesnesine sahip olabiliriz. Böylece sorguyu kontrol akışımıza göre dinamik olarak oluşturabilir ve işimiz bittiğinde değerlendirebiliriz:

    IEnumerable<VehicleModel> BuildQuery(int vehicleType, SearchModel search, int start = 1, int count = -1) {
        IEnumerable<VehicleModel> query = _entities.Vehicles
            .Where(x => x.Active && x.Type == vehicleType)
            .Select(x => new VehicleModel {
                Id = v.Id,
                Year = v.Year,
                Class = v.Class,
                Make = v.Make,
                Model = v.Model,
                Cylinders = v.Cylinders ?? 0
            });

Koşullu olarak filtreler uygulayabiliriz:

        if (!search.Years.Contains("all", StringComparer.OrdinalIgnoreCase))
            query = query.Where(v => search.Years.Contains(v.Year));

        if (!search.Makes.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Makes.Contains(v.Make));
        }

        if (!search.Models.Contains("all", StringComparer.OrdinalIgnoreCase)) {
            query = query.Where(v => search.Models.Contains(v.Model));
        }

        if (!search.Cylinders.Equals("all", StringComparer.OrdinalIgnoreCase)) {
            decimal minCylinders = 0;
            decimal maxCylinders = 0;
            switch (search.Cylinders) {
                case "2-4":
                    maxCylinders = 4;
                    break;
                case "5-6":
                    minCylinders = 5;
                    maxCylinders = 6;
                    break;
                case "8":
                    minCylinders = 8;
                    maxCylinders = 8;
                    break;
                case "10+":
                    minCylinders = 10;
                    break;
            }
            if (minCylinders > 0) {
                query = query.Where(v => v.Cylinders >= minCylinders);
            }
            if (maxCylinders > 0) {
                query = query.Where(v => v.Cylinders <= maxCylinders);
            }
        }

Bir koşula göre sorguya bir sıralama düzeni ekleyebiliriz:

        switch (search.SortingColumn.ToLower()) {
            case "make_model":
                query = query.OrderBy(v => v.Make).ThenBy(v => v.Model);
                break;
            case "year":
                query = query.OrderBy(v => v.Year);
                break;
            case "engine_size":
                query = query.OrderBy(v => v.EngineSize).ThenBy(v => v.Cylinders);
                break;
            default:
                query = query.OrderBy(v => v.Year); //The default sorting.
        }
        
Sorgumuz belirli bir noktadan başlayacak şekilde tanımlanabilir:
        
        query = query.Skip(start - 1);
        
ve belirli sayıda kayıt döndürmek için tanımlanmıştır:

        if (count > -1) {
            query = query.Take(count);
        }
        return query;
    }
    
---

Sorgu nesnesine sahip olduğumuzda, sonuçları bir "foreach" döngüsüyle veya "ToList" veya "ToArray" gibi bir dizi değer döndüren LINQ yöntemlerinden biriyle değerlendirebiliriz:

    SearchModel sm;
    
    // populate the search model here
    // ...
    
    List<VehicleModel> list = BuildQuery(5, sm).ToList();

## Dış aralık değişkeniyle GroupJoin


    Customer[] customers = Customers.ToArray();
    Purchase[] purchases = Purchases.ToArray();
    
    var groupJoinQuery =
        from c in customers
        join p in purchases on c.ID equals p.CustomerID
        into custPurchases
        select new
        {
            CustName = c.Name,
            custPurchases
        };



## Linq Niceleyiciler
Bir dizideki öğelerin bazıları veya tümü bir koşulu karşılıyorsa, niceleyici işlemleri bir Boole değeri döndürür. Bu yazıda, bu operatörleri kullanabileceğimiz bazı yaygın LINQ to Objects senaryolarını göreceğiz.
LINQ'da kullanılabilecek 3 Niceleyici işlemi vardır:

"Tümü" - bir dizideki tüm öğelerin bir koşulu karşılayıp karşılamadığını belirlemek için kullanılır.
Örneğin:

    int[] array = { 10, 20, 30 }; 
       
    // Are all elements >= 10? YES
    array.All(element => element >= 10); 
       
    // Are all elements >= 20? NO
    array.All(element => element >= 20);
        
    // Are all elements < 40? YES
    array.All(element => element < 40);
    
"Herhangi" - bir dizideki herhangi bir öğenin bir koşulu karşılayıp karşılamadığını belirlemek için kullanılır.
Örneğin:

    int[] query=new int[] { 2, 3, 4 }
    query.Any (n => n == 3);

"İçerir" - bir dizinin belirli bir öğe içerip içermediğini belirlemek için kullanılır.
Örneğin:

    //for int array
    int[] query =new int[] { 1,2,3 };
    query.Contains(1);
    
    //for string array
    string[] query={"Tom","grey"};
    query.Contains("Tom");
    
    //for a string
    var stringValue="hello";
    stringValue.Contains("h");



## Alırken
"TakeWhile", koşul doğru olduğu sürece bir dizideki öğeleri döndürür

    int[] list = { 1, 10, 40, 50, 44, 70, 4 };
    var result = list.TakeWhile(item => item < 50).ToList();
    // result = { 1, 10, 40 }

## IEnumerable<T> için kendi Linq operatörlerinizi oluşturun
Linq ile ilgili harika şeylerden biri, genişletmenin çok kolay olmasıdır. Argümanı 'IEnumerable<T>' olan bir [uzantı yöntemi][1] oluşturmanız yeterlidir.

    public namespace MyNamespace
    {
        public static class LinqExtensions
        {
            public static IEnumerable<List<T>> Batch<T>(this IEnumerable<T> source, int batchSize)
            {
                var batch = new List<T>();
                foreach (T item in source)
                {
                    batch.Add(item);
                    if (batch.Count == batchSize)
                    {
                        yield return batch;
                        batch = new List<T>();
                    }
                }
                if (batch.Count > 0)
                    yield return batch;
            }
        }
    }

Bu örnek, bir "IEnumerable<T>" içindeki öğeleri, sabit boyutlu listelere böler; son liste, öğelerin geri kalanını içerir. Uzantı yönteminin uygulandığı nesnenin, "this" anahtar sözcüğü kullanılarak ilk bağımsız değişken olarak ("kaynak" bağımsız değişkenine) nasıl aktarıldığına dikkat edin. Ardından, o noktadan itibaren yürütmeye devam etmeden önce 'IEnumerable<T>' çıktısındaki bir sonraki öğenin çıktısını almak için 'yield' anahtar sözcüğü kullanılır (bkz. [verim anahtar sözcüğü][2]).

Bu örnek, kodunuzda şu şekilde kullanılacaktır:

    //using MyNamespace;
    var items = new List<int> { 2, 3, 4, 5, 6 };
    foreach (List<int> sublist in items.Batch(3))
    {
        // do something
    }

İlk döngüde, alt liste `{2, 3, 4}` ve ikinci `{5, 6}` olacaktır.

Özel LinQ yöntemleri, standart LinQ yöntemleriyle de birleştirilebilir. Örneğin.:

    //using MyNamespace;
    var result = Enumerable.Range(0, 13)         // generate a list
                           .Where(x => x%2 == 0) // filter the list or do something other
                           .Batch(3)             // call our extension method
                           .ToList()             // call other standard methods

Bu sorgu, gruplar halinde gruplanmış çift sayıları 3 boyutunda döndürür: `{0, 2, 4}, {6, 8, 10}, {12}`

Uzantı yöntemine erişebilmek için `using MyNamespace;` satırına ihtiyacınız olduğunu unutmayın.

[1]: https://www.wikiod.com/tr/docs/c%23/20/extension-methods/33/using-an-extension-method#t=201607280952261411896
[2]: https://www.wikiod.com/tr/docs/c%23/61/yield-keyword#t=201607281004094832778

## Tersi

- Bir dizideki öğelerin sırasını tersine çevirir.
- Herhangi bir öğe yoksa, bir `ArgumentNullException: source is null.` öğesini atar.

***Örnek:***

    // Create an array.
    int[] array = { 1, 2, 3, 4 };                         //Output:
    // Call reverse extension method on the array.        //4
    var reverse = array.Reverse();                        //3
    // Write contents of array to screen.                 //2
    foreach (int value in reverse)                        //1
        Console.WriteLine(value);
  
[Canlı kod örneği](https://dotnetfiddle.net/ckrWUo)

LINQ ifadelerinizin zincir sırasına bağlı olarak `Reverse()`nin farklı çalışabileceğini unutmayın.

            //Create List of chars
            List<int> integerlist = new List<int>() { 1, 2, 3, 4, 5, 6 };

            //Reversing the list then taking the two first elements
            IEnumerable<int> reverseFirst = integerlist.Reverse<int>().Take(2);
            
            //Taking 2 elements and then reversing only thos two
            IEnumerable<int> reverseLast = integerlist.Take(2).Reverse();
            
            //reverseFirst output: 6, 5
            //reverseLast output:  2, 1

[Canlı kod örneği](https://dotnetfiddle.net/ckrWUo)

*Reverse()*, her şeyi arabelleğe alarak çalışır, ardından geriye doğru yürür, bu çok verimli değildir, ancak OrderBy de bu açıdan değildir.

LINQ-to-Objects'de arabelleğe alma işlemleri (Reverse, OrderBy, GroupBy, vb.) ve arabelleğe alma olmayan işlemler (Where, Take, Skip, vb.) vardır.


***Örnek: Tamponsuz Ters genişletme***
    
    public static IEnumerable<T> Reverse<T>(this IList<T> list) {
        for (int i = list.Count - 1; i >= 0; i--) 
            yield return list[i];
    }

[Canlı kod örneği](https://dotnetfiddle.net/ckrWUo)

Listeyi yinelerken değiştirirseniz, bu yöntem sorunlarla karşılaşabilir.





## Azalan Sırala
> Belirli bir değere göre bir koleksiyon sipariş eder.

Değer bir **tamsayı**, **double** veya **kayan** olduğunda, *maksimum değer* ile başlar, bu, sıfırdan önce pozitif değerleri ve ardından negatif değerleri alacağınız anlamına gelir (bkz. Örnek 1).

Bir **char** ile sipariş verdiğinizde, yöntem, koleksiyonu sıralamak için karakterlerin *ascii değerlerini* karşılaştırır (bkz. Örnek 2).

**dizeleri** sıraladığınızda, OrderBy yöntemi onları [KültürBilgisi][1]'ne bakarak karşılaştırır, ancak normalde alfabedeki *son harfle* başlar (z,y,x,...).

Bu tür bir düzene azalan denir, bunun tersini istiyorsanız, artan bir sıraya ihtiyacınız vardır (bkz. OrderBy).

**Örnek 1:**

    int[] numbers = {-2, -1, 0, 1, 2};
    IEnumerable<int> descending = numbers.OrderByDescending(x => x);
    // returns {2, 1, 0, -1, -2}

**Örnek 2:**

    char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
    IEnumerable<char> descending = letters.OrderByDescending(x => x);
    // returns { '{', 'z', 'y', 'b', 'a', '[', 'Z', 'Y', 'B', 'A', '?', '9', '1', '+', '!', ' ' }

**Örnek 3:**
    
    class Person
    {
       public  string Name { get; set; }
       public  int Age { get; set; }
    }

    var people = new[]
    {
        new Person {Name = "Alice", Age = 25},
        new Person {Name = "Bob", Age = 21},
        new Person {Name = "Carol", Age = 43}
    };
    var oldestPerson = people.OrderByDescending(x => x.Age).First();
    var name = oldestPerson.Name; // Carol


[1]: https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx

## Uyum
İki koleksiyonu birleştirir (yinelenenleri kaldırmadan)

    List<int> foo = new List<int> { 1, 2, 3 };
    List<int> bar = new List<int> { 3, 4, 5 };

    // Through Enumerable static class
    var result = Enumerable.Concat(foo, bar).ToList(); // 1,2,3,3,4,5

    // Through extension method
    var result = foo.Concat(bar).ToList(); // 1,2,3,3,4,5

## Func<TSource, int, TResult> seçiciyle seçin - Öğelerin sıralamasını almak için kullanın
'Select' uzantı yöntemlerinin aşırı yüklemelerinden biri, 'seçilen' koleksiyondaki geçerli öğenin 'index'ini de geçer. Bunlar onun birkaç kullanımı.

**Öğelerin "satır numarasını" alın**

    var rowNumbers = collection.OrderBy(item => item.Property1)
                               .ThenBy(item => item.Property2)
                               .ThenByDescending(item => item.Property3)
                               .Select((item, index) => new { Item = item, RowNumber = index })
                               .ToList();

**Grup içindeki* bir öğenin derecesini alın**

    var rankInGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .SelectMany(group => group.OrderBy(item => item.Property2)
                                                       .ThenByDescending(item => item.Property3)
                                                       .Select((item, index) => new 
                                                       { 
                                                           Item = item, 
                                                           RankInGroup = index 
                                                       })).ToList();

**Grupların sıralamasını alın (Oracle'da yoğun_rank olarak da bilinir)**

    var rankOfBelongingGroup = collection.GroupBy(item => item.Property1)
                                .OrderBy(group => group.Key)
                                .Select((group, index) => new
                                {
                                    Items = group,
                                    Rank = index
                                })
                                .SelectMany(v => v.Items, (s, i) => new
                                {
                                    Item = i,
                                    DenseRank = s.Rank
                                }).ToList();

Bunu test etmek için şunları kullanabilirsiniz:

    public class SomeObject
    {
        public int Property1 { get; set; }
        public int Property2 { get; set; }
        public int Property3 { get; set; }

        public override string ToString()
        {
            return string.Join(", ", Property1, Property2, Property3);
        }
    }

Ve veriler:

    List<SomeObject> collection = new List<SomeObject>
    {
        new SomeObject { Property1 = 1, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 1, Property2 = 2, Property3 = 2},
        new SomeObject { Property1 = 2, Property2 = 1, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
        new SomeObject { Property1 = 2, Property2 = 3, Property3 = 1}
    };


