---
title: "demetler"
slug: "demetler"
draft: false
images: []
weight: 9908
type: docs
toc: true
---

## Tuple öğelerine erişme
Tuple elemanlarına erişmek için 'Item1'-'Item8' özelliklerini kullanın. Yalnızca dizin numarası tanımlama grubu boyutuna eşit veya daha küçük olan özellikler kullanılabilir olacaktır (yani, "Tuple<T1,T2>" içindeki "Item3" özelliğine erişilemez).

    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());
    var item1 = tuple.Item1; // "foo"
    var item2 = tuple.Item2; // 123
    var item3 = tuple.Item3; // true
    var item4 = tuple.Item4; // new My Class()

## Tuple oluşturma
Tuple'lar, `Tuple<T1>`-`Tuple<T1,T2,T3,T4,T5,T6,T7,T8>` genel türleri kullanılarak oluşturulur. Türlerin her biri, 1 ila 8 eleman içeren bir demeti temsil eder. Elementler farklı tiplerde olabilir.

    // tuple with 4 elements
    var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());

Tuple'lar, statik "Tuple.Create" yöntemleri kullanılarak da oluşturulabilir. Bu durumda, öğelerin türleri C# Derleyicisi tarafından belirlenir.

    // tuple with 4 elements
    var tuple = Tuple.Create("foo", 123, true, new MyClass());
<!-- eğer sürüm [gte 7.0] -->
C# 7.0'dan beri, Tuple'lar [ValueTuple][1] kullanılarak kolayca oluşturulabilir.

    var tuple = ("foo", 123, true, new MyClass());

Elemanlar daha kolay ayrıştırma için isimlendirilebilir.

    (int number, bool flag, MyClass instance) tuple = (123, true, new MyClass());

<!-- eğer --> son sürüm


[1]: https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

## Tuple'ları karşılaştırma ve sıralama
Tuple'lar, elementlerine göre karşılaştırılabilir.

Örnek olarak, öğeleri "Tuple" türünden bir numaralandırılabilir, belirtilen bir öğe üzerinde tanımlanan karşılaştırma operatörlerine göre sıralanabilir:

    List<Tuple<int, string>> list = new List<Tuple<int, string>>();
    list.Add(new Tuple<int, string>(2, "foo"));
    list.Add(new Tuple<int, string>(1, "bar"));
    list.Add(new Tuple<int, string>(3, "qux"));

    list.Sort((a, b) => a.Item2.CompareTo(b.Item2)); //sort based on the string element

    foreach (var element in list) {
        Console.WriteLine(element);
    }
    
    // Output:
    // (1, bar)
    // (2, foo)
    // (3, qux)

Veya sıralamayı tersine çevirmek için şunu kullanın:

    list.Sort((a, b) => b.Item2.CompareTo(a.Item2));

## Bir yöntemden birden çok değer döndür
Tuple'lar, out parametreleri kullanmadan bir yöntemden birden çok değer döndürmek için kullanılabilir. Aşağıdaki örnekte "AddMultiply" iki değer (toplam, ürün) döndürmek için kullanılır.

    void Write()
    {
        var result = AddMultiply(25, 28);
        Console.WriteLine(result.Item1);
        Console.WriteLine(result.Item2);
    }
 
    Tuple<int, int> AddMultiply(int a, int b)
    {
        return new Tuple<int, int>(a + b, a * b);
    }

Çıktı:

> 53
> 700


Şimdi C# 7.0, değer gruplarını kullanan yöntemlerden birden çok değer döndürmenin alternatif bir yolunu sunuyor [`ValueTuple` yapısı hakkında daha fazla bilgi][1].


[1]: https://www.wikiod.com/tr/docs/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514

