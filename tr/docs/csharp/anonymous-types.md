---
title: "anonim türler"
slug: "anonim-turler"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

## Anonim ve dinamik
Anonim türler, statik tür denetimini sürdürürken, türlerini önceden açıkça tanımlamak zorunda kalmadan nesnelerin oluşturulmasına izin verir.

    var anon = new { Value = 1 };
    Console.WriteLine(anon.Id); // compile time error

Tersine, "dinamik", derleme zamanı hataları yerine çalışma zamanı hatalarını seçen dinamik tip denetimine sahiptir.
    
    dynamic val = "foo";
    Console.WriteLine(val.Id); // compiles, but throws runtime error

## Anonim bir tür oluşturma
Anonim türler adlandırılmadığından, bu türlerin değişkenleri örtük olarak yazılmalıdır ('var').

    var anon = new { Foo = 1, Bar = 2 };
    // anon.Foo == 1
    // anon.Bar == 2
    
Üye adları belirtilmemişse, nesneyi başlatmak için kullanılan özelliğin/değişkenin adına ayarlanırlar.

    int foo = 1;
    int bar = 2;
    var anon2 = new { foo, bar };
    // anon2.foo == 1
    // anon2.bar == 2

Adların yalnızca anonim tür bildirimindeki ifade basit bir özellik erişimi olduğunda atlanabileceğini unutmayın; yöntem çağrıları veya daha karmaşık ifadeler için bir özellik adı belirtilmelidir.

    string foo = "some string";
    var anon3 = new { foo.Length };
    // anon3.Length == 11
    var anon4 = new { foo.Length <= 10 ? "short string" : "long string" };
    // compiler error - Invalid anonymous type member declarator.
    var anon5 = new { Description = foo.Length <= 10 ? "short string" : "long string" };
    // OK

## Anonim tip eşitliği
Anonim tür eşitliği, 'Equals' örnek yöntemiyle verilir. Her özellik için aynı türe ve eşit değerlere sahiplerse ('a.Prop.Equals(b.Prop)' aracılığıyla) iki nesne eşittir.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 1, Bar = 2 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon3 = new { Foo = 5, Bar = 10 };
    var anon4 = new { Bar = 2, Foo = 1 };
    // anon.Equals(anon2) == true
    // anon.Equals(anon3) == false
    // anon.Equals(anon4) == false (anon and anon4 have different types, see below)

İki anonim tür, ancak ve ancak özellikleri aynı ada ve türe sahipse ve aynı sırada görünüyorsa aynı kabul edilir.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 7, Bar = 1 };
    var anon3 = new { Bar = 1, Foo = 3 };
    var anon4 = new { Fa = 1, Bar = 2 };
    // anon and anon2 have the same type
    // anon and anon3 have diferent types (Bar and Foo appear in different orders)
    // anon and anon4 have different types (property names are different)

## Anonim türlerle genel yöntemler
Genel yöntemler, tür çıkarımı yoluyla anonim türlerin kullanımına izin verir.

    void Log<T>(T obj) {
        // ...
    }
    Log(new { Value = 10 });

Bu, LINQ ifadelerinin anonim türlerle kullanılabileceği anlamına gelir:

    var products = new[] {
        new { Amount = 10, Id = 0 },
        new { Amount = 20, Id = 1 },
        new { Amount = 15, Id = 2 }
    };
    var idsByAmount = products.OrderBy(x => x.Amount).Select(x => x.Id);
    // idsByAmount: 0, 2, 1

## Anonim türlerle genel türleri örnekleme
Genel kurucuların kullanılması, anonim türlerin adlandırılmasını gerektirir, bu mümkün değildir. Alternatif olarak, tür çıkarımının gerçekleşmesine izin vermek için genel yöntemler kullanılabilir.

    var anon = new { Foo = 1, Bar = 2 };
    var anon2 = new { Foo = 5, Bar = 10 };
    List<T> CreateList<T>(params T[] items) {
        return new List<T>(items);
    }
    
    var list1 = CreateList(anon, anon2);

"List<T>" durumunda, örtük olarak yazılan diziler, "ToList" LINQ yöntemi aracılığıyla bir "List<T>"ye dönüştürülebilir:

    var list2 = new[] {anon, anon2}.ToList();

## Örtülü olarak yazılan diziler
Örtük yazma ile anonim tür dizileri oluşturulabilir.

    var arr = new[] {
        new { Id = 0 },
        new { Id = 1 }
    };

