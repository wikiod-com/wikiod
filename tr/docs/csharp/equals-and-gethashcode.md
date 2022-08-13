---
title: "Equals ve GetHashCode"
slug: "equals-ve-gethashcode"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Her "Equals" uygulaması aşağıdaki gereksinimleri karşılamalıdır:

- **Dönüşlü**: Bir nesne kendisine eşit olmalıdır.<br/>`x.Equals(x)```true` değerini döndürür.

- **Simetrik**: x'i y'ye veya y'ye x'i karşılaştırırsam hiçbir fark yoktur - sonuç aynıdır. <br/>`x.Equals(y)`, `y.Equals(x)` ile aynı değeri döndürür.

- **Geçişli**: Bir nesne başka bir nesneye eşitse ve bu üçüncü nesneye eşitse, ilki üçüncüye eşit olmalıdır.<br/>if `(x.Equals(y) && y .Equals(z))`, "true" değerini, ardından "x.Equals(z)", "true" değerini döndürür.

- **Tutarlı**: Bir nesneyi başka bir nesneyle birden çok kez karşılaştırırsanız, sonuç her zaman aynıdır.<br/>Ardışık "x.Equals(y)" çağrıları, nesneler tarafından başvuruda bulunulduğu sürece aynı değeri döndürür. x ve y değiştirilmez.

- **null ile karşılaştırma**: Hiçbir nesne "null" değerine eşit değildir.<br/>"x.Equals(null)", "false" değerini döndürür.

GetHashCode'un Uygulamaları:

- **"Equals"** ile uyumlu: İki nesne eşitse ("Equals" öğesinin doğru olduğu anlamına gelir), o zaman "GetHashCode" **her biri için aynı değeri döndürmelidir**.

- **Geniş aralık**: İki nesne eşit değilse ("Equals" yanlış diyorsa), **yüksek bir olasılık** olmalıdır, bunların hash kodları farklıdır. *Mükemmel* karma, seçilebilecek sınırlı sayıda değer olduğundan genellikle mümkün değildir.

- **Ucuz**: Her durumda hash kodunu hesaplamak ucuz olmalıdır.

Bakınız: [Equals() ve Operatör Aşırı Yükleme Yönergeleri ==](https://msdn.microsoft.com/en-us/library/ms173147.aspx)


## İyi bir GetHashCode geçersiz kılma yazma
GetHashCode'un Sözlük<> ve HashTable üzerinde önemli performans etkileri vardır.

İyi "GetHashCode" Yöntemleri

- eşit bir dağılıma sahip olmalıdır
- her tamsayı, rastgele bir örnek için kabaca eşit bir geri dönüş şansına sahip olmalıdır
- yönteminiz her örnek için aynı tamsayıyı (ör. '999' sabiti) döndürürse, performansınız kötü olur
- hızlı olmalı
- Bunlar, yavaşlığın bir özellik olduğu kriptografik karmalar DEĞİLDİR
- hash fonksiyonunuz ne kadar yavaşsa, sözlüğünüz o kadar yavaş olur
- "Eşittir"in true olarak değerlendirdiği iki örnekte aynı HashCode'u döndürmelidir
- yapmazlarsa (örneğin, "GetHashCode" rastgele bir sayı döndürdüğü için), öğeler "Liste", "Sözlük" veya benzerlerinde bulunmayabilir.

GetHashCode'u uygulamak için iyi bir yöntem, başlangıç ​​değeri olarak bir asal sayı kullanmak ve diğer asal sayılarla çarpılan türdeki alanların karma kodlarını buna eklemektir:

    public override int GetHashCode()
    {
        unchecked // Overflow is fine, just wrap
        {
            int hash = 3049; // Start value (prime number).

            // Suitable nullity checks etc, of course :)
            hash = hash * 5039 + field1.GetHashCode();
            hash = hash * 883 + field2.GetHashCode();
            hash = hash * 9719 + field3.GetHashCode();
            return hash;
        }
    }

Karma işlevi için yalnızca "Equals" yönteminde kullanılan alanlar kullanılmalıdır.

Dictionary/HashTable'lar için aynı türü farklı şekillerde ele almanız gerekiyorsa, IEqualityComparer<T> kullanabilirsiniz.

## Varsayılan Eşittir davranışı.
"Equals", "Object" sınıfının kendisinde bildirilir.

    public virtual bool Equals(Object obj);

Varsayılan olarak, "Equals" aşağıdaki davranışa sahiptir:

- Örnek bir referans türüyse, "Equals", yalnızca referanslar aynıysa true değerini döndürür.

- Örnek bir değer türüyse, "Equals" yalnızca tür ve değer aynıysa true değerini döndürür.

- "string" özel bir durumdur. Bir değer türü gibi davranır.


    namespace ConsoleApplication
    {
        public class Program
        {
            public static void Main(string[] args)
            {
                //areFooClassEqual: False
                Foo fooClass1 = new Foo("42");
                Foo fooClass2 = new Foo("42");
                bool areFooClassEqual = fooClass1.Equals(fooClass2);
                Console.WriteLine("fooClass1 and fooClass2 are equal: {0}", areFooClassEqual);
                //False
    
                //areFooIntEqual: True
                int fooInt1 = 42;
                int fooInt2 = 42;
                bool areFooIntEqual = fooInt1.Equals(fooInt2);
                Console.WriteLine("fooInt1 and fooInt2 are equal: {0}", areFooIntEqual);
    
                //areFooStringEqual: True
                string fooString1 = "42";
                string fooString2 = "42";
                bool areFooStringEqual = fooString1.Equals(fooString2);
                Console.WriteLine("fooString1 and fooString2 are equal: {0}", areFooStringEqual);
            }
        }
    
        public class Foo
        {
            public string Bar { get; }
    
            public Foo(string bar)
            {
                Bar = bar;
            }
        }
    }

## Özel türlerde Equals ve GetHashCode'u geçersiz kıl
gibi bir 'Kişi' sınıfı için:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }
    
    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

    bool result = person1.Equals(person2); //false because it's reference Equals

Ancak 'Equals' ve 'GetHashCode' aşağıdaki gibi tanımlanır:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }

        public override bool Equals(object obj)
        {
            var person = obj as Person;
            if(person == null) return false;
            return Name == person.Name && Age == person.Age; //the clothes are not important when comparing two persons
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode()*Age;
        }
    }

    var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
    var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };
    
    bool result = person1.Equals(person2); // result is true

Ayrıca kişiler üzerinde farklı sorgular yapmak için LINQ kullanmak, hem "Equals" hem de "GetHashCode"u kontrol edecektir:

    var persons = new List<Person>
    {
         new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
         new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
         new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();//distinctPersons has Count = 2

## IEqualityComparator'da Equals ve GetHashCode
Verilen tip 'Kişi' için:

    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
        public string Clothes { get; set; }
    }

    List<Person> persons = new List<Person>
    {
        new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
        new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
        new Person{ Name = "Jon", Age = 20, Clothes = ""}
    };

    var distinctPersons = persons.Distinct().ToList();// distinctPersons has Count = 3

Ancak 'Equals' ve 'GetHashCode'u bir 'IEqualityComparator' olarak tanımlamak:

    public class PersonComparator : IEqualityComparer<Person>
    {
        public bool Equals(Person x, Person y)
        {
            return x.Name == y.Name && x.Age == y.Age; //the clothes are not important when comparing two persons;
        }

        public int GetHashCode(Person obj) { return obj.Name.GetHashCode() * obj.Age; }
    }

    var distinctPersons = persons.Distinct(new PersonComparator()).ToList();// distinctPersons has Count = 2

Bu sorgu için, hem "Equals" true döndürdü hem de "GetHashCode" iki kişi için aynı karma kodunu döndürdüyse, iki nesnenin eşit olarak kabul edildiğini unutmayın.

