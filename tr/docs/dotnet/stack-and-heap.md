---
title: "Yığın ve Yığın"
slug: "ygn-ve-ygn"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Bir referans türü bildirildiğinde, başlangıç ​​değerinin "null" olacağını belirtmekte fayda var. Bunun nedeni, henüz bellekte bir konuma işaret etmemesi ve tamamen geçerli bir durum olmasıdır.
Ancak, null yapılabilir türler dışında, değer türleri genellikle her zaman bir değere sahip olmalıdır.



## Kullanımdaki değer türleri
Değer türleri yalnızca bir _**değer**_ içerir.

Tüm değer türleri, [System.ValueType][1] sınıfından türetilir ve bu, yerleşik türlerin çoğunu içerir.

Yeni bir değer türü oluştururken, __* yığın*__ adlı bir bellek alanı kullanılır.
Yığın, beyan edilen türün boyutuna göre buna göre büyüyecektir. Örneğin, bir int, yığında her zaman 32 bit bellek tahsis edilecektir. Değer türü artık kapsamda olmadığında, yığındaki alan serbest bırakılır.

Aşağıdaki kod, yeni bir değişkene atanan bir değer türünü gösterir. Özel bir değer türü oluşturmanın uygun bir yolu olarak bir yapı kullanılıyor (System.ValueType sınıfı başka türlü genişletilemez).

Anlaşılması gereken önemli şey, bir değer türü atarken, değerin kendisinin yeni değişkene _**kopyalanmasıdır**_, yani nesnenin birbirini etkileyemeyen iki farklı örneğine sahip olduğumuz anlamına gelir.

    struct PersonAsValueType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsValueType personA;

            personA.Name = "Bob";

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(                // Outputs 'False' - because 
                object.ReferenceEquals(       // personA and personB are referencing 
                    personA,                  // different areas of memory
                    personB));                

            Console.WriteLine(personA.Name);  // Outputs 'Linda'
            Console.WriteLine(personB.Name);  // Outputs 'Bob'
        }
    }


[1]: https://msdn.microsoft.com/en-us/library/system.valuetype.aspx

## Kullanılan referans türleri
Referans türleri, hem bir bellek alanına bir _**referans**_ hem de bu alanda depolanan bir _**değerden**_ oluşur.
Bu, C/C++'daki işaretçilere benzer.

Tüm referans türleri, _**yığın**_ olarak bilinen şeyde depolanır.
Yığın, nesnelerin depolandığı yönetilen bir bellek alanıdır. Yeni bir nesne başlatıldığında, yığının bir kısmı o nesne tarafından kullanılmak üzere tahsis edilecek ve yığının o konumuna bir başvuru döndürülecektir. Yığın, _çöp toplayıcı_ tarafından yönetilir ve korunur ve manuel müdahaleye izin vermez.

Örneğin kendisi için gereken bellek alanına ek olarak, .NET CLR'nin gerektirdiği ek geçici bilgilerle birlikte başvurunun kendisini depolamak için ek alan gerekir.

Aşağıdaki kod, yeni bir değişkene atanan bir referans türünü gösterir. Bu örnekte bir sınıf kullanıyoruz, tüm sınıflar referans türleridir (statik olsalar bile).

Bir referans türü başka bir değişkene atandığında, üzerine kopyalanan nesnenin _**referansı**_, değerin kendisi __değildir. Bu, değer türleri ve referans türleri arasındaki önemli bir ayrımdır.

Bunun sonuçları, artık aynı nesneye _iki_ referansımız olmasıdır.
Bu nesne içindeki değerlerde yapılan herhangi bir değişiklik, her iki değişken tarafından da yansıtılacaktır.

    class PersonAsReferenceType
    {
        public string Name;
    }

    class Program
    {
        static void Main()
        {
            PersonAsReferenceType personA;

            personA = new PersonAsReferenceType { Name = "Bob" };

            var personB = personA;

            personA.Name = "Linda";

            Console.WriteLine(               // Outputs 'True' - because
                object.ReferenceEquals(      // personA and personB are referencing 
                    personA,                 // the *same* memory location
                    personB));

            Console.WriteLine(personA.Name); // Outputs 'Linda'
            Console.WriteLine(personB.Name); // Outputs 'Linda'
        }

