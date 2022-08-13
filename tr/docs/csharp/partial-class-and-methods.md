---
title: "Kısmi sınıf ve yöntemler"
slug: "ksmi-snf-ve-yontemler"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

Kısmi sınıflar bize sınıfları birden çok parçaya ve birden çok kaynak dosyaya bölme seçeneği sunar. Derleme süresi boyunca tüm parçalar tek bir sınıfta birleştirilir. Tüm parçalar 'kısmi' anahtar kelimesini içermeli, aynı erişilebilirliğe sahip olmalıdır. Derleme sırasında dahil edilebilmesi için tüm parçalar aynı derlemede bulunmalıdır.

## Sözdizimi
- genel **kısmi** sınıfı MyPartialClass { }

- Kısmi sınıflar, genişlettikleri sınıfla aynı derleme ve ad alanı içinde tanımlanmalıdır.

- Sınıfın tüm bölümleri 'partial' anahtar sözcüğünü kullanmalıdır.

- Sınıfın tüm bölümleri aynı erişilebilirliğe sahip olmalıdır; "genel"/"korumalı"/"özel" vb.

- Herhangi bir kısım 'abstract' anahtar sözcüğünü kullanıyorsa, birleşik tür soyut olarak kabul edilir.

- Herhangi bir parça 'mühürlü' anahtar kelimesini kullanıyorsa, birleşik tip mühürlü olarak kabul edilir.

- Herhangi bir parça temel türü kullanıyorsa, birleşik tür bu türden devralır.

- Birleştirilmiş tür, tüm kısmi sınıflarda tanımlanan tüm arabirimleri devralır.

## Kısmi sınıflar
Kısmi sınıflar, sınıf bildirimini (genellikle ayrı dosyalara) bölme yeteneği sağlar. Kısmi sınıflarla çözülebilecek yaygın bir sorun, kullanıcıların otomatik oluşturulan kodu, kod yeniden oluşturulursa değişikliklerinin üzerine yazılacağından korkmadan değiştirmelerine izin vermektir. Ayrıca birden fazla geliştirici aynı sınıf veya yöntemler üzerinde çalışabilir.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass
        {
            public void ExampleMethod() {
                Console.WriteLine("Method call from the first declaration.");
            }
        }
    
        public partial class PartialClass
        {
            public void AnotherExampleMethod()
            {
                Console.WriteLine("Method call from the second declaration.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.ExampleMethod(); // outputs "Method call from the first declaration."
                partial.AnotherExampleMethod(); // outputs "Method call from the second declaration."
            }
        }
    }

## Kısmi yöntemler
Kısmi yöntem, bir kısmi sınıf bildirimindeki tanımdan (ortak bir senaryo olarak - otomatik olarak oluşturulmuş olanda) ve başka bir kısmi sınıf bildirimindeki uygulamadan oluşur.

    using System;
    
    namespace PartialClassAndMethods
    {
        public partial class PartialClass // Auto-generated
        {
            partial void PartialMethod();
        }
    
        public partial class PartialClass // Human-written
        {
            public void PartialMethod()
            {
                Console.WriteLine("Partial method called.");
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                PartialClass partial = new PartialClass();
                partial.PartialMethod(); // outputs "Partial method called."
            }
        }
    }

## Bir temel sınıftan miras alınan kısmi sınıflar
Herhangi bir temel sınıftan miras alınırken, yalnızca bir kısmi sınıfın temel sınıfın belirtilmesi gerekir.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass {}

Birden fazla kısmi sınıfta *aynı* temel sınıfı *belirtebilirsiniz*. Bazı IDE araçları tarafından gereksiz olarak işaretlenir, ancak doğru şekilde derlenir.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {}

    // PartialClass2.cs
    public partial class PartialClass : BaseClass {} // base class here is redundant

Birden çok kısmi sınıfta *farklı* temel sınıfları *belirtemezsiniz*, bu bir derleyici hatasına neden olur.

    // PartialClass1.cs
    public partial class PartialClass : BaseClass {} // compiler error

    // PartialClass2.cs
    public partial class PartialClass : OtherBaseClass {} // compiler error

