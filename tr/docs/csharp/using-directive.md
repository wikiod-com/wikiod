---
title: "Direktifi Kullanma"
slug: "direktifi-kullanma"
draft: false
images: []
weight: 9956
type: docs
toc: true
---

'using' anahtar sözcüğü hem bir yönerge (bu konu) hem de bir ifadedir.

'Using' ifadesi için (yani, bir 'IDisposable' nesnesinin kapsamını kapsüllemek, bu kapsamın dışında nesnenin temiz bir şekilde atılmasını sağlamak için) lütfen [Using Statement][1]'e bakın.


[1]: https://www.wikiod.com/tr/docs/c%23/38/using-statement

## Bir Sınıfın Statik Üyelerine Erişme
<!-- eğer [gte 6.0] versiyonu -->

Belirli bir türü içe aktarmanıza ve türün statik üyelerini tür adıyla nitelemeden kullanmanıza olanak tanır. Bu, statik yöntemleri kullanan bir örneği gösterir:

    using static System.Console;

    // ...

    string GetName()
    {
        WriteLine("Enter your name.");
        return ReadLine();
    }

Bu, statik özellikleri ve yöntemleri kullanan bir örneği gösterir:

    using static System.Math;

    namespace Geometry
    {
        public class Circle
        {
            public double Radius { get; set; };

            public double Area => PI * Pow(Radius, 2);
        }
    }

<!-- eğer --> son sürüm

## Çakışmaları Çözmek için Bir Takma Ad İlişkilendirme
Aynı ad sınıflarına sahip olabilecek birden çok ad alanı kullanıyorsanız ("System.Random" ve "UnityEngine.Random" gibi), "Random" öğesinin birinden veya diğerinden geldiğini belirtmek için bir takma ad kullanabilirsiniz. çağrıdaki tüm ad alanı.

Örneğin:

    using UnityEngine;
    using System;

    Random rnd = new Random();

Bu, derleyicinin yeni değişkeni hangi 'Rastgele' olarak değerlendireceğinden emin olmamasına neden olur. Bunun yerine şunları yapabilirsiniz:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();

Bu, diğerini tam nitelikli ad alanıyla aramanızı engellemez, şöyle:

    using UnityEngine;
    using System;
    using Random = System.Random;

    Random rnd = new Random();
    int unityRandom = UnityEngine.Random.Range(0,100);

"rnd" bir "System.Random" değişkeni olacak ve "unityRandom" bir "UnityEngine.Random" değişkeni olacaktır.

## Takma ad yönergelerini kullanma
Bir ad alanı veya tür için bir takma ad ayarlamak için 'kullanarak' kullanabilirsiniz. Daha fazla ayrıntı [burada][1]'de bulunabilir.

Sözdizimi:

    using <identifier> = <namespace-or-type-name>;

Örnek:

    using NewType = Dictionary<string, Dictionary<string,int>>;
    NewType multiDictionary = new NewType();
    //Use instances as you are using the original one
    multiDictionary.Add("test", new Dictionary<string,int>());
 


[1]: https://msdn.microsoft.com/en-us/library/aa664765(v=vs.71).aspx

## Temel Kullanım
    using System;
    using BasicStuff = System;
    using Sayer = System.Console;
    using static System.Console;  //From C# 6
    
    class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Ignoring usings and specifying full type name");
            Console.WriteLine("Thanks to the 'using System' directive");
            BasicStuff.Console.WriteLine("Namespace aliasing");
            Sayer.WriteLine("Type aliasing");
            WriteLine("Thanks to the 'using static' directive (from C# 6)");
        }
    }



## Ad Alanı Referansı
    using System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //without prefixing them with the namespace.  i.e:

    //...
    var sb = new StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

## Bir Takma Adı Ad Alanıyla İlişkilendirme
    using st = System.Text;
    //allows you to access classes within this namespace such as StringBuilder
    //prefixing them with only the defined alias and not the full namespace.  i.e:

    //...
    var sb = new st.StringBuilder();
    //instead of
    var sb = new System.Text.StringBuilder();

