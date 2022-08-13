---
title: "BüyükTamsayı"
slug: "buyuktamsay"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Ne Zaman Kullanılır ##
'BigInteger' nesneleri doğaları gereği RAM üzerinde çok ağırdır. Sonuç olarak, yalnızca kesinlikle gerekli olduğunda, yani gerçekten astronomik ölçekteki sayılar için kullanılmalıdırlar.

Buna ek olarak, bu nesneler üzerindeki tüm aritmetik işlemler, ilkel benzerlerinden daha yavaş bir büyüklük sırasıdır, sabit bir boyutta olmadıkları için sayı büyüdükçe bu sorun daha da karmaşıklaşır. Bu nedenle, hileli bir 'BigInteger'ın mevcut tüm RAM'i tüketerek bir çökmeye neden olması mümkün olabilir.

## Alternatifler ##

Çözümünüz için hız zorunluysa, bir "Byte[]" sarmalayan bir sınıf kullanarak ve gerekli operatörleri kendiniz aşırı yükleyerek bu işlevi kendiniz uygulamak daha verimli olabilir. Ancak, bu önemli miktarda ekstra çaba gerektirir.



## İlk 1000 Basamaklı Fibonacci Sayısını Hesaplayın
"System.Numerics'i kullanma"yı dahil edin ve projeye "System.Numerics" referansını ekleyin.

    using System;
    using System.Numerics;
    
    namespace Euler_25
    {
        class Program
        {
            static void Main(string[] args)
            {
                BigInteger l1 = 1;
                BigInteger l2 = 1;
                BigInteger current = l1 + l2;
                while (current.ToString().Length < 1000)
                {
                    l2 = l1;
                    l1 = current;
                    current = l1 + l2;
                }
                Console.WriteLine(current);
            }
        }
    }

Bu basit algoritma, en az 1000 ondalık basamağa ulaşana kadar Fibonacci sayılarını yineler ve ardından yazdırır. Bu değer, bir "ulong"un bile tutabileceğinden çok daha büyüktür.

Teorik olarak, "BigInteger" sınıfındaki tek sınır, uygulamanızın tüketebileceği RAM miktarıdır.

Not: "BigInteger" yalnızca .NET 4.0 ve sonraki sürümlerde kullanılabilir.

