---
title: "yineleyiciler"
slug: "yineleyiciler"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

Yineleyici, verim anahtar sözcüğünü kullanarak bir dizi veya koleksiyon sınıfı üzerinde özel bir yineleme gerçekleştiren bir yöntem, erişimci veya işleçtir.

## Verimi Kullanarak Yineleyici Oluşturma
Yineleyiciler * numaralandırıcılar * üretirler. C#'da numaralandırıcılar, "verim" ifadeleri içeren yöntemler, özellikler veya dizinleyiciler tanımlanarak üretilir.

Çoğu yöntem, tüm yerel durumları bu yönteme göre bertaraf eden normal 'dönüş' ifadeleri aracılığıyla arayanlarına denetimi döndürür. Buna karşılık, "verim" ifadelerini kullanan yöntemler, istek üzerine arayana birden çok değer döndürmelerine izin verirken, bu değerleri döndürme arasında yerel durumu *koruyarak* sağlar. Bu döndürülen değerler bir dizi oluşturur. Yineleyicilerde kullanılan iki tür "verim" ifadesi vardır:

- Kontrolü arayana geri veren ancak durumu koruyan "verim dönüşü". Aranan kişi, kontrol kendisine geri aktarıldığında bu hattan yürütmeye devam edecektir.

- normal bir "dönüş" ifadesine benzer şekilde işlev gören "verim kesintisi" - bu, dizinin sonunu belirtir. Normal "dönüş" ifadelerinin kendileri bir yineleyici bloğu içinde geçersizdir.


Aşağıdaki örnek, [Fibonacci dizisini][1] oluşturmak için kullanılabilecek bir yineleyici yöntemini göstermektedir:

    IEnumerable<int> Fibonacci(int count)
    {
        int prev = 1;
        int curr = 1;
        
        for (int i = 0; i < count; i++)
        {
            yield return prev;
            int temp = prev + curr;
            prev = curr;
            curr = temp;
        }
    }

Bu yineleyici, daha sonra, bir çağrı yöntemi tarafından tüketilebilecek Fibonacci dizisinin bir numaralandırıcısını üretmek için kullanılabilir. Aşağıdaki kod, Fibonacci dizisindeki ilk on terimin nasıl numaralandırılabileceğini gösterir:

    void Main()
    {
        foreach (int term in Fibonacci(10))
        {
            Console.WriteLine(term);
        }
    }

**Çıktı**

    1
    1
    2
    3
    5
    8
    13
    21
    34
    55

[1]: https://en.wikipedia.org/wiki/Fibonacci_number

## Basit Sayısal Yineleyici Örneği

Yineleyiciler için yaygın bir kullanım durumu, bir sayı koleksiyonu üzerinde bazı işlemler gerçekleştirmektir. Aşağıdaki örnek, bir sayı dizisindeki her bir öğenin konsola nasıl ayrı ayrı yazdırılabileceğini gösterir.

Bu mümkündür, çünkü diziler "IEnumerable" arabirimini uygular ve istemcilerin "GetEnumerator()" yöntemini kullanarak dizi için bir yineleyici elde etmesine olanak tanır. Bu yöntem, dizideki her sayı üzerinde salt okunur, salt ileri bir imleç olan bir * numaralandırıcı* döndürür.

    int[] numbers = { 1, 2, 3, 4, 5 };

    IEnumerator iterator = numbers.GetEnumerator();

    while (iterator.MoveNext())
    {
        Console.WriteLine(iterator.Current);
    }

**Çıktı**

    1
    2
    3
    4
    5

Aynı sonuçları bir "foreach" ifadesi kullanarak da elde etmek mümkündür:

    foreach (int number in numbers)
    {
        Console.WriteLine(number);
    }



