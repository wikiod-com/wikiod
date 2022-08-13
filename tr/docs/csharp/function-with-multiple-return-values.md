---
title: "Birden çok dönüş değeri olan işlev"
slug: "birden-cok-donus-degeri-olan-islev"
draft: false
images: []
weight: 9923
type: docs
toc: true
---

Bu - sözde - ihtiyaca C#'da doğal bir cevap yoktur. Bununla birlikte, bu ihtiyacı karşılamak için geçici çözümler vardır.

İhtiyacı "sözde" olarak nitelendirmemin nedeni, iyi programlama ilkelerini ihlal ettiğimizde geri dönmek için yalnızca 2 veya 2'den fazla değere sahip yöntemlere ihtiyacımız olmasıdır. Özellikle [Tek Sorumluluk İlkesi][1].

Bu nedenle, 2 veya daha fazla değer döndüren fonksiyonlara ihtiyacımız olduğunda uyarı almak ve tasarımımızı geliştirmek daha iyi olacaktır.


[1]: https://en.wikipedia.org/wiki/Single_responsibility_principle

## "anonim nesne" + "dinamik anahtar kelime" çözümü
İşlevinizden anonim bir nesne döndürebilirsiniz

    public static object FunctionWithUnknowReturnValues ()
    {
        /// anonymous object
        return new { a = 1, b = 2 };
    }

Ve sonucu dinamik bir nesneye atayın ve içindeki değerleri okuyun.

    /// dynamic object
    dynamic x = FunctionWithUnknowReturnValues();

    Console.WriteLine(x.a);
    Console.WriteLine(x.b);

## Tuple çözümü
"Tuple<string, MyClass>" olarak iki şablon parametresiyle işlevinizden bir "Tuple" sınıfının bir örneğini döndürebilirsiniz:

    public Tuple<string, MyClass> FunctionWith2ReturnValues ()
    {
        return Tuple.Create("abc", new MyClass());
    }

Ve aşağıdaki gibi değerleri okuyun:

    Console.WriteLine(x.Item1);
    Console.WriteLine(x.Item2);

## Ref ve Out Parametreleri
'ref' anahtar sözcüğü, bir [Argüman Olarak Referans][1] iletmek için kullanılır. 'out', 'ref' ile aynı şeyi yapacaktır, ancak işlevi çağırmadan önce arayan tarafından atanmış bir değer gerektirmez.

**Ref Parameter** :-Eğer bir değişkeni ref parametresi olarak iletmek istiyorsanız, onu metoda ref parametresi olarak geçirmeden önce onu başlatmanız gerekir.

**Çıkış Parametresi :-**
Bir değişkeni out parametresi olarak iletmek istiyorsanız, metoda out parametresi olarak iletmeden önce onu başlatmanız gerekmez.

    static void Main(string[] args)
    {
        int a = 2;
        int b = 3;
        int add = 0;
        int mult= 0;
        AddOrMult(a, b, ref add, ref mult); //AddOrMult(a, b, out add, out mult);
        Console.WriteLine(add); //5
        Console.WriteLine(mult); //6
    }
    
    private static void AddOrMult(int a, int b, ref int add, ref int mult) //AddOrMult(int a, int b, out int add, out int mult)
    {
        add = a + b;
        mult = a * b;
    }


[1]: https://www.wikiod.com/tr/docs/c%23/3014/value-type-vs-reference-type#t=201607261617231313768

