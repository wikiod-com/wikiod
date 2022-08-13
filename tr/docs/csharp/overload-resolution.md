---
title: "Aşırı Yük Çözünürlüğü"
slug: "asr-yuk-cozunurlugu"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Aşırı yük çözme işlemi, [C# belirtimi][1], bölüm 7.5.3'te açıklanmıştır. 7.5.2 (tür çıkarımı) ve 7.6.5 (çağrı ifadeleri) bölümleri de konuyla ilgilidir.

Aşırı yük çözünürlüğünün nasıl çalıştığı muhtemelen C# 7'de değiştirilecektir. Tasarım notları, Microsoft'un hangi yöntemin daha iyi olduğunu (karmaşık senaryolarda) belirlemek için yeni bir sistem sunacağını gösteriyor.


[1]: https://www.microsoft.com/en-us/download/details.aspx?id=7029

## Temel Aşırı Yükleme Örneği
Bu kod, **Merhaba** adlı aşırı yüklenmiş bir yöntem içerir:

    class Example
    {
        public static void Hello(int arg)
        {
            Console.WriteLine("int");
        }
     
        public static void Hello(double arg)
        {
            Console.WriteLine("double");
        }
     
        public static void Main(string[] args) 
        {
            Hello(0);
            Hello(0.0);
        }
    }

**Main** yöntemi çağrıldığında yazdırılacaktır

    int
    double

Derleme zamanında, derleyici `Hello(0)` yöntem çağrısını bulduğunda, `Merhaba` adındaki tüm yöntemleri bulur. Bu durumda, iki tane bulur. Ardından yöntemlerden hangisinin *daha iyi* olduğunu belirlemeye çalışır. Hangi yöntemin daha iyi olduğunu belirleme algoritması karmaşıktır, ancak genellikle "mümkün olduğunca az örtük dönüşüm yapmak" için kaynar.

Bu nedenle, "Merhaba(0)" durumunda, "Hello(int)" yöntemi için herhangi bir dönüştürme gerekmez, ancak "Hello(double)" yöntemi için örtük bir sayısal dönüştürme gerekir. Böylece ilk yöntem derleyici tarafından seçilir.

"Merhaba(0.0)" durumunda, "0.0"ı dolaylı olarak "int"e dönüştürmenin bir yolu yoktur, bu nedenle "Merhaba(int)" yöntemi aşırı yük çözümü için bile dikkate alınmaz. Yalnızca yöntem kalır ve bu nedenle derleyici tarafından seçilir.

## "params" gerekli olmadıkça genişletilmez.
Aşağıdaki program:

    class Program
    {
        static void Method(params Object[] objects)
        {
            System.Console.WriteLine(objects.Length);
        }   
        static void Method(Object a, Object b)
        {
            System.Console.WriteLine("two");
        }
        static void Main(string[] args)
        {
            object[] objectArray = new object[5];

            Method(objectArray);
            Method(objectArray, objectArray);
            Method(objectArray, objectArray, objectArray);
        }
    }

yazdıracak:

    5
    two
    3

"Method(objectArray)" çağrı ifadesi iki şekilde yorumlanabilir: bir dizi olan tek bir "Nesne" argümanı (böylece program "1" verir, çünkü bu argüman sayısı veya bir dizi olur. 'Yöntem' yönteminde 'params' anahtar kelimesi yokmuş gibi normal biçimde verilen argümanların sayısı.Bu durumlarda, normal, genişletilmemiş biçim her zaman önceliklidir.Dolayısıyla, program '5' çıktısını verir.

İkinci ifadede, `Method(objectArray, objectArray)`, hem birinci yöntemin genişletilmiş biçimi hem de geleneksel ikinci yöntem uygulanabilir. Bu durumda da, genişletilmemiş formlar önceliklidir, bu nedenle program 'iki' yazdırır.

Üçüncü ifadede, `Method(objectArray, objectArray, objectArray)`, tek seçenek ilk yöntemin genişletilmiş biçimini kullanmaktır ve böylece program `3` yazdırır.

## Argümanlardan biri olarak null iletme
eğer varsa

    void F1(MyType1 x) {
        // do something
    }

    void F1(MyType2 x) {
        // do something else
    }

ve bir nedenden dolayı 'F1'in ilk aşırı yüklemesini ancak 'x = null' ile çağırmanız gerekir, ardından

    F1(null);

çağrı belirsiz olduğu için derlenmeyecek. Buna karşı koymak için şunları yapabilirsiniz

    F1(null as MyType1);

