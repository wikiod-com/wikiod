---
title: "İşaretçiler ve Güvenli Olmayan Kod"
slug: "isaretciler-ve-guvenli-olmayan-kod"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## Güvenli olmayan koda giriş
C#, "güvensiz" değiştirici tarafından işaretlendiğinde, kod bloğunun bir işlevinde işaretçi değişkenlerinin kullanılmasına izin verir. Güvenli olmayan kod veya yönetilmeyen kod, bir işaretçi değişkeni kullanan bir kod bloğudur.

İşaretçi, değeri başka bir değişkenin adresi, yani bellek konumunun doğrudan adresi olan bir değişkendir. herhangi bir değişken veya sabite benzer şekilde, herhangi bir değişken adresini saklamak için kullanmadan önce bir işaretçi bildirmeniz gerekir.

Bir işaretçi bildiriminin genel biçimi şöyledir:

    type *var-name;

Aşağıdakiler geçerli işaretçi bildirimleridir:

    int    *ip;    /* pointer to an integer */
    double *dp;    /* pointer to a double */
    float  *fp;    /* pointer to a float */
    char   *ch     /* pointer to a character */
Aşağıdaki örnek, güvenli olmayan değiştiriciyi kullanarak C#'ta işaretçilerin kullanımını gösterir:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          static unsafe void Main(string[] args)
          {
             int var = 20;
             int* p = &var;
             Console.WriteLine("Data is: {0} ",  var);
             Console.WriteLine("Address is: {0}",  (int)p);
             Console.ReadKey();
          }
       }
    }
Yukarıdaki kod derlendiğinde ve çalıştırıldığında, aşağıdaki sonucu verir:

    Data is: 20
    Address is: 99215364

Bir yöntemin tamamını güvensiz olarak bildirmek yerine, kodun bir bölümünü güvensiz olarak da bildirebilirsiniz:

    // safe code
    unsafe
    {
        // you can use pointers here
    }
    // safe code

## İşaretçi Kullanarak Dizi Öğelerine Erişme
C#'da, bir dizi adı ve dizi verileriyle aynı bir veri türünün işaretçisi aynı değişken türü değildir. Örneğin, 'int *p' ve 'int[] p' aynı tür değildir. "p" işaretçi değişkenini artırabilirsiniz, çünkü bu bellekte sabit değildir, ancak bir dizi adresi bellekte sabittir ve bunu artıramazsınız.

Bu nedenle, geleneksel olarak C veya C++'da yaptığımız gibi, bir işaretçi değişkeni kullanarak bir dizi verisine erişmeniz gerekiyorsa, işaretçiyi sabit anahtar sözcüğü kullanarak düzeltmeniz gerekir.

Aşağıdaki örnek bunu göstermektedir:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe static void Main()
          {
             int[]  list = {10, 100, 200};
             fixed(int *ptr = list)
             
             /* let us have array address in pointer */
             for ( int i = 0; i < 3; i++)
             {
                Console.WriteLine("Address of list[{0}]={1}",i,(int)(ptr + i));
                Console.WriteLine("Value of list[{0}]={1}", i, *(ptr + i));
             }
             
             Console.ReadKey();
          }
       }
    }

Yukarıdaki kod derlenip çalıştırıldığında, aşağıdaki sonucu verir:

    Address of list[0] = 31627168
    Value of list[0] = 10
    Address of list[1] = 31627172
    Value of list[1] = 100
    Address of list[2] = 31627176
    Value of list[2] = 200

## Güvenli Olmayan Kodu Derleme
Güvenli olmayan kodu derlemek için, komut satırı derleyicisi ile `/unsafe' komut satırı anahtarını belirtmeniz gerekir.

Örneğin, güvenli olmayan kod içeren prog1.cs adlı bir programı komut satırından derlemek için şu komutu verin:

    csc /unsafe prog1.cs

Visual Studio IDE kullanıyorsanız, proje özelliklerinde güvenli olmayan kod kullanımını etkinleştirmeniz gerekir.

[![buraya resim açıklamasını girin][1]][1]

Bunu yapmak için:

- Özellikler düğümüne çift tıklayarak proje özelliklerini açın.
Çözüm Gezgini.
- Oluştur sekmesine tıklayın.
- "İzin ver" seçeneğini seçin
güvenli olmayan kod"


[1]: https://i.stack.imgur.com/2aPFY.png

## İşaretçi Kullanarak Veri Değerini Alma
ToString() yöntemini kullanarak, işaretçi değişkeni tarafından başvurulan konumda depolanan verileri alabilirsiniz. Aşağıdaki örnek bunu göstermektedir:

    using System;
    namespace UnsafeCodeApplication
    {
       class Program
       {
          public static void Main()
          {
             unsafe
             {
                int var = 20;
                int* p = &var;
                Console.WriteLine("Data is: {0} " , var);
                Console.WriteLine("Data is: {0} " , p->ToString());
                Console.WriteLine("Address is: {0} " , (int)p);
             }
             
             Console.ReadKey();
          }
       }
    }
Yukarıdaki kod derlendiğinde ve çalıştırıldığında, aşağıdaki sonucu verir:

    Data is: 20
    Data is: 20
    Address is: 77128984

## Metotlara Parametre Olarak İşaretçileri Geçirme
Bir işaretçi değişkenini bir yönteme parametre olarak iletebilirsiniz. Aşağıdaki örnek bunu göstermektedir:

    using System;
    namespace UnsafeCodeApplication
    {
       class TestPointer
       {
          public unsafe void swap(int* p, int *q)
          {
             int temp = *p;
             *p = *q;
             *q = temp;
          }
          
          public unsafe static void Main()
          {
             TestPointer p = new TestPointer();
             int var1 = 10;
             int var2 = 20;
             int* x = &var1;
             int* y = &var2;
             
             Console.WriteLine("Before Swap: var1:{0}, var2: {1}", var1, var2);
             p.swap(x, y);
    
             Console.WriteLine("After Swap: var1:{0}, var2: {1}", var1, var2);
             Console.ReadKey();
          }
       }
    }

Yukarıdaki kod derlenip çalıştırıldığında aşağıdaki sonucu verir:

    Before Swap: var1: 10, var2: 20
    After Swap: var1: 20, var2: 10

