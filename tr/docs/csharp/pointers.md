---
title: "işaretçiler"
slug: "isaretciler"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

# İşaretçiler ve "güvensiz"

İşaretçiler doğası gereği doğrulanamayan kodlar üretir. Bu nedenle, herhangi bir işaretçi türünün kullanımı "güvenli olmayan" bir bağlam gerektirir.

'System.IntPtr' türü, bir 'void*' etrafındaki güvenli bir sarmalayıcıdır. Eldeki görevi gerçekleştirmek için güvenli olmayan bir bağlam gerekli olmadığında, "void*"e daha uygun bir alternatif olarak tasarlanmıştır.

# Tanımsız davranış

C ve C++'da olduğu gibi, işaretçilerin yanlış kullanımı, olası yan etkileri bellek bozulması ve istenmeyen kodun yürütülmesiyle birlikte tanımsız davranışı başlatabilir. Çoğu işaretçi işleminin doğrulanamaz doğası nedeniyle, işaretçilerin doğru kullanımı tamamen programcının sorumluluğundadır.

# İşaretçileri destekleyen türler

C ve C++'dan farklı olarak, tüm C# türlerinin karşılık gelen işaretçi türleri yoktur. Aşağıdaki kriterlerin her ikisi de geçerliyse, bir "T" türünün karşılık gelen bir işaretçi türü olabilir:

- `T` bir yapı tipi veya bir işaretçi tipidir.
- "T", yalnızca bu kriterlerin her ikisini de yinelemeli olarak karşılayan üyeleri içerir.

## Dizi erişimi için işaretçiler
Bu örnek, işaretçilerin C# dizilerine C benzeri erişim için nasıl kullanılabileceğini gösterir.

    unsafe
    {
        var buffer = new int[1024];
        fixed (int* p = &buffer[0])
        {
            for (var i = 0; i < buffer.Length; i++)
            {
                *(p + i) = i;
            }
        }
    }

'güvensiz' anahtar sözcüğü gereklidir, çünkü işaretçi erişimi, C# dizilerine normal yoldan erişirken normalde yayılan herhangi bir sınır denetimi yayınlamayacaktır.

'fixed' anahtar sözcüğü, C# derleyicisine nesneyi istisna açısından güvenli bir şekilde sabitlemek için talimatlar vermesini söyler. Sabitleme, çöp toplayıcının diziyi bellekte hareket ettirmemesini sağlamak için gereklidir, çünkü bu, dizi içinde işaret eden herhangi bir işaretçiyi geçersiz kılacaktır.

## İşaretçi aritmetiği
İşaretçilerde toplama ve çıkarma, tamsayılardan farklı çalışır. Bir işaretçi artırıldığında veya azaltıldığında, işaret ettiği adres, başvuru türünün boyutuna göre artırılır veya azaltılır.

Örneğin, "int" türünün ("System.Int32"nin diğer adı) boyutu 4'tür. Bir "int" 0 adresinde saklanabiliyorsa, sonraki "int" adres 4'te saklanabilir, vb . Kodda:

    var ptr = (int*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 4
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8

Benzer şekilde, 'long' tipinin ('System.Int64'ün diğer adı) boyutu 8'dir. Kodda:

    var ptr = (long*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr)); // prints 0
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 8
    ptr++;
    Console.WriteLine(new IntPtr(ptr)); // prints 16

'void' türü özeldir ve 'void' işaretçileri de özeldir ve türün bilinmediği veya önemli olmadığı durumlarda tümünü yakalama işaretçileri olarak kullanılırlar. Boyuttan bağımsız yapıları nedeniyle, "void" işaretçileri artırılamaz veya azaltılamaz:

    var ptr = (void*)IntPtr.Zero;
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));
    ptr++; // compile-time error
    Console.WriteLine(new IntPtr(ptr));

## Yıldız işareti türün bir parçasıdır
C ve C++'da, bir işaretçi değişkeninin bildirimindeki yıldız işareti, bildirilen *ifadenin* bir parçasıdır. C#'da, bildirimdeki yıldız işareti *türünün bir parçasıdır*.

C, C++ ve C#'da aşağıdaki kod parçası bir "int" işaretçisi bildirir:

    int* a;

C ve C++'da aşağıdaki kod parçası bir "int" işaretçisi ve bir "int" değişkeni bildirir. C#'da iki "int" işaretçisi bildirir:

    int* a, b; 

C ve C++'da aşağıdaki kod parçası iki "int" işaretçisi bildirir. C#'da geçersizdir:

    int *a, *b;

## geçersiz*
C#, tipten bağımsız ve boyuttan bağımsız bir işaretçi olarak "void*" kullanımını C ve C++'dan devralır.

    void* ptr;

Herhangi bir işaretçi türü, örtük bir dönüştürme kullanılarak "void*"e atanabilir:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;

Tersi açık bir dönüşüm gerektirir:

    int* p1 = (int*)IntPtr.Zero;
    void* ptr = p1;
    int* p2 = (int*)ptr;

## -> kullanarak üye erişimi
C#, bir örneğin üyelerine yazılı bir işaretçi aracılığıyla erişmenin bir yolu olarak '->' sembolünün kullanımını C ve C++'dan devralır.

Aşağıdaki yapıyı göz önünde bulundurun:

    struct Vector2
    {
        public int X;
        public int Y;
    }

Bu, üyelerine erişmek için `->` kullanımına bir örnektir:

    Vector2 v;
    v.X = 5;
    v.Y = 10;

    Vector2* ptr = &v;
    int x = ptr->X;
    int y = ptr->Y;
    string s = ptr->ToString();

    Console.WriteLine(x); // prints 5
    Console.WriteLine(y); // prints 10
    Console.WriteLine(s); // prints Vector2

## Genel işaretçiler
İşaretçileri desteklemek için bir türün karşılaması gereken ölçütler (bkz. *Açıklamalar*) genel kısıtlamalar cinsinden ifade edilemez. Bu nedenle, genel bir tür parametresi aracılığıyla sağlanan bir türe işaretçi bildirme girişimi başarısız olur.

    void P<T>(T obj) 
        where T : struct
    {
        T* ptr = &obj; // compile-time error
    }


