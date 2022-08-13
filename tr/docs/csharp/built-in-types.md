---
title: "Yerleşik Tipler"
slug: "yerlesik-tipler"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Kutulu değer türlerinin dönüştürülmesi
[Kutulu](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) değer türleri, iki "Tür"ün dönüştürülmesi geçerli olsa bile, yalnızca orijinal "Tür"lerine çıkarılabilir , Örneğin.:

    object boxedInt = (int)1; // int boxed in an object

    long unboxedInt1 = (long)boxedInt; // invalid cast

Bu, ilk olarak orijinal "Tür"e kutudan çıkarılarak önlenebilir, örneğin:

    long unboxedInt2 = (long)(int)boxedInt; // valid

## Değişmez referans türü - dize
    // assign string from a string literal
    string s = "hello";

    // assign string from an array of characters
    char[] chars = new char[] { 'h', 'e', 'l', 'l', 'o' };
    string s = new string(chars, 0, chars.Length);

    // assign string from a char pointer, derived from a string
    string s;
    unsafe
    {
        fixed (char* charPointer = "hello")
        {
            s = new string(charPointer);
        }
    }


## Değer türü - karakter
    // single character s
    char c = 's';

    // character s: casted from integer value
    char c = (char)115;

    // unicode character: single character s
    char c = '\u0073';

    // unicode character: smiley face
    char c = '\u263a';

## Değer türü - kısa, int, uzun (işaretli 16 bit, 32 bit, 64 bit tam sayılar)
    // assigning a signed short to its minimum value
    short s = -32768;
    
    // assigning a signed short to its maximum value
    short s = 32767;
    
    // assigning a signed int to its minimum value
    int i = -2147483648;
    
    // assigning a signed int to its maximum value
    int i = 2147483647;
    
    // assigning a signed long to its minimum value (note the long postfix)
    long l = -9223372036854775808L;
    
    // assigning a signed long to its maximum value (note the long postfix)
    long l = 9223372036854775807L;

Bu türlerin null yapılabilir olması da mümkündür, yani normal değerlere ek olarak null da atanabilir. Null yapılabilir türden bir değişken başlatılmazsa, 0 yerine null olur. Null yapılabilir türler, türden sonra bir soru işareti (?) eklenerek işaretlenir.

    int a; //This is now 0.
    int? b; //This is now null.

## Değer türü - ushort, uint, ulong (işaretsiz 16 bit, 32 bit, 64 bit tam sayılar)
    // assigning an unsigned short to its minimum value
    ushort s = 0;
    
    // assigning an unsigned short to its maximum value
    ushort s = 65535;
    
    // assigning an unsigned int to its minimum value
    uint i = 0;
    
    // assigning an unsigned int to its maximum value
    uint i = 4294967295;
    
    // assigning an unsigned long to its minimum value (note the unsigned long postfix)
    ulong l = 0UL;
    
    // assigning an unsigned long to its maximum value (note the unsigned long postfix)
    ulong l = 18446744073709551615UL;

Bu türleri null yapılabilir hale getirmek de mümkündür, yani normal değerlere ek olarak null da atanabilir. Null yapılabilir türden bir değişken başlatılmazsa, 0 yerine null olur. Null yapılabilir türler, türden sonra bir soru işareti (?) eklenerek işaretlenir.

    uint a; //This is now 0.
    uint? b; //This is now null.

## Değer türü - bool
    // default value of boolean is false
    bool b;
    //default value of nullable boolean is null
    bool? z;
    b = true;
    if(b) {
        Console.WriteLine("Boolean has true value");
    }

bool anahtar sözcüğü, System.Boolean'ın bir diğer adıdır. Boolean değerlerini, 'true' ve 'false' depolamak için değişkenleri bildirmek için kullanılır.

## Kutulu değer türleri ile karşılaştırmalar
Değer türleri "nesne" türündeki değişkenlere atanmışsa, bunlar [*kutulu*](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) - değer bir örneğinde saklanır "Sistem.Nesnesi". Bu, değerleri "==" ile karşılaştırırken istenmeyen sonuçlara yol açabilir, örneğin:

    object left = (int)1;  // int in an object box
    object right = (int)1; // int in an object box

    var comparison1 = left == right;      // false

Bu, beklenen sonucu verecek olan aşırı yüklenmiş "Equals" yöntemi kullanılarak önlenebilir.

    var comparison2 = left.Equals(right); // true

Alternatif olarak, aynısı, "int" değerlerinin karşılaştırılabilmesi için "left" ve "right" değişkenlerini kutudan çıkararak da yapılabilir:

    var comparison3 = (int)left == (int)right; // true

