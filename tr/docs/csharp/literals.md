---
title: "değişmezler"
slug: "degismezler"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Sözdizimi
- **bool:** doğru veya yanlış
- **bayt:** Yok, tamsayı değişmezi örtük olarak int'den dönüştürülür
- **sbyte:** Yok, int'den örtük olarak dönüştürülen tamsayı değişmezi
- **char:** Değeri tek tırnak içine alın
- **ondalık:** M veya m
- **çift:** D, d veya gerçek bir sayı
- **yüzer:** F veya f
- **int:** Yok, int aralığındaki integral değerleri için varsayılan
- **uint:** U, u veya uint aralığındaki integral değerleri
- **uzun:** L, l veya uzun aralığındaki integral değerler
- **Ulong:** UL, UL, Ul, uL, LU, lu, Lu, lU veya ulong aralığındaki integral değerler
- **kısa:** Yok, int'den örtük olarak dönüştürülen tamsayı değişmezi
- **ushort:** Yok, int'den örtük olarak dönüştürülen tamsayı değişmezi
- **string:** Değeri çift tırnak içine alın, isteğe bağlı olarak başına `@' ekleyin
- **null**: "null" değişmezi

## uint değişmezleri
"uint" değişmezleri "U" veya "u" son eki kullanılarak veya "uint" aralığında bir integral değerleri kullanılarak tanımlanır:

    uint ui = 5U;

## int değişmezleri
"int" değişmez değerleri, "int" aralığındaki integral değerleri kullanılarak tanımlanır:

    int i = 5;

## sbyte değişmezleri
"sbyte" türünün değişmez son eki yoktur. Tamsayı değişmezleri örtük olarak "int"den dönüştürülür:

    sbyte sb = 127;

## ondalık değişmez değerler
"decimal" değişmezleri, gerçek bir sayı üzerinde M veya m son eki kullanılarak tanımlanır:

    decimal m = 30.5M;

## çift değişmez
"double" değişmezleri, D veya d son eki veya gerçek bir sayı kullanılarak tanımlanır:

    double d = 30.5D;


## kayan değişkenler
"float" değişmezleri, F veya f son eki veya gerçek bir sayı kullanılarak tanımlanır:

    float f = 30.5F;


## uzun değişmezler
"uzun" değişmez değerler "L" veya "l" son eki kullanılarak veya "uzun" aralığında bir integral değerleri kullanılarak tanımlanır:

    long l = 5L;

## baş harfi
"ulong" değişmezleri "UL", "ul", "Ul", "uL", "LU", "lu", "Lu" veya "lU" sonekleri kullanılarak veya bir integral değerleri kullanılarak tanımlanır ​​"kafa" aralığında:

    ulong ul = 5UL;

## dize değişmezleri
"dize" değişmezleri, değeri çift tırnak """ içine alarak tanımlanır:

    string s = "hello, this is a string literal";

Dize değişmezleri kaçış dizileri içerebilir. Bkz. [String Kaçış Dizileri][1]

Ek olarak, C# harfi harfine dize değişmezlerini destekler (Bkz. [Verbatim Dizeleri][2]). Bunlar, değeri çift tırnak `"` ile sararak ve başına `@` ekleyerek tanımlanır. Kaçış dizileri kelimesi kelimesine dize değişmezlerinde yok sayılır ve tüm boşluk karakterleri dahil edilir:

    string s = @"The path is:
    C:\Windows\System32";
    //The backslashes and newline are included in the string


[1]: https://www.wikiod.com/tr/docs/c%23/39/string-escape-sequences
[2]: https://www.wikiod.com/tr/docs/c%23/16/verbatim-strings

## karakter değişmezleri
"char" değişmezleri, değeri tek tırnak "" içine alarak tanımlanır:

    char c = 'h';

Karakter değişmezleri kaçış dizileri içerebilir. Bkz. [String Kaçış Dizileri][1]

Bir karakter değişmezi tam olarak bir karakter uzunluğunda olmalıdır (tüm kaçış dizileri değerlendirildikten sonra). Boş karakter değişmezleri geçerli değil. Varsayılan karakter ("varsayılan(char)" veya "yeni karakter()" tarafından döndürülür) "\0" veya NULL karakterdir ("boş" değişmez ve boş referanslarla karıştırılmamalıdır).

[1]: https://www.wikiod.com/tr/docs/c%23/39/string-escape-sequences

## bayt değişmezleri
"bayt" türünün değişmez son eki yoktur. Tamsayı değişmezleri örtük olarak "int"den dönüştürülür:

    byte b = 127;

## kısa gerçek
"kısa" türün değişmez değeri yoktur. Tamsayı değişmezleri örtük olarak "int"den dönüştürülür:

    short s = 127;

## ushort değişmezi
"ushort" türünün değişmez son eki yoktur. Tamsayı değişmezleri örtük olarak "int"den dönüştürülür:

    ushort us = 127;

## bool değişmezleri
"bool" değişmezleri "doğru" veya "yanlış"tır;

    bool b = true;

