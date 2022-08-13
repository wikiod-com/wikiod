---
title: "Dize Enterpolasyonu"
slug: "dize-enterpolasyonu"
draft: false
images: []
weight: 9769
type: docs
toc: true
---

## Sözdizimi
- $"içerik {ifade} içeriği"
- $"içerik {ifadesi:biçim} içerik"
- $"içerik {ifade} {{parantez içindeki içerik}} içerik}"
- $"içerik {ifade:format} {{parantez içindeki içerik}} içerik}"

Dize enterpolasyonu, içinde değişken ve ifade değerleri olan dizeler oluşturmayı kolaylaştıran "string.Format()" yönteminin bir kısaltmasıdır.

    var name = "World";
    var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
    var newWay = $"Hello, {name}!";                   // returns "Hello, World"

## Tarihleri ​​dizelerde biçimlendir
    var date = new DateTime(2015, 11, 11);
    var str = $"It's {date:MMMM d, yyyy}, make a wish!";
    System.Console.WriteLine(str);

"DateTime" nesnesini biçimlendirmek için [`DateTime.ToString`][1] yöntemini de kullanabilirsiniz. Bu, yukarıdaki kodla aynı çıktıyı üretecektir.

    var date = new DateTime(2015, 11, 11);
    var str = date.ToString("MMMM d, yyyy");
    str = "It's " + str + ", make a wish!";
    Console.WriteLine(str);

**Çıktı:**
>11 Kasım 2015, bir dilek tut!

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/DpRwV5)

[DateTime.ToString kullanarak Canlı Demo](https://dotnetfiddle.net/YnV9J0)

> **Not:** "MM" ayları ve "mm" dakikaları temsil eder. Bunları kullanırken çok dikkatli olun çünkü hatalar, keşfedilmesi zor olabilecek hatalara neden olabilir.


[1]: https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx

## Çıktıyı doldurma
Dize, eklenen dizenin kaç karakter konumu kullanacağını belirten bir dolgu parametresini kabul edecek şekilde biçimlendirilebilir:

    ${value, padding}

> **NOT:** Pozitif dolgu değerleri sol dolguyu ve negatifi gösterir
> dolgu değerleri sağ dolguyu gösterir.

**Sol Dolgu**
----

5'lik bir sol dolgu (sayı değerinden önce 3 boşluk ekler, böylece elde edilen dizede toplam 5 karakter konumu alır.)

    var number = 42;
    var str = $"The answer to life, the universe and everything is {number, 5}.";
    //str is "The answer to life, the universe and everything is    42.";
    //                                                           ^^^^^
    System.Console.WriteLine(str);
    
**Çıktı:**
       
    The answer to life, the universe and everything is    42.
[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/PpZXmk)

**Sağ Dolgu**
----

Negatif bir dolgu değeri kullanan sağ dolgu, geçerli değerin sonuna boşluk ekler.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, -5}.";
    //str is "The answer to life, the universe and everything is 42   .";
    //                                                           ^^^^^
    System.Console.WriteLine(str);

**Çıktı:**

    The answer to life, the universe and everything is 42   .

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/QtKjGF)

**Biçim Belirticilerle Doldurma**
----

Doldurma ile birlikte mevcut biçimlendirme belirteçlerini de kullanabilirsiniz.

    var number = 42;
    var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
    //str is "The answer to life, the universe and everything is 42.1 ";
    //                                                           ^^^^^

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/34ZxP0)



## İfade
Tam ifadeler, enterpolasyonlu dizelerde de kullanılabilir.

    var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"
    
    string world = "world";
    var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"


[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/u9lzeg)



## Basit Kullanım
    var name = "World";
    var str = $"Hello, {name}!";
    //str now contains: "Hello, World!";

## Kamera ARKASI

Dahili olarak bu

    $"Hello, {name}!" 

Bunun gibi bir şeye derlenecek:

    string.Format("Hello, {0}!", name);

    


## Sayıları dizelerde biçimlendirme
Sayıların nasıl biçimlendirildiğini denetlemek için iki nokta üst üste ve [standart sayısal biçim sözdizimi](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) kullanabilirsiniz.

    var decimalValue = 120.5;

    var asCurrency = $"It costs {decimalValue:C}";
    // String value is "It costs $120.50" (depending on your local currency settings)

    var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
    // String value is "Exactly 120.500"

    var integerValue = 57;

    var prefixedIfNecessary = $"{integerValue:D5}";
    // String value is "00057"


[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/z2XbG7)

