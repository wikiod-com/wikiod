---
title: "dize Manipülasyonu"
slug: "dize-manipulasyonu"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

## Bir dize içindeki bir dizeyi değiştirme
[`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx) yöntemini kullanarak, bir dizenin bir kısmını bir başkasıyla değiştirebilirsiniz. sicim.

    string s = "Hello World";
    s = s.Replace("World", "Universe"); // s = "Hello Universe"

Arama dizesinin tüm oluşumları değiştirilir:

    string s = "Hello World";
    s = s.Replace("l", "L"); // s = "HeLLo WorLD"

"String.Replace", değiştirme değeri olarak boş bir dize belirterek bir dizenin bir kısmını *kaldırmak* için de kullanılabilir:

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"

## Bir String içindeki karakterlerin büyük/küçük harf durumunu değiştirme
[`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) sınıfı, büyük harf ve küçük harf arasında dönüştürme yapmak için bir dizi yöntemi destekler bir dizedeki karakterler.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) dönüştürülen bir String nesnesini döndürmek için kullanılır küçük harfe.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) dönüştürülen bir String nesnesini döndürmek için kullanılır büyük harfe.

**Not:** Bu yöntemlerin *değişmez* sürümlerini kullanmanın nedeni, kültüre özgü beklenmeyen harflerin üretilmesini önlemektir. Bu, [burada ayrıntılı olarak](http://stackoverflow.com/a/19778131/1379664) açıklanmıştır.

Örnek:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Belirli bir **[Kültür](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** belirtmeyi * seçebileceğinizi* unutmayın. [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) ve [String.ToUpper kullanarak küçük ve büyük harfe dönüştürürken (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) yöntemleri buna göre.



## Bir dize içinde bir dize bulma
Kullanmak
[`System.String.Contains`][1] bir dize içinde belirli bir dizenin olup olmadığını öğrenebilirsiniz. Yöntem, bir boolean döndürür, eğer dize false ise true.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

[`System.String.IndexOf`](https://msdn.microsoft.com/en-us/library/k8b1470s(v=vs.110).aspx) yöntemini kullanarak, bir alt dizenin başlangıç ​​konumunu bulabilirsiniz. mevcut bir dize içinde.
Döndürülen konumun sıfır tabanlı olduğuna dikkat edin, alt dize bulunamazsa -1 değeri döndürülür.

    string s = "Hello World";
    int location = s.IndexOf("ello"); // location = 1

Bir dizenin ***sonundan*** ilk konumu bulmak için [`System.String.LastIndexOf `](https://msdn.microsoft.com/en-us/library/system.string. lastindexof(v=vs.110).aspx) yöntemi:

    string s = "Hello World";
    int location = s.LastIndexOf("l"); // location = 9


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Bir dizeden boşluk kaldırma (Kırpma)
[`System.String.Trim`](https://msdn.microsoft.com/en-us/library/t97s7bs3(v=vs.110).aspx) yöntemi, baştaki ve sondaki tüm beyazları kaldırmak için kullanılabilir. bir dizeden boşluk karakterleri:

    string s = "     String with spaces at both ends      ";
    s = s.Trim(); // s = "String with spaces at both ends"

Ek olarak:
- Boşluğu yalnızca bir dizenin *başlangıcından* kaldırmak için şunu kullanın: [`System.String.TrimStart`](https://msdn.microsoft.com/en-us/library/system.string.trimstart(v) =vs.110).aspx)

- Bir dizenin yalnızca * sonundaki* boşlukları kaldırmak için şunu kullanın: [`System.String.TrimEnd`](https://msdn.microsoft.com/en-us/library/system.string.trimend(v) =vs.110).aspx)

**Bir dizenin bir kısmını çıkarmak için alt dize.**

[`System.String.Substring`][1] yöntemi, dizenin bir bölümünü çıkarmak için kullanılabilir.

    string s ="A portion of word that is retained";
    s=str.Substring(26);  //s="retained"

    s1 = s.Substring(0,5);  //s="A por"
    

[1]: https://msdn.microsoft.com/en-us/library/hxthx5h6(v=vs.110).aspx

## Sınırlayıcı kullanarak bir dizeyi bölme
Bir dize dizisi döndürmek için [`System.String.Split`](https://msdn.microsoft.com/en-us/library/system.string.split(v=vs.110).aspx) yöntemini kullanın. belirtilen bir sınırlayıcıya göre bölünmüş orijinal dizenin alt dizelerini içerir:

    string sentence = "One Two Three Four";
    string[] stringArray = sentence.Split(' ');

    foreach (string word in stringArray)
    {
        Console.WriteLine(word);    
    }

Çıktı:

> Bir
> İki
> Üç
> Dört

## Bir dizi diziyi tek bir dizede birleştirin
[`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) yöntemi, kullanarak bir dize dizisindeki tüm öğeleri birleştirmeye izin verir her öğe arasında belirli bir ayırıcı:

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Dize Birleştirme
Dize Birleştirme, [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) yöntemi kullanılarak yapılabilir. veya (çok daha kolay) `+` operatörünü kullanarak:

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

