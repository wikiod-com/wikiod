---
title: "Verbatim Dizeleri"
slug: "verbatim-dizeleri"
draft: false
images: []
weight: 9434
type: docs
toc: true
---

## Sözdizimi
- @"verbatim dizeleri, içeriği çıkış yapılmayan dizelerdir, bu nedenle bu durumda \n yeni satır karakterini değil, iki ayrı karakteri temsil eder: \ ve n. Verbatim dizeleri, dize içeriğinin önüne @ karakteri eklenerek oluşturulur"

- @"Tırnak işaretlerinden kurtulmak için ""çift tırnak"" kullanılır."

Dize değişmezlerini birleştirmek için, her dizenin başındaki @ simgesini kullanın.

    var combinedString = @"\t means a tab" + @" and \n means a newline";

## Enterpolasyonlu Verbatim Dizeleri
Verbatim dizeleri, C#6'da bulunan yeni https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/49/string-interpolation özellikleriyle birleştirilebilir.

    Console.WriteLine($@"Testing \n 1 2 {5 - 2}
    New line");

**Çıktı:**

> Test \n 1 2 3
> Yeni hat

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/cWyQE2)

Bir kelimesi kelimesine dizeden beklendiği gibi, ters eğik çizgiler kaçış karakterleri olarak yok sayılır. Ve enterpolasyonlu bir dizgeden beklendiği gibi, küme parantezleri içindeki herhangi bir ifade, bu konumda dizgeye eklenmeden önce değerlendirilir.


## Çift Alıntılardan Kaçış
Verbatim dizeleri içindeki Çift Tırnaklardan, elde edilen dizede bir çift tırnak `"` temsil etmek için 2 ardışık çift tırnak `""` kullanılarak kaçılabilir.

    var str = @"""I don't think so,"" he said.";
    Console.WriteLine(str);

**Çıktı:**
>"Sanmıyorum" dedi.

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/c4OJoq)

## Verbatim dizeleri, derleyiciye karakter çıkışlarını kullanmamasını söyler
Normal bir dizgede, ters eğik çizgi karakteri, dizgedeki gerçek karakteri belirlemek için derleyiciye sonraki karakter(ler)e bakmasını söyleyen kaçış karakteridir. ([Karakter kaçışlarının tam listesi][1])

Verbatim dizelerinde, karakter kaçışları yoktur (bir ""`e dönüştürülen """ dışında).
Bir kelimesi kelimesine dize kullanmak için, başlangıç ​​tırnak işaretlerinden önce bir "@" işareti koymanız yeterlidir.

Bu sözlü dize

    var filename = @"c:\temp\newfile.txt"

**Çıktı:**

>c:\temp\yenidosya.txt

Sıradan (sözlü olmayan) bir dize kullanmanın aksine:

    var filename = "c:\temp\newfile.txt"

bu çıktı:

    c:    emp
    ewfile.txt

karakter kaçışını kullanma. (`\t` bir sekme karakteri ile değiştirilir ve`\n` yeni bir satır ile değiştirilir.)

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/7kslXQ)






[1]: https://www.wikiod.com/tr/docs/c%23/39/string-escape-sequences#t=201607172257361795538&a=syntax

## Çok Satırlı Dizeler
    var multiLine = @"This is a 

    multiline paragraph";

**Çıktı:**
>Bu bir
>
>çok satırlı paragraf

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/kfOUcH)

Çift tırnak içeren çok satırlı dizeler, aynen dizeler olduklarından, tek bir satırda olduğu gibi kaçılabilir.
 
    var multilineWithDoubleQuotes = @"I went to a city named

                            ""San Diego""

                          during summer vacation.";

[.NET Fiddle'da Canlı Demo](https://dotnetfiddle.net/0hwJpf)

*Burada 2. ve 3. satırların başındaki boşlukların/tablolamaların aslında değişkenin değerinde mevcut olduğuna dikkat edilmelidir; olası çözümler için [bu soruyu](http://stackoverflow.com/questions/7178136/multiline-formatting-for-verbatim-strings-in-c-sharp-prefix-with) kontrol edin.*


