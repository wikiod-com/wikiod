---
title: "Dize Kaçış Dizileri"
slug: "dize-kacs-dizileri"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Sözdizimi
- \\' — tek tırnak (0x0027)
- \\" — çift tırnak (0x0022)
- \\\ — ters eğik çizgi (0x005C)
- \0 — boş (0x0000)
- \a — uyarı (0x0007)
- \b — geri al (0x0008)
- \f — form beslemesi (0x000C)
- \n — yeni satır (0x000A)
- \r — satır başı (0x000D)
- \t — yatay sekme (0x0009)
- \v — dikey sekme (0x000B)
- \u0000 - \uFFFF — Unicode karakter
- \x0 - \xFFFF — Unicode karakter (değişken uzunlukta kod)
- \U00000000 - \U0010FFFF — Unicode karakter (vekiller oluşturmak için)

Dize kaçış dizileri, **derleme zamanında** karşılık gelen karaktere dönüştürülür. Ters eğik çizgi içeren sıradan dizeler **dönüştürülmez**.

Örneğin, aşağıdaki 'notEscaped' ve 'notEscaped2' dizeleri yeni satır karakterine dönüştürülmez, ancak iki farklı karakter (''\'' ve ''n'') olarak kalır.

    string escaped = "\n";
    string notEscaped = "\\" + "n";
    string notEscaped2 = "\\n";

    Console.WriteLine(escaped.Length); // 1
    Console.WriteLine(notEscaped.Length); // 2            
    Console.WriteLine(notEscaped2.Length); // 2

## Dize değişmezlerinde kaçan özel semboller
**Ters eğik çizgi**

    // The filename will be c:\myfile.txt in both cases
    string filename = "c:\\myfile.txt";
    string filename = @"c:\myfile.txt";

İkinci örnek, ters eğik çizgiyi bir kaçış karakteri olarak değerlendirmeyen bir [verbatim dize değişmezi](https://www.wikiod.com/tr/docs/c%23/16/verbatim-strings#t=20151122021216101385) kullanır.

**Alıntılar**

    string text = "\"Hello World!\", said the quick brown fox.";
    string verbatimText = @"""Hello World!"", said the quick brown fox.";

Her iki değişken de aynı metni içerecektir.

> "Merhaba Dünya!", dedi hızlı kahverengi tilki.

**Yeni satırlar**

Verbatim dize değişmezleri yeni satırlar içerebilir:

    string text = "Hello\r\nWorld!";
    string verbatimText = @"Hello
    World!";

Her iki değişken de aynı metni içerecektir.

## Unicode karakter kaçış dizileri
    string sqrt = "\u221A";      // √
    string emoji = "\U0001F601"; // 😁
    string text = "\u0022Hello World\u0022"; // "Hello World"
    string variableWidth = "\x22Hello World\x22"; // "Hello World"

## Karakter değişmezlerinde kaçan özel semboller
**kesme işaretleri**

    char apostrophe = '\'';
**Ters eğik çizgi**

    char oneBackslash = '\\';

## Tanımlayıcılarda kaçış dizileri kullanma
Kaçış dizileri "string" ve "char" değişmezleri ile sınırlı değildir.

Bir üçüncü taraf yöntemini geçersiz kılmanız gerektiğini varsayalım:

    protected abstract IEnumerable<Texte> ObtenirŒuvres();

ve C# kaynak dosyalarınız için kullandığınız karakter kodlamasında `Œ` karakterinin bulunmadığını varsayalım. Şanslısınız, kodda ___tanımlayıcılarda `\u####` veya `\U########` türünden kaçışlar kullanmanıza izin verilir. Bu yüzden yazmak yasaldır:

    protected override IEnumerable<Texte> Obtenir\u0152uvres()
    {
        // ...
    }

ve C# derleyicisi `Œ` ve `\u0152`nin aynı karakter olduğunu bilecektir.

(Ancak, UTF-8'e veya tüm karakterleri işleyebilen benzer bir kodlamaya geçmek iyi bir fikir olabilir.)

## Tanınmayan kaçış dizileri derleme zamanı hataları üretir
Aşağıdaki örnekler derlenmeyecektir:

    string s = "\c";
    char c = '\c';

Bunun yerine, derleme zamanında 'Tanınmayan kaçış dizisi' hatasını üreteceklerdir.

