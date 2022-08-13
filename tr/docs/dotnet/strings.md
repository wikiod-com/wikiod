---
title: "Teller"
slug: "teller"
draft: false
images: []
weight: 9954
type: docs
toc: true
---

.NET dizelerinde "System.String", "System.Char" karakter dizisidir, her karakter bir UTF-16 kodlu kod birimidir. Bu ayrım önemlidir, çünkü _karakter_'in _konuşulan dil_ tanımı ile .NET (ve diğer birçok dilde) karakter tanımları farklıdır.

Doğru bir şekilde [grapheme][1] olarak adlandırılması gereken bir _character_, [glif][2] olarak görüntülenir ve bir veya daha fazla Unicode [kod noktası][3] ile tanımlanır. Her kod noktası daha sonra bir [kod birimleri][4] dizisinde kodlanır. Şimdi neden tek bir 'System.Char'ın her zaman bir grafiği temsil etmediği açık olmalı, gerçek dünyada nasıl farklı olduklarını görelim:

* Bir grafik, [karakterleri birleştirmek][5] nedeniyle iki veya daha fazla kod noktasıyla sonuçlanabilir: <kbd>à</kbd> iki kod noktasından oluşur: _U+0061 LATIN KÜÇÜK HARF A_ ve _U+ 0300 BİRLEŞTİRİLEN MEZAR ACCENT_. Bu en yaygın hatadır çünkü `"à".Length == 2` iken `1` olmasını bekleyebilirsiniz.
* Yinelenen karakterler var, örneğin <kbd>à</kbd> tek bir kod noktası _U+00E0 LATIN KÜÇÜK A HARFİ İLE MEZAR_ veya yukarıda açıklandığı gibi iki kod noktası olabilir. Açıkçası, aynı şeyi karşılaştırmaları gerekir: `"\u00e0" == "\u0061\u0300"` (`"\u00e0".Length != "\u0061\u0300".Length` olsa bile). Bu, `String.Normalize()` yöntemiyle gerçekleştirilen _string normalleştirme_ nedeniyle mümkündür.
* Bir Unicode dizisi, oluşturulmuş veya ayrıştırılmış bir dizi içerebilir, örneğin <kbd>한</kbd> _U+D55C HAN CHARACTER_ karakteri, tek bir kod noktası (UTF-16'da tek bir kod birimi olarak kodlanmış) veya bir <kbd>ᄒ</kbd>, <kbd>ᅡ</kbd> ve <kbd>ᆫ</kbd> hecelerinin ayrıştırılmış dizisi. Eşit olarak karşılaştırılmalıdır.
* Bir kod noktası, birden fazla kod birimine kodlanabilir: <kbd>𠂊</kbd> _U+2008A HAN CHARACTER_ karakteri, hatta iki `System.Char` (`"\ud840\udc8a"`) olarak kodlanmıştır. sadece bir kod noktası ise: UTF-16 kodlaması sabit boyutta değildir! Bu sayısız hatanın (aynı zamanda ciddi güvenlik hatalarının) kaynağıdır, örneğin uygulamanız maksimum bir uzunluk uygularsa ve bu durumda dizgiyi körü körüne keserse, geçersiz bir dizge oluşturabilirsiniz.
* Bazı dillerde [digraph][6] ve trigraf vardır, örneğin Çekçe'de <kbd>ch</kbd> bağımsız bir harftir (<kbd>h</kbd>'den sonra ve <kbd>i</kbd>'den önce dizelerin bir listesini sipariş ederken *chemie*'den önce *fyzika* olacaktır.

Metin işlemeyle ilgili çok daha fazla sorun var, örneğin daha geniş bir giriş ve ilgili argümanlara daha fazla bağlantı için [Karakter karşılaştırmasına göre Unicode uyumlu bir karakteri nasıl gerçekleştirebilirim?][7] konusuna bakın.

Genel olarak, _international_ metinle uğraşırken, bir dizedeki metin öğelerini numaralandırmak için bu basit işlevi kullanabilirsiniz (Unicode vekillerini ve kodlamayı bozmaktan kaçınarak):

    public static class StringExtensions
    {
        public static IEnumerable<string> EnumerateCharacters(this string s)
        {
            if (s == null)
                return Enumerable.Empty<string>();

            var enumerator = StringInfo.GetTextElementEnumerator(s.Normalize());
            while (enumerator.MoveNext())
                yield return (string)enumerator.Value;
        }
    }


[1]: https://en.wikipedia.org/wiki/Grapheme
[2]: https://en.wikipedia.org/wiki/Glyph
[3]: https://en.wikipedia.org/wiki/Code_point
[4]: https://en.wikipedia.org/wiki/Character_encoding#Code_unit
[5]: https://en.wikipedia.org/wiki/Combining_character
[6]: https://en.wikipedia.org/wiki/Digraph_(imla)
[7]: http://stackoverflow.com/q/27229589/1207195

## Karakterleri say
Eğer _characters_ saymanız gerekiyorsa, o zaman, _Remarks_ bölümünde açıklanan nedenlerden dolayı, Length özelliğini kullanamazsınız çünkü bu, karakter değil kod birimleri olan `System.Char` dizisinin uzunluğudur (Unicode kodu değil). noktalar veya grafikler). Doğru kod o zaman:

    int length = text.EnumerateCharacters().Count();

Küçük bir optimizasyon, `EnumerateCharacters()` uzantı yöntemini özellikle bu amaç için yeniden yazabilir:

    public static class StringExtensions
    {
        public static int CountCharacters(this string text)
        {
            if (String.IsNullOrEmpty(text))
                return 0;
    
            int count = 0;
            var enumerator = StringInfo.GetTextElementEnumerator(text);
            while (enumerator.MoveNext())
                ++count;
    
            return count;
        }
    }

## Farklı karakterleri sayın
Eğer farklı karakterleri saymanız gerekiyorsa, *Açıklamalar* bölümünde açıklanan nedenlerden dolayı, sadece 'Length' özelliğini kullanamazsınız çünkü bu, karakter değil kod birimi olan 'System.Char' dizisinin uzunluğudur. (Unicode kod noktaları veya grafikler değil). Örneğin, sadece `text.Distinct().Count()` yazarsanız, yanlış sonuçlar alırsınız, kodu düzeltin:

    int distinctCharactersCount = text.EnumerateCharacters().Count();

Bir adım daha ileri, **her karakterin oluşumunu saymaktır**, performans bir sorun değilse, bunu basitçe şu şekilde yapabilirsiniz (bu örnekte, duruma bakılmaksızın):

    var frequencies = text.EnumerateCharacters()
        .GroupBy(x => x, StringComparer.CurrentCultureIgnoreCase)
        .Select(x => new { Character = x.Key, Count = x.Count() };

## Dizeyi başka bir kodlamaya/koddan dönüştürme
.NET dizeleri 'System.Char' (UTF-16 kod birimleri) içerir. Metni başka bir kodlamayla kaydetmek (veya yönetmek) istiyorsanız, bir `System.Byte` dizisiyle çalışmanız gerekir.

Dönüşümler, birlikte başka bir kodlamaya/bir kodlamadan (bir bayt _X_ kodlu dizi 'bayt[]' bir UTF-16'ya) dönüştürebilen 'System.Text.Encoder' ve 'System.Text.Decoder'dan türetilen sınıflar tarafından gerçekleştirilir. "System.String" olarak kodlanmıştır ve bunun tersi de geçerlidir).

Kodlayıcı/kod çözücü genellikle birbirine çok yakın çalıştığı için, bunlar 'System.Text.Encoding'den türetilen bir sınıfta birlikte gruplandırılırlar, türetilmiş sınıflar popüler kodlamalara (UTF-8, UTF-16 vb.) ).

Örnekler:
=

Bir dizeyi UTF-8'e dönüştürün
-
    byte[] data = Encoding.UTF8.GetBytes("This is my text");
---
UTF-8 verilerini bir dizgeye dönüştürün
-
    var text = Encoding.UTF8.GetString(data);

---
Mevcut bir metin dosyasının kodlamasını değiştirme
-

Bu kod, UTF-8 kodlu bir metin dosyasının içeriğini okuyacak ve UTF-16 olarak kodlanmış olarak geri kaydedecektir. Tüm içeriğini belleğe okuyacağından, dosya büyükse bu kodun uygun olmadığını unutmayın:

    var content = File.ReadAllText(path, Encoding.UTF8);
    File.WriteAllText(content, Encoding.UTF16);

## Dizeleri karşılaştırma
`String` bir referans tipi olmasına rağmen `==` operatörü referanslar yerine string değerlerini karşılaştırır.

Bildiğiniz gibi `string` sadece bir karakter dizisidir. Ancak karakter karakter dizilerin eşitlik kontrolü ve karşılaştırması yapıldığını düşünüyorsanız yanılıyorsunuz. Bu işlem kültüre özgüdür (aşağıdaki Açıklamalara bakın): [kültüre][1] bağlı olarak bazı karakter dizileri eşit olarak değerlendirilebilir.

İki dizenin 'Uzunluk' [özelliklerini][2] karşılaştırarak eşitlik kontrolünü kısa devre yapmadan önce iki kez düşünün!

Varsayılan davranışı değiştirmeniz gerekiyorsa, ek 'StringComparison' [numaralandırma][4] değerini kabul eden 'String.Equals' [method][3] aşırı yüklemelerini kullanın.


[1]: https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx
[2]: https://msdn.microsoft.com/library/system.string.length(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/t4411bks(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.stringcomparison(v=vs.110).aspx

## Bir karakterin oluşumlarını say
_Açıklamalar_ bölümünde açıklanan nedenlerden dolayı bunu basitçe yapamazsınız (belirli bir kod biriminin oluşumlarını saymak istemiyorsanız):

    int count = text.Count(x => x == ch);

Daha karmaşık bir işleve ihtiyacınız var:

    public static int CountOccurrencesOf(this string text, string character)
    {
        return text.EnumerateCharacters()
            .Count(x => String.Equals(x, character, StringComparer.CurrentCulture));
    }

Dizi karşılaştırmasının (kültürde değişmez olan karakter karşılaştırmasının aksine) her zaman belirli bir kültürün kurallarına göre yapılması gerektiğini unutmayın.

## Dizeyi sabit uzunlukta bloklara ayırın
Bir dizgiyi rastgele noktalara bölemeyiz (çünkü bir 'System.Char' tek başına geçerli olmayabilir çünkü bu bir birleştirici karakter veya bir vekilin parçası olduğundan), kod bunu hesaba katmalıdır (_uzunluk_ ile _grafem_ sayısını kastediyorum. _kod-birim_ sayısı değil):

    public static IEnumerable<string> Split(this string value, int desiredLength)
    {
        var characters = StringInfo.GetTextElementEnumerator(value);
        while (characters.MoveNext())
            yield return String.Concat(Take(characters, desiredLength));
    }
    
    private static IEnumerable<string> Take(TextElementEnumerator enumerator, int count)
    {
        for (int i = 0; i < count; ++i)
        {
            yield return (string)enumerator.Current;
    
            if (!enumerator.MoveNext())
                yield break;
        }
    }

## Object.ToString() sanal yöntemi
.NET'teki her şey bir nesnedir, bu nedenle her türün geçersiz kılınabilen 'Nesne' [sınıf][2] içinde tanımlanmış 'ToString()' [yöntemi][1] vardır. Bu yöntemin varsayılan uygulaması yalnızca türün adını döndürür:

    public class Foo
    {
    }
    
    var foo = new Foo();
    Console.WriteLine(foo); // outputs Foo

Değer bir dizeyle birleştirilirken `ToString()` örtük olarak çağrılır:

    public class Foo
    {
        public override string ToString()
        {
            return "I am Foo";
        }
    }
    
    var foo = new Foo();
    Console.WriteLine("I am bar and "+foo);// outputs I am bar and I am Foo

Bu yöntemin sonucu, hata ayıklama araçları tarafından da yaygın olarak kullanılmaktadır. Herhangi bir nedenle bu yöntemi geçersiz kılmak istemiyorsanız, ancak hata ayıklayıcının türünüzün değerini nasıl göstereceğini özelleştirmek istiyorsanız, [DebuggerDisplay Attribute][4] ([MSDN][3]) kullanın:

    // [DebuggerDisplay("Person = FN {FirstName}, LN {LastName}")]
    [DebuggerDisplay("Person = FN {"+nameof(Person.FirstName)+"}, LN {"+nameof(Person.LastName)+"}")]
    public class Person
    {
        public string FirstName { get; set; }
        public string LastName { get; set;}
        // ...
    }


[1]: https://msdn.microsoft.com/en-us/library/system.object.tostring(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.object(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.diagnostics.debuggerdisplayattribute(v=vs.110).aspx
[4]: https://www.wikiod.com/tr/docs/c%23/1062/attributes/4689/debuggerdisplay-attribute#t=201702221225586559231

## Dizelerin değişmezliği
Dizeler değişmezdir. Sadece mevcut dizeyi değiştiremezsiniz. Dize üzerindeki herhangi bir işlem, yeni değere sahip dizenin yeni bir örneğini oluşturur. Bu, çok uzun bir dizede tek bir karakteri değiştirmeniz gerekirse, yeni bir değer için bellek ayrılacağı anlamına gelir.

    string veryLongString = ...
    // memory is allocated
    string newString = veryLongString.Remove(0,1); // removes first character of the string.

Dize değeriyle birçok işlem gerçekleştirmeniz gerekiyorsa, verimli dize işleme için tasarlanmış `StringBuilder` [class][1] kullanın:

    var sb = new StringBuilder(someInitialString);
    foreach(var str in manyManyStrings)
    {
        sb.Append(str);
    } 
    var finalString = sb.ToString();

[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

