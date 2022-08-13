---
title: "C# 6.0 Özellikleri"
slug: "c-60-ozellikleri"
draft: false
images: []
weight: 994
type: docs
toc: true
---

C# dilinin bu altıncı yinelemesi Roslyn derleyicisi tarafından sağlanır. Bu derleyici, .NET Framework'ün 4.6 sürümüyle çıktı, ancak önceki çerçeve sürümlerini hedeflemeye izin vermek için geriye dönük uyumlu bir şekilde kod üretebilir. C# sürüm 6 kodu, .NET 4.0 ile tamamen geriye dönük uyumlu bir şekilde derlenebilir. Daha önceki çerçeveler için de kullanılabilir, ancak ek çerçeve desteği gerektiren bazı özellikler düzgün çalışmayabilir.

C#'ın altıncı sürümü, Visual Studio 2015 ve .NET 4.6 ile birlikte Temmuz 2015'te yayınlandı.

Bazı yeni dil özellikleri eklemenin yanı sıra, derleyicinin tamamen yeniden yazılmasını da içerir. Önceden "csc.exe" C++ ile yazılmış yerel bir Win32 uygulamasıydı, C# 6 ile artık C# ile yazılmış .NET tarafından yönetilen bir uygulamadır. Bu yeniden yazma, "Roslyn" projesi olarak biliniyordu ve kod artık açık kaynak ve [GitHub][1]'de mevcut.


[1]: https://github.com/dotnet/roslyn

## İstisna filtreleri
<!-- tüm diller: lang-cs -->
[İstisna filtreleri][1] geliştiricilere bir [catch][2] bloğuna bir koşul ("boolean" ifadesi biçiminde) ekleme yeteneği vererek "catch"in yalnızca koşul " olarak değerlendirilirse" yürütülmesine izin verir. doğru'.

İstisna filtreleri, orijinal istisnada hata ayıklama bilgilerinin yayılmasına izin verir, burada bir "catch" bloğu içinde bir "if" ifadesi kullanmak ve istisnayı yeniden atmak, orijinal istisnada hata ayıklama bilgilerinin yayılmasını durdurur. İstisna filtreleri ile istisna, koşul karşılanmadıkça* çağrı yığınında yukarı doğru yayılmaya devam eder. Sonuç olarak, istisna filtreleri hata ayıklama deneyimini çok daha kolay hale getirir. 'Throw' deyiminde durmak yerine, hata ayıklayıcı, mevcut durum ve tüm yerel değişkenler korunarak istisnayı atan deyimde duracaktır. Kilitlenme dökümleri de benzer şekilde etkilenir.

>İstisna filtreleri, en başından beri [**CLR**][3] tarafından desteklenmiştir ve on yıldan fazla bir süredir CLR'nin istisna işleme modelinin bir bölümünü ortaya çıkararak VB.NET ve F#'dan erişilebilir durumdadırlar. Yalnızca C# 6.0'ın piyasaya sürülmesinden sonra işlevsellik C# geliştiricileri için de kullanılabilir hale geldi.
---

İstisna filtrelerini kullanma
-

İstisna filtreleri, "catch" ifadesine bir "ne zaman" yan tümcesi eklenerek kullanılır. Bir "ne zaman" yan tümcesinde "bool" döndüren herhangi bir ifadeyi kullanmak mümkündür ([await][4] dışında). Bildirilen İstisna değişkeni "ex", "When" yan tümcesinden erişilebilir:

    var SqlErrorToIgnore = 123;
    try
    {
        DoSQLOperations();
    }
    catch (SqlException ex) when (ex.Number != SqlErrorToIgnore)
    {
        throw new Exception("An error occurred accessing the database", ex);
    }

'When' yan tümceleri ile birden fazla 'catch' bloğu birleştirilebilir. 'true' döndüren ilk 'When' yan tümcesi, istisnanın yakalanmasına neden olacaktır. Onun "catch" bloğu girilecek, diğer "catch" maddeleri yok sayılacak (bunların "ne zaman" maddeleri değerlendirilmeyecektir). Örneğin:

    try
    { ... }
    catch (Exception ex) when (someCondition) //If someCondition evaluates to true,
                                              //the rest of the catches are ignored.
    { ... }
    catch (NotImplementedException ex) when (someMethod()) //someMethod() will only run if
                                                           //someCondition evaluates to false
    { ... }
    catch(Exception ex) // If both when clauses evaluate to false
    { ... }

---
Riskli When cümlesi
-

>>Dikkat**
>
>İstisna filtreleri kullanmak riskli olabilir: "ne zaman" tümcesinden bir "İstisna" atıldığında, "ne zaman" tümcesinden gelen "İstisna" yok sayılır ve "yanlış" olarak değerlendirilir. Bu yaklaşım, geliştiricilerin geçersiz durumlarla ilgilenmeden "ne zaman" yan tümcesi yazmasına olanak tanır.

Aşağıdaki örnek böyle bir senaryoyu göstermektedir:

    public static void Main()
    {
        int a = 7;
        int b = 0;
        try
        {
            DoSomethingThatMightFail();
        }
        catch (Exception ex) when (a / b == 0)
        {
            // This block is never reached because a / b throws an ignored
            // DivideByZeroException which is treated as false.
        }
        catch (Exception ex)
        {
            // This block is reached since the DivideByZeroException in the 
            // previous when clause is ignored.
        }
    }

    public static void DoSomethingThatMightFail()
    {
        // This will always throw an ArgumentNullException.
        Type.GetType(null);
    }

[Demoyu Görüntüle][5]

İstisna filtrelerinin, başarısız kod aynı işlev içindeyken "atma" kullanımıyla ilişkili kafa karıştırıcı satır numarası sorunlarından kaçındığını unutmayın. Örneğin bu durumda satır numarası 3 yerine 6 olarak bildirilir:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) {
    6.     throw;
    7. }

Hata yakalandığından ve 6. satırdaki "throw" ifadesi ile yeniden atıldığından istisna satır numarası 6 olarak bildirilir.

İstisna filtrelerinde aynı şey olmaz:

    1. int a = 0, b = 0;
    2. try {
    3.     int c = a / b;
    4. }
    5. catch (DivideByZeroException) when (a != 0) {
    6.     throw;
    7. }

Bu örnekte 'a' 0 ise, 'catch' yan tümcesi yok sayılır ancak satır numarası olarak 3 rapor edilir. Bunun nedeni, ** yığını açmamalarıdır**. Daha spesifik olarak, istisna 5 satırda yakalanmaz* çünkü "a" aslında "0"a eşittir ve bu nedenle 6. satır çalışmadığından istisnanın 6. satırda yeniden atılması için bir fırsat yoktur.

---

Yan etki olarak günlüğe kaydetme
-

Koşuldaki yöntem çağrıları yan etkilere neden olabilir, bu nedenle istisna filtreleri, istisnaları yakalamadan kod çalıştırmak için kullanılabilir. Bundan yararlanan yaygın bir örnek, her zaman "yanlış" döndüren bir "Günlük" yöntemidir. Bu, istisnayı yeniden oluşturmaya gerek kalmadan hata ayıklama sırasında günlük bilgilerinin izlenmesini sağlar.

>**Bunun** günlük kaydı yapmanın rahat bir yolu gibi görünse de, özellikle 3. taraf kayıt düzenekleri kullanılıyorsa riskli olabileceğini unutmayın. Bunlar, kolayca tespit edilemeyen, açık olmayan durumlarda oturum açarken istisnalar verebilir (yukarıdaki **Risky `When(...)` deyimine** bakın).

<pre><code>dene
{
    DoSomethingThatMightFail(s);
}
catch (örn. istisna) <b>ne zaman</b> (Günlük(örn, "Bir hata oluştu"))
{
    // This catch block will never be reached
}

// ...

statik bool Günlüğü(İstisna ex, dize mesajı, params nesnesi[] argümanlar)
{
    Debug.Print(message, args);
    return false;
}</code></pre>

[Demoyu Görüntüle][6]

C#'ın önceki sürümlerinde yaygın olan yaklaşım, istisnayı günlüğe kaydetmek ve yeniden atmaktı.

<!-- eğer sürüm [lt 6.0] -->
    try
    {
        DoSomethingThatMightFail(s);
    }
    catch (Exception ex)
    {
         Log(ex, "An error occurred");
         throw;
    }

    // ...

    static void Log(Exception ex, string message, params object[] args)
    {
        Debug.Print(message, args);
    }

[Demoyu Görüntüle][7]
<!-- eğer --> son sürüm

---

'nihayet' bloğu
=

[`finally`][8] bloğu, istisna atılıp atılmadığına bakılmaksızın her zaman yürütülür. 'When' is istisna filtrelerindeki ifadelerle ilgili bir incelik, iç 'nihayet' bloklarına girmeden *önce* yığının yukarısında yürütülür. Bu, kod genel durumu değiştirmeye çalıştığında (geçerli iş parçacığının kullanıcısı veya kültürü gibi) beklenmedik sonuçlara ve davranışlara neden olabilir ve onu bir "nihayet" bloğuna geri ayarlar.

Örnek: "nihayet" bloğu
-

    private static bool Flag = false;

    static void Main(string[] args)
    {
        Console.WriteLine("Start");
        try
        {
            SomeOperation();
        }
        catch (Exception) when (EvaluatesTo())
        {
            Console.WriteLine("Catch");
        }
        finally
        {
            Console.WriteLine("Outer Finally");
        }
    }

    private static bool EvaluatesTo()
    {
        Console.WriteLine($"EvaluatesTo: {Flag}");
        return true;
    }

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

Üretilen Çıktı:

>Başlat
Değerlendiren: Doğru
Sonunda iç
Tutmak
Dış Sonunda

[Demoyu Görüntüle][9]

Yukarıdaki örnekte, "SomeOperation" yöntemi, global durum değişikliklerini arayanın "ne zaman" yan tümcelerinde "sızdırmak" istemiyorsa, durumu değiştirmek için bir "catch" bloğu da içermelidir. Örneğin:

    private static void SomeOperation()
    {
        try
        {
            Flag = true;
            throw new Exception("Boom");
        }
        catch
        {
           Flag = false;
           throw;
        }
        finally
        {
            Flag = false;
            Console.WriteLine("Inner Finally");
        }
    }

[`IDisposable`][10] yardımcı sınıflarının aynı amaca ulaşmak için [kullanarak][11] blokların semantiğinden yararlandığını görmek de yaygındır, çünkü `IDisposable.Dispose` her zaman bir `içinde çağrılan bir istisnadan önce çağrılır. using` bloğu yığını köpürtmeye başlar.


[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#exception-filters
[2]: https://www.wikiod.com/tr/docs/c%23/26/keywords/148/try-catch-finally-throw
[3]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx
[4]: https://www.wikiod.com/tr/docs/c%23/26/keywords/5993/async-await
[5]: https://dotnetfiddle.net/Iex6DP
[6]: https://dotnetfiddle.net/pqPc7B
[7]: https://dotnetfiddle.net/kEWLue
[8]: https://www.wikiod.com/tr/docs/c%23/40/exception-handling/172/finally-block
[9]: https://ideone.com/gxfBA8
[10]: https://www.wikiod.com/tr/docs/c%23/1795/idisposable-interface
[11]: https://www.wikiod.com/tr/docs/c%23/26/keywords/5062/using

## Dize enterpolasyonu
Dize enterpolasyonu, geliştiricinin bir dize oluşturmak için "değişkenleri" ve metni birleştirmesine olanak tanır.
___

# Temel Örnek

İki "int" değişkeni oluşturulur: "foo" ve "bar".

    int foo = 34;
    int bar = 42;
    
    string resultString = $"The foo is {foo}, and the bar is {bar}.";

    Console.WriteLine(resultString);

**Çıktı**:
>Foo 34 ve bar 42'dir.

[Demoyu Görüntüle][3]

Dizeler içindeki parantezler şu şekilde kullanılabilir:
<pre><code>var foo = 34;
burada çubuk = 42;

// Dize enterpolasyon gösterimi (yeni stil)
Console.WriteLine($"Foo <b>{{foo}}</b> ve çubuk <b>{{bar}}</b>.");</code></pre>

Bu, aşağıdaki çıktıyı üretir:

>Foo {foo} ve bar {bar}.

___

# Verbatim string değişmezleri ile enterpolasyonu kullanma

Dizeden önce `@` kullanılması, dizenin kelimesi kelimesine yorumlanmasına neden olur. Yani, ör. Unicode karakterler veya satır sonları tam olarak yazıldığı gibi kalacaktır. Ancak bu, aşağıdaki örnekte gösterildiği gibi enterpolasyonlu bir dizedeki ifadeleri etkilemeyecektir:<pre><code>Console.WriteLine($@"Net olmaması durumunda:
\u00B9
foo
<b>{foo}</b>,
ve bar
<b>{bar}</b>.");</code></pre>
Çıktı:
> Açık olmaması durumunda:
\u00B9
foo
34 yaşında,
ve bar
42 yaşında.

[Demoyu Görüntüle][4]

___

# İfade
Dize enterpolasyonuyla, küme parantezleri "{}" içindeki *ifadeler* de değerlendirilebilir. Sonuç, dize içinde karşılık gelen konuma eklenecektir. Örneğin, maksimum "foo" ve "bar"ı hesaplamak ve eklemek için küme parantezleri içinde "Math.Max" kullanın:<pre><code>Console.WriteLine($"Ve daha büyük olanı: <b >{ Math.Max(foo, bar) }</b>");</code></pre>

Çıktı:

>Ve daha büyük olanı: 42

*Not: Küme ayracı ile ifade arasındaki baştaki veya sondaki boşluklar (boşluk, sekme ve CRLF/yeni satır dahil) tamamen yok sayılır ve çıktıya dahil edilmez*

[Demoyu Görüntüle][5]

Başka bir örnek olarak, değişkenler para birimi olarak biçimlendirilebilir:<pre><code>Console.WriteLine($"Foo, 4 ondalık basamak olarak biçimlendirilmiştir: <b>{foo:c4}</b>");< /kod></pre>

Çıktı:

>Foo, para birimi olarak 4 ondalık basamak olarak biçimlendirilmiştir: $34.0000

[Demoyu Görüntüle][6]

Veya tarih olarak biçimlendirilebilirler:<pre><code>Console.WriteLine($"Bugün: <b>{DateTime.Today:dddd, MMMM gg - yyyy}</b>");</code>< /ön>

Çıktı:

>Bugün: 20 Temmuz, Pazartesi - 2015

[Demoyu Görüntüle][7]

[Koşullu (Üçlü) Operatör][8] içeren ifadeler de enterpolasyon içinde değerlendirilebilir. Bununla birlikte, bunlar parantez içine alınmalıdır, çünkü aksi halde iki nokta üst üste, yukarıda gösterildiği gibi biçimlendirmeyi belirtmek için kullanılır:

<pre><code>Console.WriteLine($"{(foo > bar ? "Foo is büyüktür bar!" : "Bar is büyüktür foo!")}");</code></pre>

Çıktı:
>Bar, foo'dan daha büyüktür!

[Demoyu Görüntüle][9]

Koşullu ifadeler ve biçim belirteçleri karıştırılabilir:

    Console.WriteLine($"Environment: {(Environment.Is64BitProcess ? 64 : 32):00'-bit'} process");

Çıktı:

> Çevre: 32 bit işlem

___

# Kaçış dizileri
Ters eğik çizgi (`\`) ve tırnak işareti (`"`) karakterlerinden çıkış, enterpolasyonlu dizelerde, enterpolasyonsuz dizelerde olduğu gibi, hem kelimesi kelimesine hem de kelimesi kelimesine olmayan dize değişmezleri için tam olarak aynı şekilde çalışır:
<pre><code>Console.WriteLine($"Foo şudur: <b>{foo}</b>. Sözlü olmayan bir dizgede, ters eğik çizgi ile \" ve \\ karakterlerinden kaçmamız gerekir.");
Console.WriteLine($@"Foo şudur: <b>{foo}</b>. Bir kelimesi kelimesine dizgede, fazladan bir alıntı ile "" çıkışına ihtiyacımız var, ancak \"'dan kaçmamıza gerek yok);
</code></pre>

Çıktı:
>Foo 34'tür. Sözlü olmayan bir dizgede, " ve \'den ters eğik çizgi ile çıkmamız gerekir.
Foo 34'tür. Bir kelimesi kelimesine dizide, " fazladan bir alıntı ile kaçmamız gerekir, ancak kaçmamız gerekmez \

Enterpolasyonlu bir dizeye bir küme ayracı `{` veya `}` eklemek için iki küme ayracı `{{` veya `}}` kullanın:<pre><code>$"{{foo}} is: <b>{ foo}</b>"</code></pre>

Çıktı:
>{foo}: 34

[Demoyu Görüntüle][10]
___

# FormattableString türü
"$"..."` dize enterpolasyon ifadesinin türü [her zaman değildir][11] basit bir dize. Derleyici, bağlama göre hangi türün atanacağına karar verir:<pre><code>string s = $"merhaba, <b>{name}</b>";
System.FormattableString s = $"Merhaba, <b>{name}</b>";
System.IFormattable s = $"Merhaba, <b>{name}</b>";</code></pre>

Bu aynı zamanda derleyicinin hangi aşırı yüklenmiş yöntemin çağrılacağını seçmesi gerektiğinde tür tercihinin sırasıdır.

Bir [yeni tür][12], "System.FormattableString", biçimlendirilecek bağımsız değişkenlerle birlikte bir bileşik biçim dizesini temsil eder. Bunu, özellikle enterpolasyon argümanlarını işleyen uygulamalar yazmak için kullanın:

    public void AddLogItem(FormattableString formattableString)
    {
        foreach (var arg in formattableString.GetArguments())
        {
            // do something to interpolation argument 'arg'
        }

        // use the standard interpolation and the current culture info
        // to get an ordinary String:
        var formatted = formattableString.ToString();

        // ...
    }
Yukarıdaki yöntemi şu şekilde çağırın:<pre><code>AddLogItem($"Foo <b>{foo}</b> ve çubuk <b>{bar}</b>.");</ kod></pre>
Örneğin, günlüğe kaydetme düzeyi zaten günlük öğesini filtreleyecekse, dize biçimlendirmenin performans maliyetine maruz kalmamayı tercih edebilirsiniz.<hr>
# Örtülü dönüşümler
Enterpolasyonlu bir dizeden örtük tür dönüşümler var:<pre><code>var s = $"Foo: <b>{foo}</b>";
System.IFormattable s = $"Foo: <b>{foo}</b>";</code></pre>
Dizeyi değişmez bağlamla dönüştürmenize izin veren bir 'IFormattable' değişkeni de üretebilirsiniz:<pre><code>var s = $"Bar: <b>{bar}</b>";
System.FormattableString s = $"Çubuk: <b>{bar}</b>";</code></pre><hr>
# Mevcut ve Değişmeyen Kültür Yöntemleri
Kod analizi açıksa, enterpolasyonlu dizelerin tümü [CA1305][13] uyarısı üretecektir ("IFormatProvider" belirtin).
Mevcut kültürü uygulamak için statik bir yöntem kullanılabilir.

    public static class Culture
    {
        public static string Current(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.CurrentCulture);
        }
        public static string Invariant(FormattableString formattableString)
        {
            return formattableString?.ToString(CultureInfo.InvariantCulture);
        }
    }
Ardından, geçerli kültür için doğru bir dize oluşturmak için şu ifadeyi kullanın:<pre><code>Culture.Current($"interpolated <b>{typeof(string).Name}</b> string.")
Culture.Invariant($"interpolated <b>{typeof(string).Name}</b> string.")</code></pre>
**Not**: "Geçerli" ve "Değişmeyen", uzantı yöntemleri olarak oluşturulamaz, çünkü derleyici varsayılan olarak "Dize" türünü *enterpolasyonlu dize ifadesine* atar ve bu da aşağıdaki kodun derlenmemesine neden olur:

    $"interpolated {typeof(string).Name} string.".Current();
"FormattableString" sınıfı zaten "Invariant()" yöntemini içeriyor, bu nedenle değişmez kültüre geçmenin en basit yolu "using static"e güvenmektir:<pre><code>using static System.FormattableString;

string değişmez = Invariant($"Şimdi = <b>{DateTime.Now}</b>");
string current = $"Now = <b>{DateTime.Now}</b>";</code></pre><hr>
# Kamera ARKASI
Enterpolasyonlu dizeler, `String.Format()` için yalnızca sözdizimsel bir şekerdir. Derleyici ([Roslyn][14]) onu sahne arkasında bir 'String.Format'a dönüştürecektir:

    var text = $"Hello {name + lastName}";
    
Yukarıdakiler şöyle bir şeye dönüştürülecektir:

    string text = string.Format("Hello {0}", new object[] {
        name + lastName
    });
<saat>

# Dize İnterpolasyonu ve Linq

Okunabilirliği daha da artırmak için Linq ifadelerinde enterpolasyonlu dizeler kullanmak mümkündür.

    var fooBar = (from DataRow x in fooBarTable.Rows
              select string.Format("{0}{1}", x["foo"], x["bar"])).ToList();

Şu şekilde yeniden yazılabilir:

    var fooBar = (from DataRow x in fooBarTable.Rows
              select $"{x["foo"]}{x["bar"]}").ToList();

# Yeniden Kullanılabilir Enterpolasyonlu Dizeler
`string.Format` ile yeniden kullanılabilir format dizeleri oluşturabilirsiniz:

    public const string ErrorFormat = "Exception caught:\r\n{0}";

    // ...

    Logger.Log(string.Format(ErrorFormat, ex));

Ancak enterpolasyonlu dizeler, var olmayan değişkenlere atıfta bulunan yer tutucularla derlenmeyecektir. Aşağıdakiler derlenmeyecek:

    public const string ErrorFormat = $"Exception caught:\r\n{error}";
    // CS0103: The name 'error' does not exist in the current context

Bunun yerine, değişkenleri tüketen ve bir "Dize" döndüren bir "Func<>" oluşturun:

    public static Func<Exception, string> FormatError =
        error => $"Exception caught:\r\n{error}";

    // ...

    Logger.Log(FormatError(ex));
<saat>

# Dize enterpolasyonu ve yerelleştirme

Uygulamanızı yerelleştiriyorsanız, yerelleştirme ile birlikte dize enterpolasyonunu kullanmanın mümkün olup olmadığını merak edebilirsiniz. Gerçekten de, `String`s like:<pre><code>"My name is <b>{name} {middlename} {surname}</b>"</b> gibi kaynak dosyalarında saklama olanağına sahip olmak güzel olurdu. kod></pre>
çok daha az okunabilir yerine:

    "My name is {0} {1} {2}"
"Dize" enterpolasyon işlemi, *çalışma zamanında* gerçekleşen dizeyi "string.Format" ile biçimlendirmenin aksine, *derleme zamanında* gerçekleşir. Enterpolasyonlu bir dizedeki ifadeler, geçerli bağlamdaki adlara başvurmalı ve kaynak dosyalarında saklanmalıdır. Bu, yerelleştirmeyi kullanmak istiyorsanız, aşağıdaki gibi yapmanız gerektiği anlamına gelir:

    var FirstName = "John";
    
    // method using different resource file "strings"
    // for French ("strings.fr.resx"), German ("strings.de.resx"), 
    // and English ("strings.en.resx")
    void ShowMyNameLocalized(string name, string middlename = "", string surname = "")
    {
        // get localized string
        var localizedMyNameIs = Properties.strings.Hello;
        // insert spaces where necessary
        name = (string.IsNullOrWhiteSpace(name) ? "" : name + " ");
        middlename = (string.IsNullOrWhiteSpace(middlename) ? "" : middlename + " ");
        surname = (string.IsNullOrWhiteSpace(surname) ? "" : surname + " ");
        // display it
        Console.WriteLine($"{localizedMyNameIs} {name}{middlename}{surname}".Trim());
    }

    // switch to French and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("fr-FR");
    ShowMyNameLocalized(FirstName);

    // switch to German and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("de-DE");
    ShowMyNameLocalized(FirstName);

    // switch to US English and greet John
    Thread.CurrentThread.CurrentUICulture = CultureInfo.GetCultureInfo("en-US");
    ShowMyNameLocalized(FirstName);

Yukarıda kullanılan diller için kaynak dizeleri tek tek kaynak dosyalarında doğru bir şekilde saklanıyorsa, aşağıdaki çıktıyı almalısınız:
> Merhaba, benim adım John<br/>
> Merhaba, benim adım John<br/>
> Merhaba, benim adım John<br/>

**Bunun, adın her dilde yerelleştirilmiş dizeyi takip ettiği anlamına geldiğine dikkat edin. Durum böyle değilse, kaynak dizelerine yer tutucular eklemeniz ve yukarıdaki işlevi değiştirmeniz veya işlevdeki kültür bilgisini sorgulamanız ve farklı durumları içeren bir switch case ifadesi sağlamanız gerekir.
Kaynak dosyaları hakkında daha fazla ayrıntı için bkz. [C#'ta yerelleştirme nasıl kullanılır](https://stackoverflow.com/a/1142840/1016343).

Bir çevirinin olmaması durumunda, çoğu kişinin anlayacağı varsayılan bir geri dönüş dili kullanmak iyi bir uygulamadır. İngilizce'yi varsayılan yedek dil olarak kullanmanızı öneririm.

# Özyinelemeli enterpolasyon

Çok kullanışlı olmasa da, enterpolasyonlu bir "dize"nin başka birinin küme parantezleri içinde yinelemeli olarak kullanılmasına izin verilir:

    Console.WriteLine($"String has {$"My class is called {nameof(MyClass)}.".Length} chars:");
    Console.WriteLine($"My class is called {nameof(MyClass)}.");

Çıktı:

> Dize 27 karaktere sahiptir:

> Sınıfımın adı Sınıfım.

[1]: https://github.com/dotnet/roslyn/wiki/New-Language-Features-in-C%23-6#string-interpolation
[2]: https://dotnetfiddle.net/0JjwL5
[3]: https://ideone.com/bRFOaV
[4]: https://dotnetfiddle.net/FLs4Ae
[5]: https://ideone.com/qY1Y4B
[6]: https://ideone.com/CPB8UJ
[7]: https://ideone.com/PkjA6k
[8]: https://msdn.microsoft.com/en-us/library/ty67wk28.aspx
[9]: https://ideone.com/sX6tO3
[10]: https://dotnetfiddle.net/BuudHP
[11]: http://stackoverflow.com/questions/38119074
[12]: https://msdn.microsoft.com/en-us/library/system.formattablestring(v=vs.110).aspx
[13]: https://msdn.microsoft.com/en-us/library/ms182190.aspx
[14]: https://github.com/dotnet/roslyn


## Otomatik özellik başlatıcıları
# Giriiş

Özellikler, `}` kapanışından sonra `=` operatörü ile başlatılabilir. Aşağıdaki "Koordinat" sınıfı, bir özelliği başlatmak için mevcut seçenekleri gösterir:


<!-- eğer [gte 6.0] versiyonu -->
    public class Coordinate
    { 
        public int X { get; set; } = 34; // get or set auto-property with initializer
   
        public int Y { get; } = 89;      // read-only auto-property with initializer              
    }
<!-- eğer --> son sürüm

---

## Farklı Görünürlüğe Sahip Aksesuarlar

Erişimcilerinde farklı görünürlüğe sahip otomatik özellikleri başlatabilirsiniz. Korumalı bir ayarlayıcıya sahip bir örnek:

        public string Name { get; protected set; } = "Cheeze";

Erişimci ayrıca "dahili", "dahili korumalı" veya "özel" olabilir.

---

## Salt Okunur Özellikler

Görünürlükle esnekliğe ek olarak, salt okunur otomatik özellikleri de başlatabilirsiniz. İşte bir örnek:

        public List<string> Ingredients { get; } = 
            new List<string> { "dough", "sauce", "cheese" };

Bu örnek ayrıca, karmaşık bir türe sahip bir özelliğin nasıl başlatılacağını da gösterir. Ayrıca, otomatik özellikler salt yazılır olamaz, bu nedenle salt okunur başlatmayı da engeller.

---

# Eski stil (C# 6.0 öncesi)

C# 6'dan önce, bu çok daha ayrıntılı kod gerektiriyordu. Mülkün varsayılan değer vermesi veya aşağıdaki gibi genel mülkü başlatması için destek özelliği adı verilen fazladan bir değişken kullanıyorduk,

<!-- eğer sürüm [lt 6.0] -->
    public class Coordinate
    {
        private int _x = 34;
        public int X { get { return _x; } set { _x = value; } }
   
        private readonly int _y = 89;
        public int Y { get { return _y; } }
        
        private readonly int _z;
        public int Z { get { return _z; } }
    
        public Coordinate()
        {
            _z = 42;
        }
    }

***Not:** C# 6.0'dan önce, yapıcının içinden okuma ve yazma [**otomatik olarak uygulanan özellikler**][2] (bir alıcı ve ayarlayıcı içeren özellikler) başlatabilirdiniz, ancak beyanı ile aynı çizgide olan özellik*

[Demoyu Görüntüle][3]
<!-- eğer --> son sürüm

---

# Kullanım

Başlatıcılar, tıpkı alan başlatıcılar gibi statik ifadeleri değerlendirmelidir. Statik olmayan üyelere başvurmanız gerekiyorsa, daha önce olduğu gibi yapıcılarda özellikleri başlatabilir veya ifade gövdeli özellikleri kullanabilirsiniz. Aşağıdaki gibi (yorumlanmış) statik olmayan ifadeler bir derleyici hatası üretecektir:

    
    // public decimal X { get; set; } = InitMe();  // generates compiler error

    decimal InitMe() { return 4m; }

Ancak, otomatik özellikleri başlatmak için statik yöntemler **kullanılabilir**:

    public class Rectangle
    {
        public double Length { get; set; } = 1;
        public double Width { get; set; } = 1;
        public double Area { get; set; } = CalculateArea(1, 1);

        public static double CalculateArea(double length, double width)
        {
            return length * width;
        }
    }

Bu yöntem, farklı düzeyde erişimcilere sahip mülklere de uygulanabilir:

    public short Type { get; private set; } = 15;

Otomatik özellik başlatıcı, özelliklerin doğrudan bildirimleri içinde atanmasına izin verir. Salt okunur mülkler için, mülkün değişmez olduğundan emin olmak için gereken tüm gereksinimleri karşılar. Örneğin, aşağıdaki örnekte "FingerPrint" sınıfını düşünün:

    public class FingerPrint
    {
      public DateTime TimeStamp { get; } = DateTime.UtcNow;

      public string User { get; } =
        System.Security.Principal.WindowsPrincipal.Current.Identity.Name;

      public string Process { get; } =
        System.Diagnostics.Process.GetCurrentProcess().ProcessName;
    }

[Demoyu Görüntüle][4]

---

# Dikkat notları

Otomatik özellik veya alan başlatıcıları, '=' yerine '=>' kullanan ve '{ get; }`.

Örneğin, aşağıdaki bildirimlerin her biri farklıdır.

    public class UserGroupDto
    {
        // Read-only auto-property with initializer:       
        public ICollection<UserDto> Users1 { get; } = new HashSet<UserDto>();
        
        // Read-write field with initializer:
        public ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // Read-only auto-property with expression body:
        public ICollection<UserDto> Users3 => new HashSet<UserDto>();
    }

Eksik `{ get; }` özellik bildiriminde ortak bir alanla sonuçlanır. Hem salt okunur otomatik özellik "Users1" hem de okuma-yazma alanı "Users2" yalnızca bir kez başlatılır, ancak bir ortak alan, genellikle istenmeyen bir durum olan koleksiyon örneğinin sınıf dışından değiştirilmesine izin verir. İfade gövdesine sahip salt okunur bir otomatik özelliği, başlatıcı ile salt okunur özelliğe değiştirmek, yalnızca ">" öğesinin "=>" öğesinden kaldırılmasını değil, aynı zamanda "{ get; }`.

'Users3' içindeki farklı sembol ('=' yerine '=>'), özelliğe her erişimin yeni bir 'HashSet<UserDto>' örneğini döndürmesiyle sonuçlanır ki bu, geçerli C# olsa da (derleyicinin bakış açısından) bir koleksiyon üyesi için kullanıldığında istenen davranış olması pek olası değildir.

Yukarıdaki kod şuna eşdeğerdir:

    public class UserGroupDto
    {
        // This is a property returning the same instance
        // which was created when the UserGroupDto was instantiated.
        private ICollection<UserDto> _users1 = new HashSet<UserDto>();
        public ICollection<UserDto> Users1 { get { return _users1; } }

        // This is a field returning the same instance
        // which was created when the UserGroupDto was instantiated.
        public virtual ICollection<UserDto> Users2 = new HashSet<UserDto>();

        // This is a property which returns a new HashSet<UserDto> as
        // an ICollection<UserDto> on each call to it.
        public ICollection<UserDto> Users3 { get { return new HashSet<UserDto>(); } }
    }


[2]: https://www.wikiod.com/tr/docs/c%23/49/properties/3365/auto-implemented-properties#t=201608062134378589394
[3]: http://ideone.com/2OgrPQ
[4]: http://ideone.com/qjDRmx
[5]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/44/expression-bodied-function-members

## Boş yayılım
`?.` operatörü ve `?[...]` operatörü, [boş koşullu operatör][1] olarak adlandırılır. Ayrıca bazen [güvenli navigasyon operatörü][2] gibi başka isimlerle de anılır.

Bu yararlıdır, çünkü '.' (üye erişimci) operatörü 'null' olarak değerlendirilen bir ifadeye uygulanırsa, program bir 'NullReferenceException' verir. Geliştirici bunun yerine `?.` (boş koşullu) operatörünü kullanırsa, ifade bir istisna atmak yerine null olarak değerlendirilir.

`?.` operatörü kullanılıyorsa ve ifade boş değilse, `?.` ve `.` eşdeğerdir.

---

# Temel bilgiler

    var teacherName = classroom.GetTeacher().Name;
    // throws NullReferenceException if GetTeacher() returns null

[Demoyu Görüntüle][3]

"Sınıf"ın bir öğretmeni yoksa, "GetTeacher()", "null" değerini döndürebilir. 'null' olduğunda ve 'Ad' özelliğine erişildiğinde, bir 'NullReferenceException' atılır.

Bu ifadeyi `?.` sözdizimini kullanacak şekilde değiştirirsek, tüm ifadenin sonucu `null` olur:

    var teacherName = classroom.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null

[Demoyu Görüntüle][4]

Daha sonra, eğer "sınıf" aynı zamanda "boş" da olabilseydi, bu ifadeyi şu şekilde de yazabiliriz:

    var teacherName = classroom?.GetTeacher()?.Name;
    // teacherName is null if GetTeacher() returns null OR classroom is null

[Demoyu Görüntüle][5]

Bu bir kısa devre örneğidir: null koşullu operatörü kullanan herhangi bir koşullu erişim işlemi null olarak değerlendirdiğinde, zincirin geri kalanını işlemeden ifadenin tamamı hemen null olarak değerlendirilir.

Boş koşullu işleci içeren bir ifadenin uçbirim üyesi bir değer türünde olduğunda, ifade bu türden bir 'Boş Verilebilir<T>' olarak değerlendirilir ve bu nedenle '?' olmadan ifadenin doğrudan yerine kullanılamaz. .

    bool hasCertification = classroom.GetTeacher().HasCertification;
    // compiles without error but may throw a NullReferenceException at runtime

    bool hasCertification = classroom?.GetTeacher()?.HasCertification;
    // compile time error: implicit conversion from bool? to bool not allowed

    bool? hasCertification = classroom?.GetTeacher()?.HasCertification;
    // works just fine, hasCertification will be null if any part of the chain is null

    bool hasCertification = classroom?.GetTeacher()?.HasCertification.GetValueOrDefault();
    // must extract value from nullable to assign to a value type variable

---

# Null-Coalescing Operatörü (??)

İfade "boş" olarak çözülürse varsayılan bir değer döndürmek için boş koşullu işleci [Boş Birleştirme İşleci][6] (`??`) ile birleştirebilirsiniz. Yukarıdaki örneğimizi kullanarak:

    var teacherName = classroom?.GetTeacher()?.Name ?? "No Name";
    // teacherName will be "No Name" when GetTeacher() 
    // returns null OR classroom is null OR Name is null

---

# Dizin Oluşturucularla Kullanım

Boş koşullu operatör, [dizin oluşturucular][7] ile kullanılabilir:

    var firstStudentName = classroom?.Students?[0]?.Name;

Yukarıdaki örnekte:

* İlk "?.", "sınıf"ın "boş" olmamasını sağlar.
* İkinci "?", tüm "Öğrenciler" koleksiyonunun "boş" olmamasını sağlar.
* Dizin oluşturucudan sonraki üçüncü "?.", "[0]" dizin oluşturucunun bir "boş" nesne döndürmemesini sağlar. Bu işlemin **hala** bir "IndexOutOfRangeException" oluşturabileceğine dikkat edilmelidir.

---

# void Fonksiyonları ile kullanın

Boş koşullu işleç, "void" işlevleriyle de kullanılabilir. Ancak bu durumda, ifade "null" olarak değerlendirilmeyecektir. Sadece bir `NullReferenceException` önleyecektir.

    List<string> list = null;
    list?.Add("hi");          // Does not evaluate to null


---

# Olay Çağrısı ile kullanın

Aşağıdaki olay tanımını varsayarsak:

    private event EventArgs OnCompleted;

Bir olayı çağırırken, geleneksel olarak, hiçbir abonenin olmaması durumunda olayın "boş" olup olmadığını kontrol etmek en iyi uygulamadır:

    var handler = OnCompleted;
    if (handler != null)
    {
        handler(EventArgs.Empty);
    }

Boş koşullu operatör tanıtıldığından, çağrı tek bir satıra indirgenebilir:

    OnCompleted?.Invoke(EventArgs.Empty);

---

# Sınırlamalar

Boş koşullu operatör, değer değil değer üretir, yani özellik ataması, olay aboneliği vb. için kullanılamaz. Örneğin, aşağıdaki kod çalışmaz:

    // Error: The left-hand side of an assignment must be a variable, property or indexer
    Process.GetProcessById(1337)?.EnableRaisingEvents = true;
    // Error: The event can only appear on the left hand side of += or -=
    Process.GetProcessById(1337)?.Exited += OnProcessExited;

---

# Yakaladım

Dikkat:

    int? nameLength = person?.Name.Length;    // safe if 'person' is null

__not__ şununla aynıdır:

    int? nameLength = (person?.Name).Length;  // avoid this

çünkü birincisi şuna karşılık gelir:

    int? nameLength = person != null ? (int?)person.Name.Length : null;

ve ikincisi şuna karşılık gelir:

    int? nameLength = (person != null ? person.Name : null).Length;

Burada iki durum arasındaki farkı açıklamak için üçlü operatör `?:` kullanılmasına rağmen, bu operatörler eşdeğer değildir. Bu, aşağıdaki örnekle kolayca gösterilebilir:

    void Main()
    {
        var foo = new Foo();
        Console.WriteLine("Null propagation");
        Console.WriteLine(foo.Bar?.Length);

        Console.WriteLine("Ternary");
        Console.WriteLine(foo.Bar != null ? foo.Bar.Length : (int?)null);
    }
    
    class Foo
    {
        public string Bar
        {
            get
            {
                Console.WriteLine("I was read");
                return string.Empty;
            }
        }
    }

Hangi çıktılar:

>Boş yayılım
>okundum
>0
>Üçlü
>okundum
>okundum
>0

[Demoyu Görüntüle][8]

Birden çok çağrıyı önlemek için eşdeğer:

    var interimResult = foo.Bar;
    Console.WriteLine(interimResult != null ? interimResult.Length : (int?)null);

Ve bu fark, ifade ağaçlarında boş yayılma operatörünün neden [henüz desteklenmediğini][9] açıklıyor.


[1]: https://msdn.microsoft.com/en-us/library/dn986595.aspx
[2]: https://en.wikipedia.org/wiki/Safe_navigation_operator
[3]: http://ideone.com/p8OGBB
[4]: http://ideone.com/3aqGlE
[5]: http://ideone.com/voljZh
[6]: https://msdn.microsoft.com/en-us/library/ms173224.aspx
[7]: https://msdn.microsoft.com/en-us/library/6x16t2tx.aspx
[8]: https://dotnetfiddle.net/BytXEz
[9]: https://roslyn.codeplex.com/discussions/571077


## İfade gövdeli işlev üyeleri
İfade gövdeli işlev üyeleri, lambda ifadelerinin üye gövdeler olarak kullanılmasına izin verir. Basit üyeler için daha temiz ve daha okunabilir kod sağlayabilir.

İfade gövdeli işlevler, özellikler, dizin oluşturucular, yöntemler ve işleçler için kullanılabilir.

---

# Özellikleri

    public decimal TotalPrice => BasePrice + Taxes;

Şuna eşdeğerdir:

    public decimal TotalPrice
    {
        get
        {
            return BasePrice + Taxes;
        }
    }

Bir özellik ile ifade gövdeli bir işlev kullanıldığında, özellik yalnızca alıcı özelliği olarak uygulanır.

[Demoyu Görüntüle][1]

---

# Dizin oluşturucular

    public object this[string key] => dictionary[key];

Şuna eşdeğerdir:

    public object this[string key]
    {
        get
        {
            return dictionary[key];
        }
    }

---

# Yöntemler

    static int Multiply(int a, int b) => a * b;

Şuna eşdeğerdir:

    static int Multiply(int a, int b)
    {
        return a * b;
    }

'void' yöntemlerle de kullanılabilir:

    public void Dispose() => resource?.Dispose();

"Pair<T>" sınıfına bir "ToString" geçersiz kılma eklenebilir:

    public override string ToString() => $"{First}, {Second}";

Ek olarak, bu basit yaklaşım "override" anahtar sözcüğüyle çalışır:

    public class Foo
    {
        public int Bar { get; }
    
        public string override ToString() => $"Bar: {Bar}";
    }

---

# Operatörler

Bu, operatörler tarafından da kullanılabilir:

    public class Land
    {
        public double Area { get; set; }

        public static Land operator +(Land first, Land second) =>
            new Land { Area = first.Area + second.Area };
    }

---

# Sınırlamalar

İfade gövdeli işlev üyelerinin bazı sınırlamaları vardır. Blok deyimleri ve blok içeren diğer deyimleri içeremezler: 'if', 'switch', 'for', 'foreach', 'while', 'do', 'try', vb.

Bazı "if" ifadeleri üçlü operatörlerle değiştirilebilir. Bazı "for" ve "foreach" ifadeleri LINQ sorgularına dönüştürülebilir, örneğin:

    IEnumerable<string> Digits
    {
        get
        {
            for (int i = 0; i < 10; i++)
                yield return i.ToString();
        }
    }

<!---->

    IEnumerable<string> Digits => Enumerable.Range(0, 10).Select(i => i.ToString());

Diğer tüm durumlarda, işlev üyeleri için eski sözdizimi kullanılabilir.

İfade gövdeli işlev üyeleri "async"/"await" içerebilir, ancak genellikle gereksizdir:

    async Task<int> Foo() => await Bar();  

Şunlarla değiştirilebilir:

    Task<int> Foo() => Bar();

[1]: https://dotnetfiddle.net/djFd7O


## Operatör adı
"nameof" operatörü, bir kod öğesinin adını "dize" olarak döndürür. Bu, yöntem argümanlarıyla ilgili istisnalar atarken ve ayrıca 'INotifyPropertyChanged' uygularken kullanışlıdır.

    public string SayHello(string greeted)
    {
        if (greeted == null)
            throw new ArgumentNullException(nameof(greeted));
        
        Console.WriteLine("Hello, " + greeted);
    }

'nameof' operatörü derleme zamanında değerlendirilir ve ifadeyi bir dizge değişmezine dönüştürür. Bu aynı zamanda, kendilerini açığa çıkaran üyelerinden sonra adlandırılan dizeler için de yararlıdır. Aşağıdakileri göz önünde bulundur:

    public static class Strings
    {
        public const string Foo = nameof(Foo); // Rather than Foo = "Foo"
        public const string Bar = nameof(Bar); // Rather than Bar = "Bar"
    }

"nameof" ifadeleri derleme zamanı sabitleri olduğundan, özniteliklerde, "case" etiketlerinde, "switch" ifadelerinde vb. kullanılabilirler.

<saat/>

"Enum" ile "nameof" kullanmak uygundur. Onun yerine:

    Console.WriteLine(Enum.One.ToString());

kullanmak mümkündür:

    Console.WriteLine(nameof(Enum.One))

Her iki durumda da çıktı "Bir" olacaktır.

<saat/>

'nameof' operatörü, statik benzeri sözdizimi kullanarak statik olmayan üyelere erişebilir. Yapmak yerine:

    string foo = "Foo";
    string lengthName = nameof(foo.Length);

Şunlarla değiştirilebilir:

    string lengthName = nameof(string.Length);

Çıktı, her iki örnekte de 'Uzunluk' olacaktır. Ancak, ikincisi gereksiz örneklerin oluşturulmasını engeller.

<saat/>

'nameof' operatörü çoğu dil yapısıyla çalışsa da, bazı sınırlamalar vardır. Örneğin, açık genel türlerde veya yöntem dönüş değerlerinde "nameof" operatörünü kullanamazsınız:

    public static int Main()
    {   
        Console.WriteLine(nameof(List<>)); // Compile-time error
        Console.WriteLine(nameof(Main())); // Compile-time error
    }

Ayrıca, onu genel bir türe uygularsanız, genel tür parametresi yoksayılır:

    Console.WriteLine(nameof(List<int>));  // "List"
    Console.WriteLine(nameof(List<bool>)); // "List"

Daha fazla örnek için, 'nameof'a ayrılmış [bu konuya][1] bakın.

<saat/>

# Önceki sürümler için geçici çözüm ([daha fazla ayrıntı][2])

6.0'dan önceki sürümler için C#'da "nameof" operatörü bulunmasa da, aşağıdaki gibi "MemberExpression" kullanılarak benzer işlevsellik elde edilebilir:

<!-- eğer sürüm [lt 6.0] -->
İfade:

    public static string NameOf<T>(Expression<Func<T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

    public static string NameOf<TObj, T>(Expression<Func<TObj, T>> propExp)
    {
        var memberExpression = propExp.Body as MemberExpression;
        return memberExpression != null ? memberExpression.Member.Name : null;
    }

Kullanım:

    string variableName = NameOf(() => variable);
    string propertyName = NameOf((Foo o) => o.Bar);

<!-- eğer --> son sürüm

Bu yaklaşımın her çağrıda bir ifade ağacı oluşturulmasına neden olduğuna dikkat edin, bu nedenle performans, derleme zamanında değerlendirilen ve çalışma zamanında sıfır ek yükü olan 'nameof' operatörüne kıyasla çok daha kötü.


[1]: https://www.wikiod.com/tr/docs/c%23/80/nameof-operator#t=201608031424500177545
[2]: https://www.wikiod.com/tr/docs/c%23/80/nameof-operator/26157/name-of-extension-support-added-for-before-c-sharp-6-version#t= 201612071107472552734

## Statik tip kullanma
'using static [Namespace.Type]' yönergesi, türlerin ve numaralandırma değerlerinin statik üyelerinin içe aktarılmasına izin verir. Uzantı yöntemleri, üst düzey kapsama değil, uzantı yöntemleri olarak içe aktarılır (yalnızca bir türden).

<!-- eğer [gte 6.0] versiyonu -->

    using static System.Console;
    using static System.ConsoleColor;
    using static System.Math;
    
    class Program
    {
        static void Main()
        {
            BackgroundColor = DarkBlue;
            WriteLine(Sqrt(2));
        }
    }

[Canlı Demo Fiddle][1]
<!-- eğer --> son sürüm

<!-- eğer sürüm [lt 6.0] -->

    using System;
    
    class Program
    {
        static void Main()
        {
            Console.BackgroundColor = ConsoleColor.DarkBlue;
            Console.WriteLine(Math.Sqrt(2));
        }
    }

<!-- eğer --> son sürüm


[1]: https://dotnetfiddle.net/7Ll3XN

## Dizin başlatıcılar
Dizin başlatıcılar, dizinleri olan nesneleri aynı anda oluşturmayı ve başlatmayı mümkün kılar.

Bu, Sözlükleri başlatmayı çok kolaylaştırır:

    var dict = new Dictionary<string, int>()
    {
        ["foo"] = 34,
        ["bar"] = 42
    };


Dizine alınmış bir alıcı veya ayarlayıcıya sahip herhangi bir nesne bu sözdizimi ile kullanılabilir:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42
            };

            Console.ReadKey();
        }
    }

Çıktı:
>İndeks: foo, değer: 34
>İndeks: çubuk, değer: 42


[Demoyu Görüntüle][1]

Sınıfta birden fazla indeksleyici varsa, hepsini tek bir ifade grubuna atamak mümkündür:

    class Program
    {
        public class MyClassWithIndexer
        {
            public int this[string index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
            public string this[int index]
            {
                set
                {
                    Console.WriteLine($"Index: {index}, value: {value}");
                }
            }
        }

        public static void Main()
        {
            var x = new MyClassWithIndexer()
            {
                ["foo"] = 34,
                ["bar"] = 42,
                [10] = "Ten",
                [42] = "Meaning of life"
            };
        }
    }

Çıktı:
>İndeks: foo, değer: 34
>İndeks: çubuk, değer: 42
>İndeks: 10, değer: On
>İndeks: 42, değer: Hayatın anlamı

Dizin oluşturucu "set" erişimcisinin "Add" yöntemine (koleksiyon başlatıcılarda kullanılır) kıyasla farklı davranabileceğine dikkat edilmelidir.

Örneğin:

    var d = new Dictionary<string, int>
    {
        ["foo"] = 34,
        ["foo"] = 42,
    }; // does not throw, second value overwrites the first one

karşı:

    var d = new Dictionary<string, int>
    {
        { "foo", 34 },
        { "foo", 42 },
    }; // run-time ArgumentException: An item with the same key has already been added.


[1]: https://dotnetfiddle.net/Evs4Qx

## Geliştirilmiş aşırı yük çözünürlüğü
Aşağıdaki kod parçası, bir temsilci beklendiğinde bir yöntem grubunu (lambda yerine) geçirme örneğini gösterir. **C# 6**'nın geçilen yöntemin dönüş türünü kontrol etme yeteneği nedeniyle, aşırı yük çözünürlüğü artık belirsiz bir aşırı yükleme hatası oluşturmak yerine bunu çözecektir.

    using System;
    public class Program
    {
        public static void Main()
        {
            Overloaded(DoSomething);
        }
    
        static void Overloaded(Action action)
        {
           Console.WriteLine("overload with action called");
        }
    
        static void Overloaded(Func<int> function)
        {
           Console.WriteLine("overload with Func<int> called");
        }
    
        static int DoSomething()
        {
            Console.WriteLine(0);
            return 0;
        }
    }

Sonuçlar:

<!-- eğer sürüm [eq 6.0] -->
**Çıktı**
>Func\<int\> ile aşırı yükleme

[Demoyu Görüntüle][1]
<!-- eğer --> son sürüm

<!-- eğer sürüm [eq 5.0] -->
**Hata**
> hata CS0121: Çağrı, aşağıdaki yöntemler veya özellikler arasında belirsizdir:
     'Program.Overloaded(System.Action)' and 'Program.Overloaded(System.Func)'

<!-- eğer --> son sürüm

**C# 6**, **C# 5**'te bir hatayla sonuçlanacak olan lambda ifadeleri için aşağıdaki tam eşleşme durumunu da iyi bir şekilde işleyebilir.

    using System;

    class Program
    {
        static void Foo(Func<Func<long>> func) {}
        static void Foo(Func<Func<int>> func) {}

        static void Main()
        {
            Foo(() => () => 7);
        }
    }


[1]: https://dotnetfiddle.net/Vnudqy

## Yakala ve sonunda bekle
C#6'da 'catch' ve 'finally' bloklarında [await operatörü][1]'i [Tasks][2] veya [Task(Of TResult)][3]'e uygulamak için 'await' ifadesini kullanmak mümkündür. .

Derleyici sınırlamaları nedeniyle önceki sürümlerde 'catch' ve 'finally' bloklarında 'await' ifadesini kullanmak mümkün değildi. C#6, 'bekliyor' ifadesine izin vererek zaman uyumsuz görevleri beklemeyi çok daha kolaylaştırır.

    try
    {
        //since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        //since C#6
        await logger.LogAsync(e);
    }
    finally
    {
        //since C#6
        await service.CloseAsync();
    }

C# 5'te, zaman uyumsuz işlemleri gerçekleştirmek için bir "bool" kullanmak veya try catch dışında bir "İstisna" bildirmek gerekiyordu. Bu yöntem aşağıdaki örnekte gösterilmiştir:
    
    bool error = false;
    Exception ex = null;

    try
    {
        // Since C#5
        await service.InitializeAsync();
    } 
    catch (Exception e)
    {
        // Declare bool or place exception inside variable
        error = true;
        ex = e;
    }

    // If you don't use the exception
    if (error)
    {
        // Handle async task
    }

    // If want to use information from the exception
    if (ex != null)
    {
        await logger.LogAsync(e);
    }    

    // Close the service, since this isn't possible in the finally
    await service.CloseAsync();


[1]: https://msdn.microsoft.com/en-us/library/hh156528.aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.aspx
[3]: https://msdn.microsoft.com/en-us/library/dd321424.aspx

## Küçük değişiklikler ve hata düzeltmeleri
Parantezler artık adlandırılmış parametrelerin etrafında yasaktır. Aşağıdakiler C#5'te derlenir, ancak C#6'da değil

<!-- eğer sürüm [lte 5.0] -->

    Console.WriteLine((value: 23));

<!-- eğer --> son sürüm

"is" ve "as" işlenenlerinin artık yöntem grupları olmasına izin verilmemektedir. Aşağıdakiler C#5'te derlenir, ancak C#6'da değil

<!-- eğer sürüm [lte 5.0] -->

    var result = "".Any is byte;

> Yerel derleyici buna izin verdi (bir uyarı göstermesine rağmen) ve aslında "1.Any is string" veya "IDisposable.Dispose is object" gibi çılgın şeylere izin vererek uzantı yöntemi uyumluluğunu kontrol etmedi.

<!-- eğer --> son sürüm

Değişikliklerle ilgili güncellemeler için [bu referans][1]'e bakın.


[1]: http://blog.slaks.net/2014-05-28/exploring-roslyn-part-3-breaking-changes/

## Koleksiyon başlatma için bir uzantı yöntemi kullanma
Koleksiyon başlatma sözdizimi, 'IEnumerable' uygulayan ve tek bir parametre alan 'Add' adlı bir yöntemi olan herhangi bir sınıfın örneğini oluştururken kullanılabilir.

Önceki sürümlerde, bu "Add" yönteminin, başlatılmakta olan sınıfta bir **örnek** yöntemi olması gerekiyordu. C#6'da bir uzatma yöntemi de olabilir.

    public class CollectionWithAdd : IEnumerable
    {
        public void Add<T>(T item)
        {
            Console.WriteLine("Item added with instance add method: " + item);
        }

        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public class CollectionWithoutAdd : IEnumerable
    {
        public IEnumerator GetEnumerator()
        {
            // Some implementation here
        }
    }
    
    public static class Extensions
    {
        public static void Add<T>(this CollectionWithoutAdd collection, T item)
        {
            Console.WriteLine("Item added with extension add method: " + item);
        }
    }
    
    public class Program
    {
        public static void Main()
        {
            var collection1 = new CollectionWithAdd{1,2,3}; // Valid in all C# versions
            var collection2 = new CollectionWithoutAdd{4,5,6}; // Valid only since C# 6
        }
    }


Bu çıktı:

>Örnek ekleme yöntemiyle eklenen öğe: 1
>Örnek ekleme yöntemiyle eklenen öğe: 2
>Örnek ekleme yöntemiyle eklenen öğe: 3
>Uzantı ekleme yöntemiyle eklenen öğe: 4
>Uzantı ekleme yöntemiyle eklenen öğe: 5
>Uzantı ekleme yöntemiyle eklenen öğe: 6

## Uyarı Geliştirmelerini Devre Dışı Bırak
C# 5.0 ve önceki sürümlerde geliştirici uyarıları yalnızca sayıya göre bastırabilirdi. Roslyn Analyzer'ların kullanıma sunulmasıyla birlikte C#, belirli kitaplıklardan verilen uyarıları devre dışı bırakmak için bir yola ihtiyaç duyuyor. C# 6.0 ile pragma yönergesi uyarıları ada göre bastırabilir.

Önceki:

    #pragma warning disable 0501

C# 6.0:

    #pragma warning disable CS0501

