---
title: "Önişlemci yönergeleri"
slug: "onislemci-yonergeleri"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Sözdizimi
- #define *[symbol]* // Bir derleyici sembolü tanımlar.
- #undef *[symbol]* // Derleyici sembolünün tanımını kaldırır.
- #warning *[uyarı mesajı]* // Derleyici uyarısı oluşturur. #if ile kullanışlıdır.
- #error *[hata mesajı]* // Derleyici hatası oluşturur. #if ile kullanışlıdır.
- #line *[satır numarası] (dosya adı)* // Derleyici satır numarasını (ve isteğe bağlı olarak kaynak dosya adını) geçersiz kılar. [T4 metin şablonları](https://msdn.microsoft.com/en-us/library/bb126445.aspx) ile kullanılır.
- #pragma uyarısı [disable|restore] *[uyarı numaraları]* // Derleyici uyarılarını devre dışı bırakır/geri yükler.
- #pragma sağlama toplamı "*[dosyaadı]*" "*[kılavuz]*" "*[sağlama toplamı]*" // Bir kaynak dosyanın içeriğini doğrular.
- #region *[bölge adı]* // Daraltılabilir bir kod bölgesi tanımlar.
- #endregion // Bir kod bölgesi bloğunu sonlandırır.
- #if *[condition]* // Koşul doğruysa aşağıdaki kodu çalıştırır.
- #else // #if'den sonra kullanılır.
- #elif *[koşul]* // #if'den sonra kullanılır.
- #endif // #if ile başlayan koşullu bloğu sonlandırır.

Önişlemci yönergeleri, genellikle kaynak programların değiştirilmesini ve farklı yürütme ortamlarında derlenmesini kolaylaştırmak için kullanılır. Kaynak dosyadaki yönergeler, önişlemciye belirli eylemleri gerçekleştirmesini söyler. Örneğin, önişlemci metindeki belirteçleri değiştirebilir, diğer dosyaların içeriğini kaynak dosyaya ekleyebilir veya metin bölümlerini kaldırarak dosyanın bir bölümünün derlenmesini engelleyebilir. Önişlemci hatları tanınır ve makro genişletmeden önce gerçekleştirilir. Bu nedenle, bir makro önişlemci komutu gibi görünen bir şeye genişlerse, bu komut önişlemci tarafından tanınmaz.
 
Önişlemci deyimleri, kaçış dizilerinin desteklenmemesi dışında, kaynak dosya deyimleriyle aynı karakter setini kullanır. Önişlemci ifadelerinde kullanılan karakter seti, yürütme karakter seti ile aynıdır. Önişlemci ayrıca negatif karakter değerlerini de tanır.

## Koşullu İfadeler

Koşullu ifadeler ("#if", "#elif" vb.) sınırlı sayıda boole işlecini destekler. Bunlar:

- `==` ve `!=`. Bunlar yalnızca sembolün doğru (tanımlanmış) veya yanlış (tanımlanmamış) olup olmadığını test etmek için kullanılabilir.
- `&&`, `||`, `!`
-`()`

Örneğin:

    #if !DEBUG && (SOME_SYMBOL || SOME_OTHER_SYMBOL) && RELEASE == true
    Console.WriteLine("OK!");
    #endif

"Tamam!" Yazan kodu derler. konsola "DEBUG" tanımlı değilse, "SOME_SYMBOL" veya "SOME_OTHER_SYMBOL" tanımlanır ve "RELEASE" tanımlanır.

Not: Bu değiştirmeler _derleme zamanında_ yapılır ve bu nedenle çalışma zamanında inceleme için uygun değildir. '#if' kullanımıyla ortadan kaldırılan kod, derleyici çıktısının bir parçası değildir.

Ayrıca Bakınız: MSDN'de [C# Önişlemci Yönergeleri](https://msdn.microsoft.com/en-us/library/ed8yd1ha.aspx).


## Koşullu İfadeler
Aşağıdakiler derlendiğinde, hangi yönergelerin tanımlandığına bağlı olarak farklı bir değer döndürecektir.

    // Compile with /d:A or /d:B to see the difference
    string SomeFunction() 
    {
    #if A
        return "A";
    #elif B
        return "B";
    #else
        return "C";
    #endif
    }

Koşullu ifadeler genellikle hata ayıklama yapılarına ilişkin ek bilgileri günlüğe kaydetmek için kullanılır.

    void SomeFunc()
    {
        try
        {
            SomeRiskyMethod();
        }
        catch (ArgumentException ex)
        {
            #if DEBUG
            log.Error("SomeFunc", ex);
            #endif

            HandleException(ex);
        }
    }



## Diğer Derleyici Talimatları
# Astar

"#line", uyarı ve hataların çıktısı alınırken derleyici tarafından bildirilen satır numarasını ve dosya adını kontrol eder.

    void Test()
    {
        #line 42 "Answer"
        #line filename "SomeFile.cs"
        int life; // compiler warning CS0168 in "SomeFile.cs" at Line 42
        #line default
        // compiler warnings reset to default
    }

# Pragma Sağlama Toplamı

"#pragma sağlama toplamı", hata ayıklama için oluşturulan bir program veritabanı (PDB) için belirli bir sağlama toplamının belirtilmesine olanak tanır.

    #pragma checksum "MyCode.cs" "{00000000-0000-0000-0000-000000000000}" "{0123456789A}"

## Sembolleri Tanımlama ve Tanımlamama
Derleyici sembolü, belirli kod bölümlerini koşullu olarak yürütmek için kontrol edilebilen derleme zamanında tanımlanan bir anahtar kelimedir.
 
Bir derleyici sembolü tanımlamanın üç yolu vardır. Kod aracılığıyla tanımlanabilirler:
 
    #define MYSYMBOL
 
Visual Studio'da Proje Özellikleri > İnşa > Koşullu Derleme Sembolleri altında tanımlanabilirler:
 
![VS Derleyici Sembolleri](http://i.imgur.com/PHG04dI.png)
 
*(`DEBUG` ve `TRACE` kendi onay kutularına sahip olduğunu ve açıkça belirtilmesi gerekmediğini unutmayın.)*
 
Veya derleme zamanında, C# derleyicisi `csc.exe` üzerindeki `/define:[name]` anahtarı kullanılarak tanımlanabilirler.

`#undefine` yönergesini kullanarak tanımsız semboller de oluşturabilirsiniz.
 
Bunun en yaygın örneği, bir uygulama Hata Ayıklama modunda (Sürüm moduna karşı) derlendiğinde Visual Studio tarafından tanımlanan "DEBUG" sembolüdür.
 
    public void DoBusinessLogic()
    {
        try
        {
            AuthenticateUser();
            LoadAccount();
            ProcessAccount();
            FinalizeTransaction();
        }
        catch (Exception ex)
        {
    #if DEBUG
            System.Diagnostics.Trace.WriteLine("Unhandled exception!");
            System.Diagnostics.Trace.WriteLine(ex);
            throw;
    #else
            LoggingFramework.LogError(ex);
            DisplayFriendlyErrorMessage();
    #endif
        }
    }
 
Yukarıdaki örnekte, uygulamanın iş mantığında bir hata oluştuğunda, uygulama Hata Ayıklama modunda derlenirse (ve 'DEBUG' sembolü ayarlanırsa), hata izleme günlüğüne yazılacak ve istisna olacaktır. hata ayıklama için yeniden atılabilir. Ancak, uygulama Yayın modunda derlenirse (ve "DEBUG" sembolü ayarlanmadıysa), hatayı sessizce günlüğe kaydetmek için bir günlük kaydı çerçevesi kullanılır ve son kullanıcıya kolay anlaşılır bir hata mesajı görüntülenir.

## Bölge Blokları
Daraltılabilir bir kod bölgesi tanımlamak için "#region" ve "#endregion" kullanın.
 
    #region Event Handlers
 
    public void Button_Click(object s, EventArgs e)
    {
        // ...
    }
 
    public void DropDown_SelectedIndexChanged(object s, EventArgs e)
    {
        // ...
    }
 
    #endregion
 
Bu yönergeler, yalnızca daraltılabilir bölgeleri ([Visual Studio](https://www.visualstudio.com/en-us/visual-studio-homepage-vs.aspx) gibi) destekleyen bir IDE kullanıldığında faydalıdır. kod.

## Derleyici Uyarılarını Devre Dışı Bırakma ve Geri Yükleme
Derleyici uyarılarını "#pragma uyarı devre dışı bırakma"yı kullanarak devre dışı bırakabilir ve "#pragma uyarı geri yükleme"yi kullanarak geri yükleyebilirsiniz:
 
    #pragma warning disable CS0168
 
    // Will not generate the "unused variable" compiler warning since it was disabled
    var x = 5;
 
    #pragma warning restore CS0168
 
    // Will generate a compiler warning since the warning was just restored
    var y = 8;
 
Virgülle ayrılmış uyarı numaralarına izin verilir:
 
    #pragma warning disable CS0168, CS0219
 
'CS' öneki isteğe bağlıdır ve hatta karıştırılabilir (bu en iyi uygulama olmasa da):
 
    #pragma warning disable 0168, 0219, CS0414

## Derleyici Uyarıları ve Hataları Oluşturma
Derleyici uyarıları '#warning' yönergesi kullanılarak oluşturulabilir ve benzer şekilde '#error' yönergesi kullanılarak hatalar oluşturulabilir.

<!-- dil: lang-none -->

    #if SOME_SYMBOL
    #error This is a compiler Error.
    #elif SOME_OTHER_SYMBOL
    #warning This is a compiler Warning.
    #endif

## Koşullu özniteliği kullanma
Bir yönteme System.Diagnostics ad alanından bir "Koşullu" özniteliği eklemek, derlemelerinizde hangi yöntemlerin çağrıldığını ve hangilerinin çağrılmadığını kontrol etmenin temiz bir yoludur.

    #define EXAMPLE_A

    using System.Diagnostics;
    class Program
    {
        static void Main()
        {
            ExampleA(); // This method will be called
            ExampleB(); // This method will not be called
        }

        [Conditional("EXAMPLE_A")]
        static void ExampleA() {...}

        [Conditional("EXAMPLE_B")]
        static void ExampleB() {...}
    }

## Proje düzeyinde Özel Ön İşlemciler
Testler için diyelim ki bazı eylemlerin atlanması gerektiğinde proje düzeyinde özel koşullu ön işleme ayarlamak uygundur.

`Çözüm Gezgini`ne gidin -> Değişkeni ayarlamak istediğiniz projede <kbd>Sağ Fare</kbd>'yi tıklayın -> `Özellikler` -> `Yapı` -> Genel olarak `Koşullu derleme sembolleri` alanını bulun ve koşullu değişken burada

[![buraya resim açıklamasını girin][1]][1]


Bazı kodları atlayacak kod örneği:

    public void Init()
    {
        #if !IGNOREREFRESHDB
        // will skip code here
         db.Initialize();
        #endif
    }

[1]: http://i.stack.imgur.com/B2pi1.png


