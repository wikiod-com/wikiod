---
title: "istisnalar"
slug: "istisnalar"
draft: false
images: []
weight: 9760
type: docs
toc: true
---

İlişkili:

* [MSDN: İstisnalar ve İstisna İşleme (C# Programlama Kılavuzu)](https://msdn.microsoft.com/en-us/library/ms173160.aspx)
* [MSDN: Özel Durumları İşleme ve Atma](https://msdn.microsoft.com/en-us/library/5b2yeyab.aspx)
* [MSDN: CA1031: Genel istisna türlerini yakalama](https://msdn.microsoft.com/en-us/library/ms182137.aspx)
* [MSDN: try-catch (C# Reference)](https://msdn.microsoft.com/en-us/library/0yd65esw.aspx)

## Yakalanan istisnaları yakalama ve yeniden atma
Bir istisna yakalamak ve bir şey yapmak istediğinizde, ancak istisna nedeniyle mevcut kod bloğunun yürütülmesine devam edemediğinizde, istisnayı çağrı yığınındaki bir sonraki istisna işleyicisine yeniden göndermek isteyebilirsiniz. Bunu yapmanın iyi yolları ve kötü yolları vardır.

    private static void AskTheUltimateQuestion()
    {
        try
        {
            var x = 42;
            var y = x / (x - x); // will throw a DivideByZeroException

            // IMPORTANT NOTE: the error in following string format IS intentional
            // and exists to throw an exception to the FormatException catch, below
            Console.WriteLine("The secret to life, the universe, and everything is {1}", y); 
        }
        catch (DivideByZeroException)
        {
            // we do not need a reference to the exception
            Console.WriteLine("Dividing by zero would destroy the universe.");

            // do this to preserve the stack trace:
            throw;
        }
        catch (FormatException ex)
        {
            // only do this if you need to change the type of the Exception to be thrown 
            // and wrap the inner Exception

            // remember that the stack trace of the outer Exception will point to the
            // next line

            // you'll need to examine the InnerException property to get the stack trace
            // to the line that actually started the problem

            throw new InvalidOperationException("Watch your format string indexes.", ex);
        }
        catch (Exception ex)
        {
            Console.WriteLine("Something else horrible happened. The exception: " + ex.Message);

            // do not do this, because the stack trace will be changed to point to
            // this location instead of the location where the exception
            // was originally thrown:
            throw ex; 
        }
    }

    static void Main()
    {
        try
        {
            AskTheUltimateQuestion();
        }
        catch
        {
            // choose this kind of catch if you don't need any information about 
            // the exception that was caught

            // this block "eats" all exceptions instead of rethrowing them
        }
    }

İstisna türüne ve hatta istisna özelliklerine göre filtreleyebilirsiniz (C# 6.0'da yeni, VB.NET'te biraz daha uzun süre kullanılabilir (alıntı gerekli)):

[Belgeler/C#/yeni özellikler][1]


[1]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/46/exception-filters

## Bir nihayet bloğu kullanma
Bir "try-finally" veya "try-catch-finally"nin "finally { ... }" bloğu, bir istisna oluşup oluşmadığına bakılmaksızın (bir "StackOverflowException" atıldığında veya çağrı yapıldığında hariç) her zaman yürütülür. `Environment.FailFast()` olarak yapılmıştır).

`try { ... }` bloğunda edinilen kaynakları güvenli bir şekilde serbest bırakmak veya temizlemek için kullanılabilir.

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream = null;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }
    finally
    {
        if (fileStream != null)
        {
            fileStream.Dispose();
        }
    }


## İstisna Filtreleri
C# 6.0'dan beri istisnalar "ne zaman" operatörü kullanılarak filtrelenebilir.

Bu, basit bir "if" kullanmaya benzer, ancak "ne zaman" içindeki koşul karşılanmıyorsa yığını çözmez.

__Örnek__
    
    try
    { 
      // ...
    }
    catch (Exception e) when (e.InnerException != null) // Any condition can go in here.
    {
      // ...
    }

Aynı bilgi burada [C# 6.0 Özellikleri][1] içinde bulunabilir: [İstisna filtreleri][2]


[1]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features
[2]: https://www.wikiod.com/tr/docs/c%23/24/c-sharp-6-0-features/46/exception-filters#t=201607211048375185447

## Bir yakalama bloğu içinde bir istisnayı yeniden atma
Bir "catch" bloğu içinde "throw" anahtar sözcüğü, bir istisna değeri belirtmeden, yeni yakalanan istisnayı *yeniden göndermek* için tek başına kullanılabilir. Bir istisnanın yeniden atılması, orijinal istisnanın, çağrı yığınını veya ilgili verilerini koruyarak istisna işleme zincirine devam etmesine izin verir:
 
    try {...}
    catch (Exception ex) {
      // Note: the ex variable is *not* used
      throw;
    }

Yaygın bir anti-desen, bunun yerine, bir sonraki istisna işleyicisinin yığın izine bakışını sınırlama etkisine sahip olan "eski atmak"tır:

    try {...}
    catch (Exception ex) {
      // Note: the ex variable is thrown
      //  future stack traces of the exception will not see prior calls
      throw ex;  
    }

Genel olarak 'throw ex' kullanmak istenmez, çünkü yığın izini denetleyen gelecekteki istisna işleyicileri yalnızca 'throw ex' kadar geriye çağrıları görebileceklerdir. 'ex' değişkenini atlayarak ve yalnızca 'throw' anahtar sözcüğünü kullanarak, orijinal istisna ["balon"[1] olacaktır.

[1]: http://stackoverflow.com/questions/4065893/about-throw-and-exception-bubbling

## Bilgilerini korurken farklı bir yöntemden istisna atmak
Bazen bir istisna yakalamak ve orijinal istisna yığınını korurken onu farklı bir iş parçacığından veya yöntemden atmak istersiniz. Bu, `ExceptionDispatchInfo` ile yapılabilir:

    using System.Runtime.ExceptionServices;

    void Main()
    {
        ExceptionDispatchInfo capturedException = null;
        try
        {
            throw new Exception();
        }
        catch (Exception ex)
        {
            capturedException = ExceptionDispatchInfo.Capture(ex);
        }
        
        Foo(capturedException);
    }
    
    void Foo(ExceptionDispatchInfo exceptionDispatchInfo)
    {
        // Do stuff
    
        if (capturedException != null)
        {
            // Exception stack trace will show it was thrown from Main() and not from Foo()
            exceptionDispatchInfo.Throw();
        }
    }

## Bir istisna yakalama
Kod, istisnai durumlarda istisnalar verebilir ve vermelidir. Bunun örnekleri şunları içerir:

- [bir akışın sonunu okumaya çalışmak][1]
- Bir dosyaya erişmek için [gerekli izinlere sahip olmamak][2]
- [sıfıra bölme] gibi geçersiz bir işlem gerçekleştirmeye çalışmak[3]
- İnternetten dosya indirirken [zaman aşımı oluyor][4]

Arayan, bu istisnaları "yakalayarak" işleyebilir ve bunu yalnızca şu durumlarda yapmalıdır:

- İstisnai durumu gerçekten çözebilir veya uygun şekilde düzeltebilir veya;
- İstisnanın yeniden atılması gerektiğinde faydalı olabilecek istisnaya ek bağlam sağlayabilir (yeniden atılan istisnalar, çağrı yığınının ilerisinde istisna işleyicileri tarafından yakalanır)

Unutulmamalıdır ki, bir istisnayı yakalamak için *değil* seçiminin, niyetin daha yüksek bir seviyede ele alınması gerekiyorsa tamamen geçerli olduğu belirtilmelidir.

İstisna yakalama, potansiyel olarak atan kodu aşağıdaki gibi bir `try { ... }` bloğuna sararak ve bir `catch (ExceptionType) { ... }` bloğunda işleyebileceği istisnaları yakalayarak yapılır:

    Console.Write("Please enter a filename: ");
    string filename = Console.ReadLine();

    Stream fileStream;

    try
    {
        fileStream = File.Open(filename);
    }
    catch (FileNotFoundException)
    {
        Console.WriteLine("File '{0}' could not be found.", filename);
    }


[1]: https://msdn.microsoft.com/en-us/library/system.io.endofstreamexception(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.unauthorizedaccessexception(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.dividebyzeroexception(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.net.webexception.aspx

