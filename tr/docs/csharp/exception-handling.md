---
title: "İstisna işleme"
slug: "istisna-isleme"
draft: false
images: []
weight: 9143
type: docs
toc: true
---

## Özel İstisnalar Oluşturma
Tıpkı diğer istisnalar gibi oluşturulabilecek özel istisnaları uygulamanıza izin verilir. Bu, çalışma zamanı sırasında istisnalarınızı diğer hatalardan ayırt etmek istediğinizde anlamlıdır.

Bu örnekte, uygulamanın karmaşık bir girdiyi ayrıştırırken karşılaşabileceği sorunların net bir şekilde ele alınması için özel bir istisna oluşturacağız.

# Özel İstisna Sınıfı Oluşturma

Özel bir istisna oluşturmak için bir "İstisna" alt sınıfı oluşturun:

    public class ParserException : Exception
    {
        public ParserException() : 
          base("The parsing went wrong and we have no additional information.") { }
    }

Yakalayıcıya ek bilgi sağlamak istediğinizde özel istisna çok kullanışlı hale gelir:

    public class ParserException : Exception
    {
        public ParserException(string fileName, int lineNumber) : 
          base($"Parser error in {fileName}:{lineNumber}") 
        {
          FileName = fileName;
          LineNumber = lineNumber;
        }
        public string FileName {get; private set;}
        public int LineNumber {get; private set;}    
    }

Şimdi, "(ParserException x)" yakaladığınızda, istisna işlemede ince ayar yapmak için ek anlambiliminiz olacak.

Özel sınıflar, ek senaryoları desteklemek için aşağıdaki özellikleri uygulayabilir.

# yeniden atma

Ayrıştırma işlemi sırasında, orijinal istisna hala ilgi çekicidir. Bu örnekte bu bir "FormatException"dır, çünkü kod bir sayı olması beklenen bir dize parçasını ayrıştırmaya çalışır. Bu durumda özel istisna, '**InnerException**'ın dahil edilmesini desteklemelidir:

    //new constructor:
    ParserException(string msg, Exception inner) : base(msg, inner) {
    }

# serileştirme

Bazı durumlarda istisnalarınızın AppDomain sınırlarını aşması gerekebilir. Bu, ayrıştırıcınız yeni ayrıştırıcı yapılandırmalarının çalışırken yeniden yüklenmesini desteklemek için kendi AppDomain'inde çalışıyorsa geçerlidir. Visual Studio'da, bunun gibi bir kod oluşturmak için `İstisna` şablonunu kullanabilirsiniz.

    [Serializable]
    public class ParserException : Exception
    {
        // Constructor without arguments allows throwing your exception without
        // providing any information, including error message. Should be included
        // if your exception is meaningful without any additional details. Should
        // set message by calling base constructor (default message is not helpful).
        public ParserException()
            : base("Parser failure.")
        {}

        // Constructor with message argument allows overriding default error message.
        // Should be included if users can provide more helpful messages than
        // generic automatically generated messages.
        public ParserException(string message) 
            : base(message)
        {}

        // Constructor for serialization support. If your exception contains custom
        // properties, read their values here.
        protected ParserException(SerializationInfo info, StreamingContext context) 
            : base(info, context)
        {}
    }

# ParserException'ı Kullanmak
   
    try
    {
        Process.StartRun(fileName)
    }
    catch (ParserException ex)
    {
        Console.WriteLine($"{ex.Message} in ${ex.FileName}:${ex.LineNumber}");
    }
    catch (PostProcessException x) 
    {
        ...
    }

İstisnaları yakalamak ve sarmak için özel istisnaları da kullanabilirsiniz. Bu şekilde birçok farklı hata, uygulama için daha kullanışlı olan tek bir hata türüne dönüştürülebilir:

    try
    {
        int foo = int.Parse(token);
    }
    catch (FormatException ex)
    {
        //Assuming you added this constructor
        throw new ParserException(
          $"Failed to read {token} as number.", 
          FileName, 
          LineNumber, 
          ex);
    }

Kendi özel istisnalarınızı yükselterek istisnaları ele alırken, yukarıda gösterildiği gibi, genellikle "InnerException" özelliğine orijinal istisnaya bir referans eklemelisiniz.

# Güvenlik endişeleri

İstisnanın nedenini açığa çıkarmak, kullanıcıların uygulamanızın iç işleyişini görmelerine izin vererek güvenliği tehlikeye atabilirse, iç istisnayı sarmak kötü bir fikir olabilir. Bu, başkaları tarafından kullanılacak bir sınıf kitaplığı oluşturuyorsanız geçerli olabilir.

İç istisnayı sarmadan özel bir istisnayı nasıl oluşturabileceğiniz aşağıda açıklanmıştır:

    try
    {
      // ...
    }
    catch (SomeStandardException ex)
    {
      // ...
      throw new MyCustomException(someMessage);
    }

# Çözüm

Özel bir istisna oluştururken (ya sarmalı veya sarmalanmamış yeni bir istisna ile), arayan için anlamlı olan bir istisna oluşturmalısınız. Örneğin, bir sınıf kitaplığının kullanıcısı, o kitaplığın kendi dahili çalışmasını nasıl yaptığı hakkında fazla bir şey bilmiyor olabilir. Sınıf kitaplığının bağımlılıkları tarafından oluşturulan istisnalar anlamlı değildir. Bunun yerine kullanıcı, sınıf kitaplığının bu bağımlılıkları hatalı bir şekilde nasıl kullandığıyla ilgili bir istisna ister.

    try
    {
      // ...
    }
    catch (IOException ex)
    {
      // ...
      throw new StorageServiceException(@"The Storage Service encountered a problem saving
    your data. Please consult the inner exception for technical details. 
    If you are not able to resolve the problem, please call 555-555-1234 for technical       
    assistance.", ex);
    }

## Sonunda engelle
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception)
    {
        /* handle the exception */
    }
    finally
    {
        /* Code that will be executed, regardless if an exception was thrown / caught or not */
    }
    
Dosyalardan okurken `try / catch / nihayet` bloğu çok kullanışlı olabilir.
Örneğin:

    FileStream f = null;
    
    try
    {
        f = File.OpenRead("file.txt");
        /* process the file here */
    }
    finally
    {
        f?.Close(); // f may be null, so use the null conditional operator.
    }


Bir try bloğunu ya bir "catch" ya da bir "finally" bloğu takip etmelidir. Ancak catch bloğu olmadığı için yürütme sonlandırmaya neden olacaktır. Sonlandırmadan önce, nihayet bloğunun içindeki ifadeler yürütülecektir.

Dosya okumada, "FileStream" ("OpenRead"in döndürdüğü) "IDisposable"ı uyguladığı için bir "using" bloğunu kullanabilirdik.

'try' bloğunda bir 'return' ifadesi olsa bile, 'finally' bloğu genellikle yürütülür; olmayacağı birkaç durum vardır:

- Bir [StackOverflow oluştuğunda][1].
- [`Environment.FailFast`](https://msdn.microsoft.com/en-us/library/system.environment.failfast.aspx)
- Başvuru süreci genellikle harici bir kaynak tarafından sonlandırılır.


[1]: https://msdn.microsoft.com/en-us/library/system.stackoverflowexception(v=vs.110).aspx

## En İyi Uygulamalar
## Kopya kağıdı

| DO| YAPMA |
| ------ | ------ |
| Kontrol ifadeleriyle kontrol akışı | İstisnalarla akışı kontrol edin|
| Günlüğe kaydederek yok sayılan (absorbe edilen) istisnayı takip edin|İstisnayı yoksay|
| `throw`|Re-throw istisna - `throw new ArgumentNullException()` veya `throw ex` |
| Önceden tanımlanmış sistem istisnalarını atın| Önceden tanımlanmış sistem istisnalarına benzer özel istisnalar atın|
| Uygulama mantığı için çok önemliyse özel/önceden tanımlanmış istisnayı atın | Akışta bir uyarı belirtmek için özel/önceden tanımlanmış istisnalar atın|
| Ele almak istediğiniz istisnaları yakalayın| Her istisnayı yakala |


## İstisnalar dışında iş mantığını YÖNETMEYİN. ##

Akış kontrolü istisnalar tarafından YAPILMAMALIDIR. Bunun yerine koşullu ifadeler kullanın. Eğer bir kontrol 'if-else' ifadesi ile açıkça yapılabiliyorsa, okunabilirliği ve performansı azalttığı için istisnalar kullanmayın.

Bay Kötü Uygulamalar tarafından yazılan şu pasajı göz önünde bulundurun:

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        Console.WriteLine(myObject.ToString());
    }

Yürütme `Console.WriteLine(myObject.ToString());` değerine ulaştığında uygulama bir NullReferenceException oluşturur. Bay Bad Practices, "myObject" öğesinin boş olduğunu fark etti ve snippet'ini "NullReferenceException"ı yakalamak ve işlemek için düzenledi:

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        try
        {
            Console.WriteLine(myObject.ToString());
        }
        catch(NullReferenceException ex)
        {
            // Hmmm, if I create a new instance of object and assign it to myObject:
            myObject = new object();
            // Nice, now I can continue to work with myObject
            DoSomethingElseWithMyObject();
        }
    }

Önceki pasaj yalnızca istisna mantığını kapsadığından, bu noktada "myObject" boş değilse ne yapmalıyım? Mantığın bu kısmını nerede ele almalıyım? `Console.WriteLine(myObject.ToString());` den hemen sonra? `try...catch` bloğundan sonra ne dersiniz?

Bay En İyi Uygulamalara ne dersiniz? Bunu nasıl halledecekti?

    // This is a snippet example for DO
    object myObject;
    void DoingSomethingWithMyObject()
    {
        if(myObject == null)
            myObject = new object();
        
        // When execution reaches this point, we are sure that myObject is not null
        DoSomethingElseWithMyObject();
    }

Mr. Best Practices aynı mantığı daha az kodla ve açık ve anlaşılır bir mantıkla elde etti.

## İstisnaları yeniden ATMAYIN ##

İstisnaları yeniden atmak pahalıdır. Performansı olumsuz etkiler. Rutin olarak başarısız olan kodlar için performans sorunlarını en aza indirmek için tasarım kalıplarını kullanabilirsiniz. [Bu konu][1], istisnaların performansı önemli ölçüde etkileyebileceği durumlarda kullanışlı olan iki tasarım modelini açıklar.

## Günlük kaydı olmayan istisnaları SEMMEYİN ##

    try
    {
        //Some code that might throw an exception
    }
    catch(Exception ex)
    {
        //empty catch block, bad practice
    }

İstisnaları asla yutmayın. İstisnaları göz ardı etmek o anı kurtaracak, ancak daha sonra sürdürülebilirlik için bir kaos yaratacaktır. İstisnaları günlüğe kaydederken, yalnızca istisna mesajının değil, tüm yığın izlemesinin günlüğe kaydedilmesi için istisna örneğini her zaman günlüğe kaydetmelisiniz.

    try
    {
        //Some code that might throw an exception
    }
    catch(NullException ex)
    {
        LogManager.Log(ex.ToString());
    }

## İşleyemeyeceğiniz istisnaları yakalama ##

[Bu] [2] gibi birçok kaynak, bir istisnayı yakaladığınız yerde neden yakaladığınızı düşünmenizi şiddetle tavsiye eder. Bir istisnayı yalnızca o konumda halledebiliyorsanız yakalamalısınız. Alternatif bir algoritma denemek, yedek bir veritabanına bağlanmak, başka bir dosya adı denemek, 30 saniye bekleyip tekrar denemek veya bir yöneticiye bildirmek gibi sorunu azaltmaya yardımcı olacak bir şey yapabilirseniz, hatayı yakalayabilir ve bunu yapabilirsiniz. Makul ve makul bir şekilde yapabileceğiniz hiçbir şey yoksa, "bırakın" ve istisnanın daha yüksek bir düzeyde ele alınmasına izin verin. İstisna yeterince yıkıcıysa ve sorunun ciddiyeti nedeniyle tüm programın çökmesinden başka makul bir seçenek yoksa, bırakın çöksün.

    try
    {
        //Try to save the data to the main database.
    }
    catch(SqlException ex)
    {
        //Try to save the data to the alternative database.
    }
    //If anything other than a SqlException is thrown, there is nothing we can do here. Let the exception bubble up to a level where it can be handled.

[1]: https://msdn.microsoft.com/en-us/library/ms229009(v=vs.100).aspx
[2]: http://c2.com/cgi/wiki?DontCatchExceptions

## İstisna Anti-desenleri
# Yutma İstisnaları

Kişi her zaman istisnayı aşağıdaki şekilde yeniden atmalıdır:

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw;
    }


Aşağıdaki gibi bir istisnayı yeniden atmak, orijinal istisnayı gizler ve orijinal yığın izini kaybeder. Bunu asla yapmamalı! Yakalama ve yeniden atma işleminden önceki yığın izi kaybolacaktır.

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw ex;
    }

# Beyzbol İstisnası İşleme

İstisnalar, if-then ifadeleri ve while döngüleri gibi [normal akış kontrol yapıları için bir ikame] olarak [1] kullanılmamalıdır. Bu anti-kalıba bazen [Beyzbol İstisna İşleme[2] adı verilir.

İşte anti-kalıba bir örnek:

    try
    {
        while (AccountManager.HasMoreAccounts())
        {
            account = AccountManager.GetNextAccount();
            if (account.Name == userName)
            {
                //We found it
                throw new AccountFoundException(account);
            }
        }
    }
    catch (AccountFoundException found)
    {
        Console.Write("Here are your account details: " + found.Account.Details.ToString());
    }

İşte bunu yapmanın daha iyi bir yolu:

    Account found = null;
    while (AccountManager.HasMoreAccounts() && (found==null))
    {
        account = AccountManager.GetNextAccount();
        if (account.Name == userName)
        {
            //We found it
            found = account;
        }
    }
    Console.Write("Here are your account details: " + found.Details.ToString());

# yakalama (İstisna)

Kodunuzda genel istisna türünü yakalamak için neredeyse hiç (bazıları yok diyor!) neden yoktur. Yalnızca olmasını beklediğiniz istisna türlerini yakalamalısınız, aksi takdirde kodunuzdaki hataları gizlersiniz.

    try 
    {
         var f = File.Open(myfile);
         // do something
    }
    catch (Exception x)
    {
         // Assume file not found
         Console.Write("Could not open file");
         // but maybe the error was a NullReferenceException because of a bug in the file handling code?
    }

Daha iyi yap:

    try 
    {
         var f = File.Open(myfile);
         // do something which should normally not throw exceptions
    }
    catch (IOException)
    {
         Console.Write("File not found");
    }
    // Unfortunatelly, this one does not derive from the above, so declare separatelly
    catch (UnauthorizedAccessException) 
    {
         Console.Write("Insufficient rights");
    }

Başka bir istisna olursa, bilerek uygulamanın çökmesine izin veririz, böylece doğrudan hata ayıklayıcıya girer ve sorunu çözebiliriz. Zaten bunlardan başka istisnaların olduğu bir program göndermemeliyiz, bu yüzden çökme olması sorun değil.

Aşağıdaki de kötü bir örnektir, çünkü bir programlama hatasına geçici bir çözüm bulmak için istisnalar kullanır. Bunun için tasarlanmamışlardır.

    public void DoSomething(String s)
    {
         if (s == null)
             throw new ArgumentNullException(nameof(s));
         // Implementation goes here
    }
    
    try 
    {    
         DoSomething(myString);
    }
    catch(ArgumentNullException x)
    {
        // if this happens, we have a programming error and we should check
        // why myString was null in the first place.
    }

[1]: http://c2.com/cgi/wiki?DontUseExceptionsForFlowControl
[2]: http://www.stackprinter.com/questions/new-programming-jargon-you-coined.html

## Temel İstisna İşleme
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception ex)
    {
        /* handle the exception */
    }
Tüm istisnaları aynı kodla ele almanın çoğu zaman en iyi yaklaşım olmadığını unutmayın.
Bu, son çare olarak, herhangi bir iç özel durum işleme rutini başarısız olduğunda yaygın olarak kullanılır.

## Belirli istisna türlerini işleme
    try
    {
        /* code to open a file */
    }
    catch (System.IO.FileNotFoundException)
    {
        /* code to handle the file being not found */
    }
    catch (System.IO.UnauthorizedAccessException)
    {
        /* code to handle not being allowed access to the file */
    }
    catch (System.IO.IOException)
    {
        /* code to handle IOException or it's descendant other than the previous two */
    }
    catch (System.Exception)
    {
        /* code to handle other errors */
    }

İstisnaların sırayla değerlendirilmesine ve kalıtım uygulanmasına dikkat edin. Bu yüzden en spesifik olanlarla başlamanız ve onların atalarıyla bitirmeniz gerekiyor.
Herhangi bir noktada, yalnızca bir yakalama bloğu yürütülür.

## Bir yöntemden istisnaları / birden çok istisnayı toplayın
Bir yöntemde birden fazla istisna atamayacağınızı kim söyledi. AggregateExceptions ile oynamaya alışkın değilseniz, yanlış giden birçok şeyi temsil etmek için kendi veri yapınızı oluşturmaya cazip gelebilirsiniz. Elbette, bir doğrulamanın sonuçları gibi bir istisna olmayan daha ideal olabilecek başka bir veri yapısı vardı. AggregateExceptions ile oynasanız bile, alıcı tarafta olabilirsiniz ve her zaman sizin için yararlı olabileceklerini fark etmeden onları ele alabilirsiniz.

Bir yöntemin yürütülmesi oldukça mantıklıdır ve bir bütün olarak bir başarısızlık olacak olsa da, atılan istisnalarda yanlış giden birden çok şeyi vurgulamak isteyeceksiniz. Örnek olarak, bu davranış, Paralel yöntemlerin nasıl çalıştığının birden çok iş parçacığına bölünmüş bir görev olduğu ve herhangi bir sayıda istisna atabileceği ve bunun raporlanması gerektiği ile görülebilir. İşte bundan nasıl yararlanabileceğinize dair aptalca bir örnek:

        public void Run()
        {
            try
            {
                this.SillyMethod(1, 2);
            }
            catch (AggregateException ex)
            {
                Console.WriteLine(ex.Message);
                foreach (Exception innerException in ex.InnerExceptions)
                {
                    Console.WriteLine(innerException.Message);
                }
            }
        }

        private void SillyMethod(int input1, int input2)
        {
            var exceptions = new List<Exception>();

            if (input1 == 1)
            {
                exceptions.Add(new ArgumentException("I do not like ones"));
            }
            if (input2 == 2)
            {
                exceptions.Add(new ArgumentException("I do not like twos"));
            }
            if (exceptions.Any())
            {
                throw new AggregateException("Funny stuff happended during execution", exceptions);
            }
        }

## Bir istisna atma
Kodunuz, olağandışı bir şey olduğunda bir istisna oluşturabilir ve genellikle olmalıdır.

    public void WalkInto(Destination destination)
    {
        if (destination.Name == "Mordor")
        {
            throw new InvalidOperationException("One does not simply walk into Mordor.");
        }
        // ... Implement your normal walking code here.
    }

## İşlenmeyen ve İş Parçacığı İstisnası
**AppDomain.UnhandledException**
Bu olay, yakalanmayan istisnaların bildirimini sağlar. Sistem varsayılan işleyicisi istisnayı kullanıcıya bildirmeden ve uygulamayı sonlandırmadan önce uygulamanın istisna hakkındaki bilgileri günlüğe kaydetmesine izin verir. Uygulamanın durumu hakkında yeterli bilgi mevcutsa, başka eylemler yapılabilir. üstlenilir — örneğin program verilerini daha sonra kurtarmak üzere kaydetmek gibi. Özel durumlar işlenmediğinde program verileri bozulabileceğinden dikkatli olunması önerilir.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);            
        }
**Application.ThreadException**
Bu olay, Windows Forms uygulamanızın, Windows Forms iş parçacıklarında meydana gelen, aksi halde işlenmeyen özel durumları işlemesine olanak tanır. Uygulamanızı bilinmeyen bir durumda bırakacak olan bu istisnalarla başa çıkmak için olay işleyicilerinizi ThreadException olayına ekleyin. Mümkün olduğunda istisnalar, yapılandırılmış bir istisna işleme bloğu tarafından ele alınmalıdır.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);
            Application.ThreadException += new ThreadExceptionEventHandler(ThreadException);
        }
Ve son olarak istisna işleme

    static void UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            Exception ex = (Exception)e.ExceptionObject;
            // your code
        }

    static void ThreadException(object sender, ThreadExceptionEventArgs e)
        {
            Exception ex = e.Exception;
            // your code
        }

## İstisna nesnesini kullanma
Kendi kodunuzda istisnalar oluşturmanıza ve atmanıza izin verilir.
Bir istisna örneği, diğer herhangi bir C# nesnesiyle aynı şekilde yapılır.

    Exception ex = new Exception();

    // constructor with an overload that takes a message string
    Exception ex = new Exception("Error message"); 

Daha sonra istisnayı oluşturmak için "throw" anahtar sözcüğünü kullanabilirsiniz:

    
    try
    {
        throw new Exception("Error");
    }
    catch (Exception ex)
    {
        Console.Write(ex.Message); // Logs 'Error' to the output window
    } 


**Not:** Bir catch bloğunun içine yeni bir istisna atıyorsanız, orijinal istisnanın "iç istisna" olarak iletildiğinden emin olun, ör.

    void DoSomething() 
    {
        int b=1; int c=5;
        try
        {
            var a = 1; 
            b = a - 1;
            c = a / b;
            a = a / c;
        }        
        catch (DivideByZeroException dEx) when (b==0)
        {
            // we're throwing the same kind of exception
            throw new DivideByZeroException("Cannot divide by b because it is zero", dEx);
        }
        catch (DivideByZeroException dEx) when (c==0)
        {
            // we're throwing the same kind of exception
            throw new DivideByZeroException("Cannot divide by c because it is zero", dEx);
        }
    }

    void Main()
    {    
        try
        {
            DoSomething();
        }
        catch (Exception ex)
        {
            // Logs full error information (incl. inner exception)
            Console.Write(ex.ToString()); 
        }    
    }

Bu durumda, istisnanın işlenemeyeceği varsayılır, ancak mesaja bazı faydalı bilgiler eklenir (ve orijinal istisnaya, bir dış istisna bloğu tarafından 'ex.InnerException' aracılığıyla hala erişilebilir).

Şunun gibi bir şey gösterecek:

> System.DivideByZeroException: Sıfır olduğu için b ile bölünemez ---> System.DivideByZeroException: Sıfıra bölünmeye çalışıldı. <br/>
> UserQuery'de.<Main>g__DoSomething0_0() içinde C:\[...]\LINQPadQuery.cs:satır 36 <br/>
> --- İç istisna yığın izlemesinin sonu --- <br/>
> UserQuery'de.<Main>g__DoSomething0_0() içinde C:\[...]\LINQPadQuery.cs:satır 42 <br/>
> C:\[...]\LINQPadQuery.cs:satır 55 içinde UserQuery.Main() adresinde <br/>

Bu örneği LinqPad'de deniyorsanız, satır numaralarının çok anlamlı olmadığını fark edeceksiniz (size her zaman yardımcı olmazlar). Ancak, çoğu zaman yukarıda önerildiği gibi yardımcı bir hata metni iletmek, hatanın yerini takip etme süresini önemli ölçüde azaltır, bu örnekte açıkça satırdır.

> c = a / b;

'DoSomething()' işlevinde.

**[.NET Fiddle'da deneyin](https://dotnetfiddle.net/Widget/JLUXXY)**


## WCF Hizmetleri için IErrorHandler Uygulaması
WCF hizmetleri için IErrorHandler'ı uygulamak, hata işlemeyi ve günlüğe kaydetmeyi merkezileştirmenin harika bir yoludur. Burada gösterilen uygulama, WCF hizmetlerinizden birine yapılan bir çağrının sonucu olarak atılan işlenmemiş istisnaları yakalamalıdır. Ayrıca bu örnekte, özel bir nesnenin nasıl döndürüleceği ve varsayılan XML yerine JSON'un nasıl döndürüleceği de gösterilmiştir.

IErrorHandler'ı uygulayın:
 
    using System.ServiceModel.Channels;
    using System.ServiceModel.Dispatcher;
    using System.Runtime.Serialization.Json;
    using System.ServiceModel;
    using System.ServiceModel.Web;

    namespace BehaviorsAndInspectors
    {
        public class ErrorHandler : IErrorHandler
        {

            public bool HandleError(Exception ex)
            {
                // Log exceptions here

                return true;

            } // end

            public void ProvideFault(Exception ex, MessageVersion version, ref Message fault)
            {
                // Get the outgoing response portion of the current context
                var response = WebOperationContext.Current.OutgoingResponse;

                // Set the default http status code 
                response.StatusCode = HttpStatusCode.InternalServerError;

                // Add ContentType header that specifies we are using JSON
                response.ContentType = new MediaTypeHeaderValue("application/json").ToString();

                // Create the fault message that is returned (note the ref parameter) with BaseDataResponseContract                
                fault = Message.CreateMessage(
                    version,
                    string.Empty,
                    new CustomReturnType { ErrorMessage = "An unhandled exception occurred!" },
                    new DataContractJsonSerializer(typeof(BaseDataResponseContract), new List<Type> { typeof(BaseDataResponseContract) }));

                if (ex.GetType() == typeof(VariousExceptionTypes))
                {
                     // You might want to catch different types of exceptions here and process them differently
                }

                // Tell WCF to use JSON encoding rather than default XML
                var webBodyFormatMessageProperty = new WebBodyFormatMessageProperty(WebContentFormat.Json);
                fault.Properties.Add(WebBodyFormatMessageProperty.Name, webBodyFormatMessageProperty);

            } // end

        } // end class

    } // end namespace

Bu örnekte, işleyiciyi hizmet davranışına ekliyoruz. Bunu IEndpointBehavior, IContractBehavior veya IOperationBehavior'a da benzer şekilde ekleyebilirsiniz.
    
Hizmet Davranışlarına Ekle:

    using System;
    using System.Collections.ObjectModel;
    using System.ServiceModel;
    using System.ServiceModel.Channels;
    using System.ServiceModel.Configuration;
    using System.ServiceModel.Description;
    using System.ServiceModel.Dispatcher;

    namespace BehaviorsAndInspectors
    {
        public class ErrorHandlerExtension : BehaviorExtensionElement, IServiceBehavior
        {
            public override Type BehaviorType
            {
                get { return GetType(); }
            }

            protected override object CreateBehavior()
            {
                return this;
            }

            private IErrorHandler GetInstance()
            {
                return new ErrorHandler();
            }

            void IServiceBehavior.AddBindingParameters(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase, Collection<ServiceEndpoint> endpoints, BindingParameterCollection bindingParameters) { } // end

            void IServiceBehavior.ApplyDispatchBehavior(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase)
            {
                var errorHandlerInstance = GetInstance();

                foreach (ChannelDispatcher dispatcher in serviceHostBase.ChannelDispatchers)
                {
                    dispatcher.ErrorHandlers.Add(errorHandlerInstance);
                }
            }

            void IServiceBehavior.Validate(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase) { } // end
          
        } // end class

    } // end namespace

Web.config'deki yapılandırmalar:

    ...
    <system.serviceModel>

        <services>      
          <service name="WebServices.MyService">
            <endpoint binding="webHttpBinding" contract="WebServices.IMyService" />
          </service>
        </services>

        <extensions>      
          <behaviorExtensions>        
            <!-- This extension if for the WCF Error Handling-->
            <add name="ErrorHandlerBehavior" type="WebServices.BehaviorsAndInspectors.ErrorHandlerExtensionBehavior, WebServices, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null" />      
          </behaviorExtensions>    
        </extensions>

        <behaviors>          
          <serviceBehaviors>        
            <behavior>
              <serviceMetadata httpGetEnabled="true"/>
              <serviceDebug includeExceptionDetailInFaults="true"/>
              <ErrorHandlerBehavior />
            </behavior>     
          </serviceBehaviors>    
        </behaviors>

        ....
    </system.serviceModel>
    ...

İşte bu konu hakkında yardımcı olabilecek birkaç bağlantı:

https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx

http://www.brainthud.com/cards/5218/25441/what-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they- uygulamak-ve

Diğer Örnekler:

http://stackoverflow.com/questions/38231970/ierrorhandler-returning-wrong-message-body-while-http-status-code-is-401-unauthor

http://stackoverflow.com/questions/3036692/ierrorhandler-doesnt-seem-to-be-handling-my-errors-in-wcf-any-ideas

http://stackoverflow.com/questions/1149037/how-to-make-custom-wcf-error-handler-return-json-response-with-non-ok-http-code

http://stackoverflow.com/questions/10679214/how-do-you-set-the-content-type-header-for-an-httpclient-request?rq=1

## İstisnaların Yuvalanması ve yakalama bloklarını deneyin.
Bir istisna / `try` `catch` bloğunu diğerinin içine yerleştirebilir.

Bu şekilde, tüm mekanizmanızı bozmadan çalışabilen küçük kod bloklarını yönetebilirsiniz.

    try 
    {
    //some code here
        try 
        {
            //some thing which throws an exception. For Eg : divide by 0
        }
        catch (DivideByZeroException dzEx)
        {
            //handle here only this exception
            //throw from here will be passed on to the parent catch block
        }
        finally
        {
            //any thing to do after it is done.
        }
     //resume from here & proceed as normal; 
    }
    catch(Exception e)
    {
        //handle here
    }

**Not:** Üst yakalama bloğuna atarken [Yutma İstisnalarından][1] kaçının


[1]: https://www.wikiod.com/tr/docs/c%23/40/exception-handling/6940/exception-anti-patterns#t=201707281310293021372

