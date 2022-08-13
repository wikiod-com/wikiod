---
title: "Açıklamayı Kullanma"
slug: "acklamay-kullanma"
draft: false
images: []
weight: 9015
type: docs
toc: true
---

[IDisposable](https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netframework-4.7) nesnelerinin doğru kullanımını sağlayan kullanışlı bir sözdizimi sağlar.



## Sözdizimi
- kullanma (tek kullanımlık) { }
- kullanma (IDisposable tek kullanımlık = new MyDisposable()) { }


"using" ifadesindeki nesne, "IDisposable" arabirimini uygulamalıdır.

    using(var obj = new MyObject())
    {
    }
    
    class MyObject : IDisposable
    {
        public void Dispose()
        {
            // Cleanup
        }
    }

'IDisposable' uygulaması için daha eksiksiz örnekler [MSDN belgelerinde][1] bulunabilir.


[1]: https://msdn.microsoft.com/en-us/library/fs2xkftw(v=vs.110).aspx
  

## Gotcha: elden çıkardığınız kaynağı iade etmek
Aşağıdaki kötü bir fikirdir çünkü 'db' değişkenini döndürmeden önce elden çıkarır.

    public IDBContext GetDBContext()
    {
        using (var db = new DBContext())
        {
            return db;
        }
    }

Bu, daha ince hatalar da yaratabilir:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age);
        }
    }

Bu iyi görünüyor, ancak yakalama, LINQ ifade değerlendirmesinin tembel olması ve muhtemelen yalnızca daha sonra temeldeki 'DBContext' zaten atıldığında yürütüleceğidir.

Yani kısaca ifade, 'kullanım'dan ayrılmadan önce değerlendirilmez. Hâlâ 'kullanma'yı kullanan bu soruna olası bir çözüm, sonucu numaralandıracak bir yöntemi çağırarak ifadenin hemen değerlendirilmesini sağlamaktır. Örneğin, 'ToList()', 'ToArray()', vb. Entity Framework'ün en yeni sürümünü kullanıyorsanız, 'ToListAsync()' veya 'ToArrayAsync()' gibi 'async' karşılıklarını kullanabilirsiniz.

Aşağıda eylemdeki örneği bulabilirsiniz:

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age).ToList();
        }
    }

Yine de, 'ToList()' veya 'ToArray()' çağrıldığında ifadenin hevesle değerlendirileceğini, yani yineleme yapmasanız bile belirtilen yaştaki tüm kişilerin belleğe yükleneceğini not etmek önemlidir. onlar üzerinde.

## Temel İfadeleri Kullanma
'use', açık bir 'try-finally' bloğuna ihtiyaç duymadan bir kaynağın temizlenmesini garanti etmenizi sağlayan sözdizimsel şekerdir. Bu, kodunuzun çok daha temiz olacağı ve yönetilmeyen kaynakları sızdırmayacağınız anlamına gelir.

"IDisposable" arabirimini uygulayan nesneler için standart "Dispose" temizleme modeli ("FileStream"in temel sınıfı "Stream"in .NET'te yaptığı):

    int Foo()
    {
        var fileName = "file.txt";

        {
            FileStream disposable = null;

            try
            {
                disposable = File.Open(fileName, FileMode.Open);

                return disposable.ReadByte();
            }
            finally
            {
                // finally blocks are always run
                if (disposable != null) disposable.Dispose();
            }
        }
    }

"kullanma", açık "try-finally" ifadesini gizleyerek sözdiziminizi basitleştirir:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            return disposable.ReadByte();
        }
        // disposable.Dispose is called even if we return earlier
    }

Tıpkı "nihai" bloklarının hatalardan veya geri dönüşlerden bağımsız olarak her zaman yürütülmesi gibi, "kullanma" da bir hata durumunda bile her zaman "Dispose()" işlevini çağırır:

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            throw new InvalidOperationException();
        }
        // disposable.Dispose is called even if we throw an exception earlier
    }
**Not:**
Kod akışından bağımsız olarak "Dispose"un çağrılması garanti edildiğinden, "Dispose" öğesinin "IDisposable"ı uyguladığınızda hiçbir zaman bir istisna oluşturmadığından emin olmak iyi bir fikirdir. Aksi takdirde, gerçek bir istisna, yeni istisna tarafından geçersiz kılınarak hata ayıklama kabusu ile sonuçlanır.

Blok kullanımından dönüş
---

    using ( var disposable = new DisposableItem() )
    {
        return disposable.SomeProperty;
    }

'Using' bloğunun çevirdiği 'try..finally' semantiği nedeniyle, 'return' ifadesi beklendiği gibi çalışır - 'finally' bloğu yürütülmeden ve değer atılmadan önce dönüş değeri değerlendirilir. Değerlendirme sırası aşağıdaki gibidir:

1. 'dene' metnini değerlendirin
2. Döndürülen değeri değerlendirin ve önbelleğe alın
3. Sonunda bloğu yürütün
4. Önbelleğe alınmış dönüş değerini döndür

Ancak geçersiz, atılmış referans içereceğinden "disposable" değişkeninin kendisini döndüremezsiniz - bkz. [ilgili örnek][1].


[1]: https://www.wikiod.com/tr/docs/c%23/38/using-statement/327/gotcha-returning-the-resource-what-you-are-disposed#t=201608220847304515557

## Bir blokla çoklu kullanım ifadeleri
Birden çok iç içe ayraç düzeyi eklemeden birden çok iç içe "kullanma" ifadesini kullanmak mümkündür. Örneğin:

    using (var input = File.OpenRead("input.txt"))
    {
        using (var output = File.OpenWrite("output.txt"))
        {
            input.CopyTo(output);
        } // output is disposed here
    } // input is disposed here

Bir alternatif yazmaktır:

    using (var input = File.OpenRead("input.txt"))
    using (var output = File.OpenWrite("output.txt"))
    {
        input.CopyTo(output);
    } // output and then input are disposed here

Hangisi ilk örneğe tam olarak eşdeğerdir.

*Not:* İç içe "kullanma" ifadeleri, Microsoft Kod Analizi kuralını [CS2002][1] tetikleyebilir (açıklama için [bu yanıt][2]'e bakın) ve bir uyarı oluşturabilir. Bağlantılı yanıtta açıklandığı gibi, 'use' ifadelerini iç içe yerleştirmek genellikle güvenlidir.

'using' deyimi içindeki türler aynı türden olduğunda, onları virgülle ayırabilir ve türü yalnızca bir kez belirtebilirsiniz (ancak bu yaygın değildir):

    using (FileStream file = File.Open("MyFile.txt"), file2 = File.Open("MyFile2.txt"))
    {
    }

Bu, türler paylaşılan bir hiyerarşiye sahip olduğunda da kullanılabilir:

    using (Stream file = File.Open("MyFile.txt"), data = new MemoryStream())
    {
    }

Yukarıdaki örnekte `var` anahtar kelimesi *cannot* kullanılamaz. Derleme hatası oluşur. Bildirilen değişkenler farklı hiyerarşilerden türlere sahip olduğunda, virgülle ayrılmış bildirim bile çalışmaz.

[1]: https://msdn.microsoft.com/en-us/library/ms182334.aspx
[2]: http://stackoverflow.com/a/22323027/501011

## Gotcha: Blokları kullanmadaki diğer hataları maskeleyen Dispose yönteminde istisna
Aşağıdaki kod bloğunu göz önünde bulundurun.

    try
    {
        using (var disposable = new MyDisposable())
        {
            throw new Exception("Couldn't perform operation.");
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine(ex.Message);
    }

    class MyDisposable : IDisposable
    {
        public void Dispose()
        {
            throw new Exception("Couldn't dispose successfully.");
        }
    }

Konsola yazdırılan "İşlem gerçekleştirilemedi" ifadesini görmeyi bekleyebilirsiniz, ancak aslında "Başarılı bir şekilde elden çıkarılamadı" ifadesini görürsünüz. İlk istisna atıldıktan sonra bile Dispose yöntemi hala çağrıldığından.

Nesnenin atılmasını engelleyen ve hata ayıklamayı zorlaştıran gerçek hatayı maskeliyor olabileceğinden, bu inceliğin farkında olmaya değer.



## İfadeleri kullanmak boş güvenlidir
'null' için 'IDisposable' nesnesini kontrol etmeniz gerekmez. 'using' bir istisna oluşturmaz ve 'Dispose()' çağrılmaz:

    DisposableObject TryOpenFile()
    {
        return null;
    }

    // disposable is null here, but this does not throw an exception 
    using (var disposable = TryOpenFile())
    {
        // this will throw a NullReferenceException because disposable is null
        disposable.DoSomething(); 

        if(disposable != null)
        {
            // here we are safe because disposable has been checked for null
            disposable.DoSomething();
        }
    }

## Özel kapsamı tanımlamak için Dispose Syntax'ı kullanma
Bazı kullanım durumlarında, özel bir kapsam tanımlamaya yardımcı olması için "kullanma" sözdizimini kullanabilirsiniz. Örneğin, belirli bir kültürde kod yürütmek için aşağıdaki sınıfı tanımlayabilirsiniz.

    public class CultureContext : IDisposable
    {
        private readonly CultureInfo originalCulture;

        public CultureContext(string culture)
        {
            originalCulture = CultureInfo.CurrentCulture;
            Thread.CurrentThread.CurrentCulture = new CultureInfo(culture);
        }

        public void Dispose()
        {
            Thread.CurrentThread.CurrentCulture = originalCulture;
        }
    }

Daha sonra, belirli bir kültürde yürütülen kod bloklarını tanımlamak için bu sınıfı kullanabilirsiniz.

    Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");

    using (new CultureContext("nl-NL"))
    {
        // Code in this block uses the "nl-NL" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25-12-2016 00:00:00
    }

    using (new CultureContext("es-ES"))
    {        
        // Code in this block uses the "es-ES" culture
        Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25/12/2016 0:00:00
    }

    // Reverted back to the original culture
    Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 12/25/2016 12:00:00 AM

Not: Oluşturduğumuz `CultureContext` örneğini kullanmadığımız için ona bir değişken atmıyoruz.

Bu teknik, ASP.NET MVC'de ``BeginForm`` [helper][1] tarafından kullanılır.

[1]: https://msdn.microsoft.com/en-us/library/dd410596%28v=vs.100%29.aspx

## İfadeleri ve Veritabanı Bağlantılarını Kullanma
'using' anahtar sözcüğü, deyim içinde tanımlanan kaynağın yalnızca deyimin kendi kapsamında var olmasını sağlar. İfade içinde tanımlanan tüm kaynaklar, "IDisposable" arabirimini uygulamalıdır.

Bunlar, 'IDisposable' arabirimini uygulayan herhangi bir bağlantıyla uğraşırken inanılmaz derecede önemlidir, çünkü bağlantıların yalnızca düzgün bir şekilde kapatılmasını sağlamakla kalmaz, aynı zamanda 'use' ifadesi kapsam dışı kaldıktan sonra kaynaklarının serbest kalmasını sağlayabilir.


**Ortak "Isposable" Veri Sınıfları**
---

Aşağıdakilerin çoğu, "IDisposable" arabirimini uygulayan ve bir "using" ifadesi için mükemmel adaylar olan veriyle ilgili sınıflardır:

- `SqlConnection`,`SqlCommand`,`SqlDataReader` vb.
- "OleDbConnection", "OleDbCommand", "OleDbDataReader" vb.
- "MySqlConnection", "MySqlCommand", "MySqlDbDataReader" vb.
- `DbContext`

Bunların tümü, C# aracılığıyla verilere erişmek için yaygın olarak kullanılır ve veri merkezli uygulamalar oluştururken yaygın olarak karşılaşılır. Aynı 'FooConnection', 'FooCommand', 'FooDataReader' sınıflarını uygulayan belirtilmeyen diğer birçok sınıfın da aynı şekilde davranması beklenebilir.

**ADO.NET Bağlantıları için Ortak Erişim Modeli**
----

Bir ADO.NET bağlantısı üzerinden verilerinize erişirken kullanılabilecek yaygın bir kalıp aşağıdaki gibi görünebilir:

    // This scopes the connection (your specific class may vary)
    using(var connection = new SqlConnection("{your-connection-string}")
    {
        // Build your query
        var query = "SELECT * FROM YourTable WHERE Property = @property");
        // Scope your command to execute
        using(var command = new SqlCommand(query, connection))
        {
             // Open your connection
             connection.Open();

             // Add your parameters here if necessary

             // Execute your query as a reader (again scoped with a using statement)
             using(var reader = command.ExecuteReader())
             {
                   // Iterate through your results here
             }
        }
    }

Veya sadece basit bir güncelleme yapıyorsanız ve bir okuyucuya ihtiyaç duymuyorsanız, aynı temel konsept geçerli olacaktır:

    using(var connection = new SqlConnection("{your-connection-string}"))
    {
         var query = "UPDATE YourTable SET Property = Value WHERE Foo = @foo";
         using(var command = new SqlCommand(query,connection))
         {
              connection.Open();
              
              // Add parameters here
              
              // Perform your update
              command.ExecuteNonQuery();
         }
    }

**Bildirimleri DataContexts ile Kullanma**
---

Entity Framework gibi birçok ORM, "DbContext" gibi sınıflar biçiminde temel veritabanlarıyla etkileşim kurmak için kullanılan soyutlama sınıflarını ortaya çıkarır. Bu bağlamlar genellikle "IDisposable" arabirimini de uygular ve mümkün olduğunda "using" ifadeleri aracılığıyla bundan yararlanmalıdır:

    using(var context = new YourDbContext())
    {
          // Access your context and perform your query
          var data = context.Widgets.ToList();
    }




## Kısıtlama bağlamında kod yürütme
Belirli bir (kısıtlama) bağlamı altında yürütmek istediğiniz kodunuz (* bir rutin*) varsa, bağımlılık enjeksiyonunu kullanabilirsiniz.

Aşağıdaki örnek, açık bir SSL bağlantısı altında yürütme kısıtlamasını gösterir. Bu ilk kısım, müşteri koduna maruz bırakmayacağınız kitaplığınızda veya çerçevenizde olacaktır.

    public static class SSLContext
    {
        // define the delegate to inject
        public delegate void TunnelRoutine(BinaryReader sslReader, BinaryWriter sslWriter);

        // this allows the routine to be executed under SSL
        public static void ClientTunnel(TcpClient tcpClient, TunnelRoutine routine)
        {
            using (SslStream sslStream = new SslStream(tcpClient.GetStream(), true, _validate))
            {
                sslStream.AuthenticateAsClient(HOSTNAME, null, SslProtocols.Tls, false);

                if (!sslStream.IsAuthenticated)
                {
                    throw new SecurityException("SSL tunnel not authenticated");
                }

                if (!sslStream.IsEncrypted)
                {
                    throw new SecurityException("SSL tunnel not encrypted");
                }

                using (BinaryReader sslReader = new BinaryReader(sslStream))
                using (BinaryWriter sslWriter = new BinaryWriter(sslStream))
                {
                    routine(sslReader, sslWriter);
                }
            }
        }
    }

Şimdi, SSL altında bir şeyler yapmak isteyen ancak tüm SSL ayrıntılarını işlemek istemeyen istemci kodu. Artık SSL tüneli içinde istediğiniz her şeyi yapabilirsiniz, örneğin simetrik bir anahtar alışverişi:

    public void ExchangeSymmetricKey(BinaryReader sslReader, BinaryWriter sslWriter)
    {
        byte[] bytes = new byte[8];
        (new RNGCryptoServiceProvider()).GetNonZeroBytes(bytes);
        sslWriter.Write(BitConverter.ToUInt64(bytes, 0));
    }

Bu rutini aşağıdaki gibi yürütürsünüz:

    SSLContext.ClientTunnel(tcpClient, this.ExchangeSymmetricKey);

Bunu yapmak için, 'using()' yan tümcesine ihtiyacınız vardır, çünkü ('try..finally' bloğu dışında) müşteri kodunun ('ExchangeSmetricKey') tek kullanımlık kodu uygun şekilde atmadan asla çıkmayacağını garanti etmenin tek yolu budur. kaynaklar. 'using()' yan tümcesi olmadan, bir rutinin bağlamın bu kaynakları elden çıkarma konusundaki kısıtlamasını kırıp kıramayacağını asla bilemezsiniz.


