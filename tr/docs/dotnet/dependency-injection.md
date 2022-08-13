---
title: "Bağımlılık Enjeksiyonu"
slug: "bagmllk-enjeksiyonu"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

**Bağımlılık Enjeksiyonu ile Çözülen Sorunlar**

Bağımlılık enjeksiyonunu kullanmasaydık, "Greeter" sınıfı daha çok şöyle görünebilir:

    public class ControlFreakGreeter
    {
        public void Greet()
        {
            var greetingProvider = new SqlGreetingProvider(
                ConfigurationManager.ConnectionStrings["myConnectionString"].ConnectionString);
            var greeting = greetingProvider.GetGreeting();
            Console.WriteLine(greeting);
        }
    }

Bu bir "kontrol manyağı" çünkü selamlamayı sağlayan sınıfın oluşturulmasını kontrol ediyor, SQL bağlantı dizesinin nereden geldiğini kontrol ediyor ve çıktıyı kontrol ediyor.

Bağımlılık enjeksiyonunu kullanan `Greeter` sınıfı, kendisine sağlanan bir selamlama yazarak, tek bir sorumluluk lehine bu sorumluluklardan feragat eder.

[Dependency Inversion Principle][1], sınıfların diğer somut sınıflardan ziyade soyutlamalara (arayüzler gibi) bağlı olması gerektiğini önerir. Sınıflar arasındaki doğrudan bağımlılıklar (birleştirme), bakımı giderek zorlaştırabilir. Soyutlamalara bağlı olarak bu bağlantıyı azaltabilir.

Bağımlılık enjeksiyonu, bu bağımlılığı tersine çevirmemize yardımcı olur çünkü soyutlamalara bağlı sınıflar yazmaya yol açar. "Greeting" sınıfı, "IGreetingProvider" ve "IGreetingWriter"ın uygulama ayrıntılarının hiçbirini "bilmez". Yalnızca enjekte edilen bağımlılıkların bu arabirimleri uyguladığını bilir. Bu, 'IGreetingProvider' ve 'IGreetingWriter'ı uygulayan somut sınıflarda yapılan değişikliklerin 'Greeter'ı etkilemeyeceği anlamına gelir. Hiçbiri onları tamamen farklı uygulamalarla değiştirmeyecek. Yalnızca arayüzlerdeki değişiklikler olacaktır. "Greeter" ayrıştırılır.

`ControlFreakGreeter` birim testinin düzgün bir şekilde yapılması mümkün değildir. Küçük bir kod birimini test etmek istiyoruz, ancak bunun yerine testimiz SQL'e bağlanmayı ve bir saklı yordamı yürütmeyi içerecek. Ayrıca konsol çıktısının test edilmesini de içerir. ControlFreakGreeter çok şey yaptığı için diğer sınıflardan ayrı olarak test etmek imkansızdır.

'Greeter' birim testi yapmak kolaydır, çünkü bir saklı yordamı çağırmak veya konsolun çıktısını okumaktan daha kolay yürütülmesi ve doğrulanması daha kolay olan bağımlılıklarının alaylı uygulamalarını enjekte edebiliriz. app.config içinde bir bağlantı dizesi gerektirmez.

'IGreetingProvider' ve 'IGreetingWriter'ın somut uygulamaları daha karmaşık hale gelebilir. Onlar da, kendilerine enjekte edilen kendi bağımlılıklarına sahip olabilirler. (Örneğin, SQL bağlantı dizesini `SqlGreetingProvider` içine enjekte ederdik.) Ancak bu karmaşıklık, yalnızca arayüzlere bağlı olan diğer sınıflardan "gizlidir". Bu, diğer sınıflarda karşılık gelen değişiklikleri yapmamızı gerektiren bir "dalgalanma etkisi" olmadan bir sınıfı değiştirmeyi kolaylaştırır.

[1]: https://en.wikipedia.org/wiki/Dependency_inversion_principle

## Bağımlılık Enjeksiyonu - Basit örnek
Bu sınıfa "Greeter" denir. Sorumluluğu bir selam vermektir. İki *bağımlılığı* vardır. Çıktı için selamlamayı verecek bir şeye ihtiyacı var ve sonra bu selamı vermenin bir yoluna ihtiyacı var. Bu bağımlılıkların her ikisi de arayüzler, "IGreetingProvider" ve "IGreetingWriter" olarak tanımlanır. Bu örnekte, bu iki bağımlılık "Greeter"a "enjekte edilmiştir". (Örneği takip eden daha fazla açıklama.)

    public class Greeter
    {
        private readonly IGreetingProvider _greetingProvider;
        private readonly IGreetingWriter _greetingWriter;

        public Greeter(IGreetingProvider greetingProvider, IGreetingWriter greetingWriter)
        {
            _greetingProvider = greetingProvider;
            _greetingWriter = greetingWriter;
        }

        public void Greet()
        {
            var greeting = _greetingProvider.GetGreeting();
            _greetingWriter.WriteGreeting(greeting);
        }
    }
  
    public interface IGreetingProvider
    {
        string GetGreeting();
    }

    public interface IGreetingWriter
    {
        void WriteGreeting(string greeting);
    }

"Greeting" sınıfı, hem "IGreetingProvider" hem de "IGreetingWriter" öğelerine bağlıdır, ancak her ikisinin de örneklerini oluşturmaktan sorumlu değildir. Bunun yerine onları yapıcısında gerektirir. Bir "Greeting" örneğini oluşturan şey, bu iki bağımlılığı sağlamalıdır. Buna bağımlılıkları "enjekte etmek" diyebiliriz.

Bağımlılıklar sınıfa yapıcısında sağlandığı için buna "yapıcı enjeksiyonu" da denir.

Birkaç yaygın sözleşme:

- Yapıcı, bağımlılıkları 'özel' alanlar olarak kaydeder. Sınıf başlatılır başlatılmaz, bu bağımlılıklar sınıfın diğer tüm statik olmayan yöntemleri için kullanılabilir.
- "özel" alanlar "salt okunur" alanlardır. Yapıcıda ayarlandıktan sonra değiştirilemezler. Bu, bu alanların yapıcı dışında değiştirilmemesi (ve değiştirilmemesi) gerektiğini gösterir. Bu ayrıca, bu bağımlılıkların sınıfın ömrü boyunca kullanılabilir olmasını sağlar.
- Bağımlılıklar arayüzlerdir. Bu kesinlikle gerekli değildir, ancak bağımlılığın bir uygulamasını diğeriyle değiştirmeyi kolaylaştırdığı için yaygındır. Ayrıca, birim testi amacıyla arayüzün alaylı bir versiyonunun sağlanmasına da izin verir.

## Bağımlılık Enjeksiyonu Birim Testini Nasıl Kolaylaştırır
Bu, "IGreetingProvider" ve "IGreetingWriter" olmak üzere iki bağımlılığı olan "Greeter" sınıfının önceki örneğini temel alır.

'IGreetingProvider'ın gerçek uygulaması, bir API çağrısından veya bir veritabanından bir dize alabilir. "IGreetingWriter" uygulaması konsolda selamlamayı görüntüleyebilir. Ancak 'Greeter' bağımlılıklarını yapıcısına enjekte ettiği için, bu arayüzlerin alaylı sürümlerini enjekte eden bir birim testi yazmak kolaydır. Gerçek hayatta [Moq][1] gibi bir çerçeve kullanabiliriz, ancak bu durumda bu alaylı uygulamaları yazacağım.

    public class TestGreetingProvider : IGreetingProvider
    {
        public const string TestGreeting = "Hello!";

        public string GetGreeting()
        {
            return TestGreeting;
        }
    }

    public class TestGreetingWriter : List<string>, IGreetingWriter
    {
        public void WriteGreeting(string greeting)
        {
            Add(greeting);
        }
    }

    [TestClass]
    public class GreeterTests
    {
        [TestMethod]
        public void Greeter_WritesGreeting()
        {
            var greetingProvider = new TestGreetingProvider();
            var greetingWriter = new TestGreetingWriter();
            var greeter = new Greeter(greetingProvider, greetingWriter);
            greeter.Greet();
            Assert.AreEqual(greetingWriter[0], TestGreetingProvider.TestGreeting);
        }
    }

'IGreetingProvider' ve 'IGreetingWriter'ın davranışı bu testle ilgili değildir. `Greeter`ın selam alıp yazdığını test etmek istiyoruz. 'Greeter' tasarımı (bağımlılık enjeksiyonunu kullanarak), karmaşık hareketli parçalar olmadan sahte bağımlılıkları enjekte etmemizi sağlar. Tek test ettiğimiz, "Greeter"ın beklediğimiz gibi bu bağımlılıklarla etkileşime girdiği.

[1]: http://www.moqthis.com/

## Neden Dependency Injection Containers (IoC Containers) Kullanıyoruz?
Bağımlılık enjeksiyonu, sınıfları bağımlılıklarını kontrol etmeyecek şekilde yazmak anlamına gelir - bunun yerine bağımlılıkları kendilerine sağlanır ("enjekte edilir").

Bu, Castle Windsor, Autofac, SimpleInjector, Ninject, Unity veya diğerleri gibi bir bağımlılık enjeksiyon çerçevesi (genellikle "DI kapsayıcısı", "IoC kabı" veya yalnızca "kapsayıcı" olarak adlandırılır) kullanmakla aynı şey değildir.

Bir kap, bağımlılık enjeksiyonunu kolaylaştırır. Örneğin, bağımlılık enjeksiyonuna dayanan bir dizi sınıf yazdığınızı varsayalım. Bir sınıf birkaç arabirime bağlıdır, bu arabirimleri uygulayan sınıflar diğer arabirimlere bağlıdır vb. Bazıları belirli değerlere bağlıdır. Ve sadece eğlence için, bu sınıflardan bazıları 'IDisposable'ı uygular ve atılması gerekir.

Her bir sınıf iyi yazılmıştır ve test edilmesi kolaydır. Ancak şimdi farklı bir sorun var: Bir sınıfın örneğini oluşturmak çok daha karmaşık hale geldi. Bir "CustomerService" sınıfının bir örneğini oluşturduğumuzu varsayalım. Bağımlılıkları vardır ve bağımlılıklarının bağımlılıkları vardır. Bir örnek oluşturmak şöyle görünebilir:

    public CustomerData GetCustomerData(string customerNumber)
    {
        var customerApiEndpoint = ConfigurationManager.AppSettings["customerApi:customerApiEndpoint"];
        var logFilePath = ConfigurationManager.AppSettings["logwriter:logFilePath"];
        var authConnectionString = ConfigurationManager.ConnectionStrings["authorization"].ConnectionString;
        using(var logWriter = new LogWriter(logFilePath ))
        {
            using(var customerApiClient = new CustomerApiClient(customerApiEndpoint))
            {
                var customerService = new CustomerService(
                    new SqlAuthorizationRepository(authorizationConnectionString, logWriter),
                    new CustomerDataRepository(customerApiClient, logWriter),
                    logWriter
                );   
                
                // All this just to create an instance of CustomerService!         
                return customerService.GetCustomerData(string customerNumber);
            }
        }
    }

Merak edebilirsiniz, neden tüm dev yapıyı 'CustomerService' döndüren ayrı bir işleve koymuyorsunuz? Bunun bir nedeni, her sınıfın bağımlılıkları ona enjekte edildiğinden, bir sınıfın bu bağımlılıkların 'Isposable' olup olmadığını bilmekten veya onları elden çıkarmaktan sorumlu olmamasıdır. Sadece onları kullanır. Bu nedenle, tamamen yapılandırılmış bir "CustomerService" döndüren bir "GetCustomerService()" işlevine sahip olsaydık, bu sınıf bir dizi tek kullanımlık kaynak içerebilir ve bunlara erişmenin veya elden çıkarmanın hiçbir yolu olmayabilir.

Ve 'IDisposable'ı elden çıkarmanın yanı sıra, kim böyle bir dizi iç içe kurucuyu çağırmak ister ki? Bu kısa bir örnek. Çok, çok daha kötüye gidebilir. Yine, bu, sınıfları yanlış yazdığımız anlamına gelmez. Sınıflar bireysel olarak mükemmel olabilir. Zorluk onları birlikte oluşturmaktır.

Bir bağımlılık enjeksiyon kabı bunu basitleştirir. Her bağımlılığı yerine getirmek için hangi sınıfın veya değerin kullanılması gerektiğini belirtmemize izin verir. Bu biraz fazla basitleştirilmiş örnek, Castle Windsor'u kullanır:

    var container = new WindsorContainer()
    container.Register(
        Component.For<CustomerService>(),
        Component.For<ILogWriter, LogWriter>()
            .DependsOn(Dependency.OnAppSettingsValue("logFilePath", "logWriter:logFilePath")),
        Component.For<IAuthorizationRepository, SqlAuthorizationRepository>()
            .DependsOn(Dependency.OnValue(connectionString, ConfigurationManager.ConnectionStrings["authorization"].ConnectionString)),
        Component.For<ICustomerDataProvider, CustomerApiClient>()
             .DependsOn(Dependency.OnAppSettingsValue("apiEndpoint", "customerApi:customerApiEndpoint"))   
    );

Buna "bağımlılıkları kaydetme" veya "kapsayıcıyı yapılandırma" diyoruz. Çevrilmiş, bu bizim 'WindsorContainer'ımıza şunu söyler:

- Bir sınıf "ILogWriter" gerektiriyorsa, bir "LogWriter" örneği oluşturun. "LogWriter" bir dosya yolu gerektirir. Bu değeri "AppSettings"den kullanın.
- Bir sınıf "IAuthorizationRepository" gerektiriyorsa, "SqlAuthorizationRepository" örneğini oluşturun. Bir bağlantı dizesi gerektirir. `ConnectionStrings` bölümündeki bu değeri kullanın.
- Bir sınıf "ICustomerDataProvider" gerektiriyorsa, bir "CustomerApiClient" oluşturun ve ihtiyaç duyduğu dizeyi "AppSettings"den sağlayın.

Kapsayıcıdan bir bağımlılık istediğimizde, buna bir bağımlılığı "çözmek" diyoruz. Bunu doğrudan kabı kullanarak yapmak kötü bir uygulamadır, ancak bu farklı bir hikaye. Gösteri amacıyla, şimdi bunu yapabiliriz:

    var customerService = container.Resolve<CustomerService>();
    var data = customerService.GetCustomerData(customerNumber);
    container.Release(customerService);

Kapsayıcı, "CustomerService"in "IAuthorizationRepository" ve "ICustomerDataProvider"a bağlı olduğunu bilir. Bu gereksinimleri karşılamak için hangi sınıfları oluşturması gerektiğini bilir. Bu sınıflar sırayla daha fazla bağımlılığa sahiptir ve kapsayıcı bunları nasıl yerine getireceğini bilir. "CustomerService" örneğini döndürene kadar ihtiyaç duyduğu her sınıfı yaratacaktır.

Bir sınıfın, "IDoesSomethingElse" gibi, bizim kaydetmediğimiz bir bağımlılık gerektirdiği bir noktaya gelirse, o zaman "CustomerService"i çözmeye çalıştığımızda, yerine getirmek için hiçbir şey kaydetmediğimizi söyleyen açık bir istisna atar. o şart.

Her DI çerçevesi biraz farklı davranır, ancak tipik olarak bize belirli sınıfların nasıl başlatıldığı konusunda bir miktar kontrol verir. Örneğin, bir 'LogWriter' örneği oluşturmasını ve bunu 'ILogWriter'a bağlı her sınıfa sağlamasını mı istiyoruz, yoksa her seferinde yeni bir tane oluşturmasını mı istiyoruz? Çoğu konteynerin bunu belirtmenin bir yolu vardır.

"IDisposable" uygulayan sınıflar ne olacak? Bu yüzden sonunda `container.Release(customerService);` diyoruz. Çoğu kap (Windsor dahil) oluşturulan tüm bağımlılıkları gözden geçirecek ve atılması gerekenleri "imha edecek". 'CustomerService', 'Isposable' ise, bunu da ortadan kaldıracaktır.

Bağımlılıkları yukarıda görüldüğü gibi kaydetmek, yazılacak daha fazla kod gibi görünebilir. Ancak, çok sayıda bağımlılığa sahip çok sayıda sınıfımız olduğunda, gerçekten işe yarar. Ve eğer aynı sınıfları *bağımlılık enjeksiyonunu kullanmadan* yazmak zorunda kalsaydık, o zaman aynı uygulamanın birçok sınıfla sürdürülmesi ve test edilmesi zor olurdu.

Bu, bağımlılık enjeksiyon kaplarını * neden * kullandığımızın yüzeyini çizer. *Uygulamamızı bir tanesini kullanmak (ve onu doğru kullanmak) için nasıl yapılandıracağımız sadece bir konu değildir - talimatlar ve örnekler bir kapsayıcıdan diğerine değiştiği için bir dizi konudur.

