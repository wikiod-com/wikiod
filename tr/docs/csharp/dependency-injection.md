---
title: "Bağımlılık Enjeksiyonu"
slug: "bagmllk-enjeksiyonu"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Bağımlılık enjeksiyonunun Wikipedia tanımı:

Yazılım mühendisliğinde, bağımlılık enjeksiyonu, bağımlılıkları çözmek için kontrolün tersine çevrilmesini uygulayan bir yazılım tasarım modelidir. Bağımlılık, kullanılabilecek bir nesnedir (hizmet). Bir enjeksiyon, onu kullanacak bir bağımlı nesneye (bir istemci) bir bağımlılığın iletilmesidir.

 
****Bu site, Bağımlılık nasıl açıklanır sorusunun cevabını içerir
5 yaşındaki çocuğa enjeksiyon. Sağlanan en yüksek puanlı cevap
John Munsch tarafından şaşırtıcı derecede doğru
(hayali) beş yaşındaki sorgucunu hedef alan benzetme:
Gidip kendiniz için buzdolabından bir şeyler aldığınızda,
sorunlara neden olabilirsiniz. Kapıyı açık bırakabilirsin, sen
Annen veya babanın istemediği bir şeyi alabilirsin
Sahip olmak. Hatta bizim bile yapmadığımız bir şeyi arıyor olabilirsiniz.
sahip veya süresi dolmuş olan.
Yapmanız gereken şey, bir ihtiyacı belirtmektir, “Bir şeye ihtiyacım var.
öğle yemeği ile içmek için” ve sonra sahip olduğunuzdan emin olacağız.
yemek için oturduğunuzda bir şey.
Bunun nesne yönelimli yazılım geliştirme açısından anlamı nedir?
şudur: işbirlikçi sınıflar (beş yaşındakiler)
sağlamak için altyapıya (ebeveynler) güvenmelidir**




** Bu kod, dll'yi dinamik olarak yüklemek ve sorunu çözmek için MEF kullanır.
bağımlılıklar. ILogger bağımlılığı MEF tarafından çözülür ve
kullanıcı sınıfı. Kullanıcı sınıfı hiçbir zaman Beton uygulamasını almaz
ILogger ve ne veya hangi tür kaydedici kullandığı hakkında hiçbir fikri yok.**

## Unity ile Dependency Injection C# ve ASP.NET
Öncelikle neden kodumuzda bağımlılık enjeksiyonunu kullanmalıyız? Programımızdaki diğer bileşenleri diğer sınıflardan ayırmak istiyoruz. Örneğin, aşağıdaki gibi bir koda sahip AnimalController sınıfımız var:

    public class AnimalController()
    {
        private SantaAndHisReindeer _SantaAndHisReindeer = new SantaAndHisReindeer();

        public AnimalController(){
                Console.WriteLine("");
        }
    }
Bu koda bakıyoruz ve her şeyin yolunda olduğunu düşünüyoruz ama şimdi AnimalController'ımız _SantaAndHisReindeer nesnesine güveniyor. Otomatik olarak Denetleyicim test için kötü ve kodumun yeniden kullanılabilirliği çok zor olacak.

Depedency Injection ve arayüzlerini neden kullanmamız gerektiğine dair çok iyi bir açıklama
[burada][1].


[1]: http://stackoverflow.com/questions/14301389/why-does-one-use-dependency-injection


Unity'nin DI ile ilgilenmesini istiyorsak, bunu başarmanın yolu çok basit :)
NuGet(paket yöneticisi) ile birliği kolayca kodumuza aktarabiliriz.

> Visual Studio Araçları'nda -> NuGet Paket Yöneticisi -> Çözüm için Paketleri Yönet -> arama girişinde birlik yazın -> projemizi seçin-> yükle'yi tıklayın

Şimdi güzel yorumlara sahip iki dosya oluşturulacak.

    
> App-Data klasöründe UnityConfig.cs ve UnityMvcActivator.cs

> UnityConfig - RegisterTypes yönteminde, yapıcılarımızda enjeksiyon yapılacak türü görebiliriz.

    namespace Vegan.WebUi.App_Start
    {
    
    public class UnityConfig
    {
        #region Unity Container
        private static Lazy<IUnityContainer> container = new Lazy<IUnityContainer>(() =>
        {
            var container = new UnityContainer();
            RegisterTypes(container);
            return container;
        });

        /// <summary>
        /// Gets the configured Unity container.
        /// </summary>
        public static IUnityContainer GetConfiguredContainer()
        {
            return container.Value;
        }
        #endregion

        /// <summary>Registers the type mappings with the Unity container.</summary>
        /// <param name="container">The unity container to configure.</param>
        /// <remarks>There is no need to register concrete types such as controllers or API controllers (unless you want to 
        /// change the defaults), as Unity allows resolving a concrete type even if it was not previously registered.</remarks>
        public static void RegisterTypes(IUnityContainer container)
        {
            // NOTE: To load from web.config uncomment the line below. Make sure to add a Microsoft.Practices.Unity.Configuration to the using statements.
            // container.LoadConfiguration();

            // TODO: Register your types here
            // container.RegisterType<IProductRepository, ProductRepository>();

            container.RegisterType<ISanta, SantaAndHisReindeer>();
            
         }
     }
    }
> UnityMvcActivator - > ayrıca bu sınıfın Unity'yi ASP.NET MVC ile entegre ettiğini söyleyen güzel yorumlarla

    using System.Linq;
    using System.Web.Mvc;
    using Microsoft.Practices.Unity.Mvc;

    [assembly: WebActivatorEx.PreApplicationStartMethod(typeof(Vegan.WebUi.App_Start.UnityWebActivator), "Start")]
    [assembly: WebActivatorEx.ApplicationShutdownMethod(typeof(Vegan.WebUi.App_Start.UnityWebActivator), "Shutdown")]
        
    namespace Vegan.WebUi.App_Start
    {
    /// <summary>Provides the bootstrapping for integrating Unity with ASP.NET MVC.</summary>
    public static class UnityWebActivator
    {
        /// <summary>Integrates Unity when the application starts.</summary>
        public static void Start() 
        {
            var container = UnityConfig.GetConfiguredContainer();

            FilterProviders.Providers.Remove(FilterProviders.Providers.OfType<FilterAttributeFilterProvider>().First());
            FilterProviders.Providers.Add(new UnityFilterAttributeFilterProvider(container));

            DependencyResolver.SetResolver(new UnityDependencyResolver(container));

            // TODO: Uncomment if you want to use PerRequestLifetimeManager
            // Microsoft.Web.Infrastructure.DynamicModuleHelper.DynamicModuleUtility.RegisterModule(typeof(UnityPerRequestHttpModule));
        }

        /// <summary>Disposes the Unity container when the application is shut down.</summary>
        public static void Shutdown()
        {
            var container = UnityConfig.GetConfiguredContainer();
            container.Dispose();
        }
    }
    }

Artık Kontrolörümüzü SantAndHisReindeer sınıfından ayırabiliriz :)

     public class AnimalController()
        {
            private readonly SantaAndHisReindeer _SantaAndHisReindeer;
    
            public AnimalController(SantaAndHisReindeer SantaAndHisReindeer){
    
                    _SantAndHisReindeer = SantaAndHisReindeer;
            }
        }

> Uygulamamızı çalıştırmadan önce yapmamız gereken son bir şey var.

Global.asax.cs'de yeni satır eklemeliyiz: Unity'yi başlatacak, yapılandıracak ve türlerimizi kaydedecek UnityWebActivator.Start().



    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Web;
    using System.Web.Mvc;
    using System.Web.Optimization;
    using System.Web.Routing;
    using Vegan.WebUi.App_Start;

    namespace Vegan.WebUi
    {
        public class MvcApplication : System.Web.HttpApplication
        {
            protected void Application_Start()
            {
                AreaRegistration.RegisterAllAreas();
                FilterConfig.RegisterGlobalFilters(GlobalFilters.Filters);
                RouteConfig.RegisterRoutes(RouteTable.Routes);
                BundleConfig.RegisterBundles(BundleTable.Bundles);
                UnityWebActivator.Start();
            }
        }
    }



## MEF kullanarak bağımlılık ekleme
    public interface ILogger
    {
        void Log(string message);
    }

    [Export(typeof(ILogger))]
    [ExportMetadata("Name", "Console")]  
    public class ConsoleLogger:ILogger
    {
        public void Log(string message)
        {
            Console.WriteLine(message);
        }
    }

    [Export(typeof(ILogger))]
    [ExportMetadata("Name", "File")]  
    public class FileLogger:ILogger
    {
        public void Log(string message)
        {
            //Write the message to file
        }
    }

    public class User
    {  
        private readonly ILogger logger;
        public User(ILogger logger)   
        {
            this.logger = logger;
        }
        public void LogUser(string message)
        {
            logger.Log(message)  ;
        }
    }

    public interface ILoggerMetaData
    {
        string Name { get; }
    }
    
    internal class Program
    {
        private CompositionContainer _container;
        
        [ImportMany]
        private IEnumerable<Lazy<ILogger, ILoggerMetaData>> _loggers;
        
        private static void Main()
        {            
            ComposeLoggers();
            Lazy<ILogger, ILoggerMetaData> loggerNameAndLoggerMapping = _ loggers.First((n) => ((n.Metadata.Name.ToUpper() =="Console"));
            ILogger logger= loggerNameAndLoggerMapping.Value
            var user = new User(logger);
            user.LogUser("user name");
        }
        
        private void ComposeLoggers()
        {
            //An aggregate catalog that combines multiple catalogs
            var catalog = new AggregateCatalog();
            string loggersDllDirectory =Path.Combine(Utilities.GetApplicationDirectory(), "Loggers");
            if (!Directory.Exists(loggersDllDirectory ))
            {
                Directory.CreateDirectory(loggersDllDirectory );
            }
            //Adds all the parts found in the same assembly as the PluginManager class
            catalog.Catalogs.Add(new AssemblyCatalog(typeof(Program).Assembly));
            catalog.Catalogs.Add(new DirectoryCatalog(loggersDllDirectory ));
            
            //Create the CompositionContainer with the parts in the catalog
            _container = new CompositionContainer(catalog);
            
            //Fill the imports of this object
            try
            {
                this._container.ComposeParts(this);
            }
            catch (CompositionException compositionException)
            {
                throw new CompositionException(compositionException.Message);
            }
        } 
    }


