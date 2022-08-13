---
title: "Inyección de dependencia"
slug: "inyeccion-de-dependencia"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

La definición de Wikipedia de inyección de dependencia es:

En ingeniería de software, la inyección de dependencia es un patrón de diseño de software que implementa la inversión de control para resolver dependencias. Una dependencia es un objeto que se puede utilizar (un servicio). Una inyección es el paso de una dependencia a un objeto dependiente (un cliente) que la usaría.

 
****Este sitio presenta una respuesta a la pregunta Cómo explicar la dependencia
Inyección a un niño de 5 años. La respuesta mejor calificada, siempre
por John Munsch proporciona una sorprendente precisión
analogía dirigida al inquisidor (imaginario) de cinco años:
Cuando vas y sacas cosas del refrigerador para ti,
puedes causar problemas. Podrías dejar la puerta abierta, tú
podría conseguir algo que mamá o papá no quieren que hagas
tener. Incluso podría estar buscando algo que ni siquiera
tiene o ha caducado.
Lo que deberías estar haciendo es declarar una necesidad, “Necesito algo
para beber con el almuerzo”, y luego nos aseguraremos de que tenga
algo cuando te sientas a comer.
Qué significa esto en términos de desarrollo de software orientado a objetos
es esto: clases colaboradoras (los niños de cinco años)
debe confiar en la infraestructura (los padres) para proporcionar**




** Este código usa MEF para cargar dinámicamente el dll y resolver el
dependencias La dependencia de ILogger es resuelta por MEF e inyectada en
la clase de usuario. La clase de usuario nunca recibe una implementación concreta de
ILogger y no tiene idea de qué o qué tipo de registrador está usando.**

## Inyección de Dependencia C# y ASP.NET con Unity
Primero, ¿por qué deberíamos usar la inyección de dependencia en nuestro código? Queremos desacoplar otros componentes de otras clases en nuestro programa. Por ejemplo, tenemos la clase AnimalController que tiene un código como este:

    public class AnimalController()
    {
        private SantaAndHisReindeer _SantaAndHisReindeer = new SantaAndHisReindeer();

        public AnimalController(){
                Console.WriteLine("");
        }
    }
Observamos este código y creemos que todo está bien, pero ahora nuestro AnimalController depende del objeto _SantaAndHisReindeer. Automáticamente, mi controlador es malo para las pruebas y la reutilización de mi código será muy difícil.

Muy buena explicación de por qué deberíamos usar Inyección de dependencia e interfaces.
[aquí][1].


[1]: http://stackoverflow.com/questions/14301389/why-does-one-use-dependency-injection


Si queremos que Unity maneje DI, el camino para lograrlo es muy simple :)
Con NuGet (administrador de paquetes) podemos importar fácilmente la unidad a nuestro código.

> en Visual Studio Tools -> Administrador de paquetes NuGet -> Administrar paquetes para la solución -> en la entrada de búsqueda escriba unidad -> elija nuestro proyecto-> haga clic en instalar

Ahora se crearán dos archivos con buenos comentarios.

    
> en la carpeta App-Data UnityConfig.cs y UnityMvcActivator.cs

> UnityConfig: en el método RegisterTypes, podemos ver el tipo que se inyectará en nuestros constructores.

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
> UnityMvcActivator -> también con buenos comentarios que dicen que esta clase integra Unity con ASP.NET MVC

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

Ahora podemos desacoplar nuestro controlador de la clase SantAndHisReindeer :)

     public class AnimalController()
        {
            private readonly SantaAndHisReindeer _SantaAndHisReindeer;
    
            public AnimalController(SantaAndHisReindeer SantaAndHisReindeer){
    
                    _SantAndHisReindeer = SantaAndHisReindeer;
            }
        }

> Hay una última cosa que debemos hacer antes de ejecutar nuestra aplicación.

En Global.asax.cs debemos agregar una nueva línea: UnityWebActivator.Start() que iniciará, configurará Unity y registrará nuestros tipos.



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



## Inyección de dependencia usando MEF
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


