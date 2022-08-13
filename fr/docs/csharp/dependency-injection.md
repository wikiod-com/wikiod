---
title: "Injection de dépendance"
slug: "injection-de-dependance"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

La définition Wikipédia de l'injection de dépendance est la suivante :

En génie logiciel, l'injection de dépendances est un modèle de conception logicielle qui implémente l'inversion de contrôle pour résoudre les dépendances. Une dépendance est un objet utilisable (un service). Une injection est le passage d'une dépendance à un objet dépendant (un client) qui l'utiliserait.

 
****Ce site propose une réponse à la question Comment expliquer la dépendance
Injection à un enfant de 5 ans. La réponse la mieux notée, fournie
par John Munsch fournit une précision surprenante
analogie visant l'inquisiteur (imaginaire) de cinq ans :
Quand tu vas sortir toi-même des choses du réfrigérateur,
vous pouvez causer des problèmes. Vous pourriez laisser la porte ouverte, vous
pourrait obtenir quelque chose que maman ou papa ne veut pas que vous obteniez
ont. Vous cherchez peut-être même quelque chose que nous ne connaissons même pas
ont ou qui ont expiré.
Ce que vous devriez faire, c'est exprimer un besoin, "J'ai besoin de quelque chose
à boire avec le déjeuner », puis nous nous assurerons que vous avez
quelque chose quand vous vous asseyez pour manger.
Ce que cela signifie en termes de développement logiciel orienté objet
c'est ça : les classes collaboratrices (les cinq ans)
devrait compter sur l'infrastructure (les parents) pour fournir**




** Ce code utilise MEF pour charger dynamiquement la dll et résoudre le
dépendances. La dépendance ILogger est résolue par MEF et injectée dans
la classe d'utilisateurs. La classe d'utilisateurs ne reçoit jamais la mise en œuvre concrète de
ILogger et il n'a aucune idée de quel ou quel type d'enregistreur il utilise.**

## Injection de dépendances C# et ASP.NET avec Unity
Tout d'abord, pourquoi devrions-nous utiliser l'injection de dépendance dans notre code ? Nous voulons dissocier les autres composants des autres classes de notre programme. Par exemple, nous avons la classe AnimalController qui a un code comme celui-ci :

    public class AnimalController()
    {
        private SantaAndHisReindeer _SantaAndHisReindeer = new SantaAndHisReindeer();

        public AnimalController(){
                Console.WriteLine("");
        }
    }
Nous regardons ce code et nous pensons que tout va bien mais maintenant notre AnimalController dépend de l'objet _SantaAndHisReindeer. Automatiquement, mon contrôleur est mauvais pour les tests et la réutilisation de mon code sera très difficile.

Très bonne explication pourquoi nous devrions utiliser l'injection de dépendance et les interfaces
[ici][1].


[1] : http://stackoverflow.com/questions/14301389/why-does-one-use-dependency-injection


Si nous voulons que Unity gère DI, la route pour y parvenir est très simple :)
Avec NuGet (gestionnaire de packages), nous pouvons facilement importer l'unité dans notre code.

> dans Visual Studio Tools -> NuGet Package Manager -> Gérer les packages pour la solution -> dans l'unité d'écriture d'entrée de recherche -> choisissez notre projet -> cliquez sur installer

Maintenant, deux fichiers avec de beaux commentaires seront créés.

    
> dans le dossier App-Data UnityConfig.cs et UnityMvcActivator.cs

> UnityConfig - dans la méthode RegisterTypes, nous pouvons voir le type qui sera injecté dans nos constructeurs.

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
> UnityMvcActivator -> également avec de beaux commentaires qui disent que cette classe intègre Unity avec ASP.NET MVC

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

Maintenant, nous pouvons dissocier notre contrôleur de la classe SantAndHisReindeer :)

     public class AnimalController()
        {
            private readonly SantaAndHisReindeer _SantaAndHisReindeer;
    
            public AnimalController(SantaAndHisReindeer SantaAndHisReindeer){
    
                    _SantAndHisReindeer = SantaAndHisReindeer;
            }
        }

> Il y a une dernière chose que nous devons faire avant d'exécuter notre application.

Dans Global.asax.cs nous devons ajouter une nouvelle ligne : UnityWebActivator.Start() qui va démarrer, configurer Unity et enregistrer nos types.



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



## Injection de dépendance à l'aide de MEF
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


