---
title: "Injeção de dependência"
slug: "injecao-de-dependencia"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

A definição da Wikipedia de injeção de dependência é:

Na engenharia de software, injeção de dependência é um padrão de projeto de software que implementa a inversão de controle para resolver dependências. Uma dependência é um objeto que pode ser usado (um serviço). Uma injeção é a passagem de uma dependência para um objeto dependente (um cliente) que a usaria.

 
****Este site apresenta uma resposta para a pergunta Como explicar a dependência
Injeção em uma criança de 5 anos. A resposta mais bem avaliada, fornecida
por John Munsch fornece um surpreendentemente preciso
analogia direcionada ao (imaginário) inquisidor de cinco anos:
Quando você vai tirar as coisas da geladeira para si mesmo,
você pode causar problemas. Você pode deixar a porta aberta, você
pode conseguir algo que mamãe ou papai não quer que você
tenho. Você pode até estar procurando por algo que nem
tem ou expirou.
O que você deve fazer é declarar uma necessidade: “Preciso de algo
para beber com o almoço”, e então nos certificaremos de que você tenha
algo quando você se senta para comer.
O que isso significa em termos de desenvolvimento de software orientado a objetos
é isso: turmas colaboradoras (as de cinco anos)
deve contar com a infraestrutura (os pais) para fornecer**




** Este código usa MEF para carregar dinamicamente a dll e resolver o
dependências. A dependência do ILogger é resolvida pelo MEF e injetada no
a classe de usuário. A classe de usuário nunca recebe Implementação concreta de
ILogger e não tem ideia de que tipo de logger está usando.**

## Injeção de Dependência C# e ASP.NET com Unity
Primeiro porque devemos usar injeção de dependência em nosso código? Queremos desacoplar outros componentes de outras classes em nosso programa. Por exemplo, temos a classe AnimalController que tem código como este:

    public class AnimalController()
    {
        private SantaAndHisReindeer _SantaAndHisReindeer = new SantaAndHisReindeer();

        public AnimalController(){
                Console.WriteLine("");
        }
    }
Nós olhamos para este código e achamos que está tudo bem, mas agora nosso AnimalController depende do objeto _SantaAndHisReindeer. Automaticamente meu Controller é ruim para testar e a reutilização do meu código será muito difícil.

Muito boa explicação por que devemos usar Injeção de Dependência e interfaces
[aqui][1].


[1]: http://stackoverflow.com/questions/14301389/why-does-one-use-dependency-injection


Se queremos que o Unity lide com DI, o caminho para conseguir isso é muito simples :)
Com NuGet( gerenciador de pacotes), podemos importar facilmente a unidade para nosso código.

> em Ferramentas do Visual Studio -> Gerenciador de Pacotes NuGet -> Gerenciar Pacotes para Solução -> na unidade de gravação de entrada de pesquisa -> escolha nosso projeto-> clique em instalar

Agora serão criados dois arquivos com comentários agradáveis.

    
> na pasta App-Data UnityConfig.cs e UnityMvcActivator.cs

> UnityConfig - no método RegisterTypes, podemos ver o tipo que será injeção em nossos construtores.

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
> UnityMvcActivator - > também com bons comentários que dizem que esta classe integra Unity com ASP.NET MVC

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

Agora podemos desacoplar nosso Controller da classe SantAndHisReindeer :)

     public class AnimalController()
        {
            private readonly SantaAndHisReindeer _SantaAndHisReindeer;
    
            public AnimalController(SantaAndHisReindeer SantaAndHisReindeer){
    
                    _SantAndHisReindeer = SantaAndHisReindeer;
            }
        }

> Há uma última coisa que devemos fazer antes de executar nosso aplicativo.

Em Global.asax.cs devemos adicionar uma nova linha: UnityWebActivator.Start() que irá iniciar, configurar o Unity e registrar nossos tipos.



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



## Injeção de dependência usando MEF
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


