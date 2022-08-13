---
title: "Injection de dépendance"
slug: "injection-de-dependance"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

**Problèmes résolus par l'injection de dépendance**

Si nous n'utilisions pas l'injection de dépendances, la classe `Greeter` pourrait ressembler davantage à ceci :

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

C'est un "maniaque du contrôle" car il contrôle la création de la classe qui fournit le message d'accueil, il contrôle l'origine de la chaîne de connexion SQL et il contrôle la sortie.

En utilisant l'injection de dépendances, la classe "Greeter" abandonne ces responsabilités en faveur d'une seule responsabilité, en écrivant une salutation qui lui est fournie.

Le [principe d'inversion de dépendance] [1] suggère que les classes doivent dépendre d'abstractions (comme les interfaces) plutôt que d'autres classes concrètes. Les dépendances directes (couplage) entre les classes peuvent rendre la maintenance progressivement difficile. Dépendre des abstractions peut réduire ce couplage.

L'injection de dépendance nous aide à réaliser cette inversion de dépendance car elle conduit à écrire des classes qui dépendent d'abstractions. La classe `Greeter` ne "connaît" rien du tout des détails d'implémentation de `IGreetingProvider` et `IGreetingWriter`. Il sait seulement que les dépendances injectées implémentent ces interfaces. Cela signifie que les modifications apportées aux classes concrètes qui implémentent `IGreetingProvider` et `IGreetingWriter` n'affecteront pas `Greeter`. Ni les remplacer par des implémentations entièrement différentes. Seules les modifications apportées aux interfaces le seront. "Greeter" est découplé.

`ControlFreakGreeter` est impossible à tester correctement. Nous voulons tester une petite unité de code, mais à la place, notre test inclurait la connexion à SQL et l'exécution d'une procédure stockée. Cela inclurait également le test de la sortie de la console. Parce que ControlFreakGreeter fait tellement de choses, il est impossible de tester isolément des autres classes.

`Greeter` est facile à tester unitaire car nous pouvons injecter des implémentations simulées de ses dépendances qui sont plus faciles à exécuter et à vérifier que d'appeler une procédure stockée ou de lire la sortie de la console. Il ne nécessite pas de chaîne de connexion dans app.config.

Les implémentations concrètes de `IGreetingProvider` et `IGreetingWriter` pourraient devenir plus complexes. Ils peuvent à leur tour avoir leurs propres dépendances qui leur sont injectées. (Par exemple, nous injecterions la chaîne de connexion SQL dans `SqlGreetingProvider`.) Mais cette complexité est "cachée" aux autres classes qui ne dépendent que des interfaces. Cela facilite la modification d'une classe sans "effet d'entraînement" qui nous oblige à apporter les modifications correspondantes aux autres classes.

[1] : https://en.wikipedia.org/wiki/Dependency_inversion_principle

## Injection de dépendance - Exemple simple
Cette classe s'appelle `Greeter`. Sa responsabilité est de produire une salutation. Il a deux *dépendances*. Il a besoin de quelque chose qui lui donnera le message d'accueil à sortir, puis il a besoin d'un moyen de sortir ce message d'accueil. Ces dépendances sont toutes deux décrites comme des interfaces, `IGreetingProvider` et `IGreetingWriter`. Dans cet exemple, ces deux dépendances sont "injectées" dans `Greeter`. (Plus d'explications en suivant l'exemple.)

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

La classe `Greeting` dépend à la fois de `IGreetingProvider` et de `IGreetingWriter`, mais elle n'est pas responsable de la création d'instances de l'un ou de l'autre. Au lieu de cela, il les requiert dans son constructeur. Tout ce qui crée une instance de `Greeting` doit fournir ces deux dépendances. Nous pouvons appeler cela "injecter" les dépendances.

Étant donné que les dépendances sont fournies à la classe dans son constructeur, cela est également appelé "injection de constructeur".

Quelques conventions courantes :

- Le constructeur enregistre les dépendances en tant que champs "privés". Dès que la classe est instanciée, ces dépendances sont disponibles pour toutes les autres méthodes non statiques de la classe.
- Les champs "privés" sont en "lecture seule". Une fois qu'ils sont définis dans le constructeur, ils ne peuvent pas être modifiés. Cela indique que ces champs ne doivent pas (et ne peuvent pas) être modifiés en dehors du constructeur. Cela garantit en outre que ces dépendances seront disponibles pendant toute la durée de vie de la classe.
- Les dépendances sont des interfaces. Ce n'est pas strictement nécessaire, mais c'est courant car cela facilite le remplacement d'une implémentation de la dépendance par une autre. Il permet également de fournir une version simulée de l'interface à des fins de test unitaire.

## Comment l'injection de dépendance facilite les tests unitaires
Ceci s'appuie sur l'exemple précédent de la classe `Greeter` qui a deux dépendances, `IGreetingProvider` et `IGreetingWriter`.

L'implémentation réelle de `IGreetingProvider` peut récupérer une chaîne à partir d'un appel d'API ou d'une base de données. L'implémentation de `IGreetingWriter` peut afficher le message d'accueil dans la console. Mais parce que `Greeter` a ses dépendances injectées dans son constructeur, il est facile d'écrire un test unitaire qui injecte des versions simulées de ces interfaces. Dans la vraie vie, nous pourrions utiliser un framework comme [Moq][1], mais dans ce cas, j'écrirai ces implémentations simulées.

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

Le comportement de `IGreetingProvider` et `IGreetingWriter` n'est pas pertinent pour ce test. Nous voulons tester que `Greeter` reçoit un message d'accueil et l'écrit. La conception de `Greeter` (utilisant l'injection de dépendances) nous permet d'injecter des dépendances simulées sans aucune pièce mobile compliquée. Tout ce que nous testons, c'est que `Greeter` interagit avec ces dépendances comme nous l'attendons.

[1] : http://www.moqthis.com/

## Pourquoi utilisons-nous des conteneurs d'injection de dépendance (conteneurs IoC)
L'injection de dépendances signifie écrire des classes de manière à ce qu'elles ne contrôlent pas leurs dépendances - à la place, leurs dépendances leur sont fournies ("injectées").

Ce n'est pas la même chose que d'utiliser un framework d'injection de dépendances (souvent appelé "conteneur DI", "conteneur IoC" ou simplement "conteneur") comme Castle Windsor, Autofac, SimpleInjector, Ninject, Unity ou autres.

Un conteneur facilite simplement l'injection de dépendances. Par exemple, supposons que vous écriviez un certain nombre de classes qui reposent sur l'injection de dépendances. Une classe dépend de plusieurs interfaces, les classes qui implémentent ces interfaces dépendent d'autres interfaces, et ainsi de suite. Certains dépendent de valeurs spécifiques. Et juste pour le plaisir, certaines de ces classes implémentent `IDisposable` et doivent être supprimées.

Chaque classe individuelle est bien écrite et facile à tester. Mais maintenant, il y a un problème différent : créer une instance d'une classe est devenu beaucoup plus compliqué. Supposons que nous créons une instance d'une classe `CustomerService`. Il a des dépendances et ses dépendances ont des dépendances. La construction d'une instance peut ressembler à ceci :

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

Vous vous demandez peut-être pourquoi ne pas mettre toute la construction géante dans une fonction séparée qui renvoie simplement `CustomerService` ? L'une des raisons est que, comme les dépendances de chaque classe y sont injectées, une classe n'est pas responsable de savoir si ces dépendances sont "IDisposables" ou de les supprimer. Il les utilise simplement. Donc, si nous avions une fonction `GetCustomerService()` qui renvoyait un `CustomerService` entièrement construit, cette classe pourrait contenir un certain nombre de ressources jetables et aucun moyen d'y accéder ou de les supprimer.

Et à part disposer de `IDisposable`, qui veut appeler une série de constructeurs imbriqués comme ça, jamais ? C'est un court exemple. Cela pourrait devenir bien pire. Encore une fois, cela ne signifie pas que nous avons écrit les classes dans le mauvais sens. Les classes pourraient être parfaites individuellement. Le défi est de les composer ensemble.

Un conteneur d'injection de dépendance simplifie cela. Cela nous permet de spécifier quelle classe ou valeur doit être utilisée pour remplir chaque dépendance. Cet exemple légèrement simplifié utilise Castle Windsor :

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

Nous appelons cela "enregistrer les dépendances" ou "configurer le conteneur". Traduit, cela indique à notre `WindsorContainer` :

- Si une classe nécessite `ILogWriter`, créez une instance de `LogWriter`. `LogWriter` nécessite un chemin de fichier. Utilisez cette valeur de `AppSettings`.
- Si une classe nécessite `IAuthorizationRepository`, créez une instance de `SqlAuthorizationRepository`. Il nécessite une chaîne de connexion. Utilisez cette valeur de la section `ConnectionStrings`.
- Si une classe nécessite `ICustomerDataProvider`, créez un `CustomerApiClient` et fournissez la chaîne dont il a besoin à partir de `AppSettings`.

Lorsque nous demandons une dépendance au conteneur, nous appelons cela « résoudre » une dépendance. C'est une mauvaise pratique de le faire directement en utilisant le conteneur, mais c'est une autre histoire. À des fins de démonstration, nous pourrions maintenant faire ceci :

    var customerService = container.Resolve<CustomerService>();
    var data = customerService.GetCustomerData(customerNumber);
    container.Release(customerService);

Le conteneur sait que `CustomerService` dépend de `IAuthorizationRepository` et `ICustomerDataProvider`. Il sait quelles classes il doit créer pour répondre à ces exigences. Ces classes, à leur tour, ont plus de dépendances, et le conteneur sait comment les remplir. Il créera toutes les classes dont il a besoin jusqu'à ce qu'il puisse renvoyer une instance de `CustomerService`.

Si cela arrive à un point où une classe nécessite une dépendance que nous n'avons pas enregistrée, comme `IDoesSomethingElse`, alors lorsque nous essaierons de résoudre `CustomerService`, une exception claire nous indiquera que nous n'avons rien enregistré à remplir cette exigence.

Chaque framework DI se comporte un peu différemment, mais généralement, ils nous donnent un certain contrôle sur la façon dont certaines classes sont instanciées. Par exemple, voulons-nous qu'il crée une instance de `LogWriter` et la fournisse à chaque classe qui dépend de `ILogWriter`, ou voulons-nous qu'il en crée une nouvelle à chaque fois ? La plupart des conteneurs ont un moyen de le spécifier.

Qu'en est-il des classes qui implémentent `IDisposable` ? C'est pourquoi nous appelons `container.Release(customerService);` à la fin. La plupart des conteneurs (y compris Windsor) passeront en revue toutes les dépendances créées et "disposeront" celles qui doivent être supprimées. Si `CustomerService` est `IDisposable`, il le supprimera également.

L'enregistrement des dépendances comme indiqué ci-dessus peut ressembler à plus de code à écrire. Mais quand nous avons beaucoup de classes avec beaucoup de dépendances, cela rapporte vraiment. Et si nous devions écrire ces mêmes classes * sans * utiliser l'injection de dépendances, cette même application avec beaucoup de classes deviendrait difficile à maintenir et à tester.

Cela effleure la surface de * pourquoi * nous utilisons des conteneurs d'injection de dépendance. *Comment* nous configurons notre application pour en utiliser un (et l'utiliser correctement) n'est pas seulement un sujet - c'est un certain nombre de sujets, car les instructions et les exemples varient d'un conteneur à l'autre.

