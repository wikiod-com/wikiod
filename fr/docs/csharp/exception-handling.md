---
title: "Gestion des exceptions"
slug: "gestion-des-exceptions"
draft: false
images: []
weight: 9143
type: docs
toc: true
---

## Création d'exceptions personnalisées
Vous êtes autorisé à implémenter des exceptions personnalisées qui peuvent être levées comme n'importe quelle autre exception. Cela a du sens lorsque vous souhaitez distinguer vos exceptions des autres erreurs lors de l'exécution.

Dans cet exemple, nous allons créer une exception personnalisée pour une gestion claire des problèmes que l'application peut rencontrer lors de l'analyse d'une entrée complexe.

# Création d'une classe d'exception personnalisée

Pour créer une exception personnalisée, créez une sous-classe de "Exception" :

    public class ParserException : Exception
    {
        public ParserException() : 
          base("The parsing went wrong and we have no additional information.") { }
    }

L'exception personnalisée devient très utile lorsque vous souhaitez fournir des informations supplémentaires au receveur :

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

Maintenant, lorsque vous `catch(ParserException x)` vous aurez une sémantique supplémentaire pour affiner la gestion des exceptions.

Les classes personnalisées peuvent implémenter les fonctionnalités suivantes pour prendre en charge des scénarios supplémentaires.

# relancer

Pendant le processus d'analyse, l'exception d'origine est toujours intéressante. Dans cet exemple, il s'agit d'une `FormatException` car le code tente d'analyser un morceau de chaîne, qui devrait être un nombre. Dans ce cas, l'exception personnalisée doit prendre en charge l'inclusion de '**InnerException**' :

    //new constructor:
    ParserException(string msg, Exception inner) : base(msg, inner) {
    }

# sérialisation

Dans certains cas, vos exceptions peuvent devoir franchir les limites de l'AppDomain. C'est le cas si votre analyseur s'exécute dans son propre AppDomain pour prendre en charge le rechargement à chaud des nouvelles configurations d'analyseur. Dans Visual Studio, vous pouvez utiliser le modèle "Exception" pour générer du code comme celui-ci.

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

# Utilisation de l'exception Parser
   
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

Vous pouvez également utiliser des exceptions personnalisées pour intercepter et encapsuler des exceptions. De cette façon, de nombreuses erreurs différentes peuvent être converties en un seul type d'erreur plus utile à l'application :

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

Lorsque vous gérez des exceptions en levant vos propres exceptions personnalisées, vous devez généralement inclure une référence à l'exception d'origine dans la propriété "InnerException", comme indiqué ci-dessus.

# Problèmes de sécurité

Si exposer la raison de l'exception peut compromettre la sécurité en permettant aux utilisateurs de voir le fonctionnement interne de votre application, cela peut être une mauvaise idée d'envelopper l'exception interne. Cela peut s'appliquer si vous créez une bibliothèque de classes qui sera utilisée par d'autres.

Voici comment déclencher une exception personnalisée sans envelopper l'exception interne :

    try
    {
      // ...
    }
    catch (SomeStandardException ex)
    {
      // ...
      throw new MyCustomException(someMessage);
    }

# Conclusion

Lorsque vous déclenchez une exception personnalisée (avec encapsulage ou avec une nouvelle exception non encapsulée), vous devez générer une exception significative pour l'appelant. Par exemple, un utilisateur d'une bibliothèque de classes peut ne pas savoir grand-chose sur la façon dont cette bibliothèque effectue son travail interne. Les exceptions levées par les dépendances de la bibliothèque de classes ne sont pas significatives. Au lieu de cela, l'utilisateur souhaite une exception pertinente pour la manière dont la bibliothèque de classes utilise ces dépendances de manière erronée.

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

## Enfin bloquer
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
    
Le bloc `try/catch/finally` peut être très pratique lors de la lecture de fichiers.
Par exemple:

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


Un bloc try doit être suivi soit d'un bloc `catch` soit d'un bloc `finally`. Cependant, comme il n'y a pas de bloc catch, l'exécution entraînera l'arrêt. Avant la fin, les instructions à l'intérieur du bloc finally seront exécutées.

Dans la lecture de fichier, nous aurions pu utiliser un bloc `using` car `FileStream` (ce que `OpenRead` renvoie) implémente `IDisposable`.

Même s'il y a une instruction `return` dans le bloc `try`, le bloc `finally` s'exécutera généralement ; il y a quelques cas où ce ne sera pas le cas :

- Lorsqu'un [StackOverflow se produit][1].
- [`Environment.FailFast`](https://msdn.microsoft.com/en-us/library/system.environment.failfast.aspx)
- Le processus de candidature est tué, généralement par une source externe.


[1] : https://msdn.microsoft.com/en-us/library/system.stackoverflowexception(v=vs.110).aspx

## Les meilleures pratiques
## Aide-mémoire

| FAIRE | NE PAS FAIRE |
| ------ | ------ |
| Flux de contrôle avec des instructions de contrôle | Flux de contrôle avec exceptions|
| Gardez une trace de l'exception ignorée (absorbée) en enregistrant|Ignorer l'exception|
| Répéter l'exception en utilisant `throw`|Relancer l'exception - `throw new ArgumentNullException()` ou `throw ex` |
| Lancer des exceptions système prédéfinies | Lancer des exceptions personnalisées similaires aux exceptions système prédéfinies |
| Lancer une exception personnalisée/prédéfinie si elle est cruciale pour la logique de l'application | Levez des exceptions personnalisées/prédéfinies pour indiquer un avertissement dans le flux |
| Attrapez les exceptions que vous souhaitez gérer | Attrapez chaque exception |


## NE PAS gérer la logique métier avec des exceptions. ##

Le contrôle de flux ne doit PAS être effectué par des exceptions. Utilisez plutôt des instructions conditionnelles. Si un contrôle peut être effectué clairement avec l'instruction `if-else`, n'utilisez pas d'exceptions car cela réduit la lisibilité et les performances.

Considérez l'extrait suivant de Mr. Bad Practices :

    // This is a snippet example for DO NOT
    object myObject;
    void DoingSomethingWithMyObject()
    {
        Console.WriteLine(myObject.ToString());
    }

Lorsque l'exécution atteint `Console.WriteLine(myObject.ToString());` l'application lèvera une NullReferenceException. M. Bad Practices s'est rendu compte que `myObject` est nul et a modifié son extrait pour intercepter et gérer `NullReferenceException` :

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

Étant donné que l'extrait de code précédent ne couvre que la logique d'exception, que dois-je faire si `myObject` n'est pas nul à ce stade ? Où dois-je couvrir cette partie de la logique ? Juste après `Console.WriteLine(myObject.ToString());` ? Qu'en est-il après le bloc `try...catch` ?

Que diriez-vous de M. Best Practices? Comment gérerait-il cela ?

    // This is a snippet example for DO
    object myObject;
    void DoingSomethingWithMyObject()
    {
        if(myObject == null)
            myObject = new object();
        
        // When execution reaches this point, we are sure that myObject is not null
        DoSomethingElseWithMyObject();
    }

Mr. Best Practices a atteint la même logique avec moins de code et une logique claire et compréhensible.

## NE PAS relancer les exceptions ##

Relancer des exceptions coûte cher. Cela a un impact négatif sur les performances. Pour le code qui échoue régulièrement, vous pouvez utiliser des modèles de conception pour minimiser les problèmes de performances. [Cette rubrique][1] décrit deux modèles de conception qui sont utiles lorsque des exceptions peuvent avoir un impact significatif sur les performances.

## NE PAS absorber les exceptions sans journalisation ##

    try
    {
        //Some code that might throw an exception
    }
    catch(Exception ex)
    {
        //empty catch block, bad practice
    }

N'avalez jamais les exceptions. Ignorer les exceptions sauvera ce moment mais créera un chaos pour la maintenabilité plus tard. Lors de la journalisation des exceptions, vous devez toujours journaliser l'instance d'exception afin que la trace complète de la pile soit journalisée et pas uniquement le message d'exception.

    try
    {
        //Some code that might throw an exception
    }
    catch(NullException ex)
    {
        LogManager.Log(ex.ToString());
    }

## N'interceptez pas les exceptions que vous ne pouvez pas gérer ##

De nombreuses ressources, telles que [celle-ci][2], vous incitent fortement à réfléchir à la raison pour laquelle vous attrapez une exception à l'endroit où vous l'attrapez. Vous ne devez intercepter une exception que si vous pouvez la gérer à cet emplacement. Si vous pouvez faire quelque chose pour aider à atténuer le problème, comme essayer un algorithme alternatif, vous connecter à une base de données de sauvegarde, essayer un autre nom de fichier, attendre 30 secondes et réessayer, ou avertir un administrateur, vous pouvez détecter l'erreur et le faire. S'il n'y a rien que vous puissiez faire de manière plausible et raisonnable, "laissez tomber" et laissez l'exception être gérée à un niveau supérieur. Si l'exception est suffisamment catastrophique et qu'il n'y a pas d'autre option raisonnable que de faire planter l'ensemble du programme en raison de la gravité du problème, laissez-le planter.

    try
    {
        //Try to save the data to the main database.
    }
    catch(SqlException ex)
    {
        //Try to save the data to the alternative database.
    }
    //If anything other than a SqlException is thrown, there is nothing we can do here. Let the exception bubble up to a level where it can be handled.

[1] : https://msdn.microsoft.com/en-us/library/ms229009(v=vs.100).aspx
[2] : http://c2.com/cgi/wiki?DontCatchExceptions

## Anti-modèles d'exception
# Exceptions d'ingestion

Il faut toujours relancer l'exception de la manière suivante :

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw;
    }


Le fait de relancer une exception comme ci-dessous masquera l'exception d'origine et perdra la trace de pile d'origine. On ne devrait jamais faire ça ! La trace de la pile avant la capture et le relancement sera perdue.

    try
    {
        ...
    }
    catch (Exception ex)
    {
        ...
        throw ex;
    }

# Gestion des exceptions au baseball

Il ne faut pas utiliser les exceptions comme [substitut aux constructions de contrôle de flux normales][1] comme les instructions if-then et les boucles while. Cet anti-pattern est parfois appelé [Baseball Exception Handling][2].

Voici un exemple d'anti-pattern :

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

Voici une meilleure façon de procéder :

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

# capture (Exception)

Il n'y a presque aucune raison (certains disent aucune !) d'attraper le type d'exception générique dans votre code. Vous ne devez intercepter que les types d'exceptions que vous attendez, sinon vous masquez les bogues dans votre code.

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

Mieux vaut faire :

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

Si une autre exception se produit, nous laissons délibérément l'application planter, elle entre directement dans le débogueur et nous pouvons résoudre le problème. Nous ne devons pas envoyer un programme où d'autres exceptions que celles-ci se produisent de toute façon, donc ce n'est pas un problème d'avoir un plantage.

Ce qui suit est également un mauvais exemple, car il utilise des exceptions pour contourner une erreur de programmation. Ce n'est pas pour ça qu'ils sont conçus.

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

[1] : http://c2.com/cgi/wiki?DontUseExceptionsForFlowControl
[2] : http://www.stackprinter.com/questions/new-programming-jargon-you-coined.html

## Gestion des exceptions de base
    try
    {
        /* code that could throw an exception */
    }
    catch (Exception ex)
    {
        /* handle the exception */
    }
Notez que la gestion de toutes les exceptions avec le même code n'est souvent pas la meilleure approche.
Ceci est couramment utilisé lorsque des routines internes de gestion des exceptions échouent, en dernier recours.

## Gestion des types d'exceptions spécifiques
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

Veillez à ce que les exceptions soient évaluées dans l'ordre et que l'héritage soit appliqué. Il faut donc commencer par les plus spécifiques et terminer par leur ancêtre.
À un moment donné, un seul bloc catch sera exécuté.

## Agréger les exceptions / plusieurs exceptions d'une méthode
Qui a dit que vous ne pouvez pas lever plusieurs exceptions dans une seule méthode. Si vous n'êtes pas habitué à jouer avec AggregateExceptions, vous pourriez être tenté de créer votre propre structure de données pour représenter de nombreux problèmes. Il y a bien sûr une autre structure de données qui ne fait pas exception serait plus idéale comme les résultats d'une validation. Même si vous jouez avec AggregateExceptions, vous êtes peut-être du côté des destinataires et vous les manipulez toujours sans vous rendre compte qu'ils peuvent vous être utiles.

Il est tout à fait plausible qu'une méthode s'exécute et même si ce sera un échec dans son ensemble, vous souhaiterez mettre en évidence plusieurs choses qui se sont mal passées dans les exceptions levées. À titre d'exemple, ce comportement peut être vu avec le fonctionnement des méthodes parallèles si une tâche était divisée en plusieurs threads et n'importe quel nombre d'entre eux pouvait lever des exceptions et cela doit être signalé. Voici un exemple idiot de la façon dont vous pourriez en bénéficier :

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

## Lancer une exception
Votre code peut, et devrait souvent, lever une exception lorsque quelque chose d'inhabituel s'est produit.

    public void WalkInto(Destination destination)
    {
        if (destination.Name == "Mordor")
        {
            throw new InvalidOperationException("One does not simply walk into Mordor.");
        }
        // ... Implement your normal walking code here.
    }

## Exception non gérée et thread
**AppDomain.UnhandledException**
Cet événement fournit une notification des exceptions non interceptées. Il permet à l'application de consigner des informations sur l'exception avant que le gestionnaire par défaut du système ne signale l'exception à l'utilisateur et ne termine l'application. Si des informations suffisantes sur l'état de l'application sont disponibles, d'autres actions peuvent être entreprises — telles que la sauvegarde des données du programme pour une récupération ultérieure. La prudence est recommandée, car les données du programme peuvent être corrompues lorsque les exceptions ne sont pas gérées.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);            
        }
**Application.ThreadException**
Cet événement permet à votre application Windows Forms de gérer des exceptions autrement non gérées qui se produisent dans les threads Windows Forms. Attachez vos gestionnaires d'événements à l'événement ThreadException pour gérer ces exceptions, ce qui laissera votre application dans un état inconnu. Dans la mesure du possible, les exceptions doivent être gérées par un bloc de gestion des exceptions structuré.

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        private static void Main(string[] args)
        {
            AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);
            Application.ThreadException += new ThreadExceptionEventHandler(ThreadException);
        }
Et enfin la gestion des exceptions

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

## Utilisation de l'objet exception
Vous êtes autorisé à créer et lever des exceptions dans votre propre code.
L'instanciation d'une exception se fait de la même manière que tout autre objet C#.

    Exception ex = new Exception();

    // constructor with an overload that takes a message string
    Exception ex = new Exception("Error message"); 

Vous pouvez ensuite utiliser le mot clé `throw` pour déclencher l'exception :

    
    try
    {
        throw new Exception("Error");
    }
    catch (Exception ex)
    {
        Console.Write(ex.Message); // Logs 'Error' to the output window
    } 


**Remarque :** Si vous lancez une nouvelle exception dans un bloc catch, assurez-vous que l'exception d'origine est transmise en tant qu'"exception interne", par ex.

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

Dans ce cas, on suppose que l'exception ne peut pas être gérée, mais certaines informations utiles sont ajoutées au message (et l'exception d'origine est toujours accessible via `ex.InnerException` par un bloc d'exception externe).

Il affichera quelque chose comme :

> System.DivideByZeroException : Impossible de diviser par b car il est égal à zéro ---> System.DivideByZeroException : Tentative de division par zéro. <br/>
> à UserQuery.<Main>g__DoSomething0_0() dans C:\[...]\LINQPadQuery.cs:line 36 <br/>
> --- Fin de la trace de la pile d'exceptions internes --- <br/>
> à UserQuery.<Main>g__DoSomething0_0() dans C:\[...]\LINQPadQuery.cs:line 42 <br/>
> à UserQuery.Main() dans C:\[...]\LINQPadQuery.cs:line 55 <br/>

Si vous essayez cet exemple dans LinqPad, vous remarquerez que les numéros de ligne ne sont pas très significatifs (ils ne vous aident pas toujours). Mais transmettre un texte d'erreur utile comme suggéré ci-dessus réduit souvent considérablement le temps nécessaire pour rechercher l'emplacement de l'erreur, qui est dans cet exemple clairement la ligne

> c = un / b ;

dans la fonction `DoSomething()`.

**[Essayez-le dans .NET Fiddle](https://dotnetfiddle.net/Widget/JLUXXY)**


## Implémentation de IErrorHandler pour les services WCF
L'implémentation de IErrorHandler pour les services WCF est un excellent moyen de centraliser la gestion et la journalisation des erreurs. L'implémentation illustrée ici doit intercepter toute exception non gérée qui est levée à la suite d'un appel à l'un de vos services WCF. Cet exemple montre également comment renvoyer un objet personnalisé et comment renvoyer JSON plutôt que le XML par défaut.

Implémentez IErrorHandler :
 
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

Dans cet exemple, nous attachons le gestionnaire au comportement de service. Vous pouvez également l'attacher à IEndpointBehavior, IContractBehavior ou IOperationBehavior de la même manière.
    
Attacher aux comportements de service :

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

Configurations dans Web.config :

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

Voici quelques liens qui peuvent être utiles sur ce sujet :

https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx

http://www.brainthud.com/cards/5218/25441/which-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they- mettre en œuvre-et

Autres exemples :

http://stackoverflow.com/questions/38231970/ierrorhandler-returning-wrong-message-body-when-http-status-code-is-401-unauthor

http://stackoverflow.com/questions/3036692/ierrorhandler-doesnt-seem-to-be-handling-my-errors-in-wcf-any-ideas

http://stackoverflow.com/questions/1149037/how-to-make-custom-wcf-error-handler-return-json-response-with-non-ok-http-code

http://stackoverflow.com/questions/10679214/how-do-you-set-the-content-type-header-for-an-httpclient-request?rq=1

## Imbrication d'exceptions et essayez de bloquer les blocs.
On peut imbriquer un bloc exception / `try` `catch` dans l'autre.

De cette façon, on peut gérer de petits blocs de code capables de fonctionner sans perturber l'ensemble de votre mécanisme.

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

**Remarque :** Évitez [Swallowing Exceptions][1] lors du lancement vers le bloc catch parent


[1] : https://www.wikiod.com/fr/docs/c%23/40/exception-handling/6940/exception-anti-patterns#t=201707281310293021372

