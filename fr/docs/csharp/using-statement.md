---
title: "Utilisation de l'instruction"
slug: "utilisation-de-linstruction"
draft: false
images: []
weight: 9015
type: docs
toc: true
---

Fournit une syntaxe pratique qui garantit l'utilisation correcte des objets [IDisposable](https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netframework-4.7).



## Syntaxe
- en utilisant (jetable) { }
- en utilisant (IDisposable jetable = new MyDisposable()) { }


L'objet dans l'instruction `using` doit implémenter l'interface `IDisposable`.

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

Des exemples plus complets pour l'implémentation de `IDisposable` peuvent être trouvés dans [MSDN docs][1].


[1] : https://msdn.microsoft.com/en-us/library/fs2xkftw(v=vs.110).aspx
  

## Gotcha : retour de la ressource dont vous disposez
Ce qui suit est une mauvaise idée car cela éliminerait la variable `db` avant de la renvoyer.

    public IDBContext GetDBContext()
    {
        using (var db = new DBContext())
        {
            return db;
        }
    }

Cela peut également créer des erreurs plus subtiles :

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age);
        }
    }

Cela semble correct, mais le problème est que l'évaluation de l'expression LINQ est paresseuse et ne sera peut-être exécutée que plus tard, lorsque le `DBContext` sous-jacent aura déjà été supprimé.

Donc, en bref, l'expression n'est pas évaluée avant de quitter le `using`. Une solution possible à ce problème, qui utilise toujours "using", est de provoquer l'évaluation immédiate de l'expression en appelant une méthode qui énumèrera le résultat. Par exemple `ToList()`, `ToArray()`, etc. Si vous utilisez la dernière version d'Entity Framework, vous pouvez utiliser les homologues `async` comme `ToListAsync()` ou `ToArrayAsync()`.

Vous trouverez ci-dessous l'exemple en action :

    public IEnumerable<Person> GetPeople(int age)
    {
        using (var db = new DBContext())
        {
            return db.Persons.Where(p => p.Age == age).ToList();
        }
    }

Il est important de noter, cependant, qu'en appelant `ToList()` ou `ToArray()`, l'expression sera évaluée avec empressement, ce qui signifie que toutes les personnes ayant l'âge spécifié seront chargées en mémoire même si vous n'itérez pas sur eux.

## Utiliser les principes de base des instructions
`using` est un sucre syntaxique qui vous permet de garantir qu'une ressource est nettoyée sans avoir besoin d'un bloc explicite `try-finally`. Cela signifie que votre code sera beaucoup plus propre et que vous ne perdrez pas de ressources non gérées.

Modèle de nettoyage standard `Dispose`, pour les objets qui implémentent l'interface `IDisposable` (ce que la classe de base `FileStream` `Stream` fait dans .NET):

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

`using` simplifie votre syntaxe en masquant le `try-finally` explicite :

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            return disposable.ReadByte();
        }
        // disposable.Dispose is called even if we return earlier
    }

Tout comme les blocs `finally` s'exécutent toujours indépendamment des erreurs ou des retours, `using` appelle toujours `Dispose()`, même en cas d'erreur :

    int Foo()
    {
        var fileName = "file.txt";

        using (var disposable = File.Open(fileName, FileMode.Open))
        {
            throw new InvalidOperationException();
        }
        // disposable.Dispose is called even if we throw an exception earlier
    }
**Noter:**
Étant donné que `Dispose` est garanti d'être appelé quel que soit le flux de code, c'est une bonne idée de s'assurer que `Dispose` ne lève jamais d'exception lorsque vous implémentez `IDisposable`. Sinon, une exception réelle serait remplacée par la nouvelle exception, ce qui entraînerait un cauchemar de débogage.

Revenant de l'utilisation du bloc
---

    using ( var disposable = new DisposableItem() )
    {
        return disposable.SomeProperty;
    }

En raison de la sémantique de `try..finally` vers laquelle se traduit le bloc `using`, l'instruction `return` fonctionne comme prévu - la valeur de retour est évaluée avant que le bloc `finally` ne soit exécuté et la valeur supprimée. L'ordre d'évaluation est le suivant :

1. Évaluer le corps `try`
2. Évaluer et mettre en cache la valeur renvoyée
3. Exécutez enfin le bloc
4. Renvoyez la valeur de retour en cache

Cependant, vous ne pouvez pas renvoyer la variable `disposable` elle-même, car elle contiendrait une référence invalide supprimée - voir [exemple associé][1].


[1] : https://www.wikiod.com/fr/docs/c%23/38/using-statement/327/gotcha-returning-the-resource-which-you-are-disposing#t=201608220847304515557

## Plusieurs instructions using avec un seul bloc
Il est possible d'utiliser plusieurs instructions `using` imbriquées sans ajouter plusieurs niveaux d'accolades imbriquées. Par exemple:

    using (var input = File.OpenRead("input.txt"))
    {
        using (var output = File.OpenWrite("output.txt"))
        {
            input.CopyTo(output);
        } // output is disposed here
    } // input is disposed here

Une alternative consiste à écrire :

    using (var input = File.OpenRead("input.txt"))
    using (var output = File.OpenWrite("output.txt"))
    {
        input.CopyTo(output);
    } // output and then input are disposed here

Ce qui est exactement équivalent au premier exemple.

*Remarque :* Les instructions "using" imbriquées peuvent déclencher la règle d'analyse de code Microsoft [CS2002][1] (voir [cette réponse][2] pour plus de précisions) et générer un avertissement. Comme expliqué dans la réponse liée, il est généralement sûr d'imbriquer les instructions "using".

Lorsque les types dans l'instruction `using` sont du même type, vous pouvez les délimiter par des virgules et spécifier le type une seule fois (bien que ce soit rare):

    using (FileStream file = File.Open("MyFile.txt"), file2 = File.Open("MyFile2.txt"))
    {
    }

Cela peut également être utilisé lorsque les types ont une hiérarchie partagée :

    using (Stream file = File.Open("MyFile.txt"), data = new MemoryStream())
    {
    }

Le mot-clé `var` *ne peut pas* être utilisé dans l'exemple ci-dessus. Une erreur de compilation se produirait. Même la déclaration séparée par des virgules ne fonctionnera pas lorsque les variables déclarées ont des types de hiérarchies différentes.

[1] : https://msdn.microsoft.com/en-us/library/ms182334.aspx
[2] : http://stackoverflow.com/a/22323027/501011

## Gotcha : Exception dans la méthode Dispose masquant d'autres erreurs dans l'utilisation de blocs
Considérez le bloc de code suivant.

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

Vous pouvez vous attendre à voir "Impossible d'effectuer l'opération" imprimé sur la console, mais vous verriez en fait "Impossible d'éliminer avec succès". car la méthode Dispose est toujours appelée même après la levée de la première exception.

Il vaut la peine d'être conscient de cette subtilité car elle peut masquer la véritable erreur qui a empêché la suppression de l'objet et le rend plus difficile à déboguer.



## Les instructions d'utilisation sont null-safe
Vous n'avez pas à vérifier l'objet `IDisposable` pour `null`. `using` ne lèvera pas d'exception et `Dispose()` ne sera pas appelé :

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

## Utilisation de Dispose Syntax pour définir une portée personnalisée
Pour certains cas d'utilisation, vous pouvez utiliser la syntaxe "using" pour aider à définir une portée personnalisée. Par exemple, vous pouvez définir la classe suivante pour exécuter du code dans une culture spécifique.

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

Vous pouvez ensuite utiliser cette classe pour définir des blocs de code qui s'exécutent dans une culture spécifique.

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

Remarque : comme nous n'utilisons pas l'instance `CultureContext` que nous créons, nous ne lui attribuons pas de variable.

Cette technique est utilisée par le ``BeginForm`` [helper][1] dans ASP.NET MVC.

[1] : https://msdn.microsoft.com/en-us/library/dd410596%28v=vs.100%29.aspx

## Utilisation des instructions et des connexions à la base de données
Le mot clé `using` garantit que la ressource définie dans l'instruction n'existe que dans la portée de l'instruction elle-même. Toutes les ressources définies dans l'instruction doivent implémenter l'interface "IDisposable".

Celles-ci sont extrêmement importantes lorsqu'il s'agit de toute connexion qui implémente l'interface "IDisposable", car elle peut garantir que les connexions sont non seulement correctement fermées, mais que leurs ressources sont libérées après que l'instruction "using" est hors de portée.


**Classes de données communes "IDisposables"**
---

La plupart des classes suivantes sont des classes liées aux données qui implémentent l'interface `IDisposable` et sont des candidats parfaits pour une instruction `using` :

- `SqlConnection`,`SqlCommand`,`SqlDataReader`, etc.
- `OleDbConnection`, `OleDbCommand`, `OleDbDataReader`, etc.
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader`, etc.
- `DbContext`

Tous ces éléments sont couramment utilisés pour accéder aux données via C # et seront couramment rencontrés lors de la création d'applications centrées sur les données. De nombreuses autres classes non mentionnées qui implémentent les mêmes classes `FooConnection`,`FooCommand`,`FooDataReader` peuvent s'attendre à se comporter de la même manière.

** Modèle d'accès commun pour les connexions ADO.NET **
----

Un modèle commun pouvant être utilisé lors de l'accès à vos données via une connexion ADO.NET peut ressembler à ceci :

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

Ou si vous ne faisiez qu'une simple mise à jour et que vous n'aviez pas besoin d'un lecteur, le même concept de base s'appliquerait :

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

**Utilisation d'instructions avec DataContexts**
---

De nombreux ORM tels que Entity Framework exposent des classes d'abstraction utilisées pour interagir avec les bases de données sous-jacentes sous la forme de classes telles que `DbContext`. Ces contextes implémentent généralement également l'interface `IDisposable` et devraient en tirer parti via des instructions `using` lorsque cela est possible :

    using(var context = new YourDbContext())
    {
          // Access your context and perform your query
          var data = context.Widgets.ToList();
    }




## Exécution de code dans un contexte de contrainte
Si vous avez du code (une *routine*) que vous souhaitez exécuter dans un contexte (contrainte) spécifique, vous pouvez utiliser l'injection de dépendances.

L'exemple suivant montre la contrainte d'exécution sous une connexion SSL ouverte. Cette première partie se trouverait dans votre bibliothèque ou votre framework, que vous n'exposerez pas au code client.

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

Maintenant, le code client qui veut faire quelque chose sous SSL mais ne veut pas gérer tous les détails SSL. Vous pouvez maintenant faire ce que vous voulez à l'intérieur du tunnel SSL, par exemple échanger une clé symétrique :

    public void ExchangeSymmetricKey(BinaryReader sslReader, BinaryWriter sslWriter)
    {
        byte[] bytes = new byte[8];
        (new RNGCryptoServiceProvider()).GetNonZeroBytes(bytes);
        sslWriter.Write(BitConverter.ToUInt64(bytes, 0));
    }

Vous exécutez cette routine comme suit :

    SSLContext.ClientTunnel(tcpClient, this.ExchangeSymmetricKey);

Pour ce faire, vous avez besoin de la clause `using()` car c'est le seul moyen (à part un bloc `try..finally`) de garantir que le code client (`ExchangeSymmetricKey`) ne se termine jamais sans disposer correctement du jetable Ressources. Sans la clause `using()`, vous ne sauriez jamais si une routine pourrait briser la contrainte du contexte pour disposer de ces ressources.


