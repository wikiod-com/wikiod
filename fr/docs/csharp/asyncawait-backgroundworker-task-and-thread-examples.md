---
title: "Asyncwait, Backgroundworker, exemples de tâches et de threads"
slug: "asyncwait-backgroundworker-exemples-de-taches-et-de-threads"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

Pour exécuter l'un de ces exemples, appelez-les simplement comme ceci :

    static void Main()
    {
        new Program().ProcessDataAsync();
        Console.ReadLine();
    }

## Configuration ASP.NET en attente
Lorsque ASP.NET gère une requête, un thread est attribué à partir du pool de threads et un **contexte de requête** est créé. Le contexte de la requête contient des informations sur la requête en cours accessibles via la propriété statique ```HttpContext.Current```. Le contexte de la demande pour la demande est ensuite attribué au thread traitant la demande.

Un contexte de requête donné ** ne peut être actif que sur un thread à la fois **.

Lorsque l'exécution atteint ```wait```, le thread traitant une requête est renvoyé au pool de threads pendant que la méthode asynchrone s'exécute et que le contexte de la requête est libre pour qu'un autre thread puisse l'utiliser.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        var products = await dbContext.Products.ToListAsync();

        // Execution resumes on a "random" thread from the pool
        // Execution continues using the original request context.
        return View(products);
    }

Lorsque la tâche est terminée, le pool de threads affecte un autre thread pour continuer l'exécution de la demande. Le contexte de la requête est alors affecté à ce thread. Cela peut ou non être le fil d'origine.

### Blocage ###

Lorsque le résultat d'un appel de méthode ```async``` est attendu de manière **synchrone**, des interblocages peuvent survenir. Par exemple, le code suivant entraînera un blocage lorsque ```IndexSync()``` est appelé :

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        List<Product> products = await dbContext.Products.ToListAsync();
    
        // Execution resumes on a "random" thread from the pool
        return View(products);
    }

    public ActionResult IndexSync()
    {
        Task<ActionResult> task = Index();

        // Block waiting for the result synchronously
        ActionResult result = Task.Result;

        return result;       
    }

En effet, par défaut la tâche attendue, dans ce cas ```db.Products.ToListAsync()``` va capturer le contexte (dans le cas d'ASP.NET le contexte de la requête) et essayer de l'utiliser une fois qu'il a complété.

Lorsque toute la pile d'appels est asynchrone, il n'y a pas de problème car, une fois que ```wait``` est atteint, le thread d'origine est libéré, libérant le contexte de la requête.

Lorsque nous bloquons de manière synchrone en utilisant ```Task.Result``` ou ```Task.Wait()``` (ou d'autres méthodes de blocage), le thread d'origine est toujours actif et conserve le contexte de la requête. La méthode attendue fonctionne toujours de manière asynchrone et une fois que le rappel essaie de s'exécuter, c'est-à-dire une fois que la tâche attendue est retournée, elle tente d'obtenir le contexte de la requête.

Par conséquent, le blocage survient parce que pendant que le thread bloquant avec le contexte de requête attend que l'opération asynchrone se termine, l'opération asynchrone essaie d'obtenir le contexte de requête afin de se terminer.

### ConfigureAwait ###

Par défaut, les appels à une tâche attendue captureront le contexte actuel et tenteront de reprendre l'exécution sur le contexte une fois terminé.

En utilisant ```ConfigureAwait(false)``` cela peut être évité et les interblocages peuvent être évités.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        List<Product> products = await dbContext.Products.ToListAsync().ConfigureAwait(false);
    
        // Execution resumes on a "random" thread from the pool without the original request context
        return View(products);
    }
    
    public ActionResult IndexSync()
    {
        Task<ActionResult> task = Index();
    
        // Block waiting for the result synchronously
        ActionResult result = Task.Result;
    
        return result;       
    }

Cela peut éviter les interblocages lorsqu'il faut bloquer sur du code asynchrone, mais cela se fait au prix de la perte du contexte dans la continuation (code après l'appel à wait).

Dans ASP.NET, cela signifie que si votre code suite à un appel à ```wait someTask.ConfigureAwait(false);``` tente d'accéder aux informations du contexte, par exemple ```HttpContext.Current.User``` alors les informations ont été perdues. Dans ce cas, le ```HttpContext.Current``` est nul. Par exemple:

    public async Task<ActionResult> Index()
    {
        // Contains information about the user sending the request
        var user = System.Web.HttpContext.Current.User;

        using (var client = new HttpClient())
        {
            await client.GetAsync("http://google.com").ConfigureAwait(false);
        }

        // Null Reference Exception, Current is null
        var user2 = System.Web.HttpContext.Current.User;

        return View();
    }

Si ```ConfigureAwait(true)``` est utilisé (ce qui équivaut à ne pas avoir de ConfigureAwait du tout), alors ```user``` et ```user2``` sont renseignés avec les mêmes données.

Pour cette raison, il est souvent recommandé d'utiliser ```ConfigureAwait(false)``` dans le code de la bibliothèque où le contexte n'est plus utilisé.

## Asynchrone/attente
Voir ci-dessous pour un exemple simple d'utilisation de async/wait pour effectuer des tâches chronophages dans un processus en arrière-plan tout en conservant la possibilité de faire d'autres tâches qui n'ont pas besoin d'attendre que les tâches chronophages soient terminées.

Cependant, si vous avez besoin de travailler ultérieurement avec le résultat de la méthode chronophage, vous pouvez le faire en attendant l'exécution.

    public async Task ProcessDataAsync()
    {
        // Start the time intensive method
        Task<int> task = TimeintensiveMethod(@"PATH_TO_SOME_FILE");

        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");

        // Wait for TimeintensiveMethod to complete and get its result
        int x = await task;
        Console.WriteLine("Count: " + x);
    }

    private async Task<int> TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = await reader.ReadToEndAsync();

            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something as a "result"
        return new Random().Next(100);
    }

## Travailleur d'arrière-plan
Voir ci-dessous un exemple simple d'utilisation d'un objet "BackgroundWorker" pour effectuer des opérations chronophages dans un thread d'arrière-plan.

Vous devez:
1. Définissez une méthode de travail qui effectue le travail chronophage et appelez-la à partir d'un gestionnaire d'événements pour l'événement `DoWork` d'un `BackgroundWorker`.
3. Démarrez l'exécution avec `RunWorkerAsync`. Tout argument requis par la méthode de travail attachée à `DoWork` peut être transmis via le paramètre `DoWorkEventArgs` à `RunWorkerAsync`.

En plus de l'événement `DoWork`, la classe `BackgroundWorker` définit également deux événements qui doivent être utilisés pour interagir avec l'interface utilisateur. Ceux-ci sont facultatifs.

* L'événement `RunWorkerCompleted` est déclenché lorsque les gestionnaires `DoWork` sont terminés.
* L'événement `ProgressChanged` est déclenché lorsque la méthode `ReportProgress` est appelée.


    public void ProcessDataAsync()
    {
        // Start the time intensive method
        BackgroundWorker bw = new BackgroundWorker();
        bw.DoWork += BwDoWork;
        bw.RunWorkerCompleted += BwRunWorkerCompleted;
        bw.RunWorkerAsync(@"PATH_TO_SOME_FILE");

        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    }

    // Method that will be called after BwDoWork exits
    private void BwRunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
    {
        // we can access possible return values of our Method via the Parameter e
        Console.WriteLine("Count: " + e.Result);
    }

    // execution of our time intensive Method
    private void BwDoWork(object sender, DoWorkEventArgs e)
    {
        e.Result = TimeintensiveMethod(e.Argument);
    }

    private int TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = reader.ReadToEnd();

           for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something as a "result"
        return new Random().Next(100);
    }


## Tâche
Voir ci-dessous un exemple simple d'utilisation d'une "tâche" pour effectuer des tâches chronophages dans un processus en arrière-plan.

Tout ce que vous avez à faire est d'envelopper votre méthode chronophage dans un appel `Task.Run()`.

    

    public void ProcessDataAsync()
    {
        // Start the time intensive method
        Task<int> t = Task.Run(() => TimeintensiveMethod(@"PATH_TO_SOME_FILE"));
    
        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    
        Console.WriteLine("Count: " + t.Result);
    }
    
    private int TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");
    
        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = reader.ReadToEnd();
    
            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");
    
        // return something as a "result"
        return new Random().Next(100);
    }

## Fil de discussion
Voir ci-dessous un exemple simple d'utilisation d'un `Thread` pour effectuer des tâches chronophages dans un processus en arrière-plan.

    

    public async void ProcessDataAsync()
    {
        // Start the time intensive method
        Thread t = new Thread(TimeintensiveMethod);
    
        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    }
    
    private void TimeintensiveMethod()
    {
        Console.WriteLine("Start TimeintensiveMethod.");
    
        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(@"PATH_TO_SOME_FILE"))
        {
            string v = reader.ReadToEnd();
    
            for (int i = 0; i < 10000; i++)
                v.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");
    }

Comme vous pouvez le voir, nous ne pouvons pas renvoyer une valeur de notre `TimeIntensiveMethod` car `Thread` attend une méthode void comme paramètre.

Pour obtenir une valeur de retour à partir d'un `Thread`, utilisez soit un événement, soit ce qui suit :

    int ret;
    Thread t= new Thread(() => 
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file))
        {
            string s = reader.ReadToEnd();

            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something to demonstrate the coolness of await-async
        ret = new Random().Next(100);
    });

    t.Start();
    t.Join(1000);
    Console.Writeline("Count: " + ret);

## Extension de tâche "exécuter et oublier"
Dans certains cas (par exemple, la journalisation), il peut être utile d'exécuter la tâche et de ne pas attendre le résultat. L'extension suivante permet d'exécuter la tâche et de poursuivre l'exécution du reste du code :

    public static class TaskExtensions
    {
        public static async void RunAndForget(
            this Task task, Action<Exception> onException = null)
        {
            try
            {
                await task;
            }
            catch (Exception ex)
            {
                onException?.Invoke(ex);
            }
        }
    }

Le résultat n'est attendu qu'à l'intérieur de la méthode d'extension. Puisque `async`/`wait` est utilisé, il est possible d'attraper une exception et d'appeler une méthode optionnelle pour la gérer.

Un exemple d'utilisation de l'extension :

    var task = Task.FromResult(0); // Or any other task from e.g. external lib.
    task.RunAndForget(
        e =>
        {
            // Something went wrong, handle it.
        });

