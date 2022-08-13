---
title: "Bibliothèque parallèle de tâches (TPL)"
slug: "bibliotheque-parallele-de-taches-tpl"
draft: false
images: []
weight: 9746
type: docs
toc: true
---

Objectif et cas d'utilisation
=======
L'objectif de la bibliothèque parallèle de tâches est de simplifier le processus d'écriture et de maintenance du code multithread et parallèle.

Quelques cas d'utilisation* :

- Garder une interface utilisateur réactive en exécutant un travail en arrière-plan sur une tâche distincte
- Répartition de la charge de travail
- Permettre à une application cliente d'envoyer et de recevoir des requêtes en même temps (rest, TCP/UDP, ect)
- Lire et/ou écrire plusieurs fichiers à la fois

*Le code doit être considéré au cas par cas pour le multithreading. Par exemple, si une boucle n'a que quelques itérations ou n'effectue qu'une petite partie du travail, la surcharge du parallélisme peut l'emporter sur les avantages.

**TPL avec .Net 3.5**

Le TPL est également disponible pour .Net 3.5 inclus dans un package NuGet, il s'appelle Task Parallel Library.

## Boucle de base producteur-consommateur (BlockingCollection)
    var collection = new BlockingCollection<int>(5);
    var random = new Random();
    
    var producerTask = Task.Run(() => {
        for(int item=1; item<=10; item++) 
        {
            collection.Add(item);
            Console.WriteLine("Produced: " + item);
            Thread.Sleep(random.Next(10,1000));
        }
        collection.CompleteAdding();
        Console.WriteLine("Producer completed!");
    });

Il convient de noter que si vous n'appelez pas `collection.CompleteAdding();`, vous pouvez continuer à ajouter à la collection même si votre tâche consommateur est en cours d'exécution. Appelez simplement `collection.CompleteAdding();` lorsque vous êtes sûr qu'il n'y a plus d'ajouts. Cette fonctionnalité peut être utilisée pour créer un modèle de producteur multiple vers un consommateur unique dans lequel plusieurs sources alimentent des éléments dans BlockingCollection<T> et un seul consommateur extrait des éléments et fait quelque chose avec eux. Si votre BlockingCollection<T> est vide avant que vous n'appeliez l'ajout complet, l'Enumerable de `collection.GetConsumingEnumerable()` se bloquera jusqu'à ce qu'un nouvel élément soit ajouté à la collection ou BlockingCollection<T>.CompleteAdding(); est appelée et la file d'attente est vide.
    
    var consumerTask = Task.Run(() => {
        foreach(var item in collection.GetConsumingEnumerable())
        {
            Console.WriteLine("Consumed: " + item);
            Thread.Sleep(random.Next(10,1000));
        }
        Console.WriteLine("Consumer completed!");
    });
      
    Task.WaitAll(producerTask, consumerTask);
           
    Console.WriteLine("Everything completed!");

## Parallel.Invoke
    var actions = Enumerable.Range(1, 10).Select(n => new Action(() =>
    {
        Console.WriteLine("I'm task " + n);
        if((n & 1) == 0)
            throw new Exception("Exception from task " + n);
    })).ToArray();

    try
    {
        Parallel.Invoke(actions);
    }
    catch(AggregateException ex)
    {
        foreach(var inner in ex.InnerExceptions)
            Console.WriteLine("Task failed: " + inner.Message);
    }

## Tâche : instanciation de base et attente
Une tâche peut être créée en instanciant directement la classe `Task`...

    var task = new Task(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });

    Console.WriteLine("Starting task...");
    task.Start();
    task.Wait();
    Console.WriteLine("Task completed!");

...ou en utilisant la méthode statique `Task.Run` :

    Console.WriteLine("Starting task...");
    var task = Task.Run(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });
    task.Wait();
    Console.WriteLine("Task completed!");

Notez que ce n'est que dans le premier cas qu'il est nécessaire d'appeler explicitement `Start`.

## Tâche.QuandTous
    var random = new Random();
    IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    }));

    Task<int[]> task = Task.WhenAll(tasks);
    int[] results = await task;

    Console.WriteLine(string.Join(",", results.Select(n => n.ToString())));
    // Output: 1,2,3,4,5

## Parallèle.ForEach
Cet exemple utilise `Parallel.ForEach` pour calculer la somme des nombres entre 1 et 10 000 en utilisant plusieurs threads. Pour assurer la sécurité des threads, `Interlocked.Add` est utilisé pour additionner les nombres.

    using System.Threading;

    int Foo()
    {
        int total = 0;
        var numbers = Enumerable.Range(1, 10000).ToList();
        Parallel.ForEach(numbers, 
            () => 0, // initial value,
            (num, state, localSum) => num + localSum,
            localSum => Interlocked.Add(ref total, localSum));
        return total; // total = 50005000
    }


## Parallèle.Pour
Cet exemple utilise `Parallel.For` pour calculer la somme des nombres entre 1 et 10000 en utilisant plusieurs threads. Pour assurer la sécurité des threads, `Interlocked.Add` est utilisé pour additionner les nombres.

    using System.Threading;

    int Foo()
    {
        int total = 0;
        Parallel.For(1, 10001, 
            () => 0, // initial value,
            (num, state, localSum) => num + localSum,
            localSum => Interlocked.Add(ref total, localSum));
        return total; // total = 50005000
    }

## Tâche : renvoyer une valeur
La tâche qui renvoie une valeur a un type de retour de <code>Task< TResult ></code> où TResult est le type de valeur qui doit être renvoyé. Vous pouvez interroger le résultat d'une tâche par sa propriété Result.

    Task<int> t = Task.Run(() => 
        {
            int sum = 0;

            for(int i = 0; i < 500; i++)
                sum += i;

            return sum;
        });

    Console.WriteLine(t.Result); // Outuput 124750

Si la tâche s'exécute de manière asynchrone, attendre que la tâche renvoie son résultat.

    public async Task DoSomeWork()
    {
        WebClient client = new WebClient();
        // Because the task is awaited, result of the task is assigned to response
        string response = await client.DownloadStringTaskAsync("http://somedomain.com");
    }

    

## Tâche : WaitAll et capture de variables
    var tasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    })).ToArray();

    foreach(var task in tasks) task.Start();
    Task.WaitAll(tasks);

    foreach(var task in tasks)
        Console.WriteLine(task.Result);

## Tâche : WaitAny
    var allTasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() => n)).ToArray();
    var pendingTasks = allTasks.ToArray();

    foreach(var task in allTasks) task.Start();

    while(pendingTasks.Length > 0)
    {
        var finishedTask = pendingTasks[Task.WaitAny(pendingTasks)];
        Console.WriteLine("Task {0} finished", finishedTask.Result);
        pendingTasks = pendingTasks.Except(new[] {finishedTask}).ToArray();
    }

    Task.WaitAll(allTasks);

**Remarque :** Le `WaitAll` final est nécessaire car `WaitAny` ne provoque pas l'observation d'exceptions.

## Tâche : gestion des exceptions (à l'aide de Wait)
    var task1 = Task.Run(() =>
    {
        Console.WriteLine("Task 1 code starting...");
        throw new Exception("Oh no, exception from task 1!!");
    });

    var task2 = Task.Run(() =>
    {
        Console.WriteLine("Task 2 code starting...");
        throw new Exception("Oh no, exception from task 2!!");
    });

    Console.WriteLine("Starting tasks...");
    try
    {
        Task.WaitAll(task1, task2);
    }
    catch(AggregateException ex)
    {
        Console.WriteLine("Task(s) failed!");
        foreach(var inner in ex.InnerExceptions)
            Console.WriteLine(inner.Message);
    }

    Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
    Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

## Tâche : gérer les exceptions (sans utiliser Wait)
    var task1 = Task.Run(() =>
    {
        Console.WriteLine("Task 1 code starting...");
        throw new Exception("Oh no, exception from task 1!!");
    });

    var task2 = Task.Run(() =>
    {
        Console.WriteLine("Task 2 code starting...");
        throw new Exception("Oh no, exception from task 2!!");
    });

    var tasks = new[] {task1, task2};

    Console.WriteLine("Starting tasks...");
    while(tasks.All(task => !task.IsCompleted));

    foreach(var task in tasks)
    {
        if(task.IsFaulted)
            Console.WriteLine("Task failed: " +
                task.Exception.InnerExceptions.First().Message);
    }

    Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
    Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

## Tâche : annulation à l'aide de CancellationToken
    var cancellationTokenSource = new CancellationTokenSource();
    var cancellationToken = cancellationTokenSource.Token;

    var task = new Task((state) =>
        {
            int i = 1;
            var myCancellationToken = (CancellationToken)state;
            while(true)
            {
                Console.Write("{0} ", i++);
                Thread.Sleep(1000);
                myCancellationToken.ThrowIfCancellationRequested();
            }
        },
        cancellationToken: cancellationToken,
        state: cancellationToken);

    Console.WriteLine("Counting to infinity. Press any key to cancel!");
    task.Start();
    Console.ReadKey();

    cancellationTokenSource.Cancel();
    try
    {
        task.Wait();
    }
    catch(AggregateException ex)
    {
        ex.Handle(inner => inner is OperationCanceledException);
    }

    Console.WriteLine($"{Environment.NewLine}You have cancelled! Task status is: {task.Status}");
    //Canceled

Comme alternative à `ThrowIfCancellationRequested`, la demande d'annulation peut être détectée avec `IsCancellationRequested` et une `OperationCanceledException` peut être levée manuellement :

    //New task delegate
    int i = 1;
    var myCancellationToken = (CancellationToken)state;
    while(!myCancellationToken.IsCancellationRequested)
    {
        Console.Write("{0} ", i++);
        Thread.Sleep(1000);
    }
    Console.WriteLine($"{Environment.NewLine}Ouch, I have been cancelled!!");
    throw new OperationCanceledException(myCancellationToken);

Notez comment le jeton d'annulation est passé au constructeur de tâche dans le paramètre `cancellationToken`. Ceci est nécessaire pour que la tâche passe à l'état "Annulé", et non à l'état "Défaillant", lorsque "ThrowIfCancellationRequested" est invoqué. De plus, pour la même raison, le jeton d'annulation est explicitement fourni dans le constructeur de `OperationCanceledException` dans le second cas.

## Tâche.QuandAny
    var random = new Random();
    IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(async() =>
    {
        Console.WriteLine("I'm task " + n);
        await Task.Delay(random.Next(10,1000));
        return n;
    }));

    Task<Task<int>> whenAnyTask = Task.WhenAny(tasks);
    Task<int> completedTask = await whenAnyTask;
    Console.WriteLine("The winner is: task " + await completedTask);

    await Task.WhenAll(tasks);
    Console.WriteLine("All tasks finished!");

## Contexte d'exécution fluide avec AsyncLocal
Lorsque vous devez transmettre des données de la tâche parent à ses tâches enfants, afin qu'elles s'enchaînent logiquement avec l'exécution, utilisez `AsyncLocal` [class][1] :

    void Main()
    {
        AsyncLocal<string> user = new AsyncLocal<string>();
        user.Value = "initial user";
        
        // this does not affect other tasks - values are local relative to the branches of execution flow
        Task.Run(() => user.Value = "user from another task"); 
        
        var task1 = Task.Run(() =>
        {
            Console.WriteLine(user.Value); // outputs "initial user"
            Task.Run(() =>
            {
                // outputs "initial user" - value has flown from main method to this task without being changed
                Console.WriteLine(user.Value);
            }).Wait();

            user.Value = "user from task1";
 
            Task.Run(() =>
            {
                // outputs "user from task1" - value has flown from main method to task1
                // than value was changed and flown to this task.
                Console.WriteLine(user.Value);
            }).Wait();
        });
        
        task1.Wait();
        
        // ouputs "initial user" - changes do not propagate back upstream the execution flow    
        Console.WriteLine(user.Value); 
    }

**Remarque :** Comme le montre l'exemple ci-dessus, `AsynLocal.Value` a une sémantique de `copie en lecture`, mais si vous transmettez un type de référence et modifiez ses propriétés, vous affecterez d'autres tâches. Par conséquent, la meilleure pratique avec `AsyncLocal` consiste à utiliser des types de valeur ou des types immuables.

[1] : https://msdn.microsoft.com/en-us/library/dn906268(v=vs.110).aspx

## Parallel.ForEach dans VB.NET
    For Each row As DataRow In FooDataTable.Rows
        Me.RowsToProcess.Add(row)
    Next
    
    Dim myOptions As ParallelOptions = New ParallelOptions()
    myOptions.MaxDegreeOfParallelism = environment.processorcount
    
    Parallel.ForEach(RowsToProcess, myOptions, Sub(currentRow, state)
                                                   ProcessRowParallel(currentRow, state)
                                               End Sub)

