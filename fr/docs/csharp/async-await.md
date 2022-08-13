---
title: "Attente asynchrone"
slug: "attente-asynchrone"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

En C #, une méthode déclarée `async` ne se bloquera pas dans un processus synchrone, si vous utilisez des opérations basées sur les E/S (par exemple, accès Web, travail avec des fichiers, ...). Le résultat de ces méthodes marquées asynchrones peut être attendu via l'utilisation du mot-clé "wait".

Une méthode `async` peut renvoyer `void`, `Task` ou `Task<T>`.

Le type de retour `Task` attendra que la méthode se termine et le résultat sera `void`. `Task<T>` renverra une valeur de type `T` une fois la méthode terminée.

Les méthodes `async` doivent renvoyer `Task` ou `Task<T>`, par opposition à `void`, dans presque toutes les circonstances. Les méthodes "async void" ne peuvent pas être "attendues", ce qui entraîne divers problèmes. Le seul scénario où un `async` devrait renvoyer `void` est dans le cas d'un gestionnaire d'événements.

`async`/`wait` fonctionne en transformant votre méthode `async` en une machine d'état. Pour ce faire, il crée une structure dans les coulisses qui stocke l'état actuel et tout contexte (comme les variables locales), et expose une méthode `MoveNext()` pour faire avancer les états (et exécuter tout code associé) chaque fois qu'un attendu attendu se termine.

## Opérateur d'attente et mot-clé asynchrone
L'opérateur `wait` et le mot-clé `async` se rejoignent :

> La méthode asynchrone dans laquelle **wait** est utilisé doit être modifiée par
> le mot-clé **async**.

L'inverse n'est pas toujours vrai : vous pouvez marquer une méthode comme "async" sans utiliser "wait" dans son corps.

Ce que `wait` fait réellement est de suspendre l'exécution du code jusqu'à ce que la tâche attendue se termine ; n'importe quelle tâche peut être attendue.

**Remarque :** vous ne pouvez pas attendre la méthode async qui ne renvoie rien (void).

En fait, le mot "suspend" est un peu trompeur car non seulement l'exécution s'arrête, mais le thread peut devenir libre pour exécuter d'autres opérations. Sous le capot, `wait` est implémenté par un peu de magie du compilateur : il divise une méthode en deux parties - avant et après `wait`. Cette dernière partie est exécutée lorsque la tâche attendue se termine.

Si nous ignorons certains détails importants, le compilateur le fait à peu près pour vous :

    public async Task<TResult> DoIt()
    {
        // do something and acquire someTask of type Task<TSomeResult>  
        var awaitedResult = await someTask;
        // ... do something more and produce result of type TResult
        return result;
    }

devient:

    public Task<TResult> DoIt()
    {
        // ...
        return someTask.ContinueWith(task => {
            var result = ((Task<TSomeResult>)task).Result;
            return DoIt_Continuation(result);
        });
    }
    
    private TResult DoIt_Continuation(TSomeResult awaitedResult)
    {
        // ...
    }

Toute méthode habituelle peut être transformée en asynchrone de la manière suivante :

    await Task.Run(() => YourSyncMethod());

Cela peut être avantageux lorsque vous devez exécuter une méthode longue sur le thread d'interface utilisateur sans geler l'interface utilisateur.

Mais il y a une remarque très importante ici : **Asynchrone ne signifie pas toujours concurrent (parallèle ou même multi-thread).** Même sur un seul thread, `async`-`await` autorise toujours le code asynchrone. Par exemple, consultez ce [planificateur de tâches][1] personnalisé. Un tel planificateur de tâches "fou" peut simplement transformer des tâches en fonctions qui sont appelées dans le traitement de la boucle de message.

Nous devons nous demander : quel thread exécutera la suite de notre méthode `DoIt_Continuation` ?

Par défaut, l'opérateur `wait` planifie l'exécution de la poursuite avec le [contexte de synchronisation] actuel [2]. Cela signifie que, par défaut, pour WinForms et WPF, la continuation s'exécute dans le thread d'interface utilisateur. Si, pour une raison quelconque, vous devez modifier ce comportement, utilisez [method][3] `Task.ConfigureAwait()` :

    await Task.Run(() => YourSyncMethod()).ConfigureAwait(continueOnCapturedContext: false);

[1] : https://msdn.microsoft.com/en-us/library/system.threading.tasks.taskscheduler(v=vs.110).aspx
[2] : https://msdn.microsoft.com/en-us/library/system.threading.synchronizationcontext(v=vs.110).aspx
[3] : https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx

## Appels simultanés
Il est possible d'attendre plusieurs appels simultanément en invoquant d'abord les tâches en attente et *ensuite* en les attendant.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await firstTask;
        await secondTask;
    }

Alternativement, `Task.WhenAll` peut être utilisé pour regrouper plusieurs tâches en une seule `Task`, qui se termine lorsque toutes ses tâches passées sont terminées.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await Task.WhenAll(firstTask, secondTask);
    }

Vous pouvez également le faire à l'intérieur d'une boucle, par exemple :

    List<Task> tasks = new List<Task>();
    while (something) {
        // do stuff
        Task someAsyncTask = someAsyncMethod();
        tasks.Add(someAsyncTask);
    }

    await Task.WhenAll(tasks);

Pour obtenir les résultats d'une tâche après avoir attendu plusieurs tâches avec Task.WhenAll, attendez simplement la tâche à nouveau. Étant donné que la tâche est déjà terminée, elle renverra simplement le résultat

    var task1 = SomeOpAsync();
    var task2 = SomeOtherOpAsync();

    await Task.WhenAll(task1, task2);

    var result = await task2;


En outre, le `Task.WhenAny` peut être utilisé pour exécuter plusieurs tâches en parallèle, comme le `Task.WhenAll` ci-dessus, à la différence que cette méthode se terminera lorsque * n'importe laquelle * des tâches fournies sera terminée.

    public async Task RunConcurrentTasksWhenAny()
    {
        var firstTask = TaskOperation("#firstTask executed");
        var secondTask = TaskOperation("#secondTask executed");
        var thirdTask = TaskOperation("#thirdTask executed");
        await Task.WhenAny(firstTask, secondTask, thirdTask);
    }

La `Task` renvoyée par `RunConcurrentTasksWhenAny` se terminera lorsque l'une des `firstTask`, `secondTask` ou `thirdTask` se terminera.



## Essayer/Attraper/Enfin
<!-- si version [gte 6.0] -->

Depuis C# 6.0, le mot-clé `await` peut désormais être utilisé dans un bloc `catch` et `finally`.

    try {
       var client = new AsyncClient();
       await client.DoSomething();
    } catch (MyException ex) {
       await client.LogExceptionAsync();
       throw;
    } finally {
       await client.CloseAsync();
    }
<!-- fin de version si -->

<!-- si version [gte 5.0] [lt 6.0] -->

Avant C# 6.0, vous deviez faire quelque chose dans le sens de ce qui suit. Notez que la version 6.0 a également nettoyé les vérifications nulles avec l'[opérateur de propagation nulle] [1].

    AsynClient client;
    MyException caughtException;
    try {
         client = new AsyncClient();
         await client.DoSomething();
    } catch (MyException ex) {
         caughtException = ex;
    }
    
    if (client != null) {
        if (caughtException != null) {
           await client.LogExceptionAsync();
        }
        await client.CloseAsync();
        if (caughtException != null) throw caughtException;
    }
<!-- fin de version si -->

Veuillez noter que si vous attendez une tâche non créée par `async` (par exemple, une tâche créée par `Task.Run`), certains débogueurs peuvent s'arrêter sur les exceptions lancées par la tâche même lorsqu'elle est apparemment gérée par le try/catch environnant. Cela se produit parce que le débogueur considère qu'il n'est pas géré par rapport au code utilisateur. Dans Visual Studio, il existe une option appelée ["Just My Code"][2], qui peut être désactivée pour empêcher le débogueur de se casser dans de telles situations.

[1] : https://www.wikiod.com/fr/docs/c%23/24/c-6-features/51/null-propagation#t=201511271308000980289
[2] : https://msdn.microsoft.com/en-us/library/dn457346.aspx "Documentation Just My Code sur MSDN"


## Renvoyer une tâche sans attendre
Les méthodes qui effectuent des opérations asynchrones n'ont pas besoin d'utiliser `wait` si :

* Il n'y a qu'un seul appel asynchrone à l'intérieur de la méthode
* L'appel asynchrone est à la fin de la méthode
* L'exception de capture/gestion qui peut se produire dans la tâche n'est pas nécessaire

Considérez cette méthode qui renvoie une `Task` :

    public async Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return await dataStore.GetByKeyAsync(lookupKey);
    }

Si `GetByKeyAsync` a la même signature que `GetUserAsync` (renvoyant un `Task<User>`), la méthode peut être simplifiée :

    public Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return dataStore.GetByKeyAsync(lookupKey);
    }

Dans ce cas, la méthode n'a pas besoin d'être marquée "async", même si elle effectue une opération asynchrone. La tâche renvoyée par `GetByKeyAsync` est transmise directement à la méthode appelante, où elle sera `attendue`.

**Important** : renvoyer la `Task` au lieu de l'attendre, modifie le comportement d'exception de la méthode, car elle ne lèvera pas l'exception à l'intérieur de la méthode qui démarre la tâche mais dans la méthode qui l'attend.

    public Task SaveAsync()
    {
        try {
            return dataStore.SaveChangesAsync();
        }
        catch(Exception ex)
        {
            // this will never be called
            logger.LogException(ex);
        }
    }

    // Some other code calling SaveAsync()

    // If exception happens, it will be thrown here, not inside SaveAsync()
    await SaveAsync();

Cela améliorera les performances car cela évitera au compilateur de générer une machine d'état **async** supplémentaire.

## Configuration de Web.config sur la cible 4.5 pour un comportement asynchrone correct.
Le web.config system.web.httpRuntime doit cibler 4.5 pour s'assurer que le thread rentera le contexte de la requête avant de reprendre votre méthode asynchrone.

    <httpRuntime targetFramework="4.5" />

Async et await ont un comportement indéfini sur ASP.NET avant 4.5. Async / await reprendra sur un thread arbitraire qui peut ne pas avoir le contexte de la requête. Les applications en charge échoueront de manière aléatoire avec des exceptions de référence nulles accédant au HttpContext après l'attente. http://stackoverflow.com/questions/24956178/using-httpcontext-current-in-webapi-is-dangerous-because-of-async

## Async/wait n'améliorera les performances que s'il permet à la machine d'effectuer un travail supplémentaire
Considérez le code suivant :

    public async Task MethodA()
    {
         await MethodB();
         // Do other work
    }

    public async Task MethodB()
    {
         await MethodC();
         // Do other work
    }

    public async Task MethodC()
    {
         // Or await some other async work
         await Task.Delay(100);
    }

Cela ne fonctionnera pas mieux que

    public void MethodA()
    {
         MethodB();
         // Do other work
    }

    public void MethodB()
    {
         MethodC();
         // Do other work
    }

    public void MethodC()
    {
         Thread.Sleep(100);
    }
Le but principal de async/wait est de permettre à la machine d'effectuer un travail supplémentaire - par exemple, pour permettre au thread appelant d'effectuer d'autres travaux pendant qu'il attend le résultat d'une opération d'E/S. Dans ce cas, le thread appelant n'est jamais autorisé à faire plus de travail qu'il n'aurait pu le faire autrement, il n'y a donc pas de gain de performances en appelant simplement `MethodA()`, `MethodB()` ​​et `MethodC()` de manière synchrone.

## Appels consécutifs simples
    public async Task<JobResult> GetDataFromWebAsync()
    {
      var nextJob = await _database.GetNextJobAsync();
      var response = await _httpClient.GetAsync(nextJob.Uri);
      var pageContents = await response.Content.ReadAsStringAsync();
      return await _database.SaveJobResultAsync(pageContents);
    }

La principale chose à noter ici est que bien que chaque méthode `wait`-ed soit appelée de manière asynchrone - et pendant le temps de cet appel, le contrôle est rendu au système - le flux à l'intérieur de la méthode est linéaire et ne nécessite aucun traitement spécial en raison de l'asynchronisme. Si l'une des méthodes appelées échoue, l'exception sera traitée "comme prévu", ce qui signifie dans ce cas que l'exécution de la méthode sera abandonnée et que l'exception remontera la pile.



## Le blocage sur le code asynchrone peut provoquer des blocages
C'est une mauvaise pratique de bloquer les appels asynchrones car cela peut provoquer des blocages dans les environnements qui ont un contexte de synchronisation. La meilleure pratique consiste à utiliser async/wait "tout en bas". Par exemple, le code Windows Forms suivant provoque un blocage :

    private async Task<bool> TryThis()
    {
        Trace.TraceInformation("Starting TryThis");
        await Task.Run(() =>
        {
            Trace.TraceInformation("In TryThis task");
            for (int i = 0; i < 100; i++)
            {
                // This runs successfully - the loop runs to completion
                Trace.TraceInformation("For loop " + i);
                System.Threading.Thread.Sleep(10);
            }
        });

        // This never happens due to the deadlock
        Trace.TraceInformation("About to return");
        return true;
    }

    // Button click event handler
    private void button1_Click(object sender, EventArgs e)
    {
        // .Result causes this to block on the asynchronous call
        bool result = TryThis().Result;
        // Never actually gets here
        Trace.TraceInformation("Done with result");
    }
Essentiellement, une fois l'appel asynchrone terminé, il attend que le contexte de synchronisation soit disponible. Cependant, le gestionnaire d'événements "s'accroche" au contexte de synchronisation pendant qu'il attend que la méthode `TryThis()` se termine, provoquant ainsi une attente circulaire.

Pour résoudre ce problème, le code doit être modifié pour

    private async void button1_Click(object sender, EventArgs e)
    {
      bool result = await TryThis();
      Trace.TraceInformation("Done with result");
    }

Remarque : les gestionnaires d'événements sont le seul endroit où `async void` doit être utilisé (car vous ne pouvez pas attendre une méthode `async void`).




