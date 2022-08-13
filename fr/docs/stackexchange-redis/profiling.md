---
title: "Profilage"
slug: "profilage"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Les fonctionnalités de profilage de StackExchange.Redis sont composées de l'interface `IProfiler` et des méthodes `ConnectionMultiplexer.RegisterProfiler(IProfiler)`, `ConnectionMultiplexer.BeginProfiling(object)`, `ConnectionMultiplexer.FinishProfiling(object)`.

Le profilage de début et de fin prend un contexte "objet" afin que les commandes associées puissent être regroupées.

Ce regroupement fonctionne en interrogeant votre interface `IProfiler` pour un objet de contexte au début d'une commande, avant que des manigances de thread ne se soient produites, et en associant cette commande à toute autre commande ayant le même objet de contexte. Begin doit être appelé avec le même objet de contexte afin que StackExchange.Redis sache démarrer les commandes de profilage avec cet objet de contexte, et Finish est appelé pour arrêter le profilage et renvoyer les résultats.

## Regrouper toutes les commandes d'un ensemble de threads ensemble
    class ToyProfiler : IProfiler
    {
        public ConcurrentDictionary<Thread, object> Contexts = new ConcurrentDictionary<Thread, object>();
    
        public object GetContext()
        {
            object ctx;
            if(!Contexts.TryGetValue(Thread.CurrentThread, out ctx)) ctx = null;
    
            return ctx;
        }
    }
    
    
    // ...
    
    ConnectionMultiplexer conn = /* initialization */;
    var profiler = new ToyProfiler();
    var thisGroupContext = new object();
    
    conn.RegisterProfiler(profiler);
    
    var threads = new List<Thread>();
    
    for (var i = 0; i < 16; i++)
    {
        var db = conn.GetDatabase(i);
    
        var thread =
            new Thread(
                delegate()
                {
                    var threadTasks = new List<Task>();
    
                    for (var j = 0; j < 1000; j++)
                    {
                        var task = db.StringSetAsync("" + j, "" + j);
                        threadTasks.Add(task);
                    }
    
                    Task.WaitAll(threadTasks.ToArray());
                }
            );
    
        profiler.Contexts[thread] = thisGroupContext;
    
        threads.Add(thread);
    }
    
    conn.BeginProfiling(thisGroupContext);
    
    threads.ForEach(thread => thread.Start());
    threads.ForEach(thread => thread.Join());
    
    IEnumerable<IProfiledCommand> timings = conn.FinishProfiling(thisGroupContext);

À la fin, les minutages contiendront 16 000 objets IProfiledCommand - un pour chaque commande envoyée à redis.

## Regrouper les commandes en fonction de l'émission du thread
    ConnectionMultiplexer conn = /* initialization */;
    var profiler = new ToyProfiler();
    
    conn.RegisterProfiler(profiler);
    
    var threads = new List<Thread>();
    
    var perThreadTimings = new ConcurrentDictionary<Thread, List<IProfiledCommand>>();
    
    for (var i = 0; i < 16; i++)
    {
        var db = conn.GetDatabase(i);
    
        var thread =
            new Thread(
                delegate()
                {
                    var threadTasks = new List<Task>();
    
                    conn.BeginProfiling(Thread.CurrentThread);
    
                    for (var j = 0; j < 1000; j++)
                    {
                        var task = db.StringSetAsync("" + j, "" + j);
                        threadTasks.Add(task);
                    }
    
                    Task.WaitAll(threadTasks.ToArray());
    
                    perThreadTimings[Thread.CurrentThread] = conn.FinishProfiling(Thread.CurrentThread).ToList();
                }
            );
    
        profiler.Contexts[thread] = thread;
    
        threads.Add(thread);
    }
    
    threads.ForEach(thread => thread.Start());
    threads.ForEach(thread => thread.Join());

`perThreadTimings` se retrouve avec 16 entrées de 1 000 IProfilingCommands, codées par le Thread qui les a émises.

