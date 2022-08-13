---
title: "perfilado"
slug: "perfilado"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Las funciones de generación de perfiles de StackExchange.Redis se componen de la interfaz `IProfiler` y los métodos `ConnectionMultiplexer.RegisterProfiler(IProfiler)`, `ConnectionMultiplexer.BeginProfiling(objeto)`, `ConnectionMultiplexer.FinishProfiling(objeto)`.

Comenzar y finalizar la creación de perfiles toman un "objeto" de contexto para que los comandos relacionados se puedan agrupar.

Esta agrupación funciona consultando su interfaz `IProfiler` para un objeto de contexto al comienzo de un comando, antes de que haya ocurrido cualquier travesura de subprocesamiento, y asociando ese comando con cualquier otro comando que tenga el mismo objeto de contexto. Se debe llamar a Begin con el mismo objeto de contexto para que StackExchange.Redis sepa que debe comenzar a generar perfiles de los comandos con ese objeto de contexto, y se llama a Finish para detener la generación de perfiles y devolver los resultados.

## Agrupe todos los comandos del conjunto de hilos juntos
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

Al final, los tiempos contendrán 16 000 objetos IProfiledCommand, uno para cada comando emitido a redis.

## Comandos de grupo basados ​​en hilo emisor
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

`perThreadTimings` termina con 16 entradas de 1,000 IProfilingCommands, codificadas por el subproceso que las emitió.

