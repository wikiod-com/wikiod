---
title: "Perfil"
slug: "perfil"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Os recursos de criação de perfil do StackExchange.Redis são compostos pela interface `IProfiler` e pelos métodos `ConnectionMultiplexer.RegisterProfiler(IProfiler)`, `ConnectionMultiplexer.BeginProfiling(object)`, `ConnectionMultiplexer.FinishProfiling(object)`.

A criação de perfil de início e término usa um `objeto` de contexto para que os comandos relacionados possam ser agrupados.

Esse agrupamento funciona consultando sua interface `IProfiler` por um objeto de contexto no início de um comando, antes que qualquer travessura de encadeamento aconteça, e associando esse comando a qualquer outro comando que tenha o mesmo objeto de contexto. Begin deve ser chamado com o mesmo objeto de contexto para que o StackExchange.Redis saiba iniciar os comandos de criação de perfil com esse objeto de contexto e o Finish seja chamado para interromper a criação de perfil e retornar os resultados.

## Agrupe todos os comandos do conjunto de threads juntos
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

No final, os tempos conterão 16.000 objetos IProfiledCommand - um para cada comando emitido para redis.

## Comandos de grupo com base no encadeamento de emissão
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

`perThreadTimings` termina com 16 entradas de 1.000 IProfilingCommands, codificados pelo Thread que os emitiu.

