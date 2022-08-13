---
title: "Profiling"
slug: "profiling"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

StackExchange.Redis's profiling features are composed of the `IProfiler` interface, and the `ConnectionMultiplexer.RegisterProfiler(IProfiler)`, `ConnectionMultiplexer.BeginProfiling(object)`, `ConnectionMultiplexer.FinishProfiling(object)` methods.

Begin and Finish profiling take a context `object` so that related commands can be grouped together.

This grouping works by querying your `IProfiler` interface for a context object at the start of a command, before any threading shenanigans have happened, and associating that command with a any other commands that have the same context object.  Begin must be called with the same context object so StackExchange.Redis knows to start profiling commands with that context object, and Finish is called to stop profiling and return the results.

## Group all commands from set of threads together
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

At the end, timings will contain 16,000 IProfiledCommand objects - one for each command issued to redis.

## Group commands based on issuing thread
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

`perThreadTimings` ends up with 16 entries of 1,000 IProfilingCommands, keyed by the Thread that issued them.

