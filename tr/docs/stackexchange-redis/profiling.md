---
title: "profil oluşturma"
slug: "profil-olusturma"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

StackExchange.Redis'in profil oluşturma özellikleri, 'IProfiler' arabirimi ve 'ConnectionMultiplexer.RegisterProfiler(IProfiler)', 'ConnectionMultiplexer.BeginProfiling(object)', 'ConnectionMultiplexer.FinishProfiling(object)' yöntemlerinden oluşur.

Başla ve Bitir profili oluşturma, ilgili komutların birlikte gruplanabilmesi için bir bağlam 'nesnesi' alır.

Bu gruplandırma, bir komutun başlangıcında, herhangi bir iş parçacığı kurnazlığı meydana gelmeden önce bir bağlam nesnesi için "IProfiller" arabiriminizi sorgulayarak ve bu komutu aynı bağlam nesnesine sahip diğer komutlarla ilişkilendirerek çalışır. Begin aynı bağlam nesnesiyle çağrılmalıdır, böylece StackExchange.Redis bu bağlam nesnesiyle profil oluşturma komutlarını başlatmayı bilir ve profil oluşturmayı durdurmak ve sonuçları döndürmek için Finish çağrılır.

## Konu kümesindeki tüm komutları birlikte gruplandırın
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

Sonunda, zamanlamalar 16.000 IProfiledCommand nesnesi içerecek - redis'e verilen her komut için bir tane.

## Yayınlanan iş parçacığına dayalı olarak grup komutları
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

'perThreadTimings', onları yayınlayan Thread tarafından anahtarlanmış 16 1.000 IProfilingCommands girişiyle sona erer.

