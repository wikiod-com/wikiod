---
title: "Görev Paralel Kitaplığı (TPL)"
slug: "gorev-paralel-kitaplg-tpl"
draft: false
images: []
weight: 9746
type: docs
toc: true
---

Amaç ve Kullanım Durumları
=======
Task Parallel Library'nin amacı, çok iş parçacıklı ve paralel kod yazma ve koruma sürecini basitleştirmektir.

Bazı Kullanım Durumları*:

- Ayrı bir görevde arka plan çalışması çalıştırarak bir kullanıcı arayüzünü duyarlı tutmak
- İş yükünün dağıtılması
- Bir istemci uygulamasının aynı anda istek gönderip almasına izin verme (dinlenme, TCP/UDP, vb.)
- Aynı anda birden fazla dosya okuma ve/veya yazma

*Kod, çoklu kullanım için duruma göre değerlendirilmelidir. Örneğin, bir döngünün yalnızca birkaç yinelemesi varsa veya işin yalnızca küçük bir miktarını yapıyorsa, paralellik için ek yük, faydalardan daha ağır basabilir.

**.Net 3.5 ile TPL**

TPL, bir NuGet paketine dahil edilen .Net 3.5 için de mevcuttur, buna Görev Paralel Kitaplığı adı verilir.

## Temel üretici-tüketici döngüsü (BlockingCollection)
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

`collection.CompleteAdding();` öğesini çağırmazsanız, tüketici göreviniz çalışıyor olsa bile koleksiyona eklemeye devam edebileceğinizi belirtmekte fayda var. Daha fazla ekleme olmadığından emin olduğunuzda `collection.CompleteAdding();` aramanız yeterlidir. Bu işlevsellik, BlockingCollection<T> öğesine öğeleri besleyen birden çok kaynağınız ve öğeleri çekip onlarla bir şeyler yapan tek bir tüketicinin olduğu Tek Tüketici modeline Çoklu Üretici yapmak için kullanılabilir. Tam eklemeyi çağırmadan önce BlockingCollection<T> boşsa, `collection.GetConsumingEnumerable()` öğesinden Enumerable, koleksiyona yeni bir öğe eklenene veya BlockingCollection<T>.CompleteAdding(); çağrılır ve sıra boştur.
    
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

## Paralel. Çağır
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

## Görev: temel örnekleme ve Bekle
"Task" sınıfını doğrudan başlatarak bir görev oluşturulabilir...

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

...veya statik "Task.Run" yöntemini kullanarak:

    Console.WriteLine("Starting task...");
    var task = Task.Run(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });
    task.Wait();
    Console.WriteLine("Task completed!");

Yalnızca ilk durumda açıkça 'Başlat'ı çağırmanın gerekli olduğunu unutmayın.

## Task.WhenAll
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

## Paralel.Herbiri İçin
Bu örnek, birden çok iş parçacığı kullanarak 1 ile 10000 arasındaki sayıların toplamını hesaplamak için 'Parallel.ForEach' kullanır. İş parçacığı güvenliğini sağlamak için, sayıları toplamak için `Interlocked.Add` kullanılır.

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


## Paralel.For
Bu örnek, birden çok iş parçacığı kullanarak 1 ile 10000 arasındaki sayıların toplamını hesaplamak için 'Parallel.For' kullanır. İş parçacığı güvenliğini sağlamak için, sayıları toplamak için `Interlocked.Add` kullanılır.

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

## Görev: Bir değer döndürme
Bir değer döndüren görevin dönüş türü <code>Task< TResult ></code> şeklindedir; burada TResult döndürülmesi gereken değer türüdür. Bir Görevin sonucunu Result özelliği ile sorgulayabilirsiniz.

    Task<int> t = Task.Run(() => 
        {
            int sum = 0;

            for(int i = 0; i < 500; i++)
                sum += i;

            return sum;
        });

    Console.WriteLine(t.Result); // Outuput 124750

Görev, Görev'i beklemekten daha eşzamansız olarak yürütülürse, sonucunu döndürür.

    public async Task DoSomeWork()
    {
        WebClient client = new WebClient();
        // Because the task is awaited, result of the task is assigned to response
        string response = await client.DownloadStringTaskAsync("http://somedomain.com");
    }

    

## Görev: WaitAll ve değişken yakalama
    var tasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    })).ToArray();

    foreach(var task in tasks) task.Start();
    Task.WaitAll(tasks);

    foreach(var task in tasks)
        Console.WriteLine(task.Result);

## Görev: Herhangi Bir Bekleyin
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

**Not:** Son 'WaitAll' gereklidir çünkü 'WaitAny' istisnaların gözlemlenmesine neden olmaz.

## Görev: istisnaları ele alma (Bekle kullanarak)
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

## Görev: istisnaları işleme (Bekle kullanmadan)
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

## Görev: CancellationToken kullanarak iptal etme
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

'ThrowIfCancellationRequested'e alternatif olarak, iptal talebi 'IsCancellationRequested' ile algılanabilir ve bir 'OperationCanceledException' manuel olarak atılabilir:

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

İptal belirtecinin "cancellationToken" parametresinde görev oluşturucuya nasıl iletildiğine dikkat edin. Bu, 'ThrowIfCancellationRequested' çağrıldığında görevin 'Hatalı' durumuna değil, 'İptal Edildi' durumuna geçmesi için gereklidir. Ayrıca, aynı nedenden dolayı, ikinci durumda 'OperationCanceledException' yapıcısında iptal belirteci açıkça sağlanır.

## Task.WhenAny
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

## AsyncLocal ile akan yürütme bağlamı
Ana görevden bazı verileri alt görevlerine iletmeniz gerektiğinde, yürütme ile mantıksal olarak akar, `AsyncLocal` [sınıf][1] kullanın:

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

**Not:** Yukarıdaki örnekte görüldüğü gibi, `AsynLocal.Value` `okunduğunda kopyala` semantiğine sahiptir, ancak bazı referans tiplerini akıtırsanız ve özelliklerini değiştirirseniz diğer görevleri de etkilersiniz. Bu nedenle, "AsyncLocal" ile en iyi uygulama, değer türlerini veya değişmez türleri kullanmaktır.

[1]: https://msdn.microsoft.com/en-us/library/dn906268(v=vs.110).aspx

## VB.NET'te Parallel.ForEach
    For Each row As DataRow In FooDataTable.Rows
        Me.RowsToProcess.Add(row)
    Next
    
    Dim myOptions As ParallelOptions = New ParallelOptions()
    myOptions.MaxDegreeOfParallelism = environment.processorcount
    
    Parallel.ForEach(RowsToProcess, myOptions, Sub(currentRow, state)
                                                   ProcessRowParallel(currentRow, state)
                                               End Sub)

