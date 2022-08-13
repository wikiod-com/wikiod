---
title: "Asyncawait, Backgroundworker, Task ve Thread Örnekleri"
slug: "asyncawait-backgroundworker-task-ve-thread-ornekleri"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

Bu örneklerden herhangi birini çalıştırmak için onları şöyle çağırmanız yeterlidir:

    static void Main()
    {
        new Program().ProcessDataAsync();
        Console.ReadLine();
    }

## ASP.NET Yapılandırma Bekliyor
ASP.NET bir isteği işlediğinde, iş parçacığı havuzundan bir iş parçacığı atanır ve bir **istek bağlamı** oluşturulur. İstek bağlamı, statik ```HttpContext.Current``` özelliği aracılığıyla erişilebilen mevcut istek hakkında bilgi içerir. İsteğin istek bağlamı daha sonra isteği işleyen iş parçacığına atanır.

Belirli bir istek bağlamı ** aynı anda yalnızca bir iş parçacığında etkin olabilir**.

Yürütme ``bekliyor```a ulaştığında, asenkron yöntem çalışırken bir isteği işleyen iş parçacığı iş parçacığı havuzuna döndürülür ve istek bağlamı başka bir iş parçacığının kullanması için serbest kalır.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        var products = await dbContext.Products.ToListAsync();

        // Execution resumes on a "random" thread from the pool
        // Execution continues using the original request context.
        return View(products);
    }

Görev tamamlandığında, iş parçacığı havuzu, isteğin yürütülmesine devam etmek için başka bir iş parçacığı atar. İstek bağlamı daha sonra bu iş parçacığına atanır. Bu orijinal konu olabilir veya olmayabilir.

### Engelleme ###

Bir ``` eşzamansız``` yöntem çağrısının sonucu **eşzamanlı** olarak beklendiğinde kilitlenmeler ortaya çıkabilir. Örneğin, aşağıdaki kod ```IndexSync()``` çağrıldığında kilitlenmeye neden olacaktır:

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

Bunun nedeni, varsayılan olarak beklenen görevin, bu durumda ```db.Products.ToListAsync()``` bağlamı yakalaması (ASP.NET durumunda istek bağlamı) ve bir kez onu kullanmaya çalışmasıdır. Tamamlandı.

Çağrı yığınının tamamı eşzamansız olduğunda sorun yoktur, çünkü "bekliyor""a ulaşıldığında, orijinal iş parçacığı serbest bırakılır ve istek bağlamı serbest bırakılır.

``Task.Result`` veya ```Task.Wait()``` (veya diğer engelleme yöntemlerini) kullanarak eşzamanlı olarak engellediğimizde, orijinal iş parçacığı hala etkindir ve istek bağlamını korur. Beklenen yöntem hala eşzamansız olarak çalışır ve geri arama çalışmaya çalıştığında, yani beklenen görev geri döndüğünde, istek bağlamını elde etmeye çalışır.

Bu nedenle kilitlenme, istek bağlamıyla bloke eden iş parçacığının zaman uyumsuz işlemin tamamlanmasını beklerken, zaman uyumsuz işlemin tamamlamak için istek bağlamını elde etmeye çalışması nedeniyle ortaya çıkar.

### ConfigureBekleme ###

Varsayılan olarak, beklenen bir göreve yapılan çağrılar mevcut bağlamı yakalar ve tamamlandıktan sonra bağlamda yürütmeye devam etmeye çalışır.

``ConfigureAwait(false)``` kullanılarak bu önlenebilir ve kilitlenmelerden kaçınılabilir.

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

Bu, asenkron kodun engellenmesi gerektiğinde kilitlenmeleri önleyebilir, ancak bu, devamdaki bağlamı kaybetme pahasına gelir (çağrıdan sonraki kod bekliyor).

ASP.NET'te bunun anlamı, ```await someTask.ConfigureAwait(false);``` aramasını izleyen kodunuz bağlamdan bilgilere erişmeye çalışırsa, örneğin ```HttpContext.Current.User`` o zaman bilgi kayboldu. Bu durumda ```HttpContext.Current``` boştur. Örneğin:

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

``ConfigureAwait(true)``` kullanılırsa (hiç ConfigureAwait olmamasına eşdeğer), o zaman hem ``user``` hem de ```user2`` aynı verilerle doldurulur.

Bu nedenle, bağlamın artık kullanılmadığı kitaplık kodunda genellikle ``ConfigureAwait(false)`` kullanılması önerilir.

## Zaman uyumsuz/bekleme
Bir arka plan işleminde zaman yoğun işler yapmak için async/await'in nasıl kullanılacağına dair basit bir örnek için aşağıya bakın, aynı zamanda tamamlanması için yoğun zaman gerektiren şeyleri beklemesi gerekmeyen başka şeyler yapma seçeneğini korurken.

Ancak daha sonra zaman yoğun yöntemin sonucu ile çalışmanız gerekirse, yürütmeyi bekleyerek bunu yapabilirsiniz.

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

## Arka PlanÇalışan
Bir arka plan iş parçacığında yoğun zaman alan işlemler gerçekleştirmek için bir "BackgroundWorker" nesnesinin nasıl kullanılacağına ilişkin basit bir örnek için aşağıya bakın.

Gerek:
1. Yoğun zaman alan işi yapan bir çalışan yöntemi tanımlayın ve bunu bir "BackgroundWorker"ın "DoWork" olayı için bir olay işleyicisinden çağırın.
3. Yürütmeyi `RunWorkerAsync` ile başlatın. 'DoWork'a eklenen çalışan yönteminin gerektirdiği herhangi bir bağımsız değişken, 'DoWorkEventArgs' parametresi aracılığıyla 'RunWorkerAsync' öğesine iletilebilir.

'DoWork' olayına ek olarak, 'BackgroundWorker' sınıfı, kullanıcı arayüzü ile etkileşim için kullanılması gereken iki olayı da tanımlar. Bunlar isteğe bağlıdır.

* 'DoWork' işleyicileri tamamlandığında 'RunWorkerCompleted' olayı tetiklenir.
* 'ReportProgress' yöntemi çağrıldığında 'ProgressChanged' olayı tetiklenir.


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


## Görev
Bir arka plan işleminde biraz zaman alan işler yapmak için bir "Görev"in nasıl kullanılacağına ilişkin basit bir örnek için aşağıya bakın.

Tek yapmanız gereken zaman yoğun yönteminizi bir `Task.Run()` çağrısına sarmak.

    

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

## İplik
Bir arka plan işleminde biraz zaman alan şeyler yapmak için bir "İş parçacığı"nın nasıl kullanılacağına ilişkin basit bir örnek için aşağıya bakın.

    

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

Gördüğünüz gibi, `Thread` parametresi olarak bir void Method beklediğinden `TimeIntensiveMethod`umuzdan bir değer döndüremiyoruz.

Bir "İş parçacığı"ndan bir dönüş değeri almak için ya bir olay ya da aşağıdakini kullanın:

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

## "Çalıştır ve unut" görevi uzantısı
Bazı durumlarda (örn. günlüğe kaydetme) görevi çalıştırmak ve sonucu beklememek faydalı olabilir. Aşağıdaki uzantı, görevi çalıştırmanıza ve kalan kodun yürütülmesine devam etmenize izin verir:

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

Sonuç sadece extension method içinde beklenir. "async"/"await" kullanıldığından, bir istisna yakalamak ve onu işlemek için isteğe bağlı bir yöntem çağırmak mümkündür.

Uzantının nasıl kullanılacağına bir örnek:

    var task = Task.FromResult(0); // Or any other task from e.g. external lib.
    task.RunAndForget(
        e =>
        {
            // Something went wrong, handle it.
        });

