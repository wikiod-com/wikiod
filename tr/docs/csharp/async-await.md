---
title: "zaman uyumsuz-bekliyor"
slug: "zaman-uyumsuz-bekliyor"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

C#'ta, G/Ç tabanlı işlemler (örneğin web erişimi, dosyalarla çalışma, ...) kullanıyorsanız, "async" olarak bildirilen bir yöntem senkronize bir işlemde engellemez. Bu tür zaman uyumsuz olarak işaretlenmiş yöntemlerin sonucu, 'bekliyor' anahtar sözcüğü kullanılarak beklenebilir.

Bir "async" yöntemi, "void", "Task" veya "Task<T>" döndürebilir.

Döndürme türü 'Task', yöntemin bitmesini bekleyecek ve sonuç 'void' olacaktır. `Task<T>`, yöntem tamamlandıktan sonra `T` türünden bir değer döndürür.

"async" yöntemleri, hemen hemen her durumda "void" yerine "Task" veya "Task<T>" döndürmelidir. "async void" yöntemleri "beklenemez", bu da çeşitli sorunlara yol açar. Bir "async"in "void" döndürmesi gereken tek senaryo, bir olay işleyicisi durumundadır.

"async"/"await", "async" yönteminizi bir durum makinesine dönüştürerek çalışır. Bunu, geçerli durumu ve herhangi bir bağlamı (yerel değişkenler gibi) saklayan ve beklenen bir beklenebilir tamamlandığında durumları ilerletmek (ve herhangi bir ilişkili kodu çalıştırmak) için bir 'MoveNext()' yöntemini ortaya çıkaran perde arkasında bir yapı oluşturarak yapar.

## Bekleme operatörü ve zaman uyumsuz anahtar kelime
'await' operatörü ve 'async' anahtar kelimesi bir araya gelir:

> **await**'in kullanıldığı eşzamansız yöntem şu şekilde değiştirilmelidir:
> **async** anahtar sözcüğü.

Bunun tersi her zaman doğru değildir: Bir yöntemi, gövdesinde 'await' kullanmadan 'async' olarak işaretleyebilirsiniz.

'Bekleme'nin gerçekte yaptığı şey, beklenen görev tamamlanana kadar kodun yürütülmesini askıya almaktır; herhangi bir görev beklenebilir.

**Not:** Hiçbir şey döndürmeyen (void) zaman uyumsuz yöntemi bekleyemezsiniz.

Aslında, 'askıya alır' kelimesi biraz yanıltıcıdır çünkü sadece yürütme durmakla kalmaz, aynı zamanda iş parçacığı diğer işlemleri yürütmek için serbest hale gelebilir. Başlık altında, 'await' biraz derleyici sihri tarafından uygulanır: bir yöntemi iki bölüme ayırır - 'bekleme'den önce ve sonra. İkinci kısım, beklenen görev tamamlandığında yürütülür.

Bazı önemli detayları görmezden gelirsek, derleyici bunu sizin için kabaca yapar:

    public async Task<TResult> DoIt()
    {
        // do something and acquire someTask of type Task<TSomeResult>  
        var awaitedResult = await someTask;
        // ... do something more and produce result of type TResult
        return result;
    }

olur:

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

Herhangi bir olağan yöntem, aşağıdaki şekilde zaman uyumsuz hale getirilebilir:

    await Task.Run(() => YourSyncMethod());

Bu, UI'yi dondurmadan UI iş parçacığında uzun süre çalışan bir yöntem yürütmeniz gerektiğinde avantajlı olabilir.

Ancak burada çok önemli bir not var: **Eşzamansız her zaman eşzamanlı (paralel veya hatta çok iş parçacıklı) anlamına gelmez.** Tek bir iş parçacığında bile, 'async'-'await' yine de eşzamansız koda izin verir. Örneğin, bu özel [görev zamanlayıcı][1] konusuna bakın. Böyle bir 'çılgın' görev zamanlayıcı, görevleri mesaj döngüsü işleme içinde çağrılan işlevlere dönüştürebilir.

Kendimize sormalıyız: `DoIt_Continuation` yöntemimizin devamını hangi iş parçacığı yürütecek?

Varsayılan olarak, 'bekliyor' operatörü, mevcut [Senkronizasyon bağlamı][2] ile devamın yürütülmesini programlar. Bu, WinForms ve WPF için varsayılan olarak UI iş parçacığında devam ettiği anlamına gelir. Herhangi bir nedenle bu davranışı değiştirmeniz gerekirse, [method][3] `Task.ConfigureAwait()` kullanın:

    await Task.Run(() => YourSyncMethod()).ConfigureAwait(continueOnCapturedContext: false);

[1]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.taskscheduler(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.synchronizationcontext(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx

## Eşzamanlı aramalar
Önce beklenen görevleri başlatarak ve *sonra* onları bekleyerek birden fazla çağrıyı aynı anda beklemek mümkündür.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await firstTask;
        await secondTask;
    }

Alternatif olarak, 'Task.WhenAll', birden çok görevi tek bir 'Görev'de gruplamak için kullanılabilir; bu, tüm geçilen görevler tamamlandığında tamamlanır.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await Task.WhenAll(firstTask, secondTask);
    }

Bunu bir döngü içinde de yapabilirsiniz, örneğin:

    List<Task> tasks = new List<Task>();
    while (something) {
        // do stuff
        Task someAsyncTask = someAsyncMethod();
        tasks.Add(someAsyncTask);
    }

    await Task.WhenAll(tasks);

Task.WhenAll ile birden fazla görevi bekledikten sonra bir görevden sonuç almak için görevi tekrar beklemeniz yeterlidir. Görev zaten tamamlandığından sonucu geri döndürür

    var task1 = SomeOpAsync();
    var task2 = SomeOtherOpAsync();

    await Task.WhenAll(task1, task2);

    var result = await task2;


Ayrıca, 'Task.WhenAny', yukarıdaki 'Task.WhenAll' gibi birden çok görevi paralel olarak yürütmek için kullanılabilir, ancak bu yöntemin sağlanan görevlerden *herhangi biri* tamamlandığında tamamlanması farkıyla.

    public async Task RunConcurrentTasksWhenAny()
    {
        var firstTask = TaskOperation("#firstTask executed");
        var secondTask = TaskOperation("#secondTask executed");
        var thirdTask = TaskOperation("#thirdTask executed");
        await Task.WhenAny(firstTask, secondTask, thirdTask);
    }

"RunConcurrentTasksWhenAny" tarafından döndürülen "Task", "firstTask", "secondTask" veya "üçüncü Görev"den herhangi biri tamamlandığında tamamlanır.



## Dene/Yakala/Sonunda
<!-- eğer [gte 6.0] versiyonu -->

C# 6.0'dan itibaren, 'await' anahtar sözcüğü artık bir 'catch' ve 'finally' bloğu içinde kullanılabilir.

    try {
       var client = new AsyncClient();
       await client.DoSomething();
    } catch (MyException ex) {
       await client.LogExceptionAsync();
       throw;
    } finally {
       await client.CloseAsync();
    }
<!-- eğer --> son sürüm

<!-- eğer sürüm [gte 5.0] [lt 6.0] -->

C# 6.0'dan önce, aşağıdaki satırlar boyunca bir şeyler yapmanız gerekir. 6.0'ın [Boş Yayma operatörü][1] ile boş denetimleri de temizlediğini unutmayın.

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
<!-- eğer --> son sürüm

Lütfen, "async" tarafından oluşturulmamış bir görevi beklerseniz (örneğin, "Task.Run" tarafından oluşturulan bir görev), bazı hata ayıklayıcıların, görünüşte çevredeki try/catch tarafından işleniyor olsa bile, görev tarafından oluşturulan istisnaları bozabileceğini unutmayın. Bunun nedeni, hata ayıklayıcının kullanıcı koduna göre işlenmediğini düşünmesidir. Visual Studio'da, bu gibi durumlarda hata ayıklayıcının bozulmasını önlemek için devre dışı bırakılabilen ["Just My Code"][2] adlı bir seçenek vardır.

[1]: https://www.wikiod.com/tr/docs/c%23/24/c-6-features/51/null-propagation#t=201511271308000980289
[2]: https://msdn.microsoft.com/en-us/library/dn457346.aspx "MSDN'de Just My Code belgeleri"


## Beklemeden Görevi Geri Döndürme
Asenkron işlemler gerçekleştiren yöntemlerin aşağıdaki durumlarda "await" kullanmasına gerek yoktur:

* Yöntemin içinde yalnızca bir eşzamansız çağrı var
* Asenkron çağrı yöntemin sonundadır.
* Görev içinde meydana gelebilecek yakalama/işleme istisnası gerekli değildir

Bir "Görev" döndüren bu yöntemi düşünün:

    public async Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return await dataStore.GetByKeyAsync(lookupKey);
    }

"GetByKeyAsync", "GetUserAsync" ile aynı imzaya sahipse ("Görev<Kullanıcı>" döndürerek), yöntem basitleştirilebilir:

    public Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return dataStore.GetByKeyAsync(lookupKey);
    }

Bu durumda, zaman uyumsuz bir işlem gerçekleştiriyor olsa bile, yöntemin "async" olarak işaretlenmesi gerekmez. 'GetByKeyAsync' tarafından döndürülen Görev, doğrudan 'bekleneceği' çağrı yöntemine iletilir.

**Önemli**: Görevi beklemek yerine döndürmek, yöntemin istisna davranışını değiştirir, çünkü istisnayı görevi başlatan yöntemin içine değil, onu bekleyen yönteme atar.

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

Bu, derleyiciye fazladan bir **async** durum makinesi oluşturmasını sağlayacağından performansı artıracaktır.

## Doğru zaman uyumsuz davranış için 4.5'i hedefleyen Web.config kurulumu.
web.config system.web.httpRuntime, zaman uyumsuz yönteminizi sürdürmeden önce iş parçacığının istek bağlamını kiralamasını sağlamak için 4.5'i hedeflemelidir.

    <httpRuntime targetFramework="4.5" />

Zaman uyumsuz ve bekliyor, 4.5'ten önce ASP.NET'te tanımsız davranışa sahiptir. Zaman uyumsuz / bekliyor, istek içeriğine sahip olmayabilecek rastgele bir iş parçacığında devam edecek. Yük altındaki uygulamalar, beklemeden sonra HttpContext'e erişen boş referans istisnaları ile rastgele başarısız olur. http://stackoverflow.com/questions/24956178/using-httpcontext-current-in-webapi-is-dangerous-because-of-async

## Async/await, yalnızca makinenin ek iş yapmasına izin veriyorsa performansı artırır
Aşağıdaki kodu göz önünde bulundurun:

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

Bu, şundan daha iyi performans göstermez:

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
async/await'in birincil amacı, makinenin ek iş yapmasına izin vermektir - örneğin, çağıran iş parçacığının bazı G/Ç işlemlerinden bir sonuç beklerken başka işler yapmasına izin vermek. Bu durumda, çağıran iş parçacığının başka türlü yapabileceğinden daha fazla iş yapmasına asla izin verilmez, bu nedenle yalnızca 'MethodA()', 'MethodB()' ve 'MethodC()' çağrıldığında herhangi bir performans kazancı yoktur. eşzamanlı olarak.

## Basit ardışık aramalar
    public async Task<JobResult> GetDataFromWebAsync()
    {
      var nextJob = await _database.GetNextJobAsync();
      var response = await _httpClient.GetAsync(nextJob.Uri);
      var pageContents = await response.Content.ReadAsStringAsync();
      return await _database.SaveJobResultAsync(pageContents);
    }

Burada dikkat edilmesi gereken en önemli nokta, her 'beklenen' yöntemin asenkron olarak çağrılmasına ve bu çağrının yapıldığı süre boyunca kontrolün sisteme geri verilmesine rağmen, yöntemin içindeki akışın lineer olduğu ve herhangi bir özel işlem gerektirmediğidir. asenkron nedeniyle. Çağrılan yöntemlerden herhangi biri başarısız olursa, istisna "beklendiği gibi" işlenir, bu durumda yöntem yürütmenin durdurulacağı ve istisna yığının yukarısına çıkacağı anlamına gelir.



## Zaman uyumsuz kodun engellenmesi kilitlenmelere neden olabilir
Senkronizasyon bağlamı olan ortamlarda kilitlenmelere neden olabileceğinden, zaman uyumsuz çağrıları engellemek kötü bir uygulamadır. En iyi uygulama, async/await'i "tamamen aşağı" kullanmaktır. Örneğin, aşağıdaki Windows Forms kodu bir kilitlenmeye neden olur:

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
Esasen, zaman uyumsuz çağrı tamamlandığında, eşitleme bağlamının kullanılabilir hale gelmesini bekler. Ancak olay işleyicisi, `TryThis()` yönteminin tamamlanmasını beklerken senkronizasyon bağlamına "tutarak" döngüsel bir beklemeye neden olur.

Bunu düzeltmek için kod şu şekilde değiştirilmelidir:

    private async void button1_Click(object sender, EventArgs e)
    {
      bool result = await TryThis();
      Trace.TraceInformation("Done with result");
    }

Not: Olay işleyicileri, "async void"in kullanılması gereken tek yerdir (çünkü bir "async void" yöntemini bekleyemezsiniz).




