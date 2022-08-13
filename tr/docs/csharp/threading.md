---
title: "diş açma"
slug: "dis-acma"
draft: false
images: []
weight: 9822
type: docs
toc: true
---

Bir **iş parçacığı**, bir programın diğer parçalardan bağımsız olarak yürütülebilen bir parçasıdır. Diğer iş parçacıkları ile aynı anda görevleri gerçekleştirebilir. **Çoklu okuma**, programların aynı anda birden fazla işlemin yapılabilmesi için eşzamanlı işlem gerçekleştirmesini sağlayan bir özelliktir.

Örneğin, aynı anda ön planda diğer görevleri gerçekleştirirken arka planda bir zamanlayıcıyı veya sayacı güncellemek için iş parçacığı oluşturmayı kullanabilirsiniz.

Çok iş parçacıklı uygulamalar, kullanıcı girdisine daha duyarlıdır ve geliştirici iş yükü arttıkça ve arttığında iş parçacığı ekleyebildiğinden, kolayca ölçeklenebilir.

Varsayılan olarak, bir C# programının bir iş parçacığı vardır - ana program iş parçacığı. Ancak ikincil iş parçacıkları oluşturulabilir ve birincil iş parçacığına paralel olarak kod yürütmek için kullanılabilir. Bu tür iş parçacıklarına çalışan iş parçacıkları denir.

Bir iş parçacığının çalışmasını kontrol etmek için CLR, İş Parçacığı Zamanlayıcı olarak bilinen işletim sistemine bir işlevi devreder. Bir iş parçacığı zamanlayıcısı, tüm iş parçacıklarına uygun yürütme süresi tahsis edilmesini sağlar. Ayrıca bloke edilen veya kilitlenen iş parçacıklarının CPU zamanının çoğunu tüketmediğini de kontrol eder.

.NET Framework `System.Threading` ad alanı, iş parçacıklarının kullanımını kolaylaştırır. System.Threading, bir dizi sınıf ve arabirim sağlayarak çoklu kullanım sağlar. Belirli bir iş parçacığı için türler ve sınıflar sağlamanın yanı sıra, bir dizi iş parçacığı, zamanlayıcı sınıfı vb. Ayrıca, paylaşılan verilere senkronize erişime izin vererek desteğini sağlar.

"Thread", "System.Threading" ad alanındaki ana sınıftır. Diğer sınıflar arasında 'AutoResetEvent', 'Interlocked', 'Monitor', 'Mutex' ve 'ThreadPool' bulunur.

System.Threading ad alanında bulunan temsilcilerden bazıları şunlardır:
"ThreadStart", "TimerCallback" ve "WaitCallback".

"System.Threading" ad alanındaki numaralandırmalar arasında "ThreadPriority", "ThreadState",
ve 'EventResetMode'.

.NET Framework 4 ve sonraki sürümlerinde, `System.Threading.Tasks.Parallel` ve `System.Threading.Tasks.Task` sınıfları, Parallel LINQ (PLINQ), yeni eşzamanlı toplama sınıfları aracılığıyla çok iş parçacıklı programlama daha kolay ve basit hale getirilmiştir. 'System.Collections.Concurrent' ad alanı ve yeni bir görev tabanlı programlama modeli.

## Aynı Anda Veri Okumaktan ve Yazmaktan Kaçınma
Bazen konularınızın aynı anda veri paylaşmasını istersiniz. Bu olduğunda, kodun farkında olmak ve yanlış gidebilecek parçaları kilitlemek önemlidir. İki iş parçacığı sayımının basit bir örneği aşağıda gösterilmiştir.

İşte bazı tehlikeli (yanlış) kodlar:

    using System.Threading;
    
    class MainClass 
    {    
        static int count { get; set; }
    
        static void Main() 
        {
            for (int i = 1; i <= 2; i++)
            {
                var thread = new Thread(ThreadMethod);
                thread.Start(i);
                Thread.Sleep(500);
            }
        }
    
        static void ThreadMethod(object threadNumber) 
        {
            while (true)
            {
                var temp = count;
                System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
                Thread.Sleep(1000);
                count = temp + 1;
                System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
                Thread.Sleep(1000);
            }
        }
    }

1,2,3,4,5 yerine 1,1,2,2,3... saydığımızı fark edeceksiniz.

Bu sorunu çözmek için, birden çok farklı iş parçacığının aynı anda okuyup yazamaması için count değerini **kilitlememiz** gerekir. Bir kilit ve bir anahtarın eklenmesiyle, iş parçacıklarının aynı anda verilere erişmesini engelleyebiliriz.

    using System.Threading;
    
    class MainClass
    {
    
        static int count { get; set; } 
        static readonly object key = new object();
    
        static void Main()
        {
            for (int i = 1; i <= 2; i++)
            {
                var thread = new Thread(ThreadMethod);
                thread.Start(i);
                Thread.Sleep(500);
            }
        }
    
        static void ThreadMethod(object threadNumber)
        {
            while (true)
            {
                lock (key) 
                {
                    var temp = count;
                    System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
                    Thread.Sleep(1000);
                    count = temp + 1;
                    System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
                }
                Thread.Sleep(1000);
            }
        }
    }

## İkinci Konu Oluşturma ve Başlatma
Birden fazla uzun hesaplama yapıyorsanız, bunları bilgisayarınızdaki farklı iş parçacıklarında aynı anda çalıştırabilirsiniz. Bunu yapmak için yeni bir **İplik** oluşturuyoruz ve farklı bir yönteme işaret etmesini sağlıyoruz.

    using System.Threading;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start();
        }
    
        static void Secondary() {
            System.Console.WriteLine("Hello World!");
        }
    }

## Paralel.ForEach Döngüsü
Hızlandırmak istediğiniz bir foreach döngüsüne sahipseniz ve çıktının hangi sırada olduğunu önemsemiyorsanız, aşağıdakileri yaparak onu paralel bir foreach döngüsüne dönüştürebilirsiniz:

    using System;
    using System.Threading;
    using System.Threading.Tasks;
    
    public class MainClass {
    
        public static void Main() {
            int[] Numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            // Single-threaded
            Console.WriteLine("Normal foreach loop: ");
            foreach (var number in Numbers) {
                Console.WriteLine(longCalculation(number));
            }
            // This is the Parallel (Multi-threaded solution)
            Console.WriteLine("Parallel foreach loop: ");
            Parallel.ForEach(Numbers, number => {
                Console.WriteLine(longCalculation(number));
            });
        }
    
        private static int longCalculation(int number) {
            Thread.Sleep(1000); // Sleep to simulate a long calculation
            return number * number;
        }
    }

## Basit Komple Diş Açma Demosu
    class Program
    {
        static void Main(string[] args)
        {
            // Create 2 thread objects.  We're using delegates because we need to pass 
            // parameters to the threads.  
            var thread1 = new Thread(new ThreadStart(() => PerformAction(1)));
            var thread2 = new Thread(new ThreadStart(() => PerformAction(2)));

            // Start the threads running 
            thread1.Start();
            // NB: as soon as the above line kicks off the thread, the next line starts; 
            // even if thread1 is still processing.
            thread2.Start();

            // Wait for thread1 to complete before continuing
            thread1.Join();
            // Wait for thread2 to complete before continuing
            thread2.Join();

            Console.WriteLine("Done");
            Console.ReadKey();
        }

        // Simple method to help demonstrate the threads running in parallel.
        static void PerformAction(int id)
        {
            var rnd = new Random(id);
            for (int i = 0; i < 100; i++)
            {
                Console.WriteLine("Thread: {0}: {1}", id, i);
                Thread.Sleep(rnd.Next(0, 1000));
            }
        }
    }

## Kilitlenmeler (kaynağı tutun ve bekleyin)
Kilitlenme, iki veya daha fazla iş parçacığının birbirinin tamamlanmasını veya sonsuza kadar bekleyecek şekilde bir kaynağı serbest bırakmasını beklediğinde meydana gelir.

İş parçacığı1, A kaynağı üzerinde bir kilit tutuyorsa ve iş parçacığı 2, B kaynağını elinde tutuyor ve A kaynağının serbest bırakılmasını beklerken B kaynağının serbest bırakılmasını bekliyorsa, bunlar kilitlenir.

Aşağıdaki örnek kod için button1'i tıklamak, uygulamanızın yukarıda bahsedilen kilitlenme durumuna geçmesine ve askıda kalmasına neden olacaktır.

    private void button_Click(object sender, EventArgs e)
    {
        DeadlockWorkers workers = new DeadlockWorkers();
        workers.StartThreads();
        textBox.Text = workers.GetResult();
    }

    private class DeadlockWorkers
    {
        Thread thread1, thread2;

        object resourceA = new object();
        object resourceB = new object();

        string output;

        public void StartThreads()
        {
            thread1 = new Thread(Thread1DoWork);
            thread2 = new Thread(Thread2DoWork);
            thread1.Start();
            thread2.Start();
        }

        public string GetResult()
        {
            thread1.Join();
            thread2.Join();
            return output;
        }

        public void Thread1DoWork()
        {
            Thread.Sleep(100);
            lock (resourceA)
            {
                Thread.Sleep(100);
                lock (resourceB)
                {
                    output += "T1#";
                }
            }
        }

        public void Thread2DoWork()
        {
            Thread.Sleep(100);
            lock (resourceB)
            {
                Thread.Sleep(100);
                lock (resourceA)
                {
                    output += "T2#";
                }
            }
        }
    }

Bu şekilde kilitlenmeyi önlemek için, bir nesnede zaten bir kilidin tutulup tutulmadığını kontrol etmek için Monitor.TryEnter(lock_object, timeout_in_milisaniye) kullanılabilir. Monitor.TryEnter, timeout_in_milisaniyeden önce lock_object üzerinde bir kilit almayı başaramazsa, false döndürür, iş parçacığına diğer tutulan kaynakları serbest bırakma ve verim verme şansı verir, böylece diğer iş parçacıklarına yukarıdakilerin bu biraz değiştirilmiş versiyonunda olduğu gibi tamamlama şansı verir. :

    private void button_Click(object sender, EventArgs e)
    {
        MonitorWorkers workers = new MonitorWorkers();
        workers.StartThreads();
        textBox.Text = workers.GetResult();
    }

    private class MonitorWorkers
    {
        Thread thread1, thread2;

        object resourceA = new object();
        object resourceB = new object();

        string output;

        public void StartThreads()
        {
            thread1 = new Thread(Thread1DoWork);
            thread2 = new Thread(Thread2DoWork);
            thread1.Start();
            thread2.Start();
        }

        public string GetResult()
        {
            thread1.Join();
            thread2.Join();
            return output;
        }

        public void Thread1DoWork()
        {
            bool mustDoWork = true;
            Thread.Sleep(100);
            while (mustDoWork)
            {
                lock (resourceA)
                {
                    Thread.Sleep(100);
                    if (Monitor.TryEnter(resourceB, 0))
                    {
                        output += "T1#";
                        mustDoWork = false;
                        Monitor.Exit(resourceB);
                    }
                }
                if (mustDoWork) Thread.Yield();
            }
        }

        public void Thread2DoWork()
        {
            Thread.Sleep(100);
            lock (resourceB)
            {
                Thread.Sleep(100);
                lock (resourceA)
                {
                    output += "T2#";
                }
            }
        }
    }

Bu geçici çözümün, thread2'nin kilitleri konusunda inatçı olmasına ve thread2'nin her zaman öncelikli olacağı şekilde thread1'in boyun eğmeye istekli olmasına dayandığını unutmayın. Ayrıca thread1'in, A kaynağını kilitledikten sonra, verdiği işi yeniden yapması gerektiğini unutmayın. Bu nedenle, birden fazla verimli iş parçacığı ile bu yaklaşımı uygularken dikkatli olun, çünkü daha sonra sözde bir livelock girme riskiyle karşı karşıya kalırsınız - iki iş parçacığının çalışmalarının ilk bitini yapmaya devam etmesi ve ardından karşılıklı olarak verim vermesi durumunda ortaya çıkan bir durum. , tekrar tekrar baştan başlamak.

## İşlemci Başına Bir İş Parçacığı Oluşturma
> `Environment.ProcessorCount` Geçerli makinedeki **mantıksal** işlemcilerin sayısını alır.

CLR daha sonra her bir iş parçacığını bir mantıksal işlemciye programlayacaktır, bu teorik olarak her iş parçacığının farklı bir mantıksal işlemcide, tüm iş parçacıklarının tek bir mantıksal işlemcide veya başka bir kombinasyonda olması anlamına gelebilir.

    using System;
    using System.Threading;
    
    class MainClass {
        static void Main() {
            for (int i = 0; i < Environment.ProcessorCount; i++) {
                var thread = new Thread(Secondary);
                thread.Start(i);
            }
            
        }
    
        static void Secondary(object threadNumber) {
            System.Console.WriteLine("Hello World from thread: " + threadNumber);
        }
    }

## Görevleri Kullanan Basit Komple Diş Açma Demosu
    class Program
    {
        static void Main(string[] args)
        {
            // Run 2 Tasks.  
            var task1 = Task.Run(() => PerformAction(1)));
            var task2 = Task.Run(() => PerformAction(2)));
    
            // Wait (i.e. block this thread) until both Tasks are complete.
            Task.WaitAll(new [] { task1, task2 });
            
            Console.WriteLine("Done");
            Console.ReadKey();
        }
    
        // Simple method to help demonstrate the threads running in parallel.
        static void PerformAction(int id)
        {
            var rnd = new Random(id);
            for (int i = 0; i < 100; i++)
            {
                Console.WriteLine("Task: {0}: {1}", id, i);
                Thread.Sleep(rnd.Next(0, 1000));
            }
        }
    }

## Açık Görev Parallizmi
        private static void explicitTaskParallism()
        {
            Thread.CurrentThread.Name = "Main";

            // Create a task and supply a user delegate by using a lambda expression. 
            Task taskA = new Task(() => Console.WriteLine($"Hello from task {nameof(taskA)}."));
            Task taskB = new Task(() => Console.WriteLine($"Hello from task {nameof(taskB)}."));

            // Start the task.
            taskA.Start();
            taskB.Start();

            // Output a message from the calling thread.
            Console.WriteLine("Hello from thread '{0}'.",
                              Thread.CurrentThread.Name);
            taskA.Wait();
            taskB.Wait();
            Console.Read();
        }

## Örtülü Görev Paralelliği
        private static void Main(string[] args)
        {
            var a = new A();
            var b = new B();
            //implicit task parallelism
            Parallel.Invoke(
                () => a.DoSomeWork(),
                () => b.DoSomeOtherWork()
                );

          }

## Parametrelerle bir iş parçacığı başlatma
System.Threading kullanarak;
    
    class MainClass {
        static void Main() {
            var thread = new Thread(Secondary);
            thread.Start("SecondThread");
        }
    
        static void Secondary(object threadName) {
            System.Console.WriteLine("Hello World from thread: " + threadName);
        }
    }

## Kilitlenmeler (birbirini bekleyen iki iş parçacığı)
Kilitlenme, iki veya daha fazla iş parçacığının birbirinin tamamlanmasını veya bir kaynağı sonsuza kadar bekleyecek şekilde serbest bırakmasını beklediğinde meydana gelir.

Birbirinin tamamlanmasını bekleyen iki iş parçacığının tipik bir senaryosu, bir Windows Forms GUI iş parçacığının bir çalışan iş parçacığını beklemesi ve çalışan iş parçacığının GUI iş parçacığı tarafından yönetilen bir nesneyi çağırmaya çalışmasıdır.
Bu kod örneğinde, button1'in tıklanmasının programın askıda kalmasına neden olacağını gözlemleyin.

    private void button1_Click(object sender, EventArgs e)
    {
        Thread workerthread= new Thread(dowork);
        workerthread.Start();
        workerthread.Join();
        // Do something after
    }

    private void dowork()
    {
        // Do something before
        textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
        // Do something after
    }

'workerthread.Join()', workthread tamamlanana kadar çağıran iş parçacığını engelleyen bir çağrıdır.
`textBox1.Invoke(invoke_delegate)` GUI iş parçacığı invoke_delegate işlenene kadar çağıran iş parçacığını engelleyen bir çağrıdır, ancak GUI iş parçacığı zaten çağıran iş parçacığının tamamlanmasını bekliyorsa bu çağrı kilitlenmelere neden olur.

Bunu aşmak için, metin kutusunu çağırmak için engelleyici olmayan bir yol kullanılabilir:

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => textBox1.Text = "Some Text"));
        // Do work that is not dependent on textBox1 being updated first
    }

Ancak, önce güncellenmekte olan metin kutusuna bağlı bir kod çalıştırmanız gerekiyorsa bu soruna neden olacaktır. Bu durumda, bunu çağırmanın bir parçası olarak çalıştırın, ancak bunun GUI iş parçacığında çalışmasını sağlayacağını unutmayın.

    private void dowork()
    {
        // Do work
        textBox1.BeginInvoke(new Action(() => {
            textBox1.Text = "Some Text";
            // Do work dependent on textBox1 being updated first, 
            // start another worker thread or raise an event
        }));
        // Do work that is not dependent on textBox1 being updated first
    }

Alternatif olarak, tamamen yeni bir iş parçacığı başlatın ve işçi iş parçacığının tamamlanabilmesi için GUI iş parçacığında beklemeyi yapmasına izin verin.

    private void dowork()
    {
        // Do work
        Thread workerthread2 = new Thread(() =>
        {
            textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
            // Do work dependent on textBox1 being updated first, 
            // start another worker thread or raise an event
        });
        workerthread2.Start();
        // Do work that is not dependent on textBox1 being updated first
    }

Karşılıklı beklemenin çıkmaza girme riskini en aza indirmek için, mümkün olduğunda iş parçacıkları arasında döngüsel referanslardan daima kaçının. Daha düşük dereceli ileti dizilerinin yalnızca daha yüksek dereceli ileti dizileri için mesaj bıraktığı ve onları asla beklemediği bir ileti dizileri hiyerarşisi bu tür bir sorunla karşılaşmaz. Ancak yine de kaynak kilitlemeye dayalı kilitlenmelere karşı savunmasız olacaktır.

