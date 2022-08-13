---
title: "Kilit Bildirimi"
slug: "kilit-bildirimi"
draft: false
images: []
weight: 9886
type: docs
toc: true
---

## Sözdizimi
* kilit (nesne) {}

'lock' ifadesini kullanarak, kod bloğu içindeki koda farklı iş parçacıklarının erişimini kontrol edebilirsiniz. Genellikle yarış koşullarını önlemek için kullanılır, örneğin birden çok iş parçacığının bir koleksiyondan öğeleri okuması ve kaldırması gibi. Kilitleme, iş parçacıklarını diğer iş parçacıklarının bir kod bloğundan çıkmasını beklemeye zorladığından, diğer eşitleme yöntemleriyle çözülebilecek gecikmelere neden olabilir.

MSDN

> Kilit anahtar sözcüğü, bir ifade bloğunu kritik bir bölüm olarak işaretler.
> belirli bir nesne için karşılıklı dışlama kilidinin elde edilmesi, bir
> ifadesi ve ardından kilidi serbest bırakın.
> 
> Kilit anahtar sözcüğü, bir iş parçacığının kritik bir değere girmemesini sağlar.
> başka bir iş parçacığı kritik bölümdeyken kod bölümü. Eğer
> başka bir iş parçacığı kilitli bir kod girmeye çalışır, bekler, engeller,
> nesne serbest bırakılıncaya kadar.
> 
> En iyi uygulama, kilitlenecek bir **özel** nesne veya **özel bir nesne tanımlamaktır.
> tüm örneklerde ortak olan verileri korumak için statik** nesne değişkeni.

<saat>

C# 5.0 ve sonraki sürümlerde, "lock" ifadesi şuna eşdeğerdir:

    bool lockTaken = false;
    try 
    {
        System.Threading.Monitor.Enter(refObject, ref lockTaken);
        // code 
    }
    finally 
    {
        if (lockTaken)
            System.Threading.Monitor.Exit(refObject);
    }

C# 4.0 ve öncesi için, "lock" ifadesi şuna eşdeğerdir:

    System.Threading.Monitor.Enter(refObject);
    try 
    {
        // code
    }
    finally 
    {
         System.Threading.Monitor.Exit(refObject);
    }

## Bir kilit ifadesinde istisna atma
Aşağıdaki kod kilidi serbest bırakacaktır. Sorun olmayacak. Perde arkasında kilit ifadesi 'nihayet dene' olarak çalışır

    lock(locker)
    {
        throw new Exception();
    }

Daha fazlası [C# 5.0 Spesifikasyonunda][1] görülebilir:

Formun bir "kilit" ifadesi

    lock (x) ...

burada "x" bir *referans tipinin* ifadesidir, tam olarak şuna eşdeğerdir
    
    bool __lockWasTaken = false;
    try {
        System.Threading.Monitor.Enter(x, ref __lockWasTaken);
        ...
    }
    finally {
        if (__lockWasTaken) System.Threading.Monitor.Exit(x);
    }

bunun dışında "x" yalnızca bir kez değerlendirilir.


[1]: https://msdn.microsoft.com/en-us/library/aa664735%28VS.71%29.aspx?f=255&MSPPError=-2147217396

## Basit kullanım
"Kilit"in yaygın kullanımı kritik bir bölümdür.

Aşağıdaki örnekte 'ReserveRoom'un farklı evrelerden çağrılması gerekiyor. 'Lock' ile senkronizasyon, burada yarış durumunu önlemenin en basit yoludur. Yöntem gövdesi, iki veya daha fazla iş parçacığının aynı anda yürütememesini sağlayan "kilit" ile çevrilidir.
 
    public class Hotel
    {
        private readonly object _roomLock = new object();

        public void ReserveRoom(int roomNumber)
        {
            // lock keyword ensures that only one thread executes critical section at once
            // in this case, reserves a hotel room of given number
            // preventing double bookings
            lock (_roomLock)
            {
                // reserve room logic goes here
            }
        }
    }

Bir iş parçacığı, içinde başka bir iş parçacığı çalışırken "kilitli" bloğa ulaşırsa, önceki bloktan çıkmak için başka bir iş parçacığı bekler.

> En iyi uygulama, kilitlenecek özel bir nesne veya özel bir nesne tanımlamaktır.
> tüm örneklerde ortak olan verileri korumak için statik nesne değişkeni.

## Kilit deyiminde dön
Aşağıdaki kod kilidi serbest bırakacaktır.

    lock(locker)
    {
        return 5;
    }

Ayrıntılı bir açıklama için [bu SO yanıtı][1] önerilir.


[1]: http://stackoverflow.com/a/266718/1519458

## Anti-Desenler ve yakalamalar
# Yığına ayrılmış/yerel bir değişkene kilitleme

'Kilit' kullanırken yapılan hatalardan biri, yerel nesnelerin bir işlevde dolap olarak kullanılmasıdır. Bu yerel nesne örnekleri, işlevin her çağrısında farklılık göstereceğinden, "kilit" beklendiği gibi çalışmayacaktır.

    List<string> stringList = new List<string>();

    public void AddToListNotThreadSafe(string something)
    {
        // DO NOT do this, as each call to this method 
        // will lock on a different instance of an Object.
        // This provides no thread safety, it only degrades performance.
        var localLock = new Object();
        lock(localLock)
        {
            stringList.Add(something);
        }
    }

    // Define object that can be used for thread safety in the AddToList method
    readonly object classLock = new object();

    public void AddToList(List<string> stringList, string something)
    {
        // USE THE classLock instance field to achieve a 
        // thread-safe lock before adding to stringList
        lock(classLock)
        {
            stringList.Add(something);
        }
    }

# Kilitlemenin, senkronizasyon nesnesinin kendisine erişimi kısıtladığını varsayarsak

Bir iş parçacığı: `lock(obj)` ve başka bir iş parçacığı `obj.ToString()` çağırırsa, ikinci iş parçacığı engellenmeyecektir.

    object obj = new Object();
     
    public void SomeMethod()
    {
         lock(obj)
        {
           //do dangerous stuff 
        }
     }

     //Meanwhile on other tread 
     public void SomeOtherMethod()
     {
       var objInString = obj.ToString(); //this does not block
     }

# Alt sınıfların ne zaman kilitleneceğini bilmesini beklemek

Bazen temel sınıflar, belirli korunan alanlara erişirken alt sınıflarının bir kilit kullanması gerekecek şekilde tasarlanır:

    public abstract class Base
    {
        protected readonly object padlock;
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public abstract void Do();
    }

    public class Derived1 : Base
    {
        public override void Do()
        {
            lock (this.padlock)
            {
                this.list.Add("Derived1");
            }
        }
    }

    public class Derived2 : Base
    {
        public override void Do()
        {
            this.list.Add("Derived2"); // OOPS! I forgot to lock!
        }
    }

Bir [Şablon Yöntemi][3] kullanarak kilitlemeyi *kapsüllemek* çok daha güvenlidir:

    public abstract class Base
    {
        private readonly object padlock; // This is now private
        protected readonly List<string> list;

        public Base()
        {
            this.padlock = new object();
            this.list = new List<string>();
        }

        public void Do()
        {
            lock (this.padlock) {
                this.DoInternal();
            }
        }

        protected abstract void DoInternal();
    }

    public class Derived1 : Base
    {
        protected override void DoInternal()
        {
            this.list.Add("Derived1"); // Yay! No need to lock
        }
    }

# Kutulu bir ValueType değişkeni üzerinde kilitleme senkronize edilmiyor

Aşağıdaki örnekte, bir işleve bir "nesne" argümanı olarak sağlandığı ve bir izleme kaynağının kilitlenmesini beklediği için özel bir değişken örtük olarak kutulanmıştır.
Kutulama, IncInSync işlevi çağrılmadan hemen önce gerçekleşir, bu nedenle kutulu örnek, işlev her çağrıldığında farklı bir yığın nesnesine karşılık gelir.

    public int Count { get; private set; }

    private readonly int counterLock = 1;
    
    public void Inc()
    {
        IncInSync(counterLock);
    }

    private void IncInSync(object monitorResource)
    {
        lock (monitorResource)
        {
            Count++;
        }
    }

Boks, "Inc" işlevinde gerçekleşir:

    BulemicCounter.Inc:
    IL_0000:  nop         
    IL_0001:  ldarg.0     
    IL_0002:  ldarg.0     
    IL_0003:  ldfld       UserQuery+BulemicCounter.counterLock
    IL_0008:  box         System.Int32**
    IL_000D:  call        UserQuery+BulemicCounter.IncInSync
    IL_0012:  nop         
    IL_0013:  ret         

Bu, kutulu bir ValueType'ın monitör kilitleme için hiçbir şekilde kullanılamayacağı anlamına gelmez:

    private readonly object counterLock = 1;

Şimdi, kilitleme için iyi olan yapıcıda boks gerçekleşir:

    IL_0001:  ldc.i4.1    
    IL_0002:  box         System.Int32
    IL_0007:  stfld       UserQuery+BulemicCounter.counterLock

# Daha güvenli bir alternatif varken gereksiz yere kilit kullanmak

Çok yaygın bir kalıp, güvenli bir iş parçacığı sınıfında özel bir "Liste" veya "Sözlük" kullanmak ve her erişildiğinde kilitlemektir:

    public class Cache
    {
        private readonly object padlock;
        private readonly Dictionary<string, object> values;

        public WordStats()
        {
            this.padlock = new object();
            this.values = new Dictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            lock (this.padlock)
            {
                this.values.Add(key, value);
            }
        }

        /* rest of class omitted */
    }

"Değerler" sözlüğüne erişmek için birden fazla yöntem varsa, kod çok uzayabilir ve daha da önemlisi, kilitleme her zaman *niyetini* gizler. Kilitlemeyi unutmak da çok kolaydır ve uygun kilitleme eksikliği, hataların bulunmasını çok zorlaştırabilir.

Bir [`ConcurrentDictionary`][1] kullanarak, tamamen kilitlemekten kaçınabiliriz:

    public class Cache
    {
        private readonly ConcurrentDictionary<string, object> values;

        public WordStats()
        {
            this.values = new ConcurrentDictionary<string, object>();
        }
        
        public void Add(string key, object value)
        {
            this.values.Add(key, value);
        }

        /* rest of class omitted */
    }

Eşzamanlı koleksiyonları kullanmak, performansı da artırır, çünkü [hepsi kilitsiz teknikler [2] kullanır] bir dereceye kadar.

[1]: https://msdn.microsoft.com/en-us/library/dd287191%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396
[2]: https://blogs.msdn.microsoft.com/pfxteam/2010/01/26/faq-are-all-of-the-new-concurrent-collections-lock-free/
[3]: https://en.wikipedia.org/wiki/Template_method_pattern

## Kilit için Object örneklerini kullanma
C#'ın yerleşik "kilit" ifadesini kullanırken, bir tür örneğe ihtiyaç duyulur, ancak durumu önemli değildir. Bir "nesne" örneği bunun için mükemmeldir:

    public class ThreadSafe {
      private static readonly object locker = new object();


      public void SomeThreadSafeMethod() {
        lock (locker) {
          // Only one thread can be here at a time.
        }
      }
    }

**NOT**. 'Type' örnekleri bunun için kullanılmamalıdır (yukarıdaki 'typeof(ThreadSafe)' kodunda) çünkü 'Type' örnekleri AppDomains arasında paylaşılır ve bu nedenle kilidin kapsamının içermemesi gereken kodu içermesi beklenir (örn. . "ThreadSafe" aynı işlemde iki AppDomain'e yüklenirse, "Type" örneğini kilitlemek karşılıklı olarak kilitlenir).

