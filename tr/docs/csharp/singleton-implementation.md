---
title: "Tekil Uygulama"
slug: "tekil-uygulama"
draft: false
images: []
weight: 9710
type: docs
toc: true
---

## Statik Olarak Başlatılmış Singleton
    public class Singleton
    {
        private readonly static Singleton instance = new Singleton();
        private Singleton() { }
        public static Singleton Instance => instance;
    }

Bu uygulama iş parçacığı için güvenlidir, çünkü bu durumda "örnek" nesnesi statik oluşturucuda başlatılır. CLR, zaten tüm statik oluşturucuların iş parçacığı güvenli yürütülmesini sağlar.

"Örneği" değiştirmek, iş parçacığı için güvenli bir işlem değildir, bu nedenle "salt okunur" özniteliği, başlatmadan sonra değişmezliği garanti eder.

## Tembel, iş parçacığı güvenli Singleton (Lazy<T> kullanarak)
.Net 4.0 tipi Lazy<T>, iş parçacığı güvenli nesne başlatmayı garanti eder, bu nedenle bu tür Singletons yapmak için kullanılabilir.


    public class LazySingleton
    {
        private static readonly Lazy<LazySingleton> _instance =
            new Lazy<LazySingleton>(() => new LazySingleton());
     
        public static LazySingleton Instance
        {
            get { return _instance.Value; }
        }

        private LazySingleton() { }
    }

Tembel<T>'yi kullanmak, nesnenin yalnızca çağrı kodunda bir yerde kullanıldığında somutlaştırılmasını sağlar.

Basit bir kullanım şöyle olacaktır:

    using System;
                        
    public class Program
    {
        public static void Main()
        {
            var instance = LazySingleton.Instance;
        }
    }

[.NET Fiddle'da Canlı Demo][1]

[1]: https://dotnetfiddle.net/oHVpK3

## Tembel, iş parçacığı güvenli Singleton (Çift Kontrollü Kilitleme kullanarak)
Bir singleton'ın bu iş parçacığı güvenli sürümü, 'statik' başlatmanın iş parçacığı için güvenli olmasının garanti edilmediği ilk .NET sürümlerinde gerekliydi. Çerçevenin daha modern sürümlerinde, çok kolay olduğu için bir [statik olarak başlatılmış singleton](https://www.wikiod.com/tr/docs/c%23/1192/singleton-implementation/3863/statically-initialized-singleton) genellikle tercih edilir. aşağıdaki modelde uygulama hataları yapmak.

    public sealed class ThreadSafeSingleton
    {
       private static volatile ThreadSafeSingleton instance;
       private static object lockObject = new Object();
    
       private ThreadSafeSingleton()
       {
       }
    
       public static ThreadSafeSingleton Instance
       {
          get 
          {
             if (instance == null) 
             {
                lock (lockObject) 
                {
                   if (instance == null)
                   {
                      instance = new ThreadSafeSingleton();
                   }
                }
             }
    
             return instance;
          }
       }
    }

`if (instance == null)` kontrolünün iki kez yapıldığına dikkat edin: kilit alınmadan önce ve sonra. Bu uygulama, ilk boş denetim olmadan bile iş parçacığı açısından güvenli olacaktır. Ancak bu, örneğin her istendiğinde * bir kilidin alınacağı ve bu da performansın düşmesine neden olacağı anlamına gelir. İlk boş kontrol, gerekli olmadıkça kilidin alınmaması için eklenir. İkinci boş denetim, yalnızca kilidi alan ilk iş parçacığının örneği oluşturmasını sağlar. Diğer iş parçacıkları doldurulacak örneği bulacak ve ileri atlayacaktır.

## Tembel, iş parçacığı güvenli singleton (.NET 3.5 veya daha eski, alternatif uygulama için)
.NET 3.5 ve daha eski sürümlerde [`Lazy<T>`][1] sınıfınız olmadığı için aşağıdaki kalıbı kullanırsınız:

    public class Singleton
    {
        private Singleton() // prevents public instantiation
        {
        }
    
        public static Singleton Instance
        {
            get
            {
                return Nested.instance;
            }
        }
        
        private class Nested
        {
            // Explicit static constructor to tell C# compiler
            // not to mark type as beforefieldinit
            static Nested()
            {
            }
    
            internal static readonly Singleton instance = new Singleton();
        }
    }

Bu, [Jon Skeet'in blog gönderisinden[2] esinlenilmiştir.

"İç içe" sınıf iç içe ve özel olduğundan, tekil örneğinin somutlaştırılması, "Sigleton" sınıfının diğer üyelerine (örneğin, genel salt okunur bir özellik gibi) erişilerek tetiklenmeyecektir.


[1]: https://msdn.microsoft.com/en-us/library/dd642331(v=vs.110).aspx
[2]: http://www.yoda.arachsys.com/csharp/singleton.html

## Singleton örneğinin artık gerekmediğinde elden çıkarılması
Çoğu örnek, bir 'LazySingleton' nesnesinin, uygulama tarafından artık ihtiyaç duyulmasa bile, sahip olan uygulama sonlandırılana kadar somutlaştırılmasını ve tutulmasını gösterir. Bunun bir çözümü, "IDisposable" öğesini uygulamak ve nesne örneğini aşağıdaki gibi null değerine ayarlamaktır:

    public class LazySingleton : IDisposable
    {
        private static volatile Lazy<LazySingleton> _instance;
        private static volatile int _instanceCount = 0;
        private bool _alreadyDisposed = false;
 
    public static LazySingleton Instance
    {
        get
        {
            if (_instance == null)
                _instance = new Lazy<LazySingleton>(() => new LazySingleton());
            _instanceCount++;
            return _instance.Value;
        }
    }

    private LazySingleton() { }

    // Public implementation of Dispose pattern callable by consumers.
    public void Dispose()
    { 
        if (--_instanceCount == 0) // No more references to this object.
        {       
           Dispose(true);
           GC.SuppressFinalize(this);           
        }
    }
   
    // Protected implementation of Dispose pattern.
    protected virtual void Dispose(bool disposing)
    {
        if (_alreadyDisposed) return; 
      
        if (disposing) 
        {
            _instance = null; // Allow GC to dispose of this instance.
            // Free any other managed objects here.
        }
      
        // Free any unmanaged objects here.
        _alreadyDisposed = true;
    }

Yukarıdaki kod, örneği uygulama sonlandırmadan önce yok eder, ancak yalnızca tüketiciler her kullanımdan sonra nesne üzerinde 'Dispose()'u çağırırsa. Bunun olacağına dair bir garanti veya zorlamanın bir yolu olmadığı için, örneğin hiçbir zaman elden çıkarılacağına dair bir garanti de yoktur. Ancak bu sınıf dahili olarak kullanılıyorsa, her kullanımdan sonra `Dispose()` yönteminin çağrılmasını sağlamak daha kolaydır. Bir örnek aşağıdaki gibidir:

    public class Program
    {
        public static void Main()
        {
            using (var instance = LazySingleton.Instance)
            {
                // Do work with instance
            }
        }
    }

Lütfen bu örneğin **iş parçacığı için güvenli olmadığını** unutmayın.

