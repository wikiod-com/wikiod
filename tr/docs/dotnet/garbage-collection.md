---
title: "Çöp toplama"
slug: "cop-toplama"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

.Net'te, new() ile oluşturulan nesneler yönetilen öbek üzerinde tahsis edilir. Bu nesneler, onları kullanan program tarafından hiçbir zaman açıkça sonlandırılmaz; bunun yerine bu işlem .Net Garbage Collector tarafından kontrol edilir.

Aşağıdaki örneklerden bazıları, Çöp Toplayıcıyı iş başında ve davranışının bazı önemli ayrıntılarını gösteren "laboratuvar vakaları" iken, diğerleri, Çöp Toplayıcı tarafından uygun şekilde ele alınması için sınıfların nasıl hazırlanacağına odaklanır.

Çöp Toplayıcı, ayrılan bellek açısından program maliyetini düşürmeyi amaçlar, ancak bunu yapmanın işlem süresi açısından bir maliyeti vardır. İyi bir genel uzlaşma elde etmek için, Garbage Collector ile programlama yaparken dikkate alınması gereken bir dizi optimizasyon vardır:

- Collect() yöntemi açıkça çağrılacaksa (ki bu çoğu zaman böyle olmamalıdır), ölü nesneyi yalnızca belleğe gerçekten ihtiyaç duyulduğunda sonlandıran "optimize edilmiş" modu kullanmayı düşünün.
- Collect() yöntemini çağırmak yerine, yalnızca gerçekten ihtiyaç duyulduğunda bir bellek koleksiyonunu tetikleyen AddMemoryPressure() ve RemoveMemoryPressure() yöntemlerini kullanmayı düşünün.
- Bir bellek koleksiyonunun tüm ölü nesneleri sonlandıracağı garanti edilmez; bunun yerine, Çöp Toplayıcı 3 "nesli" yönetir, bazen bir nesilden diğerine "hayatta kalan" bir nesne
- Kurulum ince ayarı da dahil olmak üzere çeşitli faktörlere bağlı olarak çeşitli diş açma modelleri geçerli olabilir ve bu da Çöp Toplayıcı iş parçacığı ile diğer uygulama iş parçacığı(lar) arasında farklı derecelerde girişime neden olabilir.

  

## Temel bir (çöp) toplama örneği
Aşağıdaki sınıf göz önüne alındığında:

    public class FinalizableObject 
    {
        public FinalizableObject()
        {
            Console.WriteLine("Instance initialized");
        }

        ~FinalizableObject()
        {
            Console.WriteLine("Instance finalized");
        }
    }
Kullanmadan bile bir örnek oluşturan bir program:

    new FinalizableObject(); // Object instantiated, ready to be used
Aşağıdaki çıktıyı üretir:

    <namespace>.FinalizableObject initialized
Başka hiçbir şey olmazsa, program sona erene kadar nesne sonlandırılmaz (yönetilen öbek üzerindeki tüm nesneleri serbest bırakır, bunları işlemde sonlandırır).

Çöp Toplayıcıyı belirli bir noktada aşağıdaki şekilde çalışmaya zorlamak mümkündür:

    new FinalizableObject(); // Object instantiated, ready to be used
    GC.Collect();
Hangisi aşağıdaki sonucu üretir:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
Bu sefer, Çöp Toplayıcı çağrıldığı anda kullanılmayan (diğer adıyla "ölü") nesne sonlandırıldı ve yönetilen yığından kurtarıldı.

## Canlı nesneler ve ölü nesneler - temel bilgiler
Temel kural: çöp toplama gerçekleştiğinde, "canlı nesneler" hala kullanımda olanlardır, "ölü nesneler" ise artık kullanılmayanlardır (varsa, bunlara başvuran herhangi bir değişken veya alan, toplama gerçekleşmeden önce kapsam dışına çıkmıştır) .

Aşağıdaki örnekte (kolaylık olması açısından, FinalizableObject1 ve FinalizableObject2, yukarıdaki örnekteki FinalizableObject öğesinin alt sınıflarıdır ve bu nedenle başlatma/sonlandırma mesajı davranışını devralır):

    var obj1 = new FinalizableObject1(); // Finalizable1 instance allocated here
    var obj2 = new FinalizableObject2(); // Finalizable2 instance allocated here
    obj1 = null; // No more references to the Finalizable1 instance 
    GC.Collect();
Çıktı olacaktır:

    <namespace>.FinalizableObject1 initialized
    <namespace>.FinalizableObject2 initialized
    <namespace>.FinalizableObject1 finalized
Çöp Toplayıcı çağrıldığında, FinalizableObject1 ölü bir nesnedir ve sonlandırılır, FinalizableObject2 ise canlı bir nesnedir ve yönetilen öbek üzerinde tutulur.

## Birden çok ölü nesne
Ya iki (veya birkaç) aksi halde ölü nesne birbirine referans verirse? Bu, OtherObject öğesinin FinalizableObject öğesinin genel bir özelliği olduğunu varsayarak aşağıdaki örnekte gösterilmiştir:

    var obj1 = new FinalizableObject1(); 
    var obj2 = new FinalizableObject2();
    obj1.OtherObject = obj2;
    obj2.OtherObject = obj1;
    obj1 = null; // Program no longer references Finalizable1 instance
    obj2 = null; // Program no longer references Finalizable2 instance
    // But the two objects still reference each other
    GC.Collect();
Bu, aşağıdaki çıktıyı üretir:

    <namespace>.FinalizedObject1 initialized
    <namespace>.FinalizedObject2 initialized
    <namespace>.FinalizedObject1 finalized
    <namespace>.FinalizedObject2 finalized
İki nesne sonlandırılır ve birbirlerine başvuruda bulunmalarına rağmen yönetilen yığından kurtarılır (çünkü gerçekte canlı bir nesneden hiçbirine başka başvuru yoktur).

## Zayıf Referanslar
Zayıf referanslar... diğer nesnelere yapılan referanslardır (diğer adıyla "hedefler"), ancak bu nesnelerin çöp olarak toplanmasını engellemedikleri için "zayıftır". Diğer bir deyişle, Çöp Toplayıcı nesneleri "canlı" veya "ölü" olarak değerlendirdiğinde zayıf referanslar sayılmaz.

Aşağıdaki kod:

    var weak = new WeakReference<FinalizableObject>(new FinalizableObject());
    GC.Collect();
Çıktıyı üretir:

    <namespace>.FinalizableObject initialized
    <namespace>.FinalizableObject finalized
WeakReference değişkeni tarafından başvurulmasına rağmen nesne yönetilen öbekten kurtarılır (Çöp toplayıcı çağrıldığında hala kapsam dahilindedir).

Sonuç #1: Herhangi bir zamanda, yönetilen öbek üzerinde bir WeakReference hedefinin hala tahsis edilip edilmediğini varsaymak güvenli değildir.

Sonuç #2: bir programın bir Zayıf Referansın hedefine erişmesi gerektiğinde, hedefin hala tahsis edilip edilmediğine dair her iki durum için de kod sağlanmalıdır. Hedefe erişme yöntemi TryGetTarget'tır:
 
    var target = new object(); // Any object will do as target
    var weak = new WeakReference<object>(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if(weak.TryGetTarget(out target))
    {
        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }

WeakReference'ın genel sürümü .Net 4.5'ten beri mevcuttur. Tüm çerçeve sürümleri, aynı şekilde oluşturulmuş ve aşağıdaki gibi kontrol edilen genel olmayan, türlenmemiş bir sürüm sağlar:

    var target = new object(); // Any object will do as target
    var weak = new WeakReference(target); // Create weak reference
    target = null; // Drop strong reference to the target

    // ... Many things may happen in-between

    // Check whether the target is still available
    if (weak.IsAlive)
    {
        target = weak.Target;

        // Use re-initialized target variable
        // To do whatever the target is needed for
    }
    else
    {
        // Do something when there is no more target object
        // The target variable value should not be used here
    }


  

## Dispose() ve sonlandırıcılar
Nesne artık kullanılmadığı anda bellek açısından yoğun kaynakların serbest bırakılmasını sağlamak için Dispose() yöntemini uygulayın (ve içeren sınıfı IDisposable olarak bildirin). "Yakalama", Dispose() yönteminin hiçbir zaman çağrılacağının güçlü bir garantisinin olmamasıdır (nesnenin ömrünün sonunda her zaman çağrılan sonlandırıcıların aksine).

Bir senaryo, açıkça oluşturduğu nesneler üzerinde Dispose() öğesini çağıran bir programdır:

    private void SomeFunction()
    {
        // Initialize an object that uses heavy external resources
        var disposableObject = new ClassThatImplementsIDisposable();

        // ... Use that object

        // Dispose as soon as no longer used
        disposableObject.Dispose();

        // ... Do other stuff 

        // The disposableObject variable gets out of scope here
        // The object will be finalized later on (no guarantee when)
        // But it no longer holds to the heavy external resource after it was disposed
    }


Başka bir senaryo, çerçeve tarafından somutlaştırılacak bir sınıf ilan etmektir. Bu durumda, yeni sınıf genellikle bir temel sınıfı devralır, örneğin MVC'de, System.Web.Mvc.ControllerBase'in bir alt sınıfı olarak bir denetleyici sınıfı oluşturur. Temel sınıf IDisposable arabirimini uyguladığında, bu, Dispose()'nin çerçeve tarafından düzgün bir şekilde çağrılacağına dair iyi bir ipucudur - ancak yine de güçlü bir garanti yoktur.

Bu nedenle Dispose() bir sonlandırıcının yerini tutmaz; bunun yerine, ikisi farklı amaçlar için kullanılmalıdır:

- Sonlandırıcı, aksi takdirde meydana gelebilecek bellek sızıntılarını önlemek için kaynakları eninde sonunda serbest bırakır.
- Dispose(), genel bellek tahsisi üzerindeki baskıyı hafifletmek için, artık ihtiyaç kalmadığı anda kaynakları (muhtemelen aynı olanları) serbest bırakır.

## Nesnelerin uygun şekilde elden çıkarılması ve sonlandırılması
Dispose() ve sonlandırıcılar farklı amaçlara yönelik olduğundan, harici bellek ağırlıklı kaynakları yöneten bir sınıf her ikisini de uygulamalıdır. Sonuç, sınıfı iki olası senaryoyu iyi idare edecek şekilde yazmaktır:

- Yalnızca sonlandırıcı çağrıldığında
- Dispose() önce ve sonra çağrıldığında, sonlandırıcı da çağrılır

Bir çözüm, temizleme kodunu, bir veya iki kez çalıştırmanın yalnızca bir kez çalıştırmayla aynı sonucu üreteceği şekilde yazmaktır. Fizibilite, temizliğin doğasına bağlıdır, örneğin:
- Zaten kapalı bir veritabanı bağlantısını kapatmanın muhtemelen hiçbir etkisi olmaz, bu yüzden çalışır
- Bazı "kullanım sayısını" güncellemek tehlikelidir ve bir kez yerine iki kez çağrıldığında yanlış sonuç verir.

Daha güvenli bir çözüm, tasarım gereği, dış bağlam ne olursa olsun temizleme kodunun yalnızca bir kez çağrılmasını sağlamaktır. Bu, özel bir bayrak kullanılarak "klasik yoldan" elde edilebilir:

    public class DisposableFinalizable1: IDisposable
    {
        private bool disposed = false;

        ~DisposableFinalizable1() { Cleanup(); }

        public void Dispose() { Cleanup(); }

        private void Cleanup()
        {
            if(!disposed)
            {
                // Actual code to release resources gets here, then
                disposed = true;
            }
        }
    }


Alternatif olarak, Çöp Toplayıcı, Dispose çağrıldıktan sonra sonlandırıcının atlanmasına izin veren belirli bir SuppressFinalize() yöntemi sağlar:

    public class DisposableFinalizable2 : IDisposable
    {
        ~DisposableFinalizable2() { Cleanup(); }

        public void Dispose()
        {
            Cleanup();
            GC.SuppressFinalize(this);
        }

        private void Cleanup()
        {
            // Actual code to release resources gets here
        }
    }


 

