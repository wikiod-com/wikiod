---
title: ".Net'te Çöp Toplayıcı"
slug: "nette-cop-toplayc"
draft: false
images: []
weight: 9876
type: docs
toc: true
---

## Zayıf Referanslar
.NET'te, GC, onlara hiçbir referans kalmadığında nesneleri tahsis eder. Bu nedenle, bir nesneye koddan hala ulaşılabilirken (buna güçlü bir referans var), GC bu nesneyi tahsis etmeyecektir. Çok sayıda büyük nesne varsa bu bir sorun haline gelebilir.

Zayıf bir referans, GC'nin nesneye erişmesine izin verirken nesneyi toplamasına izin veren bir referanstır. Zayıf bir referans, yalnızca güçlü referanslar olmadığında nesne toplanana kadar belirsiz bir süre boyunca geçerlidir. Zayıf bir başvuru kullandığınızda, uygulama yine de nesneye güçlü bir başvuru alabilir ve bu da nesnenin toplanmasını engeller. Bu nedenle, zayıf referanslar, başlatılması pahalı olan, ancak aktif olarak kullanımda değillerse çöp toplama için mevcut olması gereken büyük nesneleri tutmak için faydalı olabilir.

Basit kullanım:

    WeakReference reference = new WeakReference(new object(), false);
    
    GC.Collect();
    
    object target = reference.Target;
    if (target != null)
      DoSomething(target);

Bu nedenle, örneğin bir nesne önbelleğini korumak için zayıf referanslar kullanılabilir. Ancak, güçlü bir referans yeniden oluşturulmadan önce çöp toplayıcının nesneye ulaşma riskinin her zaman olduğunu hatırlamak önemlidir.

Zayıf referanslar, bellek sızıntılarını önlemek için de kullanışlıdır. Tipik bir kullanım durumu, olaylarla ilgilidir.

Bir kaynaktaki bir olay için bir işleyicimiz olduğunu varsayalım:

    Source.Event += new EventHandler(Handler)

Bu kod, bir olay işleyicisini kaydeder ve olay kaynağından dinleme nesnesine güçlü bir başvuru oluşturur. Kaynak nesne, dinleyiciden daha uzun bir ömre sahipse ve başka başvuru olmadığında dinleyici olaya artık ihtiyaç duymuyorsa, normal .NET olaylarının kullanılması bir bellek sızıntısına neden olur: kaynak nesne, dinleyici nesnelerini bellekte tutar. çöp toplanmalıdır.

Bu durumda, [Zayıf Olay Modeli][1] kullanmak iyi bir fikir olabilir.

Gibi bir şey:

    public static class WeakEventManager
        {
        public static void SetHandler<S, TArgs>(
        Action<EventHandler<TArgs>> add,
        Action<EventHandler<TArgs>> remove,
        S subscriber,
        Action<S, TArgs> action)
        where TArgs : EventArgs
        where S : class
            {
                var subscrWeakRef = new WeakReference(subscriber);
                EventHandler<TArgs> handler = null;
    
                handler = (s, e) =>
                {
                    var subscrStrongRef = subscrWeakRef.Target as S;
                    if (subscrStrongRef != null)
                    {
                        action(subscrStrongRef, e);
                    }
                    else
                    {
                        remove(handler);
                        handler = null;
                    }
                };
    
                add(handler);
            }
        }

Ve bunun gibi kullanılır:

     EventSource s = new EventSource();
     Subscriber subscriber = new Subscriber();
     WeakEventManager.SetHandler<Subscriber, SomeEventArgs>(a => s.Event += a, r => s.Event -= r, subscriber, (s,e) => { s.HandleEvent(e); });

Bu durumda elbette bazı kısıtlamalarımız var - olay bir

    public event EventHandler<SomeEventArgs> Event;

[MSDN][2]'nin önerdiği gibi:

- Uzun, zayıf referansları yalnızca gerektiğinde kullanın.
nesne sonlandırıldıktan sonra tahmin edilemez.
- Küçük nesnelere zayıf referanslar kullanmaktan kaçının çünkü işaretçi
kendisi kadar büyük veya daha büyük olabilir.
- Hafızaya otomatik bir çözüm olarak zayıf referansları kullanmaktan kaçının
yönetim sorunları. Bunun yerine, etkili bir önbelleğe alma politikası geliştirin.
uygulamanızın nesnelerini işleme.


[1]: https://msdn.microsoft.com/en-us/library/aa970850(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/ms404247(v=vs.110).aspx#anchor_1

## büyük nesne yığın sıkıştırma
Varsayılan olarak Büyük Nesne Yığını, [bellek parçalanmasına yol açabilen][1] ve dahası, `OutOfMemoryException`lara yol açabilen klasik Object Heap'in aksine sıkıştırılmaz.

.NET 4.5.1 ile başlayarak, Büyük Nesne Yığınını (çöp toplamayla birlikte) açıkça sıkıştırmak için [bir seçenek][2] vardır:

    GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce;
    GC.Collect();   

Tıpkı herhangi bir açık çöp toplama talebinin (CLR bunu yürütmek zorunda olmadığı için istek olarak adlandırılır) dikkatli kullanın ve `GC` istatistiklerini kalibre ederek performansını düşürebileceğinden, mümkünse varsayılan olarak bundan kaçının.

[1]: https://www.simple-talk.com/dotnet/.net-sramework/the-dangers-of-large-sacject-heap/
[2]: https://msdn.microsoft.com/en-us/library/system.runtime.gcsettings.largeobjectheapcompactionmode(v=vs.110).aspx

