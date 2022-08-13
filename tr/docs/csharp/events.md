---
title: "Olaylar"
slug: "olaylar"
draft: false
images: []
weight: 9780
type: docs
toc: true
---

Olay, bir şeyin meydana geldiğine (fare tıklaması gibi) veya bazı durumlarda (fiyat değişikliği gibi) gerçekleşmek üzere olduğuna dair bir bildirimdir.

Sınıflar olayları tanımlayabilir ve onların örnekleri (nesneler) bu olayları gündeme getirebilir. Örneğin, bir Düğme, bir kullanıcı tıkladığında ortaya çıkan bir Click olayı içerebilir.

Olay işleyicileri, karşılık gelen olay ortaya çıktığında çağrılan yöntemlerdir. Örneğin, bir form içerdiği her Düğme için bir Clicked olay işleyicisi içerebilir.

## Parametreler
| parametre | Ayrıntılar |
| --------- | ------- |  
| EventArgsT | EventArgs'tan türetilen ve olay parametrelerini içeren tür. |
| EtkinlikAdı | Etkinliğin adı. |
| İşleyiciAdı | Olay işleyicisinin adı. |
| GönderenNesne | Olayı çağıran nesne. |
| OlayArgümanları | Olay parametrelerini içeren EventArgsT türünün bir örneği.|

Bir etkinliği yükseltirken:

* Daima temsilcinin "null" olup olmadığını kontrol edin. Boş temsilci, etkinliğin abonesi olmadığı anlamına gelir. Abonesi olmayan bir olayı yükseltmek, bir "NullReferenceException" ile sonuçlanacaktır.

<!-- eğer sürüm [lt 6.0] -->
* Etkinliği null olup olmadığını kontrol etmeden / yükseltmeden önce temsilciyi (ör. "EventName") yerel bir değişkene (ör. "eventName") kopyalayın. Bu, çok iş parçacıklı ortamlarda yarış koşullarını önler:

**Yanlış**:

        if(Changed != null)      // Changed has 1 subscriber at this point
                                 // In another thread, that one subscriber decided to unsubscribe
            Changed(this, args); // `Changed` is now null, `NullReferenceException` is thrown.

**Doğru**:

        // Cache the "Changed" event as a local. If it is not null, then use
        // the LOCAL variable (handler) to raise the event, NOT the event itself.
        var handler = Changed;
        if(handler != null)
            handler(this, args);
<!-- eğer --> son sürüm

<!-- eğer sürüm [gt 6.0] -->
* Bir 'if' ifadesinde aboneler için temsilciyi boş denetlemek yerine yöntemi yükseltmek için boş koşullu işleci (?.) kullanın: 'EventName?.Invoke(SenderObject, new EventArgsT());'
<!-- eğer --> son sürüm

* Temsilci türlerini bildirmek için Eylem<> kullanılırken, anonim yöntem/olay işleyici imzası, olay bildiriminde bildirilen anonim temsilci türüyle aynı olmalıdır.

## Olayları Bildirme ve Yükseltme
## Etkinlik Bildirme

Aşağıdaki sözdizimini kullanarak herhangi bir "sınıf" veya "yapı" üzerinde bir olay bildirebilirsiniz:

    public class MyClass
    {
        // Declares the event for MyClass
        public event EventHandler MyEvent;

        // Raises the MyEvent event
        public void RaiseEvent()
        {
            OnMyEvent();
        }
    }    

Özel bir örneğini tuttuğunuz olayları bildirmek için genişletilmiş bir sözdizimi vardır.
olay ve "add" ve "set" erişimcilerini kullanarak genel bir örnek tanımlayın. Sözdizimi C# özelliklerine çok benzer. Her durumda, yukarıda gösterilen sözdizimi tercih edilmelidir, çünkü derleyici, sınıfınızdaki olaya birden çok iş parçacığının olay işleyicilerini güvenli bir şekilde ekleyip kaldırabilmesini sağlamak için kod yayar.

## Etkinliği Yükseltmek

<!-- eğer [gte 6.0] versiyonu -->
    
    private void OnMyEvent()
    {
        EventName?.Invoke(this, EventArgs.Empty); 
    }
<!-- eğer --> son sürüm
<!-- eğer sürüm [lt 6.0] -->
    
    private void OnMyEvent()
    {
        // Use a local for EventName, because another thread can modify the
        // public EventName between when we check it for null, and when we
        // raise the event.
        var eventName = EventName;

        // If eventName == null, then it means there are no event-subscribers,
        // and therefore, we cannot raise the event.
        if(eventName != null)
            eventName(this, EventArgs.Empty);
    
    }
<!-- eğer --> son sürüm

Olayların yalnızca bildiren tür tarafından oluşturulabileceğini unutmayın. Müşteriler yalnızca abone olabilir/abonelikten çıkabilir.

'EventName?.Invoke'un desteklenmediği 6.0'dan önceki C# sürümleri için, örnekte gösterildiği gibi, olayı çağırmadan önce geçici bir değişkene atamak iyi bir uygulamadır ve bu, birden çok iş parçacığının yürütüldüğü durumlarda iş parçacığı güvenliğini sağlar. aynı kod. Bunu yapmamak, birden çok iş parçacığının aynı nesne örneğini kullandığı belirli durumlarda bir "NullReferenceException" oluşturulmasına neden olabilir. C# 6.0'da derleyici, C# 6 için kod örneğinde gösterilene benzer bir kod yayar.


## İptal edilebilir etkinlik oluşturma
[`FormClosing`](https://msdn.microsoft.com/en-us/library/system.windows) gibi iptal edilebilecek bir eylemi gerçekleştirmek üzereyken bir sınıf tarafından iptal edilebilir bir olay oluşturulabilir. Bir [`Form`](https://msdn.microsoft.com/en-us/library/system.windows.forms.form(v) .forms.form.formclosing(v=vs.110).aspx) olayı =vs.110).aspx).

Böyle bir etkinlik oluşturmak için:

- [`CancelEventArgs`](https://msdn.microsoft.com/en-us/library/system.componentmodel.canceleventargs(v=vs.110).aspx) kaynağından türetilen yeni bir olay argümanı oluşturun ve için ek özellikler ekleyin olay verileri.
- `EventHandler<T>` kullanarak bir olay oluşturun ve oluşturduğunuz yeni iptal olayı argüman sınıfını kullanın.

**Örnek**

Aşağıdaki örnekte, bir sınıfın 'Price' özelliği için bir 'PriceChangingEventArgs' olayı oluşturuyoruz. Olay verileri sınıfı, tüketicinin yeni hakkında bilgi sahibi olmasını sağlayan bir "Değer" içerir. 'Price' özelliğine yeni bir değer atadığınızda olay yükselir ve tüketicinin değerin değiştiğini bilmesini sağlar ve olayı iptal etmesine izin verir. Tüketici etkinliği iptal ederse, önceki "Fiyat" değeri kullanılacaktır:

*Fiyat DeğiştirenEventArgs*

    public class PriceChangingEventArgs : CancelEventArgs
    {
        int value;
        public int Value
        {
            get { return value; }
        }
        public PriceChangingEventArgs(int value)
        {
            this.value = value;
        }
    }

*Ürün*

    public class Product
    {
        int price;
        public int Price
        {
            get { return price; }
            set
            {
                var e = new PriceChangingEventArgs(value);
                OnPriceChanging(e);
                if (!e.Cancel)
                    price = value;
            }
        }

        public event EventHandler<PriceChangingEventArgs> PropertyChanging;
        protected void OnPriceChanging(PriceChangingEventArgs e)
        {
            var handler = PropertyChanging;
            if (handler != null)
                PropertyChanging(this, e);
        }
    }




## Etkinlik Özellikleri
Bir sınıf çok sayıda olayı yükseltirse, temsilci başına bir alanın depolama maliyeti kabul edilebilir olmayabilir. .NET Framework, bu durumlar için [olay özellikleri](https://msdn.microsoft.com/en-us/library/8843a9ch(v=vs.110).aspx) sağlar. Bu şekilde, etkinlik temsilcilerini depolamak için [`EventHandlerList`](https://msdn.microsoft.com/en-us/library/system.componentmodel.eventhandlerlist(v=vs.110).aspx) gibi başka bir veri yapısını kullanabilirsiniz. :

    public class SampleClass 
    {
        // Define the delegate collection.
        protected EventHandlerList eventDelegates = new EventHandlerList();

        // Define a unique key for each event.
        static readonly object someEventKey = new object();
 
        // Define the SomeEvent event property.
        public event EventHandler SomeEvent
        {
            add
            {
                // Add the input delegate to the collection.
                eventDelegates.AddHandler(someEventKey, value);
            }
            remove
            {
                // Remove the input delegate from the collection.
                eventDelegates.RemoveHandler(someEventKey, value);
            }
        }

        // Raise the event with the delegate specified by someEventKey
        protected void OnSomeEvent(EventArgs e)
        {
            var handler = (EventHandler)eventDelegates[someEventKey];
            if (handler != null)
                handler(this, e);
        }
    }

Bu yaklaşım, denetimlerin düzinelerce ve hatta yüzlerce olaya sahip olabileceği WinForms gibi GUI çerçevelerinde yaygın olarak kullanılmaktadır.

'EventHandlerList'in iş parçacığı için güvenli olmadığını unutmayın, bu nedenle sınıfınızın birden çok iş parçacığından kullanılmasını bekliyorsanız, kilit ifadeleri veya başka bir eşitleme mekanizması eklemeniz (veya iş parçacığı güvenliği sağlayan bir depolama kullanmanız) gerekir.

## Standart Etkinlik Bildirimi
Etkinlik beyanı:
    
    public event EventHandler<EventArgsT> EventName;

Olay işleyici beyanı:

    public void HandlerName(object sender, EventArgsT args) { /* Handler logic */ }

Etkinliğe abone olmak:

*Dinamik olarak:*

    EventName += HandlerName;

*Tasarımcı aracılığıyla:*

1. Kontrolün özellikler penceresindeki Olaylar düğmesine tıklayın (Şimşek)
2. Etkinlik adına çift tıklayın:

[![buraya resim açıklamasını girin][1]][1]

3. Visual Studio, olay kodunu oluşturacaktır:


    private void Form1_Load(object sender, EventArgs e)
    {

    }

Yöntemi çağırmak:
    
    EventName(SenderObject, EventArguments);


[1]: https://i.stack.imgur.com/onqeE.png

## Anonim Olay İşleyici Bildirimi
Etkinlik beyanı:

    public event EventHandler<EventArgsType> EventName;

[lambda operatörü =>](https://www.wikiod.com/tr/docs/c%23/18/operators/12755/lambda-operator#t=20160727203428899399) kullanarak ve olaya abone olarak olay işleyici bildirimi:

    EventName += (obj, eventArgs) => { /* Handler logic */ };

[temsilci](https://www.wikiod.com/tr/docs/c%23/26/keywords/18720/delegate) anonim yöntem söz dizimini kullanan olay işleyici bildirimi:

    EventName += delegate(object obj, EventArgsType eventArgs) { /* Handler Logic */ };

Olayın parametresini kullanmayan bir olay işleyicisinin bildirimi ve aboneliği, bu nedenle yukarıdaki sözdizimini parametreleri belirtmeye gerek kalmadan kullanabilir:

    EventName += delegate { /* Handler Logic */ }

Olayı çağırmak:

    EventName?.Invoke(SenderObject, EventArguments);

## Standart Olmayan Olay Beyanı
Olaylar, yalnızca "EventHandler" ve "EventHandler<T>" değil, herhangi bir temsilci türünde olabilir. Örneğin:

    //Declaring an event
    public event Action<Param1Type, Param2Type, ...> EventName;

Bu, standart "EventHandler" olaylarına benzer şekilde kullanılır:

    //Adding a named event handler
    public void HandlerName(Param1Type parameter1, Param2Type parameter2, ...) {
        /* Handler logic */
    }
    EventName += HandlerName;

    //Adding an anonymous event handler
    EventName += (parameter1, parameter2, ...) => { /* Handler Logic */ };

    //Invoking the event
    EventName(parameter1, parameter2, ...);

---

Alanlar ve yerel değişkenlere benzer şekilde, aynı türden birden çok olayı tek bir ifadede bildirmek mümkündür (ancak bu genellikle kötü bir fikir olabilir):

    public event EventHandler Event1, Event2, Event3;

Bu, tümü 'EventHandler' türünde üç ayrı olay ('Event1', 'Event2' ve 'Event3') bildirir.
*Not: Bazı derleyiciler bu sözdizimini sınıfların yanı sıra arabirimlerde de kabul edebilirlerse de, C# belirtimi (v5.0 §13.2.3) buna izin vermeyen arabirimler için dilbilgisi sağlar, bu nedenle bunu arabirimlerde kullanmak farklı derleyicilerle güvenilir olmayabilir .*

## Ek veriler içeren özel EventArgs oluşturma
Özel olaylar genellikle olay hakkında bilgi içeren özel olay bağımsız değişkenlerine ihtiyaç duyar. Örneğin, "MouseDown" gibi fare olayları tarafından kullanılan [`MouseEventArgs`](https://msdn.microsoft.com/en-us/library/system.windows.forms.mouseeventargs(v=vs.110).aspx) ' veya 'MouseUp' olayları, olayı oluşturmak için kullanılan 'Konum' veya 'Düğmeler' hakkında bilgi içerir.

Yeni olaylar oluştururken, özel bir olay argümanı oluşturmak için:

- [`EventArgs`](https://msdn.microsoft.com/en-us/library/system.eventargs(v=vs.110).aspx)'den türetilen bir sınıf oluşturun ve gerekli veriler için özellikleri tanımlayın.
- Bir kural olarak, sınıfın adı 'EventArgs' ile bitmelidir.

**Örnek**

Aşağıdaki örnekte, bir sınıfın 'Price' özelliği için bir 'PriceChangingEventArgs' olayı oluşturuyoruz. Olay veri sınıfı bir "CurrentPrice" ve bir "NewPrice" içerir. "Fiyat" özelliğine yeni bir değer atadığınızda olay yükselir ve tüketicinin değerin değiştiğini bilmesini sağlar ve mevcut fiyat ve yeni fiyat hakkında bilgi sahibi olmasını sağlar:

*Fiyat DeğiştirenEventArgs*

    public class PriceChangingEventArgs : EventArgs
    {
        public PriceChangingEventArgs(int currentPrice, int newPrice)
        {
            this.CurrentPrice = currentPrice;
            this.NewPrice = newPrice;
        }

        public int CurrentPrice { get; private set; }
        public int NewPrice { get; private set; }
    }

*Ürün*

    public class Product
    {
        public event EventHandler<PriceChangingEventArgs> PriceChanging;

        int price;
        public int Price
        {
            get { return price; }
            set
            {
                var e = new PriceChangingEventArgs(price, value);
                OnPriceChanging(e);
                price = value;
            }
        }

        protected void OnPriceChanging(PriceChangingEventArgs e)
        {
            var handler = PriceChanging;
            if (handler != null)
                handler(this, e);
        }
    }

Tüketicinin yeni değeri değiştirmesine izin vererek örneği geliştirebilirsiniz ve ardından değer mülk için kullanılacaktır. Bunun için bu değişiklikleri sınıflarda uygulamak yeterlidir.

"NewPrice" tanımını ayarlanabilir olacak şekilde değiştirin:

    public int NewPrice { get; set; }

"OnPriceChanging" öğesini çağırdıktan sonra, "Price" tanımını, mülk değeri olarak "e.NewPrice" kullanacak şekilde değiştirin:

    int price;
    public int Price
    {
        get { return price; }
        set
        {
            var e = new PriceChangingEventArgs(price, value);
            OnPriceChanging(e);
            price = e.NewPrice;
        }
    }




