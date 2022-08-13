---
title: "Görev Paralel Kitaplığı (TPL) Veri Akışı Yapıları"
slug: "gorev-paralel-kitaplg-tpl-veri-aks-yaplar"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## ActionBlock<T>
(her biri için)

Bu sınıf, mantıksal olarak, her ikisini de yöneten “veri akışı bloğu” ile bu verileri işleme görevleriyle birlikte işlenecek veriler için bir arabellek olarak düşünülebilir. En temel kullanımında, bir ActionBlock<TInput> örneğini oluşturabilir ve ona veri "gönderebiliriz"; ActionBlock'un yapısında sağlanan temsilci, gönderilen her veri parçası için eşzamansız olarak yürütülecektir.

[![buraya resim açıklamasını girin][1]][1]

**Senkron Hesaplama**

    var ab = new ActionBlock<TInput>(i => 
    {
        Compute(i);
    });
    …
    ab.Post(1);
    ab.Post(2);
    ab.Post(3);

**Eşzamansız İndirmeleri aynı anda en fazla 5'e düşürme**
    
    var downloader = new ActionBlock<string>(async url =>
    {
        byte [] imageData = await DownloadAsync(url);
        Process(imageData);
    }, new DataflowBlockOptions { MaxDegreeOfParallelism = 5 }); 

    downloader.Post("http://website.com/path/to/images");
    downloader.Post("http://another-website.com/path/to/images");

[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/exRaP.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## YayınBlok<T>
(Bir öğeyi kopyalayın ve kopyalarını bağlı olduğu her bloğa gönderin)

BufferBlock<T>'dan farklı olarak, BroadcastBlock<T>'nin hayattaki görevi, bloktan bağlanan tüm hedeflerin yayınlanan her öğenin bir kopyasını almasını sağlamak ve sürekli olarak "geçerli" değerin üzerine yayılan değerlerin üzerine yazmaktır.

Ek olarak, BufferBlock<T>'den farklı olarak BroadcastBlock<T>, verileri gereksiz yere tutmaz. Tüm hedeflere belirli bir veri sunulduktan sonra, sıradaki veri parçası ne olursa olsun bu öğenin üzerine yazılacaktır (tüm veri akışı bloklarında olduğu gibi, mesajlar FIFO sırasına göre işlenir). Bu unsur tüm hedeflere sunulacak vb.

[![buraya resim açıklamasını girin][1]][1]

**Kısılmış Üreticiye Sahip Asenkron Üretici/Tüketici**

    var ui = TaskScheduler.FromCurrentSynchronizationContext();
    var bb = new BroadcastBlock<ImageData>(i => i);
    
    var saveToDiskBlock = new ActionBlock<ImageData>(item =>
        item.Image.Save(item.Path)
    );
    
    var showInUiBlock = new ActionBlock<ImageData>(item =>
        imagePanel.AddImage(item.Image), 
        new DataflowBlockOptions { TaskScheduler = TaskScheduler.FromCurrentSynchronizationContext() }
    );
    
    bb.LinkTo(saveToDiskBlock);
    bb.LinkTo(showInUiBlock);
    
**Bir Temsilciden Durumu İfşa Etme**
    
    public class MyAgent
    {
        public ISourceBlock<string> Status { get; private set; }
        
        public MyAgent()
        {
            Status = new BroadcastBlock<string>();
            Run();
        } 
    
        private void Run()
        {
            Status.Post("Starting");
            Status.Post("Doing cool stuff");
            …
            Status.Post("Done");
        }
    }
    
[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/ZStaY.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TamponBlok<T>
(FIFO Kuyruğu: Gelen veri giden veridir)

Kısacası, BufferBlock<T>, T örneklerini depolamak için sınırsız veya sınırlı bir arabellek sağlar.
T'nin örneklerini bloğa "gönderebilirsiniz", bu da gönderilen verilerin blok tarafından ilk giren ilk çıkar (FIFO) düzeninde depolanmasına neden olur.
Bloktan "alabilirsiniz", bu da daha önce depolanmış veya gelecekte mevcut olan T örneklerini eşzamanlı veya eşzamansız olarak elde etmenize olanak tanır (yine FIFO).

[![buraya resim açıklamasını girin][1]][1]

**Kısılmış Üreticiye Sahip Asenkron Üretici/Tüketici**

    // Hand-off through a bounded BufferBlock<T>
    private static BufferBlock<int> _Buffer = new BufferBlock<int>(
        new DataflowBlockOptions { BoundedCapacity = 10 });

    // Producer
    private static async void Producer()
    {
        while(true)
        {
            await _Buffer.SendAsync(Produce());
        }
    }

    // Consumer
    private static async Task Consumer()
    {
        while(true)
        {
            Process(await _Buffer.ReceiveAsync());
        } 
    }

    // Start the Producer and Consumer
    private static async Task Run()
    {
        await Task.WhenAll(Producer(), Consumer());
    }

[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/S5vXJ.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## JoinBlock<T1, T2,…>
(2-3 girdiyi toplar ve bunları bir Tuple'da birleştirir)

BatchBlock<T> gibi, JoinBlock<T1, T2, …> da birden çok veri kaynağından gelen verileri gruplayabilir. Aslında, JoinBlock<T1, T2, …>'nin birincil amacı budur.

Örneğin, JoinBlock<string, double, int>, bir ISourceBlock<Tuple<string, double, int>>'dir.

BatchBlock<T> ile olduğu gibi, JoinBlock<T1, T2,…> hem açgözlü hem de açgözlü olmayan modda çalışabilir.

- Varsayılan açgözlü modda, diğer hedef bir demet oluşturmak için gerekli verilere sahip olmasa bile, hedeflere sunulan tüm veriler kabul edilir.
- Açgözlü olmayan modda, bloğun hedefleri, tüm hedeflere bir demet oluşturmak için gerekli veriler sunulana kadar verileri erteler, bu noktada blok, kaynaklardan gerekli tüm öğeleri atomik olarak almak için iki aşamalı bir taahhüt protokolüne girer. . Bu erteleme, genel sistemin ileriye doğru ilerlemesine izin vermek için başka bir varlığın bu arada verileri tüketmesini mümkün kılar.

[![buraya resim açıklamasını girin][1]][1]

**Sınırlı Sayıda Havuzlanmış Nesnelerle İstekleri İşleme**

    var throttle = new JoinBlock<ExpensiveObject, Request>();
    for(int i=0; i<10; i++) 
    {
        requestProcessor.Target1.Post(new ExpensiveObject()); 
    }

    var processor = new Transform<Tuple<ExpensiveObject, Request>, ExpensiveObject>(pair =>
    {
        var resource = pair.Item1;
        var request = pair.Item2;
        
        request.ProcessWith(resource);
        
        return resource;
    });
    
    throttle.LinkTo(processor);
    processor.LinkTo(throttle.Target1);
    
[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/mmXJ8.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## WriteOnceBlock<T>
(Salt okunur değişken: İlk veri öğesini ezberler ve çıktı olarak kopyalarını dağıtır. Diğer tüm veri öğelerini yok sayar)

BufferBlock<T>, TPL Dataflow'daki en temel bloksa, WriteOnceBlock<T> en basitidir.
En fazla bir değer depolar ve bu değer bir kez ayarlandıktan sonra asla değiştirilmez veya üzerine yazılmaz.

WriteOnceBlock<T> öğesini C#'daki salt okunur bir üye değişkene benzer olarak düşünebilirsiniz, ancak yalnızca bir kurucuda ayarlanabilir ve ardından değişmez olması dışında, yalnızca bir kez ayarlanabilir ve ardından değişmezdir.

[![buraya resim açıklamasını girin][1]][1]

**Bir Görevin Potansiyel Çıktılarını Bölme**

    public static async void SplitIntoBlocks(this Task<T> task,
        out IPropagatorBlock<T> result, 
        out IPropagatorBlock<Exception> exception)
    {
        result = new WriteOnceBlock<T>(i => i);
        exception = new WriteOnceBlock<Exception>(i => i);
    
        try 
        { 
            result.Post(await task); 
        }
        catch(Exception ex) 
        { 
            exception.Post(ex); 
        }
    }
    
[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/7M5Mp.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## BatchedJoinBlock<T1, T2,…>
(2-3 girdiden belirli sayıda toplam öğe toplar ve bunları bir dizi veri öğesi koleksiyonunda gruplandırır)

BatchedJoinBlock<T1, T2,…> bir anlamda BatchBlock<T> ve JoinBlock<T1, T2,…> birleşimidir.
JoinBlock<T1, T2,…>, her hedeften bir girdiyi bir demete toplamak için kullanılırken ve BatchBlock<T>, N girdiyi bir koleksiyonda toplamak için kullanılırken, N'yi toplamak için BatchedJoinBlock<T1, T2,…> kullanılır. tüm hedeflerden gelen girdiler koleksiyon demetlerine.

[![buraya resim açıklamasını girin][1]][1]

**Dağıt/Topla**

N işleminin başlatıldığı, bazıları başarılı olabilen ve dize çıktıları üretebilen ve diğerleri başarısız olabilen ve İstisnalar üretebilen bir dağılım/toplama problemi düşünün.

    var batchedJoin = new BatchedJoinBlock<string, Exception>(10);
    
    for (int i=0; i<10; i++)
    {
        Task.Factory.StartNew(() => {
            try { batchedJoin.Target1.Post(DoWork()); }
            catch(Exception ex) { batchJoin.Target2.Post(ex); }
        });
    }
    
    var results = await batchedJoin.ReceiveAsync();
    
    foreach(string s in results.Item1) 
    {
        Console.WriteLine(s);
    }
    
    foreach(Exception e in results.Item2) 
    {
        Console.WriteLine(e);
    }

[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/FSgue.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformBlock<TInput, Toutput>
(Seç, bire bir)

ActionBlock<TInput> ile olduğu gibi, TransformBlock<TInput, Toutput> bir temsilcinin her giriş verisi için bir eylem gerçekleştirmesini sağlar; **ActionBlock<TInput>'tan farklı olarak, bu işlemin bir çıktısı vardır.** Bu temsilci bir Func<TInput, TOoutput> olabilir, bu durumda temsilci döndüğünde o öğenin işlenmesi tamamlanmış kabul edilir veya bir Func olabilir <TInput,Task<TOoutput>>, bu durumda o öğenin işlenmesi, temsilci döndüğünde değil, döndürülen Task tamamlandığında tamamlanmış sayılır.
LINQ'ya aşina olanlar için, bir girdi alması, bu girdiyi bir şekilde dönüştürmesi ve ardından bir çıktı üretmesi bakımından Select()'e biraz benzer.

Varsayılan olarak, TransformBlock<TInput, Toutput> verilerini 1'e eşit bir MaxDegreeOfParallelism ile sırayla işler.
Bu blok, arabelleğe alınmış girdiyi alıp işlemeye ek olarak, tüm işlenmiş çıktısını ve onu da (işlenmemiş veriler ve işlenmiş veriler) arabelleğe alacaktır.

2 görevi vardır: Biri verileri işlemek, diğeri verileri bir sonraki bloğa göndermek.

[![buraya resim açıklamasını girin][1]][1]

**Eşzamanlı Boru Hattı**

    var compressor = new TransformBlock<byte[], byte[]>(input => Compress(input));
    var encryptor = new TransformBlock<byte[], byte[]>(input => Encrypt(input));
    
    compressor.LinkTo(Encryptor); 

[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/jQcFo.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformManyBlock<TInput, Toutput>
(SelectMany, 1-m: Bu eşlemenin sonuçları, tıpkı LINQ's SelectMany gibi "düzleştirilir")

TransformManyBlock<TInput, Toutput>, TransformBlock<TInput, Toutput>'a çok benzer.
Temel fark, bir TransformBlock<TInput, Toutput> her giriş için bir ve yalnızca bir çıktı üretirken, TransformManyBlock<TInput, Toutput> her girdi için herhangi bir sayıda (sıfır veya daha fazla) çıktı üretir. ActionBlock<TInput> ve TransformBlock<TInput, Toutput> ile olduğu gibi, bu işleme, hem eşzamanlı hem de eşzamansız işleme için temsilciler kullanılarak belirtilebilir.

Eşzamanlı için bir Func<TInput, IEnumerable<TOutput>> kullanılır ve eşzamansız için bir Func<TInput, Task<IEnumerable<TOutput>>> kullanılır. Hem ActionBlock<TInput> hem de TransformBlock<TInput, Toutput> ile olduğu gibi, TransformManyBlock<TInput, Toutput> varsayılan olarak sıralı işlemeye geçer, ancak başka şekilde yapılandırılabilir.

Eşleme temsilcisi, çıktı arabelleğine ayrı ayrı eklenen bir öğe koleksiyonunu geri döndürür.

[![buraya resim açıklamasını girin][1]][1]

**Eşzamansız Web Tarayıcı**

    var downloader = new TransformManyBlock<string, string>(async url =>
    {
        Console.WriteLine(“Downloading “ + url);
        try 
        { 
            return ParseLinks(await DownloadContents(url)); 
        } 
        catch{}
        
        return Enumerable.Empty<string>();
    });
    downloader.LinkTo(downloader);
    
**Bir Numaralandırılabiliri Oluşturan Öğelerine Genişletme**

    var expanded = new TransformManyBlock<T[], T>(array => array);

**1'den 0'a veya 1 elemana gidilerek filtreleme**

    public IPropagatorBlock<T> CreateFilteredBuffer<T>(Predicate<T> filter)
    {
        return new TransformManyBlock<T, T>(item =>
            filter(item) ? new [] { item } : Enumerable.Empty<T>());
    }

[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/h7mip.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Toplu Blok<T>
(Belirli sayıda sıralı veri öğesini veri öğeleri koleksiyonlarında gruplandırır)

BatchBlock<T>, N tekli öğeyi, bir öğe dizisi olarak temsil edilen tek bir toplu iş öğesinde birleştirir. Belirli bir toplu iş boyutuna sahip bir örnek oluşturulur ve ardından blok, bu sayıda öğeyi alır almaz bir toplu iş oluşturur ve toplu işi eşzamansız olarak çıktı arabelleğine verir.

BatchBlock<T> hem açgözlü hem de açgözlü olmayan modlarda yürütme yeteneğine sahiptir.

- Varsayılan açgözlü modda, herhangi bir sayıda kaynaktan bloğa sunulan tüm mesajlar kabul edilir ve yığınlara dönüştürülmek üzere arabelleğe alınır.
- • Açgözlü olmayan modda, bir yığın oluşturmak için yeterli kaynak bloğa mesaj sunana kadar kaynaklardan gelen tüm mesajlar ertelenir. Bu nedenle, bir BatchBlock<T>, N kaynağın her birinden 1 öğe, 1 kaynaktan N öğe ve bunların arasında sayısız seçenek almak için kullanılabilir.

[![buraya resim açıklamasını girin][1]][1]

**Bir Veritabanına Göndermek İçin İstekleri 100'er kişilik gruplar halinde Toplulaştırma**

    var batchRequests = new BatchBlock<Request>(batchSize:100);
    var sendToDb = new ActionBlock<Request[]>(reqs => SubmitToDatabase(reqs));
    
    batchRequests.LinkTo(sendToDb);

**Saniyede bir toplu iş oluşturma**

    var batch = new BatchBlock<T>(batchSize:Int32.MaxValue);
    new Timer(() => { batch.TriggerBatch(); }).Change(1000, 1000);
    
    
[Stephen Toub'dan TPL Dataflow'a Giriş[2]


[1]: http://i.stack.imgur.com/tLRyw.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

