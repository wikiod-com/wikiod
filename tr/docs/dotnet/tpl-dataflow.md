---
title: "TPL Veri Akışı"
slug: "tpl-veri-aks"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

Örneklerde Kullanılan Kitaplıklar
=======
`System.Threading.Tasks.Dataflow`

`System.Threading.Tasks`

'System.Net.Http'

"Sistem.Net"

Post ve SendAsync arasındaki fark
=======
Bir bloğa öğe eklemek için "Post" veya "SendAsync" kullanabilirsiniz.

"Post", öğeyi eşzamanlı olarak eklemeye çalışacak ve başarılı olup olmadığını belirten bir "bool" döndürecektir. Örneğin, bir blok "BoundedCapcity" değerine ulaştığında ve henüz yeni öğeler için yer kalmadığında başarılı olmayabilir. Öte yandan 'SendAsync', 'bekleyebileceğiniz' tamamlanmamış bir 'Görev<bool>' döndürür. Bu görev gelecekte, blok kendi iç kuyruğunu temizlediğinde ve daha fazla öğe kabul ettiğinde "doğru" bir sonuçla veya kalıcı olarak düşüyorsa (örneğin, iptalin bir sonucu olarak) "yanlış" bir sonuçla tamamlanacaktır.

## Sınırlı BufferBlock ile Asenkron Üretici Tüketici
    var bufferBlock = new BufferBlock<int>(new DataflowBlockOptions
    {
        BoundedCapacity = 1000
    });

    var cancellationToken = new CancellationTokenSource(TimeSpan.FromSeconds(10)).Token;

    var producerTask = Task.Run(async () =>
    {
        var random = new Random();

        while (!cancellationToken.IsCancellationRequested)
        {
            var value = random.Next();
            await bufferBlock.SendAsync(value, cancellationToken);
        }
    });

    var consumerTask = Task.Run(async () =>
    {
        while (await bufferBlock.OutputAvailableAsync())
        {
            var value = bufferBlock.Receive();
            Console.WriteLine(value);
        }
    });

    await Task.WhenAll(producerTask, consumerTask);

## ActionBlock'a gönderme ve tamamlanmasını bekleme
    // Create a block with an asynchronous action
    var block = new ActionBlock<string>(async hostName =>
    {
        IPAddress[] ipAddresses = await Dns.GetHostAddressesAsync(hostName);
        Console.WriteLine(ipAddresses[0]);
    });

    block.Post("google.com"); // Post items to the block's InputQueue for processing
    block.Post("reddit.com");
    block.Post("stackoverflow.com");
    
    block.Complete(); // Tell the block to complete and stop accepting new items
    await block.Completion; // Asynchronously wait until all items completed processingu

## İşlem hattı oluşturmak için blokları bağlama
    var httpClient = new HttpClient();
    
    // Create a block the accepts a uri and returns its contents as a string
    var downloaderBlock = new TransformBlock<string, string>(
        async uri => await httpClient.GetStringAsync(uri));
    
    // Create a block that accepts the content and prints it to the console
    var printerBlock = new ActionBlock<string>(
        contents => Console.WriteLine(contents));
    
    // Make the downloaderBlock complete the printerBlock when its completed.
    var dataflowLinkOptions = new DataflowLinkOptions {PropagateCompletion = true};
    
    // Link the block to create a pipeline
    downloaderBlock.LinkTo(printerBlock, dataflowLinkOptions);
    
    // Post urls to the first block which will pass their contents to the second one.
    downloaderBlock.Post("http://youtube.com");
    downloaderBlock.Post("http://github.com");
    downloaderBlock.Post("http://twitter.com");
    
    downloaderBlock.Complete(); // Completion will propagate to printerBlock
    await printerBlock.Completion; // Only need to wait for the last block in the pipeline
    

## BufferBlock<T> ile Senkronize Üretici/Tüketici
    public class Producer
    {
        private static Random random = new Random((int)DateTime.UtcNow.Ticks);
        //produce the value that will be posted to buffer block
        public double Produce ( )
        {
            var value = random.NextDouble();
            Console.WriteLine($"Producing value: {value}");
            return value;
        }
    }

    public class Consumer
    {
        //consume the value that will be received from buffer block
        public void Consume (double value) => Console.WriteLine($"Consuming value: {value}");
    }

    class Program
    {
        private static BufferBlock<double> buffer = new BufferBlock<double>();
        static void Main (string[] args)
        {
            //start a task that will every 1 second post a value from the producer to buffer block
            var producerTask = Task.Run(async () =>
            {
                var producer = new Producer();
                while(true)
                {
                    buffer.Post(producer.Produce());
                    await Task.Delay(1000);
                }
            });
            //start a task that will recieve values from bufferblock and consume it
            var consumerTask = Task.Run(() => 
            {
                var consumer = new Consumer();
                while(true)
                {
                    consumer.Consume(buffer.Receive());
                }
            });

            Task.WaitAll(new[] { producerTask, consumerTask });
        }
    }
    

