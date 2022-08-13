---
title: "Flujo de datos TPL"
slug: "flujo-de-datos-tpl"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

Bibliotecas utilizadas en ejemplos
=======
`System.Threading.Tasks.Dataflow`

`System.Threading.Tasks`

`Sistema.Net.Http`

`Sistema.Net`

Diferencia entre publicación y SendAsync
=======
Para agregar elementos a un bloque, puede usar `Post` o `SendAsync`.

`Post` intentará agregar el elemento de forma sincrónica y devolverá un `bool` que indica si tuvo éxito o no. Puede que no tenga éxito cuando, por ejemplo, un bloque ha alcanzado su `BoundedCapcity` y todavía no tiene espacio para nuevos elementos. `SendAsync`, por otro lado, devolverá una `Task<bool>` incompleta que puedes `esperar`. Esa tarea se completará en el futuro con un resultado "verdadero" cuando el bloque borre su cola interna y pueda aceptar más elementos o con un resultado "falso" si está disminuyendo permanentemente (por ejemplo, como resultado de una cancelación).

## Consumidor productor asíncrono con un bloque de búfer acotado
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

## Publicar en un ActionBlock y esperar a que se complete
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

## Vinculación de bloques para crear un pipeline
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
    

## Productor/Consumidor síncrono con BufferBlock<T>
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
    

