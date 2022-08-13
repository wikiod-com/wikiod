---
title: "Fluxo de dados TPL"
slug: "fluxo-de-dados-tpl"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

Bibliotecas usadas em exemplos
=======
`System.Threading.Tasks.Dataflow`

`System.Threading.Tasks`

`System.Net.Http`

`System.Net`

Diferença entre Post e SendAsync
=======
Para adicionar itens a um bloco, você pode usar `Post` ou `SendAsync`.

`Post` tentará adicionar o item de forma síncrona e retornará um `bool` dizendo se foi bem-sucedido ou não. Pode não ter sucesso quando, por exemplo, um bloco atingiu seu `BoundedCapcity` e não tem mais espaço para novos itens ainda. `SendAsync`, por outro lado, retornará uma `Task<bool>` incompleta que você pode `aguardar`. Essa tarefa será concluída no futuro com um resultado `true` quando o bloco limpar sua fila interna e puder aceitar mais itens ou com um resultado `false` se estiver diminuindo permanentemente (por exemplo, como resultado de cancelamento).

## Consumidor produtor assíncrono com um BufferBlock limitado
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

## Postando em um ActionBlock e aguardando a conclusão
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

## Vinculando blocos para criar um pipeline
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
    

## Produtor/Consumidor Síncrono com BufferBlock<T>
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
    

