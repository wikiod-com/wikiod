---
title: "Flux de données TPL"
slug: "flux-de-donnees-tpl"
draft: false
images: []
weight: 9902
type: docs
toc: true
---

Bibliothèques utilisées dans les exemples
=======
`System.Threading.Tasks.Dataflow`

`System.Threading.Tâches`

`System.Net.Http`

`System.Net`

Différence entre Post et SendAsync
=======
Pour ajouter des éléments à un bloc, vous pouvez utiliser `Post` ou `SendAsync`.

`Post` essaiera d'ajouter l'élément de manière synchrone et renverra un `bool` indiquant s'il a réussi ou non. Il peut échouer lorsque, par exemple, un bloc a atteint sa "BoundedCapcity" et n'a plus de place pour de nouveaux éléments. `SendAsync`, d'autre part, renverra une `Task<bool>` inachevée que vous pouvez `attendre`. Cette tâche se terminera à l'avenir avec un résultat "vrai" lorsque le bloc aura vidé sa file d'attente interne et pourra accepter plus d'éléments ou avec un résultat "faux" s'il décline de manière permanente (par exemple, à la suite d'une annulation).

## Producteur consommateur asynchrone avec un BufferBlock borné
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

## Publication sur un ActionBlock et attente d'achèvement
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

## Relier des blocs pour créer un pipeline
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
    

## Producteur/Consommateur synchrone avec BufferBlock<T>
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
    

