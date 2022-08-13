---
title: "Construções de fluxo de dados da biblioteca paralela de tarefas (TPL)"
slug: "construcoes-de-fluxo-de-dados-da-biblioteca-paralela-de-tarefas-tpl"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## ActionBlock<T>
(para cada)

Essa classe pode ser pensada logicamente como um buffer para dados a serem processados ​​combinados com tarefas para processar esses dados, com o “bloco de fluxo de dados” gerenciando ambos. Em seu uso mais básico, podemos instanciar um ActionBlock<TInput> e “postar” dados nele; o delegado fornecido na construção do ActionBlock será executado de forma assíncrona para cada parte dos dados postados.

[![digite a descrição da imagem aqui][1]][1]

**Computação Síncrona**

    var ab = new ActionBlock<TInput>(i => 
    {
        Compute(i);
    });
    …
    ab.Post(1);
    ab.Post(2);
    ab.Post(3);

**Limitando downloads assíncronos para no máximo 5 simultaneamente**
    
    var downloader = new ActionBlock<string>(async url =>
    {
        byte [] imageData = await DownloadAsync(url);
        Process(imageData);
    }, new DataflowBlockOptions { MaxDegreeOfParallelism = 5 }); 

    downloader.Post("http://website.com/path/to/images");
    downloader.Post("http://another-website.com/path/to/images");

[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/exRaP.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

##Bloco de transmissão<T>
(Copie um item e envie as cópias para todos os blocos aos quais ele está vinculado)

Ao contrário do BufferBlock<T>, a missão do BroadcastBlock<T> na vida é permitir que todos os alvos vinculados do bloco obtenham uma cópia de cada elemento publicado, substituindo continuamente o valor "atual" pelos propagados para ele.

Além disso, ao contrário de BufferBlock<T>, BroadcastBlock<T> não retém dados desnecessariamente. Depois que um dado específico for oferecido a todos os alvos, esse elemento será sobrescrito por qualquer parte dos dados que estiver na linha (como em todos os blocos de fluxo de dados, as mensagens são tratadas na ordem FIFO). Esse elemento será oferecido a todos os alvos e assim por diante.

[![digite a descrição da imagem aqui][1]][1]

**Produtor/consumidor assíncrono com um produtor limitado**

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
    
**Expondo status de um agente**
    
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
    
[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/ZstaY.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## BufferBlock<T>
(Fila FIFO: Os dados que entram são os dados que saem)

Resumindo, BufferBlock<T> fornece um buffer ilimitado ou limitado para armazenar instâncias de T.
Você pode “postar” instâncias de T no bloco, o que faz com que os dados postados sejam armazenados em uma ordem FIFO (first in first out) pelo bloco.
Você pode “receber” do bloco, o que permite obter de forma síncrona ou assíncrona instâncias de T previamente armazenadas ou disponíveis no futuro (novamente, FIFO).

[![digite a descrição da imagem aqui][1]][1]

**Produtor/consumidor assíncrono com um produtor limitado**

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

[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/S5vXJ.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## JoinBlock<T1, T2,…>
(Coleta 2-3 entradas e as combina em uma Tupla)

Assim como BatchBlock<T>, JoinBlock<T1, T2, …> é capaz de agrupar dados de várias fontes de dados. Na verdade, esse é o objetivo principal do JoinBlock<T1, T2, …>.

Por exemplo, um JoinBlock<string, double, int> é um ISourceBlock<Tuple<string, double, int>>.

Assim como com BatchBlock<T>, JoinBlock<T1, T2,…> é capaz de operar no modo ganancioso e não ganancioso.

- No modo guloso padrão, todos os dados oferecidos aos alvos são aceitos, mesmo que o outro alvo não tenha os dados necessários para formar uma tupla.
- No modo não ganancioso, os alvos do bloco adiarão os dados até que todos os alvos tenham recebido os dados necessários para criar uma tupla, momento em que o bloco se envolverá em um protocolo de confirmação de duas fases para recuperar atomicamente todos os itens necessários das fontes . Esse adiamento permite que outra entidade consuma os dados nesse meio tempo, de modo a permitir que o sistema geral avance.

[![digite a descrição da imagem aqui][1]][1]

**Processando solicitações com um número limitado de objetos agrupados**

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
    
[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/mmXJ8.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## WriteOnceBlock<T>
(Variável Readonly: Memoriza seu primeiro item de dados e distribui cópias dele como saída. Ignora todos os outros itens de dados)

Se BufferBlock<T> for o bloco mais fundamental no TPL Dataflow, WriteOnceBlock<T> será o mais simples.
Ele armazena no máximo um valor e, uma vez definido esse valor, nunca será substituído ou sobrescrito.

Você pode pensar em WriteOnceBlock<T> como sendo semelhante a uma variável de membro readonly em C#, exceto que em vez de ser configurável apenas em um construtor e, em seguida, ser imutável, é configurável apenas uma vez e, em seguida, imutável.

[![digite a descrição da imagem aqui][1]][1]

**Dividindo as saídas potenciais de uma tarefa**

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
    
[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/7M5Mp.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## BatchedJoinBlock<T1, T2,…>
(Coleta um certo número de itens totais de 2-3 entradas e os agrupa em uma Tupla de coleções de itens de dados)

BatchedJoinBlock<T1, T2,…> é, de certa forma, uma combinação de BatchBlock<T> e JoinBlock<T1, T2,…>.
Enquanto JoinBlock<T1, T2,…> é usado para agregar uma entrada de cada destino em uma tupla e BatchBlock<T> é usado para agregar N entradas em uma coleção, BatchedJoinBlock<T1, T2,…> é usado para reunir N entradas de todos os destinos em tuplas de coleções.

[![digite a descrição da imagem aqui][1]][1]

**Dispersar/Reunir**

Considere um problema de dispersão/reunião em que N operações são iniciadas, algumas das quais podem ter sucesso e produzir saídas de string e outras podem falhar e produzir exceções.

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

[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/FSgue.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformBlock<TInput, TOutput>
(Selecione, um para um)

Assim como ActionBlock<TInput>, TransformBlock<TInput, TOutput> permite a execução de um delegado para realizar alguma ação para cada dado de entrada; **ao contrário de ActionBlock<TInput>, esse processamento tem uma saída.** Esse delegado pode ser um Func<TInput, TOutput>, caso em que o processamento desse elemento é considerado concluído quando o delegado retorna ou pode ser um Func <TInput,Task<TOutput>>, caso em que o processamento desse elemento é considerado concluído não quando o delegado retorna, mas quando a tarefa retornada é concluída.
Para aqueles familiarizados com LINQ, é um pouco semelhante a Select(), pois recebe uma entrada, transforma essa entrada de alguma maneira e produz uma saída.

Por padrão, TransformBlock<TInput, TOutput> processa seus dados sequencialmente com um MaxDegreeOfParallelism igual a 1.
Além de receber a entrada em buffer e processá-la, este bloco receberá toda a sua saída processada e armazenará em buffer também (dados que não foram processados ​​e dados que foram processados).

Ele tem 2 tarefas: uma para processar os dados e outra para enviar os dados para o próximo bloco.

[![digite a descrição da imagem aqui][1]][1]

**Um pipeline simultâneo**

    var compressor = new TransformBlock<byte[], byte[]>(input => Compress(input));
    var encryptor = new TransformBlock<byte[], byte[]>(input => Encrypt(input));
    
    compressor.LinkTo(Encryptor); 

[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/jQcFo.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformManyBlock<TInput, TOutput>
(SelectMany, 1-m: Os resultados deste mapeamento são “achatados”, assim como SelectMany do LINQ)

TransformManyBlock<TInput, TOutput> é muito semelhante a TransformBlock<TInput, TOutput>.
A principal diferença é que enquanto um TransformBlock<TInput, TOutput> produz uma e apenas uma saída para cada entrada, TransformManyBlock<TInput, TOutput> produz qualquer número (zero ou mais) saídas para cada entrada. Assim como acontece com ActionBlock<TInput> e TransformBlock<TInput, TOutput>, esse processamento pode ser especificado usando delegados, tanto para processamento síncrono quanto assíncrono.

Um Func<TInput, IEnumerable<TOutput>> é usado para síncrono e um Func<TInput, Task<IEnumerable<TOutput>>> é usado para assíncrono. Assim como ActionBlock<TInput> e TransformBlock<TInput, TOutput>, TransformManyBlock<TInput, TOutput> assume como padrão o processamento sequencial, mas pode ser configurado de outra forma.

O delegado de mapeamento retorna uma coleção de itens, que são inseridos individualmente no buffer de saída.

[![digite a descrição da imagem aqui][1]][1]

**Rastreador da Web assíncrono**

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
    
**Expandindo um enumerável em seus elementos constituintes**

    var expanded = new TransformManyBlock<T[], T>(array => array);

**Filtrar indo de 1 a 0 ou 1 elemento**

    public IPropagatorBlock<T> CreateFilteredBuffer<T>(Predicate<T> filter)
    {
        return new TransformManyBlock<T, T>(item =>
            filter(item) ? new [] { item } : Enumerable.Empty<T>());
    }

[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/h7mip.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Bloco de lote<T>
(Agrupa um certo número de itens de dados sequenciais em coleções de itens de dados)

BatchBlock<T> combina N itens únicos em um item de lote, representado como uma matriz de elementos. Uma instância é criada com um tamanho de lote específico e o bloco cria um lote assim que recebe esse número de elementos, enviando de forma assíncrona o lote para o buffer de saída.

BatchBlock<T> é capaz de executar em modos gananciosos e não gananciosos.

- No modo guloso padrão, todas as mensagens oferecidas ao bloco de qualquer número de fontes são aceitas e armazenadas em buffer para serem convertidas em lotes.
- • No modo não ganancioso, todas as mensagens são adiadas das fontes até que fontes suficientes tenham oferecido mensagens ao bloco para criar um lote. Assim, um BatchBlock<T> pode ser usado para receber 1 elemento de cada uma das N origens, N elementos de 1 origem e uma infinidade de opções intermediárias.

[![digite a descrição da imagem aqui][1]][1]

**Agrupando solicitações em grupos de 100 para enviar a um banco de dados**

    var batchRequests = new BatchBlock<Request>(batchSize:100);
    var sendToDb = new ActionBlock<Request[]>(reqs => SubmitToDatabase(reqs));
    
    batchRequests.LinkTo(sendToDb);

**Criando um lote uma vez por segundo**

    var batch = new BatchBlock<T>(batchSize:Int32.MaxValue);
    new Timer(() => { batch.TriggerBatch(); }).Change(1000, 1000);
    
    
[Introdução ao TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/tLRyw.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

