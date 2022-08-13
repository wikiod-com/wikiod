---
title: "Construcciones de flujo de datos de la biblioteca paralela de tareas (TPL)"
slug: "construcciones-de-flujo-de-datos-de-la-biblioteca-paralela-de-tareas-tpl"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Bloque de acción<T>
(para cada)

Esta clase se puede considerar lógicamente como un búfer para procesar los datos combinados con tareas para procesar esos datos, con el "bloque de flujo de datos" administrando ambos. En su uso más básico, podemos instanciar un ActionBlock<TInput> y "publicar" datos en él; el delegado proporcionado en la construcción de ActionBlock se ejecutará de forma asíncrona para cada dato publicado.

[![ingrese la descripción de la imagen aquí][1]][1]

**Cálculo sincrónico**

    var ab = new ActionBlock<TInput>(i => 
    {
        Compute(i);
    });
    …
    ab.Post(1);
    ab.Post(2);
    ab.Post(3);

**Reducción de descargas asincrónicas a un máximo de 5 al mismo tiempo**
    
    var downloader = new ActionBlock<string>(async url =>
    {
        byte [] imageData = await DownloadAsync(url);
        Process(imageData);
    }, new DataflowBlockOptions { MaxDegreeOfParallelism = 5 }); 

    downloader.Post("http://website.com/path/to/images");
    downloader.Post("http://another-website.com/path/to/images");

[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/exRaP.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Bloque de difusión<T>
(Copie un elemento y envíe las copias a cada bloque al que está vinculado)

A diferencia de BufferBlock<T>, la misión en la vida de BroadcastBlock<T> es permitir que todos los objetivos vinculados desde el bloque obtengan una copia de cada elemento publicado, sobrescribiendo continuamente el valor "actual" con los que se propagan a él.

Además, a diferencia de BufferBlock<T>, BroadcastBlock<T> no guarda datos innecesariamente. Después de que se haya ofrecido un dato en particular a todos los objetivos, ese elemento se sobrescribirá con el siguiente dato en la línea (al igual que con todos los bloques de flujo de datos, los mensajes se manejan en orden FIFO). Ese elemento se ofrecerá a todos los objetivos, y así sucesivamente.

[![ingrese la descripción de la imagen aquí][1]][1]

**Productor/consumidor asíncrono con un productor limitado**

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
    
**Exponer el estado de un agente**
    
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
    
[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/ZStaY.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Bloque de búfer<T>
(Cola FIFO: Los datos que entran son los datos que salen)

En resumen, BufferBlock<T> proporciona un búfer limitado o ilimitado para almacenar instancias de T.
Puede "publicar" instancias de T en el bloque, lo que hace que los datos que se publican se almacenen en un orden de primero en entrar, primero en salir (FIFO) por el bloque.
Puede "recibir" del bloque, lo que le permite obtener de forma sincrónica o asincrónica instancias de T previamente almacenadas o disponibles en el futuro (nuevamente, FIFO).

[![ingrese la descripción de la imagen aquí][1]][1]

**Productor/consumidor asíncrono con un productor limitado**

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

[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/S5vXJ.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## UnirBloque<T1, T2,…>
(Recopila 2-3 entradas y las combina en una Tupla)

Al igual que BatchBlock<T>, JoinBlock<T1, T2, …> puede agrupar datos de múltiples fuentes de datos. De hecho, ese es el propósito principal de JoinBlock<T1, T2, …>.

Por ejemplo, un JoinBlock<string, double, int> es un ISourceBlock<Tuple<string, double, int>>.

Al igual que con BatchBlock<T>, JoinBlock<T1, T2,…> es capaz de operar tanto en modo voraz como no voraz.

- En el modo voraz por defecto, se aceptan todos los datos ofrecidos a los objetivos, incluso si el otro objetivo no tiene los datos necesarios para formar una tupla.
- En el modo no codicioso, los objetivos del bloque pospondrán los datos hasta que a todos los objetivos se les hayan ofrecido los datos necesarios para crear una tupla, momento en el cual el bloque se involucrará en un protocolo de compromiso de dos fases para recuperar atómicamente todos los elementos necesarios de las fuentes. . Este aplazamiento hace posible que otra entidad consuma los datos mientras tanto para permitir que el sistema general avance.

[![ingrese la descripción de la imagen aquí][1]][1]

**Procesamiento de solicitudes con un número limitado de objetos agrupados**

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
    
[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/mmXJ8.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## WriteOnceBlock<T>
(Variable de solo lectura: Memoriza su primer elemento de datos y distribuye copias de él como su salida. Ignora todos los demás elementos de datos)

Si BufferBlock<T> es el bloque más fundamental en TPL Dataflow, WriteOnceBlock<T> es el más simple.
Almacena como máximo un valor, y una vez que se ha establecido ese valor, nunca se reemplazará ni se sobrescribirá.

Puede pensar en WriteOnceBlock<T> como algo similar a una variable miembro de solo lectura en C#, excepto que en lugar de que solo se pueda configurar en un constructor y luego sea inmutable, solo se puede configurar una vez y luego es inmutable.

[![ingrese la descripción de la imagen aquí][1]][1]

**Dividir los resultados potenciales de una tarea**

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
    
[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/7M5Mp.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## LoteJoinBlock<T1, T2,…>
(Recopila una cierta cantidad de elementos totales de 2-3 entradas y los agrupa en una tupla de colecciones de elementos de datos)

BatchedJoinBlock<T1, T2,…> es, en cierto sentido, una combinación de BatchBlock<T> y JoinBlock<T1, T2,…>.
Mientras que JoinBlock<T1, T2,...> se usa para agregar una entrada de cada destino en una tupla, y BatchBlock<T> se usa para agregar N entradas en una colección, BatchedJoinBlock<T1, T2,...> se usa para recopilar N entradas de todos los objetivos en tuplas de colecciones.

[![ingrese la descripción de la imagen aquí][1]][1]

** Dispersión / Recopilación **

Considere un problema de dispersión/recopilación en el que se lanzan N operaciones, algunas de las cuales pueden tener éxito y producir salidas de cadena, y otras pueden fallar y producir excepciones.

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

[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/FSgue.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformBlock<TInput, TOutput>
(Seleccionar, uno a uno)

Al igual que ActionBlock<TInput>, TransformBlock<TInput, TOutput> permite la ejecución de un delegado para realizar alguna acción para cada dato de entrada; **a diferencia de ActionBlock<TInput>, este procesamiento tiene una salida.** Este delegado puede ser un Func<TInput, TOutput>, en cuyo caso el procesamiento de ese elemento se considera completado cuando el delegado regresa, o puede ser un Func <TInput,Task<TOutput>>, en cuyo caso el procesamiento de ese elemento se considera completado no cuando el delegado regresa, sino cuando finaliza la tarea devuelta.
Para aquellos familiarizados con LINQ, es algo similar a Select() en el sentido de que toma una entrada, la transforma de alguna manera y luego produce una salida.

De forma predeterminada, TransformBlock<TInput, TOutput> procesa sus datos secuencialmente con un MaxDegreeOfParallelism igual a 1.
Además de recibir la entrada almacenada en el búfer y procesarla, este bloque tomará toda su salida procesada y también la almacenará en el búfer (datos que no han sido procesados ​​y datos que han sido procesados).

Tiene 2 tareas: una para procesar los datos y otra para enviar datos al siguiente bloque.

[![ingrese la descripción de la imagen aquí][1]][1]

**Una tubería concurrente**

    var compressor = new TransformBlock<byte[], byte[]>(input => Compress(input));
    var encryptor = new TransformBlock<byte[], byte[]>(input => Encrypt(input));
    
    compressor.LinkTo(Encryptor); 

[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/jQcFo.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformManyBlock<TInput, TOutput>
(SelectMany, 1-m: los resultados de esta asignación se "aplanan", al igual que SelectMany de LINQ)

TransformManyBlock<TInput, TOutput> es muy similar a TransformBlock<TInput, TOutput>.
La diferencia clave es que, mientras que TransformBlock<TInput, TOutput> produce una y solo una salida para cada entrada, TransformManyBlock<TInput, TOutput> produce cualquier número (cero o más) de salidas para cada entrada. Al igual que con ActionBlock<TInput> y TransformBlock<TInput, TOutput>, este procesamiento se puede especificar mediante delegados, tanto para el procesamiento sincrónico como asincrónico.

Func<TInput, IEnumerable<TOutput>> se usa para sincrónico y Func<TInput, Task<IEnumerable<TOutput>>> para asincrónico. Al igual que con ActionBlock<TInput> y TransformBlock<TInput, TOutput>, TransformManyBlock<TInput, TOutput> tiene como valor predeterminado el procesamiento secuencial, pero se puede configurar de otra manera.

El delegado de mapeo devuelve una colección de elementos, que se insertan individualmente en el búfer de salida.

[![ingrese la descripción de la imagen aquí][1]][1]

**Rastreador web asíncrono**

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
    
**Expansión de un enumerable en sus elementos constituyentes**

    var expanded = new TransformManyBlock<T[], T>(array => array);

**Filtrar pasando de 1 a 0 o 1 elemento**

    public IPropagatorBlock<T> CreateFilteredBuffer<T>(Predicate<T> filter)
    {
        return new TransformManyBlock<T, T>(item =>
            filter(item) ? new [] { item } : Enumerable.Empty<T>());
    }

[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/h7mip.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Bloque por lotes<T>
(Agrupa un cierto número de elementos de datos secuenciales en colecciones de elementos de datos)

BatchBlock<T> combina N elementos individuales en un elemento de lote, representado como una matriz de elementos. Se crea una instancia con un tamaño de lote específico, y el bloque luego crea un lote tan pronto como recibe esa cantidad de elementos, enviando asíncronamente el lote al búfer de salida.

BatchBlock<T> es capaz de ejecutarse tanto en modo voraz como no voraz.

- En el modo codicioso predeterminado, todos los mensajes ofrecidos al bloque desde cualquier número de fuentes se aceptan y almacenan en búfer para convertirlos en lotes.
- • En el modo no codicioso, todos los mensajes se posponen desde las fuentes hasta que suficientes fuentes hayan ofrecido mensajes al bloque para crear un lote. Por lo tanto, se puede usar un BatchBlock<T> para recibir 1 elemento de cada una de las N fuentes, N elementos de 1 fuente y una miríada de opciones intermedias.

[![ingrese la descripción de la imagen aquí][1]][1]

**Solicitudes por lotes en grupos de 100 para enviar a una base de datos**

    var batchRequests = new BatchBlock<Request>(batchSize:100);
    var sendToDb = new ActionBlock<Request[]>(reqs => SubmitToDatabase(reqs));
    
    batchRequests.LinkTo(sendToDb);

**Creando un lote una vez por segundo**

    var batch = new BatchBlock<T>(batchSize:Int32.MaxValue);
    new Timer(() => { batch.TriggerBatch(); }).Change(1000, 1000);
    
    
[Introducción a TPL Dataflow por Stephen Toub][2]


[1]: http://i.stack.imgur.com/tLRyw.png
[2]: https://www.microsoft.com/en-us/download/details.aspx?id=14782

