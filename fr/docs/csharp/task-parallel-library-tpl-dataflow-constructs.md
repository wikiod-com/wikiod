---
title: "Constructions de flux de données de la bibliothèque parallèle de tâches (TPL)"
slug: "constructions-de-flux-de-donnees-de-la-bibliotheque-parallele-de-taches-tpl"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## BlocAction<T>
(pour chaque)

Cette classe peut être considérée logiquement comme un tampon pour les données à traiter combiné avec des tâches de traitement de ces données, le "bloc de flux de données" gérant les deux. Dans son utilisation la plus élémentaire, nous pouvons instancier un ActionBlock<TInput> et y « publier » des données ; le délégué fourni lors de la construction de l'ActionBlock sera exécuté de manière asynchrone pour chaque élément de données publié.

[![entrez la description de l'image ici][1]][1]

**Calcul synchrone**

    var ab = new ActionBlock<TInput>(i => 
    {
        Compute(i);
    });
    …
    ab.Post(1);
    ab.Post(2);
    ab.Post(3);

** Limitation des téléchargements asynchrones à un maximum de 5 simultanément **
    
    var downloader = new ActionBlock<string>(async url =>
    {
        byte [] imageData = await DownloadAsync(url);
        Process(imageData);
    }, new DataflowBlockOptions { MaxDegreeOfParallelism = 5 }); 

    downloader.Post("http://website.com/path/to/images");
    downloader.Post("http://another-website.com/path/to/images");

[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/exRaP.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## Bloc de diffusion<T>
(Copiez un élément et envoyez les copies à chaque bloc auquel il est lié)

Contrairement à BufferBlock<T>, la mission de BroadcastBlock<T> dans la vie est de permettre à toutes les cibles liées à partir du bloc d'obtenir une copie de chaque élément publié, en écrasant continuellement la valeur "actuelle" avec celles qui lui sont propagées.

De plus, contrairement à BufferBlock<T>, BroadcastBlock<T> ne conserve pas les données inutilement. Une fois qu'une donnée particulière a été proposée à toutes les cibles, cet élément sera écrasé par la donnée suivante sur la ligne (comme pour tous les blocs de flux de données, les messages sont traités dans l'ordre FIFO). Cet élément sera proposé à toutes les cibles, et ainsi de suite.

[![entrez la description de l'image ici][1]][1]

**Producteur/Consommateur asynchrone avec un producteur limité**

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
    
**Exposer le statut d'un agent**
    
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
    
[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/ZStaY.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

##BufferBlock<T>
(File d'attente FIFO : les données qui entrent sont les données qui sortent)

En bref, BufferBlock<T> fournit un tampon illimité ou limité pour stocker les instances de T.
Vous pouvez « publier » des instances de T dans le bloc, ce qui entraîne le stockage des données publiées dans l'ordre premier entré, premier sorti (FIFO) par le bloc.
Vous pouvez "recevoir" du bloc, ce qui vous permet d'obtenir de manière synchrone ou asynchrone des instances de T précédemment stockées ou disponibles dans le futur (encore une fois, FIFO).

[![entrez la description de l'image ici][1]][1]

**Producteur/Consommateur asynchrone avec un producteur limité**

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

[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/S5vXJ.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## JoinBlock<T1, T2,…>
(Collecte 2-3 entrées et les combine dans un Tuple)

Comme BatchBlock<T>, JoinBlock<T1, T2, …> est capable de regrouper des données provenant de plusieurs sources de données. En fait, c'est l'objectif principal de JoinBlock<T1, T2, …>.

Par exemple, un JoinBlock<string, double, int> est un ISourceBlock<Tuple<string, double, int>>.

Comme avec BatchBlock<T>, JoinBlock<T1, T2,…> est capable de fonctionner en mode gourmand et non gourmand.

- Dans le mode gourmand par défaut, toutes les données proposées aux cibles sont acceptées, même si l'autre cible n'a pas les données nécessaires pour former un tuple.
- En mode non gourmand, les cibles du bloc reporteront les données jusqu'à ce que toutes les cibles aient reçu les données nécessaires pour créer un tuple, auquel cas le bloc s'engagera dans un protocole de validation en deux phases pour récupérer de manière atomique tous les éléments nécessaires à partir des sources . Ce report permet à une autre entité de consommer les données entre-temps afin de permettre à l'ensemble du système d'avancer.

[![entrez la description de l'image ici][1]][1]

**Traitement des demandes avec un nombre limité d'objets regroupés**

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
    
[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/mmXJ8.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## WriteOnceBlock<T>
(Variable en lecture seule : mémorise son premier élément de données et en transmet des copies en tant que sortie. Ignore tous les autres éléments de données)

Si BufferBlock<T> est le bloc le plus fondamental dans TPL Dataflow, WriteOnceBlock<T> est le plus simple.
Il stocke au plus une valeur, et une fois que cette valeur a été définie, elle ne sera jamais remplacée ou écrasée.

Vous pouvez considérer WriteOnceBlock<T> comme étant similaire à une variable membre en lecture seule en C#, sauf qu'au lieu d'être uniquement paramétrable dans un constructeur puis d'être immuable, elle n'est paramétrable qu'une seule fois et est ensuite immuable.

[![entrez la description de l'image ici][1]][1]

**Diviser les sorties potentielles d'une tâche**

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
    
[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/7M5Mp.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## BatchJoinBlock<T1, T2,…>
(Collecte un certain nombre d'éléments totaux à partir de 2-3 entrées et les regroupe dans un Tuple de collections d'éléments de données)

BatchedJoinBlock<T1, T2,…> est en quelque sorte une combinaison de BatchBlock<T> et JoinBlock<T1, T2,…>.
Alors que JoinBlock<T1, T2,…> est utilisé pour agréger une entrée de chaque cible dans un tuple, et que BatchBlock<T> est utilisé pour agréger N entrées dans une collection, BatchedJoinBlock<T1, T2,…> est utilisé pour rassembler N entrées de toutes les cibles dans des tuples de collections.

[![entrez la description de l'image ici][1]][1]

**Dispersion/Rassemblement**

Considérez un problème de dispersion/regroupement où N opérations sont lancées, dont certaines peuvent réussir et produire des sorties de chaîne, et d'autres peuvent échouer et produire des exceptions.

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

[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/FSgue.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformBlock<TInput, TOutput>
(Sélectionner, un à un)

Comme avec ActionBlock<TInput>, TransformBlock<TInput, TOutput> permet à l'exécution d'un délégué d'effectuer une action pour chaque donnée d'entrée ; **contrairement à ActionBlock<TInput>, ce traitement a une sortie.** Ce délégué peut être un Func<TInput, TOutput>, auquel cas le traitement de cet élément est considéré comme terminé lorsque le délégué revient, ou il peut s'agir d'un Func <TInput,Task<TOutput>>, auquel cas le traitement de cet élément est considéré comme terminé non pas lorsque le délégué revient mais lorsque la tâche renvoyée se termine.
Pour ceux qui connaissent LINQ, il est quelque peu similaire à Select () en ce sens qu'il prend une entrée, transforme cette entrée d'une manière ou d'une autre, puis produit une sortie.

Par défaut, TransformBlock<TInput, TOutput> traite ses données séquentiellement avec un MaxDegreeOfParallelism égal à 1.
En plus de recevoir une entrée mise en mémoire tampon et de la traiter, ce bloc prendra également toute sa sortie traitée et la mettra en mémoire tampon (données qui n'ont pas été traitées et données qui ont été traitées).

Il a 2 tâches : une pour traiter les données et une pour pousser les données vers le bloc suivant.

[![entrez la description de l'image ici][1]][1]

**Un pipeline simultané**

    var compressor = new TransformBlock<byte[], byte[]>(input => Compress(input));
    var encryptor = new TransformBlock<byte[], byte[]>(input => Encrypt(input));
    
    compressor.LinkTo(Encryptor); 

[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/jQcFo.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## TransformManyBlock<TInput, TOutput>
(SelectMany, 1-m : les résultats de ce mappage sont "aplatis", tout comme le SelectMany de LINQ)

TransformManyBlock<TInput, TOutput> est très similaire à TransformBlock<TInput, TOutput>.
La principale différence est que, alors qu'un TransformBlock<TInput, TOutput> produit une et une seule sortie pour chaque entrée, TransformManyBlock<TInput, TOutput> produit n'importe quel nombre (zéro ou plus) de sorties pour chaque entrée. Comme avec ActionBlock<TInput> et TransformBlock<TInput, TOutput>, ce traitement peut être spécifié à l'aide de délégués, à la fois pour le traitement synchrone et asynchrone.

Un Func<TInput, IEnumerable<TOutput>> est utilisé pour synchrone, et un Func<TInput, Task<IEnumerable<TOutput>>> est utilisé pour asynchrone. Comme pour ActionBlock<TInput> et TransformBlock<TInput, TOutput>, TransformManyBlock<TInput, TOutput> utilise par défaut le traitement séquentiel, mais peut être configuré autrement.

Le délégué de mappage renvoie une collection d'éléments, qui sont insérés individuellement dans le tampon de sortie.

[![entrez la description de l'image ici][1]][1]

**Crawler Web asynchrone**

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
    
**Développement d'un énumérable en ses éléments constitutifs**

    var expanded = new TransformManyBlock<T[], T>(array => array);

**Filtrage en passant de 1 à 0 ou 1 éléments**

    public IPropagatorBlock<T> CreateFilteredBuffer<T>(Predicate<T> filter)
    {
        return new TransformManyBlock<T, T>(item =>
            filter(item) ? new [] { item } : Enumerable.Empty<T>());
    }

[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/h7mip.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

## BatchBlock<T>
(Regroupe un certain nombre d'éléments de données séquentiels dans des collections d'éléments de données)

BatchBlock<T> combine N éléments uniques en un seul élément de lot, représenté sous la forme d'un tableau d'éléments. Une instance est créée avec une taille de lot spécifique, et le bloc crée ensuite un lot dès qu'il reçoit ce nombre d'éléments, en sortant de manière asynchrone le lot vers le tampon de sortie.

BatchBlock<T> est capable de s'exécuter en mode gourmand et non gourmand.

- Dans le mode gourmand par défaut, tous les messages proposés au bloc à partir de n'importe quel nombre de sources sont acceptés et mis en mémoire tampon pour être convertis en lots.
- • En mode non gourmand, tous les messages sont différés depuis les sources jusqu'à ce que suffisamment de sources aient proposé des messages au bloc pour créer un lot. Ainsi, un BatchBlock<T> peut être utilisé pour recevoir 1 élément de chacune des N sources, N éléments d'une source et une myriade d'options intermédiaires.

[![entrez la description de l'image ici][1]][1]

** Regroupement des requêtes en groupes de 100 à soumettre à une base de données **

    var batchRequests = new BatchBlock<Request>(batchSize:100);
    var sendToDb = new ActionBlock<Request[]>(reqs => SubmitToDatabase(reqs));
    
    batchRequests.LinkTo(sendToDb);

**Création d'un lot une fois par seconde**

    var batch = new BatchBlock<T>(batchSize:Int32.MaxValue);
    new Timer(() => { batch.TriggerBatch(); }).Change(1000, 1000);
    
    
[Introduction au flux de données TPL par Stephen Toub][2]


[1] : http://i.stack.imgur.com/tLRyw.png
[2] : https://www.microsoft.com/en-us/download/details.aspx?id=14782

