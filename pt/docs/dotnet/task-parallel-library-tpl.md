---
title: "Biblioteca Paralela de Tarefas (TPL)"
slug: "biblioteca-paralela-de-tarefas-tpl"
draft: false
images: []
weight: 9746
type: docs
toc: true
---

Finalidade e casos de uso
=======
O objetivo da Biblioteca Paralela de Tarefas é simplificar o processo de escrever e manter código paralelo e multithread.

Alguns casos de uso*:

- Mantendo uma interface do usuário responsiva executando o trabalho em segundo plano em uma tarefa separada
- Distribuir a carga de trabalho
- Permitir que um aplicativo cliente envie e receba solicitações ao mesmo tempo (rest, TCP/UDP, ect)
- Ler e/ou gravar vários arquivos de uma só vez

*O código deve ser considerado caso a caso para multithreading. Por exemplo, se um loop tiver apenas algumas iterações ou fizer apenas uma pequena quantidade de trabalho, a sobrecarga do paralelismo pode superar os benefícios.

**TPL com .Net 3.5**

O TPL também está disponível para .Net 3.5 incluído em um pacote NuGet, é chamado de Biblioteca Paralela de Tarefas.

## Loop produtor-consumidor básico (BlockingCollection)
    var collection = new BlockingCollection<int>(5);
    var random = new Random();
    
    var producerTask = Task.Run(() => {
        for(int item=1; item<=10; item++) 
        {
            collection.Add(item);
            Console.WriteLine("Produced: " + item);
            Thread.Sleep(random.Next(10,1000));
        }
        collection.CompleteAdding();
        Console.WriteLine("Producer completed!");
    });

Vale a pena notar que se você não chamar `collection.CompleteAdding();`, você poderá continuar adicionando à coleção mesmo se sua tarefa de consumidor estiver em execução. Basta chamar `collection.CompleteAdding();` quando tiver certeza de que não há mais adições. Essa funcionalidade pode ser usada para criar um padrão Multiple Producer para um Single Consumer onde você tem várias fontes alimentando itens no BlockingCollection<T> e um único consumidor retirando itens e fazendo algo com eles. Se seu BlockingCollection<T> estiver vazio antes de você chamar a adição completa, o Enumerable de `collection.GetConsumingEnumerable()` será bloqueado até que um novo item seja adicionado à coleção ou BlockingCollection<T>.CompleteAdding(); é chamado e a fila está vazia.
    
    var consumerTask = Task.Run(() => {
        foreach(var item in collection.GetConsumingEnumerable())
        {
            Console.WriteLine("Consumed: " + item);
            Thread.Sleep(random.Next(10,1000));
        }
        Console.WriteLine("Consumer completed!");
    });
      
    Task.WaitAll(producerTask, consumerTask);
           
    Console.WriteLine("Everything completed!");

## Paralelo.Invoke
    var actions = Enumerable.Range(1, 10).Select(n => new Action(() =>
    {
        Console.WriteLine("I'm task " + n);
        if((n & 1) == 0)
            throw new Exception("Exception from task " + n);
    })).ToArray();

    try
    {
        Parallel.Invoke(actions);
    }
    catch(AggregateException ex)
    {
        foreach(var inner in ex.InnerExceptions)
            Console.WriteLine("Task failed: " + inner.Message);
    }

## Tarefa: instanciação básica e espera
Uma tarefa pode ser criada instanciando diretamente a classe `Task`...

    var task = new Task(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });

    Console.WriteLine("Starting task...");
    task.Start();
    task.Wait();
    Console.WriteLine("Task completed!");

...ou usando o método estático `Task.Run`:

    Console.WriteLine("Starting task...");
    var task = Task.Run(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });
    task.Wait();
    Console.WriteLine("Task completed!");

Note que apenas no primeiro caso é necessário invocar explicitamente `Start`.

## Tarefa.WhenAll
    var random = new Random();
    IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    }));

    Task<int[]> task = Task.WhenAll(tasks);
    int[] results = await task;

    Console.WriteLine(string.Join(",", results.Select(n => n.ToString())));
    // Output: 1,2,3,4,5

## Parallel.ForEach
Este exemplo usa `Parallel.ForEach` para calcular a soma dos números entre 1 e 10.000 usando vários segmentos. Para alcançar a segurança de thread, `Interlocked.Add` é usado para somar os números.

    using System.Threading;

    int Foo()
    {
        int total = 0;
        var numbers = Enumerable.Range(1, 10000).ToList();
        Parallel.ForEach(numbers, 
            () => 0, // initial value,
            (num, state, localSum) => num + localSum,
            localSum => Interlocked.Add(ref total, localSum));
        return total; // total = 50005000
    }


## Paralelo.Para
Este exemplo usa `Parallel.For` para calcular a soma dos números entre 1 e 10.000 usando vários segmentos. Para alcançar a segurança de thread, `Interlocked.Add` é usado para somar os números.

    using System.Threading;

    int Foo()
    {
        int total = 0;
        Parallel.For(1, 10001, 
            () => 0, // initial value,
            (num, state, localSum) => num + localSum,
            localSum => Interlocked.Add(ref total, localSum));
        return total; // total = 50005000
    }

## Tarefa: Retornando um valor
A tarefa que retorna um valor tem o tipo de retorno <code>Task< TResult ></code> onde TResult é o tipo de valor que precisa ser retornado. Você pode consultar o resultado de uma tarefa por sua propriedade Result.

    Task<int> t = Task.Run(() => 
        {
            int sum = 0;

            for(int i = 0; i < 500; i++)
                sum += i;

            return sum;
        });

    Console.WriteLine(t.Result); // Outuput 124750

Se a tarefa for executada de forma assíncrona, aguardar a tarefa retornará seu resultado.

    public async Task DoSomeWork()
    {
        WebClient client = new WebClient();
        // Because the task is awaited, result of the task is assigned to response
        string response = await client.DownloadStringTaskAsync("http://somedomain.com");
    }

    

## Tarefa: WaitAll e captura de variáveis
    var tasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    })).ToArray();

    foreach(var task in tasks) task.Start();
    Task.WaitAll(tasks);

    foreach(var task in tasks)
        Console.WriteLine(task.Result);

## Tarefa: WaitAny
    var allTasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() => n)).ToArray();
    var pendingTasks = allTasks.ToArray();

    foreach(var task in allTasks) task.Start();

    while(pendingTasks.Length > 0)
    {
        var finishedTask = pendingTasks[Task.WaitAny(pendingTasks)];
        Console.WriteLine("Task {0} finished", finishedTask.Result);
        pendingTasks = pendingTasks.Except(new[] {finishedTask}).ToArray();
    }

    Task.WaitAll(allTasks);

**Nota:** O `WaitAll` final é necessário porque `WaitAny` não faz com que exceções sejam observadas.

## Tarefa: tratamento de exceções (usando Wait)
    var task1 = Task.Run(() =>
    {
        Console.WriteLine("Task 1 code starting...");
        throw new Exception("Oh no, exception from task 1!!");
    });

    var task2 = Task.Run(() =>
    {
        Console.WriteLine("Task 2 code starting...");
        throw new Exception("Oh no, exception from task 2!!");
    });

    Console.WriteLine("Starting tasks...");
    try
    {
        Task.WaitAll(task1, task2);
    }
    catch(AggregateException ex)
    {
        Console.WriteLine("Task(s) failed!");
        foreach(var inner in ex.InnerExceptions)
            Console.WriteLine(inner.Message);
    }

    Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
    Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

## Tarefa: tratamento de exceções (sem usar Wait)
    var task1 = Task.Run(() =>
    {
        Console.WriteLine("Task 1 code starting...");
        throw new Exception("Oh no, exception from task 1!!");
    });

    var task2 = Task.Run(() =>
    {
        Console.WriteLine("Task 2 code starting...");
        throw new Exception("Oh no, exception from task 2!!");
    });

    var tasks = new[] {task1, task2};

    Console.WriteLine("Starting tasks...");
    while(tasks.All(task => !task.IsCompleted));

    foreach(var task in tasks)
    {
        if(task.IsFaulted)
            Console.WriteLine("Task failed: " +
                task.Exception.InnerExceptions.First().Message);
    }

    Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
    Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

## Tarefa: cancelando usando CancellationToken
    var cancellationTokenSource = new CancellationTokenSource();
    var cancellationToken = cancellationTokenSource.Token;

    var task = new Task((state) =>
        {
            int i = 1;
            var myCancellationToken = (CancellationToken)state;
            while(true)
            {
                Console.Write("{0} ", i++);
                Thread.Sleep(1000);
                myCancellationToken.ThrowIfCancellationRequested();
            }
        },
        cancellationToken: cancellationToken,
        state: cancellationToken);

    Console.WriteLine("Counting to infinity. Press any key to cancel!");
    task.Start();
    Console.ReadKey();

    cancellationTokenSource.Cancel();
    try
    {
        task.Wait();
    }
    catch(AggregateException ex)
    {
        ex.Handle(inner => inner is OperationCanceledException);
    }

    Console.WriteLine($"{Environment.NewLine}You have cancelled! Task status is: {task.Status}");
    //Canceled

Como alternativa a `ThrowIfCancellationRequested`, a solicitação de cancelamento pode ser detectada com `IsCancellationRequested` e uma `OperationCanceledException` pode ser lançada manualmente:

    //New task delegate
    int i = 1;
    var myCancellationToken = (CancellationToken)state;
    while(!myCancellationToken.IsCancellationRequested)
    {
        Console.Write("{0} ", i++);
        Thread.Sleep(1000);
    }
    Console.WriteLine($"{Environment.NewLine}Ouch, I have been cancelled!!");
    throw new OperationCanceledException(myCancellationToken);

Observe como o token de cancelamento é passado para o construtor da tarefa no parâmetro `cancellationToken`. Isso é necessário para que a tarefa mude para o estado `Cancelado`, não para o estado `Falha`, quando `ThrowIfCancellationRequested` for invocado. Além disso, pelo mesmo motivo, o token de cancelamento é fornecido explicitamente no construtor de `OperationCanceledException` no segundo caso.

## Tarefa.WhenAny
    var random = new Random();
    IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(async() =>
    {
        Console.WriteLine("I'm task " + n);
        await Task.Delay(random.Next(10,1000));
        return n;
    }));

    Task<Task<int>> whenAnyTask = Task.WhenAny(tasks);
    Task<int> completedTask = await whenAnyTask;
    Console.WriteLine("The winner is: task " + await completedTask);

    await Task.WhenAll(tasks);
    Console.WriteLine("All tasks finished!");

## Contexto de execução fluindo com AsyncLocal
Quando você precisar passar alguns dados da tarefa pai para suas tarefas filhas, para que flua logicamente com a execução, use `AsyncLocal` [class][1]:

    void Main()
    {
        AsyncLocal<string> user = new AsyncLocal<string>();
        user.Value = "initial user";
        
        // this does not affect other tasks - values are local relative to the branches of execution flow
        Task.Run(() => user.Value = "user from another task"); 
        
        var task1 = Task.Run(() =>
        {
            Console.WriteLine(user.Value); // outputs "initial user"
            Task.Run(() =>
            {
                // outputs "initial user" - value has flown from main method to this task without being changed
                Console.WriteLine(user.Value);
            }).Wait();

            user.Value = "user from task1";
 
            Task.Run(() =>
            {
                // outputs "user from task1" - value has flown from main method to task1
                // than value was changed and flown to this task.
                Console.WriteLine(user.Value);
            }).Wait();
        });
        
        task1.Wait();
        
        // ouputs "initial user" - changes do not propagate back upstream the execution flow    
        Console.WriteLine(user.Value); 
    }

**Observação:** Como pode ser visto no exemplo acima, `AsynLocal.Value` tem a semântica `copy on read`, mas se você fluir algum tipo de referência e alterar suas propriedades, afetará outras tarefas. Portanto, a melhor prática com `AsyncLocal` é usar tipos de valor ou tipos imutáveis.

[1]: https://msdn.microsoft.com/en-us/library/dn906268(v=vs.110).aspx

## Parallel.ForEach em VB.NET
    For Each row As DataRow In FooDataTable.Rows
        Me.RowsToProcess.Add(row)
    Next
    
    Dim myOptions As ParallelOptions = New ParallelOptions()
    myOptions.MaxDegreeOfParallelism = environment.processorcount
    
    Parallel.ForEach(RowsToProcess, myOptions, Sub(currentRow, state)
                                                   ProcessRowParallel(currentRow, state)
                                               End Sub)

