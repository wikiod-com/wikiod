---
title: "Biblioteca paralela de tareas (TPL)"
slug: "biblioteca-paralela-de-tareas-tpl"
draft: false
images: []
weight: 9746
type: docs
toc: true
---

Propósito y casos de uso
=======
El propósito de Task Parallel Library es simplificar el proceso de escritura y mantenimiento de código multiproceso y paralelo.

Algunos casos de uso*:

- Mantener una interfaz de usuario receptiva ejecutando trabajo en segundo plano en una tarea separada
- Distribuir la carga de trabajo
- Permitir que una aplicación cliente envíe y reciba solicitudes al mismo tiempo (descanso, TCP/UDP, etc.)
- Leer y/o escribir varios archivos a la vez

*El código debe considerarse caso por caso para subprocesos múltiples. Por ejemplo, si un bucle solo tiene unas pocas iteraciones o solo realiza una pequeña parte del trabajo, la sobrecarga del paralelismo puede superar los beneficios.

**TPL con .Net 3.5**

El TPL también está disponible para .Net 3.5 incluido en un paquete NuGet, se llama Task Parallel Library.

## Bucle básico productor-consumidor (BlockingCollection)
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

Vale la pena señalar que si no llamas a `collection.CompleteAdding();`, puedes seguir agregando a la colección incluso si tu tarea de consumidor se está ejecutando. Simplemente llame a `collection.CompleteAdding();` cuando esté seguro de que no hay más adiciones. Esta funcionalidad se puede usar para hacer un patrón de productor múltiple a un consumidor único donde tiene múltiples fuentes que alimentan elementos en BlockingCollection<T> y un solo consumidor extrae elementos y hace algo con ellos. Si su BlockingCollection<T> está vacío antes de llamar a la adición completa, el Enumerable de `collection.GetConsumingEnumerable()` se bloqueará hasta que se agregue un nuevo elemento a la colección o BlockingCollection<T>.CompleteAdding(); se llama y la cola está vacía.
    
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

## Invocar.en.paralelo
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

## Tarea: instanciación básica y Espera
Se puede crear una tarea instanciando directamente la clase `Task`...

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

...o usando el método estático `Task.Run`:

    Console.WriteLine("Starting task...");
    var task = Task.Run(() =>
    {
        Console.WriteLine("Task code starting...");
        Thread.Sleep(2000);
        Console.WriteLine("...task code ending!");
    });
    task.Wait();
    Console.WriteLine("Task completed!");

Tenga en cuenta que solo en el primer caso es necesario invocar explícitamente `Start`.

## Tarea.CuandoTodo
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

## Paralelo.ParaCada
Este ejemplo usa `Parallel.ForEach` para calcular la suma de los números entre 1 y 10000 usando varios subprocesos. Para lograr la seguridad de subprocesos, `Interlocked.Add` se usa para sumar los números.

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
Este ejemplo usa `Parallel.For` para calcular la suma de los números entre 1 y 10000 usando varios subprocesos. Para lograr la seguridad de subprocesos, `Interlocked.Add` se usa para sumar los números.

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

## Tarea: Devolver un valor
La tarea que devuelve un valor tiene un tipo de devolución de <código>Tarea< TResult ></código> donde TResult es el tipo de valor que debe devolverse. Puede consultar el resultado de una tarea por su propiedad Result.

    Task<int> t = Task.Run(() => 
        {
            int sum = 0;

            for(int i = 0; i < 500; i++)
                sum += i;

            return sum;
        });

    Console.WriteLine(t.Result); // Outuput 124750

Si la tarea se ejecuta de forma asincrónica, esperar a que la tarea devuelva su resultado.

    public async Task DoSomeWork()
    {
        WebClient client = new WebClient();
        // Because the task is awaited, result of the task is assigned to response
        string response = await client.DownloadStringTaskAsync("http://somedomain.com");
    }

    

## Tarea: WaitAll y captura de variables
    var tasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() =>
    {
        Console.WriteLine("I'm task " + n);
        return n;
    })).ToArray();

    foreach(var task in tasks) task.Start();
    Task.WaitAll(tasks);

    foreach(var task in tasks)
        Console.WriteLine(task.Result);

## Tarea: EsperarCualquiera
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

**Nota:** El `WaitAll` final es necesario porque `WaitAny` no hace que se observen excepciones.

## Tarea: manejo de excepciones (usando Wait)
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

## Tarea: manejo de excepciones (sin usar Wait)
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

## Tarea: cancelar usando CancellationToken
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

Como alternativa a `ThrowIfCancellationRequested`, la solicitud de cancelación se puede detectar con `IsCancellationRequested` y se puede lanzar manualmente una `OperationCanceledException`:

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

Observe cómo se pasa el token de cancelación al constructor de la tarea en el parámetro `cancellationToken`. Esto es necesario para que la tarea pase al estado `Cancelado`, no al estado `Faulted`, cuando se invoca `ThrowIfCancellationRequested`. Además, por la misma razón, el token de cancelación se proporciona explícitamente en el constructor de `OperationCanceledException` en el segundo caso.

## Tarea.CuandoCualquiera
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

## Contexto de ejecución fluido con AsyncLocal
Cuando necesite pasar algunos datos de la tarea principal a sus tareas secundarias, para que fluya lógicamente con la ejecución, use `AsyncLocal` [clase][1]:

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

**Nota:** Como se puede ver en el ejemplo anterior, `AsynLocal.Value` tiene la semántica `copiar al leer`, pero si hace fluir algún tipo de referencia y cambia sus propiedades, afectará a otras tareas. Por lo tanto, la mejor práctica con `AsyncLocal` es usar tipos de valor o tipos inmutables.

[1]: https://msdn.microsoft.com/en-us/library/dn906268(v=vs.110).aspx

## Parallel.ForEach en VB.NET
    For Each row As DataRow In FooDataTable.Rows
        Me.RowsToProcess.Add(row)
    Next
    
    Dim myOptions As ParallelOptions = New ParallelOptions()
    myOptions.MaxDegreeOfParallelism = environment.processorcount
    
    Parallel.ForEach(RowsToProcess, myOptions, Sub(currentRow, state)
                                                   ProcessRowParallel(currentRow, state)
                                               End Sub)

