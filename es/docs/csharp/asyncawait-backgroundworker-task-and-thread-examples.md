---
title: "Ejemplos de Asyncawait, Backgroundworker, Task y Thread"
slug: "ejemplos-de-asyncawait-backgroundworker-task-y-thread"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

Para ejecutar cualquiera de estos ejemplos, simplemente llámelos así:

    static void Main()
    {
        new Program().ProcessDataAsync();
        Console.ReadLine();
    }

## ASP.NET Configurar Esperar
Cuando ASP.NET maneja una solicitud, se asigna un subproceso del grupo de subprocesos y se crea un **contexto de solicitud**. El contexto de la solicitud contiene información sobre la solicitud actual a la que se puede acceder a través de la propiedad estática ```HttpContext.Current```. A continuación, el contexto de solicitud para la solicitud se asigna al subproceso que gestiona la solicitud.

Un contexto de solicitud dado **solo puede estar activo en un hilo a la vez**.

Cuando la ejecución llega a ```await```, el subproceso que maneja una solicitud se devuelve al grupo de subprocesos mientras se ejecuta el método asincrónico y el contexto de la solicitud queda libre para que lo utilice otro subproceso.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        var products = await dbContext.Products.ToListAsync();

        // Execution resumes on a "random" thread from the pool
        // Execution continues using the original request context.
        return View(products);
    }

Cuando la tarea se completa, el grupo de subprocesos asigna otro subproceso para continuar con la ejecución de la solicitud. A continuación, el contexto de la solicitud se asigna a este subproceso. Este puede o no ser el hilo original.

### Bloqueo ###

Cuando se espera **sincrónicamente** el resultado de una llamada de método ```async``, pueden surgir interbloqueos. Por ejemplo, el siguiente código resultará en un interbloqueo cuando se llame ```IndexSync()```:

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        List<Product> products = await dbContext.Products.ToListAsync();
    
        // Execution resumes on a "random" thread from the pool
        return View(products);
    }

    public ActionResult IndexSync()
    {
        Task<ActionResult> task = Index();

        // Block waiting for the result synchronously
        ActionResult result = Task.Result;

        return result;       
    }

Esto se debe a que, por defecto, la tarea esperada, en este caso ```db.Products.ToListAsync()``` capturará el contexto (en el caso de ASP.NET, el contexto de la solicitud) e intentará utilizarlo una vez que lo haya terminado.

Cuando toda la pila de llamadas es asíncrona, no hay problema porque, una vez que se alcanza ```await```, se libera el hilo original, liberando el contexto de la solicitud.

Cuando bloqueamos sincrónicamente usando ```Task.Result``` o ```Task.Wait()``` (u otros métodos de bloqueo), el hilo original todavía está activo y retiene el contexto de la solicitud. El método esperado todavía funciona de forma asíncrona y una vez que la devolución de llamada intenta ejecutarse, es decir, una vez que la tarea esperada ha regresado, intenta obtener el contexto de la solicitud.

Por lo tanto, el interbloqueo surge porque mientras el subproceso de bloqueo con el contexto de solicitud está esperando que se complete la operación asincrónica, la operación asincrónica está tratando de obtener el contexto de solicitud para completarse.

### ConfigurarEsperar ###

De forma predeterminada, las llamadas a una tarea esperada capturarán el contexto actual e intentarán reanudar la ejecución en el contexto una vez completada.

Usando ```ConfigureAwait(false)``` esto se puede prevenir y se pueden evitar interbloqueos.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        List<Product> products = await dbContext.Products.ToListAsync().ConfigureAwait(false);
    
        // Execution resumes on a "random" thread from the pool without the original request context
        return View(products);
    }
    
    public ActionResult IndexSync()
    {
        Task<ActionResult> task = Index();
    
        // Block waiting for the result synchronously
        ActionResult result = Task.Result;
    
        return result;       
    }

Esto puede evitar interbloqueos cuando es necesario bloquear en código asincrónico, sin embargo, esto tiene el costo de perder el contexto en la continuación (código después de la llamada a esperar).

En ASP.NET esto significa que si su código después de una llamada a ```await someTask.ConfigureAwait(false);``` intenta acceder a la información del contexto, por ejemplo ```HttpContext.Current.User``` entonces la información se ha perdido. En este caso ```HttpContext.Current``` es nulo. Por ejemplo:

    public async Task<ActionResult> Index()
    {
        // Contains information about the user sending the request
        var user = System.Web.HttpContext.Current.User;

        using (var client = new HttpClient())
        {
            await client.GetAsync("http://google.com").ConfigureAwait(false);
        }

        // Null Reference Exception, Current is null
        var user2 = System.Web.HttpContext.Current.User;

        return View();
    }

Si se usa ```ConfigureAwait(true)``` (equivalente a no tener ConfigureAwait en absoluto), tanto ```user``` como ```user2``` se completan con los mismos datos.

Por esta razón, a menudo se recomienda usar ```ConfigureAwait(false)``` en el código de la biblioteca donde ya no se usa el contexto.

## Asíncrono/espera
Vea a continuación un ejemplo simple de cómo usar async/await para hacer algunas cosas que requieren mucho tiempo en un proceso en segundo plano mientras mantiene la opción de hacer otras cosas que no necesitan esperar a que se completen las cosas que requieren mucho tiempo.

Sin embargo, si necesita trabajar con el resultado del método intensivo de tiempo más tarde, puede hacerlo esperando la ejecución.

    public async Task ProcessDataAsync()
    {
        // Start the time intensive method
        Task<int> task = TimeintensiveMethod(@"PATH_TO_SOME_FILE");

        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");

        // Wait for TimeintensiveMethod to complete and get its result
        int x = await task;
        Console.WriteLine("Count: " + x);
    }

    private async Task<int> TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = await reader.ReadToEndAsync();

            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something as a "result"
        return new Random().Next(100);
    }

## Trabajador de fondo
Vea a continuación un ejemplo simple de cómo usar un objeto `BackgroundWorker` para realizar operaciones que requieren mucho tiempo en un subproceso en segundo plano.

Necesitas:
1. Defina un método de trabajo que haga el trabajo que requiere mucho tiempo y llámelo desde un controlador de eventos para el evento `DoWork` de un `BackgroundWorker`.
3. Inicie la ejecución con `RunWorkerAsync`. Cualquier argumento requerido por el método de trabajo adjunto a `DoWork` se puede pasar a través del parámetro `DoWorkEventArgs` a `RunWorkerAsync`.

Además del evento `DoWork`, la clase `BackgroundWorker` también define dos eventos que deben usarse para interactuar con la interfaz de usuario. Estos son opcionales.

* El evento `RunWorkerCompleted` se activa cuando se completan los controladores `DoWork`.
* El evento `ProgressChanged` se activa cuando se llama al método `ReportProgress`.


    public void ProcessDataAsync()
    {
        // Start the time intensive method
        BackgroundWorker bw = new BackgroundWorker();
        bw.DoWork += BwDoWork;
        bw.RunWorkerCompleted += BwRunWorkerCompleted;
        bw.RunWorkerAsync(@"PATH_TO_SOME_FILE");

        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    }

    // Method that will be called after BwDoWork exits
    private void BwRunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
    {
        // we can access possible return values of our Method via the Parameter e
        Console.WriteLine("Count: " + e.Result);
    }

    // execution of our time intensive Method
    private void BwDoWork(object sender, DoWorkEventArgs e)
    {
        e.Result = TimeintensiveMethod(e.Argument);
    }

    private int TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = reader.ReadToEnd();

           for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something as a "result"
        return new Random().Next(100);
    }


## Tarea
Vea a continuación un ejemplo simple de cómo usar una 'Tarea' para hacer cosas que requieren mucho tiempo en un proceso en segundo plano.

Todo lo que necesita hacer es envolver su método intensivo de tiempo en una llamada `Task.Run()`.

    

    public void ProcessDataAsync()
    {
        // Start the time intensive method
        Task<int> t = Task.Run(() => TimeintensiveMethod(@"PATH_TO_SOME_FILE"));
    
        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    
        Console.WriteLine("Count: " + t.Result);
    }
    
    private int TimeintensiveMethod(object file)
    {
        Console.WriteLine("Start TimeintensiveMethod.");
    
        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file.ToString()))
        {
            string s = reader.ReadToEnd();
    
            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");
    
        // return something as a "result"
        return new Random().Next(100);
    }

## Hilo
Vea a continuación un ejemplo simple de cómo usar un 'Subproceso' para hacer cosas que requieren mucho tiempo en un proceso en segundo plano.

    

    public async void ProcessDataAsync()
    {
        // Start the time intensive method
        Thread t = new Thread(TimeintensiveMethod);
    
        // Control returns here before TimeintensiveMethod returns
        Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
    }
    
    private void TimeintensiveMethod()
    {
        Console.WriteLine("Start TimeintensiveMethod.");
    
        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(@"PATH_TO_SOME_FILE"))
        {
            string v = reader.ReadToEnd();
    
            for (int i = 0; i < 10000; i++)
                v.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");
    }

Como puede ver, no podemos devolver un valor de nuestro `TimeIntensiveMethod` porque `Thread` espera un método vacío como parámetro.

Para obtener un valor de retorno de un 'Subproceso', use un evento o lo siguiente:

    int ret;
    Thread t= new Thread(() => 
    {
        Console.WriteLine("Start TimeintensiveMethod.");

        // Do some time intensive calculations...
        using (StreamReader reader = new StreamReader(file))
        {
            string s = reader.ReadToEnd();

            for (int i = 0; i < 10000; i++)
                s.GetHashCode();
        }
        Console.WriteLine("End TimeintensiveMethod.");

        // return something to demonstrate the coolness of await-async
        ret = new Random().Next(100);
    });

    t.Start();
    t.Join(1000);
    Console.Writeline("Count: " + ret);

## Tarea "ejecutar y olvidar" extensión
En ciertos casos (por ejemplo, registro), puede ser útil ejecutar la tarea y no esperar el resultado. La siguiente extensión permite ejecutar tareas y continuar con la ejecución del resto del código:

    public static class TaskExtensions
    {
        public static async void RunAndForget(
            this Task task, Action<Exception> onException = null)
        {
            try
            {
                await task;
            }
            catch (Exception ex)
            {
                onException?.Invoke(ex);
            }
        }
    }

El resultado se espera solo dentro del método de extensión. Dado que se usa `async`/`await`, es posible capturar una excepción y llamar a un método opcional para manejarla.

Un ejemplo de cómo usar la extensión:

    var task = Task.FromResult(0); // Or any other task from e.g. external lib.
    task.RunAndForget(
        e =>
        {
            // Something went wrong, handle it.
        });

