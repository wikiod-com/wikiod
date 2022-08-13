---
title: "Async-Await"
slug: "async-await"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

En C#, un método declarado `async` no se bloqueará dentro de un proceso síncrono, en caso de que esté utilizando operaciones basadas en E/S (por ejemplo, acceso web, trabajo con archivos, ...). Se puede esperar el resultado de dichos métodos marcados asíncronamente mediante el uso de la palabra clave `await`.

Un método `async` puede devolver `void`, `Task` o `Task<T>`.

El tipo de retorno `Tarea` esperará a que finalice el método y el resultado será `vacío`. `Task<T>` devolverá un valor del tipo `T` después de que se complete el método.

Los métodos `async` deberían devolver `Task` o `Task<T>`, en lugar de `void`, en casi todas las circunstancias. Los métodos `async void` no se pueden `esperar`, lo que conduce a una variedad de problemas. El único escenario en el que un `async` debería devolver `void` es en el caso de un controlador de eventos.

`async`/`await` funciona al transformar su método `async` en una máquina de estado. Lo hace mediante la creación de una estructura entre bastidores que almacena el estado actual y cualquier contexto (como variables locales), y expone un método `MoveNext()` para avanzar estados (y ejecutar cualquier código asociado) cada vez que se completa una espera esperada.

## Operador de espera y palabra clave asíncrona
El operador `await` y la palabra clave `async` se juntan:

> El método asíncrono en el que se usa **await** debe ser modificado por
> la palabra clave **async**.

Lo contrario no siempre es cierto: puede marcar un método como `async` sin usar `await` en su cuerpo.

Lo que `await` en realidad hace es suspender la ejecución del código hasta que se complete la tarea esperada; se puede esperar cualquier tarea.

**Nota:** no puede esperar el método asíncrono que no devuelve nada (vacío).

En realidad, la palabra 'suspende' es un poco engañosa porque no solo se detiene la ejecución, sino que el subproceso puede quedar libre para ejecutar otras operaciones. Bajo el capó, `await` se implementa con un poco de magia del compilador: divide un método en dos partes: antes y después de `await`. La última parte se ejecuta cuando se completa la tarea esperada.

Si ignoramos algunos detalles importantes, el compilador hace esto por usted:

    public async Task<TResult> DoIt()
    {
        // do something and acquire someTask of type Task<TSomeResult>  
        var awaitedResult = await someTask;
        // ... do something more and produce result of type TResult
        return result;
    }

se convierte en:

    public Task<TResult> DoIt()
    {
        // ...
        return someTask.ContinueWith(task => {
            var result = ((Task<TSomeResult>)task).Result;
            return DoIt_Continuation(result);
        });
    }
    
    private TResult DoIt_Continuation(TSomeResult awaitedResult)
    {
        // ...
    }

Cualquier método habitual se puede convertir en asíncrono de la siguiente manera:

    await Task.Run(() => YourSyncMethod());

Esto puede ser ventajoso cuando necesita ejecutar un método de ejecución prolongada en el subproceso de la interfaz de usuario sin congelar la interfaz de usuario.

Pero hay una observación muy importante aquí: **Asíncrono no siempre significa concurrente (paralelo o incluso multihilo).** Incluso en un solo hilo, `async`-`await` aún permite el código asíncrono. Por ejemplo, vea este [programador de tareas] personalizado [1]. Un programador de tareas tan 'loco' puede simplemente convertir las tareas en funciones que se llaman dentro del procesamiento de bucle de mensajes.

Necesitamos preguntarnos: ¿Qué hilo ejecutará la continuación de nuestro método `DoIt_Continuation`?

Por defecto, el operador `await` programa la ejecución de la continuación con el [contexto de sincronización] actual[2]. Significa que, de forma predeterminada, la continuación de WinForms y WPF se ejecuta en el subproceso de la interfaz de usuario. Si, por alguna razón, necesita cambiar este comportamiento, use [método][3] `Task.ConfigureAwait()`:

    await Task.Run(() => YourSyncMethod()).ConfigureAwait(continueOnCapturedContext: false);

[1]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.taskscheduler(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.synchronizationcontext(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx

## Llamadas simultáneas
Es posible esperar varias llamadas al mismo tiempo invocando primero las tareas en espera y *luego* esperándolas.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await firstTask;
        await secondTask;
    }

Alternativamente, `Task.WhenAll` se puede usar para agrupar varias tareas en una sola `Tarea`, que se completa cuando se completan todas las tareas pasadas.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await Task.WhenAll(firstTask, secondTask);
    }

También puede hacer esto dentro de un bucle, por ejemplo:

    List<Task> tasks = new List<Task>();
    while (something) {
        // do stuff
        Task someAsyncTask = someAsyncMethod();
        tasks.Add(someAsyncTask);
    }

    await Task.WhenAll(tasks);

Para obtener resultados de una tarea después de esperar varias tareas con Task.WhenAll, simplemente espere la tarea nuevamente. Dado que la tarea ya está completa, simplemente devolverá el resultado.

    var task1 = SomeOpAsync();
    var task2 = SomeOtherOpAsync();

    await Task.WhenAll(task1, task2);

    var result = await task2;


Además, `Task.WhenAny` se puede usar para ejecutar varias tareas en paralelo, como `Task.WhenAll` anterior, con la diferencia de que este método se completará cuando se complete *cualquiera* de las tareas proporcionadas.

    public async Task RunConcurrentTasksWhenAny()
    {
        var firstTask = TaskOperation("#firstTask executed");
        var secondTask = TaskOperation("#secondTask executed");
        var thirdTask = TaskOperation("#thirdTask executed");
        await Task.WhenAny(firstTask, secondTask, thirdTask);
    }

La 'Tarea' devuelta por 'RunConcurrentTasksWhenAny' se completará cuando se complete cualquiera de las 'primera tarea', 'segunda tarea' o 'tercera tarea'.



## Probar/Atrapar/Finalmente
<!-- si la versión [gte 6.0] -->

A partir de C# 6.0, la palabra clave `await` ahora se puede usar dentro de un bloque `catch` y `finally`.

    try {
       var client = new AsyncClient();
       await client.DoSomething();
    } catch (MyException ex) {
       await client.LogExceptionAsync();
       throw;
    } finally {
       await client.CloseAsync();
    }
<!-- versión final si -->

<!-- si la versión [gte 5.0] [lt 6.0] -->

Antes de C# 6.0, tendría que hacer algo como lo siguiente. Tenga en cuenta que 6.0 también limpió las comprobaciones nulas con el [operador de propagación nula][1].

    AsynClient client;
    MyException caughtException;
    try {
         client = new AsyncClient();
         await client.DoSomething();
    } catch (MyException ex) {
         caughtException = ex;
    }
    
    if (client != null) {
        if (caughtException != null) {
           await client.LogExceptionAsync();
        }
        await client.CloseAsync();
        if (caughtException != null) throw caughtException;
    }
<!-- versión final si -->

Tenga en cuenta que si espera una tarea no creada por `async` (por ejemplo, una tarea creada por `Task.Run`), algunos depuradores pueden fallar en las excepciones lanzadas por la tarea incluso cuando aparentemente es manejado por el try/catch circundante. Esto sucede porque el depurador considera que no está controlado con respecto al código de usuario. En Visual Studio, hay una opción llamada ["Solo mi código"][2], que se puede deshabilitar para evitar que el depurador se rompa en tales situaciones.

[1]: https://www.wikiod.com/es/docs/c%23/24/c-6-features/51/null-propagation#t=201511271308000980289
[2]: https://msdn.microsoft.com/en-us/library/dn457346.aspx "Documentación de Just My Code en MSDN"


## Devolver una tarea sin esperar
Los métodos que realizan operaciones asincrónicas no necesitan usar `await` si:

* Solo hay una llamada asíncrona dentro del método
* La llamada asíncrona está al final del método
* La excepción de captura/manejo que puede ocurrir dentro de la tarea no es necesaria

Considere este método que devuelve una `Tarea`:

    public async Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return await dataStore.GetByKeyAsync(lookupKey);
    }

Si `GetByKeyAsync` tiene la misma firma que `GetUserAsync` (devolviendo una `Tarea<Usuario>`), el método se puede simplificar:

    public Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return dataStore.GetByKeyAsync(lookupKey);
    }

En este caso, no es necesario marcar el método como `asincrónico`, aunque esté realizando una operación asincrónica. La tarea devuelta por `GetByKeyAsync` se pasa directamente al método de llamada, donde estará en `espera`.

**Importante**: Al devolver la `Tarea` en lugar de esperarla, cambia el comportamiento de excepción del método, ya que no lanzará la excepción dentro del método que inicia la tarea sino en el método que la espera.

    public Task SaveAsync()
    {
        try {
            return dataStore.SaveChangesAsync();
        }
        catch(Exception ex)
        {
            // this will never be called
            logger.LogException(ex);
        }
    }

    // Some other code calling SaveAsync()

    // If exception happens, it will be thrown here, not inside SaveAsync()
    await SaveAsync();

Esto mejorará el rendimiento, ya que le ahorrará al compilador la generación de una máquina de estado extra **asincrónica**.

## Configuración de Web.config para apuntar a 4.5 para un comportamiento asíncrono correcto.
El tiempo de ejecución web.config system.web.http debe apuntar a 4.5 para garantizar que el subproceso alquile el contexto de la solicitud antes de reanudar su método asíncrono.

    <httpRuntime targetFramework="4.5" />

Async y await tienen un comportamiento indefinido en ASP.NET antes de 4.5. Async/await se reanudará en un subproceso arbitrario que puede no tener el contexto de la solicitud. Las aplicaciones bajo carga fallarán aleatoriamente con excepciones de referencia nula que acceden a HttpContext después de await. http://stackoverflow.com/questions/24956178/using-httpcontext-current-in-webapi-is-dangerous-because-of-async

## Async/await solo mejorará el rendimiento si permite que la máquina realice un trabajo adicional
Considere el siguiente código:

    public async Task MethodA()
    {
         await MethodB();
         // Do other work
    }

    public async Task MethodB()
    {
         await MethodC();
         // Do other work
    }

    public async Task MethodC()
    {
         // Or await some other async work
         await Task.Delay(100);
    }

Esto no funcionará mejor que

    public void MethodA()
    {
         MethodB();
         // Do other work
    }

    public void MethodB()
    {
         MethodC();
         // Do other work
    }

    public void MethodC()
    {
         Thread.Sleep(100);
    }
El objetivo principal de async/await es permitir que la máquina realice un trabajo adicional; por ejemplo, permitir que el subproceso de llamada realice otro trabajo mientras espera el resultado de alguna operación de E/S. En este caso, nunca se permite que el subproceso de llamada haga más trabajo del que hubiera podido hacer de otro modo, por lo que no hay ganancia de rendimiento con respecto a simplemente llamar a `MethodA()`, `MethodB()` ​​y `MethodC()`. sincrónicamente

## Llamadas consecutivas simples
    public async Task<JobResult> GetDataFromWebAsync()
    {
      var nextJob = await _database.GetNextJobAsync();
      var response = await _httpClient.GetAsync(nextJob.Uri);
      var pageContents = await response.Content.ReadAsStringAsync();
      return await _database.SaveJobResultAsync(pageContents);
    }

Lo más importante a tener en cuenta aquí es que, si bien cada método `await`-ed se llama de forma asíncrona, y en el momento de esa llamada, el control se devuelve al sistema, el flujo dentro del método es lineal y no requiere ningún tratamiento especial. por asincronía. Si alguno de los métodos llamados falla, la excepción se procesará "como se esperaba", lo que en este caso significa que la ejecución del método se cancelará y la excepción subirá a la pila.



## El bloqueo en código asíncrono puede causar interbloqueos
Es una mala práctica bloquear las llamadas asincrónicas, ya que puede provocar interbloqueos en entornos que tienen un contexto de sincronización. La mejor práctica es usar async/await "hasta el final". Por ejemplo, el siguiente código de Windows Forms provoca un interbloqueo:

    private async Task<bool> TryThis()
    {
        Trace.TraceInformation("Starting TryThis");
        await Task.Run(() =>
        {
            Trace.TraceInformation("In TryThis task");
            for (int i = 0; i < 100; i++)
            {
                // This runs successfully - the loop runs to completion
                Trace.TraceInformation("For loop " + i);
                System.Threading.Thread.Sleep(10);
            }
        });

        // This never happens due to the deadlock
        Trace.TraceInformation("About to return");
        return true;
    }

    // Button click event handler
    private void button1_Click(object sender, EventArgs e)
    {
        // .Result causes this to block on the asynchronous call
        bool result = TryThis().Result;
        // Never actually gets here
        Trace.TraceInformation("Done with result");
    }
Básicamente, una vez que se completa la llamada asíncrona, espera a que el contexto de sincronización esté disponible. Sin embargo, el controlador de eventos "retiene" el contexto de sincronización mientras espera que se complete el método `TryThis()`, lo que provoca una espera circular.

Para solucionar esto, el código debe modificarse para

    private async void button1_Click(object sender, EventArgs e)
    {
      bool result = await TryThis();
      Trace.TraceInformation("Done with result");
    }

Nota: los controladores de eventos son el único lugar donde se debe usar `async void` (porque no puede esperar un método `async void`).




