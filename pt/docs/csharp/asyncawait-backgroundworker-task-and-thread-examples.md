---
title: "Asyncawait, Backgroundworker, Exemplos de tarefas e threads"
slug: "asyncawait-backgroundworker-exemplos-de-tarefas-e-threads"
draft: false
images: []
weight: 9913
type: docs
toc: true
---

Para executar qualquer um desses exemplos, basta chamá-los assim:

    static void Main()
    {
        new Program().ProcessDataAsync();
        Console.ReadLine();
    }

## Configuração ASP.NET Aguarda
Quando o ASP.NET manipula uma solicitação, um thread é atribuído do pool de threads e um **contexto de solicitação** é criado. O contexto da requisição contém informações sobre a requisição atual que pode ser acessada através da propriedade estática ```HttpContext.Current```. O contexto de solicitação para a solicitação é então atribuído ao encadeamento que trata a solicitação.

Um determinado contexto de solicitação **pode estar ativo apenas em um encadeamento por vez**.

Quando a execução atinge ```await```, a thread que trata de uma solicitação é retornada ao pool de threads enquanto o método assíncrono é executado e o contexto da solicitação fica livre para outra thread usar.

    public async Task<ActionResult> Index()
    {
        // Execution on the initially assigned thread
        var products = await dbContext.Products.ToListAsync();

        // Execution resumes on a "random" thread from the pool
        // Execution continues using the original request context.
        return View(products);
    }

Quando a tarefa é concluída, o pool de encadeamentos atribui outro encadeamento para continuar a execução da solicitação. O contexto de solicitação é então atribuído a este encadeamento. Este pode ou não ser o tópico original.

### Bloqueio ###

Quando o resultado de uma chamada de método ```async``` é esperado por **sincronicamente**, deadlocks podem surgir. Por exemplo, o código a seguir resultará em um impasse quando ```IndexSync()``` for chamado:

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

Isso ocorre porque, por padrão, a tarefa aguardada, neste caso ```db.Products.ToListAsync()``` irá capturar o contexto (no caso do ASP.NET o contexto de requisição) e tentará usá-lo assim que tiver concluído.

Quando toda a pilha de chamadas é assíncrona, não há problema porque, uma vez que ```await``` é alcançado, a thread original é liberada, liberando o contexto da solicitação.

Quando bloqueamos de forma síncrona usando ```Task.Result``` ou ```Task.Wait()``` (ou outros métodos de bloqueio), a thread original ainda está ativa e retém o contexto da solicitação. O método awaited ainda opera de forma assíncrona e uma vez que o callback tenta ser executado, ou seja, uma vez que a tarefa awaited retorna, ele tenta obter o contexto da solicitação.

Portanto, o deadlock surge porque enquanto o thread de bloqueio com o contexto de solicitação está aguardando a conclusão da operação assíncrona, a operação assíncrona está tentando obter o contexto de solicitação para concluir.

### ConfigurarAguardar ###

Por padrão, as chamadas para uma tarefa aguardada capturarão o contexto atual e tentarão retomar a execução no contexto após a conclusão.

Usando ```ConfigureAwait(false)``` isto pode ser prevenido e deadlocks podem ser evitados.

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

Isso pode evitar deadlocks quando for necessário bloquear em código assíncrono, porém isso tem o custo de perder o contexto na continuação (código após a chamada para await).

No ASP.NET isso significa que se seu código seguindo uma chamada para ```await someTask.ConfigureAwait(false);``` tenta acessar informações do contexto, por exemplo ```HttpContext.Current.User``` então a informação foi perdida. Neste caso o ```HttpContext.Current``` é nulo. Por exemplo:

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

Se ```ConfigureAwait(true)``` for usado (equivalente a não ter ConfigureAwait) então tanto ```user``` quanto ```user2``` serão preenchidos com os mesmos dados.

Por esta razão é frequentemente recomendado usar ```ConfigureAwait(false)``` no código da biblioteca onde o contexto não é mais usado.

## Async/aguardar
Veja abaixo um exemplo simples de como usar async/await para fazer algumas coisas demoradas em um processo em segundo plano, mantendo a opção de fazer outras coisas que não precisam esperar que as coisas demoradas sejam concluídas.

No entanto, se você precisar trabalhar com o resultado do método intensivo de tempo posteriormente, poderá fazer isso aguardando a execução.

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

## Trabalhador em segundo plano
Veja abaixo um exemplo simples de como usar um objeto `BackgroundWorker` para realizar operações demoradas em um thread em segundo plano.

Você precisa:
1. Defina um método de trabalho que faça o trabalho intensivo de tempo e chame-o de um manipulador de eventos para o evento `DoWork` de um `BackgroundWorker`.
3. Inicie a execução com `RunWorkerAsync`. Qualquer argumento exigido pelo método de trabalho anexado a `DoWork` pode ser passado através do parâmetro `DoWorkEventArgs` para `RunWorkerAsync`.

Além do evento `DoWork`, a classe `BackgroundWorker` também define dois eventos que devem ser usados ​​para interagir com a interface do usuário. Estes são opcionais.

* O evento `RunWorkerCompleted` é acionado quando os manipuladores `DoWork` são concluídos.
* O evento `ProgressChanged` é acionado quando o método `ReportProgress` é chamado.


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


## Tarefa
Veja abaixo um exemplo simples de como usar uma `Tarefa` para fazer algumas coisas demoradas em um processo em segundo plano.

Tudo que você precisa fazer é envolver seu método intensivo em uma chamada `Task.Run()`.

    

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

## Fio
Veja abaixo um exemplo simples de como usar um `Thread` para fazer algumas coisas demoradas em um processo em segundo plano.

    

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

Como você pode ver, não podemos retornar um valor do nosso `TimeIntensiveMethod` porque `Thread` espera um método void como parâmetro.

Para obter um valor de retorno de um `Thread`, use um evento ou o seguinte:

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

## Extensão da tarefa "executar e esquecer"
Em certos casos (por exemplo, log) pode ser útil executar a tarefa e não aguardar o resultado. A extensão a seguir permite executar a tarefa e continuar a execução do código restante:

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

O resultado é esperado apenas dentro do método de extensão. Como `async`/`await` é usado, é possível capturar uma exceção e chamar um método opcional para tratá-la.

Um exemplo de como usar a extensão:

    var task = Task.FromResult(0); // Or any other task from e.g. external lib.
    task.RunAndForget(
        e =>
        {
            // Something went wrong, handle it.
        });

