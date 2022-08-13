---
title: "Async-Await"
slug: "async-await"
draft: false
images: []
weight: 9214
type: docs
toc: true
---

Em C#, um método declarado `async` não será bloqueado em um processo síncrono, caso você esteja usando operações baseadas em E/S (por exemplo, acesso à web, trabalho com arquivos, ...). O resultado de tais métodos marcados como assíncronos pode ser aguardado através do uso da palavra-chave `await`.

Um método `async` pode retornar `void`, `Task` ou `Task<T>`.

O tipo de retorno `Task` aguardará o término do método e o resultado será `void`. `Task<T>` retornará um valor do tipo `T` após a conclusão do método.

Métodos `async` devem retornar `Task` ou `Task<T>`, ao contrário de `void`, em quase todas as circunstâncias. Métodos `async void` não podem ser `aguardados`, o que leva a uma variedade de problemas. O único cenário em que um `async` deve retornar `void` é no caso de um manipulador de eventos.

`async`/`await` funciona transformando seu método `async` em uma máquina de estado. Ele faz isso criando uma estrutura nos bastidores que armazena o estado atual e qualquer contexto (como variáveis ​​locais), e expõe um método `MoveNext()` para avançar estados (e executar qualquer código associado) sempre que um awaitable for concluído.

## Operador de espera e palavra-chave assíncrona
O operador `await` e a palavra-chave `async` se juntam:

> O método assíncrono em que **await** é usado deve ser modificado por
> a palavra-chave **assíncrona**.

O oposto nem sempre é verdade: você pode marcar um método como `async` sem usar `await` em seu corpo.

O que `await` realmente faz é suspender a execução do código até que a tarefa esperada seja concluída; qualquer tarefa pode ser aguardada.

**Observação:** você não pode esperar pelo método assíncrono que não retorna nada (void).

Na verdade, a palavra 'suspender' é um pouco enganosa porque não apenas a execução é interrompida, mas a thread pode ficar livre para executar outras operações. Sob o capô, `await` é implementado por um pouco de mágica do compilador: ele divide um método em duas partes - antes e depois de `await`. A última parte é executada quando a tarefa esperada é concluída.

Se ignorarmos alguns detalhes importantes, o compilador basicamente faz isso para você:

    public async Task<TResult> DoIt()
    {
        // do something and acquire someTask of type Task<TSomeResult>  
        var awaitedResult = await someTask;
        // ... do something more and produce result of type TResult
        return result;
    }

torna-se:

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

Qualquer método usual pode ser transformado em assíncrono da seguinte maneira:

    await Task.Run(() => YourSyncMethod());

Isso pode ser vantajoso quando você precisa executar um método de longa execução no thread da interface do usuário sem congelar a interface do usuário.

Mas há uma observação muito importante aqui: **Assíncrono nem sempre significa concorrente (paralelo ou mesmo multi-threaded).** Mesmo em um único thread, `async`-`await` ainda permite código assíncrono. Por exemplo, veja este [agendador de tarefas][1] personalizado. Um agendador de tarefas tão 'louco' pode simplesmente transformar tarefas em funções que são chamadas no processamento de loop de mensagens.

Precisamos nos perguntar: Qual thread irá executar a continuação do nosso método `DoIt_Continuation`?

Por padrão, o operador `await` agenda a execução da continuação com o atual [Contexto de sincronização][2]. Isso significa que, por padrão, a continuação do WinForms e do WPF é executada no thread da interface do usuário. Se, por algum motivo, você precisar alterar esse comportamento, use [method][3] `Task.ConfigureAwait()`:

    await Task.Run(() => YourSyncMethod()).ConfigureAwait(continueOnCapturedContext: false);

[1]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.taskscheduler(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.threading.synchronizationcontext(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx

## Chamadas simultâneas
É possível aguardar várias chamadas simultaneamente invocando primeiro as tarefas a aguardar e *depois* aguardando-as.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await firstTask;
        await secondTask;
    }

Alternativamente, `Task.WhenAll` pode ser usado para agrupar várias tarefas em uma única `Task`, que é concluída quando todas as tarefas passadas são concluídas.

    public async Task RunConcurrentTasks()
    {
        var firstTask = DoSomethingAsync();
        var secondTask = DoSomethingElseAsync();

        await Task.WhenAll(firstTask, secondTask);
    }

Você também pode fazer isso dentro de um loop, por exemplo:

    List<Task> tasks = new List<Task>();
    while (something) {
        // do stuff
        Task someAsyncTask = someAsyncMethod();
        tasks.Add(someAsyncTask);
    }

    await Task.WhenAll(tasks);

Para obter resultados de uma tarefa após aguardar várias tarefas com Task.WhenAll, basta aguardar a tarefa novamente. Como a tarefa já está concluída, ela apenas retornará o resultado

    var task1 = SomeOpAsync();
    var task2 = SomeOtherOpAsync();

    await Task.WhenAll(task1, task2);

    var result = await task2;


Além disso, o `Task.WhenAny` pode ser usado para executar várias tarefas em paralelo, como o `Task.WhenAll` acima, com a diferença de que este método será concluído quando *qualquer* das tarefas fornecidas for concluída.

    public async Task RunConcurrentTasksWhenAny()
    {
        var firstTask = TaskOperation("#firstTask executed");
        var secondTask = TaskOperation("#secondTask executed");
        var thirdTask = TaskOperation("#thirdTask executed");
        await Task.WhenAny(firstTask, secondTask, thirdTask);
    }

A `Task` retornada por `RunConcurrentTasksWhenAny` será concluída quando qualquer uma das `firstTask`, `secondTask` ou `thirdTask` for concluída.



## Tentar/Pegar/Finalmente
<!-- if versão [gte 6.0] -->

A partir do C# 6.0, a palavra-chave `await` agora pode ser usada dentro de um bloco `catch` e `finally`.

    try {
       var client = new AsyncClient();
       await client.DoSomething();
    } catch (MyException ex) {
       await client.LogExceptionAsync();
       throw;
    } finally {
       await client.CloseAsync();
    }
<!-- versão final if -->

<!-- if versão [gte 5.0] [lt 6.0] -->

Antes do C# 6.0, você precisaria fazer algo como o seguinte. Observe que o 6.0 também limpou as verificações nulas com o [operador de propagação nula][1].

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
<!-- versão final if -->

Observe que se você aguardar uma tarefa não criada por `async` (por exemplo, uma tarefa criada por `Task.Run`), alguns depuradores podem quebrar em exceções lançadas pela tarefa mesmo quando aparentemente é tratada pelo try/catch ao redor. Isso acontece porque o depurador considera que não é tratado em relação ao código do usuário. No Visual Studio, existe uma opção chamada ["Just My Code"][2], que pode ser desabilitada para evitar que o depurador seja interrompido em tais situações.

[1]: https://www.wikiod.com/pt/docs/c%23/24/c-6-features/51/null-propagation#t=201511271308000980289
[2]: https://msdn.microsoft.com/en-us/library/dn457346.aspx "Documentação Just My Code no MSDN"


## Retornando uma tarefa sem esperar
Os métodos que realizam operações assíncronas não precisam usar `await` se:

* Existe apenas uma chamada assíncrona dentro do método
* A chamada assíncrona está no final do método
* A captura/tratamento de exceção que pode acontecer dentro da Tarefa não é necessária

Considere este método que retorna uma `Task`:

    public async Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return await dataStore.GetByKeyAsync(lookupKey);
    }

Se `GetByKeyAsync` tiver a mesma assinatura que `GetUserAsync` (retornando um `Task<User>`), o método pode ser simplificado:

    public Task<User> GetUserAsync(int id)
    {
        var lookupKey = "Users" + id;
    
        return dataStore.GetByKeyAsync(lookupKey);
    }

Nesse caso, o método não precisa ser marcado como `async`, mesmo que esteja realizando uma operação assíncrona. A Tarefa retornada por `GetByKeyAsync` é passada diretamente para o método chamador, onde será `aguardada`.

**Importante**: Retornar a `Task` ao invés de esperar, altera o comportamento de exceção do método, pois não lançará a exceção dentro do método que inicia a tarefa, mas sim no método que a espera.

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

Isso melhorará o desempenho, pois economizará ao compilador a geração de uma máquina de estado extra **assíncrona**.

## Configuração do Web.config para direcionar 4.5 para comportamento assíncrono correto.
O web.config system.web.httpRuntime deve ter como alvo 4.5 para garantir que o encadeamento alugue o contexto de solicitação antes de retomar seu método assíncrono.

    <httpRuntime targetFramework="4.5" />

Async e await têm comportamento indefinido no ASP.NET antes de 4.5. Async/await será retomado em um thread arbitrário que pode não ter o contexto de solicitação. Aplicativos sob carga falharão aleatoriamente com exceções de referência nula acessando o HttpContext após a espera. http://stackoverflow.com/questions/24956178/using-httpcontext-current-in-webapi-is-dangerous-because-of-async

## Async/await só melhorará o desempenho se permitir que a máquina faça trabalho adicional
Considere o seguinte código:

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

Isso não terá um desempenho melhor do que

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
O objetivo principal de async/await é permitir que a máquina faça trabalho adicional - por exemplo, permitir que o thread de chamada faça outro trabalho enquanto aguarda um resultado de alguma operação de E/S. Neste caso, o thread de chamada nunca tem permissão para fazer mais trabalho do que poderia fazer de outra forma, então não há ganho de desempenho sobre simplesmente chamar `MethodA()`, `MethodB()` ​​e `MethodC()` de forma síncrona.

## Chamadas consecutivas simples
    public async Task<JobResult> GetDataFromWebAsync()
    {
      var nextJob = await _database.GetNextJobAsync();
      var response = await _httpClient.GetAsync(nextJob.Uri);
      var pageContents = await response.Content.ReadAsStringAsync();
      return await _database.SaveJobResultAsync(pageContents);
    }

A principal coisa a notar aqui é que enquanto todo método `await`-ed é chamado de forma assíncrona - e no momento dessa chamada o controle é devolvido ao sistema - o fluxo dentro do método é linear e não requer nenhum tratamento especial por assincronia. Se algum dos métodos chamados falhar, a exceção será processada "como esperado", o que neste caso significa que a execução do método será abortada e a exceção estará subindo na pilha.



## Bloquear em código assíncrono pode causar deadlocks
É uma prática ruim bloquear chamadas assíncronas, pois pode causar deadlocks em ambientes que possuem um contexto de sincronização. A melhor prática é usar async/await "até o fim". Por exemplo, o seguinte código do Windows Forms causa um impasse:

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
Essencialmente, quando a chamada assíncrona é concluída, ela espera que o contexto de sincronização fique disponível. No entanto, o manipulador de eventos "segura" o contexto de sincronização enquanto aguarda a conclusão do método `TryThis()`, causando uma espera circular.

Para corrigir isso, o código deve ser modificado para

    private async void button1_Click(object sender, EventArgs e)
    {
      bool result = await TryThis();
      Trace.TraceInformation("Done with result");
    }

Nota: manipuladores de eventos são o único lugar onde `async void` deve ser usado (porque você não pode esperar um método `async void`).




