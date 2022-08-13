---
title: "Recursos do C# 5.0"
slug: "recursos-do-c-50"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Sintaxe
- ***Assíncrono e Aguardo***

- public **Tarefa** MinhaTarefa**Async**(){ doSomething(); }

aguardar MinhaTaskAsync();
- public **Task`<string>`** MyStringTask**Async**(){ return getSomeString(); }
   
string MinhaString = await MyStringTaskAsync();

- ***Atributos de informações do chamador***

- public void MyCallerAttributes(string MyMessage,
    
[CallerMemberName] string MemberName = "",
    
[CallerFilePath] string SourceFilePath = "",
     
[CallerLineNumber] int LineNumber = 0)
   
- Trace.WriteLine("Minha Mensagem: " + Minha Mensagem);
   
Trace.WriteLine("Membro: " + MemberName);

Trace.WriteLine("Caminho do arquivo de origem: " + SourceFilePath);

Trace.WriteLine("Número da linha: " + LineNumber);
   

## Parâmetros
| Método/Modificador com Parâmetro | Detalhes |
| ------ | ------ |
| `Tipo<T>` | `T` é o tipo de retorno |

C# 5.0 é acoplado ao Visual Studio .NET 2012

## Assinar e aguardar
`async` e `await` são dois operadores que visam melhorar o desempenho liberando Threads e aguardando a conclusão das operações antes de avançar.

Aqui está um exemplo de como obter uma string antes de retornar seu comprimento:

    //This method is async because:
    //1. It has async and Task or Task<T> as modifiers
    //2. It ends in "Async"
    async Task<int> GetStringLengthAsync(string URL){
        HttpClient client = new HttpClient();
        //Sends a GET request and returns the response body as a string
        Task<string> getString = client.GetStringAsync(URL);
        //Waits for getString to complete before returning its length
        string contents = await getString;
        return contents.Length;
    }

    private async void doProcess(){
        int length = await GetStringLengthAsync("http://example.com/");
        //Waits for all the above to finish before printing the number
        Console.WriteLine(length);
    }

Aqui está outro exemplo de baixar um arquivo e lidar com o que acontece quando seu progresso é alterado e quando o download é concluído (há duas maneiras de fazer isso):

Método 1:

    //This one using async event handlers, but not async coupled with await
    private void DownloadAndUpdateAsync(string uri, string DownloadLocation){
        WebClient web = new WebClient();
        //Assign the event handler
        web.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
        web.DownloadFileCompleted += new AsyncCompletedEventHandler(FileCompleted);
        //Download the file asynchronously
        web.DownloadFileAsync(new Uri(uri), DownloadLocation);
    }

    //event called for when download progress has changed
    private void ProgressChanged(object sender, DownloadProgressChangedEventArgs e){
        //example code
        int i = 0;
        i++;
        doSomething();
    }

    //event called for when download has finished
    private void FileCompleted(object sender, AsyncCompletedEventArgs e){
        Console.WriteLine("Completed!")
    }
Método 2:

    //however, this one does
    //Refer to first example on why this method is async
    private void DownloadAndUpdateAsync(string uri, string DownloadLocation){
        WebClient web = new WebClient();
        //Assign the event handler
        web.DownloadProgressChanged += new DownloadProgressChangedEventHandler(ProgressChanged);
        //Download the file async
        web.DownloadFileAsync(new Uri(uri), DownloadLocation);
        //Notice how there is no complete event, instead we're using techniques from the first example
    }
    private void ProgressChanged(object sender, DownloadProgressChangedEventArgs e){
        int i = 0;
        i++;
        doSomething();
    }
    private void doProcess(){
        //Wait for the download to finish
        await DownloadAndUpdateAsync(new Uri("http://example.com/file"))
        doSomething();
    }
    

## Atributos de informações do chamador
Os C.I.A.s são uma maneira simples de obter atributos de qualquer coisa que esteja chamando o método de destino. Há realmente apenas 1 maneira de usá-los e existem apenas 3 atributos.

Exemplo:
    
    //This is the "calling method": the method that is calling the target method
    public void doProcess()
    {
        GetMessageCallerAttributes("Show my attributes.");
    }
    //This is the target method
    //There are only 3 caller attributes
    public void GetMessageCallerAttributes(string message,
        //gets the name of what is calling this method
        [System.Runtime.CompilerServices.CallerMemberName] string memberName = "",
        //gets the path of the file in which the "calling method" is in
        [System.Runtime.CompilerServices.CallerFilePath] string sourceFilePath = "",
        //gets the line number of the "calling method"
        [System.Runtime.CompilerServices.CallerLineNumber] int sourceLineNumber = 0)
    {
        //Writes lines of all the attributes
        System.Diagnostics.Trace.WriteLine("Message: " + message);
        System.Diagnostics.Trace.WriteLine("Member: " + memberName);
        System.Diagnostics.Trace.WriteLine("Source File Path: " + sourceFilePath);
        System.Diagnostics.Trace.WriteLine("Line Number: " + sourceLineNumber);
    }
Saída de exemplo:
    
    //Message: Show my attributes.
    //Member: doProcess
    //Source File Path: c:\Path\To\The\File
    //Line Number: 13

