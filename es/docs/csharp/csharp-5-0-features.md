---
title: "Características de C# 5.0"
slug: "caracteristicas-de-c-50"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Sintaxis
- ***Asíncrono y espera***

- pública **Tarea** MiTarea**Async**(){ hacerAlgo(); }

esperar MyTaskAsync();
- public **Task`<string>`** MyStringTask**Async**(){ return getSomeString(); }
   
cadena MyString = espera MyStringTaskAsync();

- ***Atributos de información de la persona que llama***

- public void MyCallerAttributes(string MyMessage,
    
[Nombre del miembro que llama] string Nombre del miembro = "",
    
[CallerFilePath] cadena SourceFilePath = "",
     
[CallerLineNumber] int LineNumber = 0)
   
- Trace.WriteLine("Mi Mensaje: " + MiMensaje);
   
Trace.WriteLine("Miembro: " + NombreMiembro);

Trace.WriteLine("Ruta del archivo de origen: " + SourceFilePath);

Trace.WriteLine("Número de línea: " + Número de línea);
   

## Parámetros
| Método/Modificador con Parámetro | Detalles |
| ------ | ------ |
| `Tipo<T>` | `T` es el tipo de retorno |

C# 5.0 está acoplado con Visual Studio .NET 2012

## Asíncrono y espera
`async` y `await` son dos operadores que están destinados a mejorar el rendimiento al liberar subprocesos y esperar a que se completen las operaciones antes de seguir adelante.

Aquí hay un ejemplo de obtener una cadena antes de devolver su longitud:

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

Aquí hay otro ejemplo de cómo descargar un archivo y manejar lo que sucede cuando su progreso ha cambiado y cuando se completa la descarga (hay dos formas de hacerlo):

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
    

## Atributos de información de la persona que llama
Los CIA están pensados ​​como una forma simple de obtener atributos de lo que sea que esté llamando al método objetivo. Realmente solo hay 1 forma de usarlos y solo hay 3 atributos.

Ejemplo:
    
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
Salida de ejemplo:
    
    //Message: Show my attributes.
    //Member: doProcess
    //Source File Path: c:\Path\To\The\File
    //Line Number: 13

