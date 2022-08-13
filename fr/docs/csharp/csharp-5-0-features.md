---
title: "Fonctionnalités C# 5.0"
slug: "fonctionnalites-c-50"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntaxe
- *** Asynchrone et en attente ***

- public **Task** MyTask**Async**(){ doSomething(); }

attendre MyTaskAsync();
- public **Task`<string>`** MyStringTask**Async**(){ return getSomeString(); }
   
chaîne MyString = attendre MyStringTaskAsync();

- ***Attributs d'informations sur l'appelant***

- public void MyCallerAttributes(string MyMessage,
    
[CallerMemberName] chaîne MemberName = "",
    
[CallerFilePath] chaîne SourceFilePath = "",
     
[CallerLineNumber] entier LineNumber = 0)
   
- Trace.WriteLine("Mon Message : " + MonMessage);
   
Trace.WriteLine("Membre : " + NomMembre);

Trace.WriteLine("Chemin du fichier source : " + SourceFilePath);

Trace.WriteLine("Numéro de ligne : " + NuméroLigne);
   

## Paramètres
| Méthode/Modificateur avec paramètre | Détails |
| ------ | ------ |
| `Type<T>` | 'T' est le type de retour |

C# 5.0 est couplé avec Visual Studio .NET 2012

## Asynchrone et en attente
`async` et `wait` sont deux opérateurs destinés à améliorer les performances en libérant des threads et en attendant que les opérations se terminent avant d'aller de l'avant.

Voici un exemple d'obtention d'une chaîne avant de renvoyer sa longueur :

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

Voici un autre exemple de téléchargement d'un fichier et de gestion de ce qui se passe lorsque sa progression a changé et lorsque le téléchargement est terminé (il y a deux façons de procéder) :

Méthode 1 :

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
Méthode 2 :

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
    

## Attributs des informations sur l'appelant
Les C.I.A.s sont conçus comme un moyen simple d'obtenir des attributs de tout ce qui appelle la méthode ciblée. Il n'y a vraiment qu'une seule façon de les utiliser et il n'y a que 3 attributs.

Exemple:
    
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
Exemple de sortie :
    
    //Message: Show my attributes.
    //Member: doProcess
    //Source File Path: c:\Path\To\The\File
    //Line Number: 13

