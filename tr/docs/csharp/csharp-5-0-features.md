---
title: "C# 5.0 Özellikleri"
slug: "c-50-ozellikleri"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Sözdizimi
- ***Eşzamansız ve Bekleme***

- genel **Görev** Görevim**Async**(){ doSomething(); }

bekle MyTaskAsync();
- public **Task`<string>`** MyStringTask**Async**(){ return getSomeString(); }
   
string MyString = bekle MyStringTaskAsync();

- ***Arayan Bilgi Nitelikleri***

- public void MyCallerAttributes(string MyMessage,
    
[ArayanÜyeAdı] string ÜyeAdı = "",
    
[CallerFilePath] string SourceFilePath = "",
     
[CallerLineNumber] int LineNumber = 0)
   
- Trace.WriteLine("Mesajım: " + Mesajım);
   
Trace.WriteLine("Üye: " + ÜyeAdı);

Trace.WriteLine("Kaynak Dosya Yolu: " + SourceFilePath);

Trace.WriteLine("Satır Numarası: " + LineNumber);
   

## Parametreler
| Parametreli Yöntem/Değiştirici | Ayrıntılar |
| ------ | ------ |
| `Tür<T>` | `T` dönüş türüdür |

C# 5.0, Visual Studio .NET 2012 ile birleştirilmiştir

## Zaman Uyumsuz ve Bekleme
"async" ve "await", Thread'leri serbest bırakarak ve ilerlemeden önce işlemlerin tamamlanmasını bekleyerek performansı artırmayı amaçlayan iki operatördür.

İşte uzunluğunu döndürmeden önce bir dize alma örneği:

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

İşte bir dosyayı indirmeye ve ilerleme değiştiğinde ve indirme tamamlandığında ne olacağını ele almanın başka bir örneği (bunu yapmanın iki yolu vardır):

Yöntem 1:

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
Yöntem 2:

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
    

## Arayan Bilgi Özellikleri
C.I.A.'lar, hedeflenen yöntemi çağıran şeyden öznitelikler almanın basit bir yolu olarak tasarlanmıştır. Bunları kullanmanın gerçekten sadece 1 yolu var ve sadece 3 özellik var.

Örnek:
    
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
Örnek Çıktı:
    
    //Message: Show my attributes.
    //Member: doProcess
    //Source File Path: c:\Path\To\The\File
    //Line Number: 13

