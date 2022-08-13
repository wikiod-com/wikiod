---
title: "teşhis"
slug: "teshis"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## TraceListeners ile günlük çıktısını yeniden yönlendirme
Debug.Listeners koleksiyonuna bir TextWriterTraceListener ekleyerek hata ayıklama çıktısını bir metin dosyasına yönlendirebilirsiniz.

    public static void Main(string[] args)
    {
        TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");

        myWriter.Flush();
    }

Bir ConsoleTraceListener kullanarak hata ayıklama çıktısını bir konsol uygulamasının çıkış akışına yönlendirebilirsiniz.

    public static void Main(string[] args)
    {
        ConsoleTraceListener myWriter = new ConsoleTraceListener();
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");
    }

## Debug.WriteLine
Uygulama hata ayıklama yapılandırmasında derlendiğinde Listeners koleksiyonundaki izleme dinleyicilerine yazar.

    public static void Main(string[] args)
    {
        Debug.WriteLine("Hello");
    }
Visual Studio veya Xamarin Studio'da bu, Uygulama Çıktısı penceresinde görünecektir. Bunun nedeni, TraceListenerCollection'da [varsayılan izleme dinleyicisinin][1] bulunmasıdır.


[1]: https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx

