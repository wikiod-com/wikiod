---
title: "Diagnostique"
slug: "diagnostique"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Redirection de la sortie du journal avec TraceListeners
Vous pouvez rediriger la sortie de débogage vers un fichier texte en ajoutant un TextWriterTraceListener à la collection Debug.Listeners.

    public static void Main(string[] args)
    {
        TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");

        myWriter.Flush();
    }

Vous pouvez rediriger la sortie de débogage vers le flux de sortie d'une application console à l'aide d'un ConsoleTraceListener.

    public static void Main(string[] args)
    {
        ConsoleTraceListener myWriter = new ConsoleTraceListener();
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");
    }

## Debug.WriteLine
Écrit dans les écouteurs de trace de la collection Listeners lorsque l'application est compilée en configuration de débogage.

    public static void Main(string[] args)
    {
        Debug.WriteLine("Hello");
    }
Dans Visual Studio ou Xamarin Studio, cela apparaîtra dans la fenêtre de sortie de l'application. Cela est dû à la présence du [écouteur de trace par défaut][1] dans TraceListenerCollection.


[1] : https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx

