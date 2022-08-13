---
title: "Diagnóstico"
slug: "diagnostico"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Redirecionando a saída de log com TraceListeners
Você pode redirecionar a saída de depuração para um arquivo de texto adicionando um TextWriterTraceListener à coleção Debug.Listeners.

    public static void Main(string[] args)
    {
        TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");

        myWriter.Flush();
    }

Você pode redirecionar a saída de depuração para o fluxo de saída de um aplicativo de console usando um ConsoleTraceListener.

    public static void Main(string[] args)
    {
        ConsoleTraceListener myWriter = new ConsoleTraceListener();
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");
    }

## Debug.WriteLine
Grava nos ouvintes de rastreamento na coleção Listeners quando o aplicativo é compilado na configuração de depuração.

    public static void Main(string[] args)
    {
        Debug.WriteLine("Hello");
    }
No Visual Studio ou no Xamarin Studio, isso aparecerá na janela Saída do aplicativo. Isso ocorre devido à presença do [escuta de rastreamento padrão][1] no TraceListenerCollection.


[1]: https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx

