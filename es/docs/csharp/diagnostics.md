---
title: "Diagnóstico"
slug: "diagnostico"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Redirigir la salida del registro con TraceListeners
Puede redirigir la salida de depuración a un archivo de texto agregando un TextWriterTraceListener a la colección Debug.Listeners.

    public static void Main(string[] args)
    {
        TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");

        myWriter.Flush();
    }

Puede redirigir la salida de depuración a la transmisión de salida de una aplicación de consola mediante ConsoleTraceListener.

    public static void Main(string[] args)
    {
        ConsoleTraceListener myWriter = new ConsoleTraceListener();
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");
    }

## Depuración.WriteLine
Escribe en los agentes de escucha de seguimiento en la colección de agentes de escucha cuando la aplicación se compila en la configuración de depuración.

    public static void Main(string[] args)
    {
        Debug.WriteLine("Hello");
    }
En Visual Studio o Xamarin Studio, esto aparecerá en la ventana Resultados de la aplicación. Esto se debe a la presencia del [detector de seguimiento predeterminado][1] en TraceListenerCollection.


[1]: https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx

