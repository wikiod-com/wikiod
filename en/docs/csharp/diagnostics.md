---
title: "Diagnostics"
slug: "diagnostics"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Redirecting log output with TraceListeners
You can redirect the debug output to a text file by adding a TextWriterTraceListener to the Debug.Listeners collection.

    public static void Main(string[] args)
    {
        TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");

        myWriter.Flush();
    }

You can redirect the debug output to a console application's out stream using a ConsoleTraceListener.

    public static void Main(string[] args)
    {
        ConsoleTraceListener myWriter = new ConsoleTraceListener();
        Debug.Listeners.Add(myWriter);
        Debug.WriteLine("Hello");
    }

## Debug.WriteLine
Writes to the trace listeners in the Listeners collection when the application is compiled in debug configuration.

    public static void Main(string[] args)
    {
        Debug.WriteLine("Hello");
    }
In Visual Studio or Xamarin Studio this will appear in the Application Output window. This is due to the presence of the [default trace listener][1] in the TraceListenerCollection.


  [1]: https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx

