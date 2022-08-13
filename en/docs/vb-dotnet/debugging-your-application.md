---
title: "Debugging your application"
slug: "debugging-your-application"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

Whenever you have a problem in your code, it is always a good idea to know what is going on inside. The class [System.Diagnostics.Debug](https://msdn.microsoft.com/en-us/library/system.diagnostics.debug(v=vs.110).aspx) in .Net Framework will help you a lot in this task.

The first advantage of the Debug class is that it produces code only if you build your application in Debug mode. When you build your application in Release mode, no code will be generated from the Debug calls.

## Debug in the console
    Module Module1
        Sub Main()
            Debug.WriteLine("This line will be shown in the Visual Studio output console")

            Console.WriteLine("Press a key to exit")
            Console.ReadKey()

            Debug.WriteLine("End of application")
        End Sub
    End Module


will produce:

[![Debug output window in Visual Studio][1]][1]


  [1]: https://i.stack.imgur.com/CSZAI.png

## Indenting your debug output
    Module Module1

        Sub Main()
            Debug.WriteLine("Starting aplication")

            Debug.Indent()
            LoopAndDoStuff(5)
            Debug.Unindent()

            Console.WriteLine("Press a key to exit")
            Console.ReadKey()

            Debug.WriteLine("End of application")
        End Sub

        Sub LoopAndDoStuff(Iterations As Integer)
            Dim x As Integer = 0
            Debug.WriteLine("Starting loop")
            Debug.Indent()
            For i As Integer = 0 To Iterations - 1
                Debug.Write("Iteration " & (i + 1).ToString() & " of " & Iterations.ToString() & ": Value of X: ")
                x += (x + 1)
                Debug.WriteLine(x.ToString())
            Next
            Debug.Unindent()
            Debug.WriteLine("Loop is over")
        End Sub
    End Module

will produce:
[![Output when indenting][1]][1]


  [1]: https://i.stack.imgur.com/wHFLT.png

## Debug in a text file
At the beginning of your application, your must add a [TextWriterTraceListener][1] to the Listeners list of the Debug class.

    Module Module1

        Sub Main()
            Debug.Listeners.Add(New TextWriterTraceListener("Debug of " & DateTime.Now.ToString() & ".txt"))

            Debug.WriteLine("Starting aplication")

            Console.WriteLine("Press a key to exit")
            Console.ReadKey()

            Debug.WriteLine("End of application")
        End Sub
    End Module

All the Debug code produced will be outputed in the Visual Studio console AND in the text file you chose.

If the file is always the same:

    Debug.Listeners.Add(New TextWriterTraceListener("Debug.txt"))

The output will be appended to the file every time AND a new file starting with a GUID then your filename will be generated.


  [1]: https://msdn.microsoft.com/en-us/library/system.diagnostics.textwritertracelistener(v=vs.110).aspx

