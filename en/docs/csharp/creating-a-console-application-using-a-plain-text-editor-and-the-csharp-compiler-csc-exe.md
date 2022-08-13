---
title: "Creating a Console Application using a Plain-Text Editor and the C# Compiler (csc.exe)"
slug: "creating-a-console-application-using-a-plain-text-editor-and-the-c-compiler-cscexe"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Creating a Console application using a Plain-Text Editor and the C# Compiler
In order to use a plain-text editor to create a Console application that is written in C#, you'll need the C# Compiler. The C# Compiler (csc.exe), can be found at the following location:
`%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe`

**N.B.** Depending upon which version of the .NET Framework that is installed on your system, you may need to change the path above, accordingly.


----------

<h1>Saving the Code</h1>
The purpose of this topic is not to teach you <i>how</i> to write a Console application, but to teach you how to <i>compile</i> one [to produce a single executable file], with nothing other than the C# Compiler and any Plain-Text Editor (such as Notepad).
<br/><br/>

1. Open the Run dialog, by using the keyboard shortcut <kbd>Windows Key</kbd> + <kbd>R</kbd>
2. Type `notepad`, then hit <kbd>Enter</kbd>
3. Paste the example code below, into Notepad
4. Save the file as `ConsoleApp.cs`, by going to **File** → **Save As...**, then entering `ConsoleApp.cs` in the 'File Name' text field, then selecting `All Files` as the file-type.
5. Click `Save`

<h1>Compiling the Source Code</h1>
1. Open the Run dialog, using <kbd>Windows Key</kbd> + <kbd>R</kbd><br/>
2. Enter:

    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe /t:exe /out:"C:\Users\yourUserName\Documents\ConsoleApp.exe" "C:\Users\yourUserName\Documents\ConsoleApp.cs"

Now, go back to where you originally saved your `ConsoleApp.cs` file. You should now see an executable file (`ConsoleApp.exe`). Double-click `ConsoleApp.exe` to open it.

That's it! Your console application has been compiled. An executable file has been created and you now have a working Console app.


    using System;
    
    namespace ConsoleApp
    {
        class Program
        {
            private static string input = String.Empty;
    
            static void Main(string[] args)
            {
                goto DisplayGreeting;
    
                DisplayGreeting:
                {
                    Console.WriteLine("Hello! What is your name?");
    
                    input = Console.ReadLine();
    
                    if (input.Length >= 1)
                    {
                        Console.WriteLine(
                            "Hello, " + 
                            input + 
                            ", enter 'Exit' at any time to exit this app.");
    
                        goto AwaitFurtherInstruction;
                    }
                    else
                    {
                        goto DisplayGreeting;
                    }
                }
    
                AwaitFurtherInstruction:
                {
                    input = Console.ReadLine();
    
                    if(input.ToLower() == "exit")
                    {
                        input = String.Empty;
    
                        Environment.Exit(0);
                    }
                    else
                    {
                        goto AwaitFurtherInstruction;
                    }
                }
            }
        }
    }

