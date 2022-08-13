---
title: "Getting started with C# Language"
slug: "getting-started-with-c-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating a new console application (Visual Studio)
1. Open Visual Studio
2. In the toolbar, go to **File** → **New Project**
3. Select the **Console Application** project type
4. Open the file `Program.cs` in the Solution Explorer
5. Add the following code to `Main()`:


    public class Program
    {
        public static void Main()
        {
            // Prints a message to the console.
            System.Console.WriteLine("Hello, World!");

            /* Wait for the user to press a key. This is a common
               way to prevent the console window from terminating
               and disappearing before the programmer can see the contents
               of the window, when the application is run via Start from within VS. */
            System.Console.ReadKey();
        }
    }

6. In the toolbar, click **Debug** -> **Start Debugging** or hit **F5** or **ctrl + F5** (running without debugger) to run the program.


[Live Demo on ideone][1]

-----------------------------------------------------------------------------------

# Explanation

 - `class Program` is a class declaration. The class `Program` contains the data and method definitions that your program uses. Classes generally contain multiple methods. Methods define the behavior of the class. However, the `Program` class has only one method: `Main`.

- `static void Main()` defines the `Main` method, which is the entry point for all C# programs. The `Main` method states what the class does when executed. Only one `Main` method is allowed per class.

- `System.Console.WriteLine("Hello, world!");` method prints a given data (in this example, `Hello, world!`) as an output in the console window.

-  `System.Console.ReadKey()`, ensures that the program won't close immediately after displaying the message. It does this by waiting for the user to press a key on the keyboard. Any key press from the user will terminate the program. The program terminates when it has finished the last line of code in the `main()` method.

-----------------------------------------------------------------------------------

# Using the command line

To compile via command line use either `MSBuild` or `csc.exe` _(the C# compiler)_, both part of the [Microsoft Build Tools](https://www.visualstudio.com/downloads/download-visual-studio-vs#d-build-tools) package.

To compile this example, run the following command in the same directory where `HelloWorld.cs` is located:

<!-- language: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

It can also be possible that you have two main methods inside one application. In this case, you have to tell the compiler which main method to execute by typing the following command in the **console**.(suppose Class `ClassA` also has a main method in the same `HelloWorld.cs` file in HelloWorld namespace)

<!-- language: lang-none -->
    %WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

where HelloWorld is namespace


***Note**: This is the path where **.NET framework v4.0** is located in general. Change the path according to your .NET version. In addition, the directory might be **framework** instead of **framework64** if you're using the 32-bit .NET Framework.  From the Windows Command Prompt, you can list all the csc.exe Framework paths by running the following commands (the first for 32-bit Frameworks):*

    dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
    dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

![Compiling the .cs file][2]

There should now be an executable file named `HelloWorld.exe` in the same directory. To execute the program from the command prompt, simply type the executable's name and hit <kbd>Enter</kbd> as follows:

<!-- language: lang-none -->
    HelloWorld.exe

This will produce:

>Hello, world!

![Executing the exe file in the console][3]

You may also double click the executable and launch a new console window with the message "**Hello, world!**"

![Running the executable and using double click][4]

  [1]: https://ideone.com/3OhmnG
  [2]: http://i.stack.imgur.com/xT8kk.png
  [3]: http://i.stack.imgur.com/x0Fek.png
  [4]: http://i.stack.imgur.com/qstu1.png

## Creating a new project in Visual Studio (console application) and Running it in Debug mode
 1. **Download and install [Visual Studio][1]**. Visual Studio can be downloaded from [VisualStudio.com][2]. The Community edition is suggested, first because it is free, and second because it involves all the general features and can be extended further.

 2. **Open Visual Studio.**
 3. **Welcome.** Go to **File  → **New**  → Project**.
    [![Microsoft Visual Studio - File Menu][3]][3]

 4. Click **Templates** → **Visual C#** → **Console Application**

    [![Microsoft Visual Studio - New Project window][4]][4]

 5. **After selecting Console Application,** Enter a name for your project, and a location to save and press <kbd>OK</kbd>. Don't worry about the Solution name.

 6. **Project created**. The newly created project will look similar to:

    [![Microsoft Visual Studio - c# Default Project][5]][5]

    _(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)_

 7. **Write code.** You can now update your `Program.cs` to present "Hello world!" to the user.

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                }
            }
        }

    Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

        Console.WriteLine("Hello world!");
        Console.Read();

    **Why** `Console.Read()`__?__ The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

        using System;
        
        namespace ConsoleApplication1
        {
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello world!");
                    Console.Read();
                }
            }
        }

 8.  **Debug your program.** Press the Start Button on the toolbar near the top of the window [![Start Debugging Button][6]][6] or press <kbd>F5</kbd> on your keyboard to run your application. If the button is not present, you can run the program from the top menu: **Debug → Start Debugging**. The program will compile and then open a console window. It should look similar to the following screenshot:

   [![Console running the Hello World application][7]][7]

 9.  **Stop the program.** To close the program, just press any key on your keyboard. The `Console.Read()` we added was for this same purpose. Another way to close the program is by going to the menu where the <kbd>Start</kbd> button was, and clicking on the <kbd>Stop</kbd> button.

     


  [1]: https://www.visualstudio.com/products/vs-2015-product-editions
  [2]: http://www.visualstudio.com
  [3]: http://i.stack.imgur.com/fpvTX.png
  [4]: http://i.stack.imgur.com/kKGls.png
  [5]: http://i.stack.imgur.com/WVkeF.png
  [6]: https://i.stack.imgur.com/odDu6.png
  [7]: http://i.stack.imgur.com/ZD5MF.png

## Creating a new program using Mono
First install [Mono][1] by going through the install instructions for the platform of your choice as described in their [installation section][2].

Mono is available for Mac OS X, Windows and Linux.

After installation is done, create a text file, name it `HelloWorld.cs` and copy the following content into it:

    public class Program
    {
        public static void Main()
        {
            System.Console.WriteLine("Hello, world!");
            System.Console.WriteLine("Press any key to exit..");
            System.Console.Read();
        }
    }


If you are using Windows, run the Mono Command Prompt which is included in the Mono installation and ensures that the necessary environment variables are set. If on Mac or Linux, open a new terminal.

To compile the newly created file, run the following command in the directory containing `HelloWorld.cs`:

 <!-- language: lang-none -->
    mcs -out:HelloWorld.exe HelloWorld.cs
 

 The resulting `HelloWorld.exe` can then be executed with:
 
 <!-- language: lang-none -->
    mono HelloWorld.exe
 
 which will produce the output:
 
 
<!-- language: lang-none -->    
    Hello, world!   
    Press any key to exit..

 
 [1]: http://www.mono-project.com/
 [2]: http://www.mono-project.com/docs/getting-started/install/

## Creating a new program using .NET Core
First install the [**.NET Core SDK**][1] by going through the installation instructions for the platform of your choice: 

 - [Windows][2]
 - [OSX][3]
 - [Linux][4]
 - [Docker][5]

After the installation has completed, open a command prompt, or terminal window.

 1. Create a new directory with `mkdir hello_world` and change into the newly created directory with `cd hello_world`.

 2. Create a new console application with `dotnet new console`.  
This will produce two files:

    - **hello_world.csproj**

          <Project Sdk="Microsoft.NET.Sdk">

            <PropertyGroup>
              <OutputType>Exe</OutputType>
              <TargetFramework>netcoreapp1.1</TargetFramework>
            </PropertyGroup>

          </Project>
          
    - **Program.cs**

          using System;
        
          namespace hello_world
          {
              class Program
              {
                  static void Main(string[] args)
                  {
                      Console.WriteLine("Hello World!");
                  }
              }
          }

 3. Restore the needed packages with `dotnet restore`.

 4. *Optional* Build the application with `dotnet build` for Debug or `dotnet build -c Release` for Release. `dotnet run` will also run the compiler and throw build errors, if any are found.

 5. Run the application with `dotnet run` for Debug or `dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll` for Release.

-----------------------------------------------------------------------------------

Command Prompt output
---------------------
[![enter image description here][6]][6]


  [1]: https://docs.microsoft.com/en-us/dotnet/articles/core/
  [2]: https://www.microsoft.com/net/core#windows
  [3]: https://www.microsoft.com/net/core#macos
  [4]: https://www.microsoft.com/net/core#linuxubuntu
  [5]: https://www.microsoft.com/net/core#dockercmd
  [6]: https://i.stack.imgur.com/arqCl.png


## Creating a new query using LinqPad
LinqPad is a great tool that allows you to learn and test features of .Net languages (C#, F# and VB.Net.)

 1. Install [LinqPad][1]
 2. Create a new Query (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[![enter image description here][2]][2]
 3. Under language, select "C# statements"
[![enter image description here][3]][3]
 4. Type the following code and hit run (<kbd>F5</kbd>)

        string hw = "Hello World";

        hw.Dump(); //or Console.WriteLine(hw);
[![enter image description here][4]][4]

 5. You should see "Hello World" printed out in the results screen.
[![enter image description here][5]][5]
 6. Now that you have created your first .Net program, go and check out the samples included in LinqPad via the "Samples" browser. There are many great examples that will show you many different features of the .Net languages.
[![enter image description here][6]][6]

**Notes:**
1. If you click on "IL", you can inspect the IL code that your .net code generates. This is a great learning tool.
[![enter image description here][7]][7]
2. When using `LINQ to SQL` or `Linq to Entities` you can inspect the SQL that's being generated which is another great way to learn about LINQ.


  [1]: http://www.linqpad.net/
  [2]: http://i.stack.imgur.com/D0tSi.png
  [3]: http://i.stack.imgur.com/kC5Ur.jpg
  [4]: http://i.stack.imgur.com/LO4kD.jpg
  [5]: http://i.stack.imgur.com/GzsrS.jpg
  [6]: http://i.stack.imgur.com/yucuf.jpg
  [7]: http://i.stack.imgur.com/XPumO.jpg

## Creating a new project using Xamarin Studio
 1. Download and install [Xamarin Studio Community][1].
 2. Open Xamarin Studio.
 3. Click **File** → **New** → **Solution**.

[![Creating New Project in Xamarin Studio][2]][2]

 4. Click **.NET** → **Console Project** and choose **C#**.
 5. Click <kbd>Next</kbd> to proceed.

[![Choosing Template for new project][3]][3]
 
 6. Enter the **Project Name** and <kbd>Browse...</kbd> for a **Location** to Save and then click <kbd>Create</kbd>.

[![Project name and location][4]][4]

 7. The newly created project will look similar to:

[![enter image description here][5]][5]

 8. This is the code in the Text Editor:


    using System;
    
    namespace FirstCsharp
    {
        public class MainClass
        {
            public static void Main(string[] args)
            {
                Console.WriteLine("Hello World!");
                Console.ReadLine();
            }
        }
    }

 9. To run the code, press <kbd>F5</kbd> or click the **Play Button** as shown below:

[![Run the code][6]][6]

 10. Following is the Output:

[![output][7]][7]


  [1]: https://store.xamarin.com/
  [2]: http://i.stack.imgur.com/hHjMM.png
  [3]: http://i.stack.imgur.com/s58Ju.png
  [4]: http://i.stack.imgur.com/lrK8L.png
  [5]: http://i.stack.imgur.com/vva82.png
  [6]: http://i.stack.imgur.com/6q4ZN.png
  [7]: http://i.stack.imgur.com/cqBsK.png

