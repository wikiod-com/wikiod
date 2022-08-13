---
title: "Getting started with F#"
slug: "getting-started-with-f"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World!
This is the code for a simple console project, that prints "Hello, World!" to STDOUT, and exits with an exit code of `0`

    [<EntryPoint>]
    let main argv = 
        printfn "Hello, World!"
        0 

Example breakdown Line-by-line:
- `[<EntryPoint>]` - A [.net Attribute][1] that marks "the method that you use to set the entry point" of your program ([source][2]).
- `let main argv` - this defines a function called `main` with a single parameter `argv`. Because this is the program entry point, `argv` will be an array of strings. The contents of the array are the arguments that were passed to the program when it was executed.
- `printfn "Hello, World!"` - the [`printfn`][3] function outputs the string** passed as its first argument, also appending a newline.
- `0` - F# functions always return a value, and the value returned is the result of the last expression in the function. Putting `0` as the last line means that the function will always return zero (an integer).


** This is actually _not_ a string even though it looks like one. It's actually a [TextWriterFormat][4], which optionally allows the usage of statically type checked arguments. But for the purpose of a "hello world" example it can be thought of as being a string.


  [1]: http://stackoverflow.com/q/20346/4289902
  [2]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/entry-point-%5Bfsharp%5D
  [3]: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/printf.printfn%5B't%5D-function-%5Bfsharp%5D
  [4]: https://msdn.microsoft.com/visualfsharpdocs/conceptual/printf.textwriterformat%5B%27t%5D-type-abbreviation-%5Bfsharp%5D

## Installation or Setup
# Windows

If you have Visual Studio (any version including express and community) installed, F# should already be included. Just choose F# as the language when you create a new project. Or see http://fsharp.org/use/windows/ for more options.

# OS X

[Xamarin Studio](https://developer.xamarin.com/guides/cross-platform/fsharp/fsharp_support_overview/) supports F#. Alternately, you could use [VS Code for OS X][1], which is a cross-platform editor by Microsoft.  
Once done with installing VS Code, launch `VS Code Quick Open` (<kbd>Ctrl</kbd>+<kbd>P</kbd>) then run `ext install Ionide-fsharp`

You may also consider [Visual Studio for Mac](https://www.visualstudio.com/vs/visual-studio-mac/).

There are other alternatives [described here][2].

# Linux

Install the `mono-complete` and `fsharp` packages via your distribution's package manager (Apt, Yum, etc.). For a good editing experience, use either [Visual Studio Code](https://code.visualstudio.com/) and install the `ionide-fsharp` plugin, or use [Atom](https://atom.io/) and install the `ionide-installer` plugin. See http://fsharp.org/use/linux/ for more options.


  [1]: https://code.visualstudio.com/Docs/?dv=osx
  [2]: http://fsharp.org/use/mac/

## F# Interactive
F# Interactive, is a REPL environment that lets you execute F# code, one line at a time.

If you have installed Visual Studio with F#, you can run F# Interactive in console by typing `"C:\Program Files (x86)\Microsoft SDKs\F#\4.0\Framework\v4.0\Fsi.exe"`. On Linux or OS X, the command is `fsharpi` instead, which should be either in `/usr/bin` or in `/usr/local/bin` depending on how you installed F# -- either way, the command should be on your `PATH` so you can just type `fsharpi`.

Example of F# interactive usage:

    > let i = 1 // fsi prompt, declare i
    - let j = 2 // declare j
    - i+j // compose expression
    - ;; // execute commands
    
    val i : int = 1 // fsi output started, this gives the value of i
    val j : int = 2 // the value of j
    val it : int = 3 // computed expression
    
    > #quit;; //quit fsi

Use `#help;;` for help

Please note the use of `;;` to tell the REPL to execute any previously-typed commands.

