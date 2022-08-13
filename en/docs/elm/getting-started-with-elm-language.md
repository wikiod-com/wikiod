---
title: "Getting started with Elm Language"
slug: "getting-started-with-elm-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation

To start development with Elm, you need to install a set of tools called [elm-platform][1].

It includes: [elm-make][2], [elm-reactor][3], [elm-repl][4] and [elm-package][5].

All of these tools are available through CLI, in other words you can use them from your terminal.

Pick one of the following methods to install Elm:

# Using the installer #

Download the installer from the [official website][6] and follow the installation wizard.

# Using npm #
You can use [Node Package Manager][7] to install Elm platform.

Global installation:

    $ npm install elm -g

Local installation:

    $ npm install elm

Locally installed Elm platform tools are accessible via:

    $ ./node_modules/.bin/elm-repl  # launch elm-repl from local node_modules/

# Using homebrew #

    $ brew install elm

# Switch between versions with elm-use

Install elm-use

    $ npm install -g elm-use

Switch to an older or newer elm version

    $ elm-use 0.18  // or whatever version you want to use
   
# Further reading
Learn how to [Initialize and build][8] your first project.


  [1]: https://github.com/elm-lang/elm-platform
  [2]: https://github.com/elm-lang/elm-make
  [3]: https://github.com/elm-lang/elm-reactor
  [4]: https://github.com/elm-lang/elm-repl
  [5]: https://github.com/elm-lang/elm-package
  [6]: http://elm-lang.org/install
  [7]: https://www.npmjs.com/
  [8]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build

## Initialize and build
You should have Elm platform installed on your computer, the following tutorial is written with the assumption, that you are familiar with terminal.

# Initialization #
Create a folder and navigate to it with your terminal:

    $ mkdir elm-app
    $ cd elm-app/

Initialize Elm project and install core dependencies:

    $ elm-package install -y
`elm-package.json` and `elm-stuff` folder should appear in your project.

Create the entry point for your application `Main.elm` and paste [Hello World][1] example in to it.

# Building the project #

To build your first project, run:

    $ elm-make Main.elm

This will produce `index.html` with the `Main.elm` file (and all dependencies) compiled into JavaScript and inlined into the HTML. **Try and open it in your browser!**

If this fails with the error `I cannot find module 'Html'.` it means that you are not using the latest version of Elm. You can solve the problem either by upgrading Elm and redoing the first step, or with the following command:

    $ elm-package install elm-lang/html -y

In case you have your own `index.html` file (eg. when working with ports), you can also compile your Elm files to a JavaScript file:

    $ elm-make Main.elm --output=elm.js

More info in the example [Embedding into HTML][2].

  [1]: https://www.wikiod.com/elm/getting-started-with-elm-language#Hello World
  [2]: https://www.wikiod.com/elm/getting-started-with-elm-language#Embedding into HTML

## Hello World
See how to compile this code in [Initialize and build][1]
<!-- language: lang-hs -->

    import Html
    
    main = Html.text "Hello World!"
   


  [1]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build

## Editors
# Atom #

 - https://atom.io/packages/language-elm
 - https://atom.io/packages/elmjutsu

# Light Table #

 - https://github.com/rundis/elm-light

# Sublime Text #

 - https://packagecontrol.io/packages/Elm%20Language%20Support

# Vim #

 - https://github.com/ElmCast/elm-vim

# Emacs #

 - https://github.com/jcollard/elm-mode

# IntelliJ IDEA #

 - https://plugins.jetbrains.com/plugin/8192

# Brackets #
  - https://github.com/tommot348/elm-brackets

# VS Code #
  - https://marketplace.visualstudio.com/items?itemName=sbrink.elm

## Style Guide and elm-format
The official style guide is located on [the homepage](http://elm-lang.org/docs/style-guide) and generally goes for:

- readability (instead of compactness)
- ease of modification
- clean diffs

This means that, for example, this:

<!-- language: lang-elm -->

    homeDirectory : String
    homeDirectory =
      "/root/files"
    
    
    evaluate : Boolean -> Bool
    evaluate boolean =
      case boolean of
        Literal bool ->
            bool
    
        Not b ->
            not (evaluate b)
    
        And b b' ->
            evaluate b && evaluate b'
    
        Or b b' ->
            evaluate b || evaluate b'

is considered **better** than:

<!-- language: lang-elm -->

    homeDirectory = "/root/files"
    
    eval boolean = case boolean of
        Literal bool -> bool
        Not b        -> not (eval b)
        And b b'     -> eval b && eval b'
        Or b b'      -> eval b || eval b'

<!-- if version [gte 0.16] -->

The tool [elm-format](https://github.com/avh4/elm-format) helps by formatting your source code for you **automatically** (typically on save), in a similar vein to Go language's [gofmt](https://golang.org/cmd/gofmt/). Again, the underlying value is having **one consistent style** and saving arguments and flamewars about various issues like *tabs vs. spaces* or *indentation length*.

You can install `elm-format` following the [instructions](https://github.com/avh4/elm-format#installation-) on the [Github repo](https://github.com/avh4/elm-format). Then [configure your editor](https://github.com/avh4/elm-format#editor-integration) to format the Elm files automatically or run `elm-format FILE_OR_DIR --yes` manually.

<!-- end version if -->

## Embedding into HTML
There are three possibilities to insert Elm code into a existing HTML page.

# Embed into the body tag #
Supposing you have compiled the [Hello World][1] example into `elm.js` file, you can let Elm take over the `<body>` tag like so:

<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
        <body>
            <script src="elm.js"></script>
            <script>
              Elm.Main.fullscreen()
            </script>
        </body>
    </html>
**WARNING**: Sometimes some chrome extensions mess with `<body>` which can cause your app to break in production. It's recommended to always embed in a specific div. More info [here](https://github.com/elm-lang/html/issues/44).


# Embed into a Div (or other DOM node)
Alternatively, by providing concrete HTML element, Elm code can be run in that specific page element:
<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
        <head>
            <title>Hello World</title>
        </head>
        <body>
            <div id='app'></div>
            <script src="elm.js"></script>
            <script>
                Elm.Main.embed(document.getElementById('app'))
            </script>
        </body>
    </html>


# Embed as a Web worker (no UI) #
Elm code can also be started as a worker and communicate thru [ports][2]:
<!-- language: lang-html -->

    <!DOCTYPE html>
    <html>
        <head>
            <title>Hello Worker</title>
        </head>
        <body>
            <script src="elm.js"></script>
            <script>
                var app = Elm.Main.worker();
                app.ports.fromElmToJS.subscribe(function(world) {
                    console.log(world)
                });
                app.ports.fromJSToElm.send('hello');
            </script>
        </body>
    </html>

  [1]: https://www.wikiod.com/elm/getting-started-with-elm-language#Hello World
  [2]: https://www.wikiod.com/elm/ports-js-interop

## REPL
A good way to learn about Elm is to try writing some expressions in the REPL (Read-Eval-Print Loop). Open a console in your `elm-app` folder (that you have created in the [Initialize and build][1] phase) and try the following:

    $ elm repl
    ---- elm-repl 0.17.1 -----------------------------------------------------------
     :help for help, :exit to exit, more at <https://github.com/elm-lang/elm-repl>
    --------------------------------------------------------------------------------
    > 2 + 2
    4 : number
    > \x -> x
    <function> : a -> a
    > (\x -> x + x)
    <function> : number -> number
    > (\x -> x + x) 2
    4 : number
    >

`elm-repl` is actually a pretty powerful tool.
Let's say you create a `Test.elm` file inside your `elm-app` folder with the following code:

<!-- language: lang-elm -->
    module Test exposing (..)


    a = 1


    b = "Hello"

Now, you go back to your REPL (which has stayed opened) and type:

    import Test exposing (..)
    > a
    1 : number
    > b
    "Hello" : String
    >

Even more impressive, if you add a new definition to your `Test.elm` file, such as

<!-- language: lang-elm -->
    s = """
    Hello,
    Goodbye.
    """

Save your file, go back once again to your REPL, and without importing `Test` again, the new definition is available immediately:

    > s
    "\nHello,\nGoodbye.\n" : String
    >

It's really convenient when you want to write expressions which span many lines.
It's also very useful to quickly test functions that you have just defined. Add the following to your file:

<!-- language: lang-elm -->
    f x =
      x + x * x

Save and go back to the REPL:

    > f
    <function> : number -> number
    > f 2
    6 : number
    > f 4
    20 : number
    >

Each time you modify and save a file that you have imported, and you go back to the REPL and try to do anything, the full file is recompiled. Therefore it will tell you about any error in your code. Add this:

<!-- language: lang-elm -->
    c = 2 ++ 2

Try that:

    > 0
    -- TYPE MISMATCH -------------------------------------------------- ././Test.elm

    The left argument of (++) is causing a type mismatch.

    22|     2 ++ 2
            ^
    (++) is expecting the left argument to be a:

        appendable

    But the left argument is:

        number

    Hint: Only strings, text, and lists are appendable.


    > 

To conclude this introduction to the REPL, let's add that `elm-repl` also knows about the packages that you have installed with `elm package install`. For instance:

    > import Html.App
    > Html.App.beginnerProgram
    <function>
        : { model : a, update : b -> a -> a, view : a -> Html.Html b }
          -> Platform.Program Basics.Never
    >

[1]: https://www.wikiod.com/elm/getting-started-with-elm-language#Initialize and build

## Local Build Server (Elm Reactor)
Elm Reactor is the essential tool for prototyping your application.

Please note, that you will not be able to compile `Main.elm` with Elm Reactor, if you are using [Http.App.programWithFlags][1] or [Ports][2]

Running elm-reactor in a projects directory will start a web server with a project explorer, that allows you to compile every separate component.

Any changes you make to your code are updated when you reload the page.

    $ elm-reactor                     # launch elm-reactor on localhost:8000
    $ elm-reactor -a=0.0.0.0 -p=3000  # launch elm-reactor on 0.0.0.0:3000


  [1]: https://www.wikiod.com/elm/the-elm-architecture#Program with Flags
  [2]: https://www.wikiod.com/elm/ports-js-interop

