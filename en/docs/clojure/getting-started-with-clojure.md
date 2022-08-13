---
title: "Getting started with clojure"
slug: "getting-started-with-clojure"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation and Setup
## Option 1: [Leiningen]

*Requires JDK 6 or newer.*

The easiest way to get started with Clojure is to download and install Leiningen, the de facto standard tool to manage Clojure projects, then run **`lein repl`** to open a [REPL].

### Linux

```sh
curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > ~/bin/lein
export PATH=$PATH:~/bin
chmod 755 ~/bin/lein
```

### OS X

Follow Linux steps above or install with macOS package managers.

### Install with [Homebrew]

```sh
brew install leiningen
```

### Install with [MacPorts]

First install Clojure
```sh
sudo port -R install clojure
```
Install `Leiningen`, a build tool for Clojure
```sh
sudo port -R install leiningen
```
```sh
lein self-install
```

### Windows

See [the official documentation](https://github.com/technomancy/leiningen#windows).


## Option 2: [Official Distribution](http://clojure.org)

*Requires JRE 6 or newer.*

Clojure releases are published as simple [JAR] files to be run on the JVM.  This is what typically happens inside the Clojure build tools.

1. Go to <http://clojure.org> and download the latest Clojure archive
2. Extract the downloaded [ZIP] file into a directory of your choice
3. Run `java -cp clojure-1.8.0.jar clojure.main` in that directory

   You may have to substitute the `clojure-1.8.0.jar` in that command for the name of the JAR file that you actually downloaded.

   For a better command-line REPL experience (e.g. cycling through your previous commands), you might want to install [`rlwrap`][rlwrap]: `rlwrap java -cp clojure-1.8.0.jar clojure.main`

## Option 3: [Boot]

*Requires JDK 7 or newer.*

Boot is a multi-purpose Clojure build tool.  Understanding it requires some knowledge of Clojure, so it may not be the best option for beginners. See [the website][boot] (click *Get Started* there) for installation instructions.

Once it's installed and in your `PATH`, you can run `boot repl` anywhere to start a Clojure REPL.

[boot]: http://boot-clj.com/
[homebrew]: https://github.com/Homebrew
[jar]: https://en.wikipedia.org/wiki/JAR_(file_format)
[leiningen]: http://leiningen.org/
[zip]: https://en.wikipedia.org/wiki/Zip_(file_format)
[rlwrap]: https://github.com/hanslub42/rlwrap
[REPL]: https://en.wikipedia.org/wiki/Read–eval–print_loop
[Macports]: https://www.macports.org/

## "Hello, world!" in the REPL
The Clojure community puts a large emphasis on interactive development, so quite a lot of interaction with Clojure happens within a [REPL (read-eval-print-loop)][repl]. When you input an expression into it, Clojure **reads** it, **evaluates** it, and **prints** the result of the evaluation, all in a **loop**.

You should be able to launch a Clojure REPL by now. If you don't know how, follow the **Installation and Setup** section in this topic. Once you've got it running, type the following into it:

    (println "Hello, world!")

Then hit <kbd>Enter</kbd>. This should print out `Hello, world!`, followed by this expression's return value, `nil`.

If you want to run some clojure instantly, try online REPL. For example  http://www.tryclj.com/. 

[repl]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

## "Hello, world!" using Boot
**Note:** you need to install Boot before trying this example out. See the **Installation and Setup** section if you haven't installed it yet.

Boot allows making executable Clojure files using **[shebang][1]** (#!) line. Place the following text into a file of your choice (this example assumes it's in the "current working directory" and is named `hello.clj`).

    #!/usr/bin/env boot
    
    (defn -main [& args]
      (println "Hello, world!"))

Then mark it as executable (if applicable, typically by running `chmod +x hello.clj`).  
...and run it (`./hello.clj`).

The program should output "Hello, world!" and finish.

  [1]: https://en.wikipedia.org/wiki/Shebang_(Unix)

## Create a new application
After following the instructions above and installing Leiningen, start a new project by running:

    lein new <project-name>

This will setup a Clojure project with the default Leiningen template within the `<project-name>` folder. There are several templates for Leiningen, which affect the project's structure. Most commonly is the template "app" used, which adds a main-function and prepares the project to be packed into a jar-file (which the main-function being the entrypoint of the application). This can be achieved with this by running:

    lein new app <project-name> 

Assuming you used the app-template to create a new application, you can test that everything was setup correctly, by entering the created directory and running the application using:

    lein run

If you see `Hello, World!` on your console, you're all set and ready to start building your application.

You can pack this simple application into two jar-files with the following command:

    lein uberjar

## Create a new application (with boot)
    boot -d seancorfield/boot-new new -t app -n <appname>

This command will tell boot to grab the task `boot-new` from https://github.com/seancorfield/boot-new and execute the task with the `app` template (see link for other templates). The task will create a new directory called `<appname>` with a typical Clojure application structure. See the generated README for more information.

To run the application: `boot run`. Other commands are specified in `build.boot` and described in the README.

