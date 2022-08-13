---
title: "Getting started with Go"
slug: "getting-started-with-go"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World!
Place the following code into a file name `hello.go`:

    package main
    
    import "fmt"
    
    func main() {
        fmt.Println("Hello, 世界")
    }

[Playground](https://play.golang.org/p/I3l_5RKJts)

When `Go` is [installed correctly][1] this program can be compiled and run like this:

    go run hello.go

# Output:

```
Hello, 世界
```

Once you are happy with the code it can be compiled to an executable by running:

    go build hello.go

This will create  an executable file appropriate for your operating system in the current directory, which you can then run with the following command:

**Linux, OSX, and other Unix-like systems**

    ./hello

**Windows**

    hello.exe


_**Note**: The Chinese characters are important because they demonstrate that Go strings are stored as read-only slices of bytes._


  [1]: https://golang.org/dl/

## Setting up the environment
If Go is not pre-installed in your system you can go to https://golang.org/dl/ and choose your platform to download and install Go.

To set up a basic Go development environment, only a few of the many environment variables that affect the behavior of the `go` tool (See: [Listing Go Environment Variables][1] for a full list) need to be set (generally in your shell's `~/.profile` file, or equivalent on Unix-like OSs).

## `GOPATH`

Like the system `PATH` environment variable, Go path is a `:`(`;` on Windows) delimited list of directories where Go will look for packages. The `go get` tool will also download packages to the first directory in this list.

The `GOPATH` is where Go will setup associated `bin`, `pkg`, and `src` folders needed for the workspace:

 - `src` — location of source files: `.go`, `.c`, `.g`, `.s`
 - `pkg` — has compiled `.a` files
 - `bin` — contains executable files built by Go

From Go 1.8 onwards, the `GOPATH` environment variable will have a [default value][2] if it is unset. It defaults to $HOME/go on Unix/Linux and %USERPROFILE%/go on Windows.

Some tools assume that `GOPATH` will contain a single directory.

## `GOBIN`

The bin directory where `go install` and `go get` will place binaries after building `main` packages. Generally this is set to somewhere on the system `PATH` so that installed binaries can be run and discovered easily.

## `GOROOT`

This is the location of your Go installation. It is used to find the standard libraries. It is very rare to have to set this variable as Go embeds the build path into the toolchain. Setting `GOROOT` is needed if the installation directory differs from the build directory (or the value set when building).


  [1]: https://www.wikiod.com/go/getting-started-with-go#Listing Go Environment Variables
  [2]: https://golang.org/doc/go1.8#gopath

## FizzBuzz
Another example of "Hello World" style programs is [FizzBuzz][1]. This is one example of a FizzBuzz implementation. Very idiomatic Go in play here.

    package main

    // Simple fizzbuzz implementation

    import "fmt"
    
    func main() {
        for i := 1; i <= 100; i++ {
            s := ""       
            if i % 3 == 0 {
                s += "Fizz"
            }
            if i % 5 == 0 {
                s += "Buzz"
            }
            if s != "" {
                fmt.Println(s) 
            } else {
                fmt.Println(i) 
            }
        }
    }

[Playground](https://play.golang.org/p/ckp5s9Fepm)

  [1]: https://blog.codinghorror.com/why-cant-programmers-program/

## Listing Go Environment Variables
Environment variables that affect the `go` tool can be viewed via the `go env [var ...]` command:

    $ go env
    GOARCH="amd64"
    GOBIN="/home/yourname/bin"
    GOEXE=""
    GOHOSTARCH="amd64"
    GOHOSTOS="linux"
    GOOS="linux"
    GOPATH="/home/yourname"
    GORACE=""
    GOROOT="/usr/lib/go"
    GOTOOLDIR="/usr/lib/go/pkg/tool/linux_amd64"
    CC="gcc"
    GOGCCFLAGS="-fPIC -m64 -pthread -fmessage-length=0 -fdebug-prefix-map=/tmp/go-build059426571=/tmp/go-build -gno-record-gcc-switches"
    CXX="g++"
    CGO_ENABLED="1"

By default it prints the list as a shell script; however, if one or more variable names are given as arguments, it prints the value of each named variable.

    $go env GOOS GOPATH
    linux
    /home/yourname

## Accessing Documentation Offline
For full documentation, run the command:
```
godoc -http=:<port-number>
```

For a tour of Go (highly recommended for beginners in the language):
```
go tool tour
```
The two commands above will start web-servers with documentation similar to what is found online [here](https://golang.org/doc/) and [here](https://tour.golang.org/) respectively.

For quick reference check from command-line, eg for fmt.Print:
```
godoc cmd/fmt Print
# or
go doc fmt Print
```
General help is also available from command-line:
```
go help [command]
```

## Running Go online
## The Go Playground ##

One little known Go tool is **[The Go Playground](https://play.golang.org)**. If one wants to experiment with Go without downloading it, they can easily do so simply by . . .

1. Visiting the [Playground](https://play.golang.org) in their web browser
2. Entering their code
3. Clicking “Run”

## Sharing your code ##

The Go Playground also has tools for sharing; if a user presses the “Share” button, a link (like [this one](https://play.golang.org/p/v3rrZLwEUC)) will be generated that can be sent to other people to test and edit.

## In action ##

[![The Go Playground in action](https://i.stack.imgur.com/1v2fz.png)](https://i.stack.imgur.com/1v2fz.png)

