---
title: "The Go Command"
slug: "the-go-command"
draft: false
images: []
weight: 9940
type: docs
toc: true
---

The `go` command is a command-line program that allows for the management of Go development. It enables building, running, and testing code, as well as a variety of other Go-related tasks. 

## Go Fmt
`go fmt` will format a program's source code in a neat, idiomatic way that is easy to read and understand. It is recommended that you use `go fmt` on any source before you submit it for public viewing or committing into a version control system, to make reading it easier.

To format a file:

    go fmt main.go

Or all files in a directory:

    go fmt myProject

You can also use `gofmt -s` (**not** `go fmt`) to attempt to simplify any code that it can.

`gofmt` (**not** `go fmt`) can also be used to refactor code. It understands Go, so it is more powerful than using a simple search and replace. For example, given this program (`main.go`):

    package main
    
    type Example struct {
        Name string
    }
    
    func (e *Example) Original(name string) {
        e.Name = name
    }

    func main() {
        e := &Example{"Hello"}
        e.Original("Goodbye")
    }

You can replace the method `Original` with `Refactor` with `gofmt`:

    gofmt -r 'Original -> Refactor' -d main.go

Which will produce the diff:

    -func (e *Example) Original(name string) {
    +func (e *Example) Refactor(name string) {
         e.Name = name
     }
     
     func main() {
         e := &Example{"Hello"}
    -    e.Original("Goodbye")
    +    e.Refactor("Goodbye")
     }

## Go Run
`go run` will run a program without creating an executable file. Mostly useful for development. `run` will only execute packages whose *package name* is **main**.

To demonstrate, we will use a simple Hello World example `main.go`:

    package main
    
    import fmt
    
    func main() {
        fmt.Println("Hello, World!")
    }

Execute without compiling to a file:

    go run main.go

Output:

    Hello, World!

## Run multiple files in package

If the package is **main** and split into multiple files, one must include the other files in the `run` command:

    go run main.go assets.go


## Go Build
`go build` will compile a program into an executable file.

To demonstrate, we will use a simple Hello World example main.go:

<!-- language: go-lang -->

    package main
    
    import fmt
        
    func main() {
        fmt.Println("Hello, World!")
    }


Compile the program:

    go build main.go

`build` creates an executable program, in this case: `main` or `main.exe`. You can then run this file to see the output `Hello, World!`. You can also copy it to a similar system that doesn't have Go installed, *make it executable*, and run it there.

## Specify OS or Architecture in build:

You can specify what system or architecture to build by modifying the `env` before `build`:

    env GOOS=linux go build main.go # builds for Linux
    env GOARCH=arm go build main.go # builds for ARM architecture

## Build multiple files

If your package is split into multiple files **and** the package name is **main** (that is, *it is not an importable package*), you must specify all the files to build:

    go build main.go assets.go # outputs an executable: main

## Building a package

To build a package called `main`, you can simply use:

    go build . # outputs an executable with name as the name of enclosing folder

## Go Clean
`go clean` will clean up any temporary files created when invoking `go build` on a program. It will also clean files left over from Makefiles.

## Go Get
`go get` downloads the packages named by the import paths, along with their
dependencies. It then installs the named packages, like 'go install'. Get also accepts build flags to control the installation.

> go get github.com/maknahar/phonecountry

When checking out a new package, get creates the target directory
`$GOPATH/src/<import-path>`. If the GOPATH contains multiple entries,
get uses the first one. Similarly, it will install compiled binaries in `$GOPATH/bin`. 

When checking out or updating a package, get looks for a branch or tag
that matches the locally installed version of Go. The most important
rule is that if the local installation is running version "go1", get
searches for a branch or tag named "go1". If no such version exists it
retrieves the most recent version of the package.

When using `go get`, the `-d` flag causes it to download but not install the given package. The `-u` flag will allow it to update the package and its dependencies.

Get never checks out or updates code stored in vendor directories.

## Go env
`go env [var ...]` prints go environment information.

>By default it prints all the information.

`$go env`

```
GOARCH="amd64"
GOBIN=""
GOEXE=""
GOHOSTARCH="amd64"
GOHOSTOS="darwin"
GOOS="darwin"
GOPATH="/Users/vikashkv/work"
GORACE=""
GOROOT="/usr/local/Cellar/go/1.7.4_1/libexec"
GOTOOLDIR="/usr/local/Cellar/go/1.7.4_1/libexec/pkg/tool/darwin_amd64"
CC="clang"
GOGCCFLAGS="-fPIC -m64 -pthread -fno-caret-diagnostics -Qunused-arguments -fmessage-length=0 -fdebug-prefix-map=/var/folders/xf/t3j24fjd2b7bv8c9gdr_0mj80000gn/T/go-build785167995=/tmp/go-build -gno-record-gcc-switches -fno-common"
CXX="clang++"
CGO_ENABLED="1"
```

>If one or more variable names is given as arguments, it prints the value of each named variable on its own line.

`$go env GOOS GOPATH`
```
darwin
/Users/vikashkv/work
```



