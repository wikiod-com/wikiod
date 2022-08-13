---
title: "Cross Compilation"
slug: "cross-compilation"
draft: false
images: []
weight: 9216
type: docs
toc: true
---

The Go compiler can produce binaries for many platforms, i.e. processors and systems. Unlike with most other compilers, there is no specific requirement to cross-compiling, it is as easy to use as regular compiling.

## Syntax
 - GOOS=linux GOARCH=amd64 go build

Supported Operating System and Architecture target combinations [(source)](https://golang.org/doc/install/source#environment)

| $GOOS | $GOARCH |
| ----- | ------- |
| android | arm |
| darwin    | 386 |
| darwin    | amd64 |
| darwin    | arm |
| darwin    | arm64 |
| dragonfly    | amd64 |
| freebsd    | 386 |
| freebsd    | amd64 |
| freebsd    | arm |
| linux    | 386 |
| linux    | amd64 |
| linux    | arm |
| linux    | arm64 |
| linux    | ppc64 |
| linux    | ppc64le |
| linux    | mips64 |
| linux    | mips64le |
| netbsd    | 386 |
| netbsd    | amd64 |
| netbsd    | arm |
| openbsd    | 386 |
| openbsd    | amd64 |
| openbsd    | arm |
| plan9    | 386 |
| plan9    | amd64 |
| solaris    | amd64 |
| windows    | 386 |
| windows    | amd64 |

## Simple cross compilation with go build
From your project directory, run the `go build` command and specify the operating system and architecture target with the `GOOS` and `GOARCH` environment variables:

Compiling for Mac (64-bit):
```
GOOS=darwin GOARCH=amd64 go build
```

Compiling for Windows x86 processor:
```
GOOS=windows GOARCH=386 go build
```

You might also want to set the filename of the output executable manually to keep track of the architecture:

```
GOOS=windows GOARCH=386 go build -o appname_win_x86.exe
```

From version 1.7 and onwards you can get a list of all possible GOOS and GOARCH combinations with:

```
go tool dist list
```
(or for easier machine consumption `go tool dist list -json`)

## Compile all architectures using a Makefile
This Makefile will cross compile and zip up executables for Windows, Mac and Linux (ARM and x86).

```
# Replace demo with your desired executable name
appname := demo

sources := $(wildcard *.go)

build = GOOS=$(1) GOARCH=$(2) go build -o build/$(appname)$(3)
tar = cd build && tar -cvzf $(1)_$(2).tar.gz $(appname)$(3) && rm $(appname)$(3)
zip = cd build && zip $(1)_$(2).zip $(appname)$(3) && rm $(appname)$(3)

.PHONY: all windows darwin linux clean

all: windows darwin linux

clean:
    rm -rf build/

##### LINUX BUILDS #####
linux: build/linux_arm.tar.gz build/linux_arm64.tar.gz build/linux_386.tar.gz build/linux_amd64.tar.gz

build/linux_386.tar.gz: $(sources)
    $(call build,linux,386,)
    $(call tar,linux,386)

build/linux_amd64.tar.gz: $(sources)
    $(call build,linux,amd64,)
    $(call tar,linux,amd64)

build/linux_arm.tar.gz: $(sources)
    $(call build,linux,arm,)
    $(call tar,linux,arm)

build/linux_arm64.tar.gz: $(sources)
    $(call build,linux,arm64,)
    $(call tar,linux,arm64)

##### DARWIN (MAC) BUILDS #####
darwin: build/darwin_amd64.tar.gz

build/darwin_amd64.tar.gz: $(sources)
    $(call build,darwin,amd64,)
    $(call tar,darwin,amd64)

##### WINDOWS BUILDS #####
windows: build/windows_386.zip build/windows_amd64.zip

build/windows_386.zip: $(sources)
    $(call build,windows,386,.exe)
    $(call zip,windows,386,.exe)

build/windows_amd64.zip: $(sources)
    $(call build,windows,amd64,.exe)
    $(call zip,windows,amd64,.exe)
```
(be cautious that [Makefile's need hard tabs not spaces][1])


  [1]: http://stackoverflow.com/a/16945143/1462575

## Cross compilation by using gox
Another convenient solution for cross compilation is the usage of `gox`: https://github.com/mitchellh/gox

# Installation
The installation is done very easily by executing `go get github.com/mitchellh/gox`. The resulting executable gets placed at Go's binary directory, e.g. `/golang/bin` or `~/golang/bin`. Ensure that this folder is part of your path in order to use the `gox` command from an arbitrary location.

# Usage
From within a Go project's root folder (where you perform e.g. `go build`), execute `gox` in order to build all possible binaries for any architecture (e.g. x86, ARM) and operating system (e.g. Linux, macOS, Windows) which is available.

In order to build for a certain operating system, use e.g. `gox -os="linux"` instead. Also the architecture option could be defined: `gox -osarch="linux/amd64"`.

## Simple Example: Compile helloworld.go for arm architecture on Linux machine
**Prepare** helloworld.go (find below)

    package main
    
    import "fmt"
    
    func main(){
            fmt.Println("hello world")
    }

**Run** `GOOS=linux GOARCH=arm go build helloworld.go`

**Copy** generated `helloworld` (arm executable) file to your target machine.


 


