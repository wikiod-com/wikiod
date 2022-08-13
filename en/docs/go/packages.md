---
title: "Packages"
slug: "packages"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Package initalization
Package can have `init` methods which are run **only once** before main.

    package usefull

    func init() {
        // init code
    }

If you just want to run the package initialization without referencing anything from it use the following import expression.

    import _ "usefull"

## Managing package dependencies
A common way to download Go dependencies is by using the `go get <package>` command, which will save the package into the global/shared `$GOPATH/src` directory.  This means that a single version of each package will be linked into each project that includes it as a dependency. This also means that when a new developers deploys your project, they will `go get` the latest version of each dependency.

However you can keep the build environment consistent, by attaching all the dependencies of a project into the `vendor/` directory. Keeping vendored dependencies committed along with your project's repository allows you to do per-project dependency versioning, and provide a consistent environment for your build.

This is what your project's structure will look like:

    $GOPATH/src/
    ├── github.com/username/project/
    |   ├── main.go 
    |   ├── vendor/
    |   |   ├── github.com/pkg/errors
    |   |   ├── github.com/gorilla/mux

## Importing packages
You can import a single package with the statement:

    import "path/to/package"

or group multiple imports together:

    import (
        "path/to/package1"
        "path/to/package2"
    )

This will look in the corresponding `import` paths inside of the `$GOPATH` for `.go` files and lets you access exported names through `packagename.AnyExportedName`.

You can also access local packages inside of the current folder by prefacing packages with `./`. In a project with a structure like this:

    project
    ├── src
    │   ├── package1
    │   │   └── file1.go
    │   └── package2
    │       └── file2.go
    └── main.go

You could call this in `main.go` in order to import the code in `file1.go` and `file2.go`:

    import (
        "./src/package1"
        "./src/package2"
    )

Since package-names can collide in different libraries you may want to alias one package to a new name. You can do this by prefixing your import-statement with the name you want to use.

    import (
        "fmt" //fmt from the standardlibrary
        tfmt "some/thirdparty/fmt" //fmt from some other library
    )

This allows you to access the former `fmt` package using `fmt.*` and the latter `fmt` package using `tfmt.*`.

You can also import the package into the own namespace, so that you can refer to the exported names without the `package.` prefix using a single dot as alias:

    import (
        . "fmt"
    )

Above example imports `fmt` into the global namespace and lets you call, for example, `Printf` directly: [Playground](https://play.golang.org/p/CT2V79T7h3)

If you import a package but don't use any of it's exported names, the Go compiler will print an error-message. To circumvent this, you can set the alias to the underscore:

    import (
        _ "fmt"
    )

This can be useful if you don't access this package directly but need it's `init` functions to run.

Note:

As the package names are based on the folder structure, any changes in the folder names & import references (including case sensitivity) will cause a compile time error "case-insensitive import collision" in Linux & OS-X, which is difficult to trace and fix (the error message is kinda cryptic for mere mortals as it tries to convey the opposite - that, the comparison failed due to case sensitivity).

ex: "path/to/Package1" vs "path/to/package1"

Live example: 
https://github.com/akamai-open/AkamaiOPEN-edgegrid-golang/issues/2

## Using different package and folder name
It is perfectly fine to use a package name other than the folder name. If we do so, we still have to import the package based on the directory structure, but after the import we have to refer to it by the name we used in the package clause.

For example, if you have a folder `$GOPATH/src/mypck`, and in it we have a file `a.go`:

    package apple

    const Pi = 3.14

Using this package:

    package main

    import (
        "mypck"
        "fmt"
    )

    func main() {
        fmt.Println(apple.Pi)
    }

Even though this works, you should have a good reason to deviate package name from the folder name (or it may become source of misunderstanding and confusion).

### What's the use of this?

Simple. A package name is a Go [idetifier][2]:

    identifier = letter { letter | unicode_digit } .

Which allows unicode letters to be used in identifiers, e.g. `αβ` is a valid identifier in Go. Folder and file names are not handled by Go but by the Operating System, and different file systems have different restrictions. There are actually many file systems which would not allow all valid Go identifiers as folder names, so you would not be able to name your packages what otherwise the language spec would allow.

Having the option to use different package names than their containing folders, you have the option to really name your packages what the language spec allows, regardless of the underlying operating and file system.

  [1]: https://golang.org/ref/spec#Package_clause
  [2]: https://golang.org/ref/spec#Identifiers

