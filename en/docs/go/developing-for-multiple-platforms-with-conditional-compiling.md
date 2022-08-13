---
title: "Developing for Multiple Platforms with Conditional Compiling"
slug: "developing-for-multiple-platforms-with-conditional-compiling"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Platform based conditional compiling comes in two forms in Go, one is with file suffixes and the other is with build tags.

## Syntax
* After "`// +build`", a single platform or a list can follow
* Platform can be reverted by preceding it by `!` sign
* List of space separated platforms are ORed together


**Caveats for build tags:**
* The `// +build` constraint must be placed at the top of the file, even before package clause.
* It must be followed by one blank line to separate from package comments.

| List of valid platforms for both build tags and file suffixes |
| -------- |
| android  |
| darwin   |
| dragonfly|
| freebsd  |
| linux    |
| netbsd   |
| openbsd  |
| plan9    | 
| solaris  | 
| windows  |

Refer to `$GOOS` list in https://golang.org/doc/install/source#environment for the most up-to-date platform list.

## Build tags
<!-- language: lang -->
```
// +build linux

package lib

var OnlyAccessibleInLinux int // Will only be compiled in Linux
```

Negate a platform by placing `!` before it:
<!-- language: lang -->
```
// +build !windows

package lib

var NotWindows int // Will be compiled in all platforms but not Windows
```

List of platforms can be specified by separating them with spaces
<!-- language: lang -->
```
// +build linux darwin plan9

package lib

var SomeUnix int // Will be compiled in linux, darwin and plan9 but not on others
```

## File suffix
If you name your file `lib_linux.go`, all the content in that file will only be compiled in linux environments:

<!-- language: lang -->
```
package lib

var OnlyCompiledInLinux string
```

## Defining separate behaviours in different platforms
Different platforms can have separate implementations of the same method. This example also illustrates how build tags and file suffixes can be used together.

File `main.go`:
<!-- language: lang -->
```
package main

import "fmt"

func main() {
    fmt.Println("Hello World from Conditional Compilation Doc!")
    printDetails()
}
```

`details.go`:
<!-- language: lang -->
```
// +build !windows

package main

import "fmt"

func printDetails() {
    fmt.Println("Some specific details that cannot be found on Windows")
}
```

`details_windows.go`:
<!-- language: lang -->
```
package main

import "fmt"

func printDetails() {
    fmt.Println("Windows specific details")
}
```


