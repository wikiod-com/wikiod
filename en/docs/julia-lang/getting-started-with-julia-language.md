---
title: "Getting started with Julia Language"
slug: "getting-started-with-julia-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, World!
    println("Hello, World!")

To run Julia, first get the interpreter from the website’s [download page](http://julialang.org/downloads/). The current stable release is v0.5.0, and this version is recommended for most users. Certain package developers or power users may choose to use the nightly build, which is far less stable.

When you have the interpreter, write your program in a file named `hello.jl`. It can then be run from a system terminal as:

    $ julia hello.jl
    Hello, World!

Julia can also be run interactively, by running the `julia` program. You should see a header and prompt, as follows:

                   _
       _       _ _(_)_     |  A fresh approach to technical computing
      (_)     | (_) (_)    |  Documentation: http://docs.julialang.org
       _ _   _| |_  __ _   |  Type "?help" for help.
      | | | | | | |/ _` |  |
      | | |_| | | | (_| |  |  Version 0.4.2 (2015-12-06 21:47 UTC)
     _/ |\__'_|_|_|\__'_|  |  Official http://julialang.org/ release
    |__/                   |  x86_64-w64-mingw32
    
    julia> 

You can run any Julia code in this [REPL][2], so try:

    julia> println("Hello, World!")
    Hello, World!

This example makes use of a [string][3], `"Hello, World!"`, and of the `println` [function][4]—one of many in the standard library. For more information or help, try the following sources:

- The REPL has an integrated [help mode][5] to access documentation.
- The official [documentation](http://docs.julialang.org/en/stable/) is quite comprehensive.
- Stack Overflow has a small but growing collection of examples.
- Users on [Gitter](https://gitter.im/JuliaLang/julia) are happy to help with small questions.
- The primary online discussion venue for Julia is the Discourse forum at [discourse.julialang.org](https://discourse.julialang.org/). More involved questions should be posted here.
- A collection of tutorials and books can be found [here](http://julialang.org/learning/).



  [1]: https://www.wikiod.com/shell/getting-started-with-shell
  [2]: https://www.wikiod.com/julia-lang/repl
  [3]: https://www.wikiod.com/julia-lang/strings
  [4]: https://www.wikiod.com/julia-lang/functions
  [5]: https://www.wikiod.com/julia-lang/repl#Using REPL Modes

