---
title: "Write your first OCaml Script"
slug: "write-your-first-ocaml-script"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Hello World
This example assumes you've [installed OCaml][1]. 

# Compiling OCaml Code

Create a new file named `hello.ml`, with the following contents:

    print_string "Hello world!\n"

`ocamlc` is the OCaml compiler. To compile and run this script, run

    $ ocamlc -o hello hello.ml

and then execute the resulting binary

    $ ./hello
    Hello world!

# Executing OCaml Code

You can also run this script without compiling it into a binary. You can do so by using `ocaml`, the ocaml toplevel system that permits interactive use of OCaml. In your shell, simply run

    $ ocaml hello.ml
    Hello world!

# In the REPL

Open a new shell, and type `ocaml` to open the toplevel system. Once in the session, you can type the same program:

           OCaml version 4.02.1

    # print_string "hello world!\n";;

press enter to evaluate the expression, and trigger the print.
    
    hello world!
    - : unit = ()
             
Success! We see it printed `hello world!`, but what is the `- : unit = ()` about? OCaml has no statements, everything is an expression that evaluates to some typed value. In this case, `print_string` is a function that takes in a `string `as input, and returns a `unit`. Think of `unit` as a type that can only take one value, `()` (also referred to as unit), and represents a finished computation that returns no meaningful value. 

In this case, `print_string` also has the side-effect of putting characters it received as input onto the screen, which is why we see the first line. 

To exit the REPL, press `ctrl+D`.

# As a Unix script

We have two ways to create an OCaml script. The first use the system toplevel (provided by your package manager like `apt-get`) and the second use the toplevel provided by [OPAM][1].

## Use the system toplevel

Open your favorite editor, and write:

    #!/usr/bin/ocaml
    
    print_string "hello worlds!\n";;

After, you can use `chmod +x your_file.ml` and you can execute your script with `./your_file.ml`.

## Use the toplevel provided by OPAM

    #!/usr/bin/env ocaml
    
    print_string "hello worlds!\n";;

The big difference is about the version of your toplevel. Indeed, if you configured your OPAM with a specific switch (like `opam switch 4.03.0`), the script will use OCaml 4.03.0. In the first way, in Debian Sid for example, the script will use OCaml 4.02.3.

You can replace the [*shebang*][2] by `#!/usr/bin/env utop` to use `utop` instead the vanilla toplevel.

# utop

`utop` is another ocaml toplevel outside the distribution - that means, you need to download and install `utop` (the easy way is to use OPAM: `opam install utop`). `utop` has many features like the historic, the completion and the interactive line editing.

So, if you want an easy way to try some ocaml codes, `utop` is the best.

## Why `utop` and not `ocaml`?

`utop` and `ocaml` have no a big difference if you want an ocaml script like above. But the common thing in the OCaml community is to use `utop` instead `ocaml`.

In fact, the `ocaml` REPL is provided by the ocaml distribution. So, this REPL follows the release cycle of the compiler and if you want some extras features, you need to wait the next release of the compiler. `utop`, as we explained, is outside the distribution, so the release cycle is not constraint by the compiler and if you want an extra feature, you will be more likely to try to push this feature inside `utop` than `ocaml` :) !

For this point (and for the historic feature) most people in the ocaml community prefer to use `utop` than `ocaml`.

  [1]: https://www.wikiod.com/ocaml/getting-started-with-ocaml
  [2]: https://en.wikipedia.org/wiki/Shebang_(Unix)

