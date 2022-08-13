---
title: "Getting started with OCaml"
slug: "getting-started-with-ocaml"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
# Installing OPAM

[OPAM][1] is a package manager for OCaml. It builds and manages compiler versions and OCaml libraries for you easily.

The easiest way to install OPAM on your operating system is to use a package manager for your system. e.g apt-get, yum or homebrew.

## Mac OSX Installation Instructions

Update [homebrew][2] formulae and install OPAM.

```
brew update
brew install opam
```

## Ubuntu Installation Instructions

```
add-apt-repository ppa:avsm/ppa
apt-get update
apt-get install ocaml opam
```

## Compiling from source

```
wget http://caml.inria.fr/pub/distrib/ocaml-4.03/ocaml-4.03.0.tar.gz
tar xf ocaml-4.03.0.tar.gz
cd ocaml-4.03.0
./configure -prefix installation_path
make world.opt
make install
```

## Initializing OPAM

Once you have OPAM installed, run `opam init` and follow the instructions.

Once done you should be able to run the OCaml interpreter from your shell.

```
$ ocaml
        OCaml version 4.03.0

#
```

  [1]: https://opam.ocaml.org/
  [2]: http://brew.sh/


## Your first program in OCaml
Now that the OCaml distribution is available on your favorite operating system, we can create your first program in OCaml: the Hello World!

We have different ways to launch an OCaml program.

# The REPL (toplevel)

You can execute your code *interactively* with the *toplevel*. With the OCaml *toplevel*, you can write and execute OCaml code, as a UNIX shell. Afterwards, the *toplevel* checks the type of your code immediately. So, you can quickly and easily test some parts of code without compilation and execution.

You can launch the *toplevel* with the `ocaml` command. Then, you can write an OCaml *sentence* ended by `;;` which is evaluated immediately. The *toplevel* displays the type and the value of your expression just after:

    # "Hello Worlds!";;
    - : string = "Hello Worlds!"

It is also possible to launch the *toplevel* on your file. You can see this [explanation][1] about that.

To facilitate your input in the *toplevel*, you can use a tool like `ledit` or `rlwrap` which provides some features (like input history):

     $ ledit ocaml

     $ rlwrap ocaml

# Compilation to the *bytecode*

We have two different compilers, one which compiles to *bytecode* and the other which compiles to native code. The first is the same as the *bytecode* of the Java's virtual machine. So, the *bytecode* is less efficient but more portable.

We have some extensions files used by the OCaml compilers:

extension        | definition
---------------- | ----------
`.ml`            | The source code (as `.c` in C)
`.mli`           | The interface (as `.h` in C)
`.cmo`           | Source code compiled by `ocamlc` in *bytecode*
`.cmi`           | Interface code compiled by `ocamlc`
`.cmx` and `.o`  | Source code compiled by `ocamlopt` in native code
`.cma`           | Library (bucket of some `*.cmo`) in *bytecode*
`.cmxa` and `.a` | Library in native code
`.cmxs`          | Library in native code (to load dynamicaly)

The *bytecode* compiler is `ocamlc`.

You have different common options:
* `-c`: to compile a source file without the linkage process (to produce an executable). So, the command `ocaml -c foo.ml` produces a `.cmo` file. Unlike C in which the header file does not need to be compiled, it's necessary in OCaml to compile the `.mli` file: `ocaml -c foo.mli`.

You need to compile the interface first. When you compile the source file afterwards, OCaml tries to check that the implementation matches the interface.

The `.mli` file is not a mandatory. If you compile a `.ml` file without a `.mli` file, OCaml will produce a `.cmi` file automatically.

* `-o`: to compile some `.cmo` files to an executable. For example: `ocamlc -o program foo.cmo bar.cmo`. These files need to be arranged by the dependencies for which the first file has no dependence.

* `-I`: to indicate an other directory where the compiler can find the necessary files for the compilation (like the interface or source code). It's the same than the `-I` from a C compiler.

We have many other options. You can see the [manual][manual] for more information.

So, you can write the `hello.ml` now, and compile this file with `ocamlc -o hello hello.ml` to produce a *bytecode* program:

    let () = print_endline "Hello World!"

The `let () = ...` is the first entry of your program (like the `main` in C). After, we use the function `print_endline` (provided by the standard library) with the argument `"Hello World!"` to print `Hello Worlds` with a newline in the standard output.

After the compilation, you have the `.cmo` file and the `.cmi` file automatically produced by the compiler and your program `hello`. You can open your program, and in the top of this file, you can see:

    #!/usr/local/bin/ocamlrun

That means your program need the `ocamlrun` program (provided by the distribution) to execute the *bytecode* (like the JVM).

## Compilation to the native code

We have an another compiler that produces native code. The compiler is: `ocamlopt`. However, the resultant executable can't work on most other architectures.

`ocamlopt` uses the same options as `ocamlc` so you can execute `ocamlopt -o hello hello.ml`. After, you can see a `.cmx` and a `.o` file.

Finally, from your *bytecode*/native code program, you can execute:

    $ ./hello
    Hello World!
    $


  [1]: https://www.wikiod.com/ocaml/write-your-first-ocaml-script#Hello World
  [manual]: http://caml.inria.fr/pub/docs/manual-ocaml/comp.html


## Installation on Windows (native)
Premise
=======
These instruction shows a procedure to install native OCaml binaries in Windows. If your operative system is `Windows 10 (Insider Preview) build 14316` or later you can also install OCaml through [Bash on Ubuntu on Windows][1]. In this case, follow the instruction to install OCaml on Ubuntu.

Install OCaml and Opam
===================
Download [OCaml official distribution][2]. It contains both OCaml compilers and Opam packet manager. Suppose you have installed the software in `C:/OCaml`. To be sure you've correcly installed OCaml open `cmd.exe` and type `ocaml`. 

If you see the message `'ocaml' is not recognized as an internal or external command, operable program or batch file` you need to add `C:/OCaml/bin` to your Path (Environment Variable).

Add OCaml binaries to path
--------------------------

 in `Control Panel > System and Security > System > Advanced system settings (on the left) > Environment Variables` and then select `Path` in `System Variable` tab, then `Edit`. 

Add `C:/OCaml/bin;` to the list.

Install Cygwin
==============
Without Cygwin you can't use Opam. In fact, if you try to open Opam typing `opam` in `cmd.exe` it shows a message: `Fatal error: exception Unix.Unix_error(20, "create_process", "cygcheck")`.

Download [Cygwin][3] and start the installer. Be sure to check the following packages:

 - automake
 - diffutils
 - libreadline
 - make
 - m4
 - mingw64-x86_64-gcc-core
 - mingw64-x86_64-gmp
 - mingw64-x86_64-openssl
 - mingw64-x86_64-pkg-config
 - mingw64-x86_64-sqlite3
 - patch
 - rlwrap
 - unzip
 - wget

Suppose you have installed the software in `C:/cygwin` (`C:/cygwin64` for 64bit version). Open `cmd` and type `wget` (or one of the executable present in `C:/cygwin/bin`) to check if you can use the Cygwin executables. If the executable won't open, add `C:/cygwin/bin` to your Path (Environment Variable).

Configure Opam
==============
Open `cmd.exe` and type `opam init` to configure Opam.

Then install `ocamlfind` (part of the OCaml compiler) with 

    opam install ocamlfind
    opam config env

Check if `ocamlfind` is installed typing it in `cmd.exe`.

The command `opam config env` is used to add `opam`'s executables directory to the enviroment path. If after logout you cannot reach `ocamlfind` anymore, you can manually add it adding to path the following line: `C:/Users/<your user>/Documents/.opam/system/bin/`.

Installing packages
===================
Packages are installed through Opam with the command `opam install xyz` where `xyz` is the name of the package.

Install UTop
------------
Try running the command `opam install utop`. If you have no errors, then typing `utop` will open the executable.

If you see the message

    [ERROR] The compilation of zed failed at "ocaml setup.ml -build".

you have to manually install the single packages. Try again typing:

    opam install zed
    opam install lambda-term
    opam install utop

Both `lambda-term` and `utop` might not install. See Troubleshoot section.

Installing Core
---------------
You can install `core` package with `opam install core`. On Windows 64bit version (and 64bit Cygwin) you will see the following error:

    [ERROR] core is not available because your system doesn't comply with os != "win32" & ocaml-version = "4.02.3".


Troubleshoot: cannot create regular file
--------------------------------------------
If package with name `xyz.10.1` fails to install (where xyz is the name of the package, and 10.1 its version) with the following message:

    install: cannot create regular file '/cygdrive/c/Users/<your user>/Documents/.opam/system/bin/<something>': File exists

You have to go in this directory:

    C:\Users\<your user>\Documents\.opam\repo\default\packages\<xyz>\<xyz.10.1>\files 

and delete the file `xyz.10.1.install`.

Troubleshoot: cannot load shared library
----------------------------------------
If you try to open some Opam's package (eg: `utop`) and you see this error:

    Fatal error: cannot load shared library dlllwt-unix_stubs
    Reason: The specified module could not be found.

Run `opam config env` again and try to reopen the executable.


  [1]: https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux
  [2]: http://protz.github.io/ocaml-installer/
  [3]: https://cygwin.com/install.html

