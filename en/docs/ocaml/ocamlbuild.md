---
title: "Ocamlbuild"
slug: "ocamlbuild"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Basic example with no external dependency
If your project has no external dependency and has `foo.ml` as its main entry point, you can compile a bytecode version with
```
ocamlbuild foo.byte
```

To get a native executable, run
```
ocamlbuild foo.native
```

## Project depending on external libraries
If your project depends on the external libraries, you should first install them with opam.
Assuming your dependencies are `foo` and `bar` and the main entry point of your project is `foobar.ml` you can then build a bytecode executable with

```
ocamlbuild -use-ocamlfind -pkgs 'foo,bar' foobar.byte
```

Warning: the names `foo` and `bar` must be the names of the ocamlfind packages, they may differ from the names of the opam packages.

Instead of specifying the packages on the command line, you can create a config file named `_tags` with the following content

```
true: package(foo), package(bar)
```


