---
title: "Performance"
slug: "performance"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Abstract machine
For efficiency, Prolog code is typically compiled to **abstract machine code** before it is run.

Many different abstract machine architectures and variants have been proposed for efficient execution of Prolog programs. These include:

- **WAM**, the *Warren Abstract Machine*
- **TOAM**, an abstract machine used in B-Prolog.
- **ZIP**, used for example as the basis for the VM of SWI-Prolog
- **VAM**, a research architecture developped in Vienna.

## Tail call optimization
Virtually all Prolog systems implement **tail call optimization**&nbsp;(TCO). This means that predicate calls that are in a *tail&nbsp;position* can be executed in *constant* stack space if the predicate is deterministic.

Tail **recursion** optimization (TRO) is a special case of tail call optimization.

## Indexing
All widely used Prolog interpreters use **argument indexing** to efficiently select suitable clauses.

Users can typically rely on at least *first argument indexing*, meaning that clauses can be efficiently told apart by the functor and arity of the outermost term of the *first* argument. In calls where that argument is sufficiently instantiated, matching clauses can essentially be selected in *constant* time via hashing on that argument.

More recently, **JIT indexing** has been implemented in more systems, enabling dynamic indexing on any argument that is sufficiently instantiated when the predicate is called.

