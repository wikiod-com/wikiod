---
title: "Compiling an MPI Program"
slug: "compiling-an-mpi-program"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

MPI needs to add extra libraries and include directories to your compilation line when compiling your program. Rather than tracking all of them yourself, you can usually use one of the compiler wrappers.

## C Wrapper
    mpicc -o my_prog my_prog.c

