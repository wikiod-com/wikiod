---
title: "Program units and file layout"
slug: "program-units-and-file-layout"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Internal subprograms
A program unit which is not an internal subprogram may contain other program units, called _internal subprograms_.

    program prog
      implicit none
    contains
      function f()
      end function f
      subroutine g()
      end subroutine g
    end program

Such an internal subprogram has a number of features:

  - there is host association between entities in the subprogram and the outer program
  - implicit typing rules are inherited (`implicit none` is in effect in `f` above)
  - internal subprograms have an explicit interface available in the host

Module subprograms and external subprograms may have internal subprograms, such as

    module mod
      implicit none
    contains
      function f()
      contains
        subroutine s()
        end subroutine s
      end function f
    end module mod

## Fortran programs
A complete Fortran program is made up from a number of distinct program units.  Program units are:
 - main program
 - function or subroutine subprogram
 - module or submodule
 - block data program unit

The main program and some procedure (function or subroutine) subprograms may be provided by a language other than Fortran.  For example a C main program may call a function defined by a Fortran function subprogram, or a Fortran main program may call a procedure defined by C.

These Fortran program units may be given be distinct files or within a single file.

For example, we may see the two files:

_prog.f90_

    program main
      use mod
    end program main

_mod.f90_

    module mod
    end module mod

And the compiler (invoked correctly) will be able to associate the main program with the module.

The single file may contain many program units

_everything.f90_

    module mod
    end module mod

    program prog
      use mod
    end program prog

    function f()
    end function f()

In this case, though, it must be noted that the function `f` is still an _external function_ as far as the main program and module are concerned.  The module will be accessible by the main program, however.

Typing scope rules apply to each individual program unit and not to the file in which they are contained.  For example, if we want each scoping unit to have no implicit typing, the above file need be written as

    module mod
      implicit none
    end module mod

    program prog
      use mod
      implicit none
    end program prog

    function f()
      implicit none
      <type> f
    end function f

## Modules and submodules
Modules are [documented elsewhere](https://www.wikiod.com/fortran/usage-of-modules).

Compilers often generate so-called _module files_: usually the file containing

    module my_module
    end module

will result in a file named something like `my_module.mod` by the compiler.  In such cases, for a module to be accessible by a program unit, that module file must be visible before this latter program unit is processed.

## External procedures
An external procedure is one which is defined outside another program unit, or by a means other than Fortran.

The function contained in a file like

    integer function f()
      implicit none
    end function f

is an external function.

For external procedures, their existence may be declared by using an interface block (to given an explicit interface)

    program prog
      implicit none
      interface
        integer function f()
      end interface
    end program prog

or by a declaration statement to give an implicit interface

    program prog
      implicit none
      integer, external :: f
    end program prog

or even

    program prog
      implicit none
      integer f
      external f
    end program prog

The `external` attribute is not necessary:

    program prog
      implicit none
      integer i
      integer f
      i = f()   ! f is now an external function
    end program prog

## Source code files
A source code file is a (generally) plain text file which is to processed by the compiler.  A source code file may contain up to one main program and any number of modules and external subprograms.  For example, a source code file may contain the following

    module mod1
    end module mod1

    module mod2
    end module mod2

    function func1()    ! An external function
    end function func1

    subroutine sub1()   ! An external subroutine
    end subroutine sub1

    program prog        ! The main program starts here...
    end program prog    ! ... and ends here

    function func2()    ! An external function
    end function func2

We should recall here that, even though the external subprograms are given in the same file as the modules and the main program, the external subprograms are not explicitly known by any other component.

Alternatively, the individual components may be spread across multiple files, and even compiled at different times.  Compiler documentation should be read on how to combine multiple files into a single program.

A single source code file may contain either [fixed-form](https://www.wikiod.com/fortran/modern-alternatives-to-historical-features#Fixed Source Form) or free-form source code: they cannot be mixed, although multiple files being combined at compile-time may have different styles.

To indicate to the compiler the source form there are generally two options:

  - choice of filename suffix
  - use of compiler flags

The compile-time flag to indicate fixed- or free-form source can be found in the compiler's documentation.

The significant filename suffixes are also to be found in the compiler's documentation, but as a general rule a file named `file.f90` is taken to contain free-form source whereas the file `file.f` is taken to contain fixed-form source.

The use of `.f90` suffix to indicate free-form source (which was introduced in the Fortran 90 standard) often tempts the programmer to use the suffix to indicate the language standard to which the source code conforms.  For example, we may see files with `.f03` or `.f08` suffixes.  This is generally to be avoided: most Fortran 2003 source is also compliant with Fortran 77, Fortran 90/5 and Fortran 2008.  Further, many comilers don't automatically consider such suffixes.

---

Compilers also often offer a built-in code preprocessor (generally based on cpp).  Again, a compile-time flag may be used to indicate that the preprocessor should be run before compilation, but the source code file suffix may also indicate such preprocessing requirement.

For case-sensitive filesystems the file `file.F` is often taken to be a fixed-form source file to be preprocessed and `file.F90` to be a free-form source file to be preprocessed.  As before, the compiler's documentation should be consulted for such flags and file suffixes.

## Block data program units
Block data program units are program units which provide initial values for objects in common blocks.  These are deliberately left undocumented here, and will feature in the documentation of historic Fortran features.

