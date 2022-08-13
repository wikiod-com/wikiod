---
title: "C interoperability"
slug: "c-interoperability"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Calling C from Fortran
Fortran 2003 introduced language features which can guarantee interoperability between C and Fortran (and to more languages by using C as an intermediary). These features are mostly accessed through the intrinsic module `iso_c_binding`:

    use, intrinsic :: iso_c_binding

The `intrinsic` keyword here ensures the correct module is used, and not a user created module of the same name.

`iso_c_binding` gives access to _interoperable_ kind type parameters:

    integer(c_int) :: foo    ! equivalent of 'int foo' in C
    real(c_float) :: bar     ! equivalent of 'float bar' in C

Use of C kind type parameters guarantees that the data can be transferred between C and Fortran programs.

*Interoperability of C char and Fortran characters is probably a topic for itself and so not discussed here*

---

To actually call a C function from Fortran, first the interface must be declared. This is essentially equivalent to the C function prototype, and lets the compiler know about the number and type of the arguments, etc.
The `bind` attribute is used to tell the compiler the name of the function in C, which may be different to the Fortran name.

*geese.h*
<!-- language: lang-c -->
    // Count how many geese are in a given flock
    int howManyGeese(int flock);

*geese.f90*
    
    ! Interface to C routine
    interface
      integer(c_int) function how_many_geese(flock_num) bind(C, 'howManyGeese')
        ! Interface blocks don't know about their context,
        ! so we need to use iso_c_binding to get c_int definition
        use, intrinsic :: iso_c_binding, only : c_int
        integer(c_int) :: flock_num
      end function how_many_geese
    end interface

The Fortran program needs to be linked against the C library (*compiler dependent, include here?*) that includes the implementation of `howManyGeese()`, and then `how_many_geese()` can be called from Fortran.

## C structs in Fortran
The `bind` attribute can also be applied to derived types:

*geese.h*

<!-- language: lang-c -->

    struct Goose {
       int flock;
       float buoyancy;
    }

    struct Goose goose_c;

*geese.f90*

    use, intrinsic :: iso_c_binding, only : c_int, c_float

    type, bind(C) :: goose_t
      integer(c_int) :: flock
      real(c_float) :: buoyancy
    end type goose_t

    type(goose_t) :: goose_f

Data can now be transferred between `goose_c` and `goose_f`. C routines which take arguments of type `Goose` can be called from Fortran with `type(goose_t)`.


