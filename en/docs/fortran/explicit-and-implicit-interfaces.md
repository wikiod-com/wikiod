---
title: "Explicit and implicit interfaces"
slug: "explicit-and-implicit-interfaces"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## External subprograms and implicit interfaces
A subprogram is said to be *external* when it is not contained in the main program, nor in a module or antoher subprogram. In particular it can be defined by means of a programming language other than Fortran.

When an external subprogram is invoked, the compiler cannot access to its code, so all the information allowable to the compiler is implicitly contained in the calling statement of the calling program and in the type an properties of the acutal arguments, not the dummy arguments (whose declaration is unknown to the compiler).
In this case we say that the interface is *implicit*.

An `external` statement can be used to specify that a procedure's name is relative to an external procedure,

    external external_name_list

but even so, the interface remain implicit.

An `interface` block can be used to specify the interface of an external procedure,

    interface
      interface_body
    end interface

where the `interface_body` is normally an exact copy of the procedure header followed by the declaration of all its arguments and, if it is a function, of the result. 

For example, for function `WindSpeed` 

    real function WindSpeed(u, v)
      real, intent(in) :: u, v
      WindSpeed = sqrt(u*u + v*v)
    end function WindSpeed

You can write the following interface

    interface
      real function WindSpeed(u, v)
        real, intent(in) :: u, v
      end function WindSpeed
    end interface

## Internal/module subprograms and explicit interfaces
A *subprogram* (which defines a *procedure*), can be either a `subroutine` or a `function`; it is said to be an *internal subprogram* if it is called or invoked from the same `program` or *subprogram* that `contains` it, as follows

    program my_program

      ! declarations
      ! executable statements,
      ! among which an invocation to
      ! internal procedure(s),
      call my_sub(arg1,arg2,...)
      fx = my_fun(xx1,xx2,...)

    contains
      
      subroutine my_sub(a1,a2,...)
        ! declarations
        ! executable statements
      end subroutine my_sub

      function my_fun(x1,x2,...) result(f)
        ! declarations
        ! executable statements
      end function my_fun

    end program my_program

In this case the compiler will know all about any internal procedure, since it treats the program unit as a whole. In particular, it will "see" the procedure's `interface`, that is

 - whether it is a `function` or `subroutine`,
 - which are the names and properties of the arguments `a1`, `a2`, `x1`, `x2`, ...,
 - which are the properties of the *result* `f` (in the case of a `function`).

Being the interface known, the compiler can check whether the actual arguments (`arg1`, `arg2`, `xx1`, `xx2`, `fx`, ...) passed to the procedure match with the dummy arguments (`a1`, `a2`, `x1`, `x2`, `f`, ...).

In this case we say that the interface is *explicit*.

A subprogram is said to be *module subprogram* when it is invoked by a statement in the containing module itself,

    module my_mod

      ! declarations

    contains
      
      subroutine my_mod_sub(b1,b2,...)
        ! declarations
        ! executable statements
        r = my_mod_fun(b1,b2,...)
      end subroutine my_sub

      function my_mod_fun(y1,y2,...) result(g)
        ! declarations
        ! executable statements
      end function my_fun

    end module my_mod


 or by a statement in another program unit that `use`s that module,

    program my_prog

      use my_mod

      call my_mod_sub(...)

    end program my_prog

 As in the preceding situation, the compiler will know everything about the subprogram and, therefore, we say that the interface is *explicit*.

