---
title: "Usage of Modules"
slug: "usage-of-modules"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Module syntax
Module is a collection of type declarations, data declarations and procedures.
The basic syntax is:
    
    module module_name
      use other_module_being_used
    
      ! The use of implicit none here will set it for the scope of the module. 
      ! Therefore, it is not required (although considered good practice) to repeat 
      ! it in the contained subprograms. 
      implicit none
   
      ! Parameters declaration
      real, parameter, public :: pi = 3.14159
      ! The keyword private limits access to e parameter only for this module
      real, parameter, private :: e = 2.71828
    
      ! Type declaration
      type my_type
        integer :: my_int_var
      end type
    
      ! Variable declaration
      integer :: my_integer_variable
    
    ! Subroutines and functions belong to the contains section
    contains
    
      subroutine my_subroutine
        !module variables are accessible
        print *, my_integer_variable
      end subroutine

      real function my_func(x)
        real, intent(in) :: x
        my_func = x * x
      end function my_func
    end module

## Intrinsic modules
Fortran 2003 introduced intrinsic modules which provide access to special named constants, derived types and module procedures. There are now five standard intrinsic modules:

  - `ISO_C_Binding`; supporting C interoperability;
  - `ISO_Fortran_env`; detailing the Fortran environment;
  - `IEEE_Exceptions`, `IEEE_Arithmetic` and `IEEE_Features`; supporting so-called IEEE arithmetic facility.

These intrinsic modules are part of the Fortran library and accessed like other modules except that the `use` statement may have the intrinsic nature explicitly stated:

    use, intrinsic :: ISO_C_Binding

This ensures that the intrinsic module is used when a user-provided module of the same name is available.  Conversely

    use, non_intrinsic :: ISO_C_Binding

ensures that that same user-provided module (which must be accessible) is accessed instead of the intrinsic module.  Without the module nature specified as in

    use ISO_C_Binding

an available non-intrinsic module will be preferred over the intrinsic module.

---

The intrinsic IEEE modules are different from other modules in that their accessibility in a scoping unit may change the behaviour of code there even without reference to any of the entities defined in them.

## Using modules from other program units
To access entities declared in a module from another program unit (module, procedure or program), the module must be *used* with the `use` statement.

    module shared_data
      implicit none
    
      integer :: iarray(4) = [1, 2, 3, 4]
      real :: rarray(4) = [1., 2., 3., 4.]
    end module
   
    
    program test
    
      !use statements most come before implicit none
      use shared_data
    
      implicit none
    
      print *, iarray
      print *, rarray
    end program

The `use` statement supports importing only selected names

    program test
    
      !only iarray is accessible
      use shared_data, only: iarray
    
      implicit none
    
      print *, iarray
      
    end program

Entities can be also accessed under different name by using a *rename-list*:


    program test
    
      !only iarray is locally renamed to local_name, rarray is still acessible
      use shared_data, local_name => iarray
    
      implicit none
    
      print *, local_name

      print *, rarray
      
    end program

Further, renaming can be combined with the `only` option

    program test
      use shared_data, only : local_name => iarray
    end program

so that only the module entity `iarray` is accessed, but it has the local name `local_name`.

If selected for importing names mark as _private_ you can not import them to your program.

## Protected module entities
As well as allowing module entities to have access control (being `public` or `private`) modules entities may also have the `protect` attribute.  A public protected entity may be use associated, but the used entity is subject to restrictions on its use.

    module mod
      integer, public, protected :: i=1
    end module

    program test
      use mod, only : i
      print *, i   ! We are allowed to get the value of i
      i = 2        ! But we can't change the value
    end program test

A public protected target is not allowed to be pointed at outside its module

    module mod
      integer, public, target, protected :: i
    end module mod

    program test
      use mod, only : i
      integer, pointer :: j
      j => i   ! Not allowed, even though we aren't changing the value of i
    end program test

For a public protected pointer in a module the restrictions are different.  What is protected is the association status of the pointer

    module mod
      integer, public, target :: j
      integer, public, protected, pointer :: i => j
    end module mod

    program test
      use mod, only : i
      i = 2   ! We may change the value of the target, just not the association status
    end program test

As with variable pointers, procedure pointers may also be protected, again preventing change of target association.

## Access control
Accessibility of symbols declared in a module can be controlled using `private` and `public` attributes and statement.

Syntax of the statement form:

    !all symbols declared in the module are private by default
    private
    
    !all symbols declared in the module are public by default
    public
    
    !symbols in the list will be private
    private :: name1, name2
    
    !symbols in the list will be public
    public :: name3, name4

Syntax of the attribute form:

    integer, parameter, public :: maxn = 1000
    
    real, parameter, private :: local_constant = 42.24

Public symbols can be accessed from program units using the module, but private symbols cannot.

When no specification is used, the default is `public`.


The default access specification using

    private

or

    public

can be changed by specifying different access with *entity-declaration-list*

    public :: name1, name2

or using attributes.


This access control also affects symbols imported from another module:

    module mod1
      integer :: var1
    end module
    
    module mod2
      use mod1, only: var1
    
      public
    end module

    program test
      use mod2, only: var1
    end program

is possible, but


    module mod1
      integer :: var1
    end module
    
    module mod2
      use mod1, only: var1
    
      public
      private :: var1
    end module

    program test
      use mod2, only: var1
    end program

is not possible because `var` is private in `mod2`.

