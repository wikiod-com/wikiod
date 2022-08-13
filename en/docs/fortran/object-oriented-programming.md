---
title: "Object Oriented Programming"
slug: "object-oriented-programming"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Derived type definition
Fortran 2003 introduced support for object oriented programming. This feature allows to take advantage of modern programming techniques. Derived types are defined with the following form:

    TYPE [[, attr-list] :: ] name [(name-list)]
       [def-stmts]
       [PRIVATE statement or SEQUENCE statement]. . .
       [component-definition]. . .
       [procedure-part]
    END TYPE [name]

where,

 - **attr-list** - a list of attribute specifiers
 - **name** - the name of derived data type
 - **name-list** - a list of type parameter names separated by commas
 - **def-stmts** -  one or more INTEGER declarations of the type parameters named in the name-list
 - **component-definition** - one or more type declaration statements or procedure pointer statements defining the component of derived type
 - **procedure-part** - a CONTAINS statement, optionally followed by a PRIVATE statement, and one or more procedure binding statements

Example:

    type shape
        integer :: color
    end type shape

## Abstract derived types
An extensible derived type may be _abstract_

    type, abstract :: base_type
    end type

Such a derived type may never be instantiated, such as by

    type(base_type) t1
    allocate(type(base_type) :: t2)

but a polymorphic object may have this as its declared type

    class(base_type), allocatable :: t1

or

    function f(t1)
      class(base_type) t1
    end function

Abstract types may have components and type-bound procedures

    type, abstract :: base_type
      integer i
    contains
      procedure func
      procedure(func_iface), deferred :: def_func
    end type

The procedure `def_func` is a _deferred_ type-bound procedure with interface `func_iface`.  Such a deferred type-bound procedure must be implemented by each extending type. 

## Type Procedures
In order to obtain class-like behavior, type and related procedures (subroutine and functions) shall be placed in a module:

Example:

    module MShape
        implicit none
        private
    
        type, public :: Shape
        private
            integer :: radius
        contains
            procedure :: set   => shape_set_radius
            procedure :: print => shape_print
        end type Shape
    
    contains
        subroutine shape_set_radius(this, value)
            class(Shape), intent(in out) :: self
            integer, intent(in)          :: value
    
            self%radius = value
        end subroutine shape_set_radius

        subroutine shape_print(this)
            class(Shape), intent(in) :: self
    
            print *, 'Shape: r = ', self%radius
        end subroutine shape_print
    end module MShape

Later, in a code, we can use this Shape class as follows:

    ! declare a variable of type Shape
    type(Shape) :: shape
    
    ! call the type-bound subroutine
    call shape%set(10)
    call shape%print 

         

## Type extension
A derived type is _extensible_ if it has neither the `bind` attribute nor the `sequence` attribute.  Such a type may be extended by another type.

    module mod

      type base_type
        integer i
      end type base_type

      type, extends(base_type) :: higher_type
        integer j
      end type higher_type

    end module mod

A polymorphic variable with declared type `base_type` is type compatible with type `higher_type` and may have that as dynamic type

    class(base_type), allocatable :: obj
    allocate(obj, source=higher_type(1,2))

Type compatability descends through a chain of children, but a type may extend only one other type.

An extending derived type inherits type bound procedures from the parent, but this can be overriden

    module mod

      type base_type
      contains
        procedure :: sub => sub_base
      end type base_type

      type, extends(base_type) :: higher_type
      contains
        procedure :: sub => sub_higher
      end type higher_type

    contains

      subroutine sub_base(this)
        class(base_type) this
      end subroutine sub_base

      subroutine sub_higher(this)
        class(higher_type) this
      end subroutine sub_higher

    end module mod

    program prog
      use mod

      class(base_type), allocatable :: obj

      obj = base_type()
      call obj%sub

      obj = higher_type()
      call obj%sub

    end program

## Type constructor
Custom constructors can be made for derived types by using an interface to overload the type name. This way, keyword arguments that don't correspond to components can be used when constructing an object of that type.
    
    module ball_mod
      implicit none
    
      ! only export the derived type, and not any of the
      ! constructors themselves
      private
      public :: ball
    
      type :: ball_t
         real :: mass
      end type ball_t
    
      ! Writing an interface overloading 'ball_t' allows us to
      ! overload the type constructor
      interface ball_t
         procedure :: new_ball
      end interface ball_t
    
    contains
    
      type(ball_t) function new_ball(heavy)
        logical, intent(in) :: heavy
    
        if (heavy) then
           new_ball%mass = 100
        else
           new_ball%mass = 1
        end if
        
      end function new_ball
      
    end module ball_mod
    
    program test
      use ball_mod
      implicit none
    
      type(ball_t) :: football
      type(ball_t) :: boulder
      
      ! sets football%mass to 4.5
      football = ball_t(4.5)
      ! calls 'ball_mod::new_ball'
      boulder = ball_t(heavy=.true.)
    end program test

This can be used to make a neater API than using separate initialisation routines:

    subroutine make_heavy_ball(ball)
      type(ball_t), intent(inout) :: ball
      ball%mass = 100
    end subroutine make_heavy_ball

    ...

    call make_heavy_ball(boulder)

