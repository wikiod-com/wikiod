---
title: "Procedures - Functions and Subroutines"
slug: "procedures---functions-and-subroutines"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

*Functions* and *subroutines*, in conjunction with *modules*, are the tools to break down a *program* into units. This makes the program more readable and manageable. Each one of these units can be thought of as part of the code that, ideally, could be compiled and tested in isolation.
The main program(s) can call (or invoke) such subprograms (functions or subroutines) to accomplish a task.

Functions and subroutines are different in the following sense:

 - **Functions** return a single object and - usually - don't alter the values of its arguments (i.e. they act just like a mathematical function!);
 - **Subroutines** usually perform a more complicated task and they ordinarily alter their arguments (if any is present), as well as other variables (e.g. those declared in the module that contains the subroutine).

Functions and subroutines collectively go under the name of *procedures*. (In the following we will use the verb "call" as synonym of "invoke" even if technically the procedures to be `call`ed are `subroutine`s, whereas `function`s appear as right hand side of assignment or in expressions.)

## Function syntax
Functions can be written using several types of syntax

    function name()
      integer name
      name = 42
    end function

<!-- --> 
    
    integer function name()
      name = 42
    end function

<!-- --> 
    
    function name() result(res)
      integer res
      res = 42
    end function

Functions return values through a _function result_.  Unless the function statement has a `result` clause the function's result has the same name as the function.  With `result` the function result is that given by the `result`.  In each of the first two examples above the function result is given by `name`; in the third by `res`.

The function result must be defined during execution of the function.

Functions allow to use some special prefixes.

_Pure_ function means that this function has no side effect:

    pure real function square(x)
      real, intent(in) :: x
      square = x * x
    end function


_Elemental_ function is defined as scalar operator but it can be invoked with array as actual argument in which case the function will be applied element-wise. Unless the `impure` prefix (introduced in Fortran 2008) is specified an _elemental_ function is also a _pure_ function.

    elemental real function square(x)
      real, intent(in) :: x
      square = x * x
    end function

## Return statement
The `return` statement can be used to exit function and subroutine. Unlike many other programming languages it is not used to set the return value.

    real function f(x)
      real, intent(in) :: x
      integer :: i

      f = x

      do i = 1, 10

        f = sqrt(f) - 1.0

        if (f < 0) then
          f = -1000.
          return
        end if

      end do
    end function

This function performs an iterative computation. If the value of `f` becomes negative the function returns value -1000.

## Recursive Procedures
In Fortran functions and subroutines need to be explicitly declared as _recursive_, if they are to call themselves again, directly or indirectly. Thus, a recursive implementation of the Fibonacci series could look like this:

    recursive function fibonacci(term) result(fibo)
      integer, intent(in) :: term
      integer :: fibo

      if (term <= 1) then
        fibo = 1
      else
        fibo = fibonacci(term-1) + fibonacci(term-2)
      end if
      
    end function fibonacci

Another example is allowed to calculate factorial:

    recursive function factorial(n)  result(f)
      integer :: f
      integer, intent(in) :: n
      
      if(n == 0) then
        f = 1
      else
        f = n * f(n-1)
      end if
    end function factorial

For a function to directly recursively reference itself its definition must use the `result` suffix.
It is not possible for a function to be both `recursive` and `elemental`.

## The Intent of Dummy Arguments
The `intent` attribute of a dummy argument in a subroutine or function declares its intended use. The syntax is either one of

    intent(IN)
    intent(OUT)
    intent(INOUT)

For example, consider this function: 

    real function f(x)
      real, intent(IN) :: x
    
      f = x*x
    end function

The `intent(IN)` specifies that the (non-pointer) dummy argument `x` may never be defined or become undefined throughout the function or its initialization. If a pointer dummy argument has the attribute `intent(IN)`, this applies to its association. 

`intent(OUT)` for a non-pointer dummy argument means that dummy argument becomes undefined on invocation of the subprogram (except for any components of a derived type with default initialization) and is to be set during execution. The actual argument passed as dummy argument must be definable: passing a named or literal constant, or an expression, is not allowed.

Similarly to before, if a pointer dummy argument is `intent(OUT)` the association status of the pointer becomes undefined.  The actual argument here must be a pointer variable.

`intent(INOUT)` specifies that the actual argument is definable and is suitable for both passing in and returning data from the procedure.

Finally, a dummy argument may be without the `intent` attribute.  Such a dummy argument has its use limited by the actual argument passed.

For example, consider

    integer :: i = 0
    call sub(i, .TRUE.)
    call sub(1, .FALSE.)

    end

    subroutine sub(i, update)
      integer i
      logical, intent(in) :: update
      if (update) i = i+1
    end subroutine

The argument `i` can have no `intent` attribute which allows both of the subroutine calls of the main program.

## Referencing a procedure
For a function or subroutine to be useful it has to be referenced.  A subroutine is referenced in a `call` statement

    call sub(...)

and a function within an expression.  Unlike in many other languages, an expression does not form a complete statement, so a function reference is often seen in an assignment statement or used in some other way:

    x = func(...)
    y = 1 + 2*func(...)

There are three ways to designate a procedure being referenced:

 - as the name of a procedure or procedure pointer
 - a procedure component of a derived type object
 - a type bound procedure binding name

The first can be seen as

    procedure(), pointer :: sub_ptr=>sub
    call sub()   ! With no argument list the parentheses are optional
    call sub_ptr()
    end

    subroutine sub()
    end subroutine

and the final two as

    module mod
      type t
        procedure(sub), pointer, nopass :: sub_ptr=>sub
      contains
        procedure, nopass :: sub
      end type

    contains

      subroutine sub()
      end subroutine

    end module

    use mod
    type(t) x
    call x%sub_ptr()   ! Procedure component
    call x%sub()       ! Binding name

    end

---

For a procedure with dummy arguments the reference requires corresponding _actual_ arguments, although optional dummy arguments may be not given.

Consider the subroutine

    subroutine sub(a, b, c)
      integer a, b
      integer, optional :: c
    end subroutine

This may be referenced in the following two ways

    call sub(1, 2, 3)   ! Passing to the optional dummy c
    call sub(1, 2)      ! Not passing to the optional dummy c

This is so-called _positional_ referencing: the actual arguments are associated based on the position in the argument lists.  Here, the dummy `a` is associated with `1`, `b` with `2` and `c` (when specified) with `3`.

Alternatively, _keyword_ referencing may be used when the procedure has an explicit interface available

    call sub(a=1, b=2, c=3)
    call sub(a=1, b=2)

which is the same as the above.

However, with keywords the actual arguments may be offered in any order

    call sub(b=2, c=3, a=1)
    call sub(b=2, a=1)

Positional and keyword referencing may both be used

    call sub(1, c=3, b=2)

as long as a keyword is given for every argument following the first appearance of a keyword

    call sub(b=2, 1, 3)  ! Not valid: all keywords must be specified

The value of keyword referencing is particularly pronounced when there are multiple optional dummy arguments, as seen below if in the subroutine definition above `b` were also optional

    call sub(1, c=3)  ! Optional b is not passed

---

The argument lists for type-bound procedures or component procedure pointers with a passed argument are considered separately.
    

