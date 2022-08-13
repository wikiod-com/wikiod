---
title: "Execution Control"
slug: "execution-control"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## If construct
The `if` construct (called a block IF statement in FORTRAN 77) is common across many programming languages. It conditionally executes one block of code when a logical expression is evaluated to true.

    [name:] IF (expr) THEN
        block
    [ELSE IF (expr) THEN [name]
        block]
    [ELSE [name]
       block]
    END IF [name]

where,

 - **name**    - the name of the if construct (optional) 
 - **expr**    - a scalar logical expression enclosed in parentheses 
 - **block**   - a sequence of zero or more statements or constructs

A construct name at the beginning of an `if then` statement must have the same value as the construct name at the `end if` statement, and it should be unique for the current scoping unit.

In `if` statements, (in)equalities and logical expressions evaluating a statement can be used with the following operators:

    .LT.  which is <   ! less than
    .LE.           <=  ! less than or equal
    .GT.           >   ! greater than
    .GE.           >=  ! greater than or equal
    .EQ.           =   ! equal
    .NE.           /=  ! not equal
    .AND.              ! logical and
    .OR.               ! logical or
    .NOT.              ! negation

Examples:

    ! simplest form of if construct
    if (a > b) then
        c =  b / 2
    end if
    !equivalent example with alternate syntax
    if(a.gt.b)then
       c=b/2
    endif

    ! named if construct
    circle: if (r >= 0) then
        l = 2 * pi * r
    end if circle

    ! complex example with nested if construct
    block: if (a < e) then
        if (abs(c - e) <= d) then
            a = a * c
        else
            a = a * d
        end if
    else
        a = a * e
    end if block

---

A historical usage of the `if` construct is in what is called an "arithmetic if" statement.  Since this can be replaced by more modern constructs, however, it is not covered here.  More details can be found [here][1].  


  [1]: https://www.wikiod.com/fortran/modern-alternatives-to-historical-features#Arithmetic if statement

## SELECT CASE construct
A `select case` construct conditionally executes one block of constructs or statements depending on the value of a scalar expression in a `select case` statement. This control construct can be considered as a replacement for computed `goto`.
    
    [name:] SELECT CASE (expr)
    [CASE (case-value [, case-value] ...) [name]
       block]...
    [CASE DEFAULT [name]
       block]
    END SELECT [name]

where, 
 - **name** - the name of the `select case` construct (optional)
 - **expr** - a scalar expression of type integer, logical, or character (enclosed in parentheses)
 - **case-value** -  one or more scalar integer, logical, or character initialization expressions enclosed in parentheses
 - **block** - a sequence of zero or more statements or constructs

Examples:

    ! simplest form of select case construct
    select case(i)
    case(:-1)
        s = -1
    case(0)
        s = 0
    case(1:)
        s = 1
    case default
        print "Something strange is happened"
    end select

In this example, `(:-1)` case value is a range of values matches to all values less than zero, `(0)` matches to zeroes, and `(1:)` matches to all values above zero, `default` section involves if other sections did not executed.

## Block DO construct
A `do` construct is a looping construct which has a number of iterations governed by a loop control

    integer i
    do i=1, 5
      print *, i
    end do
    print *, i

In the form above, the loop variable `i` passes through the loop 5 times, taking the values 1 to 5 in turn.  After the construct has completed the loop variable has the value 6, that is, **the loop variable is incremented once more after the completion of the loop**.

More generally, the `do` loop construct can be understood as follows 

    integer i, first, last, step
    do i=first, last, step
    end do

The loop starts with `i` with the value `first`, incrementing each iteration by `step` until `i` is greater than `last` (or less than `last` if the step size is negative).

It is important to note that since Fortran 95, the loop variable and the loop control expressions must be integer.

An iteration may be ended prematurely with the `cycle` statement

    do i=1, 5
      if (i==4) cycle
    end do

and the whole construct may cease execution with the `exit` statement

    do i=1, 5
      if (i==4) exit
    end do
    print *, i

---

`do` constructs may be named:

    do_name: do i=1, 5
    end do do_name

which is particularly useful when there are nested `do` constructs

    do1: do i=1, 5
      do j=1,6
        if (j==3) cycle        ! This cycles the j construct
        if (j==4) cycle        ! This cycles the j construct
        if (i+j==7) cycle do1  ! This cycles the i construct
        if (i*j==15) exit do1  ! This exits the i construct
      end do
    end do1

---

`do` constructs may also have indeterminate loop control, either "forever" or until a given condition is met

    integer :: i=0
    do
      i=i+1
      if (i==5) exit
    end do

or

    integer :: i=0
    do while (i<6)
      i=i+1
    end do

This also allows for an infinite `do` loop via a `.true.` statement 

    print *,'forever'
    do while(.true.)
      print *,'and ever'
    end do

---

A `do` construct may also leave the order of iterations indeterminate

    do concurrent (i=1:5)
    end do

noting that the form of loop control is the same as in a `forall` control.

There are various restrictions on the statements that may be executed within the range of a `do concurrent` construct which are designed to ensure that there are no data dependencies between iterations of the construct.  This explicit indication by the programmer may enable greater optimization (including parallelization) by the compiler which may be difficul to determine otherwise.

"Private" variables within an interation can be realized by use of a `block` construct within the `do concurrent`:

    do concurrent (i=1:5, j=2:7)
      block
        real tempval  ! This is independent across iterations
      end block
    end do

---

Another form of the block `do` construct uses a labelled `continue` statement instead of an `end do`:

        do 100, i=1, 5
    100 continue

It is even possible to nest such constructs with a shared termination statement

        do 100, i=1,5
        do 100, j=1,5
    100 continue

Both of these forms, and especially the second (which is obsolescent), are generally to be avoided in the interests of clarity.

---

Finally, there is also a non-block `do` construct.  This is also deemed to be obsolescent and is [described elsewhere](https://www.wikiod.com/fortran/modern-alternatives-to-historical-features#Non-block DO constructs), along with methods to restructure as a block `do` construct.

## WHERE construct
The `where` construct, available in Fortran90 onwards represents a masked `do` construct. The masking statement follows the same rules of the `if` statement, but is applied to all the elements of the given array. Using `where` allows operations to be carried out on an array (or multiple arrays of the same size), the elements of which satisfy a certain rule. This can be used to simplify simultaneous operations on several variables.

Syntax:

    [name]: where (mask)
        block
    [elsewhere (mask)
        block]
    [elsewhere
        block]
    end where [name]

Here,

 - **name** - is the name given to the block (if named)
 - **mask** - is a logical expression applied to all elements
 - **block** - series of commands to be executed
    
Examples:
    
    ! Example variables
    real:: A(5),B(5),C(5)
    A = 0.0
    B = 1.0
    C = [0.0, 4.0, 5.0, 10.0, 0.0]
 
    ! Simple where construct use
    where (C/=0)
        A=B/C
    elsewhere
        A=0.0
    end

    ! Named where construct
    Block: where (C/=0)
        A=B/C
    elsewhere
        A=0.0
    end where Block

