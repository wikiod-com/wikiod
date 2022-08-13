---
title: "Modern alternatives to historical features"
slug: "modern-alternatives-to-historical-features"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Arithmetic if statement
Arithmetic `if` statement allows one to use three branches depending on the result of an arithmetic expression

    if (arith_expr) label1, label2, label3

This `if` statement transfers control flow to one of the labels in a code. If the result of `arith_expr` is negative `label1` is involved, if the result is zero `label2` is used, and if the result is positive last `label3` is applied. Arithmetic `if` requires all three labels but it allows the re-use of labels, therefore this statement can be simplified to a two branch `if`.

Examples:

    if (N * N - N / 2) 130, 140, 130
    
    if (X) 100, 110, 120

Now this feature is obsolete with the same functionality being offered by the `if` statement and `if-else` construct.  For example, the fragment

        if (X) 100, 110, 120
    100 print*, "Negative"
        goto 200
    110 print*, "Zero"
        goto 200
    120 print*, "Positive"
    200 continue

may be written as the `if-else` construct

    if (X<0) then
      print*, "Negative"
    else if (X==0) then
      print*, "Zero"
    else
      print*, "Positive"
    end if

An `if` statement replacement for

        if (X) 100, 100, 200
    100 print *, "Negative or zero"
    200 continue

may be

    if (X<=0) print*, "Negative or zero"

   

## Implicit variable types


## Fixed Source Form
Fortran originally was designed for a [fixed format form](https://en.wikipedia.org/wiki/Fortran#Fixed_layout_and_punched_cards) based on an 80 column punched card:

[![enter image description here][1]][1]

<sup><sub>Yes: This is a line of the author's own code</sub></sup>

These were created on a card punch machine, much like this:

[![enter image description here][2]][2]

<sup><sub>Images are original photography by the author</sub></sup>

The format, as shown on the illustrated sample card, had the first five columns reserved for statement labels. The first column was used to denote comments by a letter **C**. The sixth column was used to denote a statement continuation (by inserting any character other than a zero '0'). The last 8 columns were used for card identification and sequencing, which was pretty valuable if you dropped your deck of cards on the floor! The character coding for punched cards had only a limited set of characters and was upper case only. As a result, Fortran programs looked like this:

           DIMENSION A(10)                                                    00000001
    C THIS IS A COMMENT STATEMENT TO EXPLAIN THIS EXAMPLE PROGRAM             00000002
           WRITE (6,100)                                                      00000003
     100   FORMAT(169HTHIS IS A RATHER LONG STRING BEING OUTPUT WHICH GOES OVE00000004
          1R MORE THAN ONE LINE, AND USES THE STATEMENT CONTINUATION MARKER IN00000005
          2COLUMN 6, AND ALSO USES HOLLERITH STRING FORMAT)                   00000006
           STOP                                                               00000007
           END                                                                00000008

The space character was also ignored everywhere, except inside a *Hollerith* character constant (as shown above). This meant that spaces could occur inside reserved words and constants, or completely missed out. This had the side effect of some rather misleading statements such as:

           DO 1 I = 1.0

is an assignment to the variable `DO1I` whereas:

           DO1I = 1,0 

is actually a `DO` loop on the variable `I`.

-----

Modern Fortran does not now required this fixed form of input and permits free form using any columns. Comments are now indicated by a `!` which can also be appended to a statement line. Spaces are now not permitted anywhere and must be used as separators, much as in most other languages. The above program could be written in modern Fortran as:

    ! This is a comment statement to explain this example program
    Print *,"THIS IS A RATHER LONG STRING BEING OUTPUT WHICH no longer GOES OVER MORE THAN ONE LINE, AND does not need to USE THE STATEMENT CONTINUATION MARKER IN COLUMN 6, or the HOLLERITH STRING FORMAT"

Although the old-style continuation is no longer used, the above example illustrates that very long statements will still occur. Modern Fortran uses a `&` symbol at the end and beginning of the continuation. For example, we could write the above in a more readable form:

    ! This is a comment statement to explain this example program
    Print *,"THIS IS A RATHER LONG STRING BEING OUTPUT WHICH still &
             &GOES OVER MORE THAN ONE LINE, AND does need to USE THE STATEMENT &
             &CONTINUATION notation"


  [1]: http://i.stack.imgur.com/oJiKo.jpg
  [2]: http://i.stack.imgur.com/jMhV0m.jpg

## Common Blocks
In the early forms of Fortran the only mechanism for creating global variable store visible from subroutines and functions is to use the `COMMON` block mechanism. This permitted sequences of variables to be names and shared in common.

In addition to named common blocks there may also be a blank (unnamed) common block.

A blank common block could be declared like

    common i, j

whereas the named block `variables` could be declared like

    common /variables/ i, j

As a complete example, we could imagine a heap store that is used by routines that can add and remove values:

           PROGRAM STACKING
           COMMON /HEAP/ ICOUNT, ISTACK(1023)
           ICOUNT = 0
           READ *, IVAL
           CALL PUSH(IVAL)
           CALL POP(IVAL)
           END

           SUBROUTINE PUSH(IVAL)
           COMMON /HEAP/ ICOUNT, ISTACK(1023)
           ICOUNT = ICOUNT + 1
           ISTACK(ICOUNT) = IVAL
           RETURN
           END

           SUBROUTINE POP(IVAL)
           COMMON /HEAP/ ICOUNT, ISTACK(1023)
           IVAL = ISTACK(ICOUNT)
           ICOUNT = ICOUNT - 1
           RETURN
           END

---

Common statements may be used to implicitly declare the type of a variable and to specify the `dimension` attribute.  This behaviour alone is often a sufficient source of confusion.  Further, the implied storage association and requirements for repeated definitions across program units makes the use of common blocks prone to error.

Finally, common blocks are very restricted in the objects they contain.  For example, an array in a common block must be of explicit size; allocatable objects may not occur; derived types must not have default initialization.

In modern Fortran this sharing of variables can be handled by the use of [modules](https://www.wikiod.com/fortran/usage-of-modules). The above example can be written as:

    module heap
      implicit none
      ! In Fortran 2008 all module variables are implicitly saved
      integer, save :: count = 0
      integer, save :: stack(1023)
    end module heap

    program stacking
      implicit none
      integer val
      read *, val
      call push(val)
      call pop(val)

    contains
      subroutine push(val)
        use heap, only : count, stack
        integer val
        count = count + 1
        stack(count) = val
      end subroutine push

      subroutine pop(val)
        use heap, only : count, stack
        integer val
        val = stack(count)
        count = count - 1
      end subroutine pop
    end program stacking

---

Named and blank common blocks have slightly different behaviours.  Of note:

 - objects in named common blocks may be defined initially; objects in blank common shall not be
 - objects in blank common blocks behave as though the common block has the `save` attribute; objects in named common blocks without the `save` attribute may become undefined when the block is not in the scope of an active program unit

This latter point can be contrasted with the behaviour of module variables in modern code.  All module variables in Fortran 2008 are implicitly saved and do not become undefined when the module goes out of scope.  Before Fortran 2008 module variables, like variables in named common blocks, would also become undefined when the module went out of scope.

## Non-block DO constructs
The non-block `do` construct looks like

        integer i
        do 100, i=1, 5
    100 print *, i

That is, where the labelled termination statement is not a `continue` statement.  There are various restrictions on the statement that can be used as the termination statement and the whole thing is generally very confusing.

Such a non-block construct can be rewritten in block form as

        integer i
        do 100 i=1,5
          print *, i
    100 continue

or better, using an `end do` termination statement,

    integer i
    do i=1,5
      print *, i
    end do

## Alternate return
Alternate return is a facility to control the flow of execution on return from a subroutine.  It is often used as a form of error handling:

    real x

    call sub(x, 1, *100, *200)
    print*, "Success:", x
    stop
    
    100 print*, "Negative input value"
    stop
    
    200 print*, "Input value too large"
    stop
    
    end

    subroutine sub(x, i, *, *)
      real, intent(out) :: x
      integer, intent(in) :: i
      if (i<0) return 1
      if (i>10) return 2
      x = i
    end subroutine

The alternate return is marked by the arguments `*` in the subroutine dummy argument list.  

In the `call` statement above `*100` and `*200` refer to the statements labelled `100` and `200` respectively.

In the subroutine itself the `return` statements corresponding to alternate return have a number.  This number is not a return value, but denotes the provided label to which execution is passed on return.  In this case, `return 1` passes execution to the statement labelled `100` and `return 2` passes execution to the statement labelled `200`.  An unadorned `return` statement, or completion of subroutine execution without a `return` statement, passess execution to immediately after the call statement.

The alternate return syntax is very different from other forms of argument association and the facility introduces flow control contrary to modern tastes.  More pleasing flow control can be managed with return of an integer "status" code.

    real x
    integer status

    call sub(x, 1, status)
    select case (status)
    case (0)
      print*, "Success:", x
    case (1)
      print*, "Negative input value"
    case (2)
      print*, "Input value too large"
    end select

    end

    subroutine sub(x, i, status)
      real, intent(out) :: x
      integer, intent(in) :: i
      integer, intent(out) :: status

      status = 0

      if (i<0) then
        status = 1
      else if (i>10)
        status = 2
      else
        x = i
      end if

    end subroutine

## Assigned GOTO
Assigned GOTO uses integer variable to which a statement label is assigned using the ASSIGN statement.

    100 CONTINUE
    
    ...
    
    ASSIGN 100 TO ILABEL
    
    ...
    
    
    GOTO ILABEL


Assigned GOTO is obsolescent in Fortran 90 and deleted in Fortran 95 and later. It can be avoided in modern code by using procedures, internal procedures, procedure pointers and other features.

## Computed GOTO
Computed GOTO allows branching of the program according to the value of an integer expression.

    GOTO (label_1, label_2,... label_n) scalar-integer-expression

If `scalar-integer-expression` is equal to 1 the program continues at statement label `label_1`, if it is equal to 2 it goes to `label_2` and so on. If it is less then `1` or larger than `n` program continues on next line.

Example:

    ivar = 2

    ...

    GOTO (10, 20, 30, 40) ivar

will jump to statement label 20.

This form of `goto` is obsolescent in Fortran 95 and later, being superseded by the `select case` construct.

## Assigned format specifiers
Before Fortran 95 it was possible to use assigned formats for input or output.  Consider

    integer i, fmt
    read *, i
    
    assign 100 to fmt
    if (i<100000) assign 200 to fmt
    
    print fmt, i

    100 format ("This is a big number", I10)
    200 format ("This is a small number", I6)

    end

The `assign` statement assigns a statement label to an integer variable.  This integer variable is later used as the format specifier in the `print` statement.

Such format specifier assignment was deleted in Fortran 95.  Instead, more modern code can use some other form of execution flow control

    integer i
    read *, i

    if (i<100000) then
      print 100, i
    else
      print 200, i
    end if

    100 format ("This is a big number", I10)
    200 format ("This is a small number", I6)

    end

or a character variable may be used as the format specifier

    character(29), target :: big_fmt='("This is a big number", I10)'
    character(30), target :: small_fmt='("This is a small number", I6)'
    character(:), pointer :: fmt
    
    integer i
    read *, i
    
    fmt=>big_fmt
    if (i<100000) fmt=>small_fmt
    
    print fmt, i
    
    end

## Statement functions
Consider the program

    implicit none
    integer f, i
    f(i)=i

    print *, f(1)
    end

Here `f` is a statement function.  It has integer result type, taking one integer dummy argument.<sup>1</sup>

Such a statement function exists within the scope in which it is defined.  In particular, it has access to variables and named constants accessible in that scope.

However, statement functions are subject to many restrictions and are potentially confusing (looking at casual glance like an array element assignment statement).  Important restrictions are:

 - the function result and dummy arguments must be scalar
 - the dummy arguments are in the same scope as the function
 - statement functions have no local variables
 - statement functions cannot be passed as actual arguments

The main benefits of statement functions are repeated by internal functions

    implicit none

    print *, f(1)

    contains

      integer function f(i)
        integer i
        f = i
      end function

    end

Internal functions are not subject to the restrictions mentioned above, although it is perhaps worth noting that an internal subprogram may not contain further internal subprogram (but it may contain a statement function).

Internal functions have their own scope but also have available host association.

---
<sup>1</sup> In real old code examples, it wouldn't be unusual to see the dummy arguments of a statement function being implicitly typed, even if the result has explicit type.

