---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9881
type: docs
toc: true
---

## Array constructors
A rank-1 array value can be created using an _array constructor_, with the syntax

    (/ ... /)
    [ ... ]

The form `[...]` was introduced in Fortran 2003 and is generally regarded as clearer to read, especially in complex expressions.  This form is used exclusively in this example.

The values featuring in an array constructor may be scalar values, array values, or implied-do loops.

The type and type parameters of the constructed array match those of the values in the array constructor

    [1, 2, 3]      ! A rank-1 length-3 array of default integer type
    [1., 2., 3.]   ! A rank-1 length-3 array of default real type
    ["A", "B"]     ! A rank-1 length-2 array of default character type

    integer, parameter :: A = [2, 4]
    [1, A, 3]      ! A rank-1 length-4 array of default integer type, with A's elements

    integer i
    [1, (i, i=2, 5), 6]  ! A rank-1 length-6 array of default integer type with an implied-do

In the forms above, all the values given must be of the same type and type parameter.  Mixing types, or type parameters, is not allowed.  The following examples **are not valid**

    [1, 2.]      ! INVALID: Mixing integer and default real
    [1e0, 2d0]   ! INVALID: Mixing default real and double precision
    [1., 2._dp]  ! INVALID: Allowed only if kind `dp` corresponds to default real
    ["Hello", "Frederick"]  ! INVALID: Different length parameters

To construct an array using different types, a type specification for the array shall be given

    [integer :: 1, 2., 3d0]    ! A default integer array
    [real(dp) :: 1, 2, 3._sp]  ! A real(dp) array
    [character(len=9) :: "Hello", "Frederick"]  ! A length-2 array of length-9 characters

This latter form for character arrays is especially convenient to avoid space padding, such as the alternative

    ["Hello    ", "Frederick"]  ! A length-2 array of length-9 characters

The size of an array named constant may be implied by the array constructor used to set its value

    integer, parameter :: ids(*) = [1, 2, 3, 4]

and for length-parameterized types the length parameter may be assumed

    character(len=*), parameter :: names(*) = [character(3) :: "Me", "You", "Her"]

The type specification is also required in the construction of zero-length arrays.  From

    [ ] ! Not a valid array constructor

the type and type parameters cannot be determined from the non-existing value set.  To create a zero-length default integer array:

    [integer :: ]

---

Array constructors construct only rank-1 arrays.  At times, such as in setting the value of a named constant, higher rank arrays are also required in an expression.  Higher rank arrays can be taken from the result of `reshape` with a constructed rank-1 array

    integer, parameter :: multi_rank_ids(2,2) = RESHAPE([1,2,3,4], shape=[2,2])
---

In an array constructor the values of the array in element order with any arrays in the value list being as though the individual elemets were given themselves in array element order.  Thus, the earlier example

    integer, parameter :: A = [2, 4]
    [1, A, 3]      ! A rank-1 length-4 array of default integer type, with A's elements

is equivalent to

    [1, 2, 4, 3]   ! With the array written out in array element order

Generally the values in the constructor may be arbitrary expressions, including nested array constructors.  For such an array constructor to meet certain conditions, such as being a constant or specification expression, restrictions apply to constituent values.

---

Although not an array constructor, certain array values may also be conveniently created using the `spread` intrinsic function.  For example

    [(0, i=1,10)]  ! An array with 10 default integers each of value 0

is also the result of the function reference

    SPREAD(0, 1, 10)

## Basic notation
Any type can be declared as an array using either the *dimension* attribute or by just indicating directly the `dimension`(s) of the array:

    ! One dimensional array with 4 elements
    integer, dimension(4) :: foo

    ! Two dimensional array with 4 rows and 2 columns
    real, dimension(4, 2) :: bar

    ! Three dimensional array
    type(mytype), dimension(6, 7, 8) :: myarray

    ! Same as above without using the dimension keyword
    integer :: foo2(4)
    real :: bar2(4, 2)
    type(mytype) :: myarray2(6, 7, 8)

The latter way of declaring multidimensional array, allows the declaration of same-type different-rank/dimensions arrays in one line, as follows

    real :: pencil(5), plate(3,-2:4), cuboid(0:3,-10:5,6)

The maximum rank (number of dimensions) allowed is 15 in Fortran 2008 standard and was 7 before.

Fortran stores arrays in *column-major* order. That is, the elements of `bar` are stored in memory as follows:

    bar(1, 1), bar(2, 1), bar(3, 1), bar(4, 1), bar(1, 2), bar(2, 2), ...

In Fortran, array numbering starts at **1** by default, in contrast to C which starts  at **0**. In fact, in Fortran, you can specify the upper and lower bounds for each dimension explicitly:

    integer, dimension(7:12, -3:-1) :: geese

This declares an array of shape `(6, 3)`, whose first element is `geese(7, -3)`.

Lower and upper bounds along the 2 (or more) dimensions can be accessed by the intrinsic functions `ubound` and `lbound`. Indeed `lbound(geese,2)` would return `-3`, whereas `ubound(geese,1)` would return `12`.

Size of an array can be accessed by intrinsic function `size`. For example, `size(geese, dim = 1)` returns the size of first dimension which is 6.

## Whole arrays, array elements and array sections
Consider the array declared as

    real x(10)

Then we have three aspects of interest:

1. The whole array `x`;
2. Array elements, like `x(1)`;
3. Array sections, like `x(2:6)`.

Whole arrays
-

In most cases the whole array `x` refers to all of the elements of the array as a single entity.  It may appear in executable statements such as `print *, SUM(x)`, `print *, SIZE(x)` or `x=1`.

A whole array may reference arrays which aren't explicitly shaped (such as `x` above):

    function f(y)
      real, intent(out) :: y(:)
      real, allocatable :: z(:)

      y = 1.         ! Intrinsic assignment for the whole array
      z = [1., 2.,]  ! Intrinsic assignment for the whole array, invoking allocation
    end function

An assumed-size array may also appear as a whole array, but in limited circumstances only (to be documented elsewhere).

Array elements
-

An array element is referred to be giving integer indexes, one for each rank of the array, denoting the location in the whole array:

    real x(5,2)
    x(1,1) = 0.2
    x(2,4) = 0.3

An array element is a scalar.

Array sections
-

An array section is a reference to a number of elements (perhaps just one) of a whole array, using a syntax involving colons:

    real x(5,2)
    x(:,1) = 0.         ! Referring to x(1,1), x(2,1), x(3,1), x(4,1) and x(5,1)
    x(2,:) = 0.         ! Referring to x(2,1), x(2,2)
    x(2:4,1) = 0.       ! Referring to x(2,1), x(3,1) and x(4,1)
    x(2:3,1:2) = 0.     ! Referring to x(2,1), x(3,1), x(2,2) and x(3,2)
    x(1:1,1) = 0.       ! Referring to x(1,1)
    x([1,3,5],2) = 0.   ! Referring to x(1,2), x(3,2) and x(5,2)

The final form above uses a [_vector subscript_](https://www.wikiod.com/fortran/arrays#Whole arrays, array elements and array sections).  This is subject to a number of restrictions beyond other array sections.

Each array section is itself an array, even when just one element is referenced.  That is `x(1:1,1)` is an array of rank 1 and `x(1:1,1:1)` is an array of rank 2.

Array sections do not in general have an attribute of the whole array.  In particular, where

    real, allocatable :: x(:)
    x = [1,2,3]     ! x is allocated as part of the assignment
    x = [1,2,3,4]   ! x is dealloacted then allocated to a new shape in the assignment

the assignment

    x(:) = [1,2,3,4,5]   ! This is bad when x isn't the same shape as the right-hand side

is not allowed: `x(:)`, although an array section with all elements of `x`, is not an allocatable array.

    x(:) = [5,6,7,8]

is fine when `x` is of the shape of the right-hand side.

---

Array components of arrays
-

    type t
       real y(5)
    end type t

    type(t) x(2)

We may also refer to whole arrays, array elements and array sections in more complicated settings.

From the above, `x` is a whole array.  We also have

    x(1)%y        ! A whole array
    x(1)%y(1)     ! An array element
    x%y(1)        ! An array section
    x(1)%y(:)     ! An array section
    x([1,2]%y(1)  ! An array section
    x(1)%y(1:1)   ! An array section

In such cases we are not allowed to have more than one part of the reference consisting of an array of rank 1.  The following, for example, are not allowed

    x%y             ! Both the x and y parts are arrays
    x(1:1)%y(1:1)   ! Recall that each part is still an array section
    



## Allocatable arrays
Arrays can have the *allocatable* attribute:

    ! One dimensional allocatable array
    integer, dimension(:), allocatable :: foo
    ! Two dimensional allocatable array
    real, dimension(:,:), allocatable :: bar

This declares the variable but does not allocate any space for it.

    ! We can specify the bounds as usual
    allocate(foo(3:5))

    ! It is an error to allocate an array twice
    ! so check it has not been allocated first
    if (.not. allocated(foo)) then
      allocate(bar(10, 2))
    end if

Once a variable is no longer needed, it can be *deallocated*:

    deallocate(foo)

If for some reason an `allocate` statement fails, the program will stop. This can be prevented if the status is checked via the `stat` keyword:

    real, dimension(:), allocatable :: geese
    integer :: status

    allocate(geese(17), stat=status)
    if (stat /= 0) then
      print*, "Something went wrong trying to allocate 'geese'"
      stop 1
    end if

The `deallocate` statement has `stat` keyword too:

    deallocate (geese, stat=status)

`status` is an integer variable whose value is 0 if the allocation or deallocation was successful. 

## Array nature specification: rank and shape
The `dimension` attribute on an object specifies that that object is an array.  There are, in Fortran 2008, five array natures:<sup>1</sup>

 - explicit shape
 - assumed shape
 - assumed size
 - deferred shape
 - implied shape

Take the three rank-1 arrays<sup>2</sup>

    integer a, b, c
    dimension(5) a    ! Explicit shape (default lower bound 1), extent 5
    dimension(:) b    ! Assumed or deferred shape
    dimension(*) c    ! Assumed size or implied shape array

With these it can be seen that further context is required to determine fully the nature of an array.

Explicit shape
--

An explicit shape array is always the shape of its declaration.  Unless the array is declared as local to a subprogram or `block` construct, the bounds defining shape must be constant expressions.  In other cases, an explicit shape array may be an automatic object, using extents which may vary on each invocation of a subprogram or `block`.

    subroutine sub(n)
      integer, intent(in) :: n
      integer a(5)   ! A local explicit shape array with constant bound
      integer b(n)   ! A local explicit shape array, automatic object
    end subroutine

Assumed shape
--

An assumed shape array is a dummy argument without the `allocatable` or `pointer` attribute.  Such an array takes its shape from the actual argument with which it is associated.

    integer a(5), b(10)
    call sub(a)   ! In this call the dummy argument is like x(5)
    call sub(b)   ! In this call the dummy argument is like x(10)

    contains

      subroutine sub(x)
        integer x(:)    ! Assumed shape dummy argument
      end subroutine sub

    end

When a dummy argument has assumed shape the scope referencing the procedure must have an explicit interface available for that procedure.

Assumed size
--

An assumed size array is a dummy argument which has its size assumed from its actual argument.

    subroutine sub(x)
      integer x(*)   ! Assumed size array
    end subroutine

Assumed size arrays behave very differently from assumed shape arrays and these differences are documented elsewhere.

Deferred shape
--

A deferred shape array is an array which has the `allocatable` or `pointer` attribute.  The shape of such an array is determined by its [allocation](https://www.wikiod.com/fortran/arrays#Allocatable arrays) or pointer assignment.

    integer, allocatable :: a(:)
    integer, pointer :: b(:)

Implied shape
--

An implied shape array is a named constant which takes its shape from the expression used to establish its value

    integer, parameter :: a(*) = [1,2,3,4]

---

The implications of these array declarations on dummy arguments are to be documented elsewhere.

---

<sup>1</sup>A Technical Specification extending Fortran 2008 adds a sixth array nature: assumed rank.  This is not covered here.

<sup>2</sup> These can equivalently be written as

    integer, dimension(5) :: a
    integer, dimension(:) :: b
    integer, dimension(*) :: c

or

    integer a(5)
    integer b(:)
    integer c(*)

## Array operations
Due to its computational goals, mathematical operations on arrays are straight forward in Fortran.

## Addition and subtraction ##
Operations on arrays of the same shape and size are very similar to matrix algebra.
Instead of running through all the indices with loops, one can write addition (and subtraction):

    real, dimension(2,3) :: A, B, C
    real, dimension(5,6,3) :: D
    A    = 3.    ! Assigning single value to the whole array
    B    = 5.    ! Equivalent writing for assignment
    C    = A + B ! All elements of C now have value 8.
    D    = A + B ! Compiler will raise an error. The shapes and dimensions are not the same

Arrays from slicing are also valid:
    
    integer :: i, j
    real, dimension(3,2) :: Mat = 0.
    real, dimension(3)   :: Vec1 = 0., Vec2 = 0., Vec3 = 0.
    i = 0
    j = 0
    do i = 1,3
      do j = 1,2
        Mat(i,j) = i+j
      enddo
    enddo
    Vec1 = Mat(:,1)
    Vec2 = Mat(:,2)
    Vec3 = Mat(1:2,1) + Mat(2:3,2)

## Function ##
In the same way, most intrinsic functions can be used implicitly assuming component-wise operation (though this is not systematic):

    real, dimension(2) :: A, B
    A(1) = 6
    A(2) = 44 ! Random values
    B    = sin(A) ! Identical to B(1) = sin(6), B(2) = sin(44).

## Multiplication and division ##
Care must be taken with product and division: intrinsic operations using `*` and `/` symbols are element-wise:

    real, dimension(2) :: A, B, C
    A(1) = 2
    A(2) = 4
    B(1) = 1
    B(2) = 3
    C = A*B ! Returns C(1) = 2*1 and C(2) = 4*3

This must not be mistaken with matrix operations (see below).

## Matrix operations ##
Matrix operations are intrinsic procedures. For example, the matrix product of the arrays of the previous section is written as follows:


    real, dimension(2,1) :: A, B
    real, dimension(1,1) :: C
    A(1) = 2
    A(2) = 4
    B(1) = 1
    B(2) = 3
    C = matmul(transpose(A),B) ! Returns the scalar product of vectors A and B

Complex operations allow encapsulated of functions by creating temporary arrays. While allowed by some compilers and compilation options, this is not recommanded. For example, a product including a matrix transpose can be written:

    real, dimension(3,3) :: A, B, C
    A(:) = 4
    B(:) = 5
    C = matmul(transpose(A),matmul(B,matmul(A,transpose(B)))) ! Equivalent to A^t.B.A.B^T

## Advanced array sections: subscript triplets and vector subscripts
As mentioned in [another example](https://www.wikiod.com/fortran/arrays#Whole arrays, array elements and array sections) a subset of the elements of an array, called an array section, may be referenced.  From that example we may have

    real x(10)
    x(:)   = 0.
    x(2:6) = 1.
    x(3:4) = [3., 5.]

Array sections may be more general than this, though.  They may take the form of subscript triplets or vector subscripts.

Subscript triplets
---

A subscript triple takes the form `[bound1]:[bound2][:stride]`.  For example

    real x(10)
    x(1:10) = ...   ! Elements x(1), x(2), ..., x(10)
    x(1:) = ...     ! The omitted second bound is equivalent to the upper, same as above
    x(:10) = ...    ! The omitted first bound is equivalent to the lower, same as above
    x(1:6:2) = ...  ! Elements x(1), x(3), x(5)
    x(5:1) = ...    ! No elements: the lower bound is greater than the upper
    x(5:1:-1) = ... ! Elements x(5), x(4), x(3), x(2), x(1)
    x(::3) = ...    ! Elements x(1), x(4), x(7), x(10), assuming omitted bounds
    x(::-3) = ...   ! No elements: the bounds are assumed with the first the lower, negative stride

When a stride (which must not be zero) is specified, the sequence of elements begins with the first bound specified.  If the stride is positive (resp. negative) the selected elements following a sequence incremented (resp. decremented) by the stride until the last element not larger (resp. smaller) than the second bound is taken.  If the stride is omitted it is treated as being one.

If the first bound is larger than the second bound, and the stride is positive, no elements are specified.  If the first bound is smaller than the second bound, and the stride is negative, no elements are specified.

It should be noted that `x(10:1:-1)` is not the same as `x(1:10:1)` even though each element of `x` appears in both cases.

Vector subscripts
---

A vector subscript is a rank-1 integer array.  This designates a sequence of elements corresponding to the values of the array.

    real x(10)
    integer i
    x([1,6,4]) = ...     ! Elements x(1), x(6), x(4)
    x([(i,i=2,4)]) = ... ! Elements x(2), x(3) and x(4)
    print*, x([2,5,2])   ! Elements x(2), x(5) and x(2)

An array section with a vector subscript is restricted in how it may be used:

- it may not be argument associated with a dummy argument which is defined in the procedure;
- it may not be the target in a pointer assignment statement;
- it may not be an internal file in a data transfer statement.

Further, such an array section may not appear in a statement which involves its definition when the same element is selected twice.  From above:

    print*, x([2,5,2])   ! Elements x(2), x(5) and x(2) are printed
    x([2,5,2]) = 1.      ! Not permitted: x(2) appears twice in this definition


Higher rank array sections
---

    real x(5,2)
    print*, x(::2,2:1:-1)  ! Elements x(1,2), x(3,2), x(5,2), x(1,1), x(3,1), x(5,1)


