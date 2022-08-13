---
title: "Data Types"
slug: "data-types"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

## Precision of floating point numbers
Floating point numbers of type `real` cannot have any real value. They can represent real numbers up to certain amount of decimal digits. 

FORTRAN 77 guaranteed two floating point types and more recent standards guarantee at least two real types.  Real variables may be declared as

    real x
    double precision y

`x` here is a real of default kind and `y` is a real of kind with greater decimal precision than `x`.  In Fortran 2008, the decimal precision of `y` is at least 10 and its decimal exponent range at least 37. 

     real, parameter             :: single = 1.12345678901234567890
     double precision, parameter :: double = 1.12345678901234567890d0
     
     print *, single
     print *, double

prints

       1.12345684    
       1.1234567890123457

in common compilers using default configuration.

Notice the `d0` in the double precision constant. A real literal containing `d` instead of `e` for denoting the exponent is used to indicate double precision.

    ! Default single precision constant
    1.23e45
    ! Double precision constant
    1.23d45

Fortran 90 introduced parameterized `real` types using kinds. The kind of a real type is an integer named constant or literal constant:

    real(kind=real_kind) :: x

or just

    real(real_kind) :: x

This statement declares `x` to be of type `real` with a certain precision depending on the value of `real_kind`. 

Floating point literals can be declared with a specific kind using a suffix

    1.23456e78_real_kind

The exact value of `real_kind` is not standardized and differs from compiler to compiler. To inquire the kind of any real variable or constant, the function `kind()` can be used:

    print *, kind(1.0), kind(1.d0)

will typically print 

`4 8`

or

`1 2`

depending on the compiler. 

Kind numbers can be set in several ways:

1. Single (default) and double precision:

       integer, parameter :: single_kind = kind(1.)
       integer, parameter :: double_kind = kind(1.d0)

2. Using the intrinsic function `selected_real_kind([p, r])` to specify required decimal precision. The returned kind has precision of at least `p` digits and allows exponent of at least `r`.

       integer, parameter :: single_kind = selected_real_kind( p=6, r=37 )
       integer, parameter :: double_kind = selected_real_kind( p=15, r=200 )

3. Starting with Fortran 2003, pre-defined constants are available through the intrinsic module `ISO_C_Binding` to ensure that real kinds are inter-operable with the types `float`, `double` or `long_double` of the accompanying C compiler:

       use ISO_C_Binding

       integer, parameter :: single_kind = c_float
       integer, parameter :: double_kind = c_double
       integer, parameter :: long_kind = c_long_double

4. Starting with Fortran 2008, pre-defined constants are available through the intrinsic module `ISO_Fortran_env`. These constants provide real kinds with certain storage size in bits

       use ISO_Fortran_env

       integer, parameter :: single_kind = real32
       integer, parameter :: double_kind = real64
       integer, parameter :: quadruple_kind = real128

If certain kind is not available in the compiler, the value returned by `selected_real_kind()` or the value of the integer constant is `-1`. 


## Intrinsic types
The following are data types *intrinsic* to Fortran:

    integer
    real
    character
    complex
    logical

`integer`, `real` and `complex` are numeric types.

`character` is a type used to store character strings.

`logical` is used to store binary values `.true.` or `.false.`.

All numeric and logical intrinsic types are parametrized using kinds.

    integer(kind=specific_kind)

or just

    integer(specific_kind)

where `specific_kind` is an integer named constant.

Character variables, as well as having a kind parameter, also have a length parameter:

    character char

declares `char` to be a length-1 character variable of default kind, whereas

    character(len=len) name

declares `name` to be a character variable of default kind and length `len`.  The kind can also be specified

    character(len=len, kind=specific_kind) name
    character(kind=specific_kind) char

declares `name` to be a character of kind `kind` and length `len`.  `char` is a length-1 character of kind `kind`.

Alternatively, the obsolete form for character declaration

    character*len  name

may be seen in older code, declaring `name` to be of length `len` and default character kind.

---

Declaration of a variable of intrinsic type may be of the form above, but also may use the `type(...)` form:

    integer i
    real x
    double precision y

is equivalent to (but greatly preferred over)

    type(integer) i
    type(real) x
    type(double precision) y

## Derived data types
Define a new type, `mytype`:

    type :: mytype
      integer :: int
      real    :: float
    end type mytype

Declare a variable of type *mytype*:

    type(mytype) :: foo

The components of a derived type can be accessed with the `%` operator<sup>1</sup>:

    foo%int = 4
    foo%float = 3.142

---

A Fortran 2003 feature (not yet implemented by all compilers) allows to define parameterized data types:

    type, public :: matrix(rows, cols, k)
      integer, len :: rows, cols
      integer, kind :: k = kind(0.0)
      real(kind = k), dimension(rows, cols) :: values
    end type matrix

The derived type `matrix` has three type parameters which are listed in parentheses following the type name (they are `rows`, `cols`, and `k`). In the declaration of each type parameter it must be indicated whether they are kind (`kind`) or length (`len`) type parameters.

Kind type parameters, like those of the intrinsic types, must be constant expressions whereas length type parameters, like the length of an intrinsic character variable, may vary during execution.

Note that parameter `k` has a default value, so it may be provided or omitted when a variable of type `matrix` is declared, as follows

    type (matrix (55, 65, kind=double)) :: b, c ! default parameter provided
    type (matrix (rows=40, cols=50)     :: m    ! default parameter omitted

---
The name of a derived type may not be `doubleprecision` or the same as any of the intrinsic types.

---

1. Many people wonder why Fortran uses `%` as the component-access operator, instead of the more common `.`. This is because `.` is already taken by the operator syntax, i.e. `.not.`, `.and.`, `.my_own_operator.`.

## Literal constants
Program units often make use of literal constants.  These cover the obvious cases like

    print *, "Hello", 1, 1.0

Except in one case, each literal constant is a scalar which has type, type parameters and value given by the syntax.

Integer literal constants are of the form

    1
    -1
    -1_1   ! For valid kind parameter 1
    1_ik   ! For the named constant ik being a valid kind paramter

Real literal constants are of the form

    1.0    ! Default real
    1e0    ! Default real using exponent format
    1._1   ! Real with kind parameter 1 (if valid)
    1.0_sp ! Real with kind paramter named constant sp
    1d0    ! Double precision real using exponent format
    1e0_dp ! Real with kind named constant dp using exponent format

Complex literal constants are of the form

    (1, 1.)       ! Complex with integer and real components, literal constants
    (real, imag)  ! Complex with named constants as components

If the real and imaginary components are both integer, the complex literal constant is default complex, and the integer components are converted to default real.  If one component is real, the kind parameter of the complex literal constant is that of the real (and the integer component is converted to that real kind).  If both components are real the complex literal constant is of kind of the real with the greatest precision.

Logical literal constants are

    .TRUE.     ! Default kind, with true value
    .FALSE.    ! Default kind, with false value
    .TRUE._1   ! Of kind 1 (if valid), with true value
    .TRUE._lk  ! Of kind named constant lk (if valid), with true value

Character literal values differ slightly in concept, in that the kind specifier precedes the value

    "Hello"       ! Character value of default kind
    'Hello'       ! Character value of default kind
    ck_"Hello"    ! Character value of kind ck
    "'Bye"        ! Default kind character with a '
    '''Bye'       ! Default kind character with a '
    ""            ! A zero-length character of default kind

As suggested above, character literal constants must be delimted by apostrophes or quotation marks, and the start and end marker must match.  Literal apostrophes can be included by being within quotation mark delimiters or by appearing doubled.  The same for quotation marks.

BOZ constants are distinct from the above, in that they specify only a value: they have no type or type parameter.  A BOZ constant is a bit pattern and is specified as

    B'00000'    ! A binary bit pattern
    B"01010001" ! A binary bit pattern
    O'012517'   ! An octal bit pattern
    O"1267671"  ! An octal bit pattern
    Z'0A4F'     ! A hexadecimal bit pattern
    Z"FFFFFF"   ! A hexadecimal bit pattern

BOZ literal constants are limited in where they may appear: as constants in `data` statements and a selection of intrinsic procedures.

## Assumed and deferred length type parameters
Variables of character type or of a derived type with length parameter may have the length parameter either _assumed_ or _deferred_.  The character variable `name`

    character(len=len) name

is of length `len` throughout execution.  Conversely the length specifier may be either

    character(len=*) ...  ! Assumed length

or

    character(len=:) ...  ! Deferred length

---
Assumed length variables assume their length from another entity.

In the function

    function f(dummy_name)
      character(len=*) dummy_name
    end function f

the dummy argument `dummy_name` has length that of the actual argument.

The named constant `const_name` in

    character(len=*), parameter :: const_name = 'Name from which length is assumed'

has length given by the constant expression on the right-hand side.

---

Deferred length type parameters may vary during execution.  A variable with deferred length must have either the `allocatable` or `pointer` attribute

    character(len=:), allocatable :: alloc_name
    character(len=:), pointer :: ptr_name

Such a variable's length may be set in any of the following ways

    allocate(character(len=5) :: alloc_name, ptr_name)
    alloc_name = 'Name'         ! Using allocation on intrinsic assignment
    ptr_name => another_name    ! For given target

---

For derived types with length parameterization the syntax is similar

      type t(len)
        integer, len :: len
        integer i(len)
      end type t

      type(t(:)), allocatable :: t1
      type(t(5)) t2

      call sub(t2)
      allocate(type(t(5)) :: t1)

    contains

      subroutine sub(t2)
        type(t(*)), intent(out) :: t2
      end subroutine sub

    end  

## Accessing character substrings
For the character entity

    character(len=5), parameter :: greeting = "Hello"

a substring may be referenced with the syntax

    greeting(2:4)  ! "ell"

To access a single letter it isn't sufficient to write

    greeting(1)    ! This isn't the letter "H"

but

    greeting(1:1)  ! This is "H"

For a character array

    character(len=5), parameter :: greeting(2) = ["Hello", "Yo!  "]

we have substring access like

    greeting(1)(2:4)  ! "ell"

but we cannot reference the non-contiguous characters

    greeting(:)(2:4)  ! The parent string here is an array

We can even access substrings of literal constants

    "Hello"(2:4)

---

A portion of a character variable may also be defined by using a substring as a variable.  For example

    integer :: i=1
    character :: filename = 'file000.txt'

    filename(9:11) = 'dat'
    write(filename(5:7), '(I3.3)') i

## Accessing complex components
The complex entity

    complex, parameter :: x = (1., 4.)

has real part `1.` and complex part `4.`.  We can access these individual components as

    real(x)  ! The real component
    aimag(x) ! The complex component
    x%re     ! The real component
    y%im     ! The complex component

The `x%..` form is new to Fortran 2008 and not widely supported in compilers.  This form, however, may be used to directly set the individual components of a complex variable

    complex y
    y%re = 0.
    y%im = 1.

## Declaration and attributes
Throughout the topics and examples here we'll see many declarations of variables, functions and so on.

As well as their name, data objects may have _attributes_.  Covered  in this topic are declaration statements like

    integer, parameter :: single_kind = kind(1.)

which gives the object `single_kind` the `parameter` attribute (making it a named constant).

There are many other attributes, like

 - `target`
 - `pointer`
 - `optional`
 - `save`

Attributes may be specified with so-called _attribute specification statements_

    integer i    ! i is an integer (of default kind)...
    pointer i    ! ... with the POINTER attribute...
    optional i   ! ... and the OPTIONAL attribute

However, it is generally regarded to be better to avoid using these attribute specification statements.  For clarity the attributes may be specified as part of a single declaration

    integer, pointer, optional :: i

This also reduces the temptation to use implicit typing.

In most cases in this Fortran documentation this single declaration statement is preferred.

