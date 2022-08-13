---
title: "IO"
slug: "io"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - `WRITE(unit num, format num)` outputs the data after the brackets in a new line.
 - `READ(unit num, format num)` inputs to the variable after the brackets.
 - `OPEN(unit num, FILE=file)` opens a file. (There are more options for opening files, but they are not important for I/O.
 - `CLOSE(unit num)` closes a file.

## Simple I/O
As an example of writing input & output, we'll take in a real value and return the value and its square until the user enters a negative number.

As specified below, the `read` command takes two arguments: the unit number and the format specifier. In the example below, we use `*` for the unit number (which indicates stdin) and `*` for the format (which indicates the default for reals, in this case). We also specify the format for the `print` statement. One can alternatively use `write(*,"The value....")` or simply ignore formatting and have it as

    print *,"The entered value was ", x," and its square is ",x*x

which will likely result in some oddly spaced strings and values.

    program SimpleIO
       implicit none
       integer, parameter :: wp = selected_real_kind(15,307)
       real(kind=wp) :: x
       
       ! we'll loop over until user enters a negative number
       print '("Enter a number >= 0 to see its square. Enter a number < 0 to exit.")'
       do
          ! this reads the input as a double-pricision value
          read(*,*) x
          if (x < 0d0) exit
          ! print the entered value and it's square
          print '("The entered value was ",f12.6,", its square is ",f12.6,".")',x,x*x
       end do
       print '("Thank you!")'
       
    end program SimpleIO

## Read with some error checking

A modern Fortran example which includes error checking and a function to get a new unit number for the file.

    module functions

    contains

        function get_new_fileunit() result (f)
            implicit none

            logical    :: op
            integer    :: f

            f = 1
            do 
                inquire(f,opened=op)
                if (op .eqv. .false.) exit
                f = f + 1
            enddo

        end function

    end module

    program file_read
        use functions, only : get_new_fileunit
        implicit none

        integer            :: unitno, ierr, readerr
        logical            :: exists
        real(kind(0.d0))   :: somevalue
        character(len=128) :: filename

        filename = "somefile.txt"

        inquire(file=trim(filename), exist=exists)
        if (exists) then
            unitno = get_new_fileunit()
            open(unitno, file=trim(filename), action="read", iostat=ierr)
            if (ierr .eq. 0) then
                read(unitno, *, iostat=readerr) somevalue
                if (readerr .eq. 0) then
                    print*, "Value in file ", trim(filename), " is ", somevalue
                else
                    print*, "Error ", readerr, & 
                            " attempting to read file ", & 
                            trim(filename)
                endif
            else
                print*, "Error ", ierr ," attempting to open file ", trim(filename)
                stop
            endif
        else
            print*, "Error -- cannot find file: ", trim(filename)
            stop
        endif

    end program file_read



## Passing command line arguments
Where command line arguments are supported they can be read in via the `get_command_argument` intrinsic (introduced in the Fortran 2003 standard).  The `command_argument_count` intrinsic provides a way to know the number of arguments provided at the command line.

All command-line arguments are read in as strings, so an internal type conversion must be done for numeric data.  As an example, this simple code sums the two numbers provided at the command line:  

    PROGRAM cmdlnsum
    IMPLICIT NONE
    CHARACTER(100) :: num1char
    CHARACTER(100) :: num2char
    REAL :: num1
    REAL :: num2
    REAL :: numsum
    
    !First, make sure the right number of inputs have been provided
    IF(COMMAND_ARGUMENT_COUNT().NE.2)THEN
      WRITE(*,*)'ERROR, TWO COMMAND-LINE ARGUMENTS REQUIRED, STOPPING'
      STOP
    ENDIF
    
    CALL GET_COMMAND_ARGUMENT(1,num1char)   !first, read in the two values
    CALL GET_COMMAND_ARGUMENT(2,num2char)
    
    READ(num1char,*)num1                    !then, convert them to REALs
    READ(num2char,*)num2

    numsum=num1+num2                        !sum numbers
    WRITE(*,*)numsum                        !write out value

    END PROGRAM

The number argument in `get_command_argument` usefully ranges between `0` and the result of `command_argument_count`.  If the value is `0` then the command name is supplied (if supported).

Many compilers also offer non-standard intrinsics (such as `getarg`) to access command line arguments.  As these are non-standard, the corresponding compiler documentation should be consulted. 

---

Use of `get_command_argument` may be extended beyond the above example with the `length` and `status` arguments.  For example, with

    character(5) arg
    integer stat
    call get_command_argument(number=1, value=arg, status=stat)

the value of `stat` will be `-1` if the first argument exists and has length greater than 5.  If there is some other difficulty retrieving the argument the value of `stat` will be some positive number (and `arg` will consist entirely of blanks).  Otherwise its value will be `0`.

The `length` argument may be combined with a deferred length character variable, such as in the following example.

    character(:), allocatable :: arg
    integer arglen, stat
    call get_command_argument(number=1, length=arglen)  ! Assume for simplicity success
    allocate (character(arglen) :: arg)
    call get_command_argument(number=1, value=arg, status=stat)


