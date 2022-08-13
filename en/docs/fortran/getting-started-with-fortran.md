---
title: "Getting started with Fortran"
slug: "getting-started-with-fortran"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello, world
Any Fortran program has to include `end` as last statement. Therefore, the simplest Fortran program looks like this:

    end

Here are some examples of "hello, world" programs:

    print *, "Hello, world" 
    end

With `write` statement:

    write(*,*) "Hello, world"
    end

For clarity it is now common to use the `program` statement to start a program and give it a name. The `end` statement can then refer to this name to make it obvious what it is referring to, and let the compiler check the code for correctness. Further, all Fortran programs should include an `implicit none` statement. Thus, a minimal Fortran program actually should look as follows:

    program hello
      implicit none
      write(*,*) 'Hello world!'
    end program hello

The next logical step from this point is how to see the result of the hello world program. This section shows how to achieve that in a linux like environment. We assume that you have some basic notions of [shell commands][1], mainly you know how to get to the shell terminal. We also assume that you have already [setup your `fortran` environment][2]. Using your preferred text editor (notepad, notepad++, vi, vim, emacs, gedit, kate, etc.), save the hello program above (copy and paste) in a file named `hello.f90` in your home directory. `hello.f90` is your source file. Then go to the command line and navigate to the directory(home directory?) where you saved your source file, then type the following command:

    >gfortran -o hello hello.f90

You just created your hello world executable program. In technical terms, you just compiled your program. To run it, type the following command:
  
    >./hello

You should see the following line printed on your shell terminal.

    > Hello world!

Congratulations, you just wrote, compiled and ran the "Hello World" program.

  [1]: https://www.wikiod.com/shell/getting-started-with-shell
  [2]: https://www.wikiod.com/fortran/getting-started-with-fortran#Installation or Setup

## Quadratic equation
Today Fortran is mainly used for numerical computation. This very simple example illustrates the basic program structure to solve quadratic equations:



    program quadratic
      !a comment
    
      !should be present in every separate program unit
      implicit none
    
      real :: a, b, c
      real :: discriminant
      real :: x1, x2
    
      print *, "Enter the quadratic equation coefficients a, b and c:"
      read *, a, b, c
    
      discriminant = b**2 - 4*a*c
    
      if ( discriminant>0 ) then
    
        x1 = ( -b + sqrt(discriminant)) / (2 * a)
        x2 = ( -b - sqrt(discriminant)) / (2 * a)
        print *, "Real roots:"
        print *, x1, x2
    
        ! Comparison of floating point numbers for equality is often not recommended. 
        ! Here, it serves the purpose of illustrating the "else if" construct. 
      else if ( discriminant==0 ) then
    
        x1 = - b / (2 * a)
        print *, "Real root:"
        print *, x1
      else
    
        print *, "No real roots."
      end if
    end program quadratic

## Case insensitivity
Uppercase and lowercase letters of the alphabet are equivalent in the
Fortran character set. In other words, Fortran is _case insensitive_. This behavior is in contrast with case-sensitive languages, such as C++ and many others.

As a consequence, the variables `a` and `A` are the same variable. In principle one could write a program as follows

    pROgrAm MYproGRaM
    ..
    enD mYPrOgrAM

It's to the good programmer to avoid such ugly choices.

## Installation or Setup
Fortran is a language which can be compiled using compilers supplied by many vendors. Different compilers are available for different hardware platforms and operating systems. Some compilers are free software, some can be used free of charge and some require the purchase of a licence.

The most common free Fortran compiler is GNU Fortran or gfortran. The source code is available from GNU as a part of GCC, the GNU compiler collection. Binaries for many operating systems are available at https://gcc.gnu.org/wiki/GFortranBinaries. Linux distributions often contain gfortran in their package manager.

Further compilers are available for example:

* [EKOPath][1] by PathScale
* [LLVM (backend via DragonEgg)][2]
* [Oracle Developer Studio][3]


* [Absoft Fortran Compiler][4]
* [Intel Fortran Compiler][5]
* [NAG Fortran Compiler][6]
* [PGI Compilers][7]

On HPC-Systems there are often specialized compilers available by the system provider as for example the [IBM][8] or [Cray][9] compilers.

All these compilers support the Fortran 95 standard. An overview on the [Fortran 2003 status][10] and the [Fortran 2008 status][11] by various compilers is offered by the ACM Fortran Forum and available in the Fortran Wiki.


  [1]: http://www.pathscale.com/ekopath-compiler-suite
  [2]: http://dragonegg.llvm.org
  [3]: http://www.oracle.com/technetwork/server-storage/developerstudio/overview/index.html
  [4]: http://www.absoft.com
  [5]: https://software.intel.com/en-us/fortran-compilers
  [6]: http://www.nag.co.uk/nag-compiler
  [7]: http://www.pgroup.com
  [8]: http://www-03.ibm.com/software/products/en/fortcompfami
  [9]: http://docs.cray.com/books/S-3901-50/html-S-3901-50/f130.html
  [10]: http://fortranwiki.org/fortran/show/Fortran+2003+status
  [11]: http://fortranwiki.org/fortran/show/Fortran+2008+status

