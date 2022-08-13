---
title: "REPL"
slug: "repl"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- julia>
- help?>
- shell>
- \\[latex]

Other packages may define their own REPL modes in addition to the default modes. For instance, the `Cxx` package defines the `cxx>` shell mode for a C++ REPL. These modes are usually accessible with their own special keys; see package documentation for more details.

## Using the REPL as a Calculator
The Julia REPL is an excellent calculator. We can start with some simple operations:

    julia> 1 + 1
    2

    julia> 8 * 8
    64

    julia> 9 ^ 2
    81

The `ans` variable contains the result of the last calculation:

    julia> 4 + 9
    13

    julia> ans + 9
    22

We can define our own variables using the assignment `=` operator:

    julia> x = 10
    10

    julia> y = 20
    20

    julia> x + y
    30

Julia has implicit multiplication for numeric literals, which makes some calculations quicker to write:

    julia> 10x
    100

    julia> 2(x + y)
    60

If we make a mistake and do something that is not allowed, the Julia REPL will throw an error, often with a helpful tip on how to fix the problem:

    julia> 1 ^ -1
    ERROR: DomainError:
    Cannot raise an integer x to a negative power -n. 
    Make x a float by adding a zero decimal (e.g. 2.0^-n instead of 2^-n), or write
    1/x^n, float(x)^-n, or (x//1)^-n.
     in power_by_squaring at ./intfuncs.jl:82
     in ^ at ./intfuncs.jl:106

    julia> 1.0 ^ -1
    1.0

To access or edit previous commands, use the <kbd>↑</kbd> (Up) key, which moves to the last item in history. The <kbd>↓</kbd> moves to the next item in history. The <kbd>←</kbd> and <kbd>→</kbd> keys can be used to move and make edits to a line.

Julia has some built-in mathematical constants, including `e` and `pi` (or `π`).

    julia> e
    e = 2.7182818284590...

    julia> pi
    π = 3.1415926535897...

    julia> 3π
    9.42477796076938

We can type characters like `π` quickly by using their LaTeX codes: press <kbd>\\</kbd>, then <kbd>p</kbd> and <kbd>i</kbd>, then hit the <kbd>Tab</kbd> key to substitute the `\pi` just typed with `π`. This works for other Greek letters and additional unicode symbols.

We can use any of Julia's built-in math functions, which range from simple to fairly powerful:

    julia> cos(π)
    -1.0

    julia> besselh(1, 1, 1)
    0.44005058574493355 - 0.7812128213002889im

Complex numbers are supported using `im` as an imaginary unit:

    julia> abs(3 + 4im)
    5.0

Some functions will not return a complex result unless you give it a complex input, even if the input is real:

    julia> sqrt(-1)
    ERROR: DomainError:
    sqrt will only return a complex result if called with a complex argument. Try
    sqrt(complex(x)).
     in sqrt at math.jl:146

    julia> sqrt(-1+0im)
    0.0 + 1.0im

    julia> sqrt(complex(-1))
    0.0 + 1.0im

Exact operations on rational numbers are possible using the `//` rational division operator:

    julia> 1//3 + 1//3
    2//3

See the https://www.wikiod.com/julia-lang/arithmetic topic for more about what sorts of arithmetic operators are supported by Julia.

## Dealing with Machine Precision

Note that machine integers are constrained in size, and will [overflow](https://en.wikipedia.org/wiki/Integer_overflow) if the result is too big to be stored:

    julia> 2^62
    4611686018427387904

    julia> 2^63
    -9223372036854775808

This can be prevented by using arbitrary-precision integers in the computation:

    julia> big"2"^62
    4611686018427387904

    julia> big"2"^63
    9223372036854775808

Machine floating points are also limited in precision:

    julia> 0.1 + 0.2
    0.30000000000000004

More (but still limited) precision is possible by again using `big`:

    julia> big"0.1" + big"0.2"
    3.000000000000000000000000000000000000000000000000000000000000000000000000000017e-01

Exact arithmetic can be done in some cases using `Rational`s:

    julia> 1//10 + 2//10
    3//10

## Launch the REPL
After [installing Julia](https://www.wikiod.com/julia-lang/getting-started-with-julia-language#Hello, World!), to launch the read-eval-print-loop (REPL):

## On Unix Systems

Open a terminal window, then type `julia` at the prompt, then hit <kbd>Return</kbd>. You should see something like this come up:

[![julia in ASCII art, along with some version information][1]][1]

## On Windows

Find the Julia program in your start menu, and click it. The REPL should be launched.

  [1]: http://i.stack.imgur.com/5Wpbf.png

## Using REPL Modes
There are three built-in REPL modes in Julia: the Julia mode, the help mode, and the shell mode.

## The Help Mode

The Julia REPL comes with a built-in help system. Press <kbd>?</kbd> at the `julia>` prompt to access the `help?>` prompt.

At the help prompt, type the name of some function or type to get help for:

[![help?> abs; search: abs abs2 abspath abstract AbstractRNG AbstractFloat AbstractArray; abs(x); The absolute value of x.; When abs is applied to signed integers, overflow may occur, resulting in the return of a negative value. This overflow occurs only when abs is applied to the minimum representable value of a signed integer. That is, when x == typemin(typeof(x)), abs(x) == x < 0, not -x as might be expected.][1]][1]

Even if you do not spell the function correctly, Julia can suggest some functions that are possibly what you meant:

    help?> printline
    search:

    Couldn't find printline
    Perhaps you meant println, pipeline, @inline or print
      No documentation found.

      Binding printline does not exist.

This documentation works for other modules too, as long as they use the Julia documentation system.

    julia> using Currencies

    help?> @usingcurrencies
      Export each given currency symbol into the current namespace. The individual unit
      exported will be a full unit of the currency specified, not the smallest possible
      unit. For instance, @usingcurrencies EUR will export EUR, a currency unit worth
      1€, not a currency unit worth 0.01€.

      @usingcurrencies EUR, GBP, AUD
      7AUD  # 7.00 AUD

      There is no sane unit for certain currencies like XAU or XAG, so this macro does
      not work for those. Instead, define them manually:

      const XAU = Monetary(:XAU; precision=4)

## The Shell Mode

See https://www.wikiod.com/julia-lang/shell-scripting-and-piping#Using Shell from inside the REPL for more details about how to use Julia's shell mode, which is accessible by hitting <kbd>;</kbd> at the prompt. This shell mode supports interpolating data from the Julia REPL session, which makes it easy to call Julia functions and make their results into shell commands:

[![shell> ls $(Pkg.dir("JSON")); appveyor.yml  bench  data  LICENSE.md  nohup.out  README.md  REQUIRE  src  test][2]][2]


  [1]: http://i.stack.imgur.com/ElrTF.png
  [2]: http://i.stack.imgur.com/lW4SF.png

