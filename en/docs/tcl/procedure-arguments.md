---
title: "Procedure arguments"
slug: "procedure-arguments"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

References: 
[proc][1]  
[Argument Expansion][2] (section 5)


  [1]: http://tcl.tk/man/tcl/TclCmd/proc.htm
  [2]: http://tcl.tk/man/tcl8.6/TclCmd/Tcl.htm

## A procedure that does not accept arguments
    proc myproc {} {
        puts "hi"
    }
    myproc
    # => hi


An empty argument list (the second argument after the procedure name, "myproc") means that the procedure will not accept arguments.

## A procedure that accepts a variable number of arguments
    ### Definition
    proc myproc {alpha {beta {}} {gamma green}} {
        puts [list $alpha $beta $gamma]
    }

<!-- -->

    ### Use
    myproc A
    # => A {} green
    myproc A B
    # => A B green
    myproc A B C
    # => A B C

This procedure accepts one, two, or three arguments: those parameters whose names are the first item in a two-item list are optional. The parameter variables (`alpha`, `beta`, `gamma`) get as many argument values as are available, assigned from left to right. Parameter variables that don't get any argument values instead get their values from the second item in the list they were a part of.

Note that optional arguments must come at the end of the argument list. If argument<sub>N-1</sub> is optional, argument<sub>N</sub> must be optional too. If in a case, where user have argument<sub>N</sub> but not argument<sub>N-1</sub>, default value of argument<sub>N-1</sub> needs to be explicitly mentioned before argument<sub>N</sub>, while calling the procedure.

    myproc A B C D
    # (ERROR) wrong # args: should be "myproc alpha ?beta? ?gamma?"

The procedure does not accept more than three arguments: note that a helpful error message describing the argument syntax is automatically created.

## A procedure that accepts a name/reference to a variable
    proc myproc {varName alpha beta} {
        upvar 1 $varName var
        set var [expr {$var * $alpha + $beta}]
    }
    set foo 1
    myproc foo 10 5
    puts $foo
    # => 15

In this particular case, the procedure is given the name of a variable in the current scope. Inside a Tcl procedure, such variables aren't automatically visible, but the `upvar` command can create an alias for a variable from another stack level: 1 means the caller's stack level, #0 means the global level, etc. In this case, the stack level 1 and the name `foo` (from the parameter variable `varName`) lets `upvar` find that variable and create an alias called `var`. Every read or write operation on `var` also happens to `foo` in the caller's stack level.

## A procedure that accepts two arguments
    proc myproc {alpha beta} {
        ...
        set foo $alpha
        set beta $bar     ;# note: possibly useless invocation
    }

    myproc 12 34          ;# alpha will be 12, beta will be 34

If the argument list consists of words, those will be the names of local variables in the procedure, and their initial values will be equal to the argument values on the command line. The arguments are passed by value and whatever happens to the variable values inside the procedure will not influence the state of data outside the procedure.

## A procedure that accepts any number of arguments
    proc myproc args { ... }
    proc myproc {args} { ... } ;# equivalent

If the special parameter name `args` is the last item in the argument list, it receives a list of all arguments at that point in the command line. If there are none, the list is empty.

There can be arguments, including optional ones, before `args`:

    proc myproc {alpha {beta {}} args} { ... }

This procedure will accept one or more arguments. The first two, if present, will be consumed by `alpha` and `beta`: the list of the rest of the arguments will be assigned to `args`.

## The {*} syntax
Sometimes what you have is a list, but the command you want to pass the items in the list to demands to get each item as a separate argument. For instance: the `winfo children` command returns a list of windows, but the `destroy` command will only take a sequence of window name arguments.

    set alpha [winfo children .]
    # => .a .b .c
    destroy $alpha
    # (no response, no windows are destroyed)

The solution is to use the `{*}` syntax:

    destroy {*}[winfo children .]

or

    destroy {*}$alpha

What the `{*}` syntax does is to take the following value (no whitespace in between!) and splice the items in that value into the command line as if they were individual arguments.

If the following value is an empty list, nothing is spliced in:

    puts [list a b {*}{} c d]
    # => a b c d

If there are one or more items, they are inserted:

    puts [list a b {*}{1 2 3} c d]
    # => a b 1 2 3 c d


