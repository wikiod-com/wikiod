---
title: "Expressions"
slug: "expressions"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Another benefit from using braced expression strings is that the byte compiler usually can generate more efficient code (5 - 10x faster) from them.

## The problems with unbraced expressions
It is a good practice to provide expression string arguments as braced strings. The heading "Double Substitution" outlines important reasons behind the same.

The `expr` command evaluates an operator-based expression string to calculate a value. This string is constructed from the arguments in the invocation.

    expr 1 + 2    ; # three arguments
    expr "1 + 2"  ; # one argument
    expr {1 + 2}  ; # one argument

These three invocations are equivalent and the expression string is the same.

The commands `if`, `for`, and `while` use the same evaluator code for their condition arguments:

    if {$x > 0} ...
    for ... {$x > 0} ... ...
    while {$x > 0} ...

The main difference is that the condition expression string must always be a single argument.

As with every argument in a command invocation in Tcl, the contents may or may not be subjected to substitution, depending on how they are quoted / escaped:

    set a 1
    set b 2
    expr $a + $b   ; # expression string is {1 + 2}
    expr "$a + $b" ; # expression string is {1 + 2}
    expr \$a + \$b ; # expression string is {$a + $b}
    expr {$a + $b} ; # expression string is {$a + $b}

There is a difference in the third and fourth cases as the backslashes / braces prevent substitution. The result is still the same, since the evaluator inside `expr` can itself perform Tcl variable substitution and transform the string to `{1 + 2}`.

    set a 1
    set b "+ 2"
    expr $a $b   ; # expression string is {1 + 2}
    expr "$a $b" ; # expression string is {1 + 2}
    expr {$a $b} ; # expression string is {$a $b}: FAIL!

Here we get into trouble with the braced argument: when the evaluator in `expr` performs substitutions, the expression string has already been parsed into operators and operands, so what the evaluator sees is a string consisting of two operands with no operator between them. (The error message is "`missing operator at _@_ in expression "$a _@_$b"`".)

In this case, variable substitution before `expr` was called prevented an error. Bracing the argument prevented variable substitution until expression evaluation, which caused an error.

Situations like this can occur, most typically when an expression to evaluate is passed in as a variable or parameter. In those cases there is no other choice than to leave the argument unbraced to allow the argument evaluator to "unpack" the expression string for delivery to `expr`.

In most other cases, though, bracing the expression does no harm and indeed can avert a lot of problems. Some examples of this:

**Double substitution**

    set a {[exec make computer go boom]}
    expr $a      ; # expression string is {[exec make computer go boom]}
    expr {$a}    ; # expression string is {$a}

The unbraced form will perform the command substitution, which is a command that destroys the computer somehow (or encrypts or formats the hard disk, or what have you). The braced form will perform a variable substitution and then try (and fail) to make something of the string "[exec make computer go boom]". Disaster averted.

**Endless loops**

    set i 10
    while "$i > 0" {puts [incr i -1]}

This problem affects both `for` and `while`. While it seems that this loop would count down to 0 and exit, the condition argument to while is actually always `10>0` because that was what the argument was evaluated to be when the `while` command was activated. When the argument is braced, it is passed to the `while` command as `$i>0`, and the variable will be substituted once for every iteration. Use this instead:

    while {$i > 0} {puts [incr i -1]}


**Total evaluation**

    set a 1
    if "$a == 0 && [incr a]" {puts abc}

What is the value of `a` after running this code? Since the `&&` operator only evaluates the right operand if the left operand is true, the value should still be 1. But actually, it's 2. This is because the argument evaluator has already performed all variable and command substitutions by the time the expression string is evaluated. Use this instead:

    if {$a == 0 && [incr a]} {puts abc}

Several operators (the logical connectives `||` and `&&`, and the conditional operator `?:`) are defined to *not* evaluate all their operands, but they can only work as designed if the expression string is braced.


## Multiplying a variable by 17
    set myVariable [expr { $myVariable * 17 }]

This shows how you can use a simple expression to update a variable. The `expr` command does not update the variable for you; you need to take its result and write it to the variable with `set`.

Note that newlines are not important in the little language understood by `expr`, and adding them can make longer expressions much easier to read.

    set myVariable [expr {
        $myVariable * 17
    }]

This does _exactly_ the same thing though.

## Calling a Tcl command from an expression
Sometimes you need to call a Tcl command from your expression. For example, supposing you need the length of a string in it. To do that, you just use a `[...]` sequence in the expression:

    set halfTheStringLength [expr { [string length $theString] / 2 }]

You can call any Tcl command this way, but if you find yourself calling `expr` itself, ***stop!*** and think whether you really need that extra call. You can _usually_ do just fine by putting the inner expression in parentheses.

## Invalid bareword error
In Tcl itself, a string consisting of a single word does not need to be quoted. In the language of expression strings that `expr` evaluates, all operands must have an identifiable type.

Numeric operands are written without any decoration:

    expr {455682 / 1.96e4}

So are boolean constants:

    expr {true && !false}

Tcl variable substitution syntax is recognized: the operand will be set to the variable's value:

    expr {2 * $alpha}

The same goes for command substitution:

    expr {[llength $alpha] > 0}

Operands can also be mathematical function calls, with a comma-separated list of operands within parentheses:

    expr {sin($alpha)}

An operand can be a double-quoted or braced string. A double-quoted string will be subject to substitution just like in a command line.

    expr {"abc" < {def}}

If an operand isn't one of the above, it is illegal. Since there is no hint that shows what kind of a word it is, `expr` signals a bareword error.

