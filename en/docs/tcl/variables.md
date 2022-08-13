---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
 - set ***varName* *?value?***
 - unset ?**-nocomplain**? ?--? ?*varName varName varName*?
 - puts $*varName*
 - puts [set *varName*]
 - variable *varName*
 - global *varName* ?***varName varName***?

- Parameters enclosed within *?...?* such as *?varName?* represent optional arguments to a Tcl command.
 - Documentation: [global][1], [upvar][2]


  [1]: http://www.tcl.tk/man/tcl/TclCmd/global.htm
  [2]: http://www.tcl.tk/man/tcl/TclCmd/upvar.htm

## Assigning values to variables
The command `set` is used to assign values in Tcl. When it is called with two arguments in the following manner,


    % set tempVar "This is a string."
    This is a string.

it places the second argument ("This is a string.") in the memory space referenced by the first argument (tempVar). `set` always returns the contents of the variable named in the first argument. In the above example, `set` would return "This is a string." without the quotes.

 - If *value* is specified, then the contents of the variable *varName*
   are set equal to *value*.
 - If *varName* consists only of
   alphanumeric characters, and no parentheses, it is a scalar
   variable.
 - If *varName* has the form *varName(index)*, it is a
   member of an associative array.

Note that the name of the variable is not restricted to the Latin alphabet, it may consist of any combination of unicode characters (e.g. Armenian):

    % set տուն house
    house
    % puts ${տուն}
    house
    
 

## Scoping
    set alpha 1

    proc myproc {} {
        puts $alpha
    }

    myproc

This code doesn't work because the two alphas are in different scopes.

The command `set alpha 1` creates a variable in the global scope (which makes it a global variable).

The command `puts $alpha` is executed in a scope that is created when the command `myproc` executes.

The two scopes are distinct. This means that when `puts $alpha` tries to look up the name `alpha`, it doesn't find any such variable.

We can fix that, however:

    proc myproc {} {
        global alpha beta
        puts $alpha
    }

In this case two global variables, `alpha` and `beta`, are linked to alias variables (with the same name) in the procedure's scope. Reading from the alias variables retrieves the value in the global variables, and writing to them changes the values in the globals.

More generally, the `upvar` command creates aliases to variables from any of the previous scopes. It can be used with the global scope (`#0`):

    proc myproc {} {
        upvar #0 alpha alpha beta b
        puts $alpha
    }

The aliases can be given the same name as the variable that is linked to(`alpha`) or another name (`beta` / `b`).

If we call `myproc` from the global scope, this variant also works:

    proc myproc {} {
        upvar 1 alpha alpha beta b
        puts $alpha
    }

The scope number `1` means "the previous scope" or "the caller's scope".

Unless you really know what you're doing, `#0`, `0`, and `1` are the only scopes that make sense to use with `upvar`. (`upvar 0` creates a local alias for a local variable, not strictly a scoping operation.)

Some other languages define scope by curly braces, and let code running in each scope see all names in surrounding scopes. In Tcl one single scope is created when a procedure is called, and only its own names are visible. If a procedure calls another procedure, its scope is stacked on top of the previous scope, and so on. This means that in contrast with C-style languages that only have global scope and local scope (with subscopes), each scope acts as an enclosing (though not immediately visible) scope to any scope it has opened. When a procedure returns, its scope is destroyed.

Documentation:
[global](http://www.tcl.tk/man/tcl/TclCmd/global.htm "Access global variables"),
[upvar](http://www.tcl.tk/man/tcl/TclCmd/upvar.htm "Create link to variable in a different stack frame")


## Invoking set with one argument
`set` can also be invoked with just one argument. When called with just one argument, it returns the contents of that argument.

    % set x 235
    235
    % set x
    235

## Deleting variable/s
The `unset` command is used to remove one or more variables. 

    unset ?-nocomplain? ?--? ?name name name name?

 - Each *name* is a variable name specified in any of the ways acceptable to the `set` command.
 - If a *name* refers to an element of an array then that element is removed without affecting the remainder of the array. 
 - If a *name* consists of an array name with no index in parentheses, then the entire array is deleted.
 - If **-nocomplain** is given as the first argument, then all possible errors are suppressed from the command's output.
 - The option -- indicates the end of the options, and should be used if you wish to remove a variable with the same name as any of the options.


    % set x 235
    235
    % set x
    235
    % unset x
    % set x
    can't read "x": no such variable



## Printing the value of a variable
In order to print the value of a variable such as,

    set tempVar "This is a string."
The argument in the puts statement is preceded by a **$** sign, which tells Tcl to use the value of the variable.

    % set tempVar "This is a string."
    This is a string.
    % puts $tempVar
    This is a string.



## Namespace variables
The `variable` command ensures that a given namespace variable is created. Until a value is assigned to it, the variable's value is undefined:

    namespace eval mynamespace {
        variable alpha
        set alpha 0
    }

The variable can be accessed from outside the namespace (from anywhere, in fact) by attaching the name of the namespace to it:

    set ::mynamespace::alpha

Access can be simplified within a procedure by using the `variable` command again:

    proc ::mynamespace::myproc {} {
        variable alpha
        set alpha
    }

This creates a local alias for the namespace variable.

For a procedure defined in another namespace, the variable name must contain the namespace in the invocation of `variable`:

    proc myproc {} {
        variable ::mynamespace::alpha
        set alpha
    }




