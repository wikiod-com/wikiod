---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Conditional Variable Assignment
The `?=` operator is an extension that behaves like `=`, except that the assignment *only* occurs if the variable is not already set.

    x = hello
    x ?= world
    # $(x) will yield "hello"


## Automatic Variables
Within the context of an individual rule, Make automatically defines a number of special variables.  These variables can have a different value for each rule in a makefile and are designed to make writing rules simpler.  These variables can only be used in the recipe portion of a rule.

| Variable | Description |
| -------- | ------ |
| `$@`     | File name of the rule's target   |
| `$%`     | The target member's name, if the rule's target is an archive   |
| `$<`     | File name of the first prerequisite   |
| `$^`     | List of all prerequisites   |
| `$?`     | List of all prerequisites that are newer than the target   |
| `$*`     | The "stem" of an implicit or pattern rule   |

The following example uses automatic variables to generate a generic rule.  This instructs make how to construct a .o file out of a .c file with the same name.  Since we don't know the specific name of the affected files, we use `$@` as a placeholder for the output file's name and `$^` as a placeholder for the prerequisite list (in this case, the list of input files).

    %.o: %.c
        cc -Wall $^ -c $@

## Referencing a Variable
To use the value stored in a variable, use the dollar sign followed by the variable name enclosed by parentheses or curly braces.

    x = hello
    y = $(x)
    # y now contains the value "hello"
    y = ${x}
    # parentheses and curly braces are treated exactly the same

If a variable's name is only one character long, the parentheses/braces can be omitted (e.g., `$x`).  This practice is used for automatic variables (see below), but is not recommended for general-purpose variables.

## Simply-Expanded Variables
Simply-expanded variables behave like variables from traditional programming languages.  The expression on the right-hand side is evaluated, and the result is stored in the variable.  If the right-hand side contains a variable reference, that variable is expanded before the assignment takes place.

    x := hello
    y := $(x)
    # Both $(x) and $(y) will now yield "hello"
    x := world
    # $(x) will now yield "world", and $(y) will yield "hello"

An alternative form is to use double-colon assignment:

    x ::= hello
Single- and double-colon assignment are equivalent.  The POSIX make standard only mentions the `::=` form, so implementations with strict standards compliance may not support the single-colon version.

## Recursively-Expanded Variables
When defining a recursively-expanded variable, the contents of the right-hand side are stored as-is.  If a variable reference is present, the reference itself is stored (not the value of the variable).  Make waits to expand the variable references until the variable is actually used.

    x = hello
    y = $(x)
    # Both $(x) and $(y) will now yield "hello"
    x = world
    # Both $(x) and $(y) will now yield "world"
In this example, the definition of `y` is recursive.  The reference to `$(x)` doesn't get expanded until `$(y)` is expanded.  This means that whenever the value of `x` changes, the value of `y` will change as well.

Recursively-expanded variables are a powerful but easily-misunderstood tool.  They can be used to create constructs that resemble templates or functions, or even to automatically generate portions of a makefile.  They can also be the source of hard-to-debug problems.  Be careful to only use recursively-expanded variables when necessary.

## Appending Text To an Existing Variable
The `+=` operator is a common extension that adds the specified content to the end of the variable, separated by a space.

    x = hello
    x += world
    
Variable references in the right-hand side will be expanded if and only if the original variable was defined as a simply-expanded variable.

