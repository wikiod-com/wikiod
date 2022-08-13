---
title: "@goto and @label"
slug: "goto-and-label"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Syntax
- @goto label
- @label label

Overuse or inappropriate use of advanced control flow makes code hard to read. `@goto` or its equivalents in other languages, when used improperly, leads to unreadable spaghetti code.

Similar to languages like C, one cannot jump between functions in Julia. This also means that `@goto` is not possible at the top-level; it will only work within a function. Furthermore, one cannot jump from an inner function to its outer function, or from an outer function to an inner function.

## Input validation
Although not traditionally considered loops, the `@goto` and `@label` macros can be used for more advanced control flow. One use case is when the failure of one part should lead to the retry of an entire function, often useful in input validation:

    function getsequence()
        local a, b

    @label start
        print("Input an integer: ")
        try
            a = parse(Int, readline())
        catch
            println("Sorry, that's not an integer.")
            @goto start
        end

        print("Input a decimal: ")
        try
            b = parse(Float64, readline())
        catch
            println("Sorry, that doesn't look numeric.")
            @goto start
        end

        a, b
    end

However, this use case is often more clear using recursion:

    function getsequence()
        local a, b

        print("Input an integer: ")
        try
            a = parse(Int, readline())
        catch
            println("Sorry, that's not an integer.")
            return getsequence()
        end

        print("Input a decimal: ")
        try
            b = parse(Float64, readline())
        catch
            println("Sorry, that doesn't look numeric.")
            return getsequence()
        end

        a, b
    end

Although both examples do the same thing, the second is easier to understand. However, the first one is more performant (because it avoids the recursive call). In most cases, the cost of the call does not matter; but in limited situations, the first form is acceptable.

## Error cleanup
In languages such as C, the `@goto` statement is often used to ensure a function cleans up necessary resources, even in the event of an error. This is less important in Julia, because exceptions and `try`-`finally` blocks are often used instead.

However, it is possible for Julia code to interface with C code and C APIs, and so sometimes functions still need to be written like C code. The below example is contrived, but demonstrates a common use case. The Julia code will call `Libc.malloc` to allocate some memory (this simulates a C API call). If not all allocations succeed, then the function should free the resources obtained so far; otherwise, the allocated memory is returned.

    using Base.Libc
    function allocate_some_memory()
        mem1 = malloc(100)
        mem1 == C_NULL && @goto fail
        mem2 = malloc(200)
        mem2 == C_NULL && @goto fail
        mem3 = malloc(300)
        mem3 == C_NULL && @goto fail
        return mem1, mem2, mem3

    @label fail
        free(mem1)
        free(mem2)
        free(mem3)
    end

