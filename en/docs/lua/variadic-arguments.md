---
title: "Variadic Arguments"
slug: "variadic-arguments"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

*Varargs*, as they are commonly known, allow functions to take an arbitrary number of arguments without specification. All arguments given to such a function are packaged into a single structure known as the *vararg list*; which is written as `...` in Lua. There are basic methods for extracting the number of given arguments and the value of those arguments using the `select()` function, but more advanced usage patterns can leverage the structure to it's full utility.

## Syntax
 - *...* -- Makes the function whose arguments list in which this appears a variadic function
 - *select(what, ...)* -- If 'what' is a number in range 1 to the number of elements in the vararg, returns the 'what'th element to the last element in the vararg. The return will be nil if the index is out of bounds. If 'what' is the string '#', returns the number of elements in the vararg.

**Efficiency**

The vararg list is implemented as a linked list in the PUC-Rio implementation of the language, this means that indexes are O(n). That means that iterating over the elements in a vararg using `select()`, like the example below, is an O(n^2) operation.

    for i = 1, select('#', ...) do
        print(select(i, ...))
    end

If you plan on iterating over the elements in a vararg list, first pack the list into a table. Table accesses are O(1), so iterating is O(n) in total. Or, if you are so inclined, see the `foldr()` example from the advanced usage section; it uses recursion to iterate over a vararg list in O(n).

**Sequence Length Definition**

The vararg is useful in that the length of the vararg respects any explicitly passed (or computed) nils. For example.

    function test(...)
        return select('#', ...)
    end

    test()             --> 0
    test(nil, 1, nil)  --> 3

This behavior conflicts with the behavior of tables however, where the length operator `#` does not work with 'holes' (embedded nils) in sequences. Computing the length of a table with holes is undefined and cannot be relied on. So, depending upon the values in `...`, taking the length of `{...}` may not result in the *'correct'* answer. In Lua 5.2+ `table.pack()` was introduced to handle this deficiency (there is a function in the example that implements this function in pure Lua).

**Idiomatic Use**

Because varargs carry around their length people use them as sequences to avoid the issue with holes in tables. This was not their intended usage and one the reference implementation of Lua does not optimize for. Although such usage is explored in the examples, it is generally frowned upon.

## Basics
Variadic functions are created using the `...` ellipses syntax in the argument list of the function definition.

    function id(...)
        return
    end

If you called this function as `id(1, 2, 3, 4, 5)` then `...` (AKA the vararg list) would contain the values `1, 2, 3, 4, 5`.

Functions can take required arguments as well as `...`.

    function head(x, ...)
        return x
    end

The easiest way to pull elements from the vararg list is to simply assign variables from it.

    function head3(...)
        local a, b, c = ...
        return a, b, c
    end

`select()` can also be used to find the number of elements and extract elements from `...` indirectly.

    function my_print(...)
        for i = 1, select('#', ...) do
            io.write(tostring(select(i, ...)) .. '\t')
        end
        io.write '\n'
    end

`...` can be packed into a table for ease of use, by using `{...}`. This places all the arguments in the sequential part of the table.
<!-- if version [gte 5.2] -->
`table.pack(...)` can also be used to pack the vararg list into a table. The advantage of `table.pack(...)` is that it sets the `n` field of the returned table to the value of `select('#', ...)`. This is important if your argument list may contain nils (see remarks section below).
<!-- end version if -->

    function my_tablepack(...)
        local t = {...}
        t.n = select('#', ...)
        return t
    end

The vararg list may also be returned from functions. The result is multiple returns.

    function all_or_none(...)
        local t = table.pack(...)
        for i = 1, t.n do
            if not t[i] then
                return    -- return none
            end
        end
        return ...    -- return all
    end

## Advanced Usage
As stated in the basic examples, you can have variable bound arguments and the variable argument list (`...`). You can use this fact to recursively pull apart a list as you would in other languages (like Haskell). Below is an implementation of `foldr()` that takes advantage of that. Each recursive call binds the head of the vararg list to `x`, and passes the rest of the list to a recursive call. This destructures the list until there is only one argument (`select('#', ...) == 0`). After that, each value is applied to the function argument `f` with the previously computed result.

    function foldr(f, ...)
        if select('#', ...) < 2 then return ... end
        local function helper(x, ...)
            if select('#', ...) == 0 then
              return x
            end
            return f(x, helper(...))
        end
        return helper(...)
    end
    
    function sum(a, b)
        return a + b
    end
    
    foldr(sum, 1, 2, 3, 4)
    --> 10    

You can find other function definitions that leverage this programming style [here][1] in Issue #3 through Issue #8.

Lua's sole idiomatic data structure is the table. The table length operator is undefined if there are `nil`s located anywhere in a sequence. Unlike tables, the vararg list respects explicit `nil`s as stated in the basic examples and the remarks section (please read that section if you haven't yet). With little work the vararg list can perform every operation a table can besides mutation. This makes the vararg list a good candidate for implementing immutable tuples.

    function tuple(...)
        -- packages a vararg list into an easily passable value
        local co = coroutine.wrap(function(...)
            coroutine.yield()
            while true do
                coroutine.yield(...)
            end
        end)
        co(...)
        return co
    end
    
    local t = tuple((function() return 1, 2, nil, 4, 5 end)())
    
    print(t())                 --> 1    2    nil    4    5    | easily unpack for multiple args
    local a, b, d = t()        --> a = 1, b = 2, c = nil      | destructure the tuple
    print((select(4, t())))    --> 4                          | index the tuple
    print(select('#', t()))    --> 5                          | find the tuple arity (nil respecting)
    
    local function change_index(tpl, i, v)
        -- sets a value at an index in a tuple (non-mutating)
        local function helper(n, x, ...)
            if select('#', ...) == 0 then
                if n == i then
                    return v
                else
                    return x
                end
            else
                if n == i then
                    return v, helper(n+1, ...)
                else
                    return x, helper(n+1, ...)
                end
            end
        end
        return tuple(helper(1, tpl()))
    end
    
    local n = change_index(t, 3, 3)
    print(t())                 --> 1    2    nil    4    5
    print(n())                 --> 1    2    3    4    5

The main difference between what's above and tables is that tables are mutable and have pointer semantics, where the tuple does not have those properties. Additionally, tuples can hold explicit `nil`s and have a never-undefined length operation.
    

  [1]: http://lua-users.org/wiki/VarargTheSecondClassCitizen

