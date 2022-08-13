---
title: "Booleans in Lua"
slug: "booleans-in-lua"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

Booleans, truth, and falsity are straightforward in Lua. To review:

1. There is a boolean type with exactly two values: `true` and `false`.
1. In a conditional context (`if`, `elseif`, `while`, `until`), a boolean is not required. Any expression can be used.
1. In a conditional context, `false` and `nil` count as false, and everything else counts as true.
1. Although 3 already implies this: if you're coming from other languages, remember that `0` and the empty string count as true in conditional contexts in Lua.

## The boolean type
Booleans and other values
==

When dealing with lua it is important to differentiate between the boolean values `true` and `false` and values that evaluate to true or false.

There are only two values in lua that evaluate to false: `nil` and `false`, while everything else, including the numerical `0` evaluate to true.

Some examples of what this means:

    if 0 then print("0 is true") end --> this will print "true"
    if (2 == 3) then print("true") else print("false") end --> this prints "false"
    if (2 == 3) == false then print("true") end --> this prints "true"
    if (2 == 3) == nil then else print("false") end
    --> prints false, because even if nil and false both evaluate to false,
    --> they are still different things.

Logical Operations
==
Logical operators in lua don't necessarily return boolean values:

`and` will return the second value if the first value evaluates to true;

`or` returns the second value if the first value evaluates to false;

This makes it possible to simulate the ternary operator, just like in other languages:

    local var = false and 20 or 30 --> returns 30
    local var = true and 20 or 30 --> returns 20
    -- in C: false ? 20 : 30

This can also be used to initialize tables if they don't exist

    tab = tab or {} -- if tab already exists, nothing happens

or to avoid using if statements, making the code easier to read

    print(debug and "there has been an error") -- prints "false" line if debug is false
    debug and print("there has been an error") -- does nothing if debug is false
    -- as you can see, the second way is preferable, because it does not output
    -- anything if the condition is not met, but it is still possible.
    -- also, note that the second expression returns false if debug is false,
    -- and whatever print() returns if debug is true (in this case, print returns nil)

Checking if variables are defined
==
One can also easily check if a variable exists (if it is defined), since non-existant variables return `nil`, which evaluates to false.

    local tab_1, tab_2 = {}
    if tab_1 then print("table 1 exists") end --> prints "table 1 exists"
    if tab_2 then print("table 2 exists") end --> prints nothing

The only case where this does not apply is when a variable stores the value `false`, in which case it technically exists but still evaluates to false. Because of this, it is a bad design to create functions which return `false` and `nil` depending on the state or input. We can still check however whether we have a `nil` or a `false`:

    if nil == nil then print("A nil is present") else print("A nil is not present") end
    if false == nil then print("A nil is present") else print("A nil is not present") end
    -- The output of these calls are:
    -- A nil is present!
    -- A nil is not present

## Conditional contexts
Conditional contexts in Lua (`if`, `elseif`, `while`, `until`) do not require a boolean. Like many languages, any Lua value can appear in a condition. The rules for evaluation are simple:

1. `false` and `nil` count as false.
1. Everything else counts as true.

       if 1 then
         print("Numbers work.")
       end
       if 0 then
         print("Even 0 is true")
       end

       if "strings work" then
         print("Strings work.")
       end
       if "" then
         print("Even the empty string is true.")
       end

## Logical Operators
In Lua, booleans can be manipulated through *logical operators*. These operators include `not`, `and`, and `or`.

In simple expressions, the results are fairly straightforward:

    print(not true) --> false
    print(not false) --> true
    print(true or false) --> true
    print(false and true) --> false

***

## Order of Precedence ##

The order of precedence is similar to the math operators unary `-`, `*` and `+`:
* `not`
* then `and`
* then `or`

This can lead to complex expressions:

    print(true and false or not false and not true)
    print( (true and false) or ((not false) and (not true)) )
        --> these are equivalent, and both evaluate to false

***

## Short-cut Evaluation ##

The operators `and` and `or` might only be evaluated using the first operand, provided the second is unnecessary:

    function a()
        print("a() was called")
        return true
    end

    function b()
        print("b() was called")
        return false
    end

    print(a() or b())
        --> a() was called
        --> true
        --  nothing else
    print(b() and a())
        --> b() was called
        --> false
        --  nothing else
    print(a() and b())
        --> a() was called
        --> b() was called
        --> false
***

## Idiomatic [conditional operator][1] ##

Due to the precedence of the logical operators, the ability for short-cut evaluation and the evaluation of non-`false` and non-`nil` values as `true`, an idiomatic conditional operator is available in Lua:

    function a()
        print("a() was called")
        return false
    end
    function b()
        print("b() was called")
        return true
    end
    function c()
        print("c() was called")
        return 7
    end
    
    print(a() and b() or c())
        --> a() was called
        --> c() was called
        --> 7
        
    print(b() and c() or a())
        --> b() was called
        --> c() was called
        --> 7

Also, due to the nature of the `x and a or b` structure, `a` will never be *returned* if it evaluates to `false`, this conditional will then always return `b` no matter what `x` is.

    print(true and false or 1)  -- outputs 1


  [1]: https://en.wikipedia.org/wiki/%3F:

## Truth tables
Logical operators in Lua don't "return" boolean, but one of their arguments. Using `nil` for false and numbers for true, here's how they behave.


    print(nil and nil)       -- nil
    print(nil and 2)         -- nil
    print(1 and nil)         -- nil
    print(1 and 2)           -- 2

    print(nil or nil)        -- nil
    print(nil or 2)          -- 2
    print(1 or nil)          -- 1
    print(1 or 2)            -- 1

As you can see, Lua will always return the first value that makes the check *fail* or *succeed*. Here's the truth tables showing that.

      x  |  y  || and            x  |  y  || or
    ------------------         ------------------
    false|false||  x           false|false||  y   
    false|true ||  x           false|true ||  y   
    true |false||  y           true |false||  x   
    true |true ||  y           true |true ||  x

For those who need it, here's two function representing these logical operators.

    function exampleAnd(value1, value2)
      if value1 then
        return value2
      end
      return value1
    end

    function exampleOr(value1, value2)
      if value1 then
        return value1
      end
      return value2
    end

## Emulating Ternary Operator with 'and' 'or' logical operators.
In lua, the logical operators `and` and `or` returns one of the operands as the result instead of a boolean result. As a consequence, this mechanism can be exploited to emulate the behavior of the ternary operator despite lua not having a 'real' ternary operator in the language.

Syntax
======

> *condition* **and** *truthy_expr* **or** *falsey_expr*

Use in variable assignment/initialization
=========================================

    local drink = (fruit == "apple") and "apple juice" or "water"

Use in table constructor
========================

    local menu =
    {
      meal  = vegan and "carrot" or "steak",
      drink = vegan and "tea"    or "chicken soup"
    }

Use as function argument
========================

    print(age > 18 and "beer" or "fruit punch")

Use in return statement
=======================

    function get_gradestring(student)
      return student.grade > 60 and "pass" or "fail"
    end

Caveat
======

There are situations where this mechanism doesn't have the desired behavior. Consider this case

    local var = true and false or "should not happen"

In a 'real' ternary operator, the expected value of `var` is `false`. In lua, however, the `and` evaluation 'falls through' because the second operand is falsey. As a result `var` ends up with `should not happen` instead.

Two possible workarounds to this problem, refactor this expression so the middle operand isn't falsey. eg.

    local var = not true and "should not happen" or false

or alternatively, use the classical `if` `then` `else` construct.

