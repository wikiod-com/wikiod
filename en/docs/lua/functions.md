---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Syntax
- *funcname* = function(paramA, paramB, ...) body; return exprlist end -- a simple function
- function *funcname*(paramA, paramB, ...) body; return exprlist end -- shorthand for above
- local *funcname* = function(paramA, paramB, ...) body; return exprlist end -- a lambda
- local *funcname*; *funcname* = function(paramA, paramB, ...) body; return exprlist end -- lambda that can do recursive calls
- local function *funcname*(paramA, paramB, ...) body; return exprlist end -- shorthand for above
- *funcname*(paramA, paramB, ...) -- call a function
- local *var* = *var* or "Default" -- a default parameter
- return nil, "error messages" -- standard way to abort with an error

Functions are usually set with `function a(b,c) ... end` and rarely with setting a variable to an anonymous function (`a = function(a,b) ... end`). The opposite is true when passing functions as parameters, anonymous functions are mostly used, and normal functions aren't used as often.

## Defining a function
    function add(a, b)
        return a + b
    end
    -- creates a function called add, which returns the sum of it's two arguments

Let's look at the syntax.
First, we see a `function` keyword. Well, that's pretty descriptive. Next we see the `add` identifier; the name. We then see the arguments `(a, b)` these can be anything, and they are local. Only inside the function body can we access them. Let's skip to the end, we see... well, the `end`! And all that's in between is the function body; the code that's ran when it is called. The `return` keyword is what makes the function actually give some useful output. Without it, the function returns nothing, which is equivalent to returning nil. This can of course be useful for things that interact with IO, for example:
    
    function printHello(name)
        print("Hello, " .. name .. "!");
    end 

In that function, we did not use the return statement.

Functions can also return values conditionally, meaning that a function has the choice of returning nothing (nil) or a value. This is demonstrated in the following example.

    function add(a, b)
        if (a + b <= 100) then
            return a + b -- Returns a value
        else
            print("This function doesn't return values over 100!") -- Returns nil
        end
    end

It is also possible for a function to return multiple values seperated by commas, as shown:

    function doOperations(a, b)
        return a+b, a-b, a*b
    end
    
    added, subbed, multiplied = doOperations(4,2)

Functions can also be declared local

    do
        local function add(a, b) return a+b end
        print(add(1,2)) --> prints 3
    end
    print(add(2, 2)) --> exits with error, because 'add' is not defined here

They can be saved in tables too:
   
    tab = {function(a,b) return a+b end}
    (tab[1])(1, 2) --> returns 3

## Calling a function.
Functions are only useful if we can call them. To call a function the following syntax is used:

    print("Hello, World!")

We're calling the `print` function. Using the argument `"Hello, World"`. As is obvious, this will print `Hello, World` to the output stream. The returned value is accessible, just like any other variable would be.

    local added = add(10, 50) -- 60

Variables are also accepted in a function's parameters.

    local a = 10
    local b = 60

    local c = add(a, b)

    print(c)

Functions expecting a table or a string can be called with a neat syntactic sugar: parentheses surrounding the call can be omitted.

    print"Hello, world!"
    for k, v in pairs{"Hello, world!"} do print(k, v) end


## Anonymous functions
Creating anonymous functions
--
Anonymous functions are just like regular Lua functions, except they do not have a name. 

    doThrice(function()
        print("Hello!")
    end)

As you can see, the function is not assigned to any name like `print` or `add`. To create an anonymous function, all you have to do is omit the name. These functions can also take arguments.

## Understanding the syntactic sugar

It is important to understand that the following code

```
function double(x)
    return x * 2
end
```

is actually just a shorthand for

```
double = function(x)
    return x * 2
end
```

However, the above function is **not** anonymous as the function is directly assigned to a variable!

## Functions are first class values
This means that a function is a value with the same rights as conventional values like numbers and strings. Functions can be stored in variables, in tables, can be passed as arguments, and can be returned by other functions.

To demonstrate this, we'll also create a "half" function:

```
half = function(x)
    return x / 2
end
```

So, now we have two variables, `half` and `double`, both containing a function as a value. What if we wanted to create a function that would feed the number 4 into two given functions, and compute the sum of both results?

We'll want to call this function like `sumOfTwoFunctions(double, half, 4)`. This will feed the `double` function, the `half` function, and the integer `4` into our own function.

```
function sumOfTwoFunctions(firstFunction, secondFunction, input)
    return firstFunction(input) + secondFunction(input)
end
```

The above `sumOfTwoFunctions` function shows how functions can be passed around within arguments, and accessed by another name.

## Variable number of arguments
https://www.wikiod.com/lua/variadic-arguments

## Checking argument types
Some functions only work on a certain type of argument:

    function foo(tab)
        return tab.bar
    end
    --> returns nil if tab has no field bar, which is acceptable
    --> returns 'attempt to index a number value' if tab is, for example, 3
    --> which is unacceptable

    function kungfoo(tab)
        if type(tab) ~= "table" then
            return nil, "take your useless " .. type(tab) .." somewhere else!"
        end

        return tab.bar
    end

this has several implications:

    print(kungfoo(20)) --> prints 'nil, take your useless number somewhere else!'

    if kungfoo(20) then print "good" else print "bad" end --> prints bad

    foo = kungfoo(20) or "bar" --> sets foo to "bar"

now we can call the function with whatever parameter we want,
and it won't crash the program.

    -- if we actually WANT to abort execution on error, we can still do
    result = assert(kungfoo({bar=20})) --> this will return 20
    result = assert(kungfoo(20)) --> this will throw an error

So, what if we have a function that does something with an instance of a specific class?
This is difficult, because classes and objects are usually tables, so the `type` function will return `'table'`.

    local Class = {data="important"}
    local meta = {__index=Class}

    function Class.new()
        return setmetatable({}, meta)
    end
    -- this is just a very basic implementation of an object class in lua

    object = Class.new()
    fake = {}
    
    print(type(object)), print(type(fake)) --> prints 'table' twice

Solution: compare the metatables

    -- continuation of previous code snippet
    Class.is_instance(tab)
        return getmetatable(tab) == meta
    end

    Class.is_instance(object) --> returns true
    Class.is_instance(fake) --> returns false
    Class.is_instance(Class) --> returns false
    Class.is_instance("a string") --> returns false, doesn't crash the program
    Class.is_instance(nil) --> also returns false, doesn't crash either

## Closures
    do
        local tab = {1, 2, 3}
        function closure()
            for key, value in ipairs(tab) do
                print(key, "I can still see you")
            end
        end
        closure()
        --> 1 I can still see you
        --> 2 I can still see you
        --> 3 I can still see you
    end

    print(tab) --> nil
    -- tab is out of scope

    closure()
    --> 1 I can still see you
    --> 2 I can still see you
    --> 3 I can still see you
    -- the function can still see tab

typical usage example
--

    function new_adder(number)
        return function(input)
            return input + number
        end
    end
    add_3 = new_adder(3)
    print(add_3(2)) --> prints 5

more advanced usage example
--

    function base64.newDecoder(str) -- Decoder factory
        if #str ~= 64 then return nil, "string must be 64 characters long!" end

        local tab = {}
        local counter = 0
        for c in str:gmatch"." do
            tab[string.byte(c)] = counter
            counter = counter + 1
        end

        return function(str)
            local result = ""

            for abcd in str:gmatch"..?.?.?" do
                local a, b, c, d = string.byte(abcd,1,-1)
                a, b, c, d = tab[a], tab[b] or 0, tab[c] or 0, tab[d] or 0
                result = result .. (
                    string.char( ((a<<2)+(b>>4))%256 ) ..
                    string.char( ((b<<4)+(c>>2))%256 ) ..
                    string.char( ((c<<6)+d)%256 )
                )
            end
            return result
        end
    end


## Default parameters
    function sayHello(name)
        print("Hello, " .. name .. "!")
    end

That function is a simple function, and it works well. But what would happen if we just called `sayHello()`?

    stdin:2: attempt to concatenate local 'name' (a nil value)
    stack traceback:
        stdin:2: in function 'sayHello'
        stdin:1: in main chunk
        [C]: in ?

That's not exactly great. There are two ways of fixing this:

1. You immediately return from the function:
       
       function sayHello(name)
         if not (type(name) == "string") then
           return nil, "argument #1: expected string, got " .. type(name)
         end -- Bail out if there's no name.
         -- in lua it is a convention to return nil followed by an error message on error

         print("Hello, " .. name .. "!") -- Normal behavior if name exists.
       end

2. You set a _default_ parameter.

    To do this, simply use this simple expression


    function sayHello(name)
        name = name or "Jack" -- Jack is the default, 
                              -- but if the parameter name is given, 
                              -- name will be used instead
        print("Hello, " .. name .. "!")
    end

The idiom `name = name or "Jack"` works because `or` in Lua short circuits. If the item on the left side of an `or` is anything other than `nil` or `false`, then the right side is never evaluated. On the other hand, if `sayHello` is called with no parameter, then `name` will be `nil`, and so the string `"Jack"` will be assigned to `name`. (Note that this idiom, therefore, will not work if the boolean `false` is a reasonable value for the parameter in question.)

## Multiple results
Functions in Lua can return multiple results.

For example:   
        
    function triple(x)
        return x, x, x
    end

When calling a function, to save these values, you must use the following syntax:
   
    local a, b, c = triple(5)

Which will result in `a = b = c = 5` in this case. It is also possible to ignore returned values by using the throwaway variable `_` in the desired place in a list of variables:

    local a, _, c = triple(5)

In this case, the second returned value will be ignored. It's also possible to ignore return values by not assigning them to any variable:

    local a = triple(5)

Variable `a` will be assigned the first return value and the remaining two will be discarded.

When a variable amount of results are returned by a function, one can store them all in a table, by executing the function inside it:

    local results = {triple(5)}

This way, one can iterate over the `results` table to see what the function returned.

**Note**

This can be a surprise in some cases, for example:

    local t = {}
    table.insert(t, string.gsub("  hi", "^%s*(.*)$", "%1")) --> bad argument #2 to 'insert' (number expected, got string)

This happens because `string.gsub` returns 2 values: the given string, with occurrences of the pattern replaced, and the total number of matches that occurred. 

To solve this, either use an intermediate variable or put `()` around the call, like so:

    table.insert(t, (string.gsub("  hi", "^%s*(.*)$", "%1"))) --> works. t = {"hi"}

This grabs only the first result of the call, and ignores the rest.

## Named Arguments
    local function A(name, age, hobby)
        print(name .. "is " .. age .. " years old and likes " .. hobby)
    end
    A("john", "eating", 23) --> prints 'john is eating years old and likes 23'
    -- oops, seems we got the order of the arguments wrong...
    -- this happens a lot, specially with long functions that take a lot of arguments
    -- and where the order doesn't follow any particular logic

    local function B(tab)
        print(tab.name .. "is " .. tab.age .. " years old and likes " .. tab.hobby)
    end
    local john = {name="john", hobby="golf", age="over 9000", comment="plays too much golf"}
    B(john)
    --> will print 'John is over 9000 years old and likes golf'
    -- I also added a 'comment' argument just to show that excess arguments are ignored by the function
    
    B({name = "tim"}) -- can also be written as
    B{name = "tim"} -- to avoid cluttering the code
    --> both will print 'tim is nil years old and likes nil'
    -- remember to check for missing arguments and deal with them

    function C(tab)
        if not tab.age then return nil, "age not defined" end
        tab.hobby = tab.hobby or "nothing"
        -- print stuff
    end

    -- note that if we later decide to do a 'person' class
    -- we just need to make sure that this class has the three fields
    -- age, hobby and name, and it will be compatible with these functions

    -- example:
    local john = ClassPerson.new("John", 20, "golf") -- some sort of constructor
    john.address = "some place" -- modify the object
    john:do_something("information") -- call some function of the object
    C(john) -- this works because objects are *usually* implemented as tables

