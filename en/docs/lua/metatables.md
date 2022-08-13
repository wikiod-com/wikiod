---
title: "Metatables"
slug: "metatables"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- [[local] *mt* = ]getmetatable(*t*) --> retrieve associated metatable for '*t*'
- [[local] *t* = ]setmetatable(*t*, *mt*)  --> set the metatable for '*t*' to '*mt*' and returns '*t*'

## Parameters
| Parameter | Details |
| --------- | ------- |
| t         | Variable referring to a lua table; can also be a table literal.
| mt        | Table to use as a metatable; can have zero or more metamethod fields set.

There are some metamethods not mentioned here. For the full list and their usage, see the corresponding entry in the [lua manual][1].


  [1]: https://www.lua.org/manual/5.3/manual.html#2.4

## Simulating OOP


## Creation and usage of metatables
A metatable defines a set of operations which alter the behaviour of a lua object. A metatable is just an ordinary table, which is used in a special way.

    local meta = { } -- create a table for use as metatable

    -- a metatable can change the behaviour of many things
    -- here we modify the 'tostring' operation:
    -- this fields should be a function with one argument.
    -- it gets called with the respective object and should return a string
    meta.__tostring = function (object)
        return string.format("{ %d, %d }", object.x, object.y)
    end

    -- create an object
    local point = { x = 13, y = -2 }
    -- set the metatable
    setmetatable(point, meta)

    -- since 'print' calls 'tostring', we can use it directly:
    print(point) -- prints '{ 13, -2 }'

## Garbage collector - the __gc metamethod
<!-- if version [gte 5.2] -->
Objects in lua are garbage collected. Sometimes, you need to free some resource, want to print a message or do something else when an object is destroyed (collected). For this, you can use the `__gc` metamethod, which gets called with the object as argument when the object is destroyed. You could see this metamethod as a sort of destructor.

This example shows the `__gc` metamethod in action. When the inner table assigned to `t` gets garbage collected, it prints a message prior to being collected. Likewise for the outer table when reaching the end of script:

    local meta =
    {
        __gc = function(self)
            print("destroying self: " .. self.name)
        end
    }
    
    local t = setmetatable({ name = "outer" }, meta)
    do
        local t = { name = "inner" }
        setmetatable(t, meta)
    end
    
<!-- end version if -->

## Indexing of tables
Perhaps the most important use of metatables is the possibility to change the indexing of tables. For this, there are two actions to consider: *reading* the content and *writing* the content of the table. Note that both actions are only triggered if the corresponding key is not present in the table. 

## Reading ##

    local meta = {}
    
    -- to change the reading action, we need to set the '__index' method
    -- it gets called with the corresponding table and the used key
    -- this means that table[key] translates into meta.__index(table, key)
    meta.__index = function(object, index)
        -- print a warning and return a dummy object
        print(string.format("the key '%s' is not present in object '%s'", index, object))
        return -1
    end

    -- create a testobject
    local t = {}
    
    -- set the metatable
    setmetatable(t, meta)
    
    print(t["foo"]) -- read a non-existent key, prints the message and returns -1

This could be used to raising an error while reading a non-existent key:

    -- raise an error upon reading a non-existent key
    meta.__index = function(object, index)
        error(string.format("the key '%s' is not present in object '%s'", index, object))
    end

## Writing ##

    local meta = {}
    
    -- to change the writing action, we need to set the '__newindex' method
    -- it gets called with the corresponding table, the used key and the value
    -- this means that table[key] = value translates into meta.__newindex(table, key, value)
    meta.__newindex = function(object, index, value)
        print(string.format("writing the value '%s' to the object '%s' at the key '%s'",
                             value, object, index))
        --object[index] = value -- we can't do this, see below
    end
    
    -- create a testobject
    local t = { }
    
    -- set the metatable
    setmetatable(t, meta)

    -- write a key (this triggers the method)
    t.foo = 42

You may now ask yourself how the actual value is written in the table. In this case, it isn't. The problem here is that metamethods can trigger metamethods, which would result in an infinitive loop, or more precisely, a stack overflow. So how can we solve this? The solution for this is called *raw table access*.

## Using tables as metamethods
Some metamethods don't have to be functions. To most important example for this is the `__index` metamethod. It can also be a table, which is then used as lookup. This is quite commonly used in the creation of classes in lua. Here, a table (often the metatable itself) is used to hold all the operations (methods) of the class:

    local meta = {}
    -- set the __index method to the metatable.
    -- Note that this can't be done in the constructor!
    meta.__index = meta
    
    function create_new(name)
        local self = { name = name }
        setmetatable(self, meta)
        return self
    end
    
    -- define a print function, which is stored in the metatable
    function meta.print(self)
        print(self.name)
    end
    
    local obj = create_new("Hello from object")
    obj:print()



## More metamethods
There are many more metamethods, some of them are arithmetic (e.g. addition, subtraction, multiplication), there are bitwise operations (and, or, xor, shift), comparison (<, >) and also basic type operations like == and # (equality and length). Lets build a class which supports many of these operations: a call for rational arithmetic. While this is very basic, it shows the idea.

    local meta = {
        -- string representation
        __tostring = function(self)
            return string.format("%s/%s", self.num, self.den)
        end,
        -- addition of two rationals
        __add = function(self, rhs)
            local num = self.num * rhs.den + rhs.num * self.den
            local den = self.den * rhs.den
            return new_rational(num, den)
        end,
        -- equality
        __eq = function(self, rhs)
            return self.num == rhs.num and self.den == rhs.den
        end
    }
    
    -- a function for the creation of new rationals
    function new_rational(num, den)
        local self = { num = num, den = den }
        setmetatable(self, meta)
    
        return self
    end
    
    local r1 = new_rational(1, 2)
    print(r1) -- 1/2
    
    local r2 = new_rational(1, 3)
    print(r1 + r2) -- 5/6
    
    local r3 = new_rational(1, 2)
    print(r1 == r3) -- true
    -- this would be the behaviour if we hadn't implemented the __eq metamethod.
    -- this compares the actual tables, which are different
    print(rawequal(r1, r3)) -- false



## Make tables callable
There is a metamethod called `__call`, which defines the bevahiour of the object upon being used as a function, e.g. `object()`. This can be used to create function objects:

    -- create the metatable with a __call metamethod
    local meta = {
        __call = function(self)
            self.i = self.i + 1
        end,
        -- to view the results
        __tostring = function(self)
            return tostring(self.i)
        end
    }
    
    function new_counter(start)
        local self = { i = start }
        setmetatable(self, meta)
        return self
    end
    
    -- create a counter
    local c = new_counter(1)
    print(c) --> 1
    -- call -> count up
    c()
    print(c) --> 2

The metamethod is called with the corresponding object, all remaining arguments are passed to the function after that:

    local meta = {
        __call = function(self, ...)
            print(self.prepend, ...)
        end
    }
    
    local self = { prepend = "printer:" }
    setmetatable(self, meta)
    
    self("foo", "bar", "baz")



## Raw table access
Sometimes, you don't want to trigger metamethods, but really write or read exactly the given key, without some clever functions wrapped around the access. For this, lua provides you with raw table access methods:

    -- first, set up a metatable that allows no read/write access
    local meta = {
        __index = function(object, index)
            -- raise an error
            error(string.format("the key '%s' is not present in object '%s'", index, object))
        end,
        __newindex = function(object, index, value)
            -- raise an error, this prevents any write access to the table
            error(string.format("you are not allowed to write the object '%s'", object))
        end
    }
    
    local t = { foo = "bar" }
    setmetatable(t, meta)
    
    -- both lines raise an error:
    --print(t[1])
    --t[1] = 42

    -- we can now circumvent this problem by using raw access:
    print(rawget(t, 1)) -- prints nil
    rawset(t, 1, 42) -- ok

    -- since the key 1 is now valid, we can use it in a normal manner:
    print(t[1])

With this, we can now rewrite ower former `__newindex` method to actually write the value to the table:

    meta.__newindex = function(object, index, value)
        print(string.format("writing the value '%s' to the object '%s' at the key '%s'",
                             value, object, index))
        rawset(object, index, value)
    end

