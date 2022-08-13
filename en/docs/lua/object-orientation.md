---
title: "Object-Orientation"
slug: "object-orientation"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

Lua itself offers no class system. It is, however possible to implement classes and objects as tables with just a few tricks.

## Syntax
* `function <class>.new() return setmetatable({}, {__index=<class>}) end`

## Simple Object Orientation
Here's a basic example of how to do a very simple class system

    Class = {}
    local __instance = {__index=Class} -- Metatable for instances
    function Class.new()
        local instance = {}
        setmetatable(instance, __instance)
        return instance
    -- equivalent to: return setmetatable({}, __instance)
    end

To add variables and/or methods, just add them to the class. Both can be overridden for every instance.

    Class.x = 0
    Class.y = 0
    Class:getPosition()
        return {self.x, self.y}
    end

And to create an instance of the class:

    object = Class.new()

or

    setmetatable(Class, {__call = Class.new}
        -- Allow the class itself to be called like a function
    object = Class()

And to use it:

    object.x = 20
    -- This adds the variable x to the object without changing the x of
    -- the class or any other instance. Now that the object has an x, it
    -- will override the x that is inherited from the class
    print(object.x)
    -- This prints 20 as one would expect.
    print(object.y)
    -- Object has no member y, therefore the metatable redirects to the
    -- class table, which has y=0; therefore this prints 0
    object:getPosition() -- returns {20, 0}

## Changing metamethods of an object
Having

    local Class = {}
    Class.__meta = {__index=Class}
    function Class.new() return setmetatable({}, Class.__meta)

Assuming we want to change the behavior of a single instance `object = Class.new()` using a metatable,

there are a few mistakes to avoid:

    setmetatable(object, {__call = table.concat}) -- WRONG

This exchanges the old metatable with the new one, therefore breaking the class inheritance

    getmetatable(object).__call = table.concat -- WRONG AGAIN

Keep in mind that table "values" are only reference; there is, in fact, only one actual table for all the instances of an object unless the constructor is defined as in <sup>1</sup>, so by doing this we modify the behavior of *all* instances of the class.

---

One correct way of doing this:

Without changing the class:

    setmetatable(
        object,
        setmetatable(
            {__call=table.concat},
            {__index=getmetatable(object)}
        )
    )

How does this work? - We create a new metatable as in mistake #1, but instead of leaving it empty, we create a soft copy to the original metatable. One could say the new metatable "inherits" from the original one as if it was a class instance itself. We can now override values of the original metatable without modifying them.

Changing the class:

1st (recommended):

    local __instance_meta = {__index = Class.__meta}
    -- metatable for the metatable
    -- As you can see, lua can get very meta very fast
    function Class.new()
        return setmetatable({}, setmetatable({}, __instance_meta))
    end

2nd (less recommended): see <sup>1</sup>

---

<sup>1</sup> `function Class.new() return setmetatable({}, {__index=Class}) end`
    

