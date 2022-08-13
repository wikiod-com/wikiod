---
title: "Error Handling"
slug: "error-handling"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## Handling errors in Lua
Assuming we have the following function:

    function foo(tab)
      return tab.a
    end -- Script execution errors out w/ a stacktrace when tab is not a table

Let's improve it a bit

    function foo(tab)
      if type(tab) ~= "table" then
        error("Argument 1 is not a table!", 2)
      end
      return tab.a
    end -- This gives us more information, but script will still error out

If we don't want a function to crash a program even in case of an error, it is standard in lua to do the following:

    function foo(tab)
        if type(tab) ~= "table" then return nil, "Argument 1 is not a table!" end
        return tab.a
    end -- This never crashes the program, but simply returns nil and an error message

Now we have a function that behaves like that, we can do things like this:

    if foo(20) then print(foo(20)) end -- prints nothing
    result, error = foo(20)
    if result then print(result) else log(error) end

And if we DO want the program to crash if something goes wrong, we can still do this:

    result, error = foo(20)
    if not result then error(error) end

Fortunately we don't even have to write all that every time; lua has a function that does exactly this

    result = assert(foo(20))

## Using pcall
`pcall` stands for "protected call". It is used to add error handling to functions. `pcall` works similar as `try-catch` in other languages. The advantage of `pcall` is that the whole execution of the script is not being interrupted if errors occur in functions called with `pcall`. If an error inside a function called with `pcall` occurs an error is thrown and the rest of the code continues execution.


----------


**Syntax:**    

    pcall( f , arg1,···)


----------


**Return Values:**

Returns two values

1. status (boolean)
 - Returns **true** if the function was executed with no errors. 
 - Returns **false** if an error occured inside the function.
2. return value of the function **or** error message if an error occurred inside the function block.


----------
pcall may be used for various cases, however a common one is to catch errors from the function which has been given to your function. For instance, lets say we have this function:

    local function executeFunction(funcArg, times) then
        for i = 1, times do
            local ran, errorMsg = pcall( funcArg )
            if not ran then
                error("Function errored on run " .. tostring(i) .. "\n" .. errorMsg)
            end
        end
    end

When the given function errors on run 3, the error message will be clear to the user that it is not coming from your function, but from the function which was given to our function. Also, with this in mind a fancy BSoD can be made notifying the user. However, that is up to the application which implements this function, as an API most likely won't be doing that.


**Example A** - *Execution without pcall*

    function square(a)
      return a * "a"    --This will stop the execution of the code and throws an error, because of the attempt to perform arithmetic on a string value
    end
    
    square(10);
    
    print ("Hello World")    -- This is not being executed because the script was interrupted due to the error


**Example B** - *Execution with pcall*

    function square(a)
      return a * "a"
    end
    
    local status, retval = pcall(square,10);
    
    print ("Status: ", status)        -- will print "false" because an error was thrown.
    print ("Return Value: ", retval)  -- will print "input:2: attempt to perform arithmetic on a string value"
    print ("Hello World")    -- Prints "Hello World"

**Example** - *Execution of flawless code*

    function square(a)
      return a * a
    end
    
    local status, retval = pcall(square,10);
    
    print ("Status: ", status)        -- will print "true" because no errors were thrown 
    print ("Return Value: ", retval)  -- will print "100"
    print ("Hello World")    -- Prints "Hello World"




