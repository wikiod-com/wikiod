---
title: "Introduction to Lua C API"
slug: "introduction-to-lua-c-api"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
 - lua_State *L = lua_open(); // Create a new VM state; Lua 5.0
 - lua_State *L = luaL_newstate(); // Create a new VM state; Lua 5.1+
 - int luaL_dofile(lua_State *L, const char **filename*); // Run a lua script with the given *filename* using the specified lua_State
 - void luaL_openlibs(lua_State *L); // Load all standard libraries into the specified lua_State
 - void lua_close(lua_State *L); // Close VM state and release any resources inside
 - void lua_call(lua_State *L, int nargs, int nresults); // Call the luavalue at index -(nargs + 1)


Lua as well provides a proper C API to it's Virtual Machine. In contrary to VM itself, C API interface is stack based. So, most of the functions intended to be used with data is either adding some stuff on-top of virtual stack, or removing from it. Also, all the API calls must be used carefully within stack and it's limitations.

In general, anything available on Lua language can be done using it's C API. Also, there is some addition functionality like direct access to internal registry, change behavior of standard memory allocator or garbage collector.

You can compile provided Lua C API examples by executing following on Your terminal:

    $ gcc -Wall ./example.c -llua -ldl -lm

## Calling  Lua functions
    #include <stdlib.h>

    #include <lauxlib.h>
    #include <lua.h>
    #include <lualib.h>

    int main(void)
    {
        lua_State *lvm_hnd = lua_open();
        luaL_openlibs(lvm_hnd);

        /* Load a standard Lua function from global table: */
        lua_getglobal(lvm_hnd, "print");

        /* Push an argument onto Lua C API stack: */
        lua_pushstring(lvm_hnd, "Hello C API!");

        /* Call Lua function with 1 argument and 0 results: */
        lua_call(lvm_hnd, 1, 0);

        lua_close(lvm_hnd);

        return EXIT_SUCCESS;
     }

In the example above we're doing these things:
 - creating and setting up Lua VM as shown on the first example
 - getting and pushing a Lua function from global Lua table onto virtual stack
 - pushing string `"Hello C API"` as an input argument onto the virtual stack
 - instructing VM to call a function with one argument which is already on the stack
 - closing and cleaning up

**NOTE:**

Bare in mind, that `lua_call()` pops the function and it's arguments from the stack leaving only the result.

Also, it would be safer using Lua protected call - `lua_pcall()` instead. 

## Creating Lua Virtual Machine
    #include <lua.h>
    #include <lauxlib.h>
    #include <lualib.h>

    int main(void)
    {
<!-- if version [gte 5.1] -->
      /* Start by creating a new VM state */
      lua_State *L = luaL_newstate();

      /* Load standard Lua libraries: */
      luaL_openlibs(L);
<!-- end version if -->
<!-- if version [lt 5.1] -->
      /* For older version of Lua use lua_open instead */
      lua_State *L = lua_open();

      /* Load standard libraries*/
      luaopen_base(L);
      luaopen_io(L);
      luaopen_math(L);
      luaopen_string(L);
      luaopen_table(L);
<!-- end version if -->

        /* do stuff with Lua VM. In this case just load and execute a file: */
        luaL_dofile(L, "some_input_file.lua");
    
        /* done? Close it then and exit. */
        lua_close(L);

        return EXIT_SUCCESS;
    }


## Embedded Lua Interpreter with Custom API and Lua Customization
Demonstrate how to embed a lua interpreter in C code, expose a C-defined function to Lua script, evaluate a Lua script, call a C-defined function from Lua, and call a Lua-defined function from C (the host).

In this example, we want the mood to be set by a Lua script. Here is mood.lua:

    -- Get version information from host
    major, minor, build = hostgetversion()
    print( "The host version is ", major, minor, build)
    print("The Lua interpreter version is ", _VERSION)

    -- Define a function for host to call
    function mood( b )

        -- return a mood conditional on parameter
        if (b and major > 0) then
            return 'mood-happy'
        elseif (major == 0) then
            return 'mood-confused'
        else
            return 'mood-sad'
        end
    end

Notice, `mood()` is not called in the script. It is just defined for the host application to call. Also notice that the script calls a function called `hostgetversion()` that is not defined in the script.

Next, we define a host application that uses 'mood.lua'. Here is the 'hostlua.c':

    #include <stdio.h>
    #include <lua.h>
    #include <lualib.h>
    #include <lauxlib.h>

    /* 
     * define a function that returns version information to lua scripts
     */
    static int hostgetversion(lua_State *l)
    {
        /* Push the return values */
        lua_pushnumber(l, 0);
        lua_pushnumber(l, 99);
        lua_pushnumber(l, 32);

        /* Return the count of return values */
        return 3;
    }

    int main (void)
    {
        lua_State *l = luaL_newstate();
        luaL_openlibs(l);

        /* register host API for script */
        lua_register(l, "hostgetversion", hostgetversion);

        /* load script */
        luaL_dofile(l, "mood.lua");

        /* call mood() provided by script */
        lua_getglobal(l, "mood");
        lua_pushboolean(l, 1);
        lua_call(l, 1, 1);

        /* print the mood */
        printf("The mood is %s\n", lua_tostring(l, -1));
        lua_pop(l, 1);

        lua_close(l);
        return 0;
    }

And here is the output:

    The host version is     0    99    32
    Lua interpreter version is     Lua 5.2
    The mood is mood-confused

Even after we have compile 'hostlua.c', we are still free to modify 'mood.lua' to change the output of our program!

## Table manipulation
In order to access or alter an index on a table, you need to somehow place the table into the stack.<br>
Let's assume, for this examples that your table is a global variable named tbl.

Getting the content at a particular index:
--

    int getkey_index(lua_State *L)
    {
      lua_getglobal(L, "tbl");    // this put the table in the stack
      lua_pushstring(L, "index"); // push the key to access
      lua_gettable(L, -2);        // retrieve the corresponding value; eg. tbl["index"]

      return 1;                   // return value to caller
    }

 As we have seen, all you have to do is to push the table into the stack, push the index and call lua_gettable. the -2 argument means that the table is the second element from the top of the stack.<br>
lua_gettable triggers metamethods. If you do not want to trigger metamethods, use lua_rawget instead. It uses the same arguments.

Setting the content at a particular index:
--

    int setkey_index(lua_State *L)
    {
      // setup the stack
      lua_getglobal(L, "tbl");
      lua_pushstring(L, "index");
      lua_pushstring(L, "value");
      // finally assign the value to table; eg. tbl.index = "value"
      lua_settable(L, -3);

      return 0;
    }

The same drill as getting the content. You have to push the stack, push the index and then push the value into the stack. after that, you call lua_settable. the -3 argument is the position of the table in the stack. To avoid triggering metamethods, use lua_rawset instead of lua_settable. It uses the same arguments.

Transferring the content from a table to another:
--

    int copy_tableindex(lua_State *L)
    {
        lua_getglobal(L, "tbl1"); // (tbl1)
        lua_getglobal(L, "tbl2");// (tbl1)(tbl2)
        lua_pushstring(L, "index1");// (tbl1)(tbl2)("index1")
        lua_gettable(L, -3);// (tbl1)(tbl2)(tbl1.index1)
        lua_pushstring(L, "index2");// (tbl1)(tbl2)(tbl1.index1)("index2")
        lua_pushvalue(L, -2); // (tbl1)(tbl2)(tbl1.index1)("index2")(tbl1.index1)
        lua_settable(L, -4);// (tbl1)(tbl2)(tbl1.index1)
        lua_pop(L, 1);

        return 0;
    }

Now we are putting together all we learned here. I put the stack content on the comments so you do not get lost.

We put both tables into the stack, push the index of table 1 into the stack, and get the value at `tbl1.index1`. Note the `-3` argument on gettable. I am looking at the first table (third from the top) and not the second. Then we push the index of the second table, copy the `tbl1.index1` to the top of the stack and then call `lua_settable`, on the 4th item from the top.

For housecleaning sake, I have purged the top element, so only the two tables remains at the stack.


