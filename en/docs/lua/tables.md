---
title: "Tables"
slug: "tables"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

## Syntax
 - ipairs(numeric_table) -- Lua table with numeric indices iterator
 - pairs(input_table) -- generic Lua table iterator
 - key, value = next(input_table, input_key) -- Lua table value selector
 - table.insert(input_table, [position], value) -- insert specified value into the input table
 - removed_value = table.remove(input_table, [position]) -- pop last or remove  value specified by position

Tables are the only built-in data structure available in Lua. This is either elegant simplicity or confusing, depending on how you look at it.

A Lua table is a collection of key-value pairs where the keys are unique and neither the key nor the value is `nil`. As such, a Lua table can resemble a dictionary, hashmap or associative array from other languages. Many structural patterns can be built with tables: stacks, queues, sets, lists, graphs, etc. Finally, tables can be used to build _classes_ in Lua and to create a _module_ system.

Lua does not enforce any particular rules on how tables are used. The items contained in a table can be a mixture of Lua types. So, for example, one table could contain strings, functions, booleans, numbers, _and even other tables_ as values or keys.

A Lua table with consecutive positive integer keys beginning with 1 is said to have a sequence. The key-value pairs with positive integer keys are the elements of the sequence. Other languages call this a 1-based array. Certain standard operations and functions only work on the sequence of a table and some have non-deterministic behavior when applied to a table without a sequence. 

Setting a value in a table to `nil` removes it from the table. Iterators would no longer see the related key. When coding for a table with a sequence, it is important to avoid breaking the sequence; Only remove the last element or use a function, like the standard `table.remove`, that shifts elements down to close the gap. 


## Creating tables
Creating an empty table is as simple as this:

    local empty_table = {}

You can also create a table in the form of a simple array:

    local numeric_table = {
        "Eve", "Jim", "Peter"
    }
    -- numeric_table[1] is automatically "Eve", numeric_table[2] is "Jim", etc.

Bear in mind that by default, table indexing starts at 1.

Also possible is creating a table with associative elements:

    local conf_table = {
        hostname = "localhost",
        port     = 22,
        flags    = "-Wall -Wextra"
        clients  = {                -- nested table
            "Eve", "Jim", "Peter"
        }
    }

The usage above is syntax sugar for what's below. The keys in this instance are of the type, string. The above syntax was added to make tables appear as records. This record-style syntax is paralleled by the syntax for indexing tables with string keys, as seen in 'basic usage' tutorial.

As explained in the remarks section, the record-style syntax doesn't work for every possible key. Additionally a key can be any value of any type, and the previous examples only covered strings and sequential numbers. In other cases you'll need to use the explicit syntax:

    local unique_key = {}
    local ops_table = {
        [unique_key] = "I'm unique!"
        ["^"]  = "power",
        [true] = true
    }


## Iterating tables
The Lua standard library provides a `pairs` function which iterates over the keys and values of a table. When iterating with `pairs` there is no specified order for traversal, *even if the keys of the table are numeric*.

    for key, value in pairs(input_table) do
        print(key, " -- ", value)
    end

For tables using **numeric keys**, Lua provides an `ipairs` function. The `ipairs` function will always iterate from `table[1]`, `table[2]`, etc. until the first `nil` value is found.

    for index, value in ipairs(numeric_table) do
        print(index, ". ", value)
    end

Be warned that iteration using `ipairs()` will not work as you might want on few occasions:

 * `input_table` has "holes" in it. (See the section on "Avoiding gaps in tables used as arrays" for more information.) For example:
 
       table_with_holes = {[1] = "value_1", [3] = "value_3"}
 * keys weren't all numeric. For example:

       mixed_table = {[1] = "value_1", ["not_numeric_index"] = "value_2"}

Of course, the following also works for a table that is a proper sequence:

    for i = 1, #numeric_table do
        print(i, ". ", numeric_table[i])
    end

Iterating a numeric table in reverse order is easy:

    for i = #numeric_table, 1, -1 do
        print(i, ". ", numeric_table[i])
    end

A final way to iterate over tables is to use the `next` selector in a [generic `for` loop](https://www.lua.org/manual/5.3/manual.html#3.3.5). Like `pairs` there is no specified order for traversal. (The `pairs` method uses `next` internally. So using `next` is essentially a more manual version of `pairs`. See [`pairs` in Lua's reference manual](https://www.lua.org/manual/5.3/manual.html#pdf-pairs) and [`next` in Lua's reference manual](https://www.lua.org/manual/5.3/manual.html#pdf-next) for more details.)

    for key, value in next, input_table do
        print(key, value)
    end



## Basic Usage
Basic table usage includes accessing and assigning table elements, adding table content, and removing table content. These examples assume you know how to create tables.

**Accessing Elements**

Given the following table,

    local example_table = {"Nausea", "Heartburn", "Indigestion", "Upset Stomach",
                           "Diarrhea", cure = "Pepto Bismol"}
    
One can index the sequential part of the table by using the index syntax, the argument to the index syntax being the key of the desired key-value pair. As explained in the creation tutorial, most of the declaration syntax is syntactic sugar for declaring key-value pairs. Sequentially included elements, like the first five values in `example_table`, use increasing integer values as keys; the record syntax uses the name of the field as a string.

    print(example_table[2])        --> Heartburn
    print(example_table["cure"])   --> Pepto Bismol

For string keys there is syntax sugar to parallel the record-style syntax for string keys in table creation. The following two lines are equivalent.

    print(example_table.cure)      --> Pepto Bismol
    print(example_table["cure"])   --> Pepto Bismol

You can access tables using keys that you haven't used before, that is not an error as it in other languages. Doing so returns the default value `nil`.

**Assigning Elements**

You can modify existing table elements by assigning to a table using the index syntax. Additionally, the record-style indexing syntax is available for setting values as well

    example_table.cure = "Lots of water, the toilet, and time"
    print(example_table.cure)    --> Lots of water, the toilet, and time

    example_table[2] = "Constipation"
    print(example_table[2])      --> Constipation

You can also add new elements to an existing table using assignment.

    example_table.copyright_holder = "Procter & Gamble"
    example_table[100] = "Emergency source of water"

*Special Remark:* Some strings are not supported with the record-syntax. See the remarks section for details.

**Removing Elements**

As stated previously, the default value for a key with no assigned value is `nil`. Removing an element from a table is as simple as resetting the value of a key back to the default value.

    example_table[100] = "Face Mask"

The elements is now indistinguishable from an unset element.

**Table Length**

Tables are simply associative arrays (see remarks), but when contiguous integer keys are used starting from 1 the table is said to have a *sequence*.

Finding the length of the sequence part of a table is done using `#`:

    local example_table = {'a', 'l', 'p', 'h', 'a', 'b', 'e', 't'}
    print(#example_table)    --> 8

You can use the length operation to easily append items to a sequence table.

    example_table[#example_table+1] = 'a'
    print(#example_table)    --> 9

In the above example, the previous value of `#example_table` is `8`, adding `1` gives you the next valid integer key in the sequence, `9`, so... `example_table[9] = 'a'`. This works for any length of table.

*Special Remark:* Using integer keys that aren't contiguous and starting from 1 breaks the sequence making the table into a *sparse table*. The result of the length operation is undefined in that case. See the remarks section.

**Using Table Library Functions to Add/Remove Elements**

Another way to add elements to a table is the `table.insert()` function. The insert function only works on sequence tables. There are two ways to call the function. The first example shows the first usage, where one specifies the index to insert the element (the second argument). This pushes all elements from the given index to `#table` up one position. The second example shows the other usage of `table.insert()`, where the index isn't specified and the given value is appended to the end of the table (index `#table + 1`). 

    local t = {"a", "b", "d", "e"}
    table.insert(t, 3, "c")        --> t = {"a", "b", "c", "d", "e"}
    
    t = {"a", "b", "c", "d"}
    table.insert(t, "e")           --> t = {"a", "b", "c", "d", "e"}

To parallel `table.insert()` for removing elements is `table.remove()`. Similarly it has two calling semantics: one for removing elements at a given position, and another for removing from the end of the sequence. When removing from the middle of a sequence, all following elements are shifted down one index.

    local t = {"a", "b", "c", "d", "e"}
    local r = table.remove(t, 3)       --> t = {"a", "b", "d", "e"}, r = "c"

    t = {"a", "b", "c", "d", "e"}
    r = table.remove(t)                --> t = {"a", "b", "c", "d"}, r = "e"

These two functions mutate the given table. As you might be able to tell the second method of calling `table.insert()` and `table.remove()` provides stack semantics to tables. Leveraging that, you can write code like the example below.

    function shuffle(t)
        for i = 0, #t-1 do
            table.insert(t, table.remove(t, math.random(#t-i)))
        end
    end

It implements the Fisher-Yates Shuffle, perhaps inefficiently. It uses the `table.insert()` to append the randomly extracted element onto the end of same table, and the `table.remove()` to randomly extract an element from the remaining unshuffled portion of the table.

## Avoiding gaps in tables used as arrays
Defining our terms
--

By *array* here we mean a Lua table used as a sequence. For example:

    -- Create a table to store the types of pets we like.
    local pets = {"dogs", "cats", "birds"}

We're using this table as a sequence: a group of items keyed by integers. Many languages call this an array, and so will we. But strictly speaking, there's no such thing as an array in Lua. There are just tables, some of which are array-like, some of which are hash-like (or dictionary-like, if you prefer), and some of which are mixed.

An important point about our `pets` array is that is has no gaps. The first item, `pets[1]`, is the string "dogs", the second item, `pets[2]`, is the string "cats", and the last item, `pets[3]`, is "birds". Lua's standard library and most modules written for Lua assume 1 as the first index for sequences. A gapless array therefore has items from `1..n` without missing any numbers in the sequence. (In the limiting case, `n = 1`, and the array has only one item in it.)

Lua provides the built-in function `ipairs` to iterate over such tables.

    -- Iterate over our pet types.
    for idx, pet in ipairs(pets) do
      print("Item at position " .. idx .. " is " .. pet .. ".")
    end

This would print "Item at position 1 is dogs.", "Item at position 2 is cats.", "Item at position 3 is birds."

But what happens if we do the following?

    local pets = {"dogs", "cats", "birds"}
    pets[12] = "goldfish"
    for idx, pet in ipairs(pets) do
      print("Item at position " .. idx .. " is " .. pet .. ".")
    end

An array such as this second example is a sparse array. There are gaps in the sequence. This array looks like this:

    {"dogs", "cats", "birds", nil, nil, nil, nil, nil, nil, nil, nil, "goldfish"}
    -- 1        2       3      4    5    6    7    8    9    10   11       12     

The nil values do not take up any aditional memory; internally lua only saves the values `[1] = "dogs"`, `[2] = "cats"`, `[3] = "birtds"` and `[12] = "goldfish"`

To answer the immediate question, `ipairs` will stop after birds; "goldfish" at `pets[12]` will never be reached unless we adjust our code. This is because `ipairs` iterates from `1..n-1` where `n` is the position of the first `nil` found. Lua defines `table[length-of-table + 1]` to be `nil`. So in a proper sequence, iteration stops when Lua tries to get, say, the fourth item in a three-item array.

When?
--

The two most common places for issues to arise with sparse arrays are (i) when trying to determine the length of the array and (ii) when trying to iterate over the array. In particular:

* When using the `#` length operator since the length operator stops counting at the first `nil` found.
* When using the `ipairs()` function since as mentioned above it stops iterating at the first `nil` found.
* When using the `table.unpack()` function since this method stops unpacking at the first `nil` found.
* When using other functions that (directly or indirectly) access any of the above. 

To avoid this problem, it is important to write your code so that if you expect a table to be an array, you don't introduce gaps. Gaps can be introduced in several ways:

* If you add something to an array at the wrong position.
* If you insert a `nil` value into an array.
* If you remove values from an array.

You might think, "But I would never do any of those things." Well, not intentionally, but here's a concrete example of how things could go wrong. Imagine that you want to write a filter method for Lua like Ruby's `select` and Perl's `grep`. The method will accept a test function and an array. It iterates over the array, calling the test method on each item in turn. If the item passes, then that item gets added to a results array which is returned at the end of the method. The following is a buggy implementation:

    local filter = function (fun, t)
      local res = {}
      for idx, item in ipairs(t) do
        if fun(item) then
          res[idx] = item
        end
      end

      return res
    end

The problem is that when the function returns `false`, we skip a number in the sequence. Imagine calling `filter(isodd, {1,2,3,4,5,6,7,8,9,10})`: there will be gaps in the returned table every time there's an even number in the array passed to `filter`.

Here's a fixed implementation:

    local filter = function (fun, t)
      local res = {}
      for _, item in ipairs(t) do
        if fun(item) then
          res[#res + 1] = item
        end
      end
    
      return res
    end

Tips
--

1. Use standard functions: `table.insert(<table>, <value>)` always appends to the end of the array.  `table[#table + 1] = value` is a short hand for this. `table.remove(<table>, <index>)` will move all following values back to fill the gap (which can also make it slow).
2. Check for `nil` values **before** inserting, avoiding things like `table.pack(function_call())`, which might sneak `nil` values into our table.
3. Check for `nil` values **after** inserting, and if necessary filling the gap by shifting all consecutive values.
4. If possible, use placeholder values. For example, change `nil` for `0` or some other placeholder value.
5. If leaving gaps is unavoidable, this should be propperly documented (commented).
6. Write a `__len()` metamethod and use the `#` operator.

Example for 6.:

    tab = {"john", "sansa", "daenerys", [10] = "the imp"}
    print(#tab) --> prints 3
    setmetatable(tab, {__len = function() return 10 end})
    -- __len needs to be a function, otherwise it could just be 10
    print(#tab) --> prints 10
    for i=1, #tab do print(i, tab[i]) end
    --> prints:
    -- 1 john
    -- 2 sansa
    -- 3 daenerys
    -- 4 nil
    -- ...
    -- 10 the imp

    for key, value in ipairs(tab) do print(key, value) end
    --> this only prints '1 john \n 2 sansa \n 3 daenerys'

Another alternative is to use the `pairs()` function and filter out the non-integer indices:

    for key in pairs(tab) do
        if type(key) == "number" then
            print(key, tab[key]
        end
    end
    -- note: this does not remove float indices
    -- does not iterate in order

