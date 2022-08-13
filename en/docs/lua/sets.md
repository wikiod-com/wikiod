---
title: "Sets"
slug: "sets"
draft: false
images: []
weight: 9865
type: docs
toc: true
---

## Using a Table as a Set

## Create a set

     local set = {} -- empty set

Create a set with elements by setting their value to `true`:

     local set = {pear=true, plum=true}

     -- or initialize by adding the value of a variable:
     local fruit = 'orange'
     local other_set = {[fruit] = true} -- adds 'orange'


## Add a member to the set

add a member by setting its value to `true`

      set.peach = true
      set.apple = true
      -- alternatively
      set['banana'] = true
      set['strawberry'] = true

## Remove a member from the set

      set.apple = nil
  
Using `nil` instead of `false` to remove 'apple' from the table is preferable because it will make iterating elements simpler. `nil` deletes the entry from the table while setting to `false` changes its value.

## Membership Test

      if set.strawberry then
          print "We've got strawberries"
      end

## Iterate over elements in a set

     for element in pairs(set) do
         print(element)
     end

## Search for an item in a list
There's no built in way to search a list for a particular item. However [Programming in Lua](http://www.lua.org/pil/11.5.html) shows how you might build a set that can help:

    function Set (list)
      local set = {}
      for _, l in ipairs(list) do set[l] = true end
      return set
    end

Then you can put your list in the Set and test for membership:

    local items = Set { "apple", "orange", "pear", "banana" }

    if items["orange"] then
      -- do something
    end

