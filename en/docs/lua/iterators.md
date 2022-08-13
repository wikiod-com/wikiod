---
title: "Iterators"
slug: "iterators"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

## Stateful Iterators
Stateful iterators carry some additional information about the current state of the iterator.

## Using Tables
The addition state can be packed into the [generic for loop's](http://www.lua.org/manual/5.3/manual.html#3.3.5) invariant state.

<!-- language: lang-lua -->

      local function chars_iter(t, i)
        local i = i + 1
        if i <= t.len then
          return i, t.s:sub(i, i)
        end
      end
    
      local function chars(s)
        -- the iterators state
        local t = {
          s = s,    -- the subject
          len = #s  -- cached length
        }
        return chars_iter, t, 0
      end
    
      for i, c in chars 'abcde' do
        print(i, c) --> 1 a, 2 b, 3 c, 4 d, 5 e
      end

## Using Closures
Additional state can be wrapped within a function closure. Since the state is fully contained in the scope of the closure the invariant state and control variable are not needed.

<!-- language: lang-lua -->

      local function chars(s)
        local i, len = 0, #s
        return function() -- iterator function
          i = i + 1
          if i <= len then
            return i, s:sub(i, i)
          end
        end
      end
    
      for i, c in chars 'abcde' do
        print(i, c) --> 1 a, 2 b, 3 c, 4 d, 5 e
      end

## Using Coroutines
Additional state can be contained within a coroutine, again the invariant state and control variable are not needed.

<!-- language: lang-lua -->

      local function chars(s)
        return coroutine.wrap(function()
          for i = 1, #s do
            coroutine.yield(s:sub(i, i))
          end
        end)
      end
    
      for c in chars 'abcde' do
        print(c) --> a, b, c, d, e
      end


## Generic For Loop
Iterators utilize a form of the `for` loop known as the [generic for loop](http://www.lua.org/manual/5.3/manual.html#3.3.5).

The generic form of the `for` loop uses three parameters:

1. An __iterator function__ that gets called when the next value is needed. It receives both the invariant state and control variable as parameters. Returning `nil` signals termination.
2. The __invariant state__ is a value that doesn't change during the iteration. It is typically the subject of the iterator, such as a table, string, or userdata.
3. The __control variable__ represents an initial value for iteration.

We can write a `for` loop to iterate all key-value pairs in a table using the [next](http://www.lua.org/manual/5.3/manual.html#pdf-next) function.

<!-- language: lang-lua -->

    local t = {a=1, b=2, c=3, d=4, e=5}

    -- next is the iterator function
    -- t is the invariant state
    -- nil is the control variable (calling next with a nil gets the first key)
    for key, value in next, t, nil do
      -- key is the new value for the control variable
      print(key, value) 
      -- Lua calls: next(t, key)  
    end


## Stateless Iterators
Both [pairs](http://www.lua.org/manual/5.3/manual.html#pdf-pairs) and [ipairs](http://www.lua.org/manual/5.3/manual.html#pdf-ipairs) represent stateless iterators. A stateless iterator uses only the [generic for loop's](http://www.lua.org/manual/5.3/manual.html#3.3.5) control variable and invariant state to compute the iteration value.

## Pairs Iterator
We can implement the stateless `pairs` iterator using the `next` function.

    -- generator function which initializes the generic for loop
    local function pairs(t)
      -- next is the iterator function
      -- t is the invariant state
      -- control variable is nil
      return next, t, nil
    end

## Ipairs Iterator
We can implement the stateless `ipairs` iterator in two separate functions.

<!-- language: lang-lua -->

    -- function which performs the actual iteration
    local function ipairs_iter(t, i)
      local i = i + 1  -- next index in the sequence (i is the control variable)
      local v = t[i]   -- next value (t is the invariant state)
      if v ~= nil then
        return i, v    -- index, value
      end
      return nil       -- no more values (termination)
    end

    -- generator function which initializes the generic for loop
    local function ipairs(t)
      -- ipairs_iter is the iterator function
      -- t is the invariant state (table to be iterated)
      -- 0 is the control variable (first index)
      return ipairs_iter, t, 0
    end

## Character Iterator
We can create new stateless iterators by fulfilling the contract of the generic `for` loop.

<!-- language: lang-lua -->

    -- function which performs the actual iteration
    local function chars_iter(s, i)
      if i < #s then
        i = i + 1
        return i, s:sub(i, i)
      end
    end

    -- generator function which initializes the generic for loop
    local function chars(s)
      return chars_iter, s, 0
    end

    -- used like pairs and ipairs
    for i, c in chars 'abcde' do
        print(i, c) --> 1 a, 2 b, 3 c, 4 f, 5 e
    end

## Prime Numbers Iterator
This is one more simple example of a stateless iterator.

<!-- language: lang-lua -->

    -- prime numbers iterator
    local incr = {4, 1, 2, 0, 2}
    function primes(s, p, d)
       s, p, d = s or math.huge, p and p + incr[p % 6] or 2, 1
       while p <= s do
          repeat
             d = d + incr[d % 6]
             if d*d > p then return p end
          until p % d == 0
          p, d = p + incr[p % 6], 1
       end
    end
    
    -- print all prime numbers <= 100
    for p in primes, 100 do  -- passing in the iterator (do not call the iterator here)
       print(p)  -->  2  3  5  7  11 ... 97
    end
    
    -- print all primes in endless loop
    for p in primes do  -- please note: "in primes", not "in primes()"
       print(p)
    end


## Standard Iterators
The Lua standard library provides two iterator functions that can be used with a `for` loop to traverse key-value pairs within tables.

To iterate over a sequence table we can use the library function [ipairs](http://www.lua.org/manual/5.3/manual.html#pdf-ipairs).

<!-- language: lang-lua -->

    for index, value in ipairs {'a', 'b', 'c', 'd', 'e'} do
      print(index, value)  --> 1 a, 2 b, 3 c, 4 d, 5 e
    end

To iterator over all keys and values in any table we can use the library function [pairs](http://www.lua.org/manual/5.3/manual.html#pdf-pairs).

<!-- language: lang-lua -->

    for key, value in pairs {a=1, b=2, c=3, d=4, e=5} do
      print(key, value)  --> e 5, c 3, a 1, b 2, d 4  (order not specified)
    end


