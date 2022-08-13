---
title: "Pattern matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9835
type: docs
toc: true
---

## Syntax
* string.find(str, pattern [, init [, plain]]) -- Returns start and end index of match in str
* string.match(str, pattern [, index]) -- Matches a pattern once (starting at index)

* string.gmatch(str, pattern) -- Returns a function that iterates through all matches in str

* string.gsub(str, pattern, repl [, n]) -- Replaces substrings (up to a max of n times)

* `.` represents all characters
* `%a` represents all letters
* `%l` represents all lowercase letters
* `%u` represents all uppercase letters
* `%d` represents all digits
* `%x` represents all hexadecimal digits
* `%s` represents all whitespace characters
* `%p` represents all punctuation characters
* `%g` represents all *printable* characters except space
* `%c` represents all control characters
* `[set]` represents the class which is the union of all characters in set.
* `[^set]` represents the complement of set
* `*` greedy match 0 or more occurrences of previous character class
* `+` greedy match 1 or more occurrences of previous character class
* `-` lazy match 0 or more occurrences of previous character class
* `?` match exactly 0 or 1 occurrence of previous character class


Throughout some examples, the notation `(<string literal>):function <string literal>` is used, which is equivalent to `string.function(<string literal>, <string literal>)` because all strings have a metatable with the `__index` field set to the `string` table.

## Lua pattern matching
Instead of using regex, the Lua string library has a special set of characters used in syntax matches. Both can be very similar, but Lua pattern matching is more limited and has a different syntax. For instance, the character sequence **`%a`** matches any letter, while its upper-case version represents *all non-letters characters*, all characters classes (a character sequence that, as a pattern, can match a set of items) are listed below.

| Character class | Matching section |
| --- | --- |
| %a | letters (A-Z, a-z)  |
| %c | control characters (\n, \t, \r, ...) |
| %d | digits (0-9) |
| %l | lower-case letter (a-z) |
| %p | punctuation characters (!, ?, &, ...)
| %s | space characters |
| %u | upper-case letters |
| %w | alphanumeric characters (A-Z, a-z, 0-9) | 
| %x | hexadecimal digits (\3, \4, ...) |
| %z | the character with representation 0 |
| **.**  | Matches any character |

As mentioned above, any upper-case version of those classes represents the complement of the class. For instance, **`%D`** will match any non-digit character sequence:

    string.match("f123", "%D")          --> f

In addition to character classes, some characters have special functions as patterns:

    ( ) % . + - * [ ? ^ $

The character **`%`** represents a character escape, making **`%?`** match an interrogation and **`%%`** match the percentage symbol. You can use the **`%`** character with any other non-alphanumeric character, therefore, if you need to escape, for instance, a quote, you must use **`\\`** before it, which escapes any character from a lua string. 

A character set, represented inside square brackets (`[]`), allows you to create a special character class, combining different classes and single characters:

    local foo = "bar123bar2341"
    print(foo:match "[arb]")            --> b

You can get the complement of the character set by starting it with **`^`**:

    local foo = "bar123bar2341"
    print(string.match(foo, "[^bar]"))  --> 1

In this example, `string.match` will find the first occurrence that isn't **b**, **a** or **r**.

Patterns can be more useful with the help of repetition/optional modifiers, patterns in lua offer these four characters:

| Character | Modifier |
| ----- | ----- |
| \+ | One or more repetitions |
| \* | Zero or more repetitions |
| \- | Also zero or more repetitions|
| \? | Optional (zero or one occurrence)|

The character `+` represents one or more matched characters in the sequence and it will always return the longest matched sequence:

    local foo = "12345678bar123"
    print(foo:match "%d+")  --> 12345678

As you can see, **`*`** is similar to **`+`**, but it accepts zero occurrences of characters and is commonly used to match optional spaces between different patterns.

The character **`-`** is also similar to **`*`**, but instead of returning the longest matched sequence, it matches the shortest one.

The modifier **`?`** matches an optional character, allowing you to match, for example, a negative digit:

    local foo = "-20"
    print(foo:match "[+-]?%d+")

Lua pattern matching engine provides a few additional pattern matching items:

| Character item | Description |
| ---- | ---- |
| `%n` | for n between 1 and 9  matches a substring equal to the n-th captured string |
| `%bxy` | matches substring between two distinct characters (balanced pair of `x` and `y`) |
| `%f[set]` | frontier pattern: matches an empty string at any position such that the next character<br> belongs to set and the previous character does not belong to set |

## string.find (Introduction)
The `find` function
==
First let's take a look at the `string.find` function in general:

The function `string.find (s, substr [, init [, plain]])` returns the start and end index of a substring if found, and nil otherwise, starting at the index `init` if it is provided (defaults to 1).

    ("Hello, I am a string"):find "am" --> returns 10 11
    -- equivalent to string.find("Hello, I am a string", "am") -- see remarks

Introducing Patterns
==

    ("hello world"):find ".- " -- will match characters until it finds a space
        --> so it will return 1, 6

All **except** the following characters represent themselves `^$()%.[]*+-?)`. Any of these characters can be represented by a `%` following the character itself.

    ("137'5 m47ch s0m3 d1g175"):find "m%d%d" -- will match an m followed by 2 digit
        --> this will match m47 and return 7, 9

    ("stack overflow"):find "[abc]" -- will match an 'a', a 'b' or a 'c'
        --> this will return 3 (the A in stAck)

    ("stack overflow"):find "[^stack ]"
        -- will match all EXCEPT the letters s, t, a, c and k and the space character
        --> this will match the o in overflow

    ("hello"):find "o%d?" --> matches o, returns 5, 5
    ("hello20"):find "o%d?" --> matches o2, returns 5, 6
        -- the ? means the character is optional

    ("helllllo"):find "el+" --> will match elllll
    ("heo"):find "el+" --> won't match anything

    ("helllllo"):find "el*" --> will match elllll
    ("heo"):find "el*" --> will match e

    ("helelo"):find "h.+l" -- + will match as much as it gets
        --> this matches "helel"
    ("helelo"):find "h.-l" -- - will match as few as it can
        --> this wil only match "hel"

    ("hello"):match "o%d*"
        --> like ?, this matches the "o", because %d is optional
    ("hello20"):match "o%d*"
        --> unlike ?, it maches as many %d as it gets, "o20"
    ("hello"):match "o%d"
        --> wouldn't find anything, because + looks for 1 or more characters



## The `gmatch` function
How it works
==

The `string.gmatch` function will take an input string and a pattern. This pattern describes on what to actually get back. This function will return a function which is actually an iterator. The result of this iterator will match to the pattern.

    type(("abc"):gmatch ".") --> returns "function"

    for char in ("abc"):gmatch "." do
        print char -- this prints:
        --> a
        --> b
        --> c
    end

    for match in ("#afdde6"):gmatch "%x%x" do
        print("#" .. match) -- prints:
        --> #af
        --> #dd
        --> #e6
    end

Introducing captures:
--
This is very similair to the regular function, however it will return only the captures instead the full match.

    for key, value in ("foo = bar, bar=foo"):gmatch "(%w+)%s*=%s*(%w+)" do
        print("key: " .. key .. ", value: " .. value)
        --> key: foo, value: bar
        --> key: bar, value: foo
    end

## The gsub function
do not confuse with the string.sub function, which returns a substring!

How it works
==

string argument
--
    ("hello world"):gsub("o", "0")
        --> returns "hell0 w0rld", 2
        -- the 2 means that 2 substrings have been replaced (the 2 Os)

    ("hello world, how are you?"):gsub("[^%s]+", "word")
        --> returns "word word, word word word?", 5

    ("hello world"):gsub("([^%s])([^%s]*)", "%2%1")
        --> returns "elloh orldw", 2

function argument
--
    local word = "[^%s]+"

    function func(str)
        if str:sub(1,1):lower()=="h" then
            return str
        else
            return "no_h"
        end
    end
    ("hello world"):gsub(word, func)
        --> returns "hello no_h", 2

table argument
--
    local word = "[^%s]+"

    sub = {}
    sub["hello"] = "g'day"
    sub["world"] = "m8"

    ("hello world"):gsub(word, sub)
        --> returns "g'day m8"

    ("hello world, how are you?"):gsub(word, sub)
        --> returns "g'day m8, how are you?"
        -- words that are not in the table are simply ignored

