---
title: "Writing and using modules"
slug: "writing-and-using-modules"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

The basic pattern for writing a module is to fill a table with keys that are function names and values that are the functions themselves. The module then returns this function for calling code to `require` and use. (Functions are first-class values in Lua, so storing a function in a table is easy and common.) The table can also contain any important constants in the form of, say, strings or numbers.

## Writing the module
    --- trim: a string-trimming module for Lua
    -- Author, date, perhaps a nice license too
    --
    -- The code here is taken or adapted from  material in
    -- Programming in Lua, 3rd ed., Roberto Ierusalimschy

    -- trim_all(string) => return string with white space trimmed on both sides 
    local trim_all = function (s)
      return (string.gsub(s, "^%s*(.-)%s*$", "%1"))
    end

    -- trim_left(string) => return string with white space trimmed on left side only
    local trim_left = function (s)
      return (string.gsub(s, "^%s*(.*)$", "%1"))
    end

    -- trim_right(string) => return string with white space trimmed on right side only
    local trim_right = function (s)
      return (string.gsub(s, "^(.-)%s*$", "%1"))
    end

    -- Return a table containing the functions created by this module
    return {
      trim_all = trim_all,
      trim_left = trim_left,
      trim_right = trim_right
    }

An alternative approach to the one above is to create a top-level table and then store the functions directly in it. In that idiom, our module above would look like this:

    -- A conventional name for the table that will hold our functions
    local M = {}

    -- M.trim_all(string) => return string with white space trimmed on both sides
    function M.trim_all(s)
      return (string.gsub(s, "^%s*(.-)%s*$", "%1"))
    end
    
    -- M.trim_left(string) => return string with white space trimmed on left side only
    function M.trim_left(s)
      return (string.gsub(s, "^%s*(.*)$", "%1"))
    end
    
    -- trim_right(string) => return string with white space trimmed on right side only
    function M.trim_right(s)
      return (string.gsub(s, "^(.-)%s*$", "%1"))
    end
    
    
    return M

From the point of view of the caller, there is little difference between the two styles. (One difference worth mentioning is that the first style makes it more difficult for users to monkeypatch the module. This is either a pro or a con, depending on your point of view. For more detail about this, see [this blog post](http://kiki.to/blog/2014/04/04/rule-3-allow-monkeypatching) by Enrique García Cota.)

## Using the module
    -- The following assumes that trim module is installed or in the caller's package.path,
    -- which is a built-in variable that Lua uses to determine where to look for modules.
    local trim = require "trim"

    local msg = "    Hello, world!      "
    local cleaned = trim.trim_all(msg)
    local cleaned_right = trim.trim_right(msg)
    local cleaned_left = trim.trim_left(msg)

    -- It's also easy to alias functions to shorter names.
    local trimr = trim.trim_right
    local triml = trim.trim_left

