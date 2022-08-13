---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## String literals
    let string1 = "Hello" //simple string
    
    let string2 = "Line\nNewLine" //string with newline escape sequence
    
    let string3 = @"Line\nSameLine" //use @ to create a verbatim string literal
    
    let string4 = @"Line""with""quoutes inside" //double quoute to indicate a single quoute inside @ string
    
    let string5 = """single "quote" is ok""" //triple-quote string literal, all symbol including quote are verbatim
    
    let string6 = "ab
    cd"// same as "ab\ncd"
    
    let string7 = "xx\
        yy" //same as "xxyy", backslash at the end contunies the string without new line, leading whitespace on the next line is ignored 

## Simple string formatting
There are several ways to format and get a string as a result.

The .NET way is by using `String.Format` or `StringBuilder.AppendFormat`:

    open System
    open System.Text

    let hello = String.Format ("Hello {0}", "World")
    // return a string with "Hello World"

    let builder = StringBuilder()
    let helloAgain = builder.AppendFormat ("Hello {0} again!", "World")
    // return a StringBuilder with "Hello World again!"

F# has also functions to format string in a C-style. There are equivalents for each .NET functions:

 - `sprintf` (String.Format) :

    
    open System
    
    let hello = sprintf "Hello %s" "World" 
    // "Hello World", "%s" is for string
    
    let helloInt = sprintf "Hello %i" 42 
    // "Hello 42", "%i" is for int
    
    let helloFloat = sprintf "Hello %f" 4.2 
    // "Hello 4.2000", "%f" is for float
    
    let helloBool = sprintf "Hello %b" true 
    // "Hello true", "%b" is for bool
    
    let helloNativeType = sprintf "Hello %A again!" ("World", DateTime.Now) 
    // "Hello {formatted date}", "%A" is for native type
    
    let helloObject = sprintf "Hello %O again!" DateTime.Now 
    // "Hello {formatted date}", "%O" is for calling ToString

 - `bprintf` (StringBuilder.AppendFormat):


    open System
    open System.Text

    let builder = StringBuilder()

    // Attach the StringBuilder to the format function with partial application
    let append format = Printf.bprintf builder format
    
    // Same behavior as sprintf but strings are appended to a StringBuilder
    append "Hello %s again!\n" "World"
    append "Hello %i again!\n" 42
    append "Hello %f again!\n" 4.2
    append "Hello %b again!\n" true
    append "Hello %A again!\n" ("World", DateTime.Now)
    append "Hello %O again!\n" DateTime.Now
    
    builder.ToString() // Get the result string

Using those functions instead of the .NET functions provides some advantages:

 - Type safety
 - Partial application
 - F# native type support

