---
title: "Comments"
slug: "comments"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Single & Multiple line comments
Comments are programmer-readable annotations that are ignored at runtime. Their purpose is to make source code easier to understand.

**Single line comments**
 

The `#` character is used to add single line comments.

    #!/usr/bin/ruby -w
    # This is a single line comment.
    puts "Hello World!"

When executed, the above program will output `Hello World!`

**Multiline comments**

Multiple-line comments can be added by using `=begin` and `=end` syntax (also known as the comment block markers) as follows:

    #!/usr/bin/ruby -w
    =begin
    This is a multiline comment.
    Write as many line as you want. 
    =end
    puts "Hello World!"

When executed, the above program will output `Hello World!`


