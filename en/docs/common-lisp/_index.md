---
title : common-lisp Tutorial
slug : common-lisp-tutorial
weight : 9869
draft : false
images : []
type : docs
---

This is a simple hello world function in Common Lisp. Examples will print the text `Hello, World!` (without quotation marks; followed by a newline) to the standard output.

Common Lisp is a programming language that is largely used interactively using an interface known as a REPL. The REPL (Read Eval Print Loop) allows one to type code, have it evaluated (run) and see the results immediately. The prompt for the REPL (at which point one types the code to be run) is indicated by `CL-USER> `. Sometimes something other than `CL-USER` will appear before the `> ` but this is still a REPL.

After the prompt comes some code, usually either a single word (i.e. a variable name) or a form (a list of words/forms enclosed between `(` and `)`) (i.e. a function call or declaration, etc). On the next line will be any output that the program prints (or nothing if the program prints nothing) and then the values returned by evaluating the expression. Normally an expression returns one value but if it returns multiple values they appear once per line.

