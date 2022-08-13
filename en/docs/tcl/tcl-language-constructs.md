---
title: "Tcl Language Constructs"
slug: "tcl-language-constructs"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
 - \# This is a valid comment
 - \# This is a valid { comment }

## Placing Comments
Comments in Tcl are best thought of as another command.  
A comment consists of a `#` followed by any number of characters up to the next newline.   A comment can appear wherever a command can be placed.   

    # this is a valid comment
    proc hello { } {
      # the next comment needs the ; before it to indicate a new command is
      # being started.
      puts "hello world" ; # this is valid
      puts "dlrow olleh" # this is not a valid comment

      # the comment below appears in the middle of a string.
      # is is not valid.
      set hw {
          hello ; # this is not a valid comment 
          world 
          }

      gets stdin inputfromuser
      switch inputfromuser {
         # this is not a valid comment. 
         # switch expects a word to be here.
         go {
           # this is valid.  The switch on 'go' contains a list of commands
           hello
         }
         stop {
           exit
         }
      }
    }




## Braces in comments
Due to the way the Tcl language parser works, braces in the code must be properly matched.   This includes the braces in comments.  

    proc hw {} { 
       # this { code will fail
       puts {hello world}
    }

A *missing close-brace: possible unbalanced brace in comment* error will be thrown.

    proc hw {} {
      # this { comment } has matching braces.
      puts {hello world}
    }

This will work as the braces are paired up properly.






## Quoting
In the Tcl language in many cases, no special quoting is needed.

These are valid strings:

    abc123
    4.56e10
    my^variable-for.my%use
    
The Tcl language splits words on whitespace, so any literals or strings with whitespace should be quoted.  There are two ways to quote strings.  With braces and with quotation marks.

    {hello world}
    "hello world"

When quoting with braces, no substitutions are performed.   Embedded braces may be  escaped with a backslash, but note that the backslash is part of the string.

    % puts {\{ \}}
    \{ \}
    % puts [string length {\{ \}}]
    5
    % puts {hello [world]}
    hello [world]
    % set alpha abc123
    abc123
    % puts {$alpha}
    $alpha

When quoting with double quotes, command, backslash and variable substitutions are processed.

    % puts "hello [world]"
    invalid command name "world"
    % proc world {} { return my-world }
    % puts "hello [world]"
    hello my-world
    % puts "hello\tworld"
    hello   world
    % set alpha abc123
    abc123
    % puts "$alpha"
    abc123
    % puts "\{ \}"
    { }

    

