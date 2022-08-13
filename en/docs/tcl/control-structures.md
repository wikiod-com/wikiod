---
title: "Control Structures"
slug: "control-structures"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax
 - if expr1 ?then? body1 elseif expr2 ?then? body2 ... ?else? ?bodyN?
 - for start test next body
 - while test body
 - foreach varlist1 list1 ?varlist2 list2 ...? body

Documentation:
[break](http://www.tcl.tk/man/tcl/TclCmd/break.htm "Abort looping command"),
[for](http://www.tcl.tk/man/tcl/TclCmd/for.htm "'For' loop"),
[foreach](http://www.tcl.tk/man/tcl/TclCmd/foreach.htm "Iterate over all elements in one or more lists"),
[if](http://www.tcl.tk/man/tcl/TclCmd/if.htm "Execute scripts conditionally"),
[switch](http://www.tcl.tk/man/tcl/TclCmd/switch.htm "Evaluate one of several scripts, depending on a given value"),
[uplevel](http://www.tcl.tk/man/tcl/TclCmd/uplevel.htm "Execute a script in a different stack frame"),
[while](http://www.tcl.tk/man/tcl/TclCmd/while.htm "Execute script repeatedly as long as a condition is met")



## if / while / for
**if** *expr1* ?then? *body1* elseif *expr2* ?then? *body2* ... ?else? *?bodyN?*

*exprN* is an expression that evaluates to a boolean value.  *bodyN* is a list of commands.

    set i 5
    if {$i < 10} {
      puts {hello world}
    } elseif {$i < 70} {
      puts {enjoy world}
    } else {
      puts {goodbye world}
    }

**for** *start test next body*

*start*, *next* and *body* are lists of commands.
*test* is an expression that evaluates to a boolean values.

    
The **break** command will break out of the loop.  The **continue** command will skip to the next iteration of the loop.

The common usage is:

    for {set i 0} {$i < 5} {incr i} {
      puts "$i: hello world"
    }

Since *start* and *next* are lists of commands, any command may be present.

    for {set i 0; set j 5} {$i < 5} {incr i; incr j -1} {
      puts "i:$i j:$j"
    }

**while** *test body*

The *test* is any expression that evaluates to a boolean value.
While *test* is true, *body* is executed.

    set x 0
    while {$x < 5} {
      puts "hello world"
      incr x
    }
    
The **break** command will break out of the loop.  The **continue** command will skip to the next iteration of the loop.

    set lineCount 0
    while {[gets stdin line] >= 0} {
      puts "[incr lineCount]: $line"
      if { $line eq "exit" } {
        break
      }  
    }
   


## List iteration: foreach
**foreach** *varlist1* *list1* ?*varlist2* *list2* ...? *body*

**foreach** is a powerful control structure that allows looping over a list or multiple lists.

    set alpha [list a b c d e f]
    foreach {key} $alpha {
       puts "key: $key"
    }

Multiple variable names may be specified.

    set alphaindexes [list a 1 b 2 c 3 d 4 e 5 f 6]
    foreach {key num} $alphaindexes {
       puts "key:$key num:$num"
    }

Multiple lists can be iterated over at the same time.

    set alpha [list a b c d e f]
    set indexes [list 1 2 3 4 5 6]
    foreach {key} $alpha {idx} $indexes {
       puts "key: $key idx:$idx"
    }
    

## Adding a new control structure to Tcl
In Tcl, a control structure is basically just another command. This is one possible implementation of a `do ... while` / `do ... until` control structure.

    proc do {body keyword expression} {
        uplevel 1 $body
        switch $keyword {
            while {uplevel 1 [list while $expression $body]}
            until {uplevel 1 [list while !($expression) $body]}
            default {
                return -code error "unknown keyword \"$keyword\": must be until or while"
            }
        }
    }

Common for both kinds of `do`-loops is that the script named `body` will always be executed at least once, so we do that right away. The invocation `uplevel 1 $body` means "execute script at the caller's stack level". This way, all variables used by the script will be visible, and any results produced will stay at the caller's level. The script then selects, based on the `keyword` parameter, whether to iterate while a condition is true or until it is false, which is the same as iterating while the logical negation of the condition is true. If an unexpected keyword is given, an error message is produced.

