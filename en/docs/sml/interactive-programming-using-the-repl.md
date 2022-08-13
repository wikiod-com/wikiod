---
title: "Interactive Programming using the REPL"
slug: "interactive-programming-using-the-repl"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Syntax
- Unlike source code files, the semicolon ';' is mandatory to terminate each expression in the REPL. 

## Starting the SMLNJ REPL
REPL stands for 'Read Evaluate Print Loop.' The REPL can be used to write and execute code one line at a time and is an alternative to writing code to a file and then compiling or interpreting the entire file before execution.

To start the SMLNJ REPL from a command prompt:

    smluser> sml
    Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
    - 3+4;
    val it = 7 : int
    - (*a comment: press contrl-d to exit *)
    smluser>
    

In the Bash and similar command shells, [GNU readline][1] functionality can be added to the SML REPL using the system command `rlwrap sml`.

    smluser> rlwrap sml
    Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
    - 3+4;
    val it = 7 : int
    - (* pressing the up arrow recalls the previous input *)
    - 3+4;
    val it = 7 : int
    -
    smluser>


 


  [1]: https://cnswww.cns.cwru.edu/php/chet/readline/rluserman.html

## Using 'it'
All SML expressions return a value. The REPL stores the return value of the last evaluated expression. `it` provides the value of the last evaluated expression within the REPL.

    smluser> sml
    Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
    - 3+4;
    val it = 7 : int
    - it;
    val it = 7 : int
    - it + 1;
    val it = 8 : int
    -
    
    [1]+  Stopped                 sml
    smluser>

Effectively, comments are not evaluated by the REPL and do not change the value of it.

    smluser> sml
    Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
    - 3+4;
    val it = 7 : int
    - (* a comment *);
    - it;
    val it = 7 : int
    
    [1]+  Stopped                 sml
    smluser>





