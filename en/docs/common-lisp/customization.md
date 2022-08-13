---
title: "Customization"
slug: "customization"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## Initialization Files
Most Common Lisp implementations will try to load an *init file* on startup:

| Implementation | Init file                | Site/System Init file               |
| -------------- | ------------------------ | ----------------------------------- |
| ABCL           | `$HOME/.abclrc`          |  |
| Allegro CL     | `$HOME/.clinit.cl`       |  |
| ECL            | `$HOME/.eclrc`           |  |
| Clasp          | `$HOME/.clasprc`         |  |
| CLISP          | `$HOME/.clisprc.lisp`    |  |
| [Clozure CL][1]     | `home:ccl-init.lisp` or `home:ccl-init.fasl` or `home:.ccl-init.lisp`   |  |
| CMUCL          | `$HOME/.cmucl-init.lisp` |  |
| LispWorks      | `$HOME/.lispworks`       |  |
| MKCL           | `$HOME/.mkclrc`          |  |
| [SBCL][2]           | `$HOME/.sbclrc`          | `$SBCL_HOME/sbclrc` or `/etc/sbclrc` |
| SCL            | `$HOME/.scl-init.lisp`   |  |

Sample Initialization files:

| Implementation | Sample Init file                                               |
| -------------- | -------------------------------------------------------------- |
| LispWorks      | `Library/lib/7-0-0-0/config/a-dot-lispworks.lisp`              |


  [1]: http://ccl.clozure.com/manual/chapter2.4.html
  [2]: http://www.sbcl.org/manual/#Initialization-Files

## More features for the Read-Eval-Print-Loop (REPL) in a terminal
CLISP has an integration with GNU Readline.

For improvements for other implementations see: How to customize the [SBCL REPL][1].


  [1]: http://stackoverflow.com/questions/11109249/how-to-customize-the-sbcl-repl

## Optimization settings
Common Lisp has a way to influence the compilation strategies. It makes sense to define your preferred values.

Optimization values are between 0 (unimportant) and 3 (extremely important). **1 is the neutral value.**

It's useful to always use safe code (safety = 3) with all runtime checks enabled.

Note that the interpretation of values is implementation specific. Most Common Lisp implementations make some use of these values.

| Setting | Explanation | useful default value | useful delivery value | 
| ------- | ------ |------ |------ |
| `compilation-speed`| speed of the compilation process   | 2   | 0   |
| `debug`   | ease of debugging   | 2   | 1 or 0   |
| `safety`  | run-time error checking   | 3   | 2   |
| `space`   | both code size and run-time space   | 2   | 2   |
| `speed`   | speed of the object code   | 2   | 3   |

An `optimize` declaration for use with `declaim`, `declare` and `proclaim`:

    (optimize (compilation-speed 2)
              (debug 2)
              (safety 3)
              (space 2)
              (speed 2))

Note that you can also apply special optimization settings to portions of the code in a function using the macro `LOCALLY`.

