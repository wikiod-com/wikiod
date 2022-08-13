---
title: "Case statement"
slug: "case-statement"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Simple case statement
In its simplest form supported by all versions of bash, case statement executes the case that matches the pattern. `;;` operator breaks after the first match, if any.

    #!/bin/bash
    
    var=1
    case $var in
    1)
      echo "Antartica"
     ;;
    2)
      echo "Brazil"
     ;;
    3)
      echo "Cat"
     ;;
    esac

Outputs:

    Antartica


## Case statement with fall through
<!-- if version [gte 4.0] -->
Since bash 4.0, a new operator `;&` was introduced which provides [fall through][1] mechanism.

#!/bin/bash

    var=1
    case $var in
    1)
      echo "Antartica"
      ;&
    2)
      echo "Brazil"
      ;&
    3)
      echo "Cat"
      ;&
    esac

Outputs:

    Antartica
    Brazil
    Cat

<!-- end version if -->


  [1]: https://en.wikipedia.org/wiki/Switch_statement#Fallthrough

## Fall through only if subsequent pattern(s) match
<!-- if version [gte 4.0] -->

Since Bash 4.0, another operator `;;&` was introduced which also provides [fall through][1] *only if* the patterns in subsequent case statement(s), if any, match.


    #!/bin/bash
    
    var=abc
    case $var in
    a*)
      echo "Antartica"
      ;;&
    xyz)
      echo "Brazil"
      ;;&
    *b*)
      echo "Cat"
      ;;&
    esac

Outputs:

    Antartica
    Cat


In the below example, the `abc` matches both first and third case but not the second case. So, second case is not executed.

<!-- end version if -->


  [1]: https://en.wikipedia.org/wiki/Switch_statement#Fallthrough

