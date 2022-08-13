---
title: "Arithmetic Expansion"
slug: "arithmetic-expansion"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Numbers in arithmetic expansions must match the following ERE:
     
    [-+]?(0[0-7]+|[1-9][0-9]*|0[Xx][0-9A-Fa-f]+)

Arithmetic expressions support signed integer operators, comparisons, Boolean expressions, assignments, and ternary expressions from C.

Resources
---

- [Arithmetic expansion in POSIX](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_06_04)
- [Operator precedence](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap01.html#tag_17_01_02_01)


## Line Count
    i=0
    while read -r line; do
            i=$((i+1))
    done < file
    echo $i

With a file containing:

    Alpha
    Beta
    Gamma
    Delta
    Epsilon

The above script prints: `5`

## Parameter Expansion
Loop `n` times:

    while [ $((i=${i:=0}+1)) -le "$n" ]; do
        echo line $i
    done

Output for `n=5`:
     
    line 1
    line 2
    line 3
    line 4
    line 5

Manipulating decimals:

    $ i=3.14159; echo $((${i%.*}*2))
    6
    $ i=3.14159; echo $((${i#*.}*2))
    28318


## Ternery Expressions
Absolute value:

    $ for n in -8 -2 0 3 4; do
    >     echo $((n<0?-n:n))
    > done
    8
    2
    0
    3
    4

Fix variable range:
    
    $ min=2
    $ max=4
    $ for n in 1 2 3 4 5; do
    >     echo $((n<min?min:n>max?max:n))
    > done
    2
    2
    3
    4
    4


## Is a Power of 2
    $ ispow2() { return $((!($1!=0&&($1&$1-1)==0))); }
    $ i=0
    $ while [ $i -lt 100 ]; do
    >     if ispow2 $((i=i+1)); then
    >         echo $i
    >     fi
    > done
    1
    2
    4
    8
    16
    32
    64

`$1!=0` 0 is not a power of 2.

`($1&$1-1)==0` [Unset the lowest bit][bits].  If it was the only bit then the number was a power of 2.

The additional `!` was for correcting the value to what the shell expects, which is the opposite of the conventional true/false values
(zero for true and non-zero for false, vs zero for false and non-zero for true).

[bits]: https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan



