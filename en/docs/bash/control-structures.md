---
title: "Control Structures"
slug: "control-structures"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Syntax
 - [ "$1" = "$2" ] #A "[" bracket is actually a command. Because of this it requires a space befor and after it.
 - test "$1" = "$2" #Test is a synonym for the "[" command 

## Parameters
| Parameter to [ or test | Details |  
| --------- | ------- |  
| **File Operators** | Details |
| `-e "$file"` | Returns true if the file exists.|
| `-d "$file"` | Returns true if the file exists and is a directory|
| `-f "$file"` | Returns true if the file exists and is a regular file|
| `-h "$file"` | Returns true if the file exists and is a symbolic link|
| **String Comparators**| Details |
| `-z "$str"` | True if length of string is zero |
| `-n "$str` | True if length of string is non-zero |
| `"$str" = "$str2"` | True if string $str is equal to string $str2. Not best for integers. It may work but will be inconsitent |
| `"$str" != "$str2"` | True if the strings are not equal |
| **Integer&nbsp;Comparators**&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; | Details |
| `"$int1" -eq "$int2"` | True if the integers are equal |
| `"$int1" -ne "$int2"` | True if the integers are not equals |
| `"$int1" -gt "$int2"` | True if int1 is greater than int 2 |
| `"$int1" -ge "$int2"` | True if int1 is greater than or equal to int2
| `"$int1" -lt "$int2"` | True if int1 is less than int 2 |
| `"$int1" -le "$int2"` | True if int1 is less than or equal to int2 |




There are many comparator parameters available in bash. Not all are yet listed here.

## If statement
    if [[ $1 -eq 1 ]]; then
        echo "1 was passed in the first parameter"
    elif [[ $1 -gt 2 ]]; then
        echo "2 was not passed in the first parameter"
    else
        echo "The first parameter was not 1 and is not more than 2."
    fi

The closing `fi` is necessary, but the `elif` and/or the `else` clauses can be omitted.

The semicolons before `then` are standard syntax for combining two commands on a single line; they can be omitted only if `then` is moved to the next line.

It's important to understand that the brackets `[[` are not part of the syntax, but are treated as a command; it is the exit code from this command that is being tested. Therefore, you must always include spaces around the brackets.

This also means that the result of any command can be tested. If the exit code from the command is a zero, the statement is considered true.

    if grep "foo" bar.txt; then
        echo "foo was found"
    else
        echo "foo was not found"
    fi

Mathematical expressions, when placed inside double parentheses, also return 0 or 1 in the same way, and can also be tested:

    if (( $1 + 5 > 91 )); then
        echo "$1 is greater than 86"
    fi

You may also come across `if` statements with single brackets. These are defined in the POSIX standard and are guaranteed to work in all POSIX-compliant shells including Bash. The syntax is very similar to that in Bash:

    if [ "$1" -eq 1 ]; then
        echo "1 was passed in the first parameter"
    elif [ "$1" -gt 2 ]; then
        echo "2 was not passed in the first parameter"
    else
        echo "The first parameter was not 1 and is not more than 2."
    fi


## Looping over an array
`for` loop:

    arr=(a b c d e f)
    for i in "${arr[@]}";do
        echo "$i"
    done

Or

    for ((i=0;i<${#arr[@]};i++));do
        echo "${arr[$i]}" 
    done
`while` loop:

    i=0
    while [ $i -lt ${#arr[@]} ];do
        echo "${arr[$i]}"
        i=$(expr $i + 1)
    done

Or

    i=0
    while (( $i < ${#arr[@]} ));do
        echo "${arr[$i]}"
        ((i++))
    done

## Using For Loop to List Iterate Over Numbers
    #! /bin/bash

    for i in {1..10}; do # {1..10} expands to "1 2 3 4 5 6 7 8 9 10"
        echo $i
    done

This outputs the following:

    1
    2
    3
    4
    5
    6
    7
    8
    8
    10


## Conditional execution of command lists
### How to use conditional execution of command lists ###

Any builtin command, expression, or function, as well as any external command or script can be executed conditionally using the `&&`*(and)* and `||`*(or)* operators.

For example, this will only print the current directory if the `cd` command was successful.

    cd my_directory && pwd

Likewise, this will exit if the `cd` command fails, preventing catastrophe:

    cd my_directory || exit
    rm -rf *

When combining multiple statements in this manner, it's important to remember that (unlike many C-style languages) [these operators have no precedence and are left-associative](https://www.gnu.org/software/bash/manual/html_node/Lists.html#Lists).

Thus, this statement will work as expected...

    cd my_directory && pwd || echo "No such directory"

- If the `cd` succeeds, the `&& pwd` executes and the current working directory name is printed. Unless `pwd` fails (a rarity) the `|| echo ...` will not be executed.
- If the `cd` fails, the `&& pwd` will be skipped and the `|| echo ...` will run.

But this will not (if you're thinking `if...then...else`)...

    cd my_directory && ls || echo "No such directory"

- If the `cd` fails, the `&& ls` is skipped and the `|| echo ...` is executed.
- If the `cd` succeeds, the `&& ls` is executed.
  - If the `ls` succeeds, the `|| echo ...` is ignored. *(so far so good)*
  - ***BUT... if the `ls` fails, the `|| echo ...` will also be executed.***
    > ***It is the `ls`***, not the `cd`, ***that is the previous command***.

### Why use conditional execution of command lists ###

Conditional execution is a hair faster than `if...then` but its main advantage is allowing functions and scripts to exit early, or "short circuit".

Unlike many languages like `C` where memory is explicitly allocated for structs and variables and such (and thus must be deallocated), `bash` handles this under the covers. In most cases, we don't have to clean up anything before leaving the function. A `return` statement will deallocate everything local to the function and pickup execution at the return address on the stack.

Returning from functions or exiting scripts as soon as possible can thus significantly improve performance and reduce system load by avoiding the unnecessary execution of code. For example...

    my_function () {

        ### ALWAYS CHECK THE RETURN CODE

        # one argument required. "" evaluates to false(1)
        [[ "$1" ]]             || return 1

        # work with the argument. exit on failure
        do_something_with "$1" || return 1
        do_something_else      || return 1

        # Success! no failures detected, or we wouldn't be here
        return 0
    }

## While Loop
    #! /bin/bash

    i=0

    while [ $i -lt 5 ] #While i is less than 5
    do
        echo "i is currently $i"
        i=$[$i+1] #Not the lack of spaces around the brackets. This makes it a not a test expression
    done #ends the loop

Watch that there are spaces around the brackets during the test (after the while statement). These spaces are necessary.

This loop outputs:

    i is currently 0
    i is currently 1
    i is currently 2
    i is currently 3
    i is currently 4

## For Loop with C-style syntax
The basic format of C-style `for` loop is:

    for (( variable assignment; condition; iteration process ))

Notes:
- The assignment of the variable inside C-style `for` loop can contain spaces unlike the usual assignment
- Variables inside C-style `for` loop aren't preceded with `$`.

Example: 

    for (( i = 0; i < 10; i++ ))
    do
        echo "The iteration number is $i"
    done

Also we can process multiple variables inside C-style `for` loop:

    for (( i = 0, j = 0; i < 10; i++, j = i * i ))
    do
        echo "The square of $i is equal to $j"
    done

## Until Loop
Until loop executes until condition is true

    i=5
    until [[ i -eq 10 ]]; do #Checks if i=10
        echo "i=$i" #Print the value of i
        i=$((i+1)) #Increment i by 1
    done

Output:

    i=5
    i=6
    i=7
    i=8
    i=9

When `i` reaches 10 the condition in until loop becomes true and the loop ends.



## continue and break
Example for continue

    for i in [series]
    do
        command 1
        command 2
        if (condition) # Condition to jump over command 3
                continue # skip to the next value in "series"
        fi
        command 3
    done

Example for break

    for i in [series]
    do
        command 4
        if (condition) # Condition to break the loop
        then
                command 5 # Command if the loop needs to be broken
                break
        fi
        command 6 # Command to run if the "condition" is never true 
    done

## Loop break
Break multiple loop:

    arr=(a b c d e f)
    for i in "${arr[@]}";do
        echo "$i"
        for j in "${arr[@]}";do
            echo "$j"
            break 2
        done
    done

Output:

    a
    a

Break single loop:

    arr=(a b c d e f)
    for i in "${arr[@]}";do
        echo "$i"
        for j in "${arr[@]}";do
            echo "$j"
            break
        done
    done

Output:

    a
    a
    b
    a
    c
    a
    d
    a
    e
    a
    f
    a



## Switch statement with case
With the `case` statement you can match values against one variable.

The argument passed to `case` is expanded and try to match against each patterns.

If a match is found, the commands upto `;;` are executed.

    case "$BASH_VERSION" in
     [34]*)
        echo {1..4}
        ;;  
      *)
        seq -s" " 1 4
    esac

Pattern are not regular expressions but shell pattern matching (aka globs).

## For Loop without a list-of-words parameter
    for arg; do
        echo arg=$arg
    done

A `for` loop without a list of words parameter will iterate over the positional parameters instead. In other words, the above example is equivalent to this code:

    for arg in "$@"; do
        echo arg=$arg
    done

In other words, if you catch yourself writing `for i in "$@"; do ...; done`, just drop the `in` part, and write simply `for i; do ...; done`.

## For Loop
    #! /bin/bash
    
    for i in 1 "test" 3; do #Each space separated statement is assigned to i
        echo $i
    done

Other commands can generate statements to loop over. See "Using For Loop to Iterate Over Numbers" example.

This outputs:

    1
    test
    3

