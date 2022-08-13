---
title: "Variables"
slug: "variables"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Command-line variable assignment
To assign variables from the command-line, `-v` can be used:

    $ awk -v myvar="hello" 'BEGIN {print myvar}'
    hello

Note that there are no spaces around the equal sign.

This allows to use shell variables:

    $ shell_var="hello"
    $ awk -v myvar="$shell_var" 'BEGIN {print myvar}'
    hello

Also, this allows to set built-in variables that control `awk`:

See an example with `FS` (field separator):

    $ cat file
    1,2;3,4
    $ awk -v FS="," '{print $2}' file
    2;3
    $ awk -v FS=";" '{print $2}' file
    3,4

Or with `OFS` (output field separator):

    $ echo "2 3" | awk -v OFS="--" '{print $1, $2}'
    2--3
    $ echo "2 3" | awk -v OFS="+" '{print $1, $2}'
    2+3



## Passing parameters to a program using the -v option
The option `-v` followed by an assignment of the form *variable=value* can be used to pass parameters to an **awk** program.  This is illustrated by the **punishment** program below, whose job is to write *count* times the sentence “I shall not talk in class.” on standard output.  The following example uses the value 100, which is very popular among teachers:

    awk -v count=100 'BEGIN {
      for(i = 1; i <= count; ++i) {
        print("I shall not talk in class.")
      }
      exit
    }'

It is possible to pass multiple parameters with repeated usage of the `-v` flag:

    awk -v count=100 -v "sentence=I shall not talk in class." 'BEGIN {
      for(i = 1; i <= count; ++i) {
        print(sentence)
      }
      exit
    }'

There is no built-in support for array or list parameters, these have to be handled manually.  A classical approach to pass a list parameter is to concatenate the list using a delimiter, popular choices are `:`, `|` or `,`.  The *split* function then allows to recover the list as an **awk** array:

    awk -v 'serialised_list=a:b:c:d:e:f' 'BEGIN {
      list_sz = split(serialised_list, list, ":")
      for(i = 1; i <= list_sz; ++i) {
        printf("list: %d: %s\n", i, list[i])
      }
      exit
    }'

The output of this **awk** program is

    list: 1: a
    list: 2: b
    list: 3: c
    list: 4: d
    list: 5: e
    list: 6: f

Sometimes it is more convenient to recover list items as keys of an **awk** array, as this allows easy membership verification.  For instance, the following program print each line whose first word does not belong to a fixed list of exceptions:

    awk -v 'serialised_exception_list=apple:pear:cherry' 'BEGIN {
      _list_sz = split(serialised_exception_list, _list, ":")
      for(i = 1; i <= _list_sz; ++i) {
        exception[_list[i]]
      }
    }
    
    ! ($1 in exception) { print }' <<EOF
    apple Apples are yummy, I like them.
    pineapple Do you like pineapple?
    EOF

The output of this program is

    pineapple Do you like pineapple?


As a final example, we show how to wrap the **punishment** program into a shell script, as this illustrates how a shell script conveys parameters to an auxiliary **awk** script:

    #!/bin/sh
    
    usage()
    {
       cat <<EOF
    Usage: punishment [-c COUNT][-s SENTENCE]
     Prepare your punishments for you
    EOF
    }
    
    punishment_count='100'
    punishment_sentence='I shall not talk in class.'
    while getopts "c:hs:" OPTION; do
      case "${OPTION}" in
        c) punishment_count="${OPTARG}";;
        s) punishment_sentence="${OPTARG}";;
        h) usage; exit 0;;
        *) usage; exit 64;;
      esac
    done
    
    awk -v "count=${punishment_count}" -v "sentence=${punishment_sentence}" 'BEGIN {
      for(i = 1; i <= count; ++i) {
        print(sentence)
      }
      exit
    }'

## Local variables
The **awk** language does not directly support variables local to functions. It is however easy emulate them by adding extra arguments to functions.  It is traditional to prefix these variables by a `_` to indicate that they are not actual parameters.

We illustrate this technique with the definition of a `single_quote` function that adds single quotes around a string:

    # single_quote(TEXT)
    #  Return a string made of TEXT surrounded by single quotes

    function single_quote(text, _quote) {
      _quote = sprintf("%c", 39)
      return sprintf("%s%s%s", _quote, text, _quote);
    }

The simpler approach of using `sprintf("'%s'", text)` leads to practical problems because **awk** scripts are usually passed as single quoted arguments to the **awk** program.

## Assignment Arguments
Assignment arguments appear at the end of an `awk` invocation, in the same area as file variables, both `-v` assignments and argument assignments must match the following regular expression. (assuming a POSIX locale)

    ^[[:alpha:]_][[:alnum:]_]*=

The following example assumes a file `file` containing the following: `1 2 3` (white space separated)

    $ awk '{$1=$1}1' file OFS=, file OFS=- file
    1 2 3
    1,2,3
    1-2-3

