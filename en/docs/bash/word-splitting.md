---
title: "Word splitting"
slug: "word-splitting"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
 - Set IFS to newline: IFS=$'\n'
 - Set IFS to nullstring: IFS=
 - Set IFS to / character: IFS=/

## Parameters
Parameter | Details
------- | -------
IFS | Internal field separator
-x | Print commands and their arguments as they are executed (Shell option)

* Word splitting is not performed during assignments e.g `newvar=$var`
* Word splitting is not performed in the `[[ ... ]]` construct
* Use double quotes on variables to prevent word splitting

## Bad effects of word splitting

    $ a='I am a string with spaces'
    $ [ $a = $a ] || echo "didn't match"
    bash: [: too many arguments
    didn't match

> `[ $a = $a ]` was interpreted as `[ I am a string with spaces = I am a string with spaces ]`. `[` is the `test` command for which `I am a string with spaces` is not a single argument, rather it's **6** arguments!!
    
    $ [ $a = something ] || echo "didn't match"
    bash: [: too many arguments
    didn't match

> `[ $a = something ]` was interpreted as `[ I am a string with spaces = something ]`

    $ [ $(grep . file) = 'something' ]
    bash: [: too many arguments
> The `grep` command returns a multiline string with spaces, so you can just imagine how many arguments are there...:D

*See [what, when and why](https://www.wikiod.com/bash/word-splitting#What, when and Why?) for the basics.*

## Splitting with IFS
To be more clear, let's create a script named `showarg`:

    #!/usr/bin/env bash
    printf "%d args:" $#
    printf " <%s>" "$@"
    echo

Now let's see the differences:

    $ var="This is an example"
    $ showarg $var
    4 args: <This> <is> <an> <example>

> `$var` is split into 4 args. `IFS` is white space characters and thus word splitting occurred in spaces

    $ var="This/is/an/example"
    $ showarg $var
    1 args: <This/is/an/example>

> In above word splitting didn't occur because the `IFS` characters weren't found.

 Now let's set `IFS=/`

    $ IFS=/
    $ var="This/is/an/example"
    $ showarg $var
    4 args: <This> <is> <an> <example>

> The `$var` is splitting into 4 arguments not a single argument.


## What, when and Why?
When the shell performs *parameter expansion*, *command substitution*, *variable or arithmetic expansion*, it scans for word boundaries in the result. If any word boundary is found, then the result is split into multiple words at that position. The word boundary is defined by a shell variable `IFS` (Internal Field Separator). The default value for IFS are space, tab and newline, i.e. word splitting will occur on these three white space characters if not prevented explicitly.

    set -x
    var='I am
    a
    multiline string'
    fun() {
        echo "-$1-"
        echo "*$2*"
        echo ".$3."
    }
    fun $var
In the above example this is how the `fun` function is being executed:

    fun I am a multiline string
> `$var` is split into 5 args, only `I`, `am` and `a` will be printed.

## IFS & word splitting
*See [what, when and why](https://www.wikiod.com/bash/word-splitting#What, when and Why?) if you don't know about the affiliation of IFS to word splitting*

**let's set the IFS to space character only:**

    set -x
    var='I am
    a
    multiline string'
    IFS=' '
    fun() {
        echo "-$1-"
        echo "*$2*"
        echo ".$3."
    }
    fun $var

This time word splitting will only work on spaces. The `fun` function will be executed like this:

    fun I 'am
    a
    multiline' string

> `$var` is split into 3 args. `I`, `am\na\nmultiline` and `string` will be printed

**Let's set the IFS to newline only:**

    IFS=$'\n'
    ...

Now the `fun` will be executed like:

    fun 'I am' a 'multiline string'

> `$var` is split into 3 args. `I am`, `a`, `multiline string` will be printed

**Let's see what happens if we set IFS to nullstring:**

    IFS=
    ...

This time the `fun` will be executed like this:

    fun 'I am
    a
    multiline string'

> `$var` is not split i.e it remained a single arg. 

**You can prevent word splitting by setting the IFS to nullstring**

**A general way of preventing word splitting is to use double quote:**

    fun "$var"
will prevent word splitting in all the cases discussed above i.e the `fun` function will be executed with only one argument.


## Usefulness of word splitting
There are some cases where word splitting can be useful:

*Filling up array:*

    arr=($(grep -o '[0-9]\+' file))

> This will fill up `arr` with all numeric values found in *file*

*Looping through space separated words:*

    words='foo bar baz'
    for w in $words;do
        echo "W: $w"
    done
Output:

    W: foo
    W: bar
    W: baz


*Passing space separated parameters which don't contain white spaces:*

    packs='apache2 php php-mbstring php-mysql'
    sudo apt-get install $packs

or

    packs='
    apache2
    php
    php-mbstring
    php-mysql
    '
    sudo apt-get install $packs

> This will install the packages. If you double quote the `$packs` then it will throw an error.

> Unquoetd `$packs` is sending all the space separated package names as arguments to `apt-get`, while quoting it will send the `$packs` string as a single argument and then `apt-get` will try to install a package named `apache2 php php-mbstring php-mysql` (for the first one) which obviously doesn't exist


*See [what, when and why](https://www.wikiod.com/bash/word-splitting#What, when and Why?) for the basics.*

## Splitting by separator changes


