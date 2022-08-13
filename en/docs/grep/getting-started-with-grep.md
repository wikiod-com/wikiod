---
title: "Getting started with grep"
slug: "getting-started-with-grep"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Find text within a given directory, recursively
Using GNU grep
-------------


    grep -r 'pattern' <directory path>

To also list line numbers of matches use `-n` option

    grep -rn 'pattern' <directory path>

To search only files with particular [glob](https://www.wikiod.com/bash/pattern-matching-and-regular-expressions) pattern 

    grep --include='*.txt' -r 'pattern' <directory path>

Exclude file patterns or directories

    grep -R --exclude=*.log 'pattern' <directory path>
    grep -R --exclude={*.log,*.class} 'pattern' <directory path>

    grep -R --exclude-dir=tmp 'pattern' <directory path>
    grep -R --exclude-dir={tmp,lib} 'pattern' <directory path>

**Notes and other useful options**
*  `<directory path>` can be skipped if searching in current directory
* The `-R` options follows all symbolic links, unlike `-r` which follows symbolic links only if they are on the
              command line
* `-l` to only list matching files
* `-h` to suppress filename prefix
* `--color=auto` to highlight matched patterns
* `-m <num>` to specify maximum number of matches for each file input

<br>

POSIX workaround to search recursively
--------------------------------------

    find <directory path> -type f -exec grep -l 'pattern' {} +

* Options like `-n` , `-l` , etc can be used as required
* If `{} +` is not supported, use `{} \;` instead 
* See [find](https://www.wikiod.com/bash/find) documentation for more help on `find` command like how to include/exclude file types, directories etc

## Basic usage


## Ignore case
Given a file `sample`:

    hello
    Hello
    HELLO_there

A normal `grep` for "hello" returns:

    $ grep "hello" sample 
    hello

Using `-i` allows to ignore case and match any "hello":

    $ grep -i "hello" sample
    hello
    Hello
    HELLO_there


## Match whole words
Given a file `sample`:

    hello world
    ahello here
    hello_there

A normal `grep` for "hello" returns:

    $ grep hello sample 
    hello world
    ahello here
    hello_there

Using `-w` allows to select those lines containing matches that form whole words:

    $ grep -w hello sample 
    hello world




## Grep Context Control
Given a file Sample called movieslist.

    Troy
    Gladiator
    Robin Hood
    King Arthur
    BraveHeart
    The Last Samurai

Normal grep returns

    grep "Gladiator" movieslist
    Gladiator

Now,using grep to print the below or above lines of the file.

**To print the below line**

    grep -A 1 Gladiator movieslist 
    Gladiator
    Robin Hood

**To print the above line**

    grep -B 1 Gladiator movieslist 
    Troy
    Gladiator

**To print both**

    grep -C 1 Gladiator movieslist 
    Troy
    Gladiator
    Robin Hood



## Prints only the matching part of the lines
    
    echo "Prints only the matching part of the lines" | grep -o "matching"
    # prints matching

