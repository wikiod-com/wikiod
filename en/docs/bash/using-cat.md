---
title: "Using cat"
slug: "using-cat"
draft: false
images: []
weight: 9862
type: docs
toc: true
---

## Syntax
 - cat [OPTIONS]... [FILE]...



## Parameters
| Option | Details |  
| --------- | ------- |  
| -n | Print line numbers |  
| -v | Show non-printing characters using ^ and M- notation except LFD and TAB |
| -T | Show TAB characters as ^I |
| -E | Show linefeed(LF) characters as $ |
| -e | Same as -vE |
| -b | Number nonempty output lines, overrides -n |
| -A | equivalent to -vET |
| -s | suppress repeated empty output lines, s refers to squeeze

`cat` can read from both files and standard inputs and concatenates them to standard output

## Concatenate files
This is the primary purpose of `cat`.

    cat file1 file2 file3 > file_all

`cat` can also be used similarly to concatenate files as part of a pipeline, e.g.

    cat file1 file2 file3 | grep foo

## Printing the Contents of a File
    cat file.txt

will print the contents of a file.

If the file contains non-ASCII characters, you can display those characters symbolically with `cat -v`. This can be quite useful for situations where control characters would otherwise be invisible.

    cat -v unicode.txt

Very often, for interactive use, you are better off using an interactive pager like `less` or `more`, though. (`less` is far more powerful than `more` and it is advised to use `less` more often than `more`.)

    less file.txt

To pass the contents of a file as input to a command. An approach usually seen as better ([UUOC][1]) is to use redirection.

    tr A-Z a-z <file.txt   # as an alternative to cat file.txt | tr A-Z a-z

In case the content needs to be listed backwards from its end the command `tac` can be used:
    
    tac file.txt

If you want to print the contents with line numbers, then use `-n` with `cat`:

    cat -n file.txt

To display the contents of a file in a completely unambiguous byte-by-byte form, a hex dump is the standard solution.  This is good for very brief snippets of a file, such as when you don't know the precise encoding.  The standard hex dump utility is `od -cH`, though the representation is slightly cumbersome; common replacements include `xxd` and `hexdump`.

    $ printf 'Hëllö wörld' | xxd
    0000000: 48c3 ab6c 6cc3 b620 77c3 b672 6c64       H..ll.. w..rld


  [1]: https://en.wikipedia.org/wiki/Cat_(Unix)#Useless_use_of_cat

## Write to a file
    cat >file

It will let you write the text on terminal which will be saved in a file named *file*.

    cat >>file

will do the same, except it will append the text to the end of the file.

N.B: <kbd>Ctrl+D</kbd> to end writing text on terminal (Linux)
<hr>

A here document can be used to inline the contents of a file into a command line or a script:
    
    cat <<END >file
    Hello, World.
    END

The token after the `<<` redirection symbol is an arbitrary string which needs to occur alone on a line (with no leading or trailing whitespace) to indicate the end of the here document.  You can add quoting to prevent the shell from performing command substitution and variable interpolation:

    cat <<'fnord'
    Nothing in `here` will be $changed
    fnord

(Without the quotes, `here` would be executed as a command, and `$changed` would be substituted with the value of the variable `changed` -- or nothing, if it was undefined.)

## Display line numbers with output
Use the `--number` flag to print line numbers before each line. Alternatively, `-n` does the same thing.

    $ cat --number file
    
     1  line 1
     2  line 2
     3
     4  line 4
     5  line 5

To skip empty lines when counting lines, use the `--number-nonblank`, or simply `-b`.

    $ cat -b file

     1  line 1
     2  line 2

     3  line 4
     4  line 5

## Read from standard input
    cat < file.txt

Output is same as `cat file.txt`, but it reads the contents of the file from standard input instead of directly from the file.

    printf "first line\nSecond line\n" | cat -n 

The echo command before `|` outputs two lines. The cat command acts on the output to add line numbers.    

## Show non printable characters
This is useful to see if there are any non-printable characters, or non-ASCII characters.

e.g. If you have copy-pasted the code from web, you may have quotes like `”` instead of standard `"`.

    $ cat -v file.txt
    $ cat -vE file.txt # Useful in detecting trailing spaces.

e.g.

    $ echo '”     ' | cat -vE # echo | will be replaced by actual file.
    M-bM-^@M-^]     $

You may also want to use `cat -A` (A for All) that is equivalent to `cat -vET`.
It will display TAB characters (displayed as `^I`), non printable characters and end of each line:

    $ echo '” `' | cat -A
    M-bM-^@M-^]^I`$

## Concatenate gzipped files
Files compressed by `gzip` can be directly concatenated into larger gzipped files.

    cat file1.gz file2.gz file3.gz > combined.gz

This is a property of `gzip` that is less efficient than concatenating the input files and gzipping the result:

    cat file1 file2 file3 | gzip > combined.gz

A complete demonstration:

    echo 'Hello world!' > hello.txt
    echo 'Howdy world!' > howdy.txt
    gzip hello.txt 
    gzip howdy.txt
    
    cat hello.txt.gz howdy.txt.gz > greetings.txt.gz

    gunzip greetings.txt.gz
    
    cat greetings.txt

Which results in 

    Hello world!
    Howdy world!

Notice that `greetings.txt.gz` is a ***single file*** and is decompressed as the ***single file*** `greeting.txt`. Contrast this with `tar -czf hello.txt howdy.txt > greetings.tar.gz`, which keeps the files separate inside the tarball.

