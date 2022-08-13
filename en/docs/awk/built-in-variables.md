---
title: "Built-in Variables"
slug: "built-in-variables"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

## FS - Field Separator
The variable `FS` is used to set the _input field separator_. In `awk`, space and tab act as default field separators. The corresponding field value can be accessed through `$1`, `$2`, `$3`... and so on.

    awk -F'=' '{print $1}' file

 - `-F` - command-line option for setting input field separator.

    
    awk 'BEGIN { FS="=" } { print $1 }' file


## OFS - Output Field Separator
This variable is used to set the _output field separator_ which is a space by default.

    awk -F'=' 'BEGIN { OFS=":" } { print $1 }' file

**Example:**

    $ cat file.csv 
    col1,col2,col3,col4
    col1,col2,col3
    col1,col2
    col1
    col1,col2,col3,col4,col5
    
    $ awk -F',' 'BEGIN { OFS="|" } { $1=$1 } 1' file.csv
    col1|col2|col3|col4
    col1|col2|col3
    col1|col2
    col1
    col1|col2|col3|col4|col5

Assigning `$1` to `$1` in `$1=$1` modifies a field (`$1` in this case) and that results in `awk` rebuilding the record `$0`. Rebuilding the record replaces the delimiters `FS` with `OFS`.

## FNR - The Current Record Number being processed
FNR contains the number of the input file row being processed. In this example you will see awk starting on 1 again when starting to process the second file.

 **Example with one file**

    $ cat file1
    AAAA
    BBBB
    CCCC
    $ awk '{ print FNR }' file1
    1
    2
    3

**Example with two files**

    $ cat file1
    AAAA
    BBBB
    CCCC
    $ cat file2
    WWWW
    XXXX
    YYYY
    ZZZZ
    $ awk '{ print FNR, FILENAME, $0 }' file1 file2
    1 file1 AAAA
    2 file1 BBBB
    3 file1 CCCC
    1 file2 WWWW
    2 file2 XXXX
    3 file2 YYYY
    4 file2 ZZZZ

**Extended example with two files**

`FNR` can be used to detect if awk is processing the first file since `NR==FNR` is true only for the first file. For example, if we want to join records from files `file1` and `file2` on their `FNR`:

    $ awk 'NR==FNR { a[FNR]=$0; next } (FNR in a) { print FNR, a[FNR], $1 }' file1 file2
    1 AAAA WWWW
    2 BBBB XXXX
    3 CCCC YYYY
Record `ZZZZ` from `file2` is missing as `FNR` has different max value for `file1` and `file2` and there is no join for differing `FNR`s.


## FS - Field Separator
Used by awk to split each record into multiple fields:

    echo "a-b-c
    d-e-f" | awk 'BEGIN {FS="-"} {print $2}'

will result in:

    b
    e

The variable `FS` can also be set using the option `-F`:

    echo "a-b-c
    d-e-f" | awk -F '-' '{print $2}'

By default, the fields are separated by whitespace (spaces and tabs) and multiple spaces and tabs count as a single separator.

## RS - Record Separator
Used by awk to split the input into multiple records. For example:

    echo "a b c|d e f" | awk 'BEGIN {RS="|"} {print $0}'

produces:

    a b c
    d e f

By default, the record separator is the newline character.

Similarly:
    echo "a b c|d e f" | awk 'BEGIN {RS="|"} {print $2}'

produces:

    b
    e


## OFS - Output Field Separator
Used by awk to separate fields output by the `print` statement. For example:

    echo "a b c
    d e f" | awk 'BEGIN {OFS="-"} {print $2, $3}'

produces:

    b-c
    e-f

The default value is ` `, a string consisting of a single space.


## ORS - Output Record Separator
Used by awk to separate records and is output at the end of every `print` statement. For example:

    echo "a b c
    d e f" | awk 'BEGIN {ORS="|"} {print $2, $3}'

produces:

    b c|e f

The default value is `\n` (newline character).


## ARGV, ARGC - Array of Command Line Arguments
Command line arguments passed to awk are stored in the internal array `ARGV` of `ARGC` elements. The first element of the array is the program name. For example:

    awk 'BEGIN {
       for (i = 0; i < ARGC; ++i) {
          printf "ARGV[%d]=\"%s\"\n", i, ARGV[i]
       }
    }' arg1 arg2 arg3

produces:

    ARGV[0]="awk"
    ARGV[1]="arg1"
    ARGV[2]="arg2"
    ARGV[3]="arg3"



## RS - Input Record Separator


## ORS - Output Record Separator


## NF - Number of Fields


## NR - Total Number of Records
Will provide the total number of records processed in the current `awk` instance.

    cat > file1
    suicidesquad
    harley quinn
    joker
    deadshot

    cat > file2
    avengers
    ironman
    captainamerica
    hulk

    awk '{print NR}' file1 file2
    1
    2
    3
    4
    5
    6
    7
    8

A total on 8 records were processed in the instance.

## FNR - Number of Records in File
Provides the total number of records processed by the `awk` instance relative to the files `awk` is processing

    cat > file1
    suicidesquad
    harley quinn
    joker
    deadshot
    
    cat > file2
    avengers
    ironman
    captainamerica
    hulk
    
    awk '{print FNR}' file1 file2
    1
    2
    3
    4
    1
    2
    3
    4

Each file had 4 lines each, so whenever `awk` encountered an `EOF` `FNR` was reset to 0.

## NF - Number of Fields
Provides the number of columns or fields in each record (record corresponds to each line). Each line is demarcated by `RS` which defaults to newline. 

    cat > file1
    Harley Quinn Loves Joker
    Batman Loves Wonder Woman
    Superman is not dead
    Why is everything I type four fielded!?

    awk '{print NF}' file1
    4
    4
    4
    7

`FS` (somewhere up there) defaults to tab or space. So Harley, Quinn, Loves, Joker are each considered as columns. The case holds for the next two lines, but the last line has 7 space separated words, which means 7 columns.

