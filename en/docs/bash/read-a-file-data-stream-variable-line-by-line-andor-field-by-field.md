---
title: "Read a file (data stream, variable) line-by-line (andor field-by-field)?"
slug: "read-a-file-data-stream-variable-line-by-line-andor-field-by-field"
draft: false
images: []
weight: 9919
type: docs
toc: true
---

## Parameters
Parameter | Details
----------- | -------
IFS | Internal field separator
file | A file name/path
`-r` | Prevents backslash interpretation when used with read
`-t` | Removes a trailing newline from each line read by `readarray`
`-d DELIM` | Continue until the first character of DELIM is read (with `read`), rather than newline

## Looping through a file line by line
    while IFS= read -r line; do
       echo "$line"
    done <file

If file may not include a newline at the end, then:

    while IFS= read -r line || [ -n "$line" ]; do
       echo "$line"
    done <file

## Looping through the output of a command field by field
Let's assume that the field separator is `:`

    while IFS= read -d : -r field || [ -n "$field" ];do
        echo "**$field**"
    done < <(ping google.com)

Or with a pipe:

    ping google.com | while IFS= read -d : -r field || [ -n "$field" ];do
        echo "**$field**"
    done

## Read lines of a file into an array

    readarray -t arr <file

Or with a loop:

    arr=()
    while IFS= read -r line; do
       arr+=("$line")
    done <file



## Read lines of a string into an array
    var='line 1
    line 2
    line3'
    readarray -t arr <<< "$var"

or with a loop:


    arr=()
    while IFS= read -r line; do
       arr+=("$line")
    done <<< "$var"



## Looping through a string line by line
    var='line 1
    line 2
    line3'
    while IFS= read -r line; do
       echo "-$line-"
    done <<< "$var"

or

    readarray -t arr <<< "$var"
    for i in "${arr[@]}";do
        echo "-$i-"
    done

## Looping through the output of a command line by line
    while IFS= read -r line;do
        echo "**$line**"
    done < <(ping google.com)

or with a pipe:

    ping google.com |
    while IFS= read -r line;do
        echo "**$line**"
    done

## Read a file field by field
Let's assume that the field separator is `:` (colon) in the file *file*.

    while IFS= read -d : -r field || [ -n "$field" ]; do
       echo "$field"
    done <file

For a content:

    first : se
    con
    d:
        Thi rd:
        Fourth
The output is:

    **first **
    ** se
    con
    d**
    **
        Thi rd**
    **
        Fourth
    **



## Read a string field by field
Let's assume that the field separator is `:`

    var='line: 1
    line: 2
    line3'
    while IFS= read -d : -r field || [ -n "$field" ]; do
       echo "-$field-"
    done <<< "$var"

Output:

    -line-
    - 1
    line-
    - 2
    line3
    -



## Read fields of a file into an array
Let's assume that the field separator is `:`

    arr=()
    while IFS= read -d : -r field || [ -n "$field" ]; do
       arr+=("$field")
    done <file

## Read fields of a string into an array
Let's assume that the field separator is `:`

    var='1:2:3:4:
    newline'
    arr=()
    while IFS= read -d : -r field || [ -n "$field" ]; do
       arr+=("$field")
    done <<< "$var"
    echo "${arr[4]}"

Output:

    newline



## Reads file (/etc/passwd) line by line and field by field


