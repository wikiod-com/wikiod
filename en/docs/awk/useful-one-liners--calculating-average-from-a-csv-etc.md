---
title: "Useful one-liners - calculating average from a CSV etc"
slug: "useful-one-liners---calculating-average-from-a-csv-etc"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Compute the average of values in a column from tabular data
Given a file using `;` as a column delimiter. We compute the mean of the values in the second column with the following program, the provided input is the list of grades of a student group:

    awk -F';' '{ sum += $2 } END { print(sum / NR) }' <<EOF
    Alice;2
    Victor;1
    Barbara;1
    Casper;4
    Deborah;0
    Ernest;1
    Fabiola;4
    Giuseppe;4
    EOF

The output of this program is `2.125`.

Remember that `NR` holds the number of the line being processed, in the `END` block it therefore hold the total number of lines in the file.

Remember that in many applications *(monitoring, statistics)*, the median is a much more useful information. See the corresponding example.

## Robust processing tabular data (CSV et al.)
Processing tabular data with **awk** is very easy, provided that the input is correctly formatted. Most software producing tabular data use specific features of this family of formats, and **awk** programs processing tabular data are often specific to a data produced by a specific software.  If a more generic or robust solution is required, most popular languages provide libraries accommodating with a lot of features found in tabular data:

 - optional column names on the first line
 - mixture of quoted and unquoted column values
 - various delimiters
 - localised formats for floating numbers

While it definitely possible to handle all these features cleanly and generically with **awk** this is probably not worth the effort.

## Exchanging two columns in tabular data
Given a file using `;` as a column delimiter.  Permuting the first and the second column is accomplished by

    awk -F';' -v 'OFS=;' '{ swap = $2; $2 = $1; $1 = swap; print }'


## Selecting specific columns in tabular data
We assume a file using ; as a column delimiter. Selecting a specific set of columns only requires a *print* statement. For instance, the following program selects the columns 3, 4 and 7 from its input:

    awk -F';' -v 'OFS=;' '{ print $3, $4, $7 }'

It is as usual possible to more carefully choose lines to print. The following program selects the columns 3, 4 and 7 from its input when the first field is `Alice` or `Bob`:

    awk -F';' -v 'OFS=;' '($1 == "Alice") || ($1 == "Bob") { print $3, $4, $7 }'


## Compute the median of values in a column from tabular data
Given a file using `;` as a column delimiter. We compute the median of the values in the second column with the following program, written for **GNU awk**. The provided input is the list of grades of a student group:

    gawk -F';' '{ sample[NR] = $2 }
     END {
       asort(sample);
       if(NR % 2 == 1) {
         print(sample[int(NR/2) + 1])
       } else {
         print(sample[NR/2])
       }
    }' <<EOF
    Alice;2
    Victor;1
    Barbara;1
    Casper;4
    Deborah;0
    Ernest;1
    Fabiola;4
    Giuseppe;4
    EOF

The output of this program is `1`.

Remember that `NR` holds the number of the line being processed, in the `END` block it therefore hold the total number of lines in the file.

Many implementations of **awk** do not have a function to sort arrays, which therefore need to be defined before the code above could be used.

## Selecting a set of lines between two patterns
Pattern matching can be used effectively with `awk` as it controls the actions that follows it i.e. `{ pattern } { action }`. One cool use of the pattern-matching is to select multiple between two patterns in a file say `patternA` and `patternB`

    $ awk '/patternA/,/patternB/' file

Assume my file contents are as follows, and I want to extract the lines only between the above pattern:-

    $ cat file
    This is line - 1
    This is line - 2
    patternA
    This is line - 3
    This is line - 4
    This is line - 5
    patternB
    This is line - 6

    $ awk '/patternA/,/patternB/' file
    patternA
    This is line - 3
    This is line - 4
    This is line - 5
    patternB

The above command doesn't do any specific `{ action }` other than to print the lines matching, but any specific actions within the subset of lines can be applied with an action block (`{}`).

