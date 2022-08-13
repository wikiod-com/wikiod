---
title: "String manipulation functions"
slug: "string-manipulation-functions"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Syntax
- index(big, little)
- length or length()
- length(string)
- match(string, regex)
- split(string, array, separator)
- split(string, array)
- sprintf(format, ...)
- sub(regex, subst, string)
- sub(regex, subst)
- gsub(regex, subst)
- gsub(regex, subst, string)
- substr(string, start, end)
- substr(string, start)
- tolower(string)
- toupper(string)

## Parameters
| Parameter | Details                                              |
| --------- | ---------------------------------------------------- |
| big       | The string which is scanned for "little".            |
| end       | The index at which to end the sub-string.            |
| format    | A [`printf`][sodoc-c-printf] format string.          |
| little    | The string to scan for in "big".                     |
| regex     | An [Extended-Regular-Expression][sodoc-awk-regex].   |
| start     | The index at which to start the sub-string.          |
| string    | A string.                                            |
| subst     | The string to substitute in for the matched portion. |

[sodoc-awk-regex]: https://www.wikiod.com/awk/patterns
[sodoc-c-printf]: https://www.wikiod.com/c/formatted-inputoutput

## Converting string to upper case
The function `toupper` will convert a string to upper case (capital letters). For example:

    BEGIN {
        greeting = "hello"
        loud_greeting = toupper(greeting)
        print loud_greeting
    }

This code will output "HELLO" when run.

## Computing a hash of a string
While implementing one of the standard hashing algorithm in **awk** is probably a tedious task, defining a *hash* function that can be used as a handle to text documents is much more tractable.  A practical situation where such a function is useful is to assign short ids to items given their description, for instance test cases, so that the short id can be given as reference to the item by the user instead of supplying its long description.

The *hash* function needs to convert characters to numeric codes, which is accomplished by using a lookup table initialised at the beginning of the script. The *hash* function is then computed using modular arithmetic transformations, a very classical approach to the computation of hashes.

For demonstration purposes, we add a rule to decorate input lines with their hash, but this rule is not needed to use the function:

    BEGIN{
      for(n=0;n<256;n++) {
        ord[sprintf("%c",n)] = n
      }
    }
    
    function hash(text, _prime, _modulo, _ax, _chars, _i)
    {
      _prime = 104729;
      _modulo = 1048576;
      _ax = 0;
      split(text, _chars, "");
      for (_i=1; _i <= length(text); _i++) {
        _ax = (_ax * _prime + ord[_chars[_i]]) % _modulo;
      };
      return sprintf("%05x", _ax)
    }

    # Rule to demonstrate the function
    #  These comments and the following line are not relevant
    #  to the definition of the hash function but illustrate
    #  its use.

    { printf("%s|%s\n", hash($0), $0) }

We save the program above to the file `hash.awk` and demonstrate it on a short list of classical english book titles:

    awk -f hash.awk <<EOF
    Wuthering Heights
    Jane Eyre
    Pride and Prejudice
    The Mayor of Casterbridge
    The Great Gatsby
    David Copperfield
    Great Expectations
    The Return of the Soldier
    Alice's Adventures in Wonderland
    Animal Farm
    EOF

The output is

    6d6b1|Wuthering Heights
    7539b|Jane Eyre
    d8fba|Pride and Prejudice
    fae95|The Mayor of Casterbridge
    17fae|The Great Gatsby
    c0005|David Copperfield
    7492a|Great Expectations
    12871|The Return of the Soldier
    c3ab6|Alice's Adventures in Wonderland
    46dc0|Animal Farm

When applied on each of the 6948 non-blank lines of [my favourite novel](http://www.gutenberg.org/cache/epub/32596/pg32596.txt) this hash function does not generate any collision.


## String Concatenation
String concatenation is done simply by writing expressions next to one another without any operator. For example:

    BEGIN {
       user = "root"
       print "Hello "user "!"
    }

will print: `Hello root!`

Note that expressions do not have to be separated by whitespace.


## String text substitution
SUB function allows to substitute text inside awk

> sub(regexp, replacement, target)

where regexp could be a full [regular expression][1]



    $ cat file
    AAAAA
    BBBB
    CCCC
    DDDD
    EEEE
    FFFF
    GGGG
    $ awk '{sub("AAA","XXX", $0); print}' file
    XXXAA
    BBBB
    CCCC
    DDDD
    EEEE
    FFFF
    GGGG


  [1]: http://www.regular-expressions.info/

## Convert string to lower case
AWK often used for manipulating entire files containing a list of strings. Let's say file  *awk_test_file.txt* contains:

    First String
    Second String
    Third String

To convert all the strings to lower case execute:

    awk '{ print tolower($0) }' awk_test_file.txt

This will result:

    first string
    second string
    third string
    

## Substring extraction
`GNU` awk supports a sub-string extraction function to return a fixed length character sequence from a main string. The syntax is

    *substr(string, start [, length ])* 

where, `string` is source string and `start` marks the start of the sub-string position you want the extraction to be done for an optional length `length` characters. If the length is not specified, the extraction is done up to the end of the string.

The first character of the string is treated as character number one.

    awk '
    BEGIN {
        testString = "MyTESTstring"
        substring  =  substr(testString, 3, 4)    # Start at character 3 for a length of 4 characters
        print substring
    }'
     
will output the sub-string `TEST`.

    awk '
    BEGIN {
        testString = "MyTESTstring"
        substring  =  substr(testString, 3)    # Start at character 3 till end of the string
        print substring
    }'

this extracts the sub-string from character position 3 to end of the whole string, returning `TESTstring`

Note:-

 - If `start` is given a negative value, `GNU` awk prints the whole string and if `length` is given a non-zero value `GNU` awk behavior returns a `null` string and the behavior varies among different implementations of `awk`.  

