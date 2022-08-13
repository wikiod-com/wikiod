---
title: "Difference between grep, egrep, fgrep, pgrep."
slug: "difference-between-grep-egrep-fgrep-pgrep"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

**grep**, **egrep**, **fgrep**, **rgrep**, **pgrep** - are commands in Unix-like operating systems that print lines matching a pattern. The grep searches the named        input *FILEs* for lines containing a match to the given *PATTERN*. By default, it prints the matching lines. In addition, the variant programs **egrep**, **fgrep**, and rgrep are the same as **grep -E**, **grep -F**, and **grep -r**, respectively. These variants are deprecated, but are provided for backward compatibility.

## Syntax
- `grep [OPTIONS] PATTERN [FILE...]`

- `grep [OPTIONS] [-e PATTERN]...  [-f FILE]...  [FILE...]`

## Parameters
|**Symbol**| **Details Basic Regular Expressions (BRE)**|
| ------ | ------ |
|`^` |the circumflex is used to match the beginning of a line.|
|`$` | used to match the end of a line.|
|`.`| matches any character except a new line.|
|`[]`| matches single character inside the brackets. If there's a `^` inside, it  would match anything but the characters in the bracket. |
|`\` | before any of the non-alphanumeric characters quotes them. | 
|`*`| symbol matches the preceding character or subexpression zero, one or more times. |
|`\1` | backreferences 1-9 match the exact text by the corresponding group. |
|`\{m,n\}` | matches the preceding elements at least *m* and no more than *n* times.| 
|`\|`| `foo\|bar` matches foo or bar.|
|`\?`| short for \{0,1\} |
| `\+` |(short for **\{1,\}**) match the preceding character or subexpression at most 1 time, or at least 1 time respectively.|
|`\n`| matches a newline, `\t` matches a tab, etc.|
|`\w`| matches any word constituent and \W matches any character that isn't a word constituent.|
|`\<\>`| match the empty string only at the beginning or end of a word|
|`\b` | matches either and **\B** matches where \b doesn't.|
|**Symbol**| **Details Extended Regular Expressions (ERE)**|
|`^`|match only at the beginning |
|`$`| match only at the end of a line.|
|`.`| matches any character (or any character except a newline).|
|`[…]`| matches any one character listed inside the brackets (character set). Add an initial ^ and ranges work like in BRE (see above). |
|`(…)`| syntactic group, for use with * or \DIGIT replacements.|
|`\|`|for alternation: `foo`\|`bar` matches foo or bar.|
|`*`| matches the preceding character or subexpression a number of times: 0, 1 or more times|
|`+`|  matches 1 or more times preceding character.|
|`?`| matches preceding characters 0 or 1 times.| 
|`\`|Backslash quotes the next character if it is not alphanumeric.|
|`{m,n}`| matches the preceding character or subexpression between m and n times (missing from some implementations); n or m can be omitted, and `{m}` means exactly m|


**fgrep** stands for "Fixed-string Global Regular Expressions Print". **fgrep** is the same as **`grep -F`**. This commands is a faster grep and behaves as grep but does NOT recognize any regular expression meta-characters as being special. The search will complete faster because it only processes a simple string rather than a complex pattern.


*** 

**pgrep** is an acronym that stands for "Process-ID Global Regular Expressions Print". pgrep looks through the currently running processes and lists the process IDs which matches the selection criteria to stdout. pgrep is handy when all you want to know is the process id integer of a process.
 



|grep| egrep(grep -E)|fgrep(grep -F)|pgrep|
| ------ | ------ | ------ | ------ |
| [Basic Regular Expressions (BRE)][4]| [Extended Regular Expressions (ERE)][5]| Searches only strings | Searches process by name|




For more information and reference use some of the following links:

[What is the difference between grep, egrep and fgrep ? Unix&Linux StackExchange][1]


[Why does my regular expression work in X but not in Y? Unix&Linux StackExchange][2]


[What is the difference between grep, pgrep, egrep, fgrep? Superuser][3]


  [1]: https://unix.stackexchange.com/questions/17949/what-is-the-difference-between-grep-egrep-and-fgrep
  [2]: https://unix.stackexchange.com/questions/119905/why-does-my-regular-expression-work-in-x-but-not-in-y
  [3]: https://superuser.com/questions/508881/what-is-the-difference-between-grep-pgrep-egrep-fgrep

  [4]: https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended
  [5]: https://en.wikipedia.org/wiki/Regular_expression#POSIX_extended


## egrep with Extended Regular Expressions
    $ egrep '^(0|1)+ [a-zA-Z]+$' searchfile.txt
    011 AaBBS

## fgrep with no Regular expressions
    $ fgrep "." .bashrc
    # will match lines with a dot.

 

## grep with Basic Regular Expressions
    $ grep root /etc/passwd
    root:x:0:0:root:/root:/bin/bash
    operator:x:11:0:operator:/root:/sbin/nologin



## pgrep with name of process
    $ pgrep python
    1299

