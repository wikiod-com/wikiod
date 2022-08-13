---
title: "Capture Groups"
slug: "capture-groups"
draft: false
images: []
weight: 9918
type: docs
toc: true
---

## Basic Capture Groups
A *group* is a section of a regular expression enclosed in parentheses `()`. This is commonly called "sub-expression" and serves two purposes:

 - It makes the sub-expression atomic, i.e. it will either match, fail or repeat as a whole.
 - The portion of text it matched is accessible in the remainder of the expression and the rest of the program.

***

Groups are numbered in regex engines, starting with 1. Traditionally, the maximum group number is 9, but many modern regex flavors support higher group counts. Group 0 always matches the entire pattern, the same way surrounding the entire regex with brackets would.

The ordinal number increases with each opening parenthesis, regardless of whether the groups are placed one-after-another or nested:

<!-- language: lang-none -->

    foo(bar(baz)?) (qux)+|(bla)
       1   2       3      4

<sup>groups and their numbers</sup>

After an expression achieves an overall match, all of its groups will be in use - whether a particular group has managed to match anything or not.

A group can be optional, like `(baz)?` above, or in an alternative part of the expression that was not used of the match, like `(bla)` above. In these cases, non-matching groups simply won't contain any information.

If a quantifier is placed behind a group, like in `(qux)+` above, the overall group count of the expression stays the same. If a group matches more than once, its content will be the last match occurrence. However, modern regex flavors allow accessing all sub-match occurrences.

***

If you wished to retrieve the date and error level of a log entry like this one:

<!-- language: lang-none -->

    2012-06-06 12:12.014 ERROR: Failed to connect to remote end

You could use something like this:

<!-- language: lang-none -->

    ^(\d{4}-\d{2}-\d{2}) \d{2}:\d{2}.\d{3} (\w*): .*$

This would extract the date of the log entry `2012-06-06` as capture group 1 and the error level `ERROR` as capture group 2.


## Named Capture Groups
Some regular expression flavors allow *named capture groups*. Instead of by a numerical index you can refer to these groups by name in subsequent code, i.e. in backreferences, in the replace pattern as well as in the following lines of the program.

Numerical indexes change as the number or arrangement of groups in an expression changes, so they are more brittle in comparison.

For example, to match a word (`\w+`) enclosed in either single or double quotes (`['"]`), we could use:

<!-- language: lang-none -->

    (?<quote>['"])\w+\k{quote}
    
Which is equivalent to:

<!-- language: lang-none -->

    (['"])\w+\1

In a simple situation like this a regular, numbered capturing group does not have any draw-backs.

In more complex situations the use of named groups will make the structure of the expression more apparent to the reader, which improves maintainability.

Log file parsing is an example of a more complex situation that benefits from group names. This is the [Apache Common Log Format][1] (CLF):

<!-- language: lang-none -->

    127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326

The following expression captures the parts into named groups:

<!-- language: lang-none -->

    (?<ip>\S+) (?<logname>\S+) (?<user>\S+) (?<time>\[[^]]+\]) (?<request>"[^"]+") (?<status>\S+) (?<bytes>\S+)

    
---

The syntax depends on the flavor, common ones are:

 - `(?<name>...)`
 - `(?'name'...)`
 - `(?P<name>...)`
 
Backreferences:

 - `\k<name>`
 - `\k{name}`
 - `\k'name'`
 - `\g{name}`
 - `(?P=name)`

---

In the .NET flavor you can have several groups sharing the same name, they will use [capture stacks](http://stackoverflow.com/a/17004406/3764814).

In PCRE you have to explicitly enable it by using the `(?J)` modifier (`PCRE_DUPNAMES`), or by using the branch reset group `(?|)`. Only the last captured value will be accessible though.

    (?J)(?<a>...)(?<a>...)
    (?|(?<a>...)|(?<a>...))


  [1]: https://httpd.apache.org/docs/1.3/logs.html#common

## Backreferences and Non-Capturing Groups
Since Groups are "numbered" some engines also support matching what a group has previously matched again.

Assuming you wanted to match something where two equals strings of length three are divided by a `$` you'd use:

    (.{3})\$\1

This would match any of the following strings:

    "abc$abc"
    "a b$a b"
    "af $af "
    "   $   "

If you want a group to not be numbered by the engine, You may declare it non-capturing. A non-capturing group looks like this:

    (?:)
    
They are particularly useful to repeat a certain pattern any number of times, since a group can also be used as an "atom". Consider:

    (\d{4}(?:-\d{2}){2} \d{2}:\d{2}.\d{3}) (.*)[\r\n]+\1 \2

This will match two logging entries in the adjacent lines that have the same timestamp and the same entry.

