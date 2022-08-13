---
title: "Format Strings"
slug: "format-strings"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
 - io:format(FormatString, Args) % write to standard output
 - io:format(standard_error, FormatString, Args) % write to standard error
 - io:format(F, FormatString, Args) % write to open file
 - io_lib:format(FormatString, Args) % return an iolist


## Common control sequences in format strings
While [there are many different control sequences](http://erlang.org/doc/man/io.html#format-1) for `io:format` and `io_lib:format`, most of the time you'll use only three different ones: `~s`, `~p` and `~w`.

# ~s

The `~s` is for _strings_.

It prints strings, binaries and atoms.  (Anything else will cause a `badarg` error.)  It doesn't quote or escape anything; it just prints the string itself:

    %% Printing a string:
    > io:format("~s\n", ["hello world"]).
    hello world

    %% Printing a binary:
    > io:format("~s\n", [<<"hello world">>]).
    hello world

    %% Printing an atom:
    > io:format("~s\n", ['hello world']).
    hello world

# ~w

The `~w` is for _writing with standard syntax_.

It can can print any Erlang term.  The output can be parsed to return the original Erlang term, unless it contained terms that don't have a parsable written representation, i.e. pids, ports and references.  It doesn't insert any newlines or indentation, and strings are always interpreted as lists:

    > io:format("~w\n", ["abc"]).
    [97,98,99]

# ~p

The `~p` is for _pretty-printing_.

It can can print any Erlang term.  The output differs from `~w` in the following ways:

* Newlines are inserted if the line would otherwise be too long.
* When newlines are inserted, the next line is indented to line up with a previous term on the same level.
* If a list of integers looks like a printable string, it is interpreted as one.


    > io:format("~p\n", [{this,is,a,tuple,with,many,elements,'and',a,list,'of',numbers,[97,98,99],that,'end',up,making,the,line,too,long}]).
    {this,is,a,tuple,with,many,elements,'and',a,list,'of',numbers,"abc",that,
          'end',up,making,the,line,too,long}

If you don't want lists of integers to be printed as strings, you can use the `~lp` sequence (insert a lowercase letter L before `p`):

    > io:format("~lp\n", [[97,98,99]]).
    [97,98,99]

    > io:format("~lp\n", ["abc"]).
    [97,98,99]



