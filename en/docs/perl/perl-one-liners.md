---
title: "Perl one-liners"
slug: "perl-one-liners"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Execute some Perl code from command line
Simple one-liners may be specified as command line arguments to perl using the `-e` switch (think "execute"):

    perl -e'print "Hello, World!\n"'

Due to Windows quoting rules you can't use single-quoted strings but have to use one of these variants:

    perl -e"print qq(Hello, World!\n)"
    perl -e"print \"Hello, World!\n\""

Note that to avoid breaking old code, only syntax available up to Perl 5.8.x can be used with `-e`. To use anything newer your perl version may support, use `-E` instead. E.g. to use `say` available from 5.10.0 on plus Unicode 6.0 from >=v5.14.0 (also uses `-CO` to make sure `STDOUT` prints UTF-8):
<!-- if version [gte 5.14.0] -->
    perl -CO -E'say "\N{PILE OF POO}"'
<!-- end version if -->

## Using double-quoted strings in Windows one-liners
Windows uses only double quotes to wrap command line parameters. In order to use double quotes in perl one-liner (i.e. to print a string with an interpolated variable), you have to escape them with backslashes:

    perl -e "my $greeting = 'Hello'; print \"$greeting, world!\n\""

To improve readability, you may use a `qq()` operator:

    perl -e "my $greeting = 'Hello'; print qq($greeting, world!\n)"


## Print lines matching a pattern (PCRE grep)
    perl -ne'print if /foo/' file.txt

Case-insensitive:

    perl -ne'print if /foo/i' file.txt

## Replace a substring with another (PCRE sed)
    perl -pe"s/foo/bar/g" file.txt

Or in-place:

    perl -i -pe's/foo/bar/g' file.txt

On Windows:

    perl -i.bak -pe"s/foo/bar/g" file.txt

## Print only certain fields
    perl -lane'print "$F[0] $F[-1]"' data.txt
    # prints the first and the last fields of a space delimited record

CSV example:

    perl -F, -lane'print "$F[0] $F[-1]"' data.csv


## Print lines 5 to 10
    perl -ne'print if 5..10' file.txt

## Edit file in-place
Without a backup copy ([not supported on Windows](http://stackoverflow.com/a/2616900/1529709))

    perl -i -pe's/foo/bar/g' file.txt

With a backup copy `file.txt.bak`

    perl -i.bak -pe's/foo/bar/g' file.txt

With a backup copy `old_file.txt.orig` in the `backup` subdirectory (provided the latter exists):

    perl -i'backup/old_*.orig' -pe's/foo/bar/g' file.txt

## Reading the whole file as a string
    perl -0777 -ne'print "The whole file as a string: --->$_<---\n"'

_Note: The `-0777` is just a convention. Any `-0400` and above would de the same._

## Upload file into mojolicious
    perl -Mojo -E 'p("http://localhost:3000" => form => {Input_Type => "XML", Input_File => {file => "d:/xml/test.xml"}})'

File `d:/xml/test.xml` will be uploaded to server which listen connections on `localhost:3000` ([Source](http://stackoverflow.com/a/37675542/4632019))

In this example:

  `-Mmodule` executes `use module;` before executing your program  
  `-E commandline` is used to enter one line of program  
  If you have no `ojo` module you can use `cpanm ojo` command to install it

To read more about how to run perl use `perldoc perlrun` command or read [here](http://perldoc.perl.org/perlrun.html)

