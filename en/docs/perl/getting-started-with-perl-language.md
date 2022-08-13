---
title: "Getting started with Perl Language"
slug: "getting-started-with-perl-language"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Getting started with Perl
Perl tries to do what you mean:

    print "Hello World\n";

The two tricky bits are the semicolon at the end of the line and the `\n`, which adds a newline (line feed). If you have a relatively new version of perl, you can use `say` instead of `print` to have the carriage return added automatically:

<!-- if version [gte 5.10.0] -->
    use feature 'say';
    say "Hello World";

The say feature is also enabled automatically with a `use v5.10` (or higher) declaration:

    use v5.10;
    say "Hello World";

<!-- end version if -->

It's pretty common to just use [perl on the command line](http://perldoc.perl.org/perlrun.html) using the `-e` option:

    $ perl -e 'print "Hello World\n"'
    Hello World

Adding the `-l` option is one way to print newlines automatically:

    $ perl -le 'print "Hello World"'
    Hello World

<!-- if version [gte 5.10.0] -->

If you want to enable [new features](http://perldoc.perl.org/feature.html), use the `-E` option instead:

    $ perl -E 'say "Hello World"'
    Hello World
<!-- end version if -->

You can also, of course, save the script in a file. Just remove the `-e` command line option and use the filename of the script: `perl script.pl`. For programs longer than a line, it's wise to turn on a couple of options:

    use strict;
    use warnings;

    print "Hello World\n";

There's no real disadvantage other than making the code slightly longer. In exchange, the strict pragma prevents you from using code that is potentially unsafe and warnings notifies you of many common errors. 

Notice the line-ending semicolon is optional for the last line, but is a good idea in case you later add to the end of your code.

For more options how to run Perl, see [perlrun](http://perldoc.perl.org/perlrun.html)  or type `perldoc perlrun` at a command prompt. For a more detailed introduction to Perl, see [perlintro](http://perldoc.perl.org/perlintro.html) or type `perldoc perlintro` at a command prompt. For a quirky interactive tutorial, [Try Perl](http://tryperl.pl).

