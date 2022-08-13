---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Matching strings
The `=~` operator attempts to match a regular expression (set apart by `/`) to a string:

    my $str = "hello world";
    print "Hi, yourself!\n" if $str =~ /^hello/;

`/^hello/` is the actual regular expression. The `^` is a special character that tells the regular expression to start with the beginning of the string and not match in the middle somewhere. Then the regex tries to find the following letters in order `h`, `e`, `l`, `l`, and `o`.

Regular expressions attempt to match the default variable (`$_`) if bare:

    $_ = "hello world";

    print "Ahoy!\n" if /^hello/;

You can also use different delimiters is you precede the regular expression with the `m` operator:

    m~^hello~;
    m{^hello}; 
    m|^hello|;

This is useful when matching strings that include the `/` character:

    print "user directory" if m|^/usr|;


## Replace a string using regular expressions
    s/foo/bar/;         # replace "foo" with "bar" in $_
    my $foo = "foo";
    $foo =~ s/foo/bar/; # do the above on a different variable using the binding operator =~
    s~ foo ~ bar ~;     # using ~ as a delimiter
    $foo = s/foo/bar/r; # non-destructive r flag: returns the replacement string without modifying the variable it's bound to
    s/foo/bar/g;        # replace all instances

## Usage of \Q and \E in pattern matching

What's between \Q and \E is treated as normal characters
------------------------------------------------------------------------   

    #!/usr/bin/perl

    my $str = "hello.it's.me";
    
    my @test = (
      "hello.it's.me",
        "hello/it's!me",
        );
    
    sub ismatched($) { $_[0] ? "MATCHED!" : "DID NOT MATCH!" }
    
    my @match = (
          [ general_match=> sub { ismatched /$str/ } ],
          [ qe_match    => sub { ismatched /\Q$str\E/ } ],
          );
    
    for (@test) {
        print "\String = '$_':\n";

    foreach my $method (@match) {
        my($name,$match) = @$method;
        print "  - $name: ", $match->(), "\n";
    }
}

Output

>     String = 'hello.it's.me':
>       - general_match: MATCHED!
>       - qe_match: MATCHED!
>     String = 'hello/it's!me':
>       - general_match: MATCHED!
>       - qe_match: DID NOT MATCH!

## Parsing a string with a regex


