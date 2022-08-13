---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
- regexp ?switches? exp string ?matchVar? ?subMatchVar subMatchVar ...?
- regsub ?switches? exp string subSpec ?varName?

This topic is not intended to discuss regular expressions themselves.   There are many resources on the internet explaining regular expressions and tools to help build regular expressions.

This topic will try to cover the common switches and methods of using regular expressions in Tcl and some of the differences between Tcl and other regular expression engines.

Regular expressions are generally slow.   The first question you should ask is "Do I really need a regular expression?".  Only match what you want.  If you don't need the other data, don't match it.

For the purposes of these regular expression examples, the -expanded switch will be used in order to be able to comment and explain the regular expression.


## Matching
The `regexp` command is used to match a regular expression against a string.

    # This is a very simplistic e-mail matcher.
    # e-mail addresses are extremely complicated to match properly.
    # there is no guarantee that this regex will properly match e-mail addresses.
    set mydata "send mail to john.doe.the.23rd@no.such.domain.com please"
    regexp -expanded {
        \y           # word boundary
        [^@\s]+      # characters that are not an @ or a space character
        @            # a single @ sign
        [\w.-]+      # normal characters and dots and dash
        \.           # a dot character
        \w+          # normal characters.
        \y           # word boundary
        } $mydata emailaddr
    puts $emailaddr
    john.doe.the.23rd@no.such.domain.com

The `regexp` command will return a 1 (true) value if a match was made or 0 (false) if not.

    set mydata "hello wrld, this is Tcl"
    # faster would be to use: [string match *world* $mydata] 
    if { [regexp {world} $mydata] } {
       puts "spelling correct"
    } else {
       puts "typographical error"
    }

To match all expressions in some data, use the -all switch and the -inline switch to return the data.  Note that the default is to treat newlines like any other data.

    # simplistic english ordinal word matcher.
    set mydata {
        This is the first line.
        This is the second line.
        This is the third line.
        This is the fourth line.
        }
    set mymatches [regexp -all -inline -expanded {
        \y                  # word boundary
        \w+                 # standard characters
        (?:st|nd|rd|th)     # ending in st, nd, rd or th
                            # The ?: operator is used here as we don't
                            # want to return the match specified inside
                            # the grouping () operator.
        \y                  # word boundary
        } $mydata]
    puts $mymatches
    first second third fourth
    # if the ?: operator was not used, the data returned would be:
    first st second nd third rd fourth th

Newline handling

    # find real numbers at the end of a line (fake data).
    set mydata {
        White 0.87 percent saturation.
        Specular reflection: 0.995
        Blue 0.56 percent saturation.
        Specular reflection: 0.421
        }
    # the -line switch will enable newline matching.
    # without -line, the $ would match the end of the data.
    set mymatches [regexp -line -all -inline -expanded {
        \y                  # word boundary
        \d\.\d+             # a real number
        $                   # at the end of a line.
        } $mydata]
    puts $mymatches
    0.995 0.421

Unicode requires no special handling.

    % set mydata {123ÂÃÄÈ456}
    123ÂÃÄÈ456
    % regexp {[[:alpha:]]+} $mydata match
    1
    % puts $match
    ÂÃÄÈ
    % regexp {\w+} $mydata match
    1
    % puts $match
    123ÂÃÄÈ456


Documentation: [regexp][1] [re_syntax][2]


  [1]: http://tcl.tk/man/tcl/TclCmd/regexp.htm
  [2]: http://tcl.tk/man/tcl/TclCmd/re_syntax.htm

## Mixing Greedy and Non-Greedy Quantifiers
If you have a greedy match as the first quantifier, the whole RE will be greedy,

If you have non-greedy match as the first quantifier, the whole RE will be non-greedy.

    set mydata {
        Device widget1: port: 156 alias: input2
        Device widget2: alias: input1 
        Device widget3: port: 238 alias: processor2
        Device widget4: alias: output2
        }
    regexp {Device\s(\w+):\s(.*?)alias} $mydata alldata devname devdata
    puts "$devname $devdata"
    widget1 port: 156 alias: input2
    regexp {Device\s(.*?):\s(.*?)alias} $mydata alldata devname devdata
    puts "$devname $devdata" 
    widget1 port: 156 

In the first case, the first \w+ is greedy, so all quantifiers are marked as greedy and the .*? matches more than is expected.

In the second case, the first .*? is non-greedy and all quantifiers are marked as non-greedy.

Other regular expression engines may not have an issue with greedy/non-greedy quantifiers, but they are much slower.

Henry Spencer [wrote][1]: *... The trouble is that it is very, very hard to write a generalization of
those statements which covers mixed-greediness regular expressions -- a
proper, implementation-independent definition of what mixed-greediness
regular expressions *should* match -- and makes them do "what people
expect".  I've tried.  I'm still trying.  No luck so far. ...*

  [1]: https://groups.google.com/d/msg/comp.lang.tcl/FddeFPbTFw8/UA3RwHwxk8QJ


## Substitution
The `regsub` command is used for regular expression matching and substitution.

    set mydata {The yellow dog has the blues.}
    # create a new string; only the first match is replaced.
    set newdata [regsub {(yellow|blue)} $mydata green]
    puts $newdata
    The green dog has the blues.
    # replace the data in the same string; all matches are replaced
    regsub -all {(yellow|blue)} $mydata red mydata
    puts $mydata
    The red dog has the reds.
    # another way to create a new string
    regsub {(yellow|blue)} $mydata red mynewdata
    puts $mynewdata
    The red dog has the blues.

Using back-references to reference matched data.

    set mydata {The yellow dog has the blues.}
    regsub {(yellow)} $mydata {"\1"} mydata
    puts $mydata
    The "yellow" dog has the blues.

Documentation: [regsub][1] [re_syntax][2]


  [1]: http://tcl.tk/man/tcl/TclCmd/regsub.htm
  [2]: http://tcl.tk/man/tcl/TclCmd/re_syntax.htm

## Differences Between Tcl's RE engine and other RE engines.
- \m : Beginning of a word.
- \M : End of a word.
- \y : Word boundary.
- \Y : a point that is not a word boundary.
- \Z : matches end of data.

Documentation: [re_syntax][1]


  [1]: http://tcl.tk/man/tcl/TclCmd/re_syntax.htm

## Matching a literal string with a regular expression
Sometimes you need to match a literal (sub-)string with a regular expression despite that substring containing RE metacharacters. While yes, it's possible to write code to insert appropriate backslashes to make that work (using `string map`) it is easiest to just prefix the pattern with `***=`, which makes the RE engine treat the rest of the string as just literal characters, disabling _all_ further metacharacters.

    set sampleText "This is some text with \[brackets\] in it."
    set searchFor {[brackets]}

    if {[ regexp ***=$searchFor $sampleText ]} {
        # This message will be printed
        puts "Found it!"
    }

Note that this also means you can't use any of the anchors.

