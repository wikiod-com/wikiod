---
title: "Dictionaries"
slug: "dictionaries"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

Dictionaries in Tcl are _values_ that hold a mapping from arbitrary values to other arbitrary values. They were introduced in Tcl 8.5, though there are limited versions for (the now unsupported) Tcl 8.4. Dictionaries are syntactically the same as lists with even numbers of elements; the first pair of elements is the first key and value of the dictionary, the second pair is the second tuple.

Thus:

    fox "quick brown" dogs "lazy"

is a valid dictionary. The same key can be multiple times, but it is exactly as if the latter's value was in the earlier's value; these are the same dictionary:

    abcd {1 2 3} defg {2 3 4} abcd {3 4 5}

<!-- -->

    abcd {3 4 5} defg {2 3 4}

Whitespace is unimportant, just as with lists.

An important concept with dictionaries is iteration order; dictionaries try to use the key insertion order as their iteration order, though when you update the value for a key that already exists, you overwrite that key's value. New keys go on the end.

References: 
[dict][1]

  [1]: http://tcl.tk/man/tcl/TclCmd/dict.htm

## List-appending to a nested dictionary
If we have this dictionary:

    set alpha {alice {items {}} bob {items {}} claudia {items {}} derek {items {}}}

And want to add "fork" and "peanut" to Alice's items, this code won't work:

    dict lappend alpha alice items fork peanut
    dict get $alpha alice
    # => items {} items fork peanut

Because it would be impossible for the command to know where the key tokens end and the values to be list-appended start, the command is limited to one key token.

The correct way to append to the inner dictionary is this:

    dict with alpha alice {
        lappend items fork peanut
    }
    dict get $alpha alice
    # => items {fork peanut}

This works because the `dict with` command lets us traverse nested dictionaries, as many levels as the number of key tokens we provide. It then creates variables with the same names as the keys on that level (only one here: `items`). The variables are initialized to the value of the corresponding item in the dictionary. If we change the value, that changed value is used to update the value of the dictionary item when the script ends.

(Note that the variables continue to exist when the command has ended.)


## Basic use of a dictionary
Creating a dictionary:

    set mydict [dict create a 1 b 2 c 3 d 4]
    dict get $mydict b ; # returns 2
    set key c
    set myval [dict get $mydict $key]
    puts $myval
    # remove a value
    dict unset mydict b
    # set a new value
    dict set mydict e 5

Dictionary keys can be nested.

    dict set mycars mustang color green
    dict set mycars mustang horsepower 500
    dict set mycars prius-c color orange
    dict set mycars prius-c horsepower 99
    set car [dict get $mycars mustang]
    # $car is: color green horsepower 500
    dict for {car cardetails} $mycars {
      puts $car
      dict for {key value} $cardetails {
        puts "  $key: $value"
      }
    }


## The dict get command can raise an error
    set alpha {a 1 b 2 c 3}
    dict get $alpha b
    # => 2
    dict get $alpha d
    # (ERROR) key "d" not known in dictionary

If `dict get` is used to retrieve the value of a missing key, an error is raised. To prevent the error, use `dict exists`:

    if {[dict exists $alpha $key]} {
        set result [dict get $alpha $key]
    } else {
        # code to deal with missing key
    }

How to deal with a missing key of course depends on the situation: one simple way is to set the `result` to a default "empty" value.

If the code never attempts to retrieve other keys that are in the dictionary, `dict get` will of course not fail. But for arbitrary keys, `dict get` is an operation that needs to be guarded. Preferably by testing with `dict exists`, though exception catching will work too.

## Iterating over a dictionary
You can iterate over the contents of a dictionary with `dict for`, which is similar to `foreach`:

    set theDict {abcd {ab cd} bcde {ef gh} cdef {ij kl}}
    dict for {theKey theValue} $theDict {
        puts "$theKey -> $theValue"
    }

This produces this output:
<pre>
abcd -> ab cd
bcde -> ef gh
cdef -> ij kl
</pre>

You'd get the same output by using `dict keys` to list the keys and iterating over that:

    foreach theKey [dict keys $theDict] {
        set theValue [dict get $theDict $theKey]
        puts "$theKey -> $theValue"
    }

But `dict for` is more efficient.



