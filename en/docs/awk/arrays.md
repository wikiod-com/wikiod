---
title: "Arrays"
slug: "arrays"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Array basics
Creating a new array is slightly confusing, as there is no real identifier for an array in awk. So, an array cannot really be initialised with our AWK code. 

An array in awk is associative, meaning that any string or number can be a key. This means that the array is more like a key-value pair dictionary, map etc. On another note, the arrays do not have a maximum size.

Creating an array in AWK is really easy, as you take a variable name, a proper key and assign it to a variable. This means when the following code is executed, we already have created an array called `myArray`:

    BEGIN {
        myArray["key"] = "value"
    }

We our not bound to creating arrays in the begin only. Lets say we have the following input stream:

    A b c
    D e f
    G h i

And execute the following code with this:

    {
        myOtherArray[$1] = $2 "-" $3
    }
    # The array will look like this:
    # myOtherArray["A"] = "b-c"
    # myOtherArray["D"] = "e-f"
    # myOtherArray["G"] = "h-i"

When an array is filled with key value pairs, one can retrieve the value with the key only. This means that if we use key `"A"` in `myOtherArray` we get `"b-c"`.

    END {
        print(myOtherArray["A"])
    }

We also have the option to loop through each key to get each value. Looping through each key of an array is a simple thing to do, however it has on downfall: it is unsorted. 
The following loop is like a for-each loop, which retrieves the key:

    END {
        for (key in myOtherArray) {
            print "myOtherArray[\"" key "\"] = " myOtherArray[key] 
        }
    }
    # Outputs (literal strings):
    myOtherArray["A"] = "b-c"
    myOtherArray["D"] = "e-f"
    myOtherArray["G"] = "h-i"


