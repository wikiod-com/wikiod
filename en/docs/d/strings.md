---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

 - Strings in D are immutable; use `.dup` to make a mutable `char` array if you want to edit in-place.

## Reversing a string
`string` is defined as `alias string = immutable(char)[];`: so need to use `dup` to make a mutable char array, before it can be reversed:

    import std.stdio;
    import std.string;
    
    int main() {
    
        string x = "Hello world!";
        char[] x_rev = x.dup.reverse;
        
        writeln(x_rev); // !dlrow olleH
        
        return 0;
    
    }

## Test for an empty or null string
# Empty string 

Empty string is not null but has zero length:

```
string emptyString = "";
// an empty string is not null...
assert(emptyString !is null);

// ... but it has zero lenght
assert(emptyString.length == 0);
```

# Null string 

```
string nullString = null;
```

a null string is null (De Lapalisse)
```
assert(nullString is null);
```
but, unlike C#,  read the length of a null string does not generate error:

```
assert(nullString.length == 0);
assert(nullString.empty);
```


# Test  for empty or null
```

if (emptyOrNullString.length == 0) {
}

// or
if (emptyOrNullString.length) {
}

// or
import std.array;
if (emptyOrNullString.empty) {
}
```

# Test for null
```
if (nullString is null) {
}
```

# References
* [What is the correct way to test for an empty string?](http://forum.dlang.org/thread/airencixtruqcagfyvgu@forum.dlang.org#post-mailman.118.1374090368.22075.digitalmars-d-learn:40puremagic.com)
* [Does D has C#'s string.Empty?](http://forum.dlang.org/post/ruwzspnunatpmhoqglns@forum.dlang.org)

## Convert string to ubyte[] and vice versa
# String to immutable `ubyte[]`
```
string s = "unogatto";
immutable(ubyte[]) ustr = cast(immutable(ubyte)[])s;

assert(typeof(ustr).stringof == "immutable(ubyte[])");
assert(ustr.length == 8);
assert(ustr[0] == 0x75); //u
assert(ustr[1] == 0x6e); //n
assert(ustr[2] == 0x6f); //o
assert(ustr[3] == 0x67); //g
assert(ustr[7] == 0x6f); //o
```

# String to `ubyte[]`
```
string s = "unogatto";
ubyte[] mustr = cast(ubyte[])s;

assert(typeof(mustr).stringof == "ubyte[]");

assert(mustr.length == 8);
assert(mustr[0] == 0x75);
assert(mustr[1] == 0x6e);
assert(mustr[2] == 0x6f);
assert(mustr[3] == 0x67);
assert(mustr[7] == 0x6f);
```

# `ubyte[]`  to string
```
ubyte[] stream = [ 0x75, 0x6e, 0x6f, 0x67];
string us  = cast(string)stream;
assert(us == "unog");
```

# References
* [DLang forum](//http://forum.dlang.org/thread/k806qm$2eci$1@digitalmars.com)

