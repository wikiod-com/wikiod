---
title: "Regular expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Using different delimiters
Given a file like this:

    $ cat file
    hello/how/are/you
    i am fine

You can use `/pattern/` to match specific lines:

    $ sed -n '/hello/p' file
    hello/how/are/you

If the pattern contains slashes itself, you can use another delimiter using `\cBREc`:

    $ sed -n '\#hello/how#p' file
    hello/how/are/you
    $ sed -n '\_hello/how_p' file
    hello/how/are/you

As defined by POSIX in:

> [**Regular Expressions in sed**][1]  
In a context address, the construction `\cBREc`, where c is any character other than backslash or <newline>, shall be identical to `/BRE/`. If the character designated by c appears following a backslash, then it shall be considered to be that literal character, which shall not terminate the BRE. For example, in the context address "\xabc\xdefx", the second x stands for itself, so that the BRE is "abcxdef".


  [1]: http://pubs.opengroup.org/onlinepubs/009695399/utilities/sed.html#tag_04_126_13_02


