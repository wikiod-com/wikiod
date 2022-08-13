---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

In Progress ABL there are two types of strings, those defined as `CHARACTER` and those defined as `LONGCHAR`. A file larger than 32K in length is a `LONGCHAR`. Most strings are unless specified any other way case insensitive.

Remember - all positions start with the position 1!

## Concatenating strings
Using the `+` operator you can easily concatenate two or more strings.

    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    
    cString = "HELLO".
    
    cString = cString + " " + "GOODBYE".
    
    DISPLAY cString FORMAT "X(20)".

## CASE-SENSITIVE strings
All strings in Progress ABL are case sensitive unless specified otherwise.

This example will display a message box saying that the strings are identical.

    DEFINE VARIABLE str1 AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE str2 AS CHARACTER   NO-UNDO.
    
    str1 = "abc".
    str2 = "ABC".
    
    IF str1 = str2 THEN 
        MESSAGE "The strings are identical" VIEW-AS ALERT-BOX.
    
To declare a string case sensitive you just add the attribute `CASE-SENSITIVE`

    DEFINE VARIABLE str1 AS CHARACTER   NO-UNDO CASE-SENSITIVE.
    DEFINE VARIABLE str2 AS CHARACTER   NO-UNDO.
    
    str1 = "abc".
    str2 = "ABC".
    
    IF str1 = str2 THEN 
        MESSAGE "The strings are identical" VIEW-AS ALERT-BOX.
    ELSE 
        MESSAGE "There's a difference" VIEW-AS ALERT-BOX.

(It's enough that one of the strings has it in this case).

## Defining, assing  and displaying a string
Generally you should always define all variable and parameters as `NO-UNDO` unless you really need to.

    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    
    cString = "HELLO".
    
    DISPLAY cString.

## String manipulation
There are a couple of useful built in functions for working with string. All functions working with the position of characters start with index 1 as the first character, not 0 as is common in many languages.

**STRING** - converts any value to a string

This example converts the integer 2000 to the string "2000".

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    i = 2000.
    
    c = STRING(i).
    
    DISPLAY c.

**CHR** and **ASC** - converts single characters to and from ascii.

> CHR(integer) 
>
>Returns the character representation for ascii code integer
>
> ASC(character)
>
>Returns the ascii integer value for the character

    DEFINE VARIABLE ix     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE letter AS CHARACTER NO-UNDO FORMAT "X(1)" EXTENT 26.
    
    DO ix = 1 TO 26:
      letter[ix] = CHR((ASC("A")) - 1 + ix).
    END.
    
    DISPLAY SKIP(1) letter WITH 2 COLUMNS NO-LABELS
      TITLE "T H E  A L P H A B E T".

 

**LENGTH** - returns the length of a string 

>LENGTH(string). //Returns an integer with the length of the string.

    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    
    cString = "HELLO".
    
    MESSAGE "The string " cString " is " LENGTH(cString) " characters long" VIEW-AS ALERT-BOX. 

**SUBSTRING** - returns or assigns a part of a string

>- SUBSTRING(string, starting-position, length). 
>
>Returns "length" characters from "string" starting on position "starting-position".

>- SUBSTRING(string, starting-position).
> 
>Returns the rest of "string", starting at position "starting-position"

    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    
    cString = "ABCDEFGH".
    
    DISPLAY SUBSTRING(cString, 4, 2). //Displays "DE" 
    DISPLAY SUBSTRING(cString, 4). //Displays "DEFGH"

Substring can also be used to overwrite a part of a string. Use the same syntax but assign that substring instead:

    DEFINE VARIABLE cString AS CHARACTER   NO-UNDO.
    
    cString = "ABCDEFGH".
     
    SUBSTRING(cString, 4, 2) = "XY". //Replaces position 4 and 5 with "XY" 
    
    DISPLAY cString.  

There's also a similar function called `OVERLAY` this example from the Progress documentation covers the differences between `OVERLAY`and `SUBSTRING`:

    /* This procedure illustrates the differences between the SUBSTRING and
       OVERLAY statements. */
    DEFINE VARIABLE cOriginal  AS CHARACTER NO-UNDO INITIAL "OpenEdge".
    DEFINE VARIABLE cSubstring AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOverlay   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResults   AS CHARACTER NO-UNDO.
    
    /* Default behavior without optional LENGTH. */
    ASSIGN
     cSubstring              = cOriginal
     SUBSTRING(cSubstring,2) = "***"
     cOverlay                = cOriginal
     OVERLAY(cOverlay,2)     = "***"
     cResults                = "target = ~"OpenEdge~". ~n~n" 
      + "If you do not supply a length, SUBSTRING and OVERLAY default as follows:
      ~n~n" + "SUBSTRING(target,2) = ~"***~"  yields:  " + cSubstring + ". ~n" 
      + "OVERLAY(target,2)     = ~"***~"  yields:  " + cOverlay + ".".
    
    /* Behavior with zero LENGTH. */
    ASSIGN
     cSubstring                = cOriginal
     SUBSTRING(cSubstring,2,0) = "***"
     cOverlay                  = cOriginal
     OVERLAY(cOverlay,2,0)     = "***"
     cResults                  = cResults + "~n~n" 
      + "For a zero length, SUBSTRING and OVERLAY behave as follows:  ~n~n" 
      + "SUBSTRING(target,2,0) = ~"***~"  yields:  " + cSubstring + ". ~n" 
      + "OVERLAY(target,2,0)     = ~"***~"  yields:  " + cOverlay + ".".
    
    /* Behavior with LENGTH < replacement. */
    ASSIGN 
     cSubstring                = cOriginal
     SUBSTRING(cSubstring,2,1) = "***"
     cOverlay                  = cOriginal
     OVERLAY(cOverlay,2,1)     = "***"
     cResults                  = cResults + "~n~n" 
      + "For a length shorter than the replacement, SUBSTRING and OVERLAY behave
      as follows: ~n~n" + "SUBSTRING(target,2,1) = ~"***~"  yields:  " 
      + cSubstring + ". ~n" + "OVERLAY(target,2,1)     = ~"***~"  yields:  " 
      + cOverlay + ".".
    
    /* Behavior with LENGTH = replacement. */
    ASSIGN 
     cSubstring                = cOriginal
     SUBSTRING(cSubstring,2,3) = "***"
     cOverlay                  = cOriginal
     OVERLAY(cOverlay,2,3)     = "***"
     cResults                  = cResults + "~n~n" 
      + "For a length equal to the replacement, SUBSTRING and OVERLAY behave as
      follows:  ~n~n" + "SUBSTRING(target,2,3) = ~"***~"  yields:  " 
      + cSubstring + ". ~n" + "OVERLAY(target,2,3)     = ~"***~"  yields:  " 
      + cOverlay + ".".
    
    /* Behavior with LENGTH > replacement. */
    ASSIGN 
     cSubstring                = cOriginal
     SUBSTRING(cSubstring,2,6) = "***"
     cOverlay                  = cOriginal
     OVERLAY(cOverlay,2,6)     = "***"
     cResults                  = cResults + "~n~n" 
      + "For a length greater than the replacement, SUBSTRING and OVERLAY behave
      as follows:  ~n~n" + "SUBSTRING(target,2,6) = ~"***~"  yields:  " 
      + cSubstring + ". ~n" + "OVERLAY(target,2,6)     = ~"***~"  yields:  " 
      + cOverlay + ".".
    
    MESSAGE cResults VIEW-AS ALERT-BOX.

**INDEX** - return the position of a string in a string.

`R-INDEX` will to the same thing but search right to left.

>INDEX(source, target)
>
> Search target within source (left to right) and return it's position. If it's missing return 0.
> 
> INDEX(source, target, starting-position).
>
> Same as above but start searching at starting-position

    DEFINE VARIABLE str AS CHARACTER   NO-UNDO.
    
    str = "ABCDEFGH".
    
    DISPLAY INDEX(str, "cd") INDEX(str, "cd", 4). //Will display 3 and 0

**REPLACE** - replaces a string within a string.

>REPLACE(string, from-string, to-string)
>
>Replaces from-string with to-string in string. From-string and to-string don't need to be of the same length, to-string can also be nothing ("") to remove a character.

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    c = "ELLO".
    
    DISPLAY REPLACE(c, "E", "HE"). // Displays "HELLO"
    
    c = "ABABABA".
    
    DISPLAY REPLACE(c, "B", ""). // Remove all Bs

**TRIM** - removes leading and trailing whitespaces  (or other characters).

This can be useful when cleaning up indata. 

> TRIM(string)
>
> Removes all leading and trailing spaces, tabs, line feeds, carriage returns.  
>
>TRIM(string, character).
>
> Removes all leading and trailing "characters".

`LEFT-TRIM` and `RIGHT-TRIM` does the same thing but only leading or trailing.

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    c = "__HELLO_WORLD_____".
    
    DISPLAY TRIM(c, "_").
    /*Displays HELLO_WORLD without all the leading and 
    trailing underscores but leaves the one in the middle.
    REPLACE would have removed that one as well */

**SUBSTITUTE** - substitutes paramters in a string.

    SUBSTITUTE is a limited function for replacing up to nine preformatted parameters in a string.

> SUBSTITUTE(string, param1, param2, ..., param9).

The parameters must be in the format `&1` to `&9`.

If you want to use an ampersand in the string (and not use it as a parameter) escape it with another ampersand: `&&`.

    DEFINE VARIABLE str AS CHARACTER   NO-UNDO.
                                          
    str = "&1 made &2 goals in &3 games playing for &4".
    
    MESSAGE SUBSTITUTE(str, "Zlatan Ibrahimovic", 113, 122, "Paris Saint-Germain") VIEW-AS ALERT-BOX.
    MESSAGE SUBSTITUTE(str, "Mats Sundin", 555, 1305, "Toronto Maple Leafs") VIEW-AS ALERT-BOX.

A parameter can appear more than once in a string, all will be replaced:

    MESSAGE SUBSTITUTE("&1 &2 or not &1 &2", "To", "Be") VIEW-AS ALERT-BOX.





## BEGINS and MATCHES
**BEGINS** - returns TRUE if one string *begins* with another string.

> string1 BEGINS string2
>
> If string1 BEGINS with (or is equal to) string2 this will return true. Otherwise it will return false. If string two is empty ("") it will always return true.

BEGINS is very useful in queries where you want to search the beginning of something, for instance a name. But it's basically a function working on strings.

    DEFINE VARIABLE str AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE beg AS CHARACTER   NO-UNDO.
    
    str = "HELLO".
    beg = "HELLO".
    DISPLAY str BEGINS beg. // yes
    
    str = "HELLO".
    beg = "H".
    DISPLAY str BEGINS beg. // yes
    
    str = "HELLO".
    beg = "".
    DISPLAY str BEGINS beg. // yes
    
    
    str = "HELLO".
    beg = "HELLO WORLD".
    DISPLAY str BEGINS beg. // no

**MATCHES**  returns true if certain wildcard critera is met in a string.

> string1 MATCHES expression
>
> Returns true if string1 matches the wildcard expression:
>
> \* (asterisk) = 0 to n characters (basically any string of any length) 
> 
> \. (period) = wildcard for any character (except null)

    DEFINE VARIABLE str AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE beg AS CHARACTER   NO-UNDO.
    
    str = "HELLO".
    beg = "HELLO".
    DISPLAY str MATCHES beg. // yes
    
    str = "HELLO".
    beg = "H*".
    DISPLAY str MATCHES beg. // yes
    
    str = "HELLO".
    beg = "*O".
    DISPLAY str MATCHES beg. // yes
    
    str = "HELLO WORLD".
    beg = "HELLO.WORLD".
    DISPLAY str MATCHES beg. // yes
    
    str = "HELLO WORLD".
    beg = "*WORL..".
    DISPLAY str MATCHES beg. // no
    
    str = "*HELLO WORLD".
    beg = "WOR*LD".
    DISPLAY str MATCHES beg. // no



## Converting upper and lower case
As mentioned before strings are normally case insensitive but that only regards comparison of strings. There's built in functions for changing case.

> CAPS (string)
>
> Makes string upper case

> LC(string)
>
> Makes string lower case


    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE d AS CHARACTER   NO-UNDO.
    
    c = "Hello".
    d = "World".
    
    DISPLAY CAPS(c) LC(d). // HELLO world

Remember strings normally are case insensitive

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE d AS CHARACTER   NO-UNDO.
    
    c = "hello".
    d = "hello".
    
    DISPLAY CAPS(c) = LC(d). // yes

Unless specificed as `CASE-SENSITIVE`

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO CASE-SENSITIVE.
    DEFINE VARIABLE d AS CHARACTER   NO-UNDO.
    
    c = "hello".
    d = "hello".
    
    DISPLAY CAPS(c) = LC(d). // no





## Lists
There are a number of functions and methods for working with comma (or other character) separated lists in Progress 4GL. 

**NUM-ENTRIES**
Returns the number of entries in a list. You can optionally specify delimiter, comma is default

> NUM-ENTRIES(string [, delimiter])

Using comma, the default delimiter:

    DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
    
    cList = "Goodbye,cruel,world!".
    
    DISPLAY NUM-ENTRIES(cList). //3

Using another delimiter, semilcolon:

    DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
    
    cList = "Goodbye;cruel;world!".
    
    DISPLAY NUM-ENTRIES(cList, ";"). //3

**ENTRY** - function - returns a specified entry in a list

*As usual starting position is 1, not 0!*

> ENTRY( entry, list [, delimiter]).

    DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
    
    cList = "Goodbye,cruel,world!".
    
    DISPLAY ENTRY(2, cList). //cruel

**ENTRY** - method - assigning the value of a specified entry in a list

> ENTRY( entry, list [, delimiter]) = value


    DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
    
    cList = "Goodbye,cruel,world!".
    
    ENTRY(1, cList) = "Hello".
    ENTRY(2, cList) = "nice".
    
    MESSAGE REPLACE(cList, ",", " ") VIEW-AS ALERT-BOX. //Hello nice world!

**LOOKUP** - check a list for a specific entry. Returns it's entry.

If the string isn't present in the list lookup will returns 0

> LOOKUP(string, list [, delimiter])

    DEFINE VARIABLE cList AS CHARACTER   NO-UNDO.
    
    cList = "Hello,nice,world!".
    
    MESSAGE LOOKUP("nice", cList) VIEW-AS ALERT-BOX. //2
    MESSAGE LOOKUP("cruel", cList) VIEW-AS ALERT-BOX. //0



## Special characters (and escaping)
In Progress 4GL the normal way to write a special character is to preceed it with a tilde character (~).

These are the default special characters

| Sequence | Interpreted as   | Comment |
| -------- | ---------------- |---------|
| ~"       | "                | Used to write " inside strings defined using "string". 
| ~'       | '                | Used to write ' inside strings defined using 'string'.
| ~~       | ~                | For instance if you want to print the sequence and not how its interpreted.
| ~\       | \  
| ~{       | {                | { is used in preprocessors and sometimes escaping is needed.
| ~nnn     | A single character | nnn is an octal number representing the ascii value of the character.
| ~t       | tab | 
| ~n       | New line/line feed |
| ~r       | Carriage return | 
| ~E       | Escape | 
| ~b       | Backspace | 
| ~f       | Form feed | 

If you want to display tilde at all it must be escaped!

    MESSAGE "A single tilde: ~~" VIEW-AS ALERT-BOX.

    MESSAGE "At sign: ~100" SKIP
            "Tab~tseparated~twords!" SKIP
            "A linefeed:~n"
            "Escaping a quote sign: ~"This is a quote!~"" SKIP VIEW-AS ALERT-BOX.




