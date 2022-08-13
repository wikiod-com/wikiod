---
title: "Matching Simple Patterns"
slug: "matching-simple-patterns"
draft: false
images: []
weight: 9955
type: docs
toc: true
---

## Matching various numbers
`[a-b]` where a and b are digits in the range `0` to `9`

    [3-7] will match a single digit in the range 3 to 7.

Matching multiple digits

    \d\d       will match 2 consecutive digits
    \d+        will match 1 or more consecutive digits
    \d*        will match 0 or more consecutive digits
    \d{3}      will match 3 consecutive digits
    \d{3,6}    will match 3 to 6 consecutive digits
    \d{3,}     will match 3 or more consecutive digits

The `\d` in the above examples can be replaced with a number range:

    [3-7][3-7]    will match 2 consecutive digits that are in the range 3 to 7
    [3-7]+        will match 1 or more consecutive digits that are in the range 3 to 7
    [3-7]*        will match 0 or more consecutive digits that are in the range 3 to 7
    [3-7]{3}      will match 3 consecutive digits that are in the range 3 to 7
    [3-7]{3,6}    will match 3 to 6 consecutive digits that are in the range 3 to 7
    [3-7]{3,}     will match 3 or more consecutive digits that are in the range 3 to 7

You can also select specific digits:

    [13579]       will only match "odd" digits
    [02468]       will only match "even" digits
    1|3|5|7|9     another way of matching "odd" digits - the | symbol means OR

Matching numbers in ranges that contain more than one digit:

    \d|10        matches 0 to 10    single digit OR 10.  The | symbol means OR
    [1-9]|10     matches 1 to 10    digit in range 1 to 9 OR 10
    [1-9]|1[0-5] matches 1 to 15    digit in range 1 to 9 OR 1 followed by digit 1 to 5
    \d{1,2}|100  matches 0 to 100   one to two digits OR 100

Matching numbers that divide by other numbers:

    \d*0         matches any number that divides by 10  - any number ending in 0
    \d*00        matches any number that divides by 100 - any number ending in 00
    \d*[05]      matches any number that divides by 5   - any number ending in 0 or 5
    \d*[02468]   matches any number that divides by 2   - any number ending in 0,2,4,6 or 8

matching numbers that divide by 4 - any number that is 0, 4 or 8 or ends in 00, 04, 08, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92 or 96

    [048]|\d*(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)

This can be shortened.  For example, instead of using `20|24|28` we can use `2[048]`. Also, as the 40s, 60s and 80s have the same pattern we can include them: `[02468][048]` and the others have a pattern too `[13579][26]`. So the whole sequence can be reduce to:

    [048]|\d*([02468][048]|[13579][26])    - numbers divisible by 4

Matching numbers that don't have a pattern like those divisible by 2,4,5,10 etc can't always be done succinctly and you usually have to resort to a range of numbers.  For example matching all numbers that divide by 7 within the range of 1 to 50 can be done simple by listing all those numbers:

    7|14|21|28|35|42|49

    or you could do it this way

    7|14|2[18]|35|4[29]

## Matching leading/trailing whitespace
# Trailing spaces
`\s*$`: This will match any (`*`) whitespace (`\s`) at the end (`$`) of the text

# Leading spaces
`^\s*`: This will match any (`*`) whitespace (`\s`) at the beginning (`^`) of the text

## Remarks
`\s` is a common metacharacter for several RegExp engines, and is meant to capture whitespace characters (spaces, newlines and tabs for example). **Note**: it probably _won't_ capture all the [unicode space characters][1]. Check your engines documentation to be sure about this.

  [1]: https://en.wikipedia.org/wiki/Whitespace_character#Unicode

## Match a single digit character using [0-9] or \d (Java)
`[0-9]` and `\d` are equivalent patterns (unless your Regex engine is unicode-aware and `\d` also matches things like ②). They will both match a single digit character so you can use whichever notation you find more readable.

Create a string of the pattern you wish to match. If using the \d notation, you will need to add a second backslash to escape the first backslash.

    String pattern = "\\d";

Create a Pattern object. Pass the pattern string into the compile() method.

    Pattern p = Pattern.compile(pattern);

Create a Matcher object. Pass the string you are looking to find the pattern in to the matcher() method. Check to see if the pattern is found.

    Matcher m1 = p.matcher("0");
    m1.matches(); //will return true

    Matcher m2 = p.matcher("5");
    m2.matches(); //will return true
    
    Matcher m3 = p.matcher("12345");
    m3.matches(); //will return false since your pattern is only for a single integer

## Match any float
```
[\+\-]?\d+(\.\d*)?
```

This will match any signed float, if you don't want signs or are parsing an equation remove `[\+\-]?` so you have `\d+(\.\d+)?`

Explanation:
- `\d+` matches any integer
- `()?` means the contents of the parentheses are optional but always have to appear together
- '\\.' matches '.', we have to escape this since '.' normally matches any character

So this expression will match

```
5
+5
-5
5.5
+5.5
-5.5
```

## Selecting a certain line from a list based on a word in certain location
I have the following list:

    1. Alon Cohen
    2. Elad Yaron
    3. Yaron Amrani
    4. Yogev Yaron

I want to select the first name of the guys with the Yaron surname.

Since I don't care about what number it is I'll just put it as whatever digit it is and a matching dot and space after it from the beginning of the line, like this: `^[\d]+\.\s`.

Now we'll have to match the space and the first name, since we can't tell whether it's capital or small letters we'll just match both: `[a-zA-Z]+\s` or `[a-Z]+\s` and can also be `[\w]+\s`.

Now we'll specify the required surname to get only the lines containing Yaron as a surname (at the end of the line): `\sYaron$`.

Putting this all together `^[\d]+\.\s[\w]+\sYaron$`.

Live example: https://regex101.com/r/nW4fH8/1

