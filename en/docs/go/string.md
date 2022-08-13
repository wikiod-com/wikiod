---
title: "String"
slug: "string"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

A string is in effect a read-only slice of bytes. In Go a string literal will always contain a valid UTF-8 representation of its content.

## Syntax
- variableName := "Hello World" // declare a string
- variableName := \`Hello World` // declare a raw literal string
- variableName := "Hello " + "World" // concatenates strings
- substring := "Hello World"[0:4] // get a part of the string
- letter := "Hello World"[6] // get a character of the string
- fmt.Sprintf("%s", "Hello World") // formats a string


## strings package
* [`strings.Contains`](https://golang.org/pkg/strings/#Contains)

      fmt.Println(strings.Contains("foobar", "foo")) // true
      fmt.Println(strings.Contains("foobar", "baz")) // false

* [`strings.HasPrefix`](https://golang.org/pkg/strings/#HasPrefix)

      fmt.Println(strings.HasPrefix("foobar", "foo")) // true
      fmt.Println(strings.HasPrefix("foobar", "baz")) // false

* [`strings.HasSuffix`](https://golang.org/pkg/strings/#HasSuffix)

      fmt.Println(strings.HasSuffix("foobar", "bar")) // true
      fmt.Println(strings.HasSuffix("foobar", "baz")) // false

* [`strings.Join`](https://golang.org/pkg/strings/#Join)

      ss := []string{"foo", "bar", "bar"}
      fmt.Println(strings.Join(ss, ", ")) // foo, bar, baz

* [`strings.Replace`](https://golang.org/pkg/strings/#Replace)

      fmt.Println(strings.Replace("foobar", "bar", "baz", 1)) // foobaz

* [`strings.Split`](https://golang.org/pkg/strings/#Split)

      s := "foo, bar, bar"
      fmt.Println(strings.Split(s, ", ")) // [foo bar baz]

* [`strings.ToLower`](https://golang.org/pkg/strings/#ToLower)

      fmt.Println(strings.ToLower("FOOBAR")) // foobar

* [`strings.ToUpper`](https://golang.org/pkg/strings/#ToUpper)

      fmt.Println(strings.ToUpper("foobar")) // FOOBAR

* [`strings.TrimSpace`](https://golang.org/pkg/strings/#TrimSpace)

      fmt.Println(strings.TrimSpace("  foobar  ")) // foobar

More: https://golang.org/pkg/strings/.


## String type
The `string` type allows you to store text, which is a series of characters. There are multiple ways to create strings. A literal string is created by writing the text between double quotes.

    text := "Hello World"

Because Go strings support UTF-8, the previous example is perfectly valid. Strings hold arbitrary bytes which does not necessarily mean every string will contain valid UTF-8 but string literals will always hold valid UTF-8 sequences.

The zero value of strings is an empty string `""`.

Strings can be concatenated using the `+` operator.

    text := "Hello " + "World"

Strings can also be defined using backticks ` `` `. This creates a raw string literal which means characters won't be escaped.

    text1 := "Hello\nWorld"
    text2 := `Hello
    World`

In the previous example, `text1` escapes the `\n` character which represents a new line while `text2` contains the new line character directly. If you compare `text1 == text2` the result will be `true`.

However, ``text2 := `Hello\nWorld` `` would not escape the `\n` character which means the string contains the text `Hello\nWorld` without a new line. It would be the equivalent of typing `text1 := "Hello\\nWorld"`.

## Formatting text
Package `fmt` implements functions to print and format text using format _verbs_. Verbs are represented with a percent sign.

General verbs:

    %v    // the value in a default format
          // when printing structs, the plus flag (%+v) adds field names
    %#v   // a Go-syntax representation of the value
    %T    // a Go-syntax representation of the type of the value
    %%    // a literal percent sign; consumes no value

Boolean:

    %t    // the word true or false

Integer:

    %b    // base 2
    %c    // the character represented by the corresponding Unicode code point
    %d    // base 10
    %o    // base 8
    %q    // a single-quoted character literal safely escaped with Go syntax.
    %x    // base 16, with lower-case letters for a-f
    %X    // base 16, with upper-case letters for A-F
    %U    // Unicode format: U+1234; same as "U+%04X"

Floating-point and complex constituents:

    %b    // decimalless scientific notation with exponent a power of two,
          // in the manner of strconv.FormatFloat with the 'b' format,
          // e.g. -123456p-78
    %e    // scientific notation, e.g. -1.234456e+78
    %E    // scientific notation, e.g. -1.234456E+78
    %f    // decimal point but no exponent, e.g. 123.456
    %F    // synonym for %f
    %g    // %e for large exponents, %f otherwise
    %G    // %E for large exponents, %F otherwise

String and slice of bytes (treated equivalently with these verbs):

    %s    // the uninterpreted bytes of the string or slice
    %q    // a double-quoted string safely escaped with Go syntax
    %x    // base 16, lower-case, two characters per byte
    %X    // base 16, upper-case, two characters per byte

Pointer:

    %p    // base 16 notation, with leading 0x

Using the verbs, you can create strings concatenating multiple types:

    text1 := fmt.Sprintf("Hello %s", "World")
    text2 := fmt.Sprintf("%d + %d = %d", 2, 3, 5)
    text3 := fmt.Sprintf("%s, %s (Age: %d)", "Obama", "Barack", 55)

The function `Sprintf` formats the string in the first parameter replacing the verbs with the value of the values in the next parameters and returns the result. Like `Sprintf`, the function `Printf` also formats but instead of returning the result it prints the string.

