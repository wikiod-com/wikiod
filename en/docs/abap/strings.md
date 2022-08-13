---
title: "Strings"
slug: "strings"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## String templates
String templates are a convenient way of mixing literal strings with values from variables:

    WRITE |Hello, { lv_name }, nice to meet you!|.

It can also format things like dates. To use the logged on user's date format:

    WRITE |The order was completed on { lv_date DATE = USER } and can not be changed|.

Functional method calls and expressions are supported:

    WRITE |Your token is { to_upper( lv_token ) }|.
    WRITE |Version is: { cond #( when lv_date < sy-datum then 'out of date' else 'up to date' ) }|.

> **Attention!** Directly implementing temporary results (like method-calls) inside of string templates can lead to massive performance problems (read more about it [here](https://blogs.sap.com/2016/08/15/performance-trap-in-string-concatenations/)). While using it inside of rarely executed statements is okay, it causes your program to rapidly slow down in loops.

## Literals
ABAP offers three different operators for declaring string- or char-like-variables


| Symbols   | Internal Type | Length | Name |
| ------     | ------ | ------ | ------ |
| '...'      | C   | 1-255 Chars | text field literals
| &#96;...&#96; | CString | 0-255 Chars | text string literals
| &#124;...&#124; | CString | 0-255 Chars | template literals

Note that the length-range only applies to hard coded values. Internally `CString`-variables have arbitrary length while variables of type `C` always have a fixed length.

## Concatenating strings
String and char-like variables can be concatenated using ABAP `CONCATENATE` command.
An extra variable for storing the results is required.

**Example:**
    
    CONCATENATE var1 var2 var3 INTO result.
    "result now contains the values of var1, var2 & var3 stringed together without spaces

**Shorthand**

Newer versions of ABAP offer a very short variant of concatenation using && (Chaining operator).

    DATA(lw_result) = `Sum: ` && lw_sum.

> **Attention!** It's worth noticing, that using temporary results in combination with the Chaining operator inside of loops can lead to massive performance problems due to growing copy instructions (read more about it [here](https://blogs.sap.com/2016/08/15/performance-trap-in-string-concatenations/)).

