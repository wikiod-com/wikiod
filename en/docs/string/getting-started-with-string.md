---
title: "Getting started with string"
slug: "getting-started-with-string"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Strings
A string is a sequence of character literals. To date, strings are supported by all modern programming languages,<sup>1</sup> but there is no consensus between language designers on how strings should be classified. As far as programming language design is concerned there are two primary concerns to take into consideration. 

1. Should a string be treated as a primitive- or a composite value?
2. What string operations should be provided by the language itself?<sup>2</sup>

By making a string a primitive value the string operations provided by the language are all built-in; and cannot be defined in the language itself. This allows certain optimizations at a compiler level &mdash; specifically with respect to memory layout and re-use of strings using a so called string pool. The trade-off lies in not being able to use drop in replacements for the string operations, and such functions would have to be called using normal function calls while the syntax for using built-in operations is usually distinctly different. The consequence being that if and when more effective algorithms are discovered one cannot simply change which string library one is using. A contrived example being having substring search as part of the core feature set before Boyer–Moore string search algorithm was developed.

Conversely, by defining a string to be a composite value such as an array of characters all the usual array operations become automatically applicable to strings. However, this results in all strings within the language as being fixed-length.<sup>3</sup>

Ultimately, another approach is representing strings as a list of characters which - as with arrays - allows the representation to inherit all list operations.

<sup>1</sup> Save the more esoteric ones such as Piet  
<sup>2</sup> Languages usually offer string comparisons, primarily equality, concatenating strings with other strings, substituting single characters and substrings and lexographic sorting of strings.
<sup>3</sup> Usually

