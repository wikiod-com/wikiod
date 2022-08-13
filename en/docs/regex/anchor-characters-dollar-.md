---
title: "Anchor Characters Dollar ($)"
slug: "anchor-characters-dollar-"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

A great deal of regex engines use a ["multi-line" mode][1] in order to search several lines in a file independently.

Therefore when using `$`, these engines will match all lines' endings. However, engines that do not use this kind of multi-line mode will only match the last position of the string provided for the search.


  [1]: http://www.regular-expressions.info/anchors.html

## Match a letter at the end of a line or string
    g$

The above matches one letter (the letter `g`) at the end of a *string* in most regex engines (not in [Oniguruma][2], where the `$` anchor matches the end of a line by default, and the `m` (*MULTILINE*) modifier is used to make a `.` match any characters including line break characters, as a DOTALL modifier in most other NFA regex flavors). The `$` anchor will match the first occurrence of a `g` letter before the end of the following strings:

In the following sentences, only the letters in **bold** match:
> Anchors are characters that, in fact, do not match any character in a strin**g**
>
> Their goal is to match a specific position in that string.
>
> Bob was helpin**g**
>
> But his edit introduced examples that were not matching!

In most regular expression flavors, the `$` anchor can also match before a newline character or line break character (sequence), in a [*MULTILINE* mode][1], where `$` matches at the end of every line instead of only at the end of a string. For example, using `g$` as our regex again, in multiline mode, the italicised characters in the following string would match:

<pre><code>tvxlt obofh necpu riist <em>g</em>\n aelxk zlhdx lyogu vcbke pzyay wtsea wbrju jzt<em>g</em>\n drosf ywhed bykie lqmzg wgyhc l<em>g</em>\n qewrx ozrvm jwenx</code></pre>


  [1]: http://www.regular-expressions.info/anchors.html
  [2]: https://github.com/geoffgarside/oniguruma/blob/master/Syntax.txt

