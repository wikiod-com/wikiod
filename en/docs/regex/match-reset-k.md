---
title: "Match Reset K"
slug: "match-reset-k"
draft: false
images: []
weight: 9936
type: docs
toc: true
---

Regex101 defines \K functionality as:

>`\K` resets the starting point of the reported match. Any previously consumed characters are no longer included in the final match



The `\K` escape sequence is supported by several engines, languages or tools, such as:

 - boost (since ???)
 - grep -P &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;*← uses PCRE*
 - Oniguruma ([since 5.13.3][4])
 - PCRE ([since 7.2][2])
 - Perl ([since 5.10.0][3])
 - PHP ([since 5.2.4][1])
 - Ruby (since 2.0.0)

...and (so far) not supported by:

 - [.NET][5]
 - awk
 - bash
 - GNU
 - [ICU](http://userguide.icu-project.org/strings/regexp)
 - [Java][6]
 - Javascript
 - Notepad++
 - Objective-C
 - POSIX
 - Python
 - Qt/QRegExp
 - sed
 - Tcl
 - vim
 - XML
 - XPath



  [1]: http://php.net/manual/en/regexp.reference.escape.php
  [2]: http://en.wikipedia.org/wiki/Perl_Compatible_Regular_Expressions
  [3]: http://search.cpan.org/~rgarcia/perl/pod/perl5100delta.pod
  [4]: https://github.com/k-takata/Onigmo/blob/master/README
  [5]: http://stackoverflow.com/questions/3417644/translate-perl-regex-to-net
  [6]: http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html



## Search and replace using \K operator
Given the text:

>foo: bar

I would like to replace anything following "foo: " with "baz", but I want to keep "foo: ". This could be done with a capturing group like this:

    s/(foo: ).*/$1baz/

Which results in the text:

>foo: baz

[Example 1][1]

or we could use `\K`, which "forgets" all that it has previously matched, with a pattern like this:

    s/foo: \K.*/baz/

The regex matches "foo: " and then encounters the `\K`, the previously match characters are taken for granted and left by the regex meaning that only the string matched by `.*` will be replaced by "baz", resulting in the text:

>foo: baz

[Example 2][2]  


  [1]: https://regex101.com/r/zS8oP4/1
  [2]: https://regex101.com/r/zS8oP4/2

