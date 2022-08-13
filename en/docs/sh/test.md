---
title: "Test"
slug: "test"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

## Syntax
 - **test**
 - **test** [!] [ -n | -z ] _string_
 - **test** [!] { -b | -c | -d | -e | -f | -g | -h | -L | -p | -r | -S | -s | -u | -w | -x } _file_
 - **test** [!] -t _fd_
 - **test** [!] _string_ { = | != } _string_
 - **test** [!] _integer_ { -eq | -ne | -gt | -ge | -lt | -le } _integer_
 - **[** **]**
 - **[** [!] [ -n | -z ] _string_ **]**
 - **[** [!] { -b | -c | -d | -e | -f | -g | -h | -L | -p | -r | -S | -s | -u | -w | -x } _file_ **]**
 - **[** [!] -t _fd_ **]**
 - **[** [!] _string_ { = | != } _string_ **]**
 - **[** [!] _integer_ { -eq | -ne | -gt | -ge | -lt | -le } _integer_ **]**


If `test(1)` is run without any arguments it returns false.

Reference
-
 - [Standard `test(1)`][std]
 - [The FreeBSD `test(1)` man-page][freebsd]
 - [The NetBSD `test(1)` man-page][netbsd]
 - [The OpenBSD `test(1)` man-page][openbsd]
 - [The Illumos `test(1)` man-page][illumos]
 - [The GNU Coreutils online manual section on `test(1)`][coreutil]

[std]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
[freebsd]: https://www.freebsd.org/cgi/man.cgi?query=test
[netbsd]: http://netbsd.gw.com/cgi-bin/man-cgi?test++NetBSD-current
[openbsd]: http://man.openbsd.org/OpenBSD-current/man1/test.1
[illumos]: https://illumos.org/man/1/test
[coreutil]: https://www.gnu.org/software/coreutils//manual/html_node/test-invocation.html#test-invocation

## Multiple Expressions
Though it is [an obsoleted part of the XSI standard][stdtestop], many implementations still support multiple expressions with Boolean operators and parenthesis.

The [(obsolete) operators][stdtestop] are listed below with decreasing precedence.

    ( expression )
    expression -a expression
    expression -o expression

Using these [(obsolete) operators][stdtestop], a complex shell expression:

    if [ "$a" -gt 0 ] && { [ "$b" -ne 2 ] || [ "$b" -e 0 ]; }
    then ...
    fi

Could be written with one invocation of `test(1)`:

    if [ "$a" -gt 0 -a '(' "$b" -ne 2 -o "$c" -ne 0 ')' ]
    then ...
    fi

[stdtestop]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html#tag_20_128_05

