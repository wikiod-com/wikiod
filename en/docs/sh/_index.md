---
title : sh Tutorial
slug : sh-tutorial
weight : 9984
draft : false
images : []
type : docs
---

`sh` is not a single shell. Rather, it is a specification with the POSIX operating system standard for how a shell should work. A script that targets this specification can be executed by any POSIX-compliant shell, such as

  * `bash`
  * `ksh`
  * `ash` and its derivatives, such as `dash`
  * `zsh`

In a POSIX-compliant operating system, the path `/bin/sh` refers to a POSIX-compliant shell. This is usually a shell that has features not found in the POSIX standard, but when run as `sh`, will restrict itself to the POSIX-compliant subset of its features.

References
-

 - [Standard `sh`][std]
 - [The FreeBSD `sh(1)` man-page][freebsd]
 - [The NetBSD `sh(1)` man-page][netbsd]
 - [The OpenBSD `sh(1)` man-page][openbsd]
 - [The Illumos `sh(1)` man-page (`ksh93(1)`)][illumos]

[std]: http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
[freebsd]: https://www.freebsd.org/cgi/man.cgi?query=sh    
[netbsd]: http://netbsd.gw.com/cgi-bin/man-cgi?sh++NetBSD-current
[openbsd]: http://man.openbsd.org/?query=sh
[illumos]: https://illumos.org/man/1/sh

