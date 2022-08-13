---
title: "Getting started with sqlite"
slug: "getting-started-with-sqlite"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
SQLite is a [C][1] library that is typically [compiled](http://www.sqlite.org/howtocompile.html) directly into the application by [downloading](http://www.sqlite.org/download.html) the source code of the latest version, and adding the `sqlite3.c` file to the project.

Many script languages (e.g., [Perl][2], [Python][3], [Ruby][4], etc.) and frameworks (e.g., [Android][5]) have support for SQLite; this is done with a built-in copy of the SQLite library, which does not need to be installed separately.

For testing SQL, it might be useful to use the command-line shell (`sqlite3` or `sqlite3.exe`).
It is already shipped with most Linux distributions; on Windows, [download](http://www.sqlite.org/download.html) the precompiled binaries in the `sqlite-tools` package, and extract them somewhere.

[1]: https://www.wikiod.com/c
[2]: https://www.wikiod.com/perl
[3]: https://www.wikiod.com/python
[4]: https://www.wikiod.com/ruby
[5]: https://www.wikiod.com/android

## Documentation
SQLite already has extensive [documentation](http://www.sqlite.org/docs.html), which should not be duplicated here.

