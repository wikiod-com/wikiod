---
title: "Command line dot-commands"
slug: "command-line-dot-commands"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

The [`sqlite3` command-line shell](http://www.sqlite.org/cli.html) implements an additional set of commands (which are not available in programs that use the SQLite library).

Official documentation: [Special commands to `sqlite3`](http://www.sqlite.org/cli.html#special_commands_to_sqlite3_dot_commands_)

## Exporting and importing a table as an SQL script
Exporting a database is a simple two step process:

    sqlite> .output mydatabase_dump.sql
    sqlite> .dump

Exporting a table is pretty similar:

    sqlite> .output mytable_dump.sql
    sqlite> .dump mytable

The output file needs to be defined with `.output` prior to using `.dump`; otherwise, the text is just output to the screen.

Importing is even simpler:

    sqlite> .read mytable_dump.sql

