---
title: "Identifier"
slug: "identifier"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This topic is about identifiers, i.e. syntax rules for names of tables, columns, and other database objects.

Where appropriate, the examples should cover variations used by different SQL implementations, or identify the SQL implementation of the example.

## Unquoted identifiers
Unquoted identifiers can use letters (`a`-`z`), digits (`0`-`9`), and underscore (`_`), and must start with a letter.

Depending on SQL implementation, and/or database settings, other characters may be allowed, some even as the first character, e.g.

- MS SQL: `@`, `$`, `#`, and other Unicode letters *([source][2])*
- MySQL: `$` *([source][4])*
- Oracle: `$`, `#`, and other letters from database character set *([source][1])*
- PostgreSQL: `$`, and other Unicode letters *([source][3])*

Unquoted identifiers are case-insensitive. How this is handled depends greatly on SQL implementation:

- MS SQL: Case-preserving, sensitivity defined by database character set, so can be case-sensitive.
- MySQL: Case-preserving, sensitivity depends on database setting and underlying file system.
- Oracle: Converted to uppercase, then handled like quoted identifier.
- PostgreSQL: Converted to lowercase, then handled like quoted identifier.
- SQLite: Case-preserving; case insensitivity only for ASCII characters.

  [1]: https://docs.oracle.com/database/121/SQLRF/sql_elements008.htm#SQLRF00223
  [2]: https://docs.microsoft.com/en-us/sql/relational-databases/databases/database-identifiers
  [3]: https://www.postgresql.org/docs/current/static/sql-syntax-lexical.html
  [4]: https://dev.mysql.com/doc/refman/5.7/en/identifiers.html

